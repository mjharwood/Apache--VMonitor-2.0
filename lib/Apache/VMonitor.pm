package Apache::VMonitor;

$Apache::VMonitor::VERSION = '1.0';

use strict;
use warnings; # XXX: 5.005
no warnings 'redefine'; # XXX

use Apache::RequestRec ();
use Apache::RequestIO ();
use APR::Table ();

use Template ();
use GTop ();

use Apache::Const qw(OK);

my $gtop = GTop->new;

my $tt = Template->new({
    #POST_CHOMP => 1,
    #PRE_CHOMP => 1,
});

########################
# default config values
########################
%Apache::VMonitor::Config = (
     # behavior
   refresh  => 0,
   verbose  => 0,

     # sections to show
   system   => 1,
   apache   => 1,
   procs    => 0,
   mount    => 0,
   fs_usage => 1,
   sort_by  => 'size',
);

my @sects = qw(system apache procs mount fs_usage verbose);

my %cfg = ();

# XXX: mp1/p5.005
sub handler : method {
    my ($class, $r) = @_;
    $class = ref($class)||$class;

    my %params = map { split('=', $_, 2) } split /[&]/, $r->args;

    # modify the default args if requested
    map {
        $cfg{$_} = $Apache::VMonitor::Config{$_};       # first the defaults
        $cfg{$_} = $params{$_} if defined $params{$_};  # second the dynamic config
    } keys %Apache::VMonitor::Config;

    my $pid = $params{pid} || 0;

    # build the updated URL (append the pid k/v pair)
    my $url = $r->uri . "?pid=$pid&" . join "&", map {"$_=$cfg{$_}"} keys %cfg;

    # if refresh is non-null, set the refresh header
    $r->headers_out->set(Refresh => "$cfg{refresh}; URL=$url") if $cfg{refresh};

    $r->content_type('text/html');

    my $self = $class->new(
        tt    => $tt,
        gtop  => $gtop,
        cfg   => \%cfg,
        url   => $url,
    );

    $self->generate;

    return OK;
}

sub new {
    my $class = shift;
    my $self = bless {@_}, ref($class)||$class;
    return $self;
}

sub generate {
    my $self = shift;
    my $cfg = $self->{cfg};
    my $tt = $self->{tt};
    my @items = 'start_html';

    my @sects = qw(fs_usage mount); # XXX: fixme
    $cfg->{$_} && push @items, $_ for (@sects);
    push @items, qw(nav_bar end_html);

    for (@items) {
        my $tmpl_sub = $self->can("tmpl_$_");
        next unless $tmpl_sub;
        my $data_sub = $self->can("data_$_");
        my $data = $data_sub ? $self->$data_sub : {};
        $tt->process($self->$tmpl_sub, $data) or warn $tt->error();
    }
}

#
# my $newurl = get_url($url, $key, $val)
# update key/val of the query and return
############
sub fixup_url {
    my($url, $key, $val) = @_;

    (my $new_url = $url) =~ s/$key=([^&]+)?/$key=$val/;

    return $new_url;
}








### start_html ###

sub data_start_html {
    my $self = shift;

    my $url = $self->{url};
    my $cfg = $self->{cfg};

    my @rates = map {
        [$_, ($cfg->{refresh} == $_ ? '' : fixup_url($url, 'refresh', $_)) ];
    } qw(0 1 5 10 20 30 60);

    return {
        rate  => $cfg->{refresh},
        rates => \@rates,
    };

}

sub tmpl_start_html {
    return \ <<'EOT';
<html>
<head>
<title>Apache::VMonitor</title>
</head>
<body bgcolor="white">
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<b><font size=+1 color="#339966">Apache::VMonitor</font></b>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
Refresh rate:&nbsp;&nbsp;
[% IF rates.size %]
    [%- FOREACH item = rates -%]
        [%- IF item.1 -%]
            <a href="[% item.1 %]">[ [% item.0 %] ]</a>&nbsp;&nbsp;
        [%- ELSE -%]
            [ [% item.0 %] ]&nbsp;&nbsp;
        [%- END -%]
    [%- END -%]
[% END %]
<br>
EOT

}






### end_html ###

# not needed
#sub data_end_html { {} }

sub tmpl_end_html {

    return \ <<'EOT';
</body>
</html>
EOT
}






### nav_bar ###

sub data_nav_bar {
    my $self = shift;

    my $url = $self->{url};
    my $cfg = $self->{cfg};
    my %hide = ();
    my %show = ();

    foreach (@sects) {
        if ($cfg->{$_}) {
            $hide{$_} = fixup_url($url, $_, 0);
        }
        else {
            $show{$_} = fixup_url($url, $_, 1);
        }
    }

    return {
        show => \%show,
        hide => \%hide,
    };
}

sub tmpl_nav_bar {

    return \ <<'EOT';
<hr>
   <font size=-1>
[% IF show.size %]
Show: 
    [%- FOREACH item = show -%]
        [ <a href="[% item.value %]">[% item.key %]</a> ]
    [%- END -%]
<br>
[% END %]

[% IF hide.size %]
Hide: 
    [%- FOREACH item = hide -%]
        [ <a href="[% item.value %]">[% item.key %]</a> ]
    [%- END -%]
<br>
[% END %]
   </font><hr>
EOT
}







### fs_usage ###

sub data_fs_usage {
    my $self = shift;

    my($mountlist, $entries) = $gtop->mountlist(1);
    my $fs_number = $mountlist->number;

    # for formatting purpose find out the max length of the filesystems
    my $max_fs_name_len = 0;
    my %fs = ();
    for (my $i = 0; $i < $fs_number; $i++) {
        my $path = $entries->mountdir($i);
        $fs{$path} = $i;
        my $len = length $path;
        $max_fs_name_len = $len if $len > $max_fs_name_len;
    }

    $max_fs_name_len = 12 if $max_fs_name_len < 12;

    # the filesystems
    my @items = ();
    for my $path (sort keys %fs){
        my $i = $fs{$path};
        my $fsusage = $gtop->fsusage($entries->mountdir($i));

        my $total_blocks      = $fsusage->blocks / 2;
        my $su_avail_blocks   = $fsusage->bfree  / 2 ;
        my $user_avail_blocks = $fsusage->bavail / 2;
        my $used_blocks       = $total_blocks - $su_avail_blocks;
        my $usage_blocks      = $total_blocks 
            ? ($total_blocks - $user_avail_blocks)* 100 / $total_blocks
            : 0;
        my $total_files       = $fsusage->files;
        my $free_files        = $fsusage->ffree;
        my $usage_files       = $total_files 
            ? ($total_files - $free_files) * 100 / $total_files
            : 0;

        push @items, {
            path => $path,

            blocks => {
                total      => $total_blocks,
                used       => $used_blocks,
                user_avail => $user_avail_blocks,
                usage      => $usage_blocks,
            },

            files => {
                total => $total_files,
                free  => $free_files,
                usage => $usage_files,
            },
        };
    }

    return {
        max_fs_name_len => $max_fs_name_len,
        items           => \@items,
    };
}

sub tmpl_fs_usage {
    return \ <<'EOT';
<hr>
<pre>
[%-
  fs_name_len = max_fs_name_len - 4;
  USE format_header = format("%-${fs_name_len}s %14s %9s %9s %3s %12s %7s %5s\n");

  format_header("FS", "1k Blks: Total", "SU Avail", "User Avail", "Usage",
    "   Files: Total", "Avail", "Usage");


  format_blocks = "%9d %9d %10d %4d%% ";
  format_files  = "       %7d %7d %4d%%";
  format_fs     = "%-${max_fs_name_len}s ";

  FOREACH item = items;
      # visual alert on filesystems of 90% usage!
      IF item.blocks.usage >= 90 AND item.files.usage >= 90;
          USE format_item = format("<b><font color=\"#ff0000\">$format_fs $format_blocks $format_files</font></b>\n");
      ELSIF item.blocks.usage >= 90;
          USE format_item = format("<b><font color=\"#ff0000\">$format_fs $format_blocks</font></b> $format_files\n");
      ELSIF item.files.usage >= 90;
          USE format_item = format("<b><font color=\"#ff0000\">$format_fs</font></b> $format_blocks <b><font color=\"#ff0000\">$format_files</font></b>\n");
      ELSE;
          USE format_item = format("$format_fs $format_blocks $format_files\n");
      END;

      format_item(item.path,
                  item.blocks.total,
                  item.blocks.used,
                  item.blocks.user_avail,
                  item.blocks.usage,
                  item.files.total,
                  item.files.free,
                  item.files.usage
      );
  END;
-%]
</pre>
EOT
}





### mount ###

sub data_mount {
    my $self = shift;

    my($mountlist, $entries) = $gtop->mountlist(1);
    my $fs_number = $mountlist->number;

    my @items = ();
    for (my $i=0; $i < $fs_number; $i++) {
        push @items, {
           devname  => $entries->devname($i),
           mountdir => $entries->mountdir($i),
           type     => $entries->type($i),
        };
    }

    # sort by device name
    @items = sort { $a->{devname} cmp $b->{devname} } @items;
    return {
        items => \@items,
    };
}

sub tmpl_mount {

    return \ <<'EOT';
<hr>
<pre>
[%-
  USE format_header = format("<b>%-30s %-30s %-10s</b>\n");

  format_header("DEVICE", "MOUNTED ON", "FS TYPE");

  USE format_item = format("%-30s %-30s %-10s\n");
  FOREACH item = items;
      format_item(item.devname,
                  item.mountdir,
                  item.type
      );
  END;
-%]
</pre>
EOT

}

1;
__END__

