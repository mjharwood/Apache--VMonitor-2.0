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

# XXX: fixme
my @sects = qw(system fs_usage mount);

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
  <style type="text/css">
  body {
    color: #000;
    background-color: #fff;
    font-size: 0.8em;
  }
  p.hdr {
    background-color: #ddd;
    border: 2px outset;
    padding: 3px;
    width: 99%;
  }
  </style>
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



### system ###

sub data_system {
    my $self = shift;

    # uptime and etc...
    my($min, $hour, $day, $mon, $year) = (localtime)[1..5];
    my %date = (
        min   => $min,
        hour  => $hour,
        day   => $day,
        month => $mon + 1,
        year  => $year + 1900,
    );

    my $loadavg = $gtop->loadavg;

    my $data = {
        date    => \%date,
        uptime  => format_time($gtop->uptime->uptime),
        loadavg => \@{ $loadavg->loadavg },
    };

    if ($^O eq 'linux') {
        $data->{tasks} = [ $loadavg->nr_tasks, $loadavg->nr_running ];
    }

    # total CPU stats
    my $cpu   = $gtop->cpu;
    my $total = $cpu->total;
    $data->{cpu} = {
        map { $_ => ( $total ? ($cpu->$_() * 100 / $total) : 0 ) }
            qw(user nice sys idle)
    };

    # total mem stats
    my $mem = $gtop->mem;
    $data->{mem} = {
        map { $_ => size_string($mem->$_()) }
            qw(total used free shared buffer)
    };

    # total swap stats
    my $swap       = $gtop->swap();
    my $swap_total = $swap->total();
    my $swap_used  = $swap->used();
    $data->{swap} = {
        usage => ($swap_total ? ($swap_used * 100 / $swap_total) : 0),
        used  => $swap_used,
        map({ ("f$_" => size_string($swap->$_)) }
            qw(total used free)),
        map({ ("f$_" => format_counts($swap->$_)) }
            qw(pagein pageout)),
    };

    return $data;
}



sub tmpl_system {

    return \ <<'EOT';
<hr>
<pre>
[%-

  # date/time/load
  USE format_date = format("%d/%.2d/%d");
  fdate = format_date(date.month, date.day, date.year);

  USE format_time = format("%d:%.2d%s");
  pam = date.hour > 11 ? "pm" : "am";
  date.hour = date.hour - 12 IF date.hour > 11;
  ftime = format_time(date.hour, date.min, pam);

  USE format_load = format("%.2f %.2f %.2f");
  floadavg = format_load(loadavg.0, loadavg.1, loadavg.2,);

  USE format_run_procs = format(", %d processes/threads: %d running");
  frun_procs = tasks
      ? format_run_procs(tasks.0, tasks.1)
      : "";

  USE format_line_time_load =
      format("<b>%s %s  up %s, load average: %s%s</b>\n");
  format_line_time_load(fdate, ftime, uptime, floadavg, frun_procs);



  # CPU
  USE format_line_cpu =
      format("<b>CPU:   %2.1f%% user, %2.1f%% nice, %2.1f%% sys, %2.1f%% idle</b>\n");
  format_line_cpu(cpu.user, cpu.nice, cpu.sys, cpu.idle);


  # Memory
  USE format_line_mem =
      format("<b>Mem:  %5s av, %5s used, %5s free, %5s shared, %5s buff</b>\n");
  format_line_mem(mem.total, mem.used, mem.free, mem.shared, mem.buffer);


  # SWAP
    # visual alert on swap usage:
    # 1) 5Mb < swap < 10 MB             color: light red
    # 2) 20% < swap (swapping is bad!)  color: red
    # 3) 70% < swap (swap almost used!) color: red + blinking

  format_swap_data = "%5s av, %5s used, %5s free, %5s pagein, %5s pageout";
  IF 5000 < swap.used AND swap.used < 10000;
      USE format_line_swap = format("<b>Swap: <font color=\"#ff99cc\">$format_swap_data</font></b>\n");
  ELSIF swap.usage >= 20;
      USE format_line_swap = format("<b>Swap: <font color=\"#ff0000\">$format_swap_data</font></b>\n");
  ELSIF swap.usage >= 70;
      # swap on fire!
      USE format_line_swap = format("<b>Swap: <blink><font color=\"#ff0000\">$format_swap_data</font></blink></b>\n");
  ELSE;
      USE format_line_swap = format("<b>Swap: $format_swap_data</b>\n");
  END;

  format_line_swap(swap.ftotal, swap.fused, swap.ffree, swap.fpagein, swap.fpageout);



-%]
</pre>
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


### helpers ###

# Takes seconds as int or float as an argument 
#
# Returns string of time in days (12d) or
# hours/minutes (11:13) if less then one day, 
# and secs.millisec (12.234s) if less than a minute
#
# The returned sting is always of 6 digits length (taken that
# length(int days)<4) so you can ensure the column with 
# printf "%7s", format_time($secs)
###############
sub format_time {
  my $secs = shift || 0;
  return sprintf "%6.3fs", $secs if $secs < 60;
  my $hours = $secs / 3600;
  return sprintf "%6.2fd", $hours / 24 if $hours > 24;
  return sprintf " %02d:%2.2dm", int $hours,
      int $secs%3600 ?  int (($secs%3600)/60) : 0;
}



sub size_string {
    my($size) = @_;

    if (!$size) {
        $size = "   0k";
    }
    elsif ($size == -1) {
        $size = "    -";
    }
    elsif ($size < 1024) {
        $size = "   1k";
    }
    elsif ($size < 1048576) {
        $size = sprintf "%4dk", ($size + 512) / 1024;
    }
    elsif ($size < 103809024) {
        $size = sprintf "%4.1fM", $size / 1048576.0;
    }
    else {
        $size = sprintf "%4dM", ($size + 524288) / 1048576;
    }

    return $size;
}

# any number that enters we return its compacted version of max 4
# chars in length (5, 123, 1.2M, 12M, 157G)
# note that here 1K is 1000 and not 1024!!!
############
sub format_counts {
  local $_ = shift || 0;

  my $digits = tr/0-9//;
  return $_                                                          if $digits < 4;
  return sprintf "%.@{[$digits%3 == 1 ? 1 : 0]}fK", $_/1000          if $digits < 7;
  return sprintf "%.@{[$digits%3 == 1 ? 1 : 0]}fM", $_/1000000       if $digits < 10;
  return sprintf "%.@{[$digits%3 == 1 ? 1 : 0]}fG", $_/1000000000    if $digits < 13;
  return sprintf "%.@{[$digits%3 == 1 ? 1 : 0]}fT", $_/1000000000000 if $digits < 16;

} # end of sub format_counts

1;
__END__

