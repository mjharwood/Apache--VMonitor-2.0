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
        r     => $r,
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
my @sects = qw(system apache fs_usage mount);

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
  div.even_item {
    background-color: #dddddd;
    color: #000000;
  }
  div.odd_item {
    background-color: #ffffff;
    color: #000000;
  }

  </style>
</head>
<body bgcolor="white">
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<b><font size=+1 color="#339966">Apache::VMonitor</font></b>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
Refresh rate:&nbsp;&nbsp;
[%-
  IF rates.size;
    FOREACH item = rates;
      IF item.1;
        "<a href=\"${item.1}\">[ ${item.0} ]</a>&nbsp;&nbsp;";
      ELSE;
        "[ ${item.0} ]&nbsp;&nbsp;";
      END;
    END;
  END;
-%]
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



### apache ###

sub data_apache {
    my $self = shift;

    # XXX, how do we make it work for mp1?
    require Apache::Scoreboard;
    die "Apache::Scoreboard 2.0 or higher is wanted, " .
        "this is only version $Apache::Scoreboard::VERSION"
            unless $Apache::Scoreboard::VERSION >= 2.0;

    my $data;

    my $image = Apache::Scoreboard->image($self->{r}->pool);

    # init the stats hash
    my %total = map {$_ => 0} qw(size real max_shared);

    # calculate the max_length of the process - note that we cannot
    # just make this field "%6s" because of the HTML with hyperlink
    # that has to be stuffed in.
    my $max_pid_len = 0;
    for (my $parent_score = $image->parent_score;
         $parent_score;
         $parent_score = $parent_score->next) {
        my $pid = $parent_score->pid;
        warn "pid = $pid\n";
        $max_pid_len = length $pid if length $pid  > $max_pid_len;

        for (my $worker_score = $parent_score->worker_score;
             $worker_score;
             $worker_score = $parent_score->next_live_worker_score($worker_score)
            ) {
            my $tid = ${ $worker_score->tid };
            warn "worker tid = $tid\n";
            my $length = length $tid;
            $max_pid_len = length $tid if length $tid  > $max_pid_len;
        }
    }

    my %cols = (
                     # WIDTH       # LABEL                   # SORT
        pid     => [$max_pid_len, 'PID'                     , 'd'],
        mode    => [           1, 'M'                       , 's'],
        elapsed => [           7, 'Elapsed'                 , 'd'],
        lastreq => [           7, 'LastReq'                 , 'd'],
        served  => [           4, 'Srvd'                    , 'd'],
        size    => [           5, 'Size'                    , 'd'],
        share   => [           5, 'Share'                   , 'd'],
        vsize   => [           5, 'VSize'                   , 'd'],
        rss     => [           5, 'Rss'                     , 'd'],
        client  => [          12, 'Client'                  , 's'],
        request => [          27, 'Request (first 64 chars)', 's'],
   );

    my @cols = qw(pid mode elapsed lastreq served size share vsize rss
                  client request);

    for (@cols) {
        push @{ $cols{$_} }, fixup_url($self->{url}, 'sort_by', $cols{$_}[1]);
    }

    $data->{cols_sorted} = \@cols;
    $data->{cols}        = \%cols;

#    my $parent_format = "par: %${max_pid_len}s %1s %7s %7s %4s %5s %5s %5s %5s\n\n";
#    my $child_format  = "%3d: %${max_pid_len}s %1s %7s %7s %4s %5s %5s %5s %5s %15.15s %.64s \n";	
#    my %data = ();

#    my $i=-1;
#    my $j=-1;
#    for (my $parent_score = $image->parent_score;
#         $parent_score;
#         $parent_score = $parent_score->next) {

##        my $pid = ($i==-1) ? getppid() : ($parent_score ? $parent_score->pid : undef);
#        my $pid = $parent_score->pid;
#        last unless $pid;
#        my $proc_mem  = $gtop->proc_mem($pid);
#        my $size      = $proc_mem->size($pid);

#        # workarond for Apache::Scoreboard (or underlying C code) bug,
#        # it reports processes that are already dead. So we easily
#        # skip them, since their size is zero!
#        next unless $size;

#        my $share = $proc_mem->share($pid);
#        my $vsize = $proc_mem->vsize($pid);
#        my $rss   = $proc_mem->rss($pid);

#        #  total http size update
#        $total{size}  += $size;
#        $total{real}  += $size-$share;
#        $total{max_shared} = $share if $total{max_shared} < $share;


#        for (my $worker_score = $parent_score->worker_score;
#             $worker_score;
#             $worker_score = $parent_score->next_active_worker_score($worker_score)
#            ) {

#            # get absolute start and stop times in usecs since epoch
#            my ($start_sec,$start_usec_delta) = $worker_score->start_time;
#            my $start_usec = $start_sec*1000000+$start_usec_delta;
    
#            my ($stop_sec, $stop_usec_delta) =  $worker_score->stop_time;
#            my $stop_usec = $stop_sec*1000000+$stop_usec_delta;
    
#            # measure running time till now if not idle
#            my $elapsed = $stop_usec < $start_usec
#                ? Time::HiRes::tv_interval
#                    ([$start_sec,$start_usec_delta], [Time::HiRes::gettimeofday])
#                : 0;
    
#            # link the pid
#            my $length   = length $pid;
#            my $stuffing = $max_pid_len - $length;
#            my $spacing  = "&nbsp;" x $stuffing;
#            my $pid_linked = qq{$spacing<A HREF="@{[get_url(pid => $pid)]}">$pid</A>};
    
#            # handle the parent case
#            if ($j == -1) {
#                $j++;
#                printf $parent_format,
#                    $pid_linked,
#                    $worker_score->status,
#                    '',
#                    '',
#                    '',
#                    Apache::Util::size_string($size),
#                    Apache::Util::size_string($share),
#                    Apache::Util::size_string($vsize),
#                    Apache::Util::size_string($rss);
#            } else {
#                push @{ $data{$pid} }, 
#                    {
#                     pid        => $pid,
#                     pid_linked => $pid_linked,
#                     mode       => $worker_score->status,
#                     elapsed    => $elapsed,
#                     lastreq    => $worker_score->req_time,
#                     served     => $worker_score->my_access_count,
#                     size       => $size,
#                     share      => $share,
#                     vsize      => $vsize,
#                     rss        => $rss,
#                     client     => $worker_score->client,
#                     request    => $worker_score->request,
#                    };
#            }
#        } # end of for (my $i=0...
#    } # end of for (my $i=0...

#    # print the httpd processes sorted 
#    my $sort_field = $config{SORT_BY} || 'size';
#    $sort_field = 'size' unless $cols{$sort_field};
#    my $count = 0;

##use Data::Dumper;
##warn "sort field: $sort_field";
##warn Dumper $cols{$sort_field};
#    # sort strings alphabetically, numbers numerically reversed
##    for ($cols{$sort_field}[2] eq 's' 
##         ? (sort {$data{$a}{$sort_field} cmp $data{$b}{$sort_field} } keys %data)
##         : (sort {$data{$b}{$sort_field} <=> $data{$a}{$sort_field} } keys %data)
##        ) {
#    for my $pid (keys %data) {
#        for my $rec (@{ $data{$pid} }) {

#	# setting visual alert for cur_req_elapsed_run hardcoded to 15
#	# secs so far
#        my $elapsed = $rec->{elapsed};
#        $elapsed = $elapsed > 15
#            ? blinking(sprintf qq{<B><FONT color="red">%7s</FONT></B>},
#                       format_time($elapsed))
#            : format_time($elapsed);

#	# setting visual alert for last_req_len hardcoded to 15secs so
#	# far
#        my $req_time = $rec->{lastreq}/1000;
#        $req_time = $req_time > 15
#            ? sprintf qq{<B><FONT color="red">%7s</FONT></B>},
#                format_time($req_time)
#            : format_time($req_time);

#        # print sorted
#	printf $child_format,
#            ++$count,
#            $rec->{pid_linked},
#            $rec->{mode},
#            $elapsed,
#            $req_time,
#            format_counts($rec->{served}),
#            Apache::Util::size_string($rec->{size}),
#            Apache::Util::size_string($rec->{share}),
#            Apache::Util::size_string($rec->{vsize}),
#            Apache::Util::size_string($rec->{rss}),
#            $rec->{client},
#            $rec->{request};
#    }
#    }

#    ### Summary of memory usage
#    printf "\n<B>Total:     %5dK (%s) size, %6dK (%s) approx real size (-shared)</B>\n",
#      $total{size}/1000,
#      Apache::Util::size_string($total{size}), 
#      ($total{real} + $total{max_shared})/1000,
#      Apache::Util::size_string($total{real} + $total{max_shared});

#    #  Note how do I calculate the approximate real usage of the memory:
#    #  1. For each process sum up the difference between shared and system
#    #  memory 2. Now if we add the share size of the process with maximum
#    #  shared memory, we will get all the memory that actually is being
#    #  used by all httpd processes but the parent process.

#    print "<HR>";


    return $data;
}



sub tmpl_apache {

    return \ <<'EOT';
<hr>
<pre>
[%-


  # header
  space = "&nbsp;";
  "<b>";
  FOR key = cols_sorted;
      col = cols.$key;
      times = col.0 - col.1.length;
      spacing = space.repeat(times);
      "$spacing<a href=\"${col.3}\">${col.1}</a>";
  END;
  "</b>";

  "<div class=\"even_item\">foo bar</div>";
  "<div class=\"odd_item\">foo bar</div>";
  "<div class=\"even_item\">foo bar</div>";


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

    my @records = qw(devname mountdir type);
    my($mountlist, $entries) = $gtop->mountlist(1);

    my $fs_number = $mountlist->number;
    my %len = map { $_ => 0 } @records;
    my @items = ();
    for (my $i=0; $i < $fs_number; $i++) {
        push @items, {
            map {
                my $val = $entries->$_($i);
                $len{$_} = length $val if length $val > $len{$_};
                $_ => $val;
            } @records
        };
    }

    # sort by device name
    @items = sort { $a->{devname} cmp $b->{devname} } @items;
    return {
        items => \@items,
        len   => \%len,
    };
}

sub tmpl_mount {

    return \ <<'EOT';
<hr>
<pre>
[%-
  header = "%-${len.devname}s   %-${len.mountdir}s   %-${len.type}s";
  USE format_header = format("<b>$header</b>\n");

  format_header("DEVICE", "MOUNTED ON", "FS TYPE");

  USE format_item =
      format("$header\n");
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

