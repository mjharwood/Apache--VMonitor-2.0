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

use constant MP2 => eval { require mod_perl; $mod_perl::VERSION > 1.99 };

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

   # sorting
   apache_sort_by        => 'size',
   apache_sort_by_ascend => 0,
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
        pid   => $pid,
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

    if ($self->{pid}) {
        push @items, qw(apache_single);
    }
    else {
        # XXX: fixme
        my @sects = qw(system apache fs_usage mount);
        $cfg->{$_} && push @items, $_ for (@sects);
        push @items, qw(nav_bar verbose);
    }

    push @items, qw(end_html);

    for (@items) {
        my $tmpl_sub = $self->can("tmpl_$_");
        next unless $tmpl_sub;
        my $data_sub = $self->can("data_$_");
        my $data = $data_sub ? $self->$data_sub : {};
        $tt->process($self->$tmpl_sub, $data) or warn $tt->error();
    }
}

# XXX: could make it a method
#
# my $newurl = fixup_url($url, $key => $val)
# my $newurl = fixup_url($url, $key => $val, $key2 => $val2)
# update key/val of the query and return
############
sub fixup_url {
    my($url, %pairs) = @_;

    my $new_url = $url;
    while (my($k, $v) = each %pairs) {
        $new_url =~ s/$k=([^&]+)?/$k=$v/;
    }

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
    border: 0px;
    padding: 0px 0px 0px 0px;
    margin: 5px 5px 5px 5px;
    font-size: 0.8em;
  }
  p.hdr {
    background-color: #ddd;
    border: 2px outset;
    padding: 3px;
    width: 99%;
  }
  span.item_even {
    background-color: #dddddd;
    color: #000000;
  }
  span.item_odd {
    background-color: #ffffff;
    color: #000000;
  }
  span.normal {
    color: #000000;
  }
  span.warn {
    color: #ff99cc;
  }
  span.alert {
    color: #ff0000;
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

    require Time::HiRes;

    # XXX, how do we make it work for mp1?
    require Apache::Scoreboard;
    die "Apache::Scoreboard 2.0 or higher is wanted, " .
        "this is only version $Apache::Scoreboard::VERSION"
            unless $Apache::Scoreboard::VERSION >= 2.0;

    my $image = Apache::Scoreboard->image($self->{r}->pool);

    # total memory usage stats
    my %mem_total = map { $_ => 0 } qw(size real max_shared);

#        for (my $worker_score = $parent_score->worker_score;
#             $worker_score;
#             $worker_score = $parent_score->next_live_worker_score($worker_score)
#            ) {
#            my $tid = ${ $worker_score->tid };
#            warn "worker tid = $tid\n";
#            my $length = length $tid;
#            $max_pid_len = length $tid if length $tid  > $max_pid_len;
#        }

    my %cols = (
                 # WIDTH # LABEL                   # SORT
        pid     => [ 3, 'PID'                     , 'd'],
        mode    => [ 1, 'M'                       , 's'],
        elapsed => [ 7, 'Elapsed'                 , 'd'],
        lastreq => [ 7, 'LastReq'                 , 'd'],
        served  => [ 4, 'Srvd'                    , 'd'],
        size    => [ 5, 'Size'                    , 'd'],
        share   => [ 5, 'Share'                   , 'd'],
        vsize   => [ 5, 'VSize'                   , 'd'],
        rss     => [ 5, 'Rss'                     , 'd'],
        client  => [15, 'Client'                  , 's'],
        request => [27, 'Request (first 64 chars)', 's'],
    );

    my @cols_sorted = qw(pid mode elapsed lastreq served size share
                         vsize rss client request);

    my $sort_field = lc($cfg{apache_sort_by}) || 'size';
    $sort_field = 'size' unless $cols{$sort_field};
    my $sort_ascend = $Apache::VMonitor::Config{apache_sort_by_ascend} || 0;
    #warn "SORT field: $sort_field, ascending $sort_ascend\n";

    for (@cols_sorted) {
        if ($sort_field eq $_) {
            $sort_ascend = $cfg{apache_sort_by_ascend} + 1;
            $sort_ascend %= 2; # reverse sorting order
        }

        # replace sort type, with link to sort
        $cols{$_}[2] = fixup_url($self->{url},
                                 apache_sort_by        => $cols{$_}[1],
                                 apache_sort_by_ascend => $sort_ascend);
    }

    my %data = ();

    for (my $parent_score = $image->parent_score;
         $parent_score;
         $parent_score = $parent_score->next) {

        my $pid = $parent_score->pid;
        next unless $pid;

        my $mem = $self->pid2mem($pid, \%mem_total);
        next unless $mem->{size};

        my $worker_score = $parent_score->worker_score;
        my $record = $self->score2record($pid, $parent_score, $worker_score);
        %$record = %$mem; # append proc data

        $data{ $record->{pid} } = $record;
    }

#        for (my $worker_score = $parent_score->worker_score;
#             $worker_score;
#             $worker_score = $parent_score->next_active_worker_score($worker_score)
#            ) {
#            use Data::Dumper;
#            warn "client: ". $worker_score->client . "\n";
#            warn Dumper 
#                $self->score2record($parent_score, $worker_score);
#        }

    # handle the parent case
    my $ppid = getppid();
warn "ppid: $ppid\n";
    my $pmem = $self->pid2mem($ppid, \%mem_total);
    my $prec = {
        count     => 0,
        pid       => $ppid,
        pid_link  => fixup_url($self->{url}, pid => $ppid),
        fsize     => size_string($pmem->{size}),
        fshare    => size_string($pmem->{share}),
        fvsize    => size_string($pmem->{vsize}),
        frss      => size_string($pmem->{rss}),
    };

    my @records = ();
    my $count = 0;
    my $max_client_len = 9;
    my $max_pid_len = 0;

    # sort strings alphabetically, numbers numerically reversed
    my $sort_sub;
    if ($cols{$sort_field}[2] eq 's') {
        $sort_sub = $sort_ascend
            ? sub { $data{$a}{$sort_field} cmp $data{$b}{$sort_field} }
            : sub { $data{$b}{$sort_field} cmp $data{$a}{$sort_field} };
    }
    else {
        $sort_sub = $sort_ascend
            ? sub { $data{$a}{$sort_field} <=> $data{$b}{$sort_field} }
            : sub { $data{$b}{$sort_field} <=> $data{$a}{$sort_field} };
    }

    for my $pid (sort $sort_sub keys %data) {

        my $rec = $data{$pid};
        my $lastreq = $rec->{lastreq}/1000;

        # print sorted
        push @records, {
            count     => ++$count,
            pid       => $rec->{pid},
            pid_link  => $rec->{pid_link},
            mode      => $rec->{mode},
            elapsed   => $rec->{elapsed},
            felapsed  => format_time($rec->{elapsed}),
            lastreq   => $lastreq,
            flastreq  => format_time($lastreq),
            fserved   => format_counts($rec->{served}),
            fsize     => size_string($rec->{size}),
            fshare    => size_string($rec->{share}),
            fvsize    => size_string($rec->{vsize}),
            frss      => size_string($rec->{rss}),
            client    => $rec->{client},
            request   => $rec->{request},
        };
        $max_client_len = length $rec->{client}
            if length $rec->{client}  > $max_client_len;
        $max_pid_len = length $pid
            if length $pid  > $max_pid_len;
    }

    $cols{client}[0] = $max_client_len;
    $cols{pid}[0]    = $max_pid_len;

    # Summary of memory usage
    #  Note how do I calculate the approximate real usage of the memory:
    #  1. For each process sum up the difference between shared and system
    #  memory 2. Now if we add the share size of the process with maximum
    #  shared memory, we will get all the memory that actually is being
    #  used by all httpd processes but the parent process.
    my $total = {
        size    => $mem_total{size}/1000,
        fsize   => size_string($mem_total{size}),
        shared  => ($mem_total{real} + $mem_total{max_shared})/1000,
        fshared => size_string($mem_total{real} + $mem_total{max_shared}),

    };

    return {
        total       => $total,
        prec        => $prec,
        records     => \@records,
        cols_sorted => \@cols_sorted,
        cols        => \%cols,
    };
}

sub pid2mem {
    my($self, $pid, $total) = @_;

    my $proc_mem = $gtop->proc_mem($pid);
    my $size  = $proc_mem ? $proc_mem->size($pid) : 0;
    # dead process?
    return {} unless $size;

    my $share = $proc_mem->share($pid);
    my $vsize = $proc_mem->vsize($pid);
    my $rss   = $proc_mem->rss($pid);

    #  total http size update
    if ($total) {
        $total->{size}  += $size;
        $total->{real}  += $size-$share;
        $total->{max_shared} = $share if $total->{max_shared} < $share;
    }

    return {
        size       => $size,
        share      => $share,
        vsize      => $vsize,
        rss        => $rss,
        pid        => $pid,
        pid_link   => fixup_url($self->{url}, pid => $pid),
    };
}

sub score2record {
    my($self, $pid, $parent_score, $worker_score) = @_;

    # get absolute start and stop times in usecs since epoch
    my ($start_sec, $start_usec_delta) = $worker_score->start_time;
    my $start_usec = $start_sec * 1000000 + $start_usec_delta;

    my($stop_sec, $stop_usec_delta) = $worker_score->stop_time;
    my $stop_usec = $stop_sec * 1000000 + $stop_usec_delta;
    #warn "time: $start_sec, $start_usec_delta, $stop_sec, $stop_usec_delta\n";

    # measure running time till now if not idle
    my $elapsed = $stop_usec < $start_usec
        ? Time::HiRes::tv_interval([$start_sec, $start_usec_delta],
                                   [Time::HiRes::gettimeofday()])
        : 0;

    return {
        pid        => $pid,
        pid_link   => fixup_url($self->{url}, pid => $pid),
        mode       => $worker_score->status,
        elapsed    => $elapsed,
        lastreq    => $worker_score->req_time,
        served     => $worker_score->my_access_count,
        client     => $worker_score->client,
        request    => $worker_score->request,
    };
}

sub tmpl_apache {

    return \ <<'EOT';
<hr>
<pre>
[%-
  USE HTML;

  # header
  space = "&nbsp;";
  "<b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
  width     = 0;
  label     = 1;
  sort_link = 2;
  FOR key = cols_sorted;
      col = cols.$key;
      times = col.$width - col.$label.length;
      spacing = times > 0 ? space.repeat(times) : "";
      "$spacing<a href=\"${col.$sort_link}\">${col.$label}</a>$space";
  END;
  "</b>\n";


  # records
  max_pid_len = cols.pid.$width;
  max_client_len = cols.client.$width;

  # parent rec
  spacing_len = cols.mode.$width + cols.elapsed.$width + cols.lastreq.$width + cols.served.$width + 8;
  USE format_parent =
      format("par: %s %${spacing_len}s %5s %5s %5s %5s\n");

  times = max_pid_len - prec.pid.length;
  spacing = times > 0 ? space.repeat(times) : "";
  pid_link = "$spacing<a href=\"${prec.pid_link}\">${prec.pid}</a>";

  format_parent(pid_link, space, prec.fsize, prec.fshare, prec.fvsize, prec.frss);

  USE format_child =
      format("%3d: %s %1s %s %s %4s %5s %5s %5s %5s %${max_client_len}.${max_client_len}s %.64s");
  FOR rec = records;

      # alert on workers that are still at work for a single request
      # for more than 15 secs
      elapsed_class = rec.elapsed > 15 ? "alert" : "normal";
      rec.felapsed = "<span class=\"$elapsed_class\">${rec.felapsed}</span>";

      # alert on workers that worked for a single request for more
      # than 15 secs
      lastreq_class = rec.lastreq > 15 ? "alert" : "normal";
      rec.flastreq = "<span class=\"$lastreq_class\">${rec.flastreq}</span>";

      # escape HTML in request URI to prevent cross-site scripting attack
      rec.frequest = HTML.escape(rec.request);

      # pid linked
      times = max_pid_len - rec.pid.length;
      spacing = times > 0 ? space.repeat(times) : "";
      pid_link = "$spacing<a href=\"${rec.pid_link}\">${rec.pid}</a>";

      item_class = loop.count % 2 ? "item_even" : "item_odd";
      "<span class=\"$item_class\">";
      format_child(rec.count, pid_link, rec.mode, rec.felapsed, rec.flastreq, rec.fserved, rec.fsize, rec.fshare, rec.fvsize, rec.frss, rec.client, rec.frequest);
      "</span>\n";
  END;

  # total apache proc memory usage
  USE format_total =
      format("\n<b>Total:     %5dK (%s) size, %6dK (%s) approx real size (-shared)</b>\n");
      format_total(total.size, total.fsize, total.shared, total.fshared);

-%]
</pre>
EOT

}


### apache_single ###

sub data_apache_single {
    my $self = shift;

    require Time::HiRes;

    # XXX, how do we make it work for mp1?
    require Apache::Scoreboard;
    die "Apache::Scoreboard 2.0 or higher is wanted, " .
        "this is only version $Apache::Scoreboard::VERSION"
            unless $Apache::Scoreboard::VERSION >= 2.0;

    my $pid = $self->{pid};
    my $data;

    my($proclist, $entries) = $gtop->proclist;

    # get the proc command name
    my $cmd = '';
    for my $proc_pid ( @$entries ){
        $cmd = $gtop->proc_state($pid)->cmd, last if $pid == $proc_pid;
    }

    $data->{link_back} = fixup_url($self->{url}, pid => 0);
    $data->{pid} = $pid;
    $data->{cmd} = $cmd;
  
    #use Data::Dumper;
    #warn Dumper $data;

    ### memory usage
    my $mem = $self->pid2mem($pid);
    # the process might be dead already by the time you click on it.
    unless ($mem->{size}) {
        $data->{proc_is_dead} = 1;
        return $data;
    }
    $data->{mem} = {
            size   => $mem->{size},
            share  => $mem->{share},
            vsize  => $mem->{vsize},
            rss    => $mem->{rss},
            fsize  => size_string($mem->{size}),
            fshare => size_string($mem->{share}),
            fvsize => size_string($mem->{vsize}),
            frss   => size_string($mem->{rss}),
    };

    if (my $parent_score = $self->pid2parent_score($pid)) {
        my $worker_score = $parent_score->worker_score;
        my $rec = $self->score2record($pid, $parent_score, $worker_score);
        my $lastreq = $rec->{lastreq}/1000;
        $data->{rec} = {
            is_httpd_proc => 1,
            proc_type => ($pid == getppid ? "Parent" : "Child"),
            mode      => $rec->{mode},
            elapsed   => $rec->{elapsed},
            felapsed  => format_time($rec->{elapsed}),
            lastreq   => $lastreq,
            flastreq  => format_time($lastreq),
            fserved   => format_counts($rec->{served}),
            client    => $rec->{client},
            request   => $rec->{request},
        };

    }

    # memory segments usage
    my $proc_segment = $gtop->proc_segment($pid);
    no strict 'refs';
    for (qw(text_rss shlib_rss data_rss stack_rss)) {
        my $size = $proc_segment->$_($pid);
        $data->{mem_segm}->{$_} = $size;
        $data->{mem_segm}->{"f$_"} = size_string($size);
    }


    # memory maps
    my($procmap, $maps) = $gtop->proc_map($pid);
    my $number = $procmap->number;
    my %libpaths = ();

    my @maps = ();
    for (my $i = 0; $i < $number; $i++) {
        my $filename = $maps->filename($i) || "-";
        $libpaths{$filename}++;
        my $device = $maps->device($i);
        push @maps, {
                start        => $maps->start($i),
                end          => $maps->end($i),
                offset       => $maps->offset($i),
                device_major => (($device >> 8) & 255),
                device_minor => ($device & 255),
                inode        => $maps->inode($i),
                perm         => $maps->perm_string($i),
                filename     => $filename,
            };

    }
    $data->{mem_maps} = {
        records  => \@maps,
        ptr_size => (length(pack("p", 0)) == 8 ? 16 : 8),
    };

    # loaded shared libs sizes
    my %libsizes = map { $_  => -s $_ } grep { -e $_} grep !/^-$/, keys %libpaths;

    my @lib_sizes = ();
    my $total = 0;
    for (sort { $libsizes{$b} <=> $libsizes{$a} } keys %libsizes) {
        $total +=  $libsizes{$_};
        push @lib_sizes, {
            size     => $libsizes{$_},
            fsize    => size_string($libsizes{$_}),
            filename => $_,
        };
    }

    $data->{libs} = {
        records  => \@lib_sizes,
        total    => $total,
        ftotal   => size_string($total),
    };

    return $data;
}

# given the pid return the corresponding parent score object or undef
# if it's not an httpd proc.
sub pid2parent_score {
    my($self, $pid) = @_;

    if (MP2) {
        my $image = Apache::Scoreboard->image($self->{r}->pool);
        my $parent_idx = $image->parent_idx_by_pid($pid);
        return $image->parent_score($parent_idx);
    }
    else {
        # XXX: mp1 untested
        my $image = Apache::Scoreboard->image();
        my $i;
        my $is_httpd_child = 0;
        for ($i=0; $i<Apache::Constants::HARD_SERVER_LIMIT; $i++) {
            $is_httpd_child = 1, last if $pid == $image->parent($i)->pid;
        }
        $i = -1 if $pid == getppid();
        if ($is_httpd_child || $i == -1) {
            return $image->servers($i);
        }
    }
}


sub tmpl_apache_single {

    return \ <<'EOT';
<hr>
[%-

   "<p>[ <a href=\"$link_back\">Back to multiproc mode</a> ]</p>";
   IF proc_is_dead;
       "Sorry, the process $pid ($cmd) doesn't exist anymore!";
   ELSE;
       "<p><b>Extensive Status for PID $pid ($cmd)</b>&nbsp; &nbsp;</p>";
       PROCESS single_process;
   END;


-%]

[% BLOCK single_process %]
<pre>
[%-

  # PROCESS single_httpd_process IF rec.is_httpd_proc;

  "<hr><b>General process info:</b>\n";

  # memory usage
  "\n<hr><b>Memory Usage</b> (in bytes):\n\n";
  USE format_mem_item = format("%-10.10s : %10d (%s)\n");
  format_mem_item("Size",  mem.size,  mem.fsize);
  format_mem_item("Share", mem.share, mem.fshare);
  format_mem_item("VSize", mem.vsize, mem.fvsize);
  format_mem_item("RSS",   mem.rss,   mem.frss);

  # memory segments usage
  "\n<HR><B>Memory Segments Usage</B> (in bytes):\n\n";
  USE format_mem_segment_item = format("%-10.10s : %10d (%s)\n");
  format_mem_segment_item("text_rss",  mem_segm.text_rss,  mem_segm.ftext_rss);
  format_mem_segment_item("shlib_rss", mem_segm.shlib_rss, mem_segm.fshlib_rss);
  format_mem_segment_item("data_rss",  mem_segm.data_rss,  mem_segm.fdata_rss);
  format_mem_segment_item("stack_rss", mem_segm.stack_rss, mem_segm.fstack_rss);

  # memory maps
  "<hr><b>Memory Maps:</b>\n\n";
   ptr_size = mem_maps.ptr_size;
   USE format_map_header = format("<b>%${ptr_size}s-%-${ptr_size}s %${ptr_size}s  %3s:%3s %7s - %4s  - %s</b>\n");
   format_map_header("start", "end", "offset", "maj", "min", "inode", "perm", "filename");
   USE format_map_item = 
       format("%0${ptr_size}lx-%0${ptr_size}lx %0${ptr_size}lx - %02x:%02x %08lu - %4s - %s\n");
   FOR rec = mem_maps.records;
       format_map_item(rec.start, rec.end, rec.offset, rec.device_major, rec.device_minor, rec.inode, rec.perm, rec.filename);
   END;

  # loaded shared libs sizes
  "<hr><b>Loaded Libs Sizes:</b> (in bytes)\n\n";
   USE format_shared_lib = format("%10d (%s): %s\n");
   FOR rec = libs.records;
       format_shared_lib(rec.size, rec.fsize, rec.filename);
   END;
   USE format_shared_lib_total = format("\n<b>%10d (%s): %s</b>\n");
   format_shared_lib_total(libs.total, libs.ftotal, "Total");

-%]
</pre>
[% END %]

[% BLOCK single_httpd_process %]
[%-
  USE HTML;

  "<b>httpd specific info:</b>\n";

  USE format_item = format("<b>%-25s</b> : %s\n");
  format_item("Process type", rec.proc_type);


  USE format_child =
      format("%3d: %s %1s %s %s %4s %5s %5s %5s %5s %${max_client_len}.${max_client_len}s %.64s");

      # alert on workers that are still at work for a single request
      # for more than 15 secs
      elapsed_class = rec.elapsed > 15 ? "alert" : "normal";
      rec.felapsed = "<span class=\"$elapsed_class\">${rec.felapsed}</span>";

      # alert on workers that worked for a single request for more
      # than 15 secs
      lastreq_class = rec.lastreq > 15 ? "alert" : "normal";
      rec.flastreq = "<span class=\"$lastreq_class\">${rec.flastreq}</span>";

      # escape HTML in request URI to prevent cross-site scripting attack
      rec.frequest = HTML.escape(rec.request);

      # pid linked
      times = max_pid_len - rec.pid.length;
      spacing = times > 0 ? space.repeat(times) : "";
      pid_link = "$spacing<a href=\"${rec.pid_link}\">${rec.pid}</a>";

      item_class = loop.count % 2 ? "item_even" : "item_odd";
      "<span class=\"$item_class\">";
      format_child(rec.count, pid_link, rec.mode, rec.felapsed, rec.flastreq, rec.fserved,  rec.client, rec.frequest);
      "</span>\n";

-%]
[% END %]
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
        $size = "   0K";
    }
    elsif ($size == -1) {
        $size = "    -";
    }
    elsif ($size < 1024) {
        $size = "   1K";
    }
    elsif ($size < 1048576) {
        $size = sprintf "%4dK", ($size + 512) / 1024;
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

