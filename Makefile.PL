use strict;
use warnings FATAL => 'all';

require 5.006;

use Apache2;
use mod_perl 1.99;

use ModPerl::MM ();

# enable 'make test|clean'
use Apache::TestMM qw(test clean);

# prerequisites
my %require = (
   "Apache::Test"       => "1.0",
   "GTop"               => "0.09",
   "mod_perl"           => "1.98",
   "Apache::Scoreboard" => "2.0",
   "Time::HiRes"        => "1.19",
   'Template'           => "2.56",
);

prereqs();

my @scripts = qw(t/TEST);

# accept the configs from command line
Apache::TestMM::filter_args();
Apache::TestMM::generate_script('t/TEST');

ModPerl::MM::WriteMakefile(
    NAME         => 'Apache::VMonitor',
    VERSION_FROM => 'lib/Apache/VMonitor.pm',
    PREREQ_PM    => \%require,
    dist         => {
        PREOP => 'pod2text VMonitor.pm > $(DISTVNAME)/README', 
    },
    clean        => {
        FILES => "@{ clean_files() }",
    }
);

sub clean_files {
    return \@scripts;
}

sub prereqs {
    for (keys %require) {
        chk_version($_ => $require{$_}) or
            warn "\n*** For Apache::VMonitor to work you require version " .
                "$require{$_}, or later, of $_.pm from CPAN\n";
    }
}

sub chk_version {
  my ($pkg, $wanted) = @_;

  no strict 'refs'; 
  local $| = 1;

  print "Checking for $pkg...";

  eval { (my ($p) = $pkg . ".pm") =~ s#::#/#g; require $p;};
  print("not ok\n$@"), return if $@;

  my $vstr = ${"${pkg}::VERSION"} ? "found v" . ${"${pkg}::VERSION"}
                                  : "not found";
  my $vnum = ${"${pkg}::VERSION"} || 0;

  print $vnum >= $wanted ? "ok\n" : " " . $vstr . "\n";

  $vnum >= $wanted;

}

__END__
