use strict;
use warnings FATAL => 'all';

use Apache::Test;
use Apache::TestUtil;
use Apache::TestRequest;

plan tests => 2;

my $location = "/vmonitor";
my $str = GET_BODY $location;

ok $str;

ok t_cmp(qr/Apache::VMonitor/, $str);

