#!perl -w
use strict;
use warnings;
use Test::More tests => 3;

my $out;
use Tell qw/:all/, {-bullets => 0,
                    -fh      => \$out,
                    -width   => 35};

$out = q{};
{ tell "Frimrodding quickly" }
is($out, "Frimrodding quickly........ [DONE]\n",  "Default autoclose severity");

Tell::set(closestat => 'OK');
$out = q{};
{ tell "Frimrodding quickly" }
is($out, "Frimrodding quickly........ [OK]\n",    "Set autoclose severity");

Tell::set(closestat => 'Yoyum');
$out = q{};
{ tell "Frimrodding quickly" }
is($out, "Frimrodding quickly........ [Yoyum]\n", "Set autoclose non-standard severity");
