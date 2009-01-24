#!perl -w
use strict;
use warnings;
use Test::More tests => 9;

my $out;
use Tell qw/:all/, {-bullets => 0,
                    -fh      => \$out,
                    -width   => 40};

$out = q{};
{ tell "The progress of man"; tell_done;}
is($out, "The progress of man............. [DONE]\n",   "Prog 0: Verify width");

# Non-overwrite progress
$out = q{};
{
    tell "Begin urglation";
    is($out, "Begin urglation...",                      "Prog A: prep");
    tell_prog " 10%";
    is($out, "Begin urglation... 10%",                  "Prog A: 10%");
    tell_prog " 20%";
    is($out, "Begin urglation... 10% 20%",              "Prog A: 20%");
}
is($out, "Begin urglation... 10% 20%...... [DONE]\n",   "Prog A: done");

# Overwrite progress
$out = q{};
{
    tell "Begin orcuation";
    is($out, "Begin orcuation...",                      "Prog B: prep");
    tell_over " 10%";
    is($out, "Begin orcuation... 10%",                  "Prog B: 10%");
    tell_over " 20%";
    is($out, "Begin orcuation... 10%\b\b\b\b    \b\b\b\b 20%",
                                                        "Prog B: 20%");
}
is($out, "Begin orcuation... 10%\b\b\b\b    \b\b\b\b 20%.......... [DONE]\n",
                                                        "Prog B: done");
