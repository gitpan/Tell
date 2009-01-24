#!perl -w
use strict;
use warnings;
use Test::More tests => 3;

my $out;
use Tell qw/:all/, {-bullets => 0,
                    -fh      => \$out,
                    -width   => 45};

$out = q{};
{ tell "Uzovating";
  tell_none; }
is($out, "Uzovating...\n",                                  "One level, quiet");

$out = q{};
{ tell "Subdoulation of quantifoobar";
  { tell "Morgozider"; tell_none; }
 }
is($out, "Subdoulation of quantifoobar...\n".
         "  Morgozider...\n".
         "Subdoulation of quantifoobar......... [DONE]\n",  "Two levels, inner quiet");

$out = q{};
{ tell "Subdoulation of quantifoobar";
  { tell "Morgozider" }
  tell_none; }
is($out, "Subdoulation of quantifoobar...\n".
         "  Morgozider......................... [DONE]\n",  "Two levels, outer quiet");
