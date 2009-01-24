#!perl -w
use strict;
use warnings;
use Test::More tests => 25;

my $out;
use Tell qw/:all/, {-bullets => 0,
                    -fh      => \$out,
                    -width   => 40};

# Scope nesting
$out = q{};
{ tell "Subdoulation of quantifoobar"; tell_done;}
is($out, "Subdoulation of quantifoobar.... [DONE]\n",
            "One level closed by unspecified DONE");

$out = q{};
{ tell "Subdoulation of quantifoobar"; }
is($out, "Subdoulation of quantifoobar.... [DONE]\n",
            "One level autoclosed");

foreach my $sev (keys %Tell::SEVLEV, "Blah") {
    $out = q{};
    { tell "Subdoulation of quantifoobar"; tell_done $sev;}
    is($out, "Subdoulation of quantifoobar.... [$sev]\n",
            "One level closed by $sev");
}

$out = q{};
{ tell "Subdoulation of quantifoobar";
    { tell "Morgozider" }}
is($out, "Subdoulation of quantifoobar...\n".
         "  Morgozider.................... [DONE]\n".
         "Subdoulation of quantifoobar.... [DONE]\n",
            "Two levels autoclosed");

$out = q{};
{ tell "Subdoulation of quantifoobar";
    { tell "Morgozider"; tell_ok }
  tell_done;}
is($out, "Subdoulation of quantifoobar...\n".
         "  Morgozider.................... [OK]\n".
         "Subdoulation of quantifoobar.... [DONE]\n",
            "Two levels closed");

$out = q{};
{ tell "Subdoulation of quantifoobar";
    { tell "Morgozider"; tell_warn }
    { tell "Nimrodicator"; tell_ok }
    { tell "Obfuscator of vanilse"; tell_notry }
  tell_done;}
is($out, "Subdoulation of quantifoobar...\n".
         "  Morgozider.................... [WARN]\n".
         "  Nimrodicator.................. [OK]\n".
         "  Obfuscator of vanilse......... [NOTRY]\n".
         "Subdoulation of quantifoobar.... [DONE]\n",
            "Two levels, inner series, closed");

$out = q{};
{ tell "Subdoulation of quantifoobar";
    { tell "Morgozider";
        { tell "Frimrodding the quark" }
        { tell "Eouing our zyxxpth"; tell_crit }
      tell_warn; }
    { tell "Nimrodicator"; tell_ok }
    { tell "Obfuscator of vanilse"; tell_notry }
  tell_done;}
is($out, "Subdoulation of quantifoobar...\n".
         "  Morgozider...\n".
         "    Frimrodding the quark....... [DONE]\n".
         "    Eouing our zyxxpth.......... [CRIT]\n".
         "  Morgozider.................... [WARN]\n".
         "  Nimrodicator.................. [OK]\n".
         "  Obfuscator of vanilse......... [NOTRY]\n".
         "Subdoulation of quantifoobar.... [DONE]\n",
            "Three levels, mixed");

# Line wrapping
$out = q{};
{ tell "Wrappification of superlinear magmafied translengthed task strings";}
is($out, "Wrappification of superlinear\n".
         "magmafied translengthed task\n".
         "strings......................... [DONE]\n",
            "One level wrapped");

$out = q{};
{ tell "Wrappification of superlinear magmafied translengthed task strings";
  { tell "Short level 1 line"; }
}
is($out, "Wrappification of superlinear\n".
         "magmafied translengthed task\n".
         "strings...\n".
         "  Short level 1 line............ [DONE]\n".
         "Wrappification of superlinear\n".
         "magmafied translengthed task\n".
         "strings......................... [DONE]\n",
            "Two levels, outer wrapped");

$out = q{};
{ tell "Short level 0";
  { tell "Wrappification of superlinear magmafied translengthed task strings"; }
}
is($out, "Short level 0...\n".
         "  Wrappification of superlinea\n".
         "  r magmafied translengthed\n".
         "  task strings.................. [DONE]\n".
         "Short level 0................... [DONE]\n",
            "Two levels, inner wrapped");

$out = q{};
{ tell "Spatial folding process is underway at this very moment";
  { tell "Wrappification of superlinear magmafied translengthed task strings"; }
}
is($out, "Spatial folding process is\n".
         "underway at this very moment...\n".
         "  Wrappification of superlinea\n".
         "  r magmafied translengthed\n".
         "  task strings.................. [DONE]\n".
         "Spatial folding process is\n".
         "underway at this very moment.... [DONE]\n",
            "Two levels, both wrapped");
