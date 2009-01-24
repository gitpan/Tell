#!perl -w
use strict;
use warnings;
use Test::More tests => 2;

my $out;
use Tell qw/:all/, {-bullets => 0,
                    -fh      => \$out,
                    -width   => 35};

# This script tests if you tell to a FD that's the same as
#  the base object 0's FD, that you end up using the base
#  object 0.

{ tell "Level 0";
  { tell \$out, "Level 1";
    { tell "Level 2";
      { tell \$out, "Level 3";
        { tell "Level 4";
        }
      }
    }
  }
}
is($out, "Level 0...\n".
         "  Level 1...\n".
         "    Level 2...\n".
         "      Level 3...\n".
         "        Level 4............ [DONE]\n".
         "      Level 3.............. [DONE]\n".
         "    Level 2................ [DONE]\n".
         "  Level 1.................. [DONE]\n".
         "Level 0.................... [DONE]\n",  "String outputs consolidate to base 0");

$out = q{};
{ tell "Level 0";
  tell_text "Explanation 0";
  { tell \$out, "Level 1";
    tell_text \$out, "Explanation 1";
    { tell "Level 2";
      tell_text "Explanation 2";
      { tell \$out, "Level 3";
        tell_text \$out, "Explanation 3";
        { tell "Level 4";
          tell_text "Explanation 4";
        }
      }
    }
  }
}
is($out, "Level 0...\n".
         "  Explanation 0\n".
         "  Level 1...\n".
         "    Explanation 1\n".
         "    Level 2...\n".
         "      Explanation 2\n".
         "      Level 3...\n".
         "        Explanation 3\n".
         "        Level 4...\n".
         "          Explanation 4\n".
         "        Level 4............ [DONE]\n".
         "      Level 3.............. [DONE]\n".
         "    Level 2................ [DONE]\n".
         "  Level 1.................. [DONE]\n".
         "Level 0.................... [DONE]\n",  "Added text consolidates to base 0");

