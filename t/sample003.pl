#!perl -w
use strict;
use warnings;
use Tell qw/:all/, {-bullets => ["* ", "+ ", "- "],
                    -color => 1,
                    -width => 70};

tell "Testing ANSI color escapes for severity levels";

foreach (qw/EMERG ALERT CRIT FAIL FATAL ERROR WARN NOTE INFO OK DEBUG NOTRY UNK/) {
    tell "This  is the $_ severity";
    tell_done($_);
}

tell_done;
exit 0;
