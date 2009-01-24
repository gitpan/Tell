#!perl -w
use strict;
use warnings;
use Tell qw/:all/, {-bullets => ' * ',
                    -color => 1,
                    -fh => *STDERR};

tell "This should have color, bullets, and go to STDERR";
exit 0;
