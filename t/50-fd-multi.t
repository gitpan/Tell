#!perl -w
use strict;
use warnings;
use Test::More tests => 2;

my $out;
use Tell qw/:all/, {-bullets => 0,
                    -fh      => \$out,
                    -width   => 50};

my $logfnam = "/tmp/50-fd-test-$$.tmp";
open (LOG, ">$logfnam")
  or die "*** Could not create log file: $!\n";

{ tell "This goes to the string at level 0";
  { tell "And this to the string at level 1";
    { tell *LOG, "The log file at level 0";
      { tell "To string at level 2";
        { tell *LOG, "to log file at level 1";
        }
      }
    }
  }
}
close LOG;

is($out, "This goes to the string at level 0...\n".
         "  And this to the string at level 1...\n".
         "    To string at level 2.................. [DONE]\n".
         "  And this to the string at level 1....... [DONE]\n".
         "This goes to the string at level 0........ [DONE]\n",  "String output check");

my $log;
open (RLOG, $logfnam)
    or die "*** Could not read back the log file: $!\n";
while (<RLOG>) {$log .= $_}
close RLOG;
unlink $logfnam;
is($log, "The log file at level 0...\n".
         "  to log file at level 1.................. [DONE]\n".
         "The log file at level 0................... [DONE]\n",  "Log file output check");