#!perl -w
use strict;
use warnings;
use Tell qw/:all/, {-step => 3};

tell "Updating Configuration";

  tell "System parameter updates";
    tell "CLOCK_UTC";
    tell_ok;
    tell "NTP Servers";
    tell_ok;
    tell "DNS Servers";
    tell_warn;
  tell_done;

  tell "Application parameter settings";
    tell "Administrative email contacts";
    tell_error;
    tell "Hop server settings";
    tell_ok;
  tell_done;

  tell "Web server primary page";
  tell_ok;

  tell "Updating crontab jobs";
  tell_ok;

  tell "Restarting web server";
  tell_done;

exit 0;
