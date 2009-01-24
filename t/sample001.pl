#!perl -w
use strict;
use warnings;
use Tell qw/:all/;

tell "Contract to build house";
build_house();
tell_done;

exit 0;

sub build_house {
    tell "Building house";
    sitework();
    shell();
    tell_text "
Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Vestibulum varius libero nec tellus. Mauris eget ipsum eget quam sodales ornare. Suspendisse nec nibh. Duis lobortis mi at augue. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas.
";
    mechanical();
    finish();
    tell_done;
}

sub sitework {
    tell;
    sleep 1;  #simulate doing something
    tell_ok;
}


sub shell {
    tell;
    foundation();
    framing();
    roofing();
    tell_ok;
}

sub foundation {
    my $tracker = tell;
    sleep 1;  #simulate doing something
    # Omit closing, will automatically be closed
}

sub framing {
    tell "Now we do the framing task, which has a really long text title that should wrap nicely in the space we give it";
    sleep 1;  #simulate doing something
    tell_warn;
}

sub roofing {
    tell;
    sleep 1;  #simulate doing something
    tell_ok;
}

sub mechanical {
    tell "The MECHANICAL task is also a lengthy one so this is a bunch of text that should also wrap";
    electrical();
    plumbing();
    hvac();
    tell_fail;
}

sub electrical {
    tell;
    sleep 1;  #simulate doing something
    tell_ok;
}

sub plumbing {
    tell;
    sleep 1;  #simulate doing something
    tell_ok;
}

sub hvac {
    tell;
    sleep 1;  #simulate doing something
    tell_ok;
}

sub finish {
    tell;
    sleep 1;  #simulate doing something
    tell_ok;
}