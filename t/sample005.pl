#!perl -w
use strict;
use warnings;
use Tell qw/:all/, {-color => 1,
                    -fh => *STDERR};

{   tell "Watch the percentage climb";
    for (1..10) {
        tell_over " " . ($_ * 10) . "%";
        select(undef,undef,undef, 0.100);
    }
    tell_over ""; # erase the percentage
}

{   tell "Watch the dots move";
    for (1..40) {
        tell_prog $_%10?q:.::':'; # just being difficult...ha!
        select(undef,undef,undef, 0.100);
    }
}

{   tell "Here's a spinner";
    my @spin = qw{| / - \\ | / - \\};
    for (1..64) {
        tell_over $spin[$_ % @spin];
        select(undef,undef,undef, 0.125);
    }
    tell_over;  # remove spinner
}

{   tell "Zig zags on parade";
    for (1..200) {
        tell_prog $_%2? '/' : '\\';
        select(undef,undef,undef, 0.025);
    }
}


{   tell "Making progress";
    for (1..10) {
        tell_over " $_/10";
        select(undef,undef,undef, 0.100);
    }
}

{   tell "Engines on";
    for (reverse(1..5)) {
        tell_prog " $_ ";
        select(undef,undef,undef, 1.000);
    }
    tell_done "Gone!";
}

exit 0;
