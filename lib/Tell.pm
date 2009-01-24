# TODO:
#   'print' wrapper to keep track of position,
#       and POD about interaction with print
#   Make tell() use indirect object notation so it's a drop-in for print
#   Change name to Msg::Tell ?
#   Read envvars for secondary defaults, so qx() wrapping looks consistent.
#   Allow message to be paired as [open,close], or reset the msg w/tell_msg()
#   Does it make sense to do Severity level filtering?
#       i.e., don't show anything >= levelV ?
#   timestamps - maybe do in another module?
#   tell_more : another tell at the same level as the prior.
#       for example:
#           tell "yomama";
#           tell_more "yopapa";  # does not start a new context, like tell_text
#             but at upper level (or call it "yell"?)
#   Thread support

# Tell - Print messages with balance, indentation, severity, and autoclosure.
use warnings;
use strict;
use 5.006;
package Tell;

  our $VERSION = '0.0.4';
  use base qw/Exporter/;
  use Scope::Upper 0.06 qw/reap :words/;

  require Exporter;
  our @ISA = qw(Exporter);
  our @EXPORT_OK = qw/tell tell_over tell_prog tell_text tell_done tell_none
                      tell_emerg
                      tell_alert
                      tell_crit tell_fail tell_fatal
                      tell_error
                      tell_warn
                      tell_note
                      tell_info tell_ok
                      tell_debug
                      tell_notry
                      tell_unk/;
  our %EXPORT_TAGS = (all      => [@EXPORT_OK],
                      syslog   => [qw(tell tell_text
                                      tell_emerg
                                      tell_alert
                                      tell_crit
                                      tell_error
                                      tell_warn
                                      tell_note
                                      tell_info
                                      tell_debug
                                     )]);
  our %SEVLEV = (EMERG => 15,
                 ALERT => 13,
                 CRIT  => 11, FAIL => 11, FATAL => 11,
                 ERROR => 9,
                 WARN  => 7,
                 NOTE  => 6,
                 INFO  => 5, OK => 5,
                 DEBUG => 4,
                 NOTRY => 3,
                 UNK   => 2,
                 OTHER => 1
                );
  our %BASE_OBJECT = ();


sub new {
    my $proto = shift;
    my $class = ref ($proto) || $proto;  # Get the class name
    my $this  = {pos  => 0,     # Current output column number
                 progwid => 0,  # Width of last progress message emitted
                 msgs => []};   # Closing message stack
    bless ($this, $class);
    $this->set(@_);
    return $this;
}

sub base {
    my ($this) = _process_args(@_);
    return $this;
}

sub clone {
    my $this = shift;   # Object to clone
    return Tell->new(%$this,@_);
}

sub import {
    my $class = shift;

    # Yank option pairs, if any, out from the arguments
    my %opts = ();
    my @args = ();
    while (@_) {
        my $arg = shift;
        if (ref($arg) eq 'HASH') {
            %opts = (%opts, %$arg); #merge
            next;
        }
        push @args, $arg;
    }
    %opts = _clean_opts(%opts);

    # Create the default base object
    $BASE_OBJECT{0} ||= new Tell(%opts);

    # Continue exporter's work
    $class->export_to_level(1, $class, @args);
}

#
# Set attributes
#
sub set {
    my ($this, $opts, %args) = _process_args(@_);

    # Merge 'em
    %args = (%$opts, %args);

    # Cleanup args
    while (my ($key, $val) = each %args) {
        delete $args{$key};
        $key =~ s/^\s*-?(\w+)\s*/$1/;
        $args{lc $key} = $val;
    }

    # Process args
    my $deffh = select();
    no strict 'refs';
    $this->{fh}        = $args{fh}        || $this->{fh}        || \*$deffh;
    use strict 'refs';
    $this->{bullets}   = exists $args{bullets} ? $args{bullets}
                                               : exists $this->{bullets} ? $this->{bullets}
                                                                         : 0;
    $this->{closestat} = $args{closestat} || $this->{closestat} || 'DONE';
    $this->{color}     = $args{color}     || $this->{color}     || 0;
    $this->{envbase}   = $args{envbase}   || $this->{envbase}   || 'tell_fd';
    $this->{maxdepth}  = exists $args{maxdepth} ? $args{maxdepth}
                                                : defined $this->{maxdepth} ? $this->{maxdepth}
                                                                            : undef;     # Max depth to show, undef=all, 0=none, 3=just first 3 levels, etc
    $this->{step}      = exists $args{step} ? $args{step}
                                            : defined $this->{step} ? $this->{step}
                                                                    : 2;    # Indentation step level
#    $this->{timefmt}   = $args{timefmt}   || $this->{timefmt}   || undef;   # Timestamp format
    $this->{trailer}   = $args{trailer}   || $this->{trailer}   || q{.};
    $this->{width}     = $args{width}     || $this->{width}     || 80;      # set this based upon tty's display width; use Term::Size::Perl etc

    # Recompute a few things
    $this->{bullet_width} = 0;
    if (ref $this->{bullets} eq 'ARRAY') {
        foreach my $b (@{$this->{bullets}}) {
            $this->{bullet_width} = length($b)
                if length($b) > $this->{bullet_width};
        }
    }
    elsif ($this->{bullets}) {
        $this->{bullet_width} = length($this->{bullets});
    }
}

#
# Tell a message, starting a new level
#
sub tell {
    my ($this, $opts, @args) = _process_args(@_);
    my $jn = defined $,? $, : q{};
    my $msg = join $jn, @args;
    if (!@args) {
        # Use our caller's subroutine name as the message
        (undef,undef,undef,$msg) = caller(1);
        $msg =~ s{^main::}{};
    }

    #  Tied closure:
    #   If we're returning into a list context,
    #   then we're tying closure to the scope of the caller's list elements.
    if (wantarray) {
        return (Tell::TiedClosure->new($this,$msg));
    }

    # Store context
    push @{$this->{msgs}}, $msg;
    my $level = $ENV{$this->_envvar()}++ || 0;

    # Setup the scope reaper for autoclosure
    reap sub {$this->tell_done($this->{closestat},$level)} => SCOPE(1);

    # Filtering by level?
    return 1
        if defined($this->{maxdepth}) && $level >= $this->{maxdepth};

    # Start back at the left
    my $s = 1;
    $s = $this->_emit("\n")
         if $this->{pos};
    return $s unless $s;
    $this->{pos} = 0;
    $this->{progwid} = 0;

    # Level adjust?
    $level += $opts->{adjust_level}
        if $opts->{adjust_level} && $opts->{adjust_level} =~ m/^-?\d+$/;

    # The message
    my $bullet = $this->_bullet($level);
    my $indent = q{ } x ($this->{step}*$level);
    my $tlen = 0;
    my $span = $this->{width} - length($bullet) - ($this->{step}*$level) - 10;
    my @mlines = _wrap($msg, int($span*2/3), $span);
    while (defined (my $txt = shift @mlines)) {
        $s = $this->_emit($bullet . $indent . $txt);
        return $s unless $s;
        $s = $this->_emit(@mlines? "\n" : "...");
        return $s unless $s;
        $tlen = length($txt);
        $bullet = q{ } x $this->{bullet_width}; # Only bullet the first line
    }
    $this->{pos} += ($this->{step}*$level) + length($bullet) + $tlen + 3;
    return 1;
}

#
# Complete the current level, with status
#
sub tell_done {
    my ($this, $opts, @args) = _process_args(@_);
    my $sev  = shift @args || 'DONE';
    my $want_level = shift @args || undef;

    # Test that we're at the right level - do this BEFORE changing the envvar
    my $ret_level = ($ENV{$this->_envvar()} || 0) - 1;
    return
        if defined $want_level && $ret_level != $want_level;

    # Decrement level
    return
        if !$ENV{$this->_envvar()};
    my $level = --$ENV{$this->_envvar()};
    delete $ENV{$this->_envvar()}
        if $level <= 0;

    # Filtering by level?
    if (defined($this->{maxdepth}) && $level >= $this->{maxdepth}) {
        pop @{$this->{msgs}};   # discard it
        return $SEVLEV{uc $sev} || $SEVLEV{'OTHER'};
    }

    # Are we silently closing this level?
    if ($sev eq 'NONE') {
        my $s = 1;
        $s = $this->_emit("\n")
            if $this->{pos};
        return $s unless $s;
        $this->{pos} = 0;
        $this->{progwid} = 0;
        pop @{$this->{msgs}};   # discard it
        return $SEVLEV{'OTHER'};
    }

    # Make the severity text
    my $sevstr = " [$sev]\n";
    my $slen = 8; ##length($sevstr);  # make left justified within max width 3+5
    $sevstr = " [" . _colorize($sev,$sev) . "]\n"
        if $this->{color};

    # Re-issue message if needed
    my $msg = pop @{$this->{msgs}};
    if ($this->{pos} == 0) {
        my $bullet = $this->_bullet($level);
        my $indent = q{ } x ($this->{step}*$level);
        my $tlen = 0;
        my $span = $this->{width} - ($this->{step}*$level) - 10;
        my @mlines = _wrap($msg, int($span*2/3), $span);
        while (defined (my $txt = shift @mlines)) {
            my $s;
            $s = $this->_emit($bullet . $indent . $txt);
            return $s unless $s;
            $s = $this->_emit("\n")
                if @mlines;
            return $s unless $s;
            $tlen = length($txt);
            $bullet = q{ } x $this->{bullet_width}; # Only bullet the first line
        }
        $this->{pos} += length($bullet) + ($this->{step}*$level) + $tlen;
    }

    # Trailer
    my $ndots = $this->{width} - $this->{pos} - $slen;
    my $s = 1;
    $s = $this->_emit($this->{trailer} x $ndots)
        if $ndots > 0;
    return $s unless $s;

    # Severity
    $s = $this->_emit($sevstr);
    return $s unless $s;
    $this->{pos} = 0;

    # Reason option?
    my $reason = $opts->{reason};
    $s = tell_text({adjust_level => 2, %$opts}, $reason)
        if $reason;
    return $s unless $s;

    # Return with a severity value
    return $SEVLEV{uc $sev} || $SEVLEV{'OTHER'};
}

#
# Progress output
#
sub tell_over {
    my ($this, $opts, @args) = _process_args(@_);

    # Filtering by level?
    my $level = $ENV{$this->_envvar()} || 0;
    return 1
        if defined($this->{maxdepth}) && $level > $this->{maxdepth};

    # Erase prior progress output
    my $s = 1;
    $s = $this->_emit(qq{\b} x $this->{progwid});
    return $s unless $s;
    $s = $this->_emit(qq{ }  x $this->{progwid});
    return $s unless $s;
    $s = $this->_emit(qq{\b} x $this->{progwid});
    return $s unless $s;
    $this->{pos} -= $this->{progwid};
    $this->{progwid} = 0;

    return $this->tell_prog(@args);
}

sub tell_prog {
    my ($this, $opts, @args) = _process_args(@_);
    my $jn = defined $,? $, : q{};
    my $msg = join $jn, @args;
    my $s;

    # Filtering by level?
    my $level = $ENV{$this->_envvar()} || 0;
    return 1
        if defined($this->{maxdepth}) && $level > $this->{maxdepth};

    # Start a new line?
    my $avail = $this->{width} - $this->{pos} - 10;
    if (length($msg) > $avail) {
        my $level = $ENV{$this->_envvar()};
        my $bspace = q{ } x $this->{bullet_width};
        my $indent = q{ } x ($this->{step}*$level);
        $s = $this->_emit("\n");
        return $s unless $s;
        $s = $this->_emit($bspace . $indent);
        return $s unless $s;
        $this->{pos} = length($bspace) + length($indent);
        $this->{progwid} = 0;
    }

    # The text
    $s = $this->_emit($msg);
    return $s unless $s;
    $this->{pos} += length($msg);
    $this->{progwid} += length($msg);

    return 1;
}

#
# Issue additional info at the current level
#
sub tell_text {
    my ($this, $opts, @args) = _process_args(@_);
    my $jn = defined $,? $, : q{};
    my $msg = join $jn, @args;

    # Filtering by level?
    my $level = $ENV{$this->_envvar()} || 0;
    return 1
        if defined($this->{maxdepth}) && $level > $this->{maxdepth};

    # Start a new line
    my $s = 1;
    $s = $this->_emit("\n")
        if $this->{pos};
    return $s unless $s;

    # Level adjust?
    $level += $opts->{adjust_level}
        if $opts->{adjust_level} && $opts->{adjust_level} =~ m/^-?\d+$/;

    # Emit the text
    my $indent = q{ } x ($this->{step}*$level);
    my $span = $this->{width} - ($this->{step}*$level) - 10;
    my @mlines = _wrap($msg, int($span*2/3), $span);
    while (defined (my $txt = shift @mlines)) {
        my $bspace = q{ } x $this->{bullet_width};
        $s = $this->_emit($bspace . $indent . $txt . "\n");
        return $s unless $s;
        $this->{pos} = 0;
    }
    return 1;
}

sub tell_emerg {tell_done @_,"EMERG"};  # syslog: Off the scale!
sub tell_alert {tell_done @_,"ALERT"};  # syslog: A major subsystem is unusable.
sub tell_crit  {tell_done @_,"CRIT"};   # syslog: a critical subsystem is not working entirely.
sub tell_fail  {tell_done @_,"FAIL"};   # Failure
sub tell_fatal {tell_done @_,"FATAL"};  # Fatal error
sub tell_error {tell_done @_,"ERROR"};  # syslog 'err': Bugs, bad data, files not found, ...
sub tell_warn  {tell_done @_,"WARN"};   # syslog 'warning'
sub tell_note  {tell_done @_,"NOTE"};   # syslog 'notice'
sub tell_info  {tell_done @_,"INFO"};   # syslog 'info'
sub tell_ok    {tell_done @_,"OK"};     # copacetic
sub tell_debug {tell_done @_,"DEBUG"};  # syslog: Really boring diagnostic output.
sub tell_notry {tell_done @_,"NOTRY"};  # Untried
sub tell_unk   {tell_done @_,"UNK"};    # Unknown
sub tell_none  {tell_done @_,"NONE"};   # *Special* closes level quietly (prints no wrapup severity)

#
# Return the bullet string for the given level
#
sub _bullet {
    my ($this, $level) = @_;
    my $bullet = q{};
    if (ref($this->{bullets}) eq 'ARRAY') {
        my $pmax = $#{$this->{bullets}};
        $bullet = $this->{bullets}->[$level > $pmax? $pmax : $level];
    }
    elsif ($this->{bullets}) {
        $bullet = $this->{bullets};
    }
    else {
        return q{};
    }
    my $pad = q{ } x ($this->{bullet_width} - length($bullet));
    return $bullet . $pad;
}

#
# Clean option keys
#
sub _clean_opts {
    my %in = @_;
    my %out = ();
    foreach my $k (keys %in) {
        my $v = $in{$k};
        $k =~ s/^-//;
        $out{lc $k} = $v;
    }
    return %out;
}

#
# Add ANSI color to a string, if ANSI is enabled
#
sub _colorize {
    my ($str,$sev) = @_;
    my $zon = '';
    my $zoff = '';
    $zon = chr(27).'[1;31;40m'  if $sev =~ m{EMERG}i; #bold red on black
    $zon = chr(27).'[1;35m'     if $sev =~ m{ALERT}i; #bold magenta
    $zon = chr(27).'[1;31m'     if $sev =~ m{CRIT}i;  #bold red
    $zon = chr(27).'[1;31m'     if $sev =~ m{FAIL}i;  #bold red
    $zon = chr(27).'[1;31m'     if $sev =~ m{FATAL}i; #bold red
    $zon = chr(27).'[31m'       if $sev =~ m{ERROR}i; #red
    $zon = chr(27).'[33m'       if $sev =~ m{WARN}i;  #yellow
    $zon = chr(27).'[36m'       if $sev =~ m{NOTE}i;  #cyan
    $zon = chr(27).'[32m'       if $sev =~ m{INFO}i;  #green
    $zon = chr(27).'[32m'       if $sev =~ m{OK}i;    #green
    $zon = chr(27).'[37;43m'    if $sev =~ m{DEBUG}i; #grey on yellow
    $zon = chr(27).'[30;47m'    if $sev =~ m{NOTRY}i; #black on grey
    $zon = chr(27).'[1;37;47m'  if $sev =~ m{UNK}i;   #bold white on gray
    $zoff = chr(27).'[0m'       if $zon;
    return $zon.$str.$zoff;
}

#
# Emit output to filehandle, string, whatever...
#
sub _emit {
    my $this = shift;
    my $out = shift;
    my $fh = $this->{fh};
    return ref($fh) eq 'SCALAR'? $$fh .= $out : print $fh $out;
}

#
#
#
sub _envvar {
    my $this = shift;
    return $this->{envbase} . _oid($this->{fh});
}

#
# Return an output identifier for our output stream
#
sub _oid {
    my $fh = shift;
    return 'str' if ref($fh) eq 'SCALAR';
    return 0 if ref($fh);
    return fileno($fh||q{}) || 0;
}

#
# Figure out what was passed to us
#
#   Each $BASE_OBJECT in the hash is associated with one output ID (the oid).
#    The oid is just the fileno() of the file handle for normal output,
#    or the special text "str" when output is to a scalar string reference.
#    That's why we use a base object _hash_ instead of an array.
#   The $BASE_OBJECT{0} is the default one.  It's equivalent to whatever
#    oid was specified in the "use Tell ... {-fh=>$blah}" which typically
#    is STDOUT (oid=1) but may be anything.
#
#   So what're we doing here?  We have to figure out which base object to use.
#   Our subs can be called four ways:
#       A) tell "blah";
#       B) tell *LOG, "blah";
#       C) $tobj->tell "blah";
#       D) $tobj->tell *LOG, "blah";
#   Also note that "tell {-fh=>*LOG},..." is considered equivalent to case B,
#   while "$tobj->tell {-fh=>*LOG},..." is considered equivalent to case D.
#
#   In case A, we simply use the default base object $BASE_OBJECT{0}.
#   In case B, we get the oid of *LOG and use that base object.
#       If the base object does not exist, then we make one,
#       cloning it from base object 0 but overriding with the file handle.
#   In case C, we use the base object $tobj - this is classic OO perl.
#   In case D, it's like case B except that if we have to make a new
#       base object, we clone from $tobj instead of base object 0.
#
sub _process_args {
    my $this = ref($_[0]) eq __PACKAGE__? shift : $BASE_OBJECT{0};
    my $oid = _oid($_[0]);
    if ($oid) {
        # We're given a filehandle or scalar ref for output.
        #   Find the associated base object or make a new one for it
        my $fh = shift;
        if ($fh eq $BASE_OBJECT{0}->{fh}) {
            # Use base object 0, 'cuz it matches
            $oid = 0;
        }
        elsif (!exists $BASE_OBJECT{$oid}) {
            $BASE_OBJECT{$oid} = $this->clone(-fh => $fh);
        }
        $this = $BASE_OBJECT{$oid};
    }
    my $opts = {};
    if (ref($_[0]) eq 'HASH') {
        $opts = {_clean_opts(%{shift()})};
    }
    return ($this, $opts, @_);
}

#
# Wap text to fit within line lengths
#
sub _wrap {
    my ($msg, $min, $max) = @_;
    return ($msg)
        if !defined $msg || 
           $max < 3 ||
           $min > $max;

    # First split on newlines
    my @lines = ();
    foreach my $line (split(/\n/, $msg)) {
        my $split = $line;

        # Then if each segment is more than the width, wrap it
        while (length($split) > $max) {
            # Look backwards for whitespace to split on
            my $pos = $max;
            while ($pos >= $min) {
                if (substr($split,$pos,1) =~ m/\s/) {
                    $pos++;
                    last;
                }
                $pos--;
            }
            $pos = $max if $pos < $min; #no good place to break, use the max

            # Break it
            my $chunk = substr($split,0,$pos);
            $chunk =~ s/\s+$//;
            push @lines, $chunk;
            $split = substr($split, $pos, length($split)-$pos);
        }
        $split =~ s/\s+$//; #trim
        push @lines, $split;
    }
    return @lines;
}

                            ### O ###

package Tell::TiedClosure;

sub new {
    my ($proto, $base, $msg) = @_;
    my $class = ref ($proto) || $proto;  # Get the class name
    my $this  = {-base => $base};
    bless ($this, $class);
    $base->tell($msg);
    return $this;
}

sub DESTROY {
    my $this = shift;
    $this->{-base}->tell_done();
}



1; # EOM
__END__

=head1 NAME

Tell - Print messages with balance, indentation, severity, and autoclosure.

***This documentation has not yet been completed. ***

=head1 VERSION

This document describes Tell version 0.1.4


=head1 SYNOPSIS

For a script like this:

    use Tell qw/:all/;
    tell "System parameter updates";
      tell "CLOCK_UTC";
      #...do_something();
      tell_ok;

      tell "NTP Servers";
      #...do_something();
      tell_error;

      tell "DNS Servers";
      #...do_something();
      tell_warn;
    tell_done;

You get this output:

   System parameter updates...
     CLOCK_UTC........................................................ [OK]
     NTP Servers...................................................... [ERROR]
     DNS Servers...................................................... [WARN]
   System parameter updates........................................... [DONE]

=head1 DESCRIPTION

The C<Tell> package is used to print balanced and nested messages
with a completion status.  These messages indent easily within each other,
autocomplete on scope exit, are easily parsed, may be bulleted, can be filtered,
and even can show status in color.

For example, you write code like this:

    use Tell qw/:all/;
    tell "Reconfiguring the grappolator";
    do_whatchamacallit();
    do_something_else();

It begins by printing:

    Reconfiguring the grappolator...

Then it does "whatchamacallit" and "something else".  When these are complete
it adds the rest of the line: a bunch of dots and the [DONE].

    Reconfiguring the grappolator...................................... [DONE]

Your do_whatchamacallit() and do_something_else() subroutines may also C<tell>
what they're doing, and indicate success or failure or whatever, so you
can get nice output like this:

    Reconfiguring the grappolator...
      Processing whatchamacallit....................................... [WARN]
      Fibulating something else...
        Fibulation phase one........................................... [OK]
        Fibulation phase two........................................... [ERROR]
        Wrapup of fibulation........................................... [OK]
    Reconfiguring the grappolator...................................... [DONE]


=head2 Examples

A series of examples will make C<Tell> easier to understand.

=head3 Getting started

    use Tell ':all';
    tell "Frobnicating the biffolator";
    sleep 1; # simulate the frobnication process
    tell_done;

First this prints:

    Frobnicating the biffolator...

Then after the "frobnication" process is complete, the line is
continued so it looks like this:

    Frobnicating the biffolator....................................... [DONE]

=head3 Autocompletion

In the above example, we end with a C<tell_done> call to indicate that
the thing we told about (I<Frobnicating the biffolator>) is now done.
We don't need to do the C<tell_done>.  It will be called automatically
for us when the current scope is exited (for this example: when the program ends).
So the code example could be just this:

    use Tell ':all';
    tell "Frobnicating the biffolator";
    sleep 1; # simulate the frobnication process

and we'd get the same results.  

Yeah, autocompletion may not seem so useful YET,
but hang in there and you'll soon see how wonderful it is.

=head3 Completion Severity

There's many ways a task can complete.  It can be simply DONE, or it can
complete with an ERROR, or it can be OK, etc.  These completion codes are
called the I<severity code>s,  C<Tell> defines many different severity codes.
The severity codes are borrowed from the UNIX syslog subsystem,
plus a few from VMS and other sources.  They should be familiar to you.

Most severity codes also have an associated numerical value.
This value is called the I<severity level>.
It's useful for comparing severities to eachother or filtering out
severities you don't want to be bothered with.

Here are the severity codes and their severity values.
Those on the same line are considered equal in severity:

    EMERG => 15,
    ALERT => 13,
    CRIT  => 11, FAIL => 11, FATAL => 11,
    ERROR => 9,
    WARN  => 7,
    NOTE  => 6,
    INFO  => 5, OK => 5,
    DEBUG => 4,
    NOTRY => 3,
    UNK   => 2,

Anything severity not listed is given the value 1.

To complete with a different severity, call C<tell_done> with the
severity code like this:

    tell_done "WARN";

C<tell_done> returns with the severity value from the above table,
otherwise it returns 1, unless there's an error in which case it
returns false.

As a convienence, it's easier to use these functions which do the same thing,
only simpler:

     Function          Equivalent                       Usual Meaning
    ----------      -----------------      -----------------------------------------------------
    tell_emerg      tell_done "EMERG";     syslog: Off the scale!
    tell_alert      tell_done "ALERT";     syslog: A major subsystem is unusable.
    tell_crit       tell_done "CRIT";      syslog: a critical subsystem is not working entirely.
    tell_fail       tell_done "FAIL";      Failure
    tell_fatal      tell_done "FATAL";     Fatal error
    tell_error      tell_done "ERROR";     syslog 'err': Bugs, bad data, files not found, ...
    tell_warn       tell_done "WARN";      syslog 'warning'
    tell_note       tell_done "NOTE";      syslog 'notice'
    tell_info       tell_done "INFO";      syslog 'info'
    tell_ok         tell_done "OK";        copacetic
    tell_debug      tell_done "DEBUG";     syslog: Really boring diagnostic output.
    tell_notry      tell_done "NOTRY";     Untried
    tell_unk        tell_done "UNK";       Unknown

We'll change our simple example to give a FATAL completion:

    use Tell ':all';
    tell "Frobnicating the biffolator";
    sleep 1; # simulate the frobnication process
    tell_fatal;

Here's how it looks:

    Frobnicating the biffolator....................................... [FATAL]

=head4 Severity Colors

A spiffy little feature of C<Tell> is that you can enable colorization of the
severity codes.  That means that the severity code inside the square brackets
is printed in color, so it's easy to see.  The standard ANSI color escape sequences
are used to do the colorization.

Here's the colors:

    EMERG    bold red on black
    ALERT    bold magenta
    CRIT     bold red
    FAIL     bold red
    FATAL    bold red
    ERROR    red
    WARN     yellow (usually looks orange)
    NOTE     cyan
    INFO     green
    OK       green
    DEBUG    grey on yellow/orange
    NOTRY    black on grey
    UNK      bold white on grey
    DONE     default font color (unchanged)

To use colors, do this when you I<use> Tell:

    use Tell ":all", {-colors => 1};

Run sample003.pl, included with this module, to see how it looks on
your terminal.

=head3 Nested Messages

=head3 Nesting Across Processes

If you write a Perl script that uses Tell, and this script invokes other
scripts that also use Tell, some nice magic happens.  The inner scripts become
aware of the outer, and they "nest" their indentation levels appropriately.
Pretty cool, eh?

=head3 Closing with Different Severities, or... Why Autocompletion is Nice

So far our examples have been rather boring.  They're not vey real-world.  
In a real script, you'll be doing various steps, checking status as you go,
and bailing out with an error status on each failed check.  It's only when
you get to the bottom of all the steps that you know it's succeeded.
Here's where tell becomes more useful:

    use Tell qw/:all/, {-closestat => "ERROR"};
    tell "Juxquolating the garfibnotor";
    return tell_fail
        if !do_kibvoration();
    return tell_warn
        if !do_rumbalation();
    $fail_reason = do_major_cleanup();
    if ($fail_reason) {
        tell_text $fail_reason;
        return;
    }
    tell_ok;

In this example, we set C<-closestat> to "ERROR".  This means that if we
exit scope without doing a tell_done (or its equivalents), a tell_done "ERROR"
will automatically be called.  This will handle exceptions or other blowouts.
Next we tell the main title of what we're doing, and then invoke three
sublevels of processing (the do_* calls).  If any of those fail, we 
return with the error (tell_fail and tell_warn).  The third captures
some reason text, and uses tell_text to emit that reason before returning.
That last return causes a scope exit and autocompletion kicks in,
giving an "ERROR" as the final severity.


=head3 Output to Other File Handles

By default, C<Tell> writes its output to STDOUT (or whatever select() is set
to).  You can tell C<Tell> to use another file handle like this:

    use Tell qw/:all/, {-fh => *LOG};

Individual "tell" lines may also take a file handle as the first
argument, in a manner similar to a print statement:

    tell *LOG, "this", " and ", "that";

Note the comma after the C<*LOG> -- if it was a C<print> you
would omit the comma.

=head3 Output to Strings

If you give Tell a scalar (string) reference instead of a file handle,
then Tell's output will be appended to this string.

For example:

    my $out = "";
    use Tell qw/:all/, {-fh => \$out};

Individual "tell" lines may also take a scalar reference as the first
argument:

    tell \$out, "this ", " and ", "that";

=head3 Output Independence

C<Tell> separates output contexts by file handle.  That means the
indentation, autoclosure, bullet style, width, etc. for any output told
to STDERR is independent of output told to STDOUT, and independent
of output told to a string.  All string output is lumped together
into one context.

=head3 Return Status

Like C<print>, the C<tell> function returns a true value on success
and false on failure.  Failure can occur, for example, when attempting
to tell to a closed filehandle.

Note that to get the return status, you must assign into a scalar context,
not a list context:

      my $stat;
      $stat = tell "Whatever";      # OK. This puts status into $stat
      ($stat) = tell "Whatever";    # NOT what it looks like!

In list context, the closure for C<tell> is bound to the list variable's
scope and autoclosure is disabled.

=head3 Bullets

You may preceed each message with a bullet.
The is usually a single character
such as a dash or an asterix, but may be multiple characters.
When it's multiple characters, typically it's just a single character
with a space on either side.

You may have a different bullet for each nesting level.
Levels deeper than the number of defined bulelts will use the last bullet.

Define bullets by passing an array reference of the bullet strings
with C<-bullet>.  If you want the bullet to be the same for all levels,
just pass the string.  Here's some popular bullet definitions:

    -bullets => "* "
    -bullets => [" * ", " + ", " - ", "   "]


=head2 Basic Use

=head2 Features

TODO: take the feature snippets from the above example section and put 'em here.

* Automatic closure of messages
* Colorized text
* Bulleted output
* Output independent by file descriptor

=head2 TBS

=head1 EXPORTS

Nothing is exported by default.  You'll want to do one of these:

    use Tell qw/tell tell_done/;    # To get just these two functions
    use Tell qw/:all/;              # To get all functions
    use Tell qw/:syslog/;           # To get base functions plus syslog severities only

Most of the time, you'll want the :all form.

=head1 INTERFACE

=over 4

=item C<base>

internal base object accessor.

=item C<clone>

Clones the current object and returns a new copy.
Any given attributes override the cloned object.

=item C<new>

Constructor for a Tell object.

=item C<set>

Sets attributes on a Tell object.

=item C<tell>

Use C<tell> to emit a message similar to how you would use C<print>.
Every use of C<tell> increases the current message level

Procedural call syntax:

    tell LIST
    tell *FH, LIST
    tell \$out, LIST
    tell {ATTRS}, LIST

Object-oriented call syntax:

    $tobj->tell (LIST)
    $tobj->tell (*FH, LIST)
    $tobj->tell (\$out, LIST)
    $tobj->tell ({ATTRS}, LIST)

=item C<tell_done>

Closes the current message level, re-printing the message
if necessary, printing dot-dot trailers to get proper alignment,
and the given completion severity.

=item C<tell_alert>

=item C<tell_crit>

=item C<tell_debug>

=item C<tell_emerg>

=item C<tell_error>

=item C<tell_fail>

=item C<tell_fatal>

=item C<tell_info>

=item C<tell_note>

=item C<tell_notry>

=item C<tell_ok>

=item C<tell_unk>

=item C<tell_warn>

All these are convienence methods that call C<tell_done>) 
with the indicated severity.  For example, C<tell_fail()> is
equivalent to C<tell_done "FAIL">.

=item C<tell_none>

This is equivalent to tell_done, except that it does NOT print
a wrapup line or a completion severity.  It simply closes out
the current level with no message.

=item C<tell_over>

=item C<tell_prog>

Tells a progress indication, such as a percent or M/N or whatever
you devise.  In fact, this simply puts *any* string on the same line
as the original message (for the current level).

Using C<tell_over> will first backspace over a prior progress string
(if any) to clear it, then it will write the progress string.
The prior progress string could have been emitted by tell_over
or tell_prog; it doesn't matter.

C<tell_prog> does not backspace, it simply puts the string out there.

For example,

  use Tell qw/:all/;
  tell "Varigating the shaft";
  tell_prog '10%...';
  tell_prog '20%...';

gives this output:

  Varigating the shaft...10%...20%...

Keep your progress string small!  The string is treated as an indivisible
entity and won't be split.  If the progress string is too big to fit on the
line, a new line will be started with the appropriate indentation.

With creativity, there's lots of progress indicator styles you could
use.  Percents, countdowns, spinners, etc.
Look at sample005.pl included with this package.
Here's some styles to get you thinking:

        Style       Example output
        -----       --------------
        N           3       (overwrites prior number)
        M/N         3/7     (overwrites prior numbers)
        percent     20%     (overwrites prior percent)
        dots        ....    (these just go on and on, one dot for every step)
        tics        .........:.........:...
                            (like dots above but put a colon every tenth)
        countdown   9... 8... 7...
                            (liftoff!)


=item C<tell_text>

This prints the given text without changing the current level.
Use it to give additional information, such as a blob of description.
A lengthy lines will be wrapped to fit nicely in the given width.

=back


=head1 CONFIGURATION AND ENVIRONMENT

Tell requires no configuration files or environment variables.
However, it does set environment variables with this form of name:

    tell_fd#_th#

This _envvar holds the current level of messages (represented
visually by indentation), so that indentation can be smoothly
maintained across process contexts.

In this _envvar's name, fd# is the fileno() of the output file handle to which
the messages are written.  By default output is to STDERR,
which has a fileno of 2, so the _envvar would be C<tell_fd2>.
If output is being written to a string (C<-fh => \$some_string>),
then fd# is the string "str", for example C<tell_fdstr>

When Tell is used with threads, the thread ID is placed
in th# in the _envvar.
Thus for thread #7, writing Tell messages to STDERR, the _envvar
would be C<tell_fd2_th7>.
For the main thread, th# and the leading underscore are omitted.

Under normal operation, this environment variable is deleted
before the program exits, so generally you won't see it.

Note: If your program's output seems excessively indented, it may be
that this _envvar has been left over from some other aborted run.
Check for it and delete it if found.

=head1 DEPENDENCIES

This pure-Perl module depends upon Scope::Upper.


=head1 INCOMPATIBILITIES

None reported.


=head1 BUGS AND LIMITATIONS

=for author to fill in:
    A list of known problems with the module, together with some
    indication Whether they are likely to be fixed in an upcoming
    release. Also a list of restrictions on the features the module
    does provide: data types that cannot be handled, performance issues
    and the circumstances in which they may arise, practical
    limitations on the size of data sets, special cases that are not
    (yet) handled, etc.

Limitation:  Output in a threaded environment isn't always pretty.  
It works OK and won't blow up, but indentation may get a bit screwy.

Bugs: No bugs have been reported.

Please report any bugs or feature requests to
C<bug-Tell@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org>.


=head1 SEE ALSO

Somewhat similar but not quite the same are:

    Debug::Message
    Log::Dispatch
    PTools::Debug

=head1 AUTHOR

Steve Roscio  C<< <roscio@cpan.org> >>

=head1 ACKNOWLEDGEMENTS

Thanx to Paul Vencel for being an idea soundingboard for this package.

=head1 LICENCE AND COPYRIGHT

Copyright (c) 2009, Steve Roscio C<< <roscio@cpan.org> >>. All rights reserved.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.


=head1 DISCLAIMER OF WARRANTY

Because this software is licensed free of charge, there is no warranty
for the software, to the extent permitted by applicable law.  Except when
otherwise stated in writing the copyright holders and/or other parties
provide the software "as is" without warranty of any kind, either
expressed or implied, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose.  The
entire risk as to the quality and performance of the software is with
you.  Should the software prove defective, you assume the cost of all
necessary servicing, repair, or correction.

In no event unless required by applicable law or agreed to in writing
will any copyright holder, or any other party who may modify and/or
redistribute the software as permitted by the above licence, be
liable to you for damages, including any general, special, incidental,
or consequential damages arising out of the use or inability to use
the software (including but not limited to loss of data or data being
rendered inaccurate or losses sustained by you or third parties or a
failure of the software to operate with any other software), even if
such holder or other party has been advised of the possibility of
such damages.
