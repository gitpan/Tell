#!perl -w
use strict;
use warnings;
use Test::More tests => 9;

my $out;
use Tell qw/:all/, {-fh      => \$out,
                    -step    => 2,
                    -width   => 40};


$out = q{};
{ tell "Subdoulation of quantifoobar";
    { tell "Morgozider";
        { tell "Frimrodding the quark";
            { tell "Eouing our zyxxpth";
                { tell "Shiniffing";
                }
              tell_crit;
            }
          tell_ok;
        }
      tell_warn;
    }
}
is($out, "Subdoulation of quantifoobar...\n".
         "  Morgozider...\n".
         "    Frimrodding the quark...\n".
         "      Eouing our zyxxpth...\n".
         "        Shiniffing.............. [DONE]\n".
         "      Eouing our zyxxpth........ [CRIT]\n".
         "    Frimrodding the quark....... [OK]\n".
         "  Morgozider.................... [WARN]\n".
         "Subdoulation of quantifoobar.... [DONE]\n",
            "Unfiltered");

Tell::set(-maxdepth => 0);
$out = q{};
{ tell "Subdoulation of quantifoobar";
    { tell "Morgozider";
        { tell "Frimrodding the quark";
            { tell "Eouing our zyxxpth";
                { tell "Shiniffing";
                }
              tell_crit;
            }
          tell_ok;
        }
      tell_warn;
    }
}
is($out, "", "All filtered - nothing shown");

Tell::set(-maxdepth => 1);
$out = q{};
{ tell "Subdoulation of quantifoobar";
    { tell "Morgozider";
        { tell "Frimrodding the quark";
            { tell "Eouing our zyxxpth";
                { tell "Shiniffing";
                }
              tell_crit;
            }
          tell_ok;
        }
      tell_warn;
    }
}
is($out,"Subdoulation of quantifoobar.... [DONE]\n",
            "Max Depth = 1");

Tell::set(-maxdepth => 2);
$out = q{};
{ tell "Subdoulation of quantifoobar";
    { tell "Morgozider";
        { tell "Frimrodding the quark";
            { tell "Eouing our zyxxpth";
                { tell "Shiniffing";
                }
              tell_crit;
            }
          tell_ok;
        }
      tell_warn;
    }
}
is($out, "Subdoulation of quantifoobar...\n".
         "  Morgozider.................... [WARN]\n".
         "Subdoulation of quantifoobar.... [DONE]\n",
            "Max Depth = 2");

Tell::set(-maxdepth => 3);
$out = q{};
{ tell "Subdoulation of quantifoobar";
    { tell "Morgozider";
        { tell "Frimrodding the quark";
            { tell "Eouing our zyxxpth";
                { tell "Shiniffing";
                }
              tell_crit;
            }
          tell_ok;
        }
      tell_warn;
    }
}
is($out, "Subdoulation of quantifoobar...\n".
         "  Morgozider...\n".
         "    Frimrodding the quark....... [OK]\n".
         "  Morgozider.................... [WARN]\n".
         "Subdoulation of quantifoobar.... [DONE]\n",
            "Max Depth = 3");

Tell::set(-maxdepth => 4);
$out = q{};
{ tell "Subdoulation of quantifoobar";
    { tell "Morgozider";
        { tell "Frimrodding the quark";
            { tell "Eouing our zyxxpth";
                { tell "Shiniffing";
                }
              tell_crit;
            }
          tell_ok;
        }
      tell_warn;
    }
}
is($out, "Subdoulation of quantifoobar...\n".
         "  Morgozider...\n".
         "    Frimrodding the quark...\n".
         "      Eouing our zyxxpth........ [CRIT]\n".
         "    Frimrodding the quark....... [OK]\n".
         "  Morgozider.................... [WARN]\n".
         "Subdoulation of quantifoobar.... [DONE]\n",
            "Max Depth = 4");

Tell::set(-maxdepth => 5);
$out = q{};
{ tell "Subdoulation of quantifoobar";
    { tell "Morgozider";
        { tell "Frimrodding the quark";
            { tell "Eouing our zyxxpth";
                { tell "Shiniffing";
                }
              tell_crit;
            }
          tell_ok;
        }
      tell_warn;
    }
}
is($out, "Subdoulation of quantifoobar...\n".
         "  Morgozider...\n".
         "    Frimrodding the quark...\n".
         "      Eouing our zyxxpth...\n".
         "        Shiniffing.............. [DONE]\n".
         "      Eouing our zyxxpth........ [CRIT]\n".
         "    Frimrodding the quark....... [OK]\n".
         "  Morgozider.................... [WARN]\n".
         "Subdoulation of quantifoobar.... [DONE]\n",
            "Max Depth = 5");

Tell::set(-maxdepth => 99);
$out = q{};
{ tell "Subdoulation of quantifoobar";
    { tell "Morgozider";
        { tell "Frimrodding the quark";
            { tell "Eouing our zyxxpth";
                { tell "Shiniffing";
                }
              tell_crit;
            }
          tell_ok;
        }
      tell_warn;
    }
}
is($out, "Subdoulation of quantifoobar...\n".
         "  Morgozider...\n".
         "    Frimrodding the quark...\n".
         "      Eouing our zyxxpth...\n".
         "        Shiniffing.............. [DONE]\n".
         "      Eouing our zyxxpth........ [CRIT]\n".
         "    Frimrodding the quark....... [OK]\n".
         "  Morgozider.................... [WARN]\n".
         "Subdoulation of quantifoobar.... [DONE]\n",
            "Max Depth = 99");

Tell::set(-maxdepth => undef);
$out = q{};
{ tell "Subdoulation of quantifoobar";
    { tell "Morgozider";
        { tell "Frimrodding the quark";
            { tell "Eouing our zyxxpth";
                { tell "Shiniffing";
                }
              tell_crit;
            }
          tell_ok;
        }
      tell_warn;
    }
}
is($out, "Subdoulation of quantifoobar...\n".
         "  Morgozider...\n".
         "    Frimrodding the quark...\n".
         "      Eouing our zyxxpth...\n".
         "        Shiniffing.............. [DONE]\n".
         "      Eouing our zyxxpth........ [CRIT]\n".
         "    Frimrodding the quark....... [OK]\n".
         "  Morgozider.................... [WARN]\n".
         "Subdoulation of quantifoobar.... [DONE]\n",
            "Max Depth back to Unfiltered");

