use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'Tell',
    AUTHOR              => 'Steve Roscio <roscio@cpan.org>',
    VERSION_FROM        => 'lib/Tell.pm',
    ABSTRACT_FROM       => 'lib/Tell.pm',
    PL_FILES            => {},
    PREREQ_PM => {
        'Scope::Upper'  => 0.06,
        'Test::More'    => 0,
        'version'       => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'Tell-*' },
);
