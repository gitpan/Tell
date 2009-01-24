use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Tell',
    license             => 'perl',
    dist_author         => 'Steve Roscio <roscio@cpan.org>',
    dist_version_from   => 'lib/Tell.pm',
    requires => {
        'Scope::Upper'  => 0,
        'Test::More'    => 0,
        'version'       => 0,
    },
    add_to_cleanup      => [ 'Tell-*' ],
);

$builder->create_build_script();
