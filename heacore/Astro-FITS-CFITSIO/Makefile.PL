use strict;
use warnings;

use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'Astro::FITS::CFITSIO',
    'VERSION_FROM' => 'CFITSIO.pm', # finds $VERSION
    'LIBS'	=> ["$ENV{HD_LFLAGS} -l$ENV{CFITSIO} -lm"],
    'INC'	=> "$ENV{HD_IFLAGS}",
    'DEFINE'	=> '',
    'OBJECT' => 'CFITSIO.o util.o',
    'clean' => { FILES => "testprog.fit testprog/testprog.fit testprog/testprog.lis" },
);
