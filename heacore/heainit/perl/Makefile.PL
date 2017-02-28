use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'HEACORE::HEAINIT',
    'VERSION_FROM' => 'HEAINIT.pm', # finds $VERSION
    'LIBS'	=> ["$ENV{HD_STD_CLIBS}"],   # e.g., '-lm' 
    'DEFINE'	=> '',     # e.g., '-DHAVE_SOMETHING' 
    'OBJECT'    => 'HEAINIT.o Av_CharPtrPtr.o',
    'INC'	=> "$ENV{HD_IFLAGS}",     # e.g., '-I/usr/include/other' 
);
