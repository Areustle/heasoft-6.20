use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'HEACORE::PIL',
    'VERSION_FROM' => 'PIL.pm', # finds $VERSION
    'LIBS'	=> ["$ENV{HD_STD_CLIBS}"],   # e.g., '-lm' 
    'DEFINE'	=> '',     # e.g., '-DHAVE_SOMETHING' 
    'OBJECT'    => 'PIL.o Av_CharPtrPtr.o',
    'INC'	=> "$ENV{HD_IFLAGS}",     # e.g., '-I/usr/include/other' 
);
