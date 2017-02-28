# Component (mission) name. Developers need not change/delete this.
HD_COMPONENT_NAME	= swift

# Software release version number. Developers need not change/delete this.
HD_COMPONENT_VERS	=

# Build static on Mac OS X (and Cygwin)
HD_LIB_STYLE		= ${HD_LIB_STYLE_F77}

# If this directory needs to build a library, list the library
# root name here. The root name of libXYZ.a is just XYZ.
HD_LIBRARY_ROOT		= swxrt

# C language source files (.c) to add to the library.
HD_LIBRARY_SRC_c	= highfits/highfits.c xrtmisc/xrt_termio.c xrtmisc/lowlevel.c xrtmisc/xrt_ran2.c xrtfits/xrtfits.c  xrtmisc/misc.c xrtmisc/astro.c xrtmisc/calculus.c xrtdasversion/xrtdasversion.c xrtcaldb/CalGetFileName.c xrtbpfunc/xrtbpfunc.c xrtmisc/slatec_c.c xrtfunc/xrttimefunc.c xrtmisc/xrt_gammq.c xrtcaldb/ReadCalFile.c

# F77 language source files (.f) to add to the library.
HD_LIBRARY_SRC_f	= xrtmisc/ranlux.f xrtmisc/slatec.f
HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${HEAUTILS} -l${CFITSIO} -l${HEAIO} \
			  ${F77LIBS4C}
HD_ADD_SHLIB_LIBS	= yes

# Header files to install (not all header files necessarily need to be).
HD_INSTALL_HEADERS	= highfits/basic.h highfits/highfits.h xrtmisc/xrt_termio.h xrtmisc/lowlevel.h xrtmisc/misc.h xrtmisc/xrt_ran2.h xrtfits/xrtfits.h xrtmisc/astro.h xrtmisc/calculus.h xrtfits/xrtdefs.h xrtdasversion/xrtdasversion.h xrtcaldb/xrtcaldb.h xrtbpfunc/xrtbpfunc.h xrtmisc/slatec.h xrtfunc/xrttimefunc.h xrtmisc/xrt_gammq.h

# C flags to use in every compilation.
HD_CFLAGS		= -I./highfits -I./xrtmisc -I./xrtfits -I./xrtdasversion -I./xrtcaldb -I./xrtfunc -I./xrtbpfunc ${HD_STD_CFLAGS}

# F77 flags to use in every compilation.
HD_FFLAGS		= ${HD_STD_FFLAGS}

# Libraries to install.
HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

# Extra items to remove during a distclean.
HD_CLEAN		= ${HD_LIBRARY_SRC_c:.c=.o} ${HD_LIBRARY_SRC_f:.f=.o}

# Perl libraries to install.
HD_INSTALL_PERL_LIBS	= xrtperl/libswxrtperl.pl

# Perl unit test library/libraries to install.
HD_TEST_PERL_LIBS	= xrtperl/ut_xrtobj.pm

include ${HD_STD_MAKEFILE}
