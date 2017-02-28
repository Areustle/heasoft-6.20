# Component (mission) name. Developers need not change/delete this.
HD_COMPONENT_NAME	= nustar

# Software release version number. Developers need not change/delete this.
HD_COMPONENT_VERS	=

# Build static on Mac OS X (and Cygwin)
HD_LIB_STYLE		= ${HD_LIB_STYLE_F77}

# If this directory needs to build a library, list the library
# root name here. The root name of libXYZ.a is just XYZ.
HD_LIBRARY_ROOT		= nustardas

# C language source files (.c) to add to the library.
HD_LIBRARY_SRC_c	= highfits/nu_highfits.c misc/nu_termio.c misc/nu_lowlevel.c misc/nu_misc.c misc/nu_gammq.c caldb/nu_caldb.c badpix/nu_badpix.c nustardasversion/nustardasversion.c

# F77 language source files (.f) to add to the library.
HD_LIBRARY_SRC_f	= 
HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${HEAUTILS} -l${CFITSIO} -l${HEAIO} \
			  ${F77LIBS4C}

# Header files to install (not all header files necessarily need to be).
HD_INSTALL_HEADERS	= highfits/nu_basic.h highfits/nu_defs.h highfits/nu_highfits.h misc/nu_termio.h misc/nu_lowlevel.h misc/nu_misc.h misc/nu_gammq.h caldb/nu_caldb.h badpix/nu_badpix.h nustardasversion/nustardasversion.h

# C flags to use in every compilation.
HD_CFLAGS		= -I./highfits -I./misc -I./caldb -I./badpix -I./nustardasversion ${HD_STD_CFLAGS}

# F77 flags to use in every compilation.
HD_FFLAGS		= ${HD_STD_FFLAGS}

# Libraries to install.
HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

# Extra items to remove during a distclean.
HD_CLEAN		= ${HD_LIBRARY_SRC_c:.c=.o} ${HD_LIBRARY_SRC_f:.f=.o}

# Perl libraries to install.
HD_INSTALL_PERL_LIBS	= perl/libnustarperl.pl

# Perl unit test library/libraries to install.
HD_TEST_PERL_LIBS	= perl/ut_nustar.pm

include ${HD_STD_MAKEFILE}
