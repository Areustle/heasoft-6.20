HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_LIBRARY_ROOT		= batimageutils

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_SHLIB_LIBS		= ${HD_LFLAGS} -lbatfftsg -l${CFITSIO}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	= imageutils.h

HD_LIBRARY_SRC_c	= imageutils.c rescalearr.c detmask.c imagefft.c \
			corrections.c imageinterp.c distortmap.c

include ${HD_STD_MAKEFILE}

${HD_LIBRARY_SRC_c:.c=.${OSUF}}: imageutils.h
