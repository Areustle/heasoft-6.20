HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	=

HD_LIBRARY_ROOT		= rew

HD_LIBRARY_SRC_c	= report.c keyutil.c genimage.c overlap.c \
				point.c vertex.c polygon.c caldbquery.c rewmath.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${HEAUTILS} -l${PIL} -l${CFITSIO} -l${READLINE} -l${HEAIO}

HD_INSTALL_HEADERS	= report.h keyutil.h genimage.h genimage.h0 overlap.h polygon.h caldbquery.h rewmath.h

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_PERL_SCRIPTS	= rewaiter

include ${HD_STD_MAKEFILE}
