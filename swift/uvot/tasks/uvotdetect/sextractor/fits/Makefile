HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= sexfits

HD_LIBRARY_SRC_c	= fitsbody.c fitscat.c \
			fitscheck.c fitscleanup.c fitsconv.c fitshead.c \
			fitskey.c fitsmisc.c fitsread.c fitstab.c \
			fitsutil.c fitswrite.c

HD_LIB_STYLE = static

DEFS = -DHAVE_SYS_MMAN_H=1

HD_CFLAGS		= ${HD_STD_CFLAGS} ${DEFS}

HD_INSTALL_HEADERS	=

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

include ${HD_STD_MAKEFILE}

