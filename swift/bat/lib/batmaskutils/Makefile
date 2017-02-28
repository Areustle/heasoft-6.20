HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_LIBRARY_ROOT		= batmaskutils

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${PIL} -l${CFITSIO} -l${READLINE} \
			  -l${HEAIO}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	= batdet.h batmask.h
 
HD_LIBRARY_SRC_c	= detbresimg.c forwmask.c maskinit.c maskutils.c \
			  maskwtimg.c maskwtimg_op.c maskcoords.c \
			  maskwtcorrs.c detmask.c fastcell.c \
			  dettop_shad.c forwdettop.c

include ${HD_STD_MAKEFILE}

${HD_LIBRARY_SRC_c:.c=.${OSUF}}: batdet.h batmask.h
