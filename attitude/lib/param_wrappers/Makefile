HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= param_wrappers

HD_LIBRARY_SRC_c	= param_wrappers.c att_fatal.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${PIL} -l${CFITSIO} -l${READLINE} -l${HEAIO}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	= param_wrappers.h att_fatal.h

include ${HD_STD_MAKEFILE}
