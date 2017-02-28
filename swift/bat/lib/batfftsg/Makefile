HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_LIBRARY_ROOT         = batfftsg

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	= batfftsg.h

HD_LIBRARY_SRC_c	= fftsg.c fftsg2d.c sfftsg.c sfftsg2d.c

include ${HD_STD_MAKEFILE}

${HD_LIBRARY_SRC_c:.c=.${OSUF}}: batfftsg.h
