HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_LIBRARY_ROOT		= batutils

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	= bat_gswdev.h 
 
HD_LIBRARY_SRC_c	= batidconvert.c svd.c \
                          bat_read_cal_coeff.c \
			  batcaldb2.c \
			  ebins.c econv.c

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${HEAUTILS} -l${PIL} \
			  -l${CFITSIO} -l${READLINE} -l${HEAIO}

include ${HD_STD_MAKEFILE}

${HD_LIBRARY_SRC_c:.c=.${OSUF}}: bat_gswdev.h
