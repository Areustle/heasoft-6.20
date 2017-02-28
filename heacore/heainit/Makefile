HD_COMPONENT_NAME	= heacore

HD_COMPONENT_VERS	=

HD_LIBRARY_ROOT		= ${HEAINIT}

HD_LIBRARY_SRC_c	= headas_init.c

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${HEAUTILS} -l${PIL} -l${CFITSIO} \
			  -l${READLINE} -l${HEAIO} ${SYSLIBS}

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	= headas.h headas_main.c

# Include this when heainit has a useful unit test.
#HD_TEST_SUBDIRS		= ut

include ${HD_STD_MAKEFILE}
