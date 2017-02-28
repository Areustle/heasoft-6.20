HD_COMPONENT_NAME	= heacore

HD_COMPONENT_VERS	=

HD_LIBRARY_ROOT		= ${HEAIO}

HD_LIBRARY_SRC_c	= headas_stdio.c

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${PIL} -l${CFITSIO} -l${READLINE} \
			  ${SYSLIBS}

HD_TEST_CTASK		= headas_stdio_test

HD_TEST_CTASK_SRC_c	= headas_stdio_test.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -l${HEAIO} ${SYSLIBS}

HD_INSTALL_HEADERS	= headas_stdio.h

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_DISTCLEAN		= test-result

include ${HD_STD_MAKEFILE}
