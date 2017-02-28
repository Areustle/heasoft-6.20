HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	=

HD_TEST_CTASK		= test-xpi

HD_TEST_CTASK_SRC_c	= test-xpi.c

HD_CFLAGS		= ${HD_STD_CFLAGS} -I../ -I../include

HD_CLIBS		= ${HD_LFLAGS} -lxanlib${FT_VERS} -l${PIL} \
			  -l${READLINE} ${SYSLIBS} ${F77LIBS4C}

HD_CLEAN		= test-ape.par test-xpi.par \
			  test-ape-get.par test-xpi-get.par \
			  test-ape-put.par test-xpi-put.par \
			  syspfiles/test-ape.par syspfiles/test-xpi.par

include ${HD_STD_MAKEFILE}

run-test:
	HEADAS=${HD_TOP_EXEC_PFX}; export HEADAS; ./run-test.pl
