HD_COMPONENT_NAME	= heacore

HD_COMPONENT_VERS	= 

HD_TEST_CTASK		= testheaapp

HD_TEST_CTASK_SRC_c	= ${HD_TEST_CTASK}.c

HD_CFLAGS		= -I../.. ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -l${AHLOG} -l${PIL} -l${HEAUTILS} \
			  -l${HEAAPP} -l${CFITSIO} -l${READLINE} -l${HEAIO} ${SYSLIBS}

HD_TEST_TASKS		= ${HD_TEST_CTASK}

# Do not use HD_TEST_SUBDIRS because that causes the build to try to
# recurse into directories that do not contain Makefiles.
#AH_TEST_SUBDIRS		= ut01-class ut02-stacktrace ut03-function ut04-c-interface

HD_CLEAN		= ${HD_TEST_CTASK}.log

include ${HD_STD_MAKEFILE}

run-test:
	${HD_TEST_CTASK}
