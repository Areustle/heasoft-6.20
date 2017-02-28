HD_COMPONENT_NAME	= heacore

HD_COMPONENT_VERS	= 

HD_TEST_CXXTASK		= testahlog

HD_TEST_CXXTASK_SRC_cxx	= ${HD_TEST_CXXTASK}.cxx

HD_TEST_CTASK		= testcahlog

HD_TEST_CTASK_SRC_c	= ${HD_TEST_CTASK}.c

HD_TEST_PFILES		= ${HD_TEST_CXXTASK}.par ${HD_TEST_CTASK}.par

HD_CXXFLAGS		= ${HD_STD_CXXFLAGS}

HD_CXXLIBS		= ${HD_LFLAGS} -l${AHLOG} ${HD_STD_LIBS} ${SYSLIBS}

HD_CFLAGS		= -I../.. ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -l${AHLOG} ${HD_STD_LIBS} ${SYSLIBS}

HD_TEST_TASKS		= ${HD_TEST_CXXTASK} ${HD_TEST_CTASK}

# Do not use HD_TEST_SUBDIRS because that causes the build to try to
# recurse into directories that do not contain Makefiles.
AH_TEST_SUBDIRS		= ut01-class ut02-stacktrace ut03-function ut04-c-interface

HD_CLEAN		= ut01-class/aht-input \
			  ut01-class/aht-output \
			  ut01-class/aht-expected-output \
			  ut02-stacktrace/aht-input \
			  ut02-stacktrace/aht-output \
			  ut02-stacktrace/aht-expected-output \
			  ut03-function/aht-input \
			  ut03-function/aht-output \
			  ut03-function/aht-expected-output \
			  ut04-c-interface/aht-input \
			  ut04-c-interface/aht-output \
			  ut04-c-interface/aht-expected-output

include ${HD_STD_MAKEFILE}

run-test:
	@status=0; for dir in ${AH_TEST_SUBDIRS}; do \
	  ${HD_MAKE} run-aht HD_SUBDIR=$$dir; \
	  if test $$? -ne 0; then status=1; fi; \
	done; exit $$status

run-aht:
	cd ${HD_SUBDIR}; ${AHT}
