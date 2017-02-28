HD_COMPONENT_NAME	= heacore

HD_COMPONENT_VERS	=

HD_TEST_CTASK		= test_heainit

HD_TEST_CTASK_SRC_c	= ${HD_TEST_CTASK}.c

HD_TEST_TASKS		= ${HD_TEST_CTASK}

# When unit test in ut01 directory works correctly, uncomment this macro.
#TEST_SUBDIRS		= ut01

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} ${HD_STD_LIBS} -l${AHLOG} ${SYSLIBS}

HD_TEST_PFILES		= test_heainit.par

include ${HD_STD_MAKEFILE}

# When unit test in ut01 directory works correctly, uncomment these targets.
# run-test:
# 	@status=0; for dir in ${TEST_SUBDIRS}; do \
# 	  ${HD_MAKE} run-aht HD_SUBDIR=$$dir; \
# 	  if test $$? -ne 0; then status=1; fi; \
# 	done; exit $$status
# 
# run-aht:
# 	cd ${HD_SUBDIR}; aht
