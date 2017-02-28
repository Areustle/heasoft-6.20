HD_COMPONENT_NAME	= heacore

HD_COMPONENT_VERS	= 

HD_TEST_CTASK		= ape_test

HD_TEST_CTASK_SRC_c	= ape_test_main.c

HD_CFLAGS		= -I. -I.. -I../../include ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -l${PIL} -l${READLINE} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_CLEAN		= pfiles ape_large.par ape_test.par ape_test-log \
			  ape_test_unix_copy.par ape_test_win32_copy.par

APETEST			= ./${HD_TEST_CTASK}

default: test

all: default

include ${HD_STD_MAKEFILE}

run-test: pfiles test clean-test ape_test_unix.par ape_test_win32.par
	@echo "unset PFILES; export PFILES"; \
	unset PFILES; export PFILES; \
	echo "cp -p unix-ref/ape_test.par pfiles/ape_test.par"; \
	cp -p unix-ref/ape_test.par pfiles/ape_test.par; \
	echo "cat ape_test-input | ${APETEST} ape_test-log"; \
	cat ape_test-input | ${APETEST} ape_test-log; status=$$?; \
	echo "diff unix-ref/ape_test-log ape_test-log"; \
	diff unix-ref/ape_test-log ape_test-log; \
	echo "diff unix-ref/ape_test.par ape_test.par"; \
	diff unix-ref/ape_test.par ape_test.par; \
	echo "diff unix-ref/ape_test.par pfiles/ape_test.par"; \
	diff unix-ref/ape_test.par pfiles/ape_test.par; \
	echo "diff pfiles/ape_test.par ape_test.par"; \
	diff pfiles/ape_test.par ape_test.par; \
	echo "diff unix-ref/ape_test.par ape_test_unix_copy.par"; \
	diff unix-ref/ape_test.par ape_test_unix_copy.par; \
	echo "diff -w win32-ref/ape_test.par ape_test_win32_copy.par"; \
	diff -w win32-ref/ape_test.par ape_test_win32_copy.par; \
	exit $$status
#	cat ape_test-input | ${APETEST} ape_test-log; status=$$?; \
#	cat ape_test-input | valgrind --leak-check=yes --show-reachable=yes --suppressions=valgrind.supp ${APETEST} ape_test-log; status=$$?; \

win-test: clean-test ape_test_unix.par ape_test_win32.par
	@echo "unset PFILES; export PFILES"; \
	unset PFILES; export PFILES; \
	echo "cp -p win32-ref/ape_test.par pfiles/ape_test.par"; \
	cp -p win32-ref/ape_test.par pfiles/ape_test.par; \
	echo "cat ape_test-input | ${WINAPETEST} ape_test-log"; \
	cat ape_test-input | ${WINAPETEST} ape_test-log; status=$$?; \
	echo "diff win32-ref/ape_test-log ape_test-log"; \
	diff win32-ref/ape_test-log ape_test-log; \
	echo "diff win32-ref/ape_test.par ape_test.par"; \
	diff win32-ref/ape_test.par ape_test.par; \
	echo "diff win32-ref/ape_test.par pfiles/ape_test.par"; \
	diff win32-ref/ape_test.par pfiles/ape_test.par; \
	echo "diff pfiles/ape_test.par ape_test.par"; \
	diff pfiles/ape_test.par ape_test.par; \
	echo "diff win32-ref/ape_test.par ape_test_win32_copy.par"; \
	diff win32-ref/ape_test.par ape_test_win32_copy.par; \
	echo "diff -w unix-ref/ape_test.par ape_test_unix_copy.par"; \
	diff -w unix-ref/ape_test.par ape_test_unix_copy.par; \
	exit $$status

clean: clean-test ${HD_TARG_PFX}clean

distclean: clean-test ${HD_TARG_PFX}distclean

clean-test:
	rm -f ape_large.par ape_test-log ape_test.par ape_test_unix.par ape_test_win32.par ape_test_unix_copy.par ape_test_win32_copy.par pfiles/ape_test.par

pfiles:
	mkdir -p $@

ape_test_unix.par: unix-ref/ape_test.par ape_test_pfile_writer
	./ape_test_pfile_writer unix-ref/ape_test.par $@

ape_test_win32.par: win32-ref/ape_test.par ape_test_pfile_writer
	./ape_test_pfile_writer win32-ref/ape_test.par $@

