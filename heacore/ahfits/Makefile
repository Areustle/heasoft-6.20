HD_COMPONENT_NAME		= heacore

HD_COMPONENT_VERS		= 

HD_SUBDIRS			= ahfits src

HD_TEST_SUBDIRS			= src/test

include ${HD_STD_MAKEFILE}

run-test:
	@status=0; for dir in ${HD_TEST_SUBDIRS}; do \
	  ${HD_MAKE} subdir HD_SUBDIR=$$dir HD_TARGET=$@; \
	  if test $$? -ne 0; then status=1; fi; \
	done; exit $$status
