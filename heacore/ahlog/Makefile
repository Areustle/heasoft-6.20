HD_COMPONENT_NAME		= heacore

HD_COMPONENT_VERS		= 

HD_SUBDIRS			= ahlog src

HD_TEST_SUBDIRS			= src/test

include ${HD_STD_MAKEFILE}

run-test:
	@for dir in ${HD_TEST_SUBDIRS}; do \
	  ${HD_MAKE} subdir HD_SUBDIR=$$dir HD_TARGET=$@; \
	  status=$$?; if test $$status -ne 0; then exit $$status; fi; \
	done
