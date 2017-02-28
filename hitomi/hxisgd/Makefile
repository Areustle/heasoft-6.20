CVSRCS_ID           := "$$Name: heasoft6_20_20170113 $$ $$Id: Makefile,v 1.12 2016/02/18 12:53:49 klrutkow Exp $$"

#$(info $$Name: heasoft6_20_20170113 $$ $$Id: Makefile,v 1.12 2016/02/18 12:53:49 klrutkow Exp $$)

HD_COMPONENT_NAME   = hitomi

HD_COMPONENT_VERS   = 

HD_SUBDIRS          = lib tasks

HD_TEST_SUBDIRS     = lib ut

include ${HD_STD_MAKEFILE}

run-test:
	@status=0; for dir in ${HD_TEST_SUBDIRS}; do \
	  ${HD_MAKE} subdir HD_SUBDIR=$$dir HD_TARGET=$@; \
	  if test $$? -ne 0; then status=1; fi; \
	done; exit $$status
