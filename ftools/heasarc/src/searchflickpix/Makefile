HD_COMPONENT_NAME       = ftools

HD_COMPONENT_VERS       =

HD_CTASK              	= searchflickpix

HD_CTASK_SRC_c      	= ${HD_CTASK}.c

HD_CFLAGS             	= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} ${HD_STD_CLIBS} -l${HEAAPP} \
			  -l${HEAUTILS} -l${AHLOG} ${F77LIBS4C} ${SYSLIBS}

HD_INSTALL_TASKS        = ${HD_CTASK}

HD_TEST_SUBDIRS		= 

HD_INSTALL_PFILES       = ${HD_CTASK}.par

HD_INSTALL_HELP         = ${HD_CTASK}.html

include ${HD_STD_MAKEFILE}

run-test:
	@status=0; for dir in ${HD_TEST_SUBDIRS}; do \
          ${HD_MAKE} subdir HD_SUBDIR=$$dir HD_TARGET=$@; \
          if test $$? -ne 0; then status=1; fi; \
        done; exit $$status
