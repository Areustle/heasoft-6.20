HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	= 

HD_CTASK		= aberrator

HD_CTASK_SRC_c		= aberrator.c io.c orbfile.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

TASK_LIBS		= -lrew -lcoordfits -lcoord \
			  -l${ATFUNCTIONS} -l${HEAUTILS}

HD_CLIBS		= ${HD_LFLAGS} ${TASK_LIBS} ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= aberrator.par

HD_INSTALL_HELP		= aberrator.html

HD_TEST_PERL_SCRIPTS	= ut-aberrator

HD_TEST_PFILES		= ut-aberrator.par

include ${HD_STD_MAKEFILE}
