HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	= 

HD_CTASK		= coordinator

HD_CTASK_SRC_c		= coordinator.c info.c keywords.c param.c earthvel.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lrew -lcoordfits -lcoord -lrandom \
			  -l${ATFUNCTIONS} -lparam_wrappers \
			  ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= coordinator.par

HD_INSTALL_HEADERS	=

HD_INSTALL_HELP		= coordinator.html

include ${HD_STD_MAKEFILE}
