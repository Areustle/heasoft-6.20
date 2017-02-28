HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	= 

HD_CTASK		= attcombine

HD_CTASK_SRC_c		= attcombine.c param.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

TASK_LIBS		= -lrew -lcoordfits -lcoord -lparam_wrappers
HD_CLIBS		= ${HD_LFLAGS} ${TASK_LIBS} ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= attcombine.par

HD_INSTALL_HEADERS	=

HD_INSTALL_HELP		= attcombine.html

include ${HD_STD_MAKEFILE}
