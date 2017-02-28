HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	= 

HD_CTASK		= aspect

HD_CTASK_SRC_c		= arrays.c aspectbins.c aspect.c gtis.c param.c writeatt.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lrew -lcoordfits -lcoord -lparam_wrappers ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= aspect.par

HD_INSTALL_HEADERS	=

HD_INSTALL_HELP		= aspect.hlp aspect.txt

include ${HD_STD_MAKEFILE}
