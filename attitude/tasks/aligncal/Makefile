HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	= 

HD_CTASK		= aligncal

HD_CTASK_SRC_c		= aligncal.c param.c points.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lcoord -lcoordfits -lparam_wrappers ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= aligncal.par

HD_INSTALL_HEADERS	= param.h points.h

include ${HD_STD_MAKEFILE}
