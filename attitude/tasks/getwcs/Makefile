HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	= 

HD_CTASK		= getwcs

HD_CTASK_SRC_c		= getwcs.c param.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

LOC_LIBS		= -lcoordfits -lcoord -lparam_wrappers -lephemeris


HD_CLIBS		= ${HD_LFLAGS} ${LOC_LIBS} ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= getwcs.par


HD_INSTALL_HELP		= getwcs.html



include ${HD_STD_MAKEFILE}
