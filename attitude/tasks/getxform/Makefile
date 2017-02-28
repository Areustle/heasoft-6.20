HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	= 

HD_CTASK		= getxform

HD_CTASK_SRC_c		= getxform.c param.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

LOC_LIBS		= -lcoordfits -lcoord -lparam_wrappers -lephemeris


HD_CLIBS		= ${HD_LFLAGS} ${LOC_LIBS} ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= getxform.par

HD_INSTALL_HELP		= getxform.html


include ${HD_STD_MAKEFILE}
