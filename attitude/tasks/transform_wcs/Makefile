HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	= 

HD_CTASK		= transform_wcs

HD_CTASK_SRC_c		= param.c transform_wcs.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

LOC_LIBS		= -lcoordfits -lcoord -lparam_wrappers -lephemeris

HD_CLIBS		= ${HD_LFLAGS} ${LOC_LIBS}  ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= transform_wcs.par

HD_INSTALL_HELP		= transform_wcs.html

include ${HD_STD_MAKEFILE}
