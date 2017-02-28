HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	= 

HD_CTASK		= draw_teldef

HD_CTASK_SRC_c		= draw_teldef.c param.c postscript.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lcoordfits -lcoord -lparam_wrappers ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= draw_teldef.par

HD_INSTALL_HEADERS	=

HD_INSTALL_HELP		= draw_teldef.hlp draw_teldef.txt

include ${HD_STD_MAKEFILE}
