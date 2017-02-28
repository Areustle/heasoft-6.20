HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	= 

HD_CTASK		= aberattitude

HD_CTASK_SRC_c		= aberattitude.c

HD_CFLAGS		= ${HD_STD_CXXFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -laber -lahlog_1.0 \
			  -lcoordfits -lcoord -l${ATFUNCTIONS} -l${HEAAPP} ${HD_STD_LIBS} ${SYSLIBS} 

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= aberattitude.par

HD_INSTALL_HEADERS	=

HD_INSTALL_HELP		= aberattitude.html

include ${HD_STD_MAKEFILE}
