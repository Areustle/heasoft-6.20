HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	= 

HD_CTASK		= coordevt

HD_CTASK_SRC_c		= coordevt.c coordevtlib.c

HD_CFLAGS		= ${HD_STD_CXXFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -laber -lahlog_1.0 \
			  -lrew -lcoordfits -lcoord -lrandom -l${ATFUNCTIONS} -lparam_wrappers -l${HEAAPP} ${HD_STD_LIBS} ${SYSLIBS} 

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= coordevt.par

HD_INSTALL_HEADERS	=

HD_INSTALL_HELP		= coordevt.html

include ${HD_STD_MAKEFILE}
