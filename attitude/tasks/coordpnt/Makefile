HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	= 

HD_CTASK		= coordpnt

HD_CTASK_SRC_c		= coordpnt.c coordpntlib.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lahlog_1.0 \
			  -lcoordfits -lcoord -l${HEAAPP} ${HD_STD_LIBS} ${SYSLIBS} 

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= coordpnt.par

HD_INSTALL_HEADERS	=

HD_INSTALL_HELP		= coordpnt.html

include ${HD_STD_MAKEFILE}
