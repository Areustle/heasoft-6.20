HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= barytime

HD_CTASK_SRC_c		= arrtim.c barytime.c delay.c getpar.c hbarytime.c \
			  timekeywds.c wrthdr.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lgro${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= barytime.par

HD_INSTALL_HELP		= barytime.hlp barytime.txt

include ${HD_STD_MAKEFILE}
