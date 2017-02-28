HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= oldaspect

HD_CTASK_SRC_c		= holdaspect.c oldaspect.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= oldaspect.par

HD_INSTALL_HELP		= oldaspect.hlp oldaspect.txt

include ${HD_STD_MAKEFILE}
