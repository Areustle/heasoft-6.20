HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= pcfilt

HD_CTASK_SRC_c		= hpcfilt.c

HD_CTASK_SRC_f		= pcfilt.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lros${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= pcfilt.par

HD_INSTALL_HELP		= pcfilt.hlp pcfilt.txt

include ${HD_STD_MAKEFILE}
