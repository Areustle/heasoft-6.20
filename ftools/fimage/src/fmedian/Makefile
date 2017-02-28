HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= fmedian

HD_CTASK_SRC_c		= hfmedian.c getmedian.c

HD_CTASK_SRC_f		= fmedian.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lfimage${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= fmedian.par

HD_INSTALL_HELP		= fmedian.hlp fmedian.txt

include ${HD_STD_MAKEFILE}
