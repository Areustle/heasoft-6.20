HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= fcarith

HD_CTASK_SRC_c		= hfcarith.c

HD_CTASK_SRC_f		= fcarith.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lfimage${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= fcarith.par

HD_INSTALL_HELP		= fcarith.hlp fcarith.txt

include ${HD_STD_MAKEFILE}
