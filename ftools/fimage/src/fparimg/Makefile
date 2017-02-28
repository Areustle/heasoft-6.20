HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= fparimg

HD_CTASK_SRC_c		= hfparimg.c fparimg.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lfimage${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= fparimg.par

HD_INSTALL_HELP		= fparimg.hlp fparimg.txt

include ${HD_STD_MAKEFILE}
