HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= fimgunzip

HD_CTASK_SRC_c		= hfimgunzip.c fimgunzip.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lfimage${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= fimgunzip.par

HD_INSTALL_HELP		= fimgunzip.hlp fimgunzip.txt

include ${HD_STD_MAKEFILE}
