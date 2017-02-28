HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= fimgzip

HD_CTASK_SRC_c		= hfimgzip.c fimgzip.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lfimage${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= fimgzip.par

HD_INSTALL_HELP		= fimgzip.hlp fimgzip.txt

include ${HD_STD_MAKEFILE}
