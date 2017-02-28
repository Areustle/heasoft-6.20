HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= fastwmap

HD_CTASK_SRC_c		= hfastwmap.c

HD_CTASK_SRC_f		= fastwmap.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= fastwmap.par

HD_INSTALL_HELP		= fastwmap.hlp fastwmap.txt

include ${HD_STD_MAKEFILE}
