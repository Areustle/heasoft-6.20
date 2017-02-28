HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= addshots

HD_CTASK_SRC_c		= haddshots.c

HD_CTASK_SRC_f		= addshots.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lxte${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= addshots.par

HD_INSTALL_HELP		= addshots.hlp addshots.txt

include ${HD_STD_MAKEFILE}
