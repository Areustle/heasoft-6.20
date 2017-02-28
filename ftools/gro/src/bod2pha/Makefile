HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= bod2pha

HD_CTASK_SRC_c		= hbod2pha.c namepha.c

HD_CTASK_SRC_f		= bod2pha.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lgro${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= bod2pha.par

HD_INSTALL_HELP		= bod2pha.hlp bod2pha.txt

include ${HD_STD_MAKEFILE}
