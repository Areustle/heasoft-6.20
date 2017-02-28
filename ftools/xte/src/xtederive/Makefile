HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= xtederive

HD_CTASK_SRC_c		= hxtederive.c

HD_CTASK_SRC_f		= alansubs.f starksubs.f todsubs.f xtederive.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lxte${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= xtederive.par

HD_INSTALL_HELP		= xtederive.hlp xtederive.txt

include ${HD_STD_MAKEFILE}
