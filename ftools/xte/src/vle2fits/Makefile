HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= vle2fits

HD_CTASK_SRC_c		= hvle2fits.c vle2fits.c addeventv.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lxte${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= vle2fits.par

HD_INSTALL_HELP		= vle2fits.hlp vle2fits.txt

include ${HD_STD_MAKEFILE}
