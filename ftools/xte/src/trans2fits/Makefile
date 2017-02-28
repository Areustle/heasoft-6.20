HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= trans2fits

HD_CTASK_SRC_c		= htrans2fits.c trans2fits.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lxte${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= trans2fits.par

HD_INSTALL_HELP		= trans2fits.hlp trans2fits.txt

include ${HD_STD_MAKEFILE}
