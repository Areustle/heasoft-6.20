HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= bod2rmf

HD_CTASK_SRC_c		= hbod2rmf.c

HD_CTASK_SRC_f		= bod2rmf.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lgro${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= bod2rmf.parin

HD_INSTALL_HELP		= bod2rmf.hlp bod2rmf.txt

HD_INSTALL_REFDATA	= inst_resp.fits

include ${HD_STD_MAKEFILE}
