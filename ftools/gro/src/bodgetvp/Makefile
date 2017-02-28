HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= bodgetvp

HD_CTASK_SRC_c		= hbodgetvp.c

HD_CTASK_SRC_f		= bodgetvp.f getdate.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lgro${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= bodgetvp.parin

HD_INSTALL_HELP		= bodgetvp.hlp bodgetvp.txt

include ${HD_STD_MAKEFILE}
