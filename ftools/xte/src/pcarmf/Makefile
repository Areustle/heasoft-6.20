HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= pcarmf

HD_CTASK_SRC_c		= hpcarmf.c

HD_CTASK_SRC_f		= pcarmf.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lxte${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= pcarmf.par

HD_INSTALL_HELP		= pcarmf.hlp pcarmf.txt

include ${HD_STD_MAKEFILE}
