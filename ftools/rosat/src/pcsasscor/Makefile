HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= pcsasscor

HD_CTASK_SRC_c		= hpcsasscor.c

HD_CTASK_SRC_f		= pcsasscor.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lros${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= pcsasscor.par

HD_INSTALL_HELP		= pcsasscor.hlp pcsasscor.txt

include ${HD_STD_MAKEFILE}
