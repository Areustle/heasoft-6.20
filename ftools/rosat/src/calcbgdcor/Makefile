HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= calcbgdcor

HD_CTASK_SRC_c		= hcalcbgdcor.c

HD_CTASK_SRC_f		= calcbgdcor.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lros${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= calcbgdcor.par

HD_INSTALL_HELP		= calcbgdcor.hlp calcbgdcor.txt

include ${HD_STD_MAKEFILE}
