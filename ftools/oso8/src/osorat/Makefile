HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= osorat

HD_CTASK_SRC_c		= hosorat.c

HD_CTASK_SRC_f		= osorat.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -l${CFITSIO} -l${XANLIB} -l${PIL} -l${READLINE} \
			  ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= osorat.par

HD_INSTALL_HELP		= osorat.hlp osorat.txt

include ${HD_STD_MAKEFILE}
