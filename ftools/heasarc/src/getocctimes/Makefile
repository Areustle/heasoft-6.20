HD_COMPONENT_NAME       = ftools

HD_COMPONENT_VERS       =

HD_CTASK                = getocctimes

HD_CTASK_SRC_c          = getocctimes.c

HD_CFLAGS               = ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -l${HEAINIT} -l${HEAUTILS} -l${PIL} \
			  -l${CFITSIO} -l${READLINE} -l${HEAIO} \
			  ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS        = ${HD_CTASK}

HD_INSTALL_PFILES       = getocctimes.par

HD_INSTALL_HELP         = getocctimes.html

include ${HD_STD_MAKEFILE}
