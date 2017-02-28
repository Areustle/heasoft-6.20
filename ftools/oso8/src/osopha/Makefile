HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= osopha

HD_CTASK_SRC_c		= hosopha.c

HD_CTASK_SRC_f		= angle.f caaab.f osocl_rates_bc.f \
			  osocr_rates_abc.f osocr_spec.f osopha.f \
			  osophagpar.f osophamake_bc.f \
			  oso_rdraw.f osowr_rates_bc.f osowr_spec.f
# unused: osophamake_a.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -l${CFITSIO} -l${XANLIB} -l${PIL} -l${READLINE} \
			  ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= osopha.par

HD_INSTALL_HELP		= osopha.hlp osopha.txt

include ${HD_STD_MAKEFILE}
