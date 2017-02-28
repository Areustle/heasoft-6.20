HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= dsdlcurve

HD_CTASK_SRC_c		= hdsdlcurve.c

HD_CTASK_SRC_f		= createscan.f dsclwt.f dscprn.f dsdlcurve.f \
			  dsdlcurveinit.f dsdlcurvemake.f dsdwt.f dseff.f \
			  mtrans.f readday.f readraw.f view.f writescan.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -l${CFITSIO} -l${XANLIB} -l${PIL} -l${READLINE} \
			  ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= dsdlcurve.par

HD_INSTALL_HELP		= dsdlcurve.hlp dsdlcurve.txt

include ${HD_STD_MAKEFILE}
