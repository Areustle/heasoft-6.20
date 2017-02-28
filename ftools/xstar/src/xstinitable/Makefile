HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= xstinitable

HD_CTASK_SRC_c		= hxstinitable.c xstinitable.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lxstar${FT_VERS} -l${CFTOOLSLIB} -l${FTOOLSLIB} \
			  -l${CFITSIO} -l${XANLIB} -l${PIL} -l${READLINE} \
			  ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= xstinitable.par

include ${HD_STD_MAKEFILE}

xstinitable.o: xstinitable.h ../../xstarlib/src/xstartablelib.h \
	       ../../xstarlib/src/xstarutillib.h \
	       ${CFITSIO_DIR}/fitsio.h ${CFITSIO_DIR}/fitsio2.h \
	       ${CFITSIO_DIR}/cfortran.h ${HD_SRC}/xanlib/xpi/xpi.h
