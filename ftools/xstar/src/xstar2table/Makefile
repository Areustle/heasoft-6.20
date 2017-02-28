HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= xstar2table

HD_CTASK_SRC_c		= hxstar2table.c xstar2table.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lxstar${FT_VERS} -l${CFTOOLSLIB} -l${FTOOLSLIB} \
			  -l${CFITSIO} -l${XANLIB} -l${PIL} -l${READLINE} \
			  ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= xstar2table.par

include ${HD_STD_MAKEFILE}

xstar2table.o: xstar2table.h ../../xstarlib/src/xstartablelib.h \
	       ../../xstarlib/src/xstarutillib.h \
	       ${CFITSIO_DIR}/fitsio.h ${CFITSIO_DIR}/fitsio2.h \
	       ${CFITSIO_DIR}/cfortran.h ${HD_SRC}/xanlib/xpi/xpi.h
