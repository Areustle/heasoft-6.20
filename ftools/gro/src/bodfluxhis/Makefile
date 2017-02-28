HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= bodfluxhis

HD_CTASK_SRC_c		= hbodfluxhis.c bodfluxhis.c checkBinsz.c getBindata.c \
			  getFitsData.c getFname.c getlist.c getnrows.c \
			  getSelectData.c getTjd.c writefits.c writeLog.c

HD_CTASK_SRC_f		= bodfluxhis_plot.f MaxMin.f oned_plot.f pginfo.f \
			  vlineplot.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lgro${FT_VERS} ${LINK_PGPLOT} \
			  ${HD_STD_LIBS} ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= bodfluxhis.par

HD_INSTALL_HELP		= bodfluxhis.hlp bodfluxhis.txt

include ${HD_STD_MAKEFILE}
