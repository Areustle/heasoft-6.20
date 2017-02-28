HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= bcmppha

HD_CTASK_SRC_c		= hbcmppha.c

HD_CTASK_SRC_f		= bcmppha.f curvePlot.f get_bs_chan.f MaxMin.f \
			  vlineplot.f wtpha1p.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lgro${FT_VERS} ${LINK_PGPLOT} \
			  ${HD_STD_LIBS} ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= bcmppha.par

HD_INSTALL_HELP		= bcmppha.hlp bcmppha.txt

include ${HD_STD_MAKEFILE}
