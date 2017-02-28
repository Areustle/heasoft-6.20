HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= bcont

HD_CTASK_SRC_c		= hbcont.c bcont.c getBcontFn.c getBinData.c getChan.c \
			  getDet.c getDetData.c getFitsData.c getFname.c \
			  getRates.c getSelectData.c getSumData.c getTjd.c \
			  getwtFlg.c writeAsiic.c writeFits.c

HD_CTASK_SRC_f		= bgetdet.f bplot.f getcurs.f getMaxMin.f MaxMin.f \
			  onedplot.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lgro${FT_VERS} ${LINK_PGPLOT} \
			  ${HD_STD_LIBS} ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= bcont.par

HD_INSTALL_HELP		= bcont.hlp bcont.txt

include ${HD_STD_MAKEFILE}
