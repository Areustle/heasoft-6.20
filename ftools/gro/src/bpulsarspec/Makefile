HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= bpulsarspec

HD_CTASK_SRC_c		= hbpulsarspec.c bpulsarspec.c getBinData.c getChan.c \
			  getExpos.c getFitsData.c getFlux.c getFname.c \
			  getHardn.c getHardnPhas.c getPhasData.c getPhase.c \
			  getpulsfn.c getSelectData.c getSelectPhas.c \
			  getSumData.c getTjd.c saveHphs.c saveHtjd.c \
			  writefits.c writeRates.c

HD_CTASK_SRC_f		= bp_onedplot.f fluxplot.f hrphasplot.f MaxMin.f \
			  pulsplot.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lgro${FT_VERS} ${LINK_PGPLOT} \
			  ${HD_STD_LIBS} ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= bpulsarspec.par

HD_INSTALL_HELP		= bpulsarspec.hlp bpulsarspec.txt

include ${HD_STD_MAKEFILE}
