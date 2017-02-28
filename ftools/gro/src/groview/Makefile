HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= groview

HD_CTASK_SRC_c		= hgroview.c CmpView.c EgretView.c getAdxyz.c getAng.c \
			  getAngThresh.c getBatseView.c getCross.c \
			  getData.c getDecTime.c getDecZ.c getDotProd.c \
			  getEuler.c getGroView.c getNorCord.c getRaZ.c \
			  getSource.c getSpaceCor.c getTime.c getViewFlag.c \
			  getXyzad.c getZlongLat.c groview.c rad2Deg.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lgro${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= groview.par

HD_INSTALL_HELP		= groview.hlp groview.txt

include ${HD_STD_MAKEFILE}
