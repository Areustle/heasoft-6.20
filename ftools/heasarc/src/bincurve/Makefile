HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= bincurve

HD_CTASK_SRC_c		= hbincurve.c

HD_CTASK_SRC_f		= bincurve.f ../binspec/binspec.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_STD_CLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= bincurve.par

HD_INSTALL_HELP		= bincurve.hlp bincurve.txt

HD_CLEAN		= ${HD_CTASK_SRC_f:.f=.o}

include ${HD_STD_MAKEFILE}
