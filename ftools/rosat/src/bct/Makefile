HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= bct

HD_CTASK_SRC_c		= hbct.c

HD_CTASK_SRC_f		= ar_jdcn.f bcread.f bct.f bctinit.f bctmake.f bcwrite.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lros${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= bct.parin

HD_INSTALL_HELP		= bct.hlp bct.txt

include ${HD_STD_MAKEFILE}
