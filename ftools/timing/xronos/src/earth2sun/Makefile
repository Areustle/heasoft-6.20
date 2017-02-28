HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= earth2sun

HD_CTASK_SRC_c		= hearth2sun.c

HD_CTASK_SRC_f		= a1utcf.f barycen.f earth2sun.f readeph.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lxron${FT_VERS} -l${CFITSIO} -lplt \
			  -l${XANLIB} -l${PIL} -l${READLINE} ${LINK_PGPLOT} \
			  ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= earth2sun.parin

HD_INSTALL_HELP		= earth2sun.hlp earth2sun.txt

include ${HD_STD_MAKEFILE}
