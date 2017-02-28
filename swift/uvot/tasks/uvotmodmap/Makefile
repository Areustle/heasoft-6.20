HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	= 

HD_CTASK		= uvotmodmap

HD_CTASK_SRC_c		= uvotmodmap.c vecmat.c fastMatrixOps.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lswuvot -lrew ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_TEST_PERL_SCRIPTS	= ut-uvotmodmap

HD_INSTALL_PFILES	= uvotmodmap.par

HD_TEST_PFILES		= ut-uvotmodmap.par

HD_INSTALL_HELP		= uvotmodmap.html

include ${HD_STD_MAKEFILE}
