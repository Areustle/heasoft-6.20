HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_CTASK		= uvotshiftadd

HD_CTASK_SRC_c		= uvotshiftadd.c exposureMapShift_Add.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

LOC_LIBS		= -g -lswuvot -lrew

HD_CLIBS		= ${HD_LFLAGS} ${LOC_LIBS} ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_PERL_SCRIPTS	= 

HD_TEST_PERL_SCRIPTS	= ut-uvotshiftadd

HD_TEST_PFILES		= ut-uvotshiftadd.par

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= uvotshiftadd.par

HD_INSTALL_HELP		= uvotshiftadd.html

include ${HD_STD_MAKEFILE}
