HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	= 

HD_CTASK		= uvotbadpix

HD_CTASK_SRC_c		= uvotbadpix.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

LOCALLIBS = -luvotcal -lswuvot -lrew
HD_CLIBS		= ${HD_LFLAGS} ${LOCALLIBS} ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_TEST_PERL_SCRIPTS	= ut-uvotbadpix

HD_INSTALL_PFILES	= uvotbadpix.par

HD_TEST_PFILES		= ut-uvotbadpix.par

HD_INSTALL_HELP		= uvotbadpix.html

include ${HD_STD_MAKEFILE}
