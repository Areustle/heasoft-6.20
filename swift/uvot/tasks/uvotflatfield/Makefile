HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	= 

HD_CTASK		= uvotflatfield

HD_CTASK_SRC_c		= uvotflatfield.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

TASK_LIBS		= -luvotcal -lswuvot -lrew

HD_CLIBS		= ${HD_LFLAGS} ${TASK_LIBS} ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= uvotflatfield.par

HD_INSTALL_HELP		= uvotflatfield.html

HD_TEST_PERL_SCRIPTS    = ut-uvotflatfield

HD_TEST_PFILES          = ut-uvotflatfield.par

include ${HD_STD_MAKEFILE}
