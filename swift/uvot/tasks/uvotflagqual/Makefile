HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	= 

HD_CTASK		= uvotflagqual

HD_CTASK_SRC_c		= uvotflagqual.c flagqual.c findbright.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

TASK_LIBS		= -luvotcal -lswuvot -lrew

HD_CLIBS		= ${HD_LFLAGS} ${TASK_LIBS} ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= ${HD_CTASK}.par

HD_INSTALL_HELP		= ${HD_CTASK}.html

HD_TEST_PERL_SCRIPTS	= ut-${HD_CTASK}

HD_TEST_PFILES		= ut-${HD_CTASK}.par

include ${HD_STD_MAKEFILE}
