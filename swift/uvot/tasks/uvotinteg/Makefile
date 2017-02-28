HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	= 

HD_CTASK		= uvotinteg

HD_CTASK_SRC_c		= uvotinteg.c ximreg.c clip.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

TASK_LIBS		= -lrew -l${HEAUTILS}

HD_CLIBS		= ${HD_LFLAGS} ${TASK_LIBS} ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= uvotinteg.par

HD_INSTALL_HELP		= uvotinteg.html

HD_TEST_PERL_SCRIPTS	= ut-uvotinteg

HD_TEST_PFILES		= ut-uvotinteg.par

include ${HD_STD_MAKEFILE}
