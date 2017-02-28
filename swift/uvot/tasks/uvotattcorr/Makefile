HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	= 

HD_CTASK		= uvotattcorr1

HD_CTASK_SRC_c		= uvotattcorr1.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

TASK_LIBS		= -lrew -lcoordfits -lcoord
HD_CLIBS		= ${HD_LFLAGS} ${TASK_LIBS} ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PERL_SCRIPTS	= uvotattcorr

HD_TEST_PERL_SCRIPTS	= ut-uvotattcorr

HD_INSTALL_PFILES	= uvotattcorr.par uvotattcorr1.par

HD_INSTALL_HEADERS	=

HD_INSTALL_HELP		= uvotattcorr.html


include ${HD_STD_MAKEFILE}
