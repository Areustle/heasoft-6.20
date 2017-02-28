HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	= 

HD_CTASK		= uvotevgrism

HD_CTASK_SRC_c		= uvotevgrism.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

TASK_LIBS		= -lcoordfits -lcoord -lrew -luvotcal
TASK_LIBS		= -lcoordfits -lcoord -lrew

HD_CLIBS		= ${HD_LFLAGS} ${TASK_LIBS} ${HD_STD_LIBS} ${SYSLIBS}

HD_TEST_PERL_SCRIPTS    = ut-uvotevgrism

HD_TEST_PFILES          = ut-uvotevgrism.par

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= uvotevgrism.par

HD_INSTALL_HELP		= uvotevgrism.html

include ${HD_STD_MAKEFILE}
