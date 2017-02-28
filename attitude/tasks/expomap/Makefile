HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	=

HD_CTASK		= expomap

HD_CTASK_SRC_c		= expomap.c mapper.c expio.c detmap.c

HD_CFLAGS		= -I./include ${HD_STD_CFLAGS}

LOC_LIBS		= -lcoordfits -lcoord -lrew

HD_CLIBS		= ${HD_LFLAGS} ${LOC_LIBS} ${HD_STD_LIBS} ${SYSLIBS}

HD_TEST_PERL_SCRIPTS	= ut-expomap

HD_TEST_PFILES		= ut-expomap.par

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= expomap.par

HD_INSTALL_HELP		= expomap.html

include ${HD_STD_MAKEFILE}
