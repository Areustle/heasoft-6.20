HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	= 

HD_CTASK		= uvotapplywcs

HD_CTASK_SRC_c		= uvotapplywcs.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

LOCALLIBS = -lrew -l${WCSLIB}
HD_CLIBS		= ${HD_LFLAGS} ${LOCALLIBS} ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_TEST_PERL_SCRIPTS	= ut-uvotapplywcs

HD_INSTALL_PFILES	= uvotapplywcs.par

HD_TEST_PFILES		= ut-uvotapplywcs.par

HD_INSTALL_HELP		= uvotapplywcs.html

include ${HD_STD_MAKEFILE}
