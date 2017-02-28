HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_SUBDIRS		= uvotshiftadd uvotfakeaf

HD_CTASK		= uvotinstmap

HD_CTASK_SRC_c		= uvotinstmap.c instmap.c

HD_CFLAGS		= -I./include ${HD_STD_CFLAGS}

LOC_LIBS		= -lcoordfits -lcoord -lrew

HD_CLIBS		= ${HD_LFLAGS} ${LOC_LIBS} ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_PERL_SCRIPTS	= uvotexpmap

HD_TEST_PERL_SCRIPTS	= ut-uvotexpmap

HD_TEST_PFILES		= ut-uvotexpmap.par

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= uvotexpmap.par uvotinstmap.par

HD_INSTALL_HELP		= uvotexpmap.html

include ${HD_STD_MAKEFILE}
