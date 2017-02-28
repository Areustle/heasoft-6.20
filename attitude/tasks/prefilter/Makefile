HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	=

HD_CTASK		= prefilter

HD_CTASK_SRC_c		= driver.c prefilter.c general.c \
			  initialize.c iterate.c derive.c compare.c \
			  fortderive.c convert.c keywords.c pill.c \
			  datetime.c sgp.c misstime.c tle.c tleio.c

HD_CFLAGS		= -I./include ${HD_STD_CFLAGS}

LOC_LIBS		= -lcoordfits -lcoord -l${ATFUNCTIONS} -lrew -lgeomag

HD_CLIBS		= ${HD_LFLAGS} ${LOC_LIBS} ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_TEST_PERL_SCRIPTS	= ut-prefilter

HD_TEST_PFILES		= ut-prefilter.par

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= prefilter.par

HD_INSTALL_HELP		= prefilter.html prefiltimp.html

HD_INSTALL_REFDATA	= refdata/prefilter.derive

include ${HD_STD_MAKEFILE}
