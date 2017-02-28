HD_COMPONENT_NAME	= heatools

HD_COMPONENT_VERS	=

HD_CTASK		= ftimgcalc

HD_CTASK_SRC_c		= util.c ftimgcalc.c wcs.c parse.c

HD_CFLAGS		= ${HD_STD_CFLAGS} ${TEST_INCLUDE}

HD_CLIBS		= ${HD_LFLAGS} -l${WCSLIB} ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= ftimgcalc.par

HD_INSTALL_HELP		= ftimgcalc.html

HD_TEST_PERL_SCRIPTS    = ut_ftimgcalc

HD_TEST_PFILES          = ut_ftimgcalc.par

include ${HD_STD_MAKEFILE}

${HD_CTASK} ${HD_CTASK_SRC_c:.c=.${OSUF}}: ftimgcalc.h
