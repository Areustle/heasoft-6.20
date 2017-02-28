HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_CTASK		= battblocks

HD_CTASK_SRC_c		= battblocks.c bbevts.c bblc.c bblcgauss.c burstdur.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS}    ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= battblocks.par

HD_INSTALL_HELP		= battblocks.html

HD_TEST_PERL_SCRIPTS    = ut_battblocks_evt ut_battblocks_poi \
			  ut_battblocks_gau

HD_TEST_PFILES		= ut_battblocks_evt.par ut_battblocks_poi.par \
			  ut_battblocks_gau.par

include ${HD_STD_MAKEFILE}

${HD_CTASK} ${HD_CTASK_SRC_c:.c=.${OSUF}}: battblocks.h
