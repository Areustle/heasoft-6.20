HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_CTASK		= batbinevt

HD_CTASK_SRC_c		= batbinevt.c binning.c fileio.c spect.c \
			  events.c dphread.c readmisc.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lbatutils ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= batbinevt.par

HD_INSTALL_HELP		= batbinevt.html

HD_TEST_PERL_SCRIPTS    = ut_batbinevt_lc ut_batbinevt_pha \
			  ut_batbinevt_dpi ut_batbinevt_dph \
			  ut_batbinevt_dph2dpi ut_batbinevt_pha1 \
			  ut_batbinevt_dph2pha

HD_TEST_PFILES          = ut_batbinevt_lc.par ut_batbinevt_pha.par \
			  ut_batbinevt_dpi.par ut_batbinevt_dph.par \
			  ut_batbinevt_dph2dpi.par \
			  ut_batbinevt_dph2pha.par

include ${HD_STD_MAKEFILE}

${HD_CTASK} ${HD_CTASK_SRC_c:.c=.${OSUF}}: batbinevt.h
