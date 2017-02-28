HD_COMPONENT_NAME	= swift 

HD_COMPONENT_VERS	=

HD_CTASK		= batmasktaglc

HD_CTASK_SRC_c		= batmasktaglc.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lbatutils \
			  -lbatmaskutils ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= batmasktaglc.par

HD_INSTALL_HELP		= batmasktaglc.html

HD_TEST_PERL_SCRIPTS    = ut_batmasktaglc

HD_TEST_PFILES          = ut_batmasktaglc.par


include ${HD_STD_MAKEFILE}
