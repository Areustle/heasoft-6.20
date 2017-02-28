HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_CTASK		= batwarpimg

HD_CTASK_SRC_c		= batwarpimg.c

HD_CFLAGS		= ${HD_STD_CFLAGS} ${TEST_INCLUDE}

HD_CLIBS		= ${HD_LFLAGS} -lbatimageutils -lbatfftsg -lbatutils \
			  ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= batwarpimg.par

HD_INSTALL_HELP		= batwarpimg.html

HD_TEST_PERL_SCRIPTS    = ut_batwarpimg

HD_TEST_PFILES          = ut_batwarpimg.par

include ${HD_STD_MAKEFILE}

${HD_CTASK} ${HD_CTASK_SRC_c:.c=.${OSUF}}: batwarpimg.h
