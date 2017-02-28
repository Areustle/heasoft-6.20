HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_CTASK		= batimgstatpos

HD_CTASK_SRC_c		= batimgstatpos.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lbatimageutils -lbatfftsg \
			  -lbatmaskutils -lcoordfits -lcoord -lbatutils \
			  ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= batimgstatpos.par

HD_INSTALL_HELP		= batimgstatpos.html

HD_TEST_PERL_SCRIPTS	= ut_batimgstatpos

HD_TEST_PFILES		= ut_batimgstatpos.par

include ${HD_STD_MAKEFILE}

