HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_CTASK		= batmaskwtimg

HD_CTASK_SRC_c		= batmaskwtimg.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lbatimageutils -lbatmaskutils -lcoordfits -lcoord \
			  -lbatfftsg -lbatutils ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= batmaskwtimg.par

HD_INSTALL_HELP		= batmaskwtimg.html

HD_TEST_PERL_SCRIPTS	= ut_batmaskwtimg

HD_TEST_PFILES		= ut_batmaskwtimg.par

include ${HD_STD_MAKEFILE}
