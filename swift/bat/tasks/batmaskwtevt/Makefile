HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_CTASK		= batmaskwtevt

HD_CTASK_SRC_c		= batmaskwtevt.c 

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lbatimageutils -lbatmaskutils -lcoordfits -lcoord \
			  -lbatfftsg -lbatutils ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= batmaskwtevt.par

HD_INSTALL_HELP		= batmaskwtevt.html

HD_TEST_PERL_SCRIPTS	= ut_batmaskwtevt

HD_TEST_PFILES		= ut_batmaskwtevt.par

include ${HD_STD_MAKEFILE}
