HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_CTASK		= bathotpix

HD_CTASK_SRC_c		= bathotpix.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lbatimageutils -lbatmaskutils \
			  -lbatfftsg -lbatutils ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= bathotpix.par

HD_INSTALL_HELP		= bathotpix.html

HD_TEST_PERL_SCRIPTS    = ut_bathotpix

HD_TEST_PFILES		= ut_bathotpix.par

include ${HD_STD_MAKEFILE}
