HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_CTASK		= baterebin

HD_CTASK_SRC_c		= baterebin.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lbatimageutils -lbatfftsg -lbatutils \
                          ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= baterebin.par

HD_INSTALL_HELP		= baterebin.html

HD_TEST_PERL_SCRIPTS    = ut_baterebin

HD_TEST_PFILES          = ut_baterebin.par

include ${HD_STD_MAKEFILE}
