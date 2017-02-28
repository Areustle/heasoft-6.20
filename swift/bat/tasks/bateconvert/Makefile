HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_CTASK		= bateconvert

HD_CTASK_SRC_c		= bateconvert.c 

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lbatutils \
                          ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= bateconvert.par

HD_INSTALL_HELP		= bateconvert.html

HD_TEST_PERL_SCRIPTS    = ut_bateconvert_1 ut_bateconvert_2 ut_bateconvert_3 \
			  ut_bateconvert_4

HD_TEST_PFILES          = ut_bateconvert.par

include ${HD_STD_MAKEFILE}
