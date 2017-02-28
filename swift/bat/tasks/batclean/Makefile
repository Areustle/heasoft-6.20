HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_CTASK		= batclean

HD_CTASK_SRC_c		= batclean.c 

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lbatutils \
			  -lbatmaskutils \
			  ${HD_STD_LIBS} ${SYSLIBS} 

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= batclean.par

HD_INSTALL_HELP		= batclean.html

HD_TEST_PERL_SCRIPTS    = ut_batclean_1 ut_batclean_2 ut_batclean_3 ut_batclean_4

HD_TEST_PFILES          = ut_batclean.par


include ${HD_STD_MAKEFILE}
