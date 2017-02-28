HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_CTASK		= batdrmgen

HD_CTASK_SRC_c		= batdrmgen.c respio.c calc.c lookup_params.c multi_mutau_func.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lbatutils ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PERL_SCRIPTS	= batdrmgen-multi

HD_INSTALL_PFILES	= batdrmgen.par batdrmgen-multi.par

HD_INSTALL_HELP		= batdrmgen.html batdrmgen-multi.html

HD_TEST_PERL_SCRIPTS    = ut_batdrmgen ut_batdrmgen-multi

HD_TEST_PFILES          = ut_batdrmgen.par ut_batdrmgen-multi.par

include ${HD_STD_MAKEFILE}
