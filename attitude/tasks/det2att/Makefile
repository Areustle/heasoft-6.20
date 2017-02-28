HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	= 

HD_CTASK		= det2att

HD_CTASK_SRC_c		= det2att.c infile.c param.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= -g ${HD_LFLAGS} \
			  -lcoordfits -lcoord -lparam_wrappers \
			  ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= ${HD_CTASK}.par

HD_INSTALL_HEADERS	=

HD_INSTALL_HELP		= ${HD_CTASK}.html

HD_TEST_PERL_SCRIPTS    = ut-det2att

HD_TEST_PFILES      = ut-det2att.par


include ${HD_STD_MAKEFILE}
