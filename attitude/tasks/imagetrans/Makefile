HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	=

HD_CTASK		= imagetrans

HD_CTASK_SRC_c		= imagetrans.c param.c image.c overlap.c methods.c \
			  red_black_tree.c \
			  center_transform.c  \
                          interpolate_transform.c interalpha_transform.c \
                          events_transform.c \
                          area_transform.c flat_transform.c flag_transform.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

LOC_LIBS		= -lcoordfits -lcoord -lparam_wrappers -lephemeris -lrandom

HD_CLIBS		= ${HD_LFLAGS} ${LOC_LIBS} ${HD_STD_LIBS} ${SYSLIBS}

HD_TEST_PERL_SCRIPTS	=

HD_TEST_PFILES		=

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= imagetrans.par

HD_INSTALL_HELP		= imagetrans.html

include ${HD_STD_MAKEFILE}
