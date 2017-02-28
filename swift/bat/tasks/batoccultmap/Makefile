HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_CTASK		= batoccultmap

HD_CTASK_SRC_c		= batoccultmap.c contour.c image.c correct.c

HD_CFLAGS		= ${HD_STD_CFLAGS} ${TEST_INCLUDE}

HD_CLIBS		= ${HD_LFLAGS} -lbatimageutils -lbatfftsg \
			  -l${WCSLIB} ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= batoccultmap.par

HD_INSTALL_HELP		= batoccultmap.html

HD_TEST_PERL_SCRIPTS    = ut_batoccultmap ut_batoccultmap_contour

HD_TEST_PFILES          = ut_batoccultmap.par ut_batoccultmap_contour.par

include ${HD_STD_MAKEFILE}

${HD_CTASK} ${HD_CTASK_SRC_c:.c=.${OSUF}}: batoccultmap.h
