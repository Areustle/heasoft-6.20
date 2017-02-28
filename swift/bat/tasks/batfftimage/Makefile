HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_CTASK		= batfftimage

HD_CTASK_SRC_c		= batfftimage.c wcs.c fileio.c corrections.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lbatimageutils -lbatfftsg \
			  -lbatmaskutils -lcoordfits -lcoord -lbatutils \
			  ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= batfftimage.par

HD_INSTALL_HELP		= batfftimage.html

HD_TEST_PERL_SCRIPTS	= ut_batfftimage ut_batfftimage_pcodemap

HD_TEST_PFILES		= ut_batfftimage.par ut_batfftimage_pcodemap.par

include ${HD_STD_MAKEFILE}

${HD_CTASK} ${HD_CTASK_SRC_c:.c=.${OSUF}}: batfftimage.h
