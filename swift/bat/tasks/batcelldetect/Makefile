HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_CTASK		= batcelldetect

HD_CTASK_SRC_c		= batcelldetect.c fileio.c pixdetect.c psffit.c \
			  srcdetect.c wcs.c sort.c mcholdc.c misc.c init.c

HD_CFLAGS		= ${HD_STD_CFLAGS} ${TEST_INCLUDE}

HD_CLIBS		= ${HD_LFLAGS} -lbatimageutils -lbatfftsg -lmpfit \
			  -lbatutils \
			  -l${WCSLIB} ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= batcelldetect.par

HD_INSTALL_HELP		= batcelldetect.html batcelldetect-mpfit.html

HD_TEST_PERL_SCRIPTS    = ut_batcelldetect

HD_TEST_PFILES          = ut_batcelldetect.par

include ${HD_STD_MAKEFILE}

${HD_CTASK} ${HD_CTASK_SRC_c:.c=.${OSUF}}: batcelldetect.h
