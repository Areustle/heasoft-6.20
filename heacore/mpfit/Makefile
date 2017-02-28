HD_COMPONENT_NAME	= heacore

HD_COMPONENT_VERS	=

HD_LIBRARY_ROOT         = mpfit

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	= mpfit.h

HD_INSTALL_HELP		= HDmpfit.html

HD_LIBRARY_SRC_c	= mpfit.c

HD_DISTCLEAN		= ${HD_INSTALL_HELP}

all: default publish docs

docs: HDmpfit.html

HDmpfit.html: README.html
	sed -e 's/MPFIT: A/HEADAS MPFIT: A/' $< > $@

include ${HD_STD_MAKEFILE}

testmpfit: testmpfit.c libmpfit.so
	$(CC) testmpfit.c -o testmpfit -L. -lmpfit -lm

README: README.html
	sed -e '/^</ d' -e 's|</?[bi]>||g' -e 's/&lt;/</g' -e 's/&gt;/>/g' -e 's/\&amp;/\&/g' $< > $@

${HD_LIBRARY_SRC_c:.c=.${OSUF}}: mpfit.h
