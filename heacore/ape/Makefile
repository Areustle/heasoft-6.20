HD_COMPONENT_NAME		= heacore

HD_COMPONENT_VERS		= 

HD_SUBDIRS			= include/ape src src/pcheck src/pget src/plist src/pquery src/pquery2 src/pset src/punlearn

HD_INSTALL_HEADERS		= include/pil.h include/pil_error.h

HD_TEST_SUBDIRS			= src/test

include ${HD_STD_MAKEFILE}

run-test:
	${HD_MAKE} subdir HD_SUBDIR=src HD_TARGET=$@
