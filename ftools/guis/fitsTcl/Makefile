HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= fitstcl

HD_LIBRARY_SRC_c	= fitsCmds.c fitsInit.c fitsIO.c fitsTcl.c \
			  fitsUtils.c fvTcl.c tclShared.c

HD_CFLAGS		= -I${CFITSIO_DIR} ${HD_STD_CFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${TCL}

HD_INSTALL_HEADERS	= fitsTcl.h fitsTclInt.h

HD_INSTALL_HELP		= fitsTcl.html

default: build-libfitstcl

all: default publish

include ${HD_STD_MAKEFILE}

# Get cfitsio source files and prepend ${CFITSIO_DIR} prefix to each filename.
CFITSIO_OBJ_TMP		= ${shell if [ -f ${CFITSIO_DIR}/Makefile ]; then ${MAKE} -f ${CFITSIO_DIR}/Makefile cfitsioLibObjs; fi | grep buffers}

CFITSIO_OBJ		= ${shell if [ "x${CFITSIO_OBJ_TMP}" != "x" ]; then echo ${CFITSIO_OBJ_TMP}; fi | sed "s: : ../../../heacore/cfitsio/:g" | sed "s:^:../../../heacore/cfitsio/:"}

${CFITSIO_OBJ}:
	@for file in ${CFITSIO_OBJ}; do \
		if [ ! -f $$file ]; then \
			echo "Cannot find CFITSIO object $$file"; exit 1; \
		fi; \
	done

build-libfitstcl:
	@if [ "x${CFITSIO_OBJ}" = x ]; then \
		echo "CFITSIO_OBJ macro is empty";  exit 1; \
	fi
	${HD_MAKE} fitstcl HD_LIBRARY_ROOT=fitstcl \
		HD_LIBRARY_OBJ="${HD_LIBRARY_SRC_c:.c=.${OSUF}} ${CFITSIO_OBJ}"
