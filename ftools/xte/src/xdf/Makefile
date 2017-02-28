HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= xdf

HD_LIBRARY_SRC_c	= SelectAppIds.c SelectFiles.c SelectObs.c xdfShared.c
# unused: tclAppInit.c

#HD_CTASK		= xtcl_wish
#HD_CTASK_SRC_c		= tkAppInit.c SelectAppIds.c SelectFiles.c SelectObs.c
#HD_CLIBS		= ${HD_STD_CLIBS} ${XLIBS}

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${CFITSIO} -l${TCL} -l${TK} ${XLIBS}

HD_INSTALL_SHELL_SCRIPTS	= xdf

HD_INSTALL_HELP		= xdf.txt

HD_INSTALL_EXTRA	= install-xdf-extras

default: build-libxdf

all: default publish

include ${HD_STD_MAKEFILE}

# Get cfitsio source files and prepend ${CFITSIO_DIR} prefix to each filename.
CFITSIO_OBJ_TMP		= ${shell if [ -f ${CFITSIO_DIR}/Makefile ]; then ${MAKE} -f ${CFITSIO_DIR}/Makefile cfitsioLibObjs; fi | grep buffers}

CFITSIO_OBJ		= ${shell if [ "x${CFITSIO_OBJ_TMP}" != "x" ]; then echo ${CFITSIO_OBJ_TMP}; fi | sed "s: : ${CFITSIO_DIR}/:g" | sed "s:^:${CFITSIO_DIR}/:"}

${CFITSIO_OBJ}:
	@for file in ${CFITSIO_OBJ}; do \
		if [ ! -f $$file ]; then \
			echo "Cannot find CFITSIO object $$file"; exit 1; \
		fi; \
	done

build-libxdf:
	@if [ "x${CFITSIO_OBJ}" = x ]; then \
		echo "CFITSIO_OBJ macro is empty"; exit 1; \
	fi
	${HD_MAKE} xdf HD_LIBRARY_ROOT=xdf \
		HD_LIBRARY_OBJ="${HD_LIBRARY_SRC_c:.c=.${OSUF}} ${CFITSIO_OBJ}"

XDF_EXTRAS		= .xdfrc widgets

install-xdf-extras:
	@for item in ${XDF_EXTRAS}; do \
		${HD_INSTALL} $$item ${HD_LIB}/xtcl/$$item ${HD_CP_P}; \
	done
