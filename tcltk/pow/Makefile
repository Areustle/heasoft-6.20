HD_COMPONENT_NAME	= tcltk

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= pow

HD_LIBRARY_SRC_c	= PowCanvCurve.c PowColormap.c PowCommands.c \
			  PowCreateCurve.c PowCreateData.c PowCreateGraph.c \
			  PowCreateImage.c PowCreateVector.c \
			  PowEventHandlers.c PowGrid.c PowInit.c PowUtils.c \
			  PowWCS.c Visu_colors.c Visu_generic.c Visu_Init.c \
			  Visu_lut.c Visu_shared_colors.c Visu_tkImgPict.c
# unused: orbit.c PowDrvr.c readpha.c tclShared.c tkAppInit.c

HD_CFLAGS		= -I${TCL_DIR}/../generic -I${TK_DIR}/../generic \
			  ${XINCLUDES} ${HD_STD_CFLAGS} \
			  -DVISU_LIBRARY=\"${HD_LIB}/pow\"

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${CFITSIO} -l${TCL} -l${TK} ${XLIBS}

HD_INSTALL_SHELL_SCRIPTS	= POWplot

HD_INSTALL_HEADERS	= pow.h tkpict.h

HD_INSTALL_EXTRA	= install-pow-extras

default: build-libpow

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

# Get wcslib source files and prepend ${WCSLIB_DIR} prefix to each filename.
WCSLIB_OBJ_TMP	= ${shell if [ -f ${WCSLIB_DIR}/makedefs ]; then cd ${WCSLIB_DIR}/C; ${MAKE} echo_modules FLAVOUR=${WCSFLAV} WCSTRIG=WRAPPER; fi | grep fitshdr}

WCSLIB_OBJ	= ${shell if [ "x${WCSLIB_OBJ_TMP}" != "x" ]; then echo ${WCSLIB_OBJ_TMP}; fi | sed "s: : ${WCSLIB_DIR}/C/:g" | sed "s:^:${WCSLIB_DIR}/C/:"}

${WCSLIB_OBJ}:
	@for file in ${WCSLIB_OBJ}; do \
		if [ ! -f $$file ]; then \
			echo "Cannot find WCSLIB object $$file"; exit 1; \
		fi; \
	done

wcslib-obj-remove:
	rm -f ${WCSLIB_OBJ} ${WCSLIB_DIR}/C/libwcs-PIC.a

build-libpow:
	@if [ "x${CFITSIO_OBJ}" = x ]; then \
		echo "CFITSIO_OBJ macro is empty"; exit 1; \
	fi
	@if [ "x${WCSLIB_OBJ}" = x ]; then \
		echo "WCSLIB_OBJ macro is empty"; exit 1; \
	fi
	${HD_MAKE} pow HD_LIBRARY_ROOT=pow \
		HD_LIBRARY_OBJ="${HD_LIBRARY_SRC_c:.c=.${OSUF}} ${CFITSIO_OBJ} ${WCSLIB_OBJ}"
 

POW_TCL			= html_library.tcl notebook.tcl Notifications.tcl \
			  PowCmdsClass.tcl powEdit.tcl powImgProbe.tcl \
			  powMovie.tcl POWplot.tcl powProfile.tcl powRgn.tcl \
			  powRuler.tcl powScript.tcl pow.tcl RegionList.tcl \
			  Region.tcl Shape.tcl visu_widgets.tcl tclIndex \
			  powXRange.tcl

POW_HTML		= About.html Blinking.html Color.html Contours.html \
			  DefaultOptions.html EditGraphs.html Edit.html \
			  EditObjects.html File.html Labels.html \
			  Moving_Graphs.html Options.html Overview.html \
			  POWAPI.html Probe.html Profile.html Regions.html \
			  ROI.html Ruler.html Scripting.html Tools.html \
			  PrintControl.html XRange.html

POW_EXTRAS		= mhh7.gif stretcharrow.xbm
# pow.def pow.dsp pow.dsw powMacResource.r pow.pch pow.sit.hqx

install-pow-extras:
	@for extra in ${POW_TCL} ${POW_HTML} ${POW_EXTRAS}; do \
		${HD_INSTALL} $$extra ${HD_LIB}/pow/$$extra ${HD_CP_P}; \
	done
