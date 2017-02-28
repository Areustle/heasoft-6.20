HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_INSTALL_EXTRA	= install-fv

HD_INSTALL_HELP		= fv.txt

include ${HD_STD_MAKEFILE}

FV_LIBRARY		= class doc fvInit.tcl

install-fv:
	@for item in ${FV_LIBRARY}; do \
		${HD_INSTALL} $$item ${HD_LIB}/fv/$$item ${HD_CP_P}; \
	done
	@${HD_INSTALL} fv ${HD_BIN}/fv ${HD_CP_P}

#cygwin:
#	@if [ "x${EXT}" = xwin ]; then \
		lhea-install ${LHEA_SRC}/tcltk2/fv/fv-cygwin ${LHEA_BIN}/fv; \
	else \
		lhea-install ${LHEA_SRC}/tcltk2/fv/fv ${LHEA_BIN}/fv; \
	fi
