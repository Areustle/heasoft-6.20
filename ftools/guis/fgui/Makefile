HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_INSTALL_EXTRA	= install-fgui

HD_INSTALL_HELP		= doc/fgui.html doc/fileSelection.html

include ${HD_STD_MAKEFILE}

FGUI_LIBRARY		= class doc fguiInit.tcl

install-fgui:
	@for item in ${FGUI_LIBRARY}; do \
		${HD_INSTALL} $$item ${HD_LIB}/fgui/$$item ${HD_CP_P}; \
	done
	@${HD_INSTALL} fgui ${HD_BIN}/fgui ${HD_CP_P}
	@${HD_INSTALL} spawnToolCmds.csh ${HD_BIN}/spawnToolCmds.csh ${HD_CP_P}
