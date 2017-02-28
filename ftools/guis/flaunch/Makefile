HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_INSTALL_EXTRA	= install-flaunch

include ${HD_STD_MAKEFILE}

FLAUNCH_LIB_FILES	= compress_tools.tcl dialog.tcl filestuff.tcl \
			  flaunch.tcl ftools_query.tcl list_it.tcl \
			  term_spawn.tcl tkterm.tcl doc

install-flaunch:
	@for file in ${FLAUNCH_LIB_FILES}; do \
		${HD_INSTALL} $$file ${HD_LIB}/flaunch/$$file ${HD_CP_P}; \
	done
	@${HD_INSTALL} flaunch ${HD_BIN}/flaunch ${HD_CP_P}
