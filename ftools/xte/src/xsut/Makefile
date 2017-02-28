HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_INSTALL_HELP		= xsut.txt

HD_INSTALL_EXTRA	= install-xsut

include ${HD_STD_MAKEFILE}

XSUT_LIB_FILES		= Xsut xsut_help.txt xsut.tcl xte_sm.gif

install-xsut:
	@for file in ${XSUT_LIB_FILES}; do \
		${HD_INSTALL} $$file ${HD_LIB}/xsut/$$file ${HD_CP_P}; \
	done
	@${HD_INSTALL} Xsut ${HD_BIN}/xsut ${HD_CP_P}
