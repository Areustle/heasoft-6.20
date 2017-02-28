HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	=

HD_INSTALL_PFILES	= xselect.par

XSEL_COMMON_FILES	= txselect.cmd txselect.cmd1 txselect.hlp txselect.key \
			  txselect.kw xselect.cmd xselect.cmd1 xselect.hlp \
			  xselect.key xselect.kw xselect.mdb xselect.udc1

HD_INSTALL_EXTRA	= install-xsel-common

include ${HD_STD_MAKEFILE}

install-xsel-common:
	@for file in ${XSEL_COMMON_FILES}; do \
		${HD_INSTALL} $$file ${HD_BIN}/$$file ${HD_CP_P}; \
	done
