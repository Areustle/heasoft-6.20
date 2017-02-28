HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	=

HD_SUBDIRS		= xronlib src

HD_INSTALL_PERL_SCRIPTS	= perl.scripts/ascii2flc perl.scripts/flc2ascii \
			  perl.scripts/xronwin

HD_INSTALL_HELP		= help/ascii2flc.txt help/flc2ascii.txt \
			  help/xronos.hlp help/xronos.txt help/xronwin.txt

HD_INSTALL_EXTRA	= install-xrdefaults

include ${HD_STD_MAKEFILE}

install-xrdefaults:
	@${HD_INSTALL} defaults ${HD_EXEC_PFX}/xrdefaults ${HD_CP_P}
