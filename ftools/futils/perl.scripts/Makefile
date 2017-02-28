HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	=

HD_INSTALL_PERL_SCRIPTS	= fapropos fmodtab fparhelp fparstamp pconfig

HD_INSTALL_HELP		= fapropos.txt fmodtab.txt fparhelp.txt fparstamp.txt pconfig.txt

HD_INSTALL_PFILES	= fmodtab.par

HD_INSTALL_SHELL_SCRIPTS = fscript

HD_INSTALL_ONLY_EXTRA	= install-hpconfig

include ${HD_STD_MAKEFILE}

install-hpconfig:
	@${HD_INSTALL} ${HD_SCRIPTS}/pconfig ${HD_SCRIPTS}/hpconfig ${HD_LN_S}
