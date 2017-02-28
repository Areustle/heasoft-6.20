HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	=

HD_INSTALL_PERL_SCRIPTS	= add_sis_rmf addascaspec ascascreen \
			  gis3bitfix lexpcor make_ascaray_images offsetcoord \
			  tkinterface.pl Tkascascreen.def

HD_INSTALL_HELP		= add_sis_rmf.txt addascaspec.txt ascascreen.txt \
			  gis3bitfix.txt make_ascaray_images.txt \
			  offsetcoord.txt

HD_INSTALL_ONLY_EXTRA	= install-tkascascreen

include ${HD_STD_MAKEFILE}

install-tkascascreen:
	@${HD_INSTALL} ${HD_SCRIPTS}/ascascreen ${HD_SCRIPTS}/tkascascreen ${HD_LN_S}
