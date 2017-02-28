# Component (mission) name. Developers need not change/delete this.
HD_COMPONENT_NAME	= suzaku

# Software release version number. Developers need not change/delete this.
HD_COMPONENT_VERS	=

# Perl script(s) to install.
HD_INSTALL_PERL_SCRIPTS	=

# perl libs
HD_INSTALL_PERL_LIBS 	=

# data files
HD_INSTALL_REFDATA	= aehxdgso_group.dat aehxdgso_group64bins.dat

# Help file(s) to install.
HD_INSTALL_HELP		= hxdpinxblc.html hxdpinxbpi.html hxdgsoxblc.html hxdgsoxbpi.html

# Parameter file(s) to install.
HD_INSTALL_PFILES	= hxdpinxblc.par hxdpinxbpi.par hxdgsoxblc.par hxdgsoxbpi.par

HD_TEST_PERL_SCRIPTS	= ut_hxdpinxblc ut_hxdpinxbpi ut_hxdgsoxblc ut_hxdgsoxbpi

HD_INSTALL_ONLY_EXTRA   = install-hxdxb

include ${HD_STD_MAKEFILE}

# names of installed scripts
HXDXB_SCRIPT_NAMES = hxdpinxblc hxdpinxbpi hxdgsoxblc hxdgsoxbpi

# name of script that will become ${HXDXB_SCRIPT_NAMES}
HXDXB_BASE_SCRIPT = hxdxb

#
# target to install hxdxb as:  hxdpinxblc, hxdpinxbpi, hxdgsoxblc and hxdgsoxbpi
# since they are the same script
#
# This target is essentially identical to the install-perl-scripts target
#
install-hxdxb:
	@for file in ${HXDXB_SCRIPT_NAMES}; do \
	  ${HD_INSTALL} ${HXDXB_BASE_SCRIPT} ${HD_SCRIPTS}/$$file ${HD_CP_P}; \
	  status=$$?; if [ $$status -ne 0 ]; then exit $$status; fi; \
	  lhea-fixperl ${HD_SCRIPTS}/$$file; \
	  status=$$?; if [ $$status -ne 0 ]; then exit $$status; fi; \
	  if [ "x${HD_SCRIPTS}" != "x${HD_TOP_SCRIPTS}" ]; then \
	   ${HD_INSTALL} ${HD_SCRIPTS}/$$file ${HD_TOP_SCRIPTS}/$$file ${HD_LN_S}; \
	   status=$$?; if [ $$status -ne 0 ]; then exit $$status; fi; \
	  fi; \
	 done

