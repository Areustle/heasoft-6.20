HD_COMPONENT_NAME       = hitomi

HD_INSTALL_PERL_SCRIPTS = 

HXI_TASK		= hxinxbgen

SXI_TASK		= sxinxbgen

SXS_TASK		= sxsnxbgen

HD_INSTALL_HELP         = ahnxbgen.html hxinxbgen.html sxinxbgen.html sxsnxbgen.html

HD_INSTALL_PFILES       = ${HXI_TASK}.par ${SXI_TASK}.par ${SXS_TASK}.par

HD_INSTALL_ONLY_EXTRA   = install-ahnxbgen

include ${HD_STD_MAKEFILE}

AHNXBGEN_SCRIPT_NAMES = ${HXI_TASK} ${SXI_TASK} ${SXS_TASK}

AHNXBGEN_BASE_SCRIPT = ahnxbgen.pl

#
# target to install ahnxbgen as: hxinxbgen sxinxbgen sxsnxbgen
# since they are the same script
#
# This target is essentially identical to the install-perl-scripts target
#
install-ahnxbgen:
	@for file in ${AHNXBGEN_SCRIPT_NAMES}; do \
	  ${HD_INSTALL} ${AHNXBGEN_BASE_SCRIPT} ${HD_SCRIPTS}/$$file ${HD_CP_P}; \
	  status=$$?; if [ $$status -ne 0 ]; then exit $$status; fi; \
	  lhea-fixperl ${HD_SCRIPTS}/$$file; \
	  status=$$?; if [ $$status -ne 0 ]; then exit $$status; fi; \
	  if [ "x${HD_SCRIPTS}" != "x${HD_TOP_SCRIPTS}" ]; then \
	   ${HD_INSTALL} ${HD_SCRIPTS}/$$file ${HD_TOP_SCRIPTS}/$$file ${HD_LN_S}; \
	   status=$$?; if [ $$status -ne 0 ]; then exit $$status; fi; \
	  fi; \
	 done

