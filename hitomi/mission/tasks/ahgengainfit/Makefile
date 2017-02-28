HD_COMPONENT_NAME       = hitomi

HD_INSTALL_PERL_SCRIPTS = 

HXI_TASK		= hxigainfit

SGD_TASK		= sgdgainfit

SXI_TASK		= sxigainfit

HD_INSTALL_HELP         = ahgengainfit.html hxigainfit.html sgdgainfit.html sxigainfit.html

HD_INSTALL_PFILES       = ${HXI_TASK}.par ${SGD_TASK}.par ${SXI_TASK}.par

HD_INSTALL_ONLY_EXTRA   = install-ahgengainfit

include ${HD_STD_MAKEFILE}

GENGAINFIT_SCRIPT_NAMES = ${HXI_TASK} ${SGD_TASK} ${SXI_TASK}

GENGAINFIT_BASE_SCRIPT = ahgengainfit.pl

#
# target to install ahgengainfit as: hxigainfit, sgdgainfit, sxigainfit
# since they are the same script
#
# This target is essentially identical to the install-perl-scripts target
#
install-ahgengainfit:
	@for file in ${GENGAINFIT_SCRIPT_NAMES}; do \
	  ${HD_INSTALL} ${GENGAINFIT_BASE_SCRIPT} ${HD_SCRIPTS}/$$file ${HD_CP_P}; \
	  status=$$?; if [ $$status -ne 0 ]; then exit $$status; fi; \
	  lhea-fixperl ${HD_SCRIPTS}/$$file; \
	  status=$$?; if [ $$status -ne 0 ]; then exit $$status; fi; \
	  if [ "x${HD_SCRIPTS}" != "x${HD_TOP_SCRIPTS}" ]; then \
	   ${HD_INSTALL} ${HD_SCRIPTS}/$$file ${HD_TOP_SCRIPTS}/$$file ${HD_LN_S}; \
	   status=$$?; if [ $$status -ne 0 ]; then exit $$status; fi; \
	  fi; \
	 done

