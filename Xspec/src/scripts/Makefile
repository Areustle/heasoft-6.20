HD_COMPONENT_NAME	= Xspec

HD_COMPONENT_VERS	= 

HD_INSTALL_ONLY_EXTRA	= install-xspec-scripts

include ${HD_STD_MAKEFILE}

XS_SCRIPTS		= addline.tcl browserLaunch.tcl calc_unc.tcl \
			  fluxerror.tcl ft2pt.tcl ft3pt.tcl simftest.tcl \
			  global_customize.tcl hardcopy.tcl lrt.tcl modid.tcl \
			  multifake.tcl rescalecov.tcl tcloutr.tcl unc_interpolate.tcl \
			  xs_continue.tcl xspec.tcl xs_tclreadline.tcl writefits.tcl\
			  tclIndex

install-xspec-scripts:
	@for script in ${XS_SCRIPTS}; do \
		${HD_INSTALL} $$script ${SPECTRAL_SCRIPTS}/$$script ${HD_CP_P}; \
	done

tcl-index:
	echo "auto_mkindex ." > /tmp/$$.tcl;
	@PATH="${HD_SRC}/BLD/${HD_HOST}/bin:${HD_TOP_EXEC_PFX}/bin:${HD_SRC}/BLD/${HD_HOST}/lib:${HD_TOP_EXEC_PFX}/lib:${PATH}"; \
	LD_LIBRARY_PATH="${HC_BLD_EXEC_PFX}/lib:${HD_BLD_EXEC_PFX}/lib:${LD_LIBRARY_PATH}"; \
	DYLD_LIBRARY_PATH="${HC_BLD_EXEC_PFX}/lib:${HD_BLD_EXEC_PFX}/lib:${DYLD_LIBRARY_PATH}"; \
	export PATH; \
	export LD_LIBRARY_PATH; \
	export DYLD_LIBRARY_PATH; \
	echo "tclsh${BSUF} /tmp/$$.tcl"; \
	tclsh${BSUF} /tmp/$$.tcl;
	rm -f /tmp/$$.tcl
