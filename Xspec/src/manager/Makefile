HD_COMPONENT_NAME	= Xspec

HD_COMPONENT_VERS	= 

HD_INSTALL_ONLY_EXTRA	= install-xspec-manager

include ${HD_STD_MAKEFILE}

XS_MANAGER_FILES	= Xspec.init abundances.dat crosssections.dat \
			  manualList.txt mixmodel.dat model.dat plotLabels.dat \
			  plotUnits.dat CommandSummary.txt

install-xspec-manager:
	@for mgrfile in ${XS_MANAGER_FILES}; do \
		${HD_INSTALL} $$mgrfile ${SPECTRAL_MANAGER}/$$mgrfile ${HD_CP_P}; \
	done

# tkdump.tcl tkedit.tcl XSgetbox.tcl XStest.tcl XStkcon.tcl
# widget
