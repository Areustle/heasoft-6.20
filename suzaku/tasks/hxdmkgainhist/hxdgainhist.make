hxdgainhist_DIR = ${HXD_SCRIPT_DIR}/hxdgainhist/1.3.2

hxdgso_gainhist: $(hxdgainhist_DIR)/hxdgso_gainhist
	rm -f ${PWD}/hxdgso_gainhist ; cp $(hxdgainhist_DIR)/hxdgso_gainhist $(hxdgainhist_DIR)/hxdgso_gainhist.par ${PWD}
	
hxdpin_gainhist: $(hxdgainhist_DIR)/hxdpin_gainhist
	rm -f ${PWD}/hxdpin_gainhist ; cp $(hxdgainhist_DIR)/hxdpin_gainhist $(hxdgainhist_DIR)/hxdpin_gainhist.par ${PWD}
