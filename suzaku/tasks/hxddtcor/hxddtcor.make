hxddtcor_DIR = ${HXD_SCRIPT_DIR}/hxddtcor/1.4

hxddtcor: $(hxddtcor_DIR)/hxddtcor $(hxddtcor_DIR)/hxddtcor.par
	rm -f ${PWD}/hxddtcor ; cp $(hxddtcor_DIR)/hxddtcor ${PWD};\
	rm -f ${PWD}/hxddtcor.par ; cp $(hxddtcor_DIR)/hxddtcor.par ${PWD};

