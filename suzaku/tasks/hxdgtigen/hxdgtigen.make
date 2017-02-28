hxdgtigen_DIR = ${HXD_SCRIPT_DIR}/hxdgtigen/1.5

hxdgtigen: $(hxdgtigen_DIR)/hxdgtigen $(hxdgtigen_DIR)/hxdgtigen.par
	rm -f ${PWD}/hxdgtigen ; cp $(hxdgtigen_DIR)/hxdgtigen ${PWD};\
	rm -f ${PWD}/hxdgtigen.par ; cp $(hxdgtigen_DIR)/hxdgtigen.par ${PWD};
