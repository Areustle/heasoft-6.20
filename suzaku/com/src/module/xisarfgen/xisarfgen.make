# xisarfgen.make

xisarfgen_DIR = ${ASTE_MODULE_DIR}/xisarfgen/1.4
xisarfgen = xisarfgen_body.o

xisarfgen_body.o: ${xisarfgen_DIR}/xisarfgen_body.c
	$(CC) -I${aeFitsHeaderUtil_DIR} -I${aste_caldb_DIR} -I${aste_gti_DIR} -I$(xis_contami_DIR) -I${xisSciUtil_DIR} -I${xisPixelQuality_DIR} -I$(xis_effarea_DIR) -I$(xis_psf_DIR) $(CFLAGS) $(ANLCFLAGS) -c ${xisarfgen_DIR}/xisarfgen_body.c
