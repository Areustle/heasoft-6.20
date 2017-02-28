# aetimecalc.make

aetimecalc_DIR = ${ASTE_MODULE_DIR}/aetimecalc/1.4
aetimecalc = aetimecalc_body.o

aetimecalc_body.o: ${aetimecalc_DIR}/aetimecalc_body.c
	$(CC) -I${aeFitsHeaderUtil_DIR} -I${aste_aspect_DIR} -I${aste_caldb_DIR} $(CFLAGS) $(ANLCFLAGS) -c ${aetimecalc_DIR}/aetimecalc_body.c
