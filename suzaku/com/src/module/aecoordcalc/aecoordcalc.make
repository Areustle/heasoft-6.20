# aecoordcalc.make

aecoordcalc_DIR = ${ASTE_MODULE_DIR}/aecoordcalc/1.6
aecoordcalc = aecoordcalc_body.o

aecoordcalc_body.o: ${aecoordcalc_DIR}/aecoordcalc_body.c
	$(CC) -I${aeFitsHeaderUtil_DIR} -I${aste_aspect_DIR} -I${aste_caldb_DIR} $(CFLAGS) $(ANLCFLAGS) -c ${aecoordcalc_DIR}/aecoordcalc_body.c
