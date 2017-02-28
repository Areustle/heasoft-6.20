# aeattcor.make

aeattcor_DIR = ${ASTE_MODULE_DIR}/aeattcor/1.2
aeattcor = aeattcor_body.o

aeattcor_body.o: ${aeattcor_DIR}/aeattcor_body.c
	$(CC) -I${aste_orbit_DIR} -I${aste_gethk_DIR} -I${aeFitsHeaderUtil_DIR} $(CFLAGS) $(ANLCFLAGS) -c ${aeattcor_DIR}/aeattcor_body.c
