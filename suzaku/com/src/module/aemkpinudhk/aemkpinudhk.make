# aemkpinudhk.make

aemkpinudhk_DIR = ${ASTE_MODULE_DIR}/aemkpinudhk/1.2
aemkpinudhk = aemkpinudhk_body.o

aemkpinudhk_body.o: ${aemkpinudhk_DIR}/aemkpinudhk_body.c
	$(CC) -o $@ -I${aeFitsHeaderUtil_DIR} -I${aste_orbit_DIR} -I${aste_caldb_DIR} $(CFLAGS) $(ANLCFLAGS) -c ${aemkpinudhk_DIR}/aemkpinudhk_body.c
