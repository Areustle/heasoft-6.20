# AEmkEHKfitsWrite.make

AEmkEHKfitsWrite_DIR = ${ASTE_MODULE_DIR}/AEmkEHKfitsWrite/2.4
AEmkEHKfitsWrite = AEmkEHKfitsWrite.o

AEmkEHKfitsWrite.o: ${AEmkEHKfitsWrite_DIR}/AEmkEHKfitsWrite.c
	$(CC) -I${aeFitsHeaderUtil_DIR} -I${aste_orbit_DIR} -I${aste_caldb_DIR} $(CFLAGS) $(ANLCFLAGS) -c ${AEmkEHKfitsWrite_DIR}/AEmkEHKfitsWrite.c
