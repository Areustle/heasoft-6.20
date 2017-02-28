# AEmkEHKtimeGen.make

AEmkEHKtimeGen_DIR = ${ASTE_MODULE_DIR}/AEmkEHKtimeGen/2.4
AEmkEHKtimeGen = AEmkEHKtimeGen.o

AEmkEHKtimeGen.o: ${AEmkEHKtimeGen_DIR}/AEmkEHKtimeGen.c
	$(CC) -I${AEmkEHKtimeGen_DIR} -I${aste_orbit_DIR} -I${aste_caldb_DIR} $(CFLAGS) $(ANLCFLAGS) -c ${AEmkEHKtimeGen_DIR}/AEmkEHKtimeGen.c
