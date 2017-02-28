# SimASTE_XISRMFsim.make

SimASTE_XISRMFsim_DIR = ${ASTE_MODULE_DIR}/SimASTE_XISRMFsim/2.5
SimASTE_XISRMFsim = SimASTE_XISRMFsim.o

SimASTE_XISRMFsim.o: ${SimASTE_XISRMFsim_DIR}/SimASTE_XISRMFsim.c
	$(CC) -I$(SimASTE_Root_DIR) -I${xis_contami_DIR} -I${xisSciUtil_DIR} -I${xisPixelQuality_DIR} -I$(aste_caldb_DIR) $(CFLAGS) $(ANLCFLAGS) -c ${SimASTE_XISRMFsim_DIR}/SimASTE_XISRMFsim.c
