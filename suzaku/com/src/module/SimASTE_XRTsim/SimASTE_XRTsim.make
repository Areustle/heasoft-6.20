# SimASTE_XRTsim.make

SimASTE_XRTsim_DIR = ${ASTE_MODULE_DIR}/SimASTE_XRTsim/3.0
SimASTE_XRTsim = SimASTE_XRTsim.o xrrtrandom.o c_fcerr.o

SimASTE_XRTsim.o: ${SimASTE_XRTsim_DIR}/SimASTE_XRTsim.c
	$(CC) -I$(SimASTE_Root_DIR) -I$(aste_caldb_DIR) $(CFLAGS) $(ANLCFLAGS) -c ${SimASTE_XRTsim_DIR}/SimASTE_XRTsim.c

xrrtrandom.o: ${SimASTE_XRTsim_DIR}/xrrtrandom.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c ${SimASTE_XRTsim_DIR}/xrrtrandom.c

c_fcerr.o: ${SimASTE_XRTsim_DIR}/c_fcerr.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c ${SimASTE_XRTsim_DIR}/c_fcerr.c
