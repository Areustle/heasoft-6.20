# SimASTE_XRSevtFitsWrite.make

SimASTE_XRSevtFitsWrite_DIR = $(ASTE_MODULE_DIR)/SimASTE_XRSevtFitsWrite/2.3
SimASTE_XRSevtFitsWrite = SimASTE_XRSevtFitsWrite.o

SimASTE_XRSevtFitsWrite.o: $(SimASTE_XRSevtFitsWrite_DIR)/SimASTE_XRSevtFitsWrite.c
	$(CC) -I$(SimASTE_Root_DIR) -I$(aeFitsHeaderUtil_DIR) -I$(aste_gti_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(SimASTE_XRSevtFitsWrite_DIR)/SimASTE_XRSevtFitsWrite.c
