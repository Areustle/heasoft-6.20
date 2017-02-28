# SimASTE_XISevtFitsWrite.make

SimASTE_XISevtFitsWrite_DIR = $(ASTE_MODULE_DIR)/SimASTE_XISevtFitsWrite/2.5
SimASTE_XISevtFitsWrite = SimASTE_XISevtFitsWrite.o

SimASTE_XISevtFitsWrite.o: $(SimASTE_XISevtFitsWrite_DIR)/SimASTE_XISevtFitsWrite.c
	$(CC) -I$(SimASTE_Root_DIR) -I$(aeFitsHeaderUtil_DIR) -I$(aste_gti_DIR) -I${xisSciUtil_DIR} -I${xisPixelQuality_DIR} $(CFLAGS) $(ANLCFLAGS) -c $(SimASTE_XISevtFitsWrite_DIR)/SimASTE_XISevtFitsWrite.c
