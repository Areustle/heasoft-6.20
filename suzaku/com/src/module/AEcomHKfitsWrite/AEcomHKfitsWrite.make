# AEcomHKfitsWrite.make

AEcomHKfitsWrite_DIR = $(ASTE_MODULE_DIR)/AEcomHKfitsWrite/1.7
AEcomHKfitsWrite = AEcomHKfitsWrite.o comFitsHeaderUtil.o

AEcomHKfitsWrite.o: $(AEcomHKfitsWrite_DIR)/AEcomHKfitsWrite.c
	$(CC) -I$(AEcomHKfitsWrite_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(AEcomHKfitsWrite_DIR)/AEcomHKfitsWrite.c

comFitsHeaderUtil.o: $(AEcomHKfitsWrite_DIR)/comFitsHeaderUtil.c
	$(CC) -I$(AEcomHKfitsWrite_DIR) $(CFLAGS) $(ANLCFLAGS) -c $(AEcomHKfitsWrite_DIR)/comFitsHeaderUtil.c
