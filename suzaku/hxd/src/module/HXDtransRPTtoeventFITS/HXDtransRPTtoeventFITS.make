HXDtransRPTtoeventFITS_DIR = ${HXD_MODULE_DIR}/HXDtransRPTtoeventFITS/0.0.5
HXDtransRPTtoeventFITS = HXDtransRPTtoeventFITS.o

HXDtransRPTtoeventFITS.o: $(HXDtransRPTtoeventFITS_DIR)/HXDtransRPTtoeventFITS.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDtransRPTtoeventFITS_DIR)/HXDtransRPTtoeventFITS.c
