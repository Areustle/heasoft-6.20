HXDfsclTime_DIR = ${HXD_MODULE_DIR}/HXDfsclTime/0.3.8
HXDfsclTime = HXDfsclTime.o
HXDfsclTimeRPT = HXDfsclTimeRPT.o
HXDfsclTimeFITS = HXDfsclTimeFITS.o

HXDfsclTime.o: $(HXDfsclTime_DIR)/HXDfsclTime.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDfsclTime_DIR)/HXDfsclTime.c
HXDfsclTimeRPT.o: $(HXDfsclTime_DIR)/HXDfsclTimeRPT.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDfsclTime_DIR)/HXDfsclTimeRPT.c
HXDfsclTimeFITS.o: $(HXDfsclTime_DIR)/HXDfsclTimeFITS.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDfsclTime_DIR)/HXDfsclTimeFITS.c
