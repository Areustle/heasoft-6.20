HXDftrnTime_DIR = ${HXD_MODULE_DIR}/HXDftrnTime/0.3.3
HXDftrnTime = HXDftrnTime.o
HXDftrnTimeRPT = HXDftrnTimeRPT.o
HXDftrnTimeFITS = HXDftrnTimeFITS.o

HXDftrnTime.o: $(HXDftrnTime_DIR)/HXDftrnTime.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDftrnTime_DIR)/HXDftrnTime.c
HXDftrnTimeRPT.o: $(HXDftrnTime_DIR)/HXDftrnTimeRPT.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDftrnTime_DIR)/HXDftrnTimeRPT.c
HXDftrnTimeFITS.o: $(HXDftrnTime_DIR)/HXDftrnTimeFITS.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDftrnTime_DIR)/HXDftrnTimeFITS.c
