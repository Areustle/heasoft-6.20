HXDfbstTime_DIR = ${HXD_MODULE_DIR}/HXDfbstTime/2.0.3
HXDfbstTime = HXDfbstTime.o
HXDfbstTimeRPT = HXDfbstTimeRPT.o
HXDfbstTimeFITS = HXDfbstTimeFITS.o

HXDfbstTime.o: $(HXDfbstTime_DIR)/HXDfbstTime.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDfbstTime_DIR)/HXDfbstTime.c
HXDfbstTimeRPT.o: $(HXDfbstTime_DIR)/HXDfbstTimeRPT.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDfbstTime_DIR)/HXDfbstTimeRPT.c
HXDfbstTimeFITS.o: $(HXDfbstTime_DIR)/HXDfbstTimeFITS.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDfbstTime_DIR)/HXDfbstTimeFITS.c
