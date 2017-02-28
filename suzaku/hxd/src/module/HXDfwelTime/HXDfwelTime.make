HXDfwelTime_DIR = ${HXD_MODULE_DIR}/HXDfwelTime/2.0.0
HXDfwelTime = HXDfwelTime.o
HXDfwelTimeRPT = HXDfwelTimeRPT.o
HXDfwelTimeFITS = HXDfwelTimeFITS.o

HXDfwelTime.o: $(HXDfwelTime_DIR)/HXDfwelTime.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDfwelTime_DIR)/HXDfwelTime.c
HXDfwelTimeRPT.o: $(HXDfwelTime_DIR)/HXDfwelTimeRPT.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDfwelTime_DIR)/HXDfwelTimeRPT.c
HXDfwelTimeFITS.o: $(HXDfwelTime_DIR)/HXDfwelTimeFITS.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDfwelTime_DIR)/HXDfwelTimeFITS.c
