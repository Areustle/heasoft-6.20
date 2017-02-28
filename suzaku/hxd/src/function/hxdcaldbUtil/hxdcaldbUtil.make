hxdcaldbUtil_DIR = $(HXD_FUNCTION_DIR)/hxdcaldbUtil/0.7.7

hxdcaldbUtil = hxdcaldbUtil.o \
		hxdcaldbUtil_gsoghf.o  hxdcaldbUtil_pinlin.o \
		hxdcaldbUtil_teldef.o  hxdcaldbUtil_gsolin.o \
		hxdcaldbUtil_pinthr.o  hxdcaldbUtil_wampht.o \
		hxdcaldbUtil_pinghf.o  hxdcaldbUtil_gsopsd.o \
		hxdcaldbUtil_xxxart.o  hxdcaldbUtil_bstidt.o \
		hxdcaldbUtil_gsoght.o  hxdcaldbUtil_gsogpt.o \
		hxdcaldbUtil_wamghf.o

hxdcaldbUtil.o: $(hxdcaldbUtil_DIR)/hxdcaldbUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdcaldbUtil_DIR)/hxdcaldbUtil.c

hxdcaldbUtil_bstidt.o: $(hxdcaldbUtil_DIR)/hxdcaldbUtil_bstidt.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdcaldbUtil_DIR)/hxdcaldbUtil_bstidt.c

hxdcaldbUtil_gsoghf.o: $(hxdcaldbUtil_DIR)/hxdcaldbUtil_gsoghf.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdcaldbUtil_DIR)/hxdcaldbUtil_gsoghf.c

hxdcaldbUtil_gsoght.o: $(hxdcaldbUtil_DIR)/hxdcaldbUtil_gsoght.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdcaldbUtil_DIR)/hxdcaldbUtil_gsoght.c

hxdcaldbUtil_gsogpt.o: $(hxdcaldbUtil_DIR)/hxdcaldbUtil_gsogpt.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdcaldbUtil_DIR)/hxdcaldbUtil_gsogpt.c

hxdcaldbUtil_gsolin.o: $(hxdcaldbUtil_DIR)/hxdcaldbUtil_gsolin.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdcaldbUtil_DIR)/hxdcaldbUtil_gsolin.c

hxdcaldbUtil_pinghf.o: $(hxdcaldbUtil_DIR)/hxdcaldbUtil_pinghf.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdcaldbUtil_DIR)/hxdcaldbUtil_pinghf.c

hxdcaldbUtil_pinlin.o: $(hxdcaldbUtil_DIR)/hxdcaldbUtil_pinlin.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdcaldbUtil_DIR)/hxdcaldbUtil_pinlin.c

hxdcaldbUtil_pinthr.o: $(hxdcaldbUtil_DIR)/hxdcaldbUtil_pinthr.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdcaldbUtil_DIR)/hxdcaldbUtil_pinthr.c

hxdcaldbUtil_gsopsd.o: $(hxdcaldbUtil_DIR)/hxdcaldbUtil_gsopsd.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdcaldbUtil_DIR)/hxdcaldbUtil_gsopsd.c

hxdcaldbUtil_teldef.o: $(hxdcaldbUtil_DIR)/hxdcaldbUtil_teldef.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdcaldbUtil_DIR)/hxdcaldbUtil_teldef.c

hxdcaldbUtil_wamghf.o: $(hxdcaldbUtil_DIR)/hxdcaldbUtil_wamghf.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdcaldbUtil_DIR)/hxdcaldbUtil_wamghf.c

hxdcaldbUtil_wampht.o: $(hxdcaldbUtil_DIR)/hxdcaldbUtil_wampht.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdcaldbUtil_DIR)/hxdcaldbUtil_wampht.c

hxdcaldbUtil_xxxart.o: $(hxdcaldbUtil_DIR)/hxdcaldbUtil_xxxart.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdcaldbUtil_DIR)/hxdcaldbUtil_xxxart.c

