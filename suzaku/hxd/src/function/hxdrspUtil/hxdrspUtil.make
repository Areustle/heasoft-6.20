hxdrspUtil_DIR = $(HXD_FUNCTION_DIR)/hxdrspUtil/0.2.2

hxdrspUtil = hxdrspUtil_arf.o hxdrspUtil_rmf.o hxdrspUtil_rsp.o 

hxdrspUtil_arf.o: $(hxdrspUtil_DIR)/hxdrspUtil_arf.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdrspUtil_DIR)/hxdrspUtil_arf.c

hxdrspUtil_rmf.o: $(hxdrspUtil_DIR)/hxdrspUtil_rmf.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdrspUtil_DIR)/hxdrspUtil_rmf.c

hxdrspUtil_rsp.o: $(hxdrspUtil_DIR)/hxdrspUtil_rsp.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdrspUtil_DIR)/hxdrspUtil_rsp.c

