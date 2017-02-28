hxdpiUtil_DIR = $(HXD_FUNCTION_DIR)/hxdpiUtil/2.4.3

hxdpiUtil = hxdpiUtil_gso.o hxdpiUtil_pin.o hxdpiUtil.o 


hxdpiUtil.o: $(hxdpiUtil_DIR)/hxdpiUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdpiUtil_DIR)/hxdpiUtil.c

hxdpiUtil_pin.o: $(hxdpiUtil_DIR)/hxdpiUtil_pin.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdpiUtil_DIR)/hxdpiUtil_pin.c

hxdpiUtil_gso.o: $(hxdpiUtil_DIR)/hxdpiUtil_gso.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(hxdpiUtil_DIR)/hxdpiUtil_gso.c





