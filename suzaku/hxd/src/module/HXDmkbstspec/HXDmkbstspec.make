HXDmkbstspec_DIR = /usr/local/astroe/hxd/src/module/HXDmkbstspec/2.0.3
HXDmkbstspec = HXDmkbstspec.o

HXDmkbstspec.o: $(HXDmkbstspec_DIR)/HXDmkbstspec.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDmkbstspec_DIR)/HXDmkbstspec.c
