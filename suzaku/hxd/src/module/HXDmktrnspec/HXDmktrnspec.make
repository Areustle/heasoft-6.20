HXDmktrnspec_DIR = /usr/local/astroe/hxd/src/module/HXDmktrnspec/0.0.9
HXDmktrnspec = HXDmktrnspec.o

HXDmktrnspec.o: $(HXDmktrnspec_DIR)/HXDmktrnspec.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(HXDmktrnspec_DIR)/HXDmktrnspec.c
