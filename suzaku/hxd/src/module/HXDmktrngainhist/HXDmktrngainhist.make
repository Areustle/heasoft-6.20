HXDmktrngainhist_DIR = ${HXD_MODULE_DIR}/HXDmktrngainhist/0.1.2

HXDmktrngainhist = HXDmktrngainhist.o

HXDmktrngainhist.o: $(HXDmktrngainhist_DIR)/HXDmktrngainhist.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c $(HXDmktrngainhist_DIR)/HXDmktrngainhist.c

HXDmktrngainhist: $(HXDmktrngainhist_DIR)/hxdmktrngainhist.pl \
	hxdmktrngainhist.pl 
	rm -f ${PWD}/hxdmktrngainhist.pl ; \
	cp $(HXDmktrngainhist_DIR)/hxdmktrngainhist.pl ${PWD}
