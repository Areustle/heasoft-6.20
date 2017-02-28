# hxdFitsHeaderUtil.make

hxdFitsHeaderUtil_DIR = ${HXD_FUNCTION_DIR}/hxdFitsHeaderUtil/2.1.5
hxdFitsHeaderUtil = hxdFitsHeaderUtil.o

hxdFitsHeaderUtil.o:	${hxdFitsHeaderUtil_DIR}/hxdFitsHeaderUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c ${hxdFitsHeaderUtil_DIR}/hxdFitsHeaderUtil.c
