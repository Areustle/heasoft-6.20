# hxdgtiFitsUtil.make

hxdgtiFitsUtil_DIR = ${HXD_FUNCTION_DIR}/hxdgtiFitsUtil/2.0.0
hxdgtiFitsUtil = hxdgtiFitsUtil.o

hxdgtiFitsUtil.o:	${hxdgtiFitsUtil_DIR}/hxdgtiFitsUtil.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) \
	-c ${hxdgtiFitsUtil_DIR}/hxdgtiFitsUtil.c
