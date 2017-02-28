# xrsFitsHeaderUtil.make

xrsFitsHeaderUtil_DIR = ${XRS_FUNCTION_DIR}/xrsFitsHeaderUtil/2.3
xrsFitsHeaderUtil = xrsFitsHeaderUtil.o

xrsFitsHeaderUtil.o: ${xrsFitsHeaderUtil_DIR}/xrsFitsHeaderUtil.c
	$(CC) -I$(aeFitsHeaderUtil_DIR) -I${xrsFitsHeaderUtil_DIR} $(CFLAGS) $(ANLCFLAGS) -c ${xrsFitsHeaderUtil_DIR}/xrsFitsHeaderUtil.c
