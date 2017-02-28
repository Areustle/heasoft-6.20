# aeaspect.make

aeaspect_DIR = ${ASTE_MODULE_DIR}/aeaspect/2.3
aeaspect = aeaspect_body.o

aeaspect_body.o: ${aeaspect_DIR}/aeaspect_body.c
	$(CC) -I${aeFitsHeaderUtil_DIR} -I${aste_aspect_DIR} -I${aste_caldb_DIR} $(CFLAGS) $(ANLCFLAGS) -c ${aeaspect_DIR}/aeaspect_body.c
