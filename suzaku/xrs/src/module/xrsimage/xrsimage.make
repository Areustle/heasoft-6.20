# xrsimage.make

xrsimage_DIR = $(XRS_MODULE_DIR)/xrsimage/0.1
xrsimage = xrsimage.o image.o info.o param.o polygon.o ephemeris.o \
xrs_cornercoord.o xrs_det_image.o xrs_sky_image.o

xrsimage.o: $(xrsimage_DIR)/xrsimage.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(xrsimage_DIR)/xrsimage.c

image.o: $(xrsimage_DIR)/image.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(xrsimage_DIR)/image.c

info.o: $(xrsimage_DIR)/info.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(xrsimage_DIR)/info.c

param.o: $(xrsimage_DIR)/param.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(xrsimage_DIR)/param.c

polygon.o: $(xrsimage_DIR)/polygon.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(xrsimage_DIR)/polygon.c

ephemeris.o: $(xrsimage_DIR)/ephemeris.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(xrsimage_DIR)/ephemeris.c

xrs_cornercoord.o: $(xrsimage_DIR)/xrs_cornercoord.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(xrsimage_DIR)/xrs_cornercoord.c

xrs_det_image.o: $(xrsimage_DIR)/xrs_det_image.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(xrsimage_DIR)/xrs_det_image.c

xrs_sky_image.o: $(xrsimage_DIR)/xrs_sky_image.c
	$(CC) $(CFLAGS) $(ANLCFLAGS) -c $(xrsimage_DIR)/xrs_sky_image.c
