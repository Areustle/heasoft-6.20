#
# Makefile for atFunctions / sample
#	13 Aug 1996	Y.Ishisaki

FITSIO_LNK = /adsoft/cfitsio/2.430/${EXT}/libcfitsio.a

FFLAGS= -O
CFLAGS=	-O -I. -I$(INC)

INC = ../include
LIB = ../src/libatFunctions.a
OBJ = samplef.o sample.o
ALL = samplef

samplef:   $(OBJ) $(LIB)
	$(FC) -o $@ $(OBJ) $(LIB) $(FITSIO_LNK)

clean:
	${RM} $(ALL) $(OBJ)
