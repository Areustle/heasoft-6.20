# Makefile

ALL = test-aste_caldb

ADSOFT = /adsoft

#
# ANL install directory
#
ANL_DIR = ${ADSOFT}/aste_anl/1.73
ANL_INC = ${ANL_DIR}/include
ANL_LIB = ${ANL_DIR}/${EXT}
ANL_BIN = ${ANL_DIR}/${EXT}/bin
ANL_LNK = -L${ANL_LIB} -lANL -lBNK -lEVS

#
# atFunctions
#
ATFUNCTIONS_DIR = ${ADSOFT}/atFunctions/2.8
#   where atFunctions.h and atError.h exists
ATFUNCTIONS_INC = ${ATFUNCTIONS_DIR}/include
#   atFunctions link option
ATFUNCTIONS_LNK = -L${ATFUNCTIONS_DIR}/${EXT} -latFunctions

#
# COM + CLI
#
COM_CLI_DIR = ${ADSOFT}/com_cli/2.05
READLINE_DIR = ${ADSOFT}/readline/5.0
#   where cli.h, com.h exists
COM_CLI_INC = ${COM_CLI_DIR}/include
#   COM + CLI [+ readline] link option
COM_CLI_LNK = -L${COM_CLI_DIR}/${EXT} -lCOM -lCLI -L${READLINE_DIR}/${EXT} -lreadline -ltermcap

#
# ASTETOOL
#
ASTETOOL_DIR = ${ADSOFT}/astetool/1.73
#   where aste_*.h exists
ASTETOOL_INC = ${ASTETOOL_DIR}/include
#   ASCATOOL link option
ASTETOOL_LNK = -L${ASTETOOL_DIR}/${EXT} -lastetool

#
# CFITSIO
#
CFITSIO_DIR = ${ADSOFT}/cfitsio/3.006
#   where fitsio.h exists
CFITSIO_INC = ${CFITSIO_DIR}/include
#   CFITSIO link option
CFITSIO_LNK = -L${CFITSIO_DIR}/${EXT} -lcfitsio

#
# HEADAS
#
HEADAS_DIR = ${ADSOFT}/headas/develop
#   where headas.h, etc exists
HEADAS_INCLUDE = -I$(HEADAS_DIR)/heacore/heainit -I$(HEADAS_DIR)/heacore/heautils -I$(HEADAS_DIR)/heacore/heaio -I$(HEADAS_DIR)/heacore/pil
#   HEADAS link option
HEADAS_LNK = $(HEADAS_DIR)/$(EXT)/lib/libhdinit_1.1.a $(HEADAS_DIR)/$(EXT)/lib/libhdutils_1.1.a $(HEADAS_DIR)/$(EXT)/lib/libhdio_1.1.a $(HEADAS_DIR)/$(EXT)/lib/libape_1.0.a

CFLAGS = -g -Wall -I$(ATFUNCTIONS_INC) -I$(ASTETOOL_INC) -I$(CFITSIO_INC) -I$(COM_CLI_INC) -I$(ANL_INC) $(HEADAS_INCLUDE)

DECOPT = FC="f77 -extend_source" LIBSYS="-lUfor -lfor -lutil -li -lots"
OSFOPT = FC="f77 -extend_source" LIBSYS="-lUfor -lfor -lFutil -lots"
SUNOPT = CC=gcc FC="f77 -e" LIBSYS="-L`echo /usr/lang/SC?.0` -lF77"
LNXOPT = CC="gcc -fwritable-strings -Dg77Fortran" FC="g77 -w -fno-second-underscore -fno-f2c" LIBSYS="-lg2c -lnsl"

start:
	$(MAKE) `uname`

all: $(ALL)

ULTRIX:
	$(MAKE) $(DECOPT) all

OSF1:
	$(MAKE) $(OSFOPT) all

SunOS:
	$(MAKE) $(SUNOPT) all

Linux:
	$(MAKE) $(LNXOPT) all

Default:
	$(MAKE) $(LNXOPT) all


test-aste_caldb: test-aste_caldb.o aste_caldb.o
	$(CC) -o $@  test-aste_caldb.o aste_caldb.o $(HEADAS_LNK) $(CFITSIO_LNK) $(ASTETOOL_LNK) $(COM_CLI_LNK) $(ATFUNCTIONS_LNK) $(LIBSYS) -lm

clean:
	-$(RM) $(ALL) *.o
