#ifndef xpi_test_test_xpi_h
#define xpi_test_test_xpi_h

#include "cfortran.h"

#define UCLGSB(parname,buffer,status) \
  CCALLSFSUB3(UCLGSB,uclgsb,STRING,PINT,PINT,parname,buffer,status)

#define UCLGSD(parname,buffer,status) \
  CCALLSFSUB3(UCLGSD,uclgsd,STRING,PDOUBLE,PINT,parname,buffer,status)

#define UCLGSI(parname,buffer,status) \
  CCALLSFSUB3(UCLGSI,uclgsi,STRING,PINT,PINT,parname,buffer,status)

#define UCLGSL(parname,buffer,status) \
  CCALLSFSUB3(UCLGSL,uclgsl,STRING,PLONG,PINT,parname,buffer,status)

#define UCLGSR(parname,buffer,status) \
  CCALLSFSUB3(UCLGSR,uclgsr,STRING,PFLOAT,PINT,parname,buffer,status)

#define UCLGSS(parname,buffer,status) \
  CCALLSFSUB3(UCLGSS,uclgss,STRING,PINT,PINT,parname,buffer,status)

#define UCLGST(P,B,S) \
  CCALLSFSUB3(UCLGST,uclgst,STRING,PSTRING,PINT,P,B,S)


#define UCLPSB(parname,buffer,status) \
  CCALLSFSUB3(UCLPSB,uclpsb,STRING,INT,PINT,parname,buffer,status)

#define UCLPSD(parname,buffer,status) \
  CCALLSFSUB3(UCLPSD,uclpsd,STRING,DOUBLE,PINT,parname,buffer,status)

#define UCLPSI(parname,buffer,status) \
  CCALLSFSUB3(UCLPSI,uclpsi,STRING,INT,PINT,parname,buffer,status)

#define UCLPSL(parname,buffer,status) \
  CCALLSFSUB3(UCLPSL,uclpsl,STRING,LONG,PINT,parname,buffer,status)

#define UCLPSR(parname,buffer,status) \
  CCALLSFSUB3(UCLPSR,uclpsr,STRING,FLOAT,PINT,parname,buffer,status)

#define UCLPSS(parname,buffer,status) \
  CCALLSFSUB3(UCLPSS,uclpss,STRING,INT,PINT,parname,buffer,status)

#define UCLPST(parname,buffer,status) \
  CCALLSFSUB3(UCLPST,uclpst,STRING,STRING,PINT,parname,buffer,status)

#endif
