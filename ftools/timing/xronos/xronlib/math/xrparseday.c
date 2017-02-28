#include "cfortran.h"

#ifdef vms
#define xrparseday xrparseday_
#endif


#include <stdio.h>
#include <stdlib.h>
#include "cfortran.h"
/* #include "cfitsio.h" */


/* subroutine xrparseday 
   status 1048 means couldn't decode the time
   status 1005 means illegal time entered */


/* note: C name- xrparsedy; FORTRAN name - xrparseday  to combat moronic
non-case sensitive VMS linker */
void xrparsedy(ifo,cinput,tewi,swi,iwi,status)
int *ifo,*status,*iwi;
double *tewi,*swi;
char *cinput;
{

if(*status != 0) {return;}

if(*ifo == 1) {
/* time is in days */
    if( sscanf(cinput,"%lf",tewi) != 1) {*status = 1048;}

} else if (*ifo == 2) {
/* time is in days and sec */
    if( sscanf(cinput,"%i %lf",&iwi[0],swi) != 2)  {*status = 1048;}
} else if (*ifo == 3) {
/* time is in day hour min sec msec */
    if( sscanf(cinput,"%i %i %i %i %i",&iwi[0],&iwi[1],&iwi[2],&iwi[3],&iwi[4])
                 != 5) {*status = 1048;}
}
if(*ifo == 2 || *ifo ==3) {
if (iwi[0]<0) *status = 1005;
}
if(*ifo == 3){
if (iwi[1]<0 || iwi[1]>23) *status = 1005;
if (iwi[2]<0 || iwi[2]>59) *status = 1005;
if (iwi[3]<0 || iwi[3]>59) *status = 1005;
if (iwi[4]<0 || iwi[4]>999) *status = 1005;
}
return;
}
FCALLSCSUB6(xrparsedy,XRPARSEDAY,xrparseday,PINT,PSTRING,PDOUBLE,PDOUBLE,PINT,PINT)

