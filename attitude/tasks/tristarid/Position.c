/*
 * $Source: /headas/headas/attitude/tasks/tristarid/Position.c,v $
 * $Revision: 1.4 $
 * $Date: 2005/08/27 12:51:57 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: Position.c,v $
 * Revision 1.4  2005/08/27 12:51:57  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.3  2005/08/09 19:41:46  drhunter
 * Considering final.
 *
 * Revision 1.2  2005/08/03 13:05:57  drhunter
 * Tristarid as of 9:00 AM, 8/3/05
 *
 * Revision 1.1  2005/07/25 18:11:24  drhunter
 * Initial revision
 */

#include "Position.h"
#include "MUtil.h"


void Position_construct (Position *p, double u[3], double r) {
   Position_setUnit(p, u);
   Position_setRadiansRad(p, r);
}

void Position_setUnit (Position *p, double u[3]) {
   p->unit[0] = u[0];
   p->unit[1] = u[1];
   p->unit[2] = u[2];
}

void Position_setRadiansRad (Position *p, double r) {
   p->radius = r;
}

void Position_setDegreesRad (Position *p, double r) {
   p->radius = Math_toRadians(r);
}

void Position_setRadiansRaDec (Position *p, double ra, double dec) {
   Math_rd2unit(ra, dec, p->unit);
}

void Position_setDegreesRaDec (Position *p, double ra, double dec) {
   Math_rd2unit(Math_toRadians(ra), Math_toRadians(dec), p->unit);
}

