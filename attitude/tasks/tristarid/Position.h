/*
 * $Source: /headas/headas/attitude/tasks/tristarid/Position.h,v $
 * $Revision: 1.2 $
 * $Date: 2005/08/27 12:52:00 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: Position.h,v $
 * Revision 1.2  2005/08/27 12:52:00  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.1  2005/07/25 18:11:24  drhunter
 * Initial revision
 */

#ifndef POSITION_H
#define POSITION_H


typedef struct
{
   double unit[3];
   double radius;
} Position;


void Position_construct (Position *p, double u[3], double r);

void Position_setUnit (Position *p, double u[3]);

void Position_setRadiansRad (Position *p, double r);

void Position_setDegreesRad (Position *p, double r);

void Position_setRadiansRaDec (Position *p, double ra, double dec);

void Position_setDegreesRaDec (Position *p, double ra, double dec); 

#endif

