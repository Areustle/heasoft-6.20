/*
 * $Source: /headas/headas/attitude/tasks/tristarid/Triplet.h,v $
 * $Revision: 1.4 $
 * $Date: 2005/08/27 12:52:51 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: Triplet.h,v $
 * Revision 1.4  2005/08/27 12:52:51  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.3  2005/08/09 19:51:17  drhunter
 * Considering final.
 *
 * Revision 1.2  2005/08/03 13:07:35  drhunter
 * Tristarid as of 9:00 AM, 8/3/05.
 *
 * Revision 1.1  2005/07/25 18:11:24  drhunter
 * Initial revision
 */

#ifndef TRIPLET_H
#define TRIPLET_H

#include "Source.h"


typedef struct
{
   Source *primary;
   Source *left;
   Source *right;

   double cosAB;
   double cosAC;
   double cosBAC;
   double angleAB;
   double angleAC;
   double angleBC;
   double angleBAC;

   int rightHanded;

   double v1[3];
   double v2[3];
} Triplet;


Triplet * Triplet_create (Source *a, Source *b, Source *c);
Triplet * Triplet_copy (Triplet * t);

void Triplet_construct (Triplet * t, Source *a, Source *b, Source *c);
void Triplet_deconstruct (Triplet * t);
void Triplet_deallocate (Triplet * t);

void Triplet_toString (Triplet * t, char * s);

int Triplet_isRightHanded (double a[3], double b[3], double c[3]);

#endif

