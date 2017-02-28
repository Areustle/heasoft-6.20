/*
 *	XrtPsfCalc.h: definitons and declarations with global relevance
 *
 *   EXPORTED FUNCTIONS:
 *	 int    ComputeEnDepParams
 *	 double PsfFunction
 *
 *   MACROS:
 *       PIX_DIST_FROM_FOV(a,b)   
 *       PIX_DIST_FROM_OFFSET(a, b, c, d)
 *
 *   AUTHOR:
 *     Italian Swift Archive Center (FRASCATI)
 */

#ifndef XRTPSFCALC_H
#define XRTPSFCALC_H

#include "xrtcaldb.h"
#include "xrtfits.h"
#include "basic.h"


		/********************************/
		/*      defines / typedefs      */
		/********************************/

#define FOVX 300.
#define FOVY 300.


#define PIX_DIST_FROM_FOV(a,b)   \
          ( sqrt( (double)(a-FOVX)*(double)(a-FOVX) +         \
		 (double)(b-FOVY)*(double)(b-FOVY) ) +0.5 )

#define PIX_DIST_FROM_OFFSET(a, b, c, d)            \
          ( sqrt( (double)(a-c)*(double)(a-c) +         \
		 (double)(b-d)*(double)(b-d) ) +0.5 )


extern int ComputeEnDepParams(double Energy,     /*    KeV    */
			      PsfPar_t *PsfParm); /*    xrtcaldb.h    */


extern double PsfFunction(double dist,           /*   arcmin    */
			  double energy,         /*   KeV       */ 
			  double offset,         /*   arcmin    */
			  PsfPar_t *PsfParm );   /* xrtcaldb.h    */

#endif

