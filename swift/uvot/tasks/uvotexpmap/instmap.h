/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotexpmap/instmap.h,v $
 * $Revision: 1.3 $
 * $Date: 2007/06/21 15:44:13 $
 *
 * $Log: instmap.h,v $
 * Revision 1.3  2007/06/21 15:44:13  rwiegand
 * Implemented mask radius parameter since corners of detector do not collect
 * counts.
 *
 * Revision 1.2  2007/05/03 15:50:27  rwiegand
 * Added MASK mode.
 *
 * Revision 1.1  2004/11/01 16:23:14  rwiegand
 * Reimplemented to only transform boundaries instead of every pixel.  Also
 * made output map flat.
 *
 * Revision 1.2  2004/10/29 22:27:15  wiegand
 * Added quality input and general transform.
 *
 * Revision 1.1  2004/10/27 19:10:33  wiegand
 * Initial revision
 *
 */

#ifndef INSTMAP_H
#define INSTMAP_H

#include "overlap.h"
#include "genimage.h"
#include "report.h"
#include "comboxform.h"



enum
{
	CCW,
	CLOCKWISE
};

enum
{
	POS_VOID = 1000000
};


enum
{
	SIDE_VOID,
	BOTTOM,
	RIGHT,
	TOP,
	LEFT,
	NSIDES = 4
};



typedef struct EdgePoint EdgePoint;

struct EdgePoint
{
	double x, y;

	EdgePoint * next;
	EdgePoint * prev;

	int setside;

};


typedef struct Parameters Parameters;


typedef struct
{
	const Parameters * par;

#if 1
	SImage * quality;
	int x0, y0;
	int width, height;
#endif

	int nx;
	int ny;

	int size;
	EdgePoint * boundary;
	EdgePoint * corner[4];

	FImage * map;
	short qualval;
	double exposure;

	int side;
	int sign;
	double area;

	int * xmin;
	int * xmax;
	int * ymin;
	int * ymax;

	COMBOXFORM * xform;

	SImage * mask;
	int dxmin, dxmax;
	int dymin, dymax;

	double maskRadius;
	double crpix1d, crpix2d;

} Task;


int solve (Task * task);


#endif
