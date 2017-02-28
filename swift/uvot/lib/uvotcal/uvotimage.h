/*
 * $Source: /headas/headas/swift/uvot/lib/uvotcal/uvotimage.h,v $
 * $Revision: 1.1 $
 * $Date: 2005/03/04 19:38:19 $
 *
 * $Log: uvotimage.h,v $
 * Revision 1.1  2005/03/04 19:38:19  rwiegand
 * Relocated functions for loading bad pixel table and applying it to an
 * image to library.
 *
 */

#ifndef UVOTIMAGE_H
#define UVOTIMAGE_H


#include "uvotquality.h"
#include "genimage.h"


#define UVOT_RAW_DIMX 2048
#define UVOT_RAW_DIMY 2048

#define UVOT_RAW_MINX 0
#define UVOT_RAW_MAXX 2047

#define UVOT_RAW_MINY 0
#define UVOT_RAW_MAXY 2047


typedef SImage QualityImage;


int apply_bad_pixel_list (const BadPixelList * badpixels,
				QualityImage * quality, double met);


#endif
