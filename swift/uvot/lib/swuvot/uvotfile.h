/*
 * $Source: /headas/headas/swift/uvot/lib/swuvot/uvotfile.h,v $
 * $Revision: 1.4 $
 * $Date: 2005/08/29 11:53:26 $
 *
 * $Log: uvotfile.h,v $
 * Revision 1.4  2005/08/29 11:53:26  rwiegand
 * Added function to determine UVOT raw window from FITSHeader object.
 *
 * Revision 1.3  2005/07/18 12:41:58  rwiegand
 * Allow caller to require UVOT raw window to be loaded from WINDOW*
 * keywords.  Allow caller to store UVOT raw window to WCS keywords.
 *
 * Revision 1.2  2004/12/02 22:12:52  rwiegand
 * Pass create_primary_header a FileSpec to take the basic header from.
 *
 * Revision 1.1  2004/12/02 21:29:45  rwiegand
 * A Swift/UVOT tool library.
 *
 */

#ifndef UVOTFILE_H
#define UVOTFILE_H


#include "fitsio.h"
#include "keyutil.h"


typedef struct
{
	int hdu1; /* 1 based extension */

	char type[FLEN_FILENAME];
	char base[FLEN_FILENAME];
	char tmp[FLEN_FILENAME];

	char ext[FLEN_FILENAME];
	char filt[FLEN_FILENAME];
	char bin[FLEN_FILENAME];
	char col[FLEN_FILENAME];

} FileSpec;



typedef struct
{
	int x0;
	int y0;
	int dx;
	int dy;

	int binx;
	int biny;

	int wcs;  /* were WCS keywords used? */
	int ignoreWCS;  /* do not allow WCS keywords for window */
	int writeWCS;  /* write RAW WCS keywords? */

} RawWindow;



int file_parse_path (const char * string, FileSpec * spec);

int file_resolve_hdu (FileSpec * spec, fitsfile * fptr);

int file_create_primary (fitsfile * ifptr, fitsfile * ofptr, FileSpec * spec);


/*
 * reads FITS keywords to determine the UVOT raw window
 * prefers WINDOW[XY]0, WINDOWD[XY], BIN[XY] but falls back on WCS
 */
int load_raw_window (fitsfile * fptr, RawWindow * window);
int parse_raw_window (const FITSHeader * header, RawWindow * window);

/*
 * writes keywords WINDOW[XY]0, WINDOWD[XY], BIN[XY]
 */
int save_raw_window (fitsfile * fptr, const RawWindow * window);


#endif
