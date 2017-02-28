/*
 * $Source: /headas/headas/swift/uvot/lib/uvotcal/uvotquality.c,v $
 * $Revision: 1.3 $
 * $Date: 2008/07/02 15:38:36 $
 *
 * $Log: uvotquality.c,v $
 * Revision 1.3  2008/07/02 15:38:36  rwiegand
 * Check for error condition before storing bad pixels.
 *
 * Revision 1.2  2005/03/04 20:23:06  rwiegand
 * Was bailing out at a bad time.
 *
 * Revision 1.1  2005/03/04 19:38:19  rwiegand
 * Relocated functions for loading bad pixel table and applying it to an
 * image to library.
 *
 */

#include <string.h>

#include "fitsio.h"
#include "report.h"
#include "genimage.h"
#include "uvotquality.h"
#include "uvotimage.h"



typedef struct
{
  char *name;
  int column;
  int *idata;
  double *ddata;
} UbadpixColumn;



void
release_bad_pixel_list (BadPixelList * badpixels)
{
#define FREE(x) { if (x) free(x); x = 0; }
  FREE(badpixels->x);
  FREE(badpixels->yTop);
  FREE(badpixels->yLength);
  FREE(badpixels->reason);
  FREE(badpixels->time);
}



int
load_bad_pixel_list (BadPixelList * badpixels, const char * path)
{
  int code = 0;
  int * pstatus = &code;
  fitsfile *fptr = 0;

  long count;
  int i;

  UbadpixColumn cols[] = {
    {"RAWX", 0, 0},
    {"RAWY", 0, 0},
    {"YLENGTH", 0, 0},
    {"QUALITY", 0, 0},
    {"TIME", 0, 0},
  };


  /*
   * toss the old data
   */
  if (!code)
    release_bad_pixel_list(badpixels);

  /*
   * open the bad pixel list
   */
  if (!code)
    {
      fits_open_file(&fptr, path, READONLY, pstatus);
      if (code)
        report_error("unable to open '%s' [%d]\n", path, code);
      else
        report_verbose("loading the bad pixel list '%s'\n", path);
    }


  /*
   * read caldb bad pixel file and store
   */
  if (!code)
    {
      int hduType = 0;
      fits_movrel_hdu(fptr, 1, &hduType, pstatus);
      if (code)
        report_error("unable to move to second HDU [%d]\n", code);
      else if (hduType != ASCII_TBL && hduType != BINARY_TBL)
        {
          code = 1;
          report_error("bad pixel list HDU is not a table\n");
        }
    }

  if (!code)
    {
      fits_get_num_rows(fptr, &count, pstatus);
      if (code)
        report_error("unable to determine number of rows [%d]\n", code);
    }

  for (i = 0; !code && i < sizeof(cols)/sizeof(cols[0]); ++i)
    {
      UbadpixColumn *p = &cols[i];
      char colnam[FLEN_KEYWORD];
      int anynul = 0;

      fits_get_colname(fptr, CASEINSEN, p->name, colnam, &p->column, pstatus);
      if (code)
        report_error("unable to determine %s column [%d]\n", p->name, code);

      if (!strcmp(p->name, "TIME"))
        {
          p->ddata = (double *) malloc(count * sizeof(double));
          fits_read_col_dbl(fptr, p->column, 1, 1, count, -999,
                        p->ddata, &anynul, pstatus);
        }
      else
        {
          p->idata = (int *) malloc(count * sizeof(int));
          fits_read_col_int(fptr, p->column, 1, 1, count, -999,
                        p->idata, &anynul, pstatus);
        }

      if (code)
        report_error("unable to read %s data [%d]\n", p->name, code);
    }

  if (!code)
    report_status("read bad pixel list\n");

  if (fptr)
    {
      int tmp = 0;
      fits_close_file(fptr, &tmp);
      if (tmp)
        report_error("unable to close bad pixel list [%d]\n", tmp);
    }

  /*
   * store information
   */
  if (!code)
    {
      badpixels->count    = count;

      badpixels->x        = cols[0].idata;
      badpixels->yTop     = cols[1].idata;
      badpixels->yLength  = cols[2].idata;
      badpixels->reason   = cols[3].idata;
      badpixels->time     = cols[4].ddata;

      for (i = 0; i < count; ++i)
        {
          if (badpixels->x[i] < 0 || badpixels->x[i] > UVOT_RAW_MAXX
              || badpixels->yTop[i] < 0 || badpixels->yTop[i] > UVOT_RAW_MAXY)
            report_warning("invalid bad pixel list entry %d x=%d yTop=%d\n",
                  i, badpixels->x[i], badpixels->yTop[i]);
        }
    }

  if (!code)
    strcpy(badpixels->path, path);

  return code;
}


int
apply_bad_pixel_list (const BadPixelList * badpixels,
        QualityImage * quality, double met)
{
  int i;
  int oldoffset = quality->offset;

  quality->offset = 0;

  if (quality->binx == 0)
    {
      quality->binx = 1;
      if (quality->warnings)
        report_warning("updated X binning from 0 to 1\n");
    }

  if (quality->biny == 0)
    {
      quality->biny = 1;
      if (quality->warnings)
        report_warning("updated Y binning from 0 to 1\n");
    }

  for (i = 0; i < badpixels->count; ++i)
    {
      int j;

      if (badpixels->time[i] > met)
        continue;

      for (j = 0; j < badpixels->yLength[i]; ++j)
        {
          int code;
          QualityPrimitive z;
          int xp = (badpixels->x[i] - quality->x0) / quality->binx;
          int yp = (badpixels->yTop[i] - j - quality->y0) / quality->biny;

          /* ignore if outside sub-image */
          code = simage_get(quality, xp, yp, &z);
          if (!code)
            {
              z |= badpixels->reason[i];
              simage_set(quality, xp, yp, z);
            }
        }
    }

  quality->offset = oldoffset;

  report_verbose("done populating bad pixel map\n");
#ifdef DUMP_BAD_PIXEL_MAP
  {
    ImageIO io = { 0 };
    simage_write(quality, "./bpm2d.fits", &io);
  }
#endif

  return 0;
}


