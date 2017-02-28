/*
   Author:  Susan Valett - NASA/GSFC Code 588  April 2002

   Description: simulates a Swift UVOT fits file from a SAS OM fits 
                file.  
*/

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "genimage.h"
#include "report.h"

#define TOOLSUB uvotsim
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

#define ARRAY_LENGTH(name) (sizeof(name) / sizeof(name[0]))
#define TOOLNAME "UVOTSIM"
#define VERSION "v3.1"
#define CREATOR TOOLNAME VERSION /* holds current version of UVOT software */


typedef struct
{
  int dummy;

  char infile[PIL_LINESIZE];    /* Input file name */
  char outfile[PIL_LINESIZE];   /* Output file name */

  int ximgsiz;                  /* image size value */
  int yimgsiz;                  /* image size value */
  char imgunit[PIL_LINESIZE];   /* image size in ARCMIN or PIXELS */
  char forcefilter[PIL_LINESIZE];       /* override FILTER keyword value */

  int xrebin;                   /* image x rebinning factor */
  int yrebin;                   /* image y rebinning factor */
  double platescale;            /* units of arcsec per pixel */

  int clobber;                  /* overwrite existing output file? */
  int history;                  /* write HISTORY keywords? */
  int chatter;                  /* verbosity level */

} Parameters;


typedef struct
{
  const Parameters *par;
  fitsfile *ifptr;
  fitsfile *ofptr;

  DImage *subset;
  DImage *unbinned;
  DImage *binned;
  long inputSize[2];
  long unbinnedSize[2];
} Uvotsim;


/**************************************************************************/
int
get_parameters (Parameters * par)
/*  read input parameters for the uvotsim tasks from the .par file */
{
  int code = 0;

  if (!code)
    code = PILGetFname("infile", par->infile);

  if (!code)
    code = PILGetString("outfile", par->outfile);

  if (!code)
    code = PILGetString("imgunit", par->imgunit);

  if (!code)
    code = PILGetInt("ximgsiz", &par->ximgsiz);

  if (!code)
    code = PILGetInt("yimgsiz", &par->yimgsiz);

  if (!code)
    code = PILGetString("forcefilter", par->forcefilter);

  if (!code)
    code = PILGetReal("platescale", &par->platescale);

  if (!code)
    code = PILGetInt("xrebin", &par->xrebin);

  if (!code)
    code = PILGetInt("yrebin", &par->yrebin);

  if (!code)
    code = PILGetBool("clobber", &par->clobber);

  if (!code)
    code = PILGetBool("history", &par->history);

  return (code);
}


void
clear_status (int *p)
{
  char err_msg[1024];
  while (*p)
    *p = fits_read_errmsg(err_msg);
}



/**************************************************************************/
int
iterate_copy_subset (DImage * out, IIState * state)
{
  int code = 0;
  DImage *in = (DImage *) state->user;
  double z;
  if ((state->x0 < in->width) && (state->y0 < in->height))
    {
      code = dimage_get(in, state->x0, state->y0, &z);
      if (code)
        report_error("unable to copy input subset to output at %d, %d [%d]\n",
                state->x0, state->y0, code);
    }
  else
    z = 0;

  if (!code)
    {
      code = dimage_set(out, state->x0, state->y0, z);
      if (code)
        report_error("unable to set output pixel at %d, %d [%d]\n",
                state->x0, state->y0, code);
    }

  return code;
}


/**************************************************************************/

int
iterate_rebin (DImage * binned, IIState * state)
{
  int code = 0;
  int p, q;
  Uvotsim *task = (Uvotsim *) state->user;
  const Parameters *par = task->par;
  double z = 0;
  for (p = 0; p < par->xrebin; ++p)
    {
      for (q = 0; q < par->yrebin; ++q)
        {
          int check;
          double u;
          int i = state->x0 * par->xrebin + p;
          int j = state->y0 * par->yrebin + q;
          check = dimage_get(task->unbinned, i, j, &u);
          if (!check)
            z += u;
        }
    }

  if (!code)
    {
      code = dimage_set(task->binned, state->x0, state->y0, z);
      if (code)
        report_error("unable to set binned pixel at %d, %d [%d]\n",
                     state->x0, state->y0, code);
    }

  return code;
}



static void
calculate_dim_input_subset (Uvotsim * task, int dim, long beg[], long end[])
{
  long unbinned = task->unbinnedSize[dim];
  long naxis = task->inputSize[dim];

  if (unbinned == naxis)
    {
      beg[dim] = 1;
      end[dim] = unbinned;
    }
  else if (unbinned < naxis)
    {
      beg[dim] = (naxis - unbinned) / 2 + 1;
      end[dim] = beg[dim] + unbinned - 1;
    }
  else
    {
      beg[dim] = 1;
      end[dim] = naxis;
    }
}


int
process_nonnull_image_hdu (Uvotsim * task, int *pstatus)
{
  const Parameters *par = task->par;
  long Binax[2] = { 0, 0 };     /* binning for each axis */
  long Xwindow;                 /* holds product of naxis and binax */
  long Ywindow;                 /* holds product of naxis and binax */
  long forceVal;                /* keyword FILTER value */
  long Window[2];               /* original lower left corner coordinates */
  long inc[2] = { 1, 1 };       /* sampling interval */
  long BegCoord[2];             /* LL of input image subset */
  long EndCoord[2];             /* UR of input image subset */
  long X0window;
  long Y0window;
  double Under = 0.005;         /* Underflow threshold */
  double Over = 0.005;          /* Overflow threshold */
  int Compress = 1;             /* Did compression errors occur */
  int crpix = 1;                /* WCS */
  int cvalx, cvaly;             /* WCS values */
  int width, height;

  fits_read_key_lng(task->ifptr, "BINAX1", &Binax[0], NULL, pstatus);
  fits_read_key_lng(task->ifptr, "BINAX2", &Binax[1], NULL, pstatus);
  clear_status(pstatus);

  if (!Binax[0])
    Binax[0] = 1;
  if (!Binax[1])
    Binax[1] = 1;

  Xwindow = task->unbinnedSize[0] * Binax[0];
  Ywindow = task->unbinnedSize[1] * Binax[1];

  fits_read_key_lng(task->ifptr, "FILTER", &forceVal, NULL, pstatus);
  clear_status(pstatus);

  /*
   * calculate subset of input image 
   */
  calculate_dim_input_subset(task, 0, BegCoord, EndCoord);
  calculate_dim_input_subset(task, 1, BegCoord, EndCoord);

  fits_read_key_lng(task->ifptr, "WINDOWX0", &Window[0], NULL, pstatus);
  fits_read_key_lng(task->ifptr, "WINDOWY0", &Window[1], NULL, pstatus);
  clear_status(pstatus);

  /*
   * allocate memory for subset of input image
   * now that we know how big it is 
   */
  {
    width = EndCoord[0] - BegCoord[0] + 1;
    height = EndCoord[1] - BegCoord[1] + 1;
    *pstatus = dimage_allocate(task->subset, width, height);
    if (*pstatus)
      report_error("unable to allocate subset image %d x %d [%d]\n",
		      width, height, *pstatus);
  }
  {
    int anynul = 0;
    int naxes = 2;
    fits_read_subset_dbl(task->ifptr, 1, naxes, task->inputSize, BegCoord,
                         EndCoord, inc, 0, task->subset->oned, &anynul,
                         pstatus);
  }

  if (!*pstatus)
    {
      width = task->unbinnedSize[0];
      height = task->unbinnedSize[1];
      *pstatus = dimage_allocate(task->unbinned, width, height);
      if (*pstatus)
      	report_error("unable to allocate unbinned image %d x %d [%d]\n",
			width, height, *pstatus);
    }

  if (!*pstatus)
    {
      IIState state = { 0 };
      state.user = task->subset;
      dimage_iterate(task->unbinned, iterate_copy_subset, &state);
    }

  if (!*pstatus)
    {
      width = task->unbinnedSize[0] / par->xrebin;
      height = task->unbinnedSize[1] / par->yrebin;
      *pstatus = dimage_allocate(task->binned, width, height);
      if (*pstatus)
        report_error("unable to allocate binned image %d x %d [%d]\n",
			width, height, *pstatus);
    }

  if (!*pstatus)
    {
      /*
       * perform rebinning or just copy to actual output 
       */
      IIState state = { 0 };
      state.user = task;
      dimage_iterate(task->binned, iterate_rebin, &state);
    }

  if (!*pstatus)
    {
      /*
       * write image to output file 
       */
      DImage *image = task->binned;
      fits_update_key_lng(task->ofptr, "NAXIS1", image->width,
                          "Length of data axis 1", pstatus);
      fits_update_key_lng(task->ofptr, "NAXIS2", image->height,
                          "Length of data axis 2", pstatus);
      fits_write_2d_dbl(task->ofptr, 1, image->width, image->width,
                        image->height, image->oned, pstatus);

      fits_update_key_lng(task->ofptr, "WINDOWDX", Xwindow,
                          "Size along X axis", pstatus);
      fits_update_key_lng(task->ofptr, "WINDOWDY", Ywindow,
                          "Size along Y axis", pstatus);

      if (!strcmp(par->forcefilter, "DEFAULT"))
        {
          char *filter;
          const char *extbase;

          switch (forceVal)
            {
            case 1200:
              filter = "BLOCKED";
              extbase = "bl";
              break;
            case 1400:
              filter = "V";
              extbase = "vv";
              break;
            case 1600:
              filter = "MAGNIFIER";
              extbase = "mg";
              break;
            case 1800:
              filter = "U";
              extbase = "uu";
              break;
            case 2000:
              filter = "B";
              extbase = "bb";
              break;
            case 0:
              filter = "WHITE";
              extbase = "wh";
              break;
            case 200:
              filter = "VGRISM";
              extbase = "gv";
              break;
            case 400:
              filter = "UVW1";
              extbase = "w1";
              break;
            case 600:
              filter = "UVM2";
              extbase = "m2";
              break;
            case 800:
              filter = "UVW2";
              extbase = "w2";
              break;
            case 1000:
              filter = "UGRISM";
              extbase = "gu";
              break;
            case 2100:
              filter = "UNKNOWN";
              extbase = "un";
              break;
            default:
              filter = "ILLEGAL";
              extbase = "il";
            }                   /* switch */

          fits_update_key_str(task->ofptr, "FILTER", filter, "Filter",
                              pstatus);
          {
            char extname[32];
            sprintf(extname, "%s%d", extbase, rand());
            fits_update_key_str(task->ofptr, "EXTNAME", extname,
                                "Extension name", pstatus);
          }
          if (*pstatus)
            report_error("unable to update EXTNAME/FILTER [%d]\n", *pstatus);
        }                       /* if forcefilter then */
      else
        {
	  char filter[32];
	  strcpy(filter, par->forcefilter);
          fits_update_key_str(task->ofptr, "FILTER", filter, "Filter", pstatus);
        }                       /* else */

#define KEY_DIGITS 6
      fits_update_key_dbl(task->ofptr, "CMPUTHRS", Under, KEY_DIGITS,
                          "underflow lossy compression threshold", pstatus);
      fits_update_key_dbl(task->ofptr, "CMPOTHRS", Over, KEY_DIGITS,
                          "overflow lossy compression threshold", pstatus);
      fits_update_key_log(task->ofptr, "CMPCNTMN", Compress,
                          "contaiminated pixel from lossy compression",
                          pstatus);

      /*
       * update WINDOWX0 and WINDOWY0 
       */
      X0window = Binax[0] * BegCoord[0] + Window[0];
      Y0window = Binax[1] * BegCoord[1] + Window[1];
      fits_update_key_lng(task->ofptr, "BINX", par->xrebin,
                          "CCD x-axis binning factor", pstatus);
      fits_update_key_lng(task->ofptr, "BINY", par->yrebin,
                          "CCD y-axis binning factor", pstatus);
      fits_update_key_lng(task->ofptr, "WINDOWX0", X0window,
                          "X coordinate of bottom left corner", pstatus);
      fits_update_key_lng(task->ofptr, "WINDOWY0", Y0window,
                          "Y coordinate of bottom left corner", pstatus);

      cvalx = X0window + (par->xrebin - 1) / 2;
      cvaly = Y0window + (par->yrebin - 1) / 2;
      fits_update_key_lng(task->ofptr, "CDELT1", par->xrebin,
                          "X axis increment", pstatus);
      fits_update_key_lng(task->ofptr, "CDELT2", par->yrebin,
                          "Y axis increment", pstatus);
      fits_update_key_lng(task->ofptr, "CRVAL1", cvalx,
                          "Coord of X ref pixel", pstatus);
      fits_update_key_lng(task->ofptr, "CRVAL2", cvaly,
                          "Coord of Y ref pixel", pstatus);
      fits_update_key_lng(task->ofptr, "CRPIX1", crpix,
                          "X axis reference pixel", pstatus);
      fits_update_key_lng(task->ofptr, "CRPIX2", crpix,
                          "Y axis reference pixel", pstatus);

      if (*pstatus)
        report_error("unable to update window/WCS keywords [%d]\n", *pstatus);

    }                           /* write image */

  dimage_release(task->subset);
  dimage_release(task->unbinned);
  dimage_release(task->binned);

  return *pstatus;
}


int
process_image_hdu (Uvotsim * task, int hdu)
{
  int status = 0;
  int bitpix;
  int naxes;
  /*
   * need to determine number of axes 
   */
  if (!status)
    {
      fits_get_img_param(task->ifptr, 2, &bitpix, &naxes, task->inputSize,
                         &status);
      if (status)
        report_error("unable to get image parameters\n");
    }

  /*
   * update header keyword values 
   */
  if (!status)
    {
      fits_copy_header(task->ifptr, task->ofptr, &status);
      if (status)
        report_error("unable to copy header\n");
    }

  if (!status)
    {
      /*
       * delete keywords in the primary header not needed by UVOT 
       */
      int tmp = 0;
      char *toast[] = {
        "CATEGORY",
        "ODSVER",
        "ODSCHAIN",
        "BINAX1",
        "BINAX2",
        "BINBPE",
        0
      };
      char **p = toast;
      while (*p)
	{
          fits_delete_key(task->ofptr, *p++, &tmp);
          if (tmp)
            clear_status(&tmp);
	}
    }

  if (!status)
    {
      fits_write_date(task->ofptr, &status);
      fits_update_key_str(task->ofptr, "CREATOR", CREATOR,
                          "UVOT Image simulator", &status);
      fits_update_key_str(task->ofptr, "ORIGIN", "Swift Science Center",
                          "Processing Site", &status);
      fits_update_key_str(task->ofptr, "TELESCOP", "SWIFT", "Swift mission",
                          &status);
      fits_update_key_str(task->ofptr, "INSTRUME", "UVOT", "Instrument",
                          &status);
      if (status)
        report_error("unable to write basic keywords\n");
    }

  if (!status)
    {
      if (hdu == 1 && naxes == 0)
        report_status("No image in primary array\n");
      else if (naxes != 2)
        report_warning("image expected to have 2 axes, but has %d\n", naxes);
      else                      /* else 2d image in HDU */
        process_nonnull_image_hdu(task, &status);
    }

  return status;
}



/**************************************************************************/
typedef struct
{
  char * name;
  int colnum;
  long value;

} MetaColumn;

int
process_table_hdu (Uvotsim * task, int hdu)
{
  int status = 0;
  int i;

  MetaColumn columns[] =
    {
      { "WINDOWDX" },
      { "WINDOWDY" },
      { "WINDOWX0" },
      { "WINDOWY0" },
    };

  columns[0].value = task->unbinnedSize[0];  /* WINDOWDX */
  columns[1].value = task->unbinnedSize[1];  /* WINDOWDY */
  columns[2].value = -1;                     /* WINDOWX0 */
  columns[3].value = -1;                     /* WINDOWY0 */

  for (i = 0; i < ARRAY_LENGTH(columns); ++i)
    {
      int anynul = 0;
      MetaColumn * p = &columns[i];
      fits_get_colnum(task->ifptr, CASEINSEN, p->name, &p->colnum, &status);
      if (status)
        report_error("unable to get %s column [%d]\n", p->name, status);
      else if (p->value < 0)
        {
          fits_read_col_lng(task->ifptr, p->colnum, 1, 1, 1, -999,
                    &p->value, &anynul, &status);
	  if (status)
            report_error("unable to read %s column value [%d]\n",
			  p->name, status);
	}
    }

  if (!status)
    {
      for (i = 0; i < ARRAY_LENGTH(columns); ++i)
        {
          MetaColumn * col = &columns[i];
          fits_write_col_lng(task->ofptr, col->colnum, 1, 1, 1,
                    &col->value, &status);
          if (status)
            report_error("unable to write %s column value [%d]\n",
                    col->name, status);
        }
    }

  return status;
}

/**************************************************************************/


int
run_parameters (Parameters * par)
{
  int status = 0;

  Uvotsim task = { 0 };
  fitsfile *infptr = 0;         /* FITS file pointers defined in fitsio.h */
  fitsfile *outfptr = 0;
  DImage subset = { 0 };        /* input image subset */
  DImage unbinned = { 0 };      /* unbinned image */
  DImage binned = { 0 };        /* output image */

  int hdu;
  int numberOfHDU;              /* number of HDUs in file */

  task.par = par;

  task.subset = &subset;
  task.unbinned = &unbinned;
  task.binned = &binned;


  /*
   * Open the input image file 
   */
  if (!status)
    {
      fits_open_file(&infptr, par->infile, READONLY, &status);
      if (status)
        report_error("unable to open %s [%d]\n", par->infile, status);
      else
        report_verbose("Opened the input file:\n %s\n", par->infile);
      task.ifptr = infptr;
    }

  /*
   * clobber existing output file 
   */
  if (par->clobber)
    remove(par->outfile);

  if (!status)
    {
      /*
       * Create the output file 
       */
      fits_create_file(&outfptr, par->outfile, &status);
      if (status)
        report_error("unable to open %s [%d]\n", par->outfile, status);
      else
        report_verbose("Created the output file:\n %s\n", par->outfile);
      task.ofptr = outfptr;
    }

  /*
   * determine how many HDUs in input file 
   */
  if (!status)
    {
      fits_get_num_hdus(infptr, &numberOfHDU, &status);
      if (status)
        report_error("unable to determine number of input HDUs [%d]\n",
                     status);
      else
        report_verbose("Input has %d HDUs\n", numberOfHDU);
    }

  /*
   * determine size of output image in pixels 
   */
  if (par->imgunit[0] == 'A')
    {
      task.unbinnedSize[0] = par->ximgsiz * par->platescale;
      task.unbinnedSize[1] = par->yimgsiz * par->platescale;
    }
  else
    {
      task.unbinnedSize[0] = par->ximgsiz;
      task.unbinnedSize[1] = par->yimgsiz;
    }

  /*
   * While there are still extensions to check 
   */
  for (hdu = 1; !status && hdu <= numberOfHDU; hdu++)
    {
      int hduType;
      report_status("moving to HDU %d [-1]\n", hdu);
      fits_movabs_hdu(infptr, hdu, &hduType, &status);

      if (status)
        report_error("unable to move to HDU %d [%d]\n", status);
      else if (hduType == IMAGE_HDU)
        process_image_hdu(&task, hdu);
      else if (hduType == BINARY_TBL)
        process_table_hdu(&task, hdu);
    }                           /* for each extension */

  if (!status)
    HDpar_stamp(outfptr, 1, &status);
  /*
   * checking of history parameter done automatically 
   */

  /*
   * close files 
   */
  if (outfptr)
    {
      int tmp = 0;
      if (status)
        fits_delete_file(outfptr, &tmp);
      else
        fits_close_file(outfptr, &tmp);
      if (tmp)
        report_error("unable to close %s [%d]\n", par->outfile, tmp);
    }

  if (infptr)
    {
      int tmp = 0;
      fits_close_file(infptr, &tmp);
    }

  return (status);
}

/**************************************************************************/
int
uvotsim (void)
{
  int code = 0;
  Parameters par = { 0 };

  set_toolname(TOOLNAME);
  set_toolversion(VERSION);
  add_report_function(&report_headas);

  if (!code)
    code = get_parameters(&par);

  if (!code)
    code = run_parameters(&par);

  remove_report_function(&report_headas);

  return code;
}


