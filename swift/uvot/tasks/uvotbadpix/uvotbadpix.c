/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotbadpix/uvotbadpix.c,v $
 * $Revision: 1.29 $
 * $Date: 2007/11/06 22:02:18 $
 *
 *
 * $Log: uvotbadpix.c,v $
 * Revision 1.29  2007/11/06 22:02:18  rwiegand
 * Write checksums for each output HDU.
 *
 * Revision 1.28  2007/10/11 13:46:37  rwiegand
 * Always write WCS keywords to output.
 *
 * Revision 1.27  2005/09/16 12:16:13  rwiegand
 * Allow user to qualify CALDB query.
 *
 * Revision 1.26  2005/07/18 12:49:31  rwiegand
 * Reorganized code so input image file does not need to be RAW provided
 * it contains the WINDOW* keywords.
 *
 * Revision 1.25  2005/05/24 20:53:21  rwiegand
 * Moved CALDB interface from UVOT specific to attitude library.
 *
 * Revision 1.24  2005/05/20 19:03:01  rwiegand
 * Moved where bad pixel list gets loaded.  Record calibration file
 * retrieved from CALDB in output.
 *
 * Revision 1.23  2005/03/04 20:49:57  rwiegand
 * Relocated loading and applying of bad pixel list to uvotcal library.
 *
 * Revision 1.22  2004/12/02 22:14:57  rwiegand
 * Style changes.
 *
 * Revision 1.21  2004/12/02 21:34:39  rwiegand
 * Special handling when user specifies that only the primary HDU is to be
 * processed.  If the UVOT windowing keywords are not available, derive the
 * raw window from WCS.  Made messages concerning missing keywords more helpful.
 *
 * Revision 1.20  2004/11/03 20:58:36  rwiegand
 * Style changes.
 *
 * Revision 1.19  2004/10/17 11:32:18  rwiegand
 * Updated codename of bad pixel list.
 *
 * Revision 1.18  2004/10/04 18:07:31  rwiegand
 * Implemented CALDB interface.
 *
 * Revision 1.17  2004/07/09 15:57:10  rwiegand
 * Renamed column in bad pixel list table.
 *
 * Revision 1.16  2004/05/14 18:16:35  rwiegand
 * Updated NULL pixel handling to use genimage support.
 *
 * Revision 1.15  2004/05/05 15:07:13  rwiegand
 * Moved writing of history before writing checksum.
 *
 * Revision 1.14  2004/04/19 12:55:18  rwiegand
 * My last change chopped out binning support so had to re-add it.  Made
 * several messages more informative.  Write checksums to each HDU.
 *
 * Revision 1.13  2004/04/12 19:31:12  rwiegand
 * Use TSTOP instead of TIME to determine mission elapsed time.
 *
 * Revision 1.12  2004/04/09 21:55:20  rwiegand
 * Input parameters renamed.  Take TIME column in bad pixel list file
 * into account.
 *
 * Revision 1.11  2003/11/24 21:59:12  rwiegand
 * Write history keywords using new HEAdas function.
 *
 * Revision 1.10  2003/11/03 18:37:26  valett
 * added check for missing row due to packet loss.  All image pixel values
 * which are equal to the BLANK keyword are assumed to be from lost packet
 * and are flagged.
 *
 * Revision 1.9  2003/08/05 16:59:51  rwiegand
 * Updated bad pixel flags to match current UVOT Data Handbook.  Use shorts
 * instead of bytes(chars) for pixel values in quality map.
 *
 * Revision 1.8  2003/07/25 20:07:02  rwiegand
 * Updated use of librew (WCS keywords).
 *
 * Revision 1.7  2003/06/11 21:17:47  miket
 * global name change from ubadpix to uvotbadpix
 *
 * Revision 1.6  2003/06/05 18:00:03  valett
 * reads in input image file into integer array and no longer byte array.
 *
 * Revision 1.5  2003/05/23 18:56:34  rwiegand
 * Support for binned input.
 *
 * Revision 1.4  2003/05/23 15:47:11  rwiegand
 * Correctly functioning version without binning.
 *
 * Revision 1.3  2003/05/23 13:44:25  rwiegand
 * Had underflow and overflow values reversed.  Need to know official values
 * for quality maps.
 *
 * Revision 1.2  2003/05/22 23:20:18  rwiegand
 * Took a stab at combining existing ubadpix with some brew.  Just worked to
 * successful linking.  No binning support.
 *
 */


#include <stdio.h>              /* pil.h uses but does not include */
#include <string.h>

#include "headas.h"
#include "pil.h"
#include "genimage.h"
#include "report.h"
#include "keyutil.h"

#include "uvottool.h"
#include "uvotfile.h"
#include "caldbquery.h"
#include "uvotquality.h"
#include "uvotimage.h"


#define TOOLSUB uvotbadpix
#include "headas_main.c"


#define VERSION 1.0
#define CREATOR _STRINGIFY(TOOLSUB) " " _STRINGIFY(VERSION)
#define ORIGIN "GSFC"

#define ARRAY_SIZE(name) (sizeof(name) / sizeof(name[0]))

#define PAR_INFILE    "infile"
#define PAR_BADPIX    "badpixlist"
#define PAR_OUTFILE   "outfile"
#define PAR_COMPRESS  "compress"


typedef int RawPrimitive;
typedef IImage RawImage;


typedef struct
{
  int chatter;
  int clobber;
  int history;

  char infile[PIL_LINESIZE];
  char badfile[PIL_LINESIZE];
  char outfile[PIL_LINESIZE];

  int compress;

} Parameters;


typedef struct
{
  fitsfile * ifptr;
  fitsfile * ofptr;

  BadPixelList * badpixels;
  RawWindow * window;

} Task;



int
get_parameters (Parameters * p)
{
  int code = 0;
  int pill = 0;

  if (!pill)
    pill = PILGetFname(PAR_INFILE, p->infile);

  if (!pill)
    pill = PILGetFname(PAR_BADPIX, p->badfile);

  if (!pill)
    pill = PILGetFname(PAR_OUTFILE, p->outfile);

  if (!pill)
    pill = PILGetBool(PAR_COMPRESS, &p->compress);

  p->clobber = headas_clobpar;
  p->chatter = headas_chatpar;
  get_history(&p->history);

  if (pill)
    {
      code = TASK_SETUP_ERROR;
      report_error("unable to load parameters\n");
    }
  else
    report_verbose("parameters loaded\n");

  return code;
}


typedef struct
{
  QualityImage *quality;
  QualityImage *badpixels;
  int checkForDamage;
  double underflow;
  double overflow;
  int y;
  int lastPixel;
} UpdateQuality;


int
iterate_update_quality (RawImage * raw, IIState * state)
{
  int code = 0;

  RawPrimitive z;
  QualityPrimitive q = QUALITY_GOOD;

  int x = state->x;
  int y = state->y;
  UpdateQuality *qualifier = (UpdateQuality *) state->user;

#define FIRST_PIXEL_IN_ROW -1
  if (y != qualifier->y)
    qualifier->lastPixel = FIRST_PIXEL_IN_ROW;
  qualifier->y = y;

  code = simage_get(qualifier->badpixels, x, y, &q);
  if (code)
    report_error("unable to get bad pixel map at %d, %d [%d]\n", x, y, code);

  if (!code && qualifier->checkForDamage)
    {
      code = iimage_get(raw, x, y, &z);
      if (code)
        report_error("unable to get raw pixel at %d, %d [%d]\n", x, y, code);

      else if (z == raw->null)
         q |= QUALITY_NULL;

      else if (qualifier->lastPixel == FIRST_PIXEL_IN_ROW)
        ;

      else if (q == QUALITY_GOOD)
        {
          int diff = z - qualifier->lastPixel;
          if (diff <= qualifier->underflow || diff >= qualifier->overflow)
            q |= QUALITY_DAMAGE;
        }

      if (!code)
        qualifier->lastPixel = z;
    }

  if (!code)
    {
      code = simage_set(qualifier->quality, x, y, q);
      if (code)
        report_error("unable to set quality pixel at %d, %d [%d]\n",
        x, y, code);
    }

  return code;
}


int
create_one_quality_map (Task * task, const Parameters * par)
{
  int code = 0;
  int status = 0;

  RawImage raw = { 0 };
  QualityImage quality = { 0 };
  QualityImage badpixels = { 0 };
  ImageIO io = { 0 };
  FITSHeader header = { 0 };
  RawWindow window = { 0 };

  double met = 1e20;
  char extname[FLEN_VALUE];
  int scheme = 0;
  double overflow;
  double underflow;


  if (!code)
    {
       int hdu;
       fits_get_hdu_num(task->ifptr, &hdu);
       if (hdu > 1)
         {
           int tmp = 0;
           fits_write_errmark();
           fits_read_key_str(task->ifptr, "EXTNAME", extname, 0, &tmp);
           if (tmp)
             {
               tmp = 0;
               fits_read_key_str(task->ifptr, "HDUNAME", extname, 0, &tmp);
             }
           if (tmp)
             strcpy(extname, "UNKNOWN");
           fits_clear_errmark();
         }
       else
         strcpy(extname, "PRIMARY");

       report_status("building %s quality map\n", extname);
    }

  if (!code)
    {
      char ctype[FLEN_VALUE];
      fits_read_key_str(task->ifptr, "CTYPE1", ctype, NULL, &status);
      if (status)
        report_warning("unable to read CTYPE1 keyword\n");

      else if (!strcasecmp(ctype, "RA---TAN"))
        {
          window.ignoreWCS = 1;
          window.writeWCS = 1;
          report_verbose("CTYPE indicates SKY image, will write RAW WCS\n");
        }

      status = 0;
    }

  if (!code)
    {
      code = load_raw_window(task->ifptr, &window);
      if (code)
        {
          code = TASK_INPUT_ERROR;
          report_error("unable to load %s raw window\n", extname);
        }
      else
        {
          raw.x0 = window.x0;
          raw.y0 = window.y0;
        }
    }

  if (!code)
    {
      if (window.ignoreWCS)
        {
           /* working from a non-RAW image, just make a flat image */
           int width = window.dx / window.binx;
           int height = window.dy / window.biny;
           code = iimage_allocate(&raw, width, height);
           iimage_set_constant(&raw, 0);
        }
      else
        {
          raw.null = -1;
          code = iimage_read_chdu(&raw, task->ifptr, &io);
        }

      if (code)
        {
          code = TASK_INPUT_ERROR;
          report_error("unable to initialize %s RAW image\n", extname);
        }
    }

  if (!code)
    {
      if (window.ignoreWCS)
        {
          int types[] = { TYP_HDUID_KEY, TYP_COMM_KEY, TYP_CONT_KEY,
                          TYP_USER_KEY, TYP_REFSYS_KEY, 0 };
          header.user = types;
          header.accept = accept_keycard_types;
          fetch_header_records_fits(&header, task->ifptr);
        }
      else
        {
          header.accept = accept_nonstructural_records;
          code = fetch_header_records_fits(&header, task->ifptr);
        }

      if (code)
        {
          code = TASK_INPUT_ERROR;
          report_error("unable to load %s header\n", extname);
        }

      header.accept = 0;
      header.user = 0;
    }

  /*
   * grab the special keywords (these are in io.header)
   */
  if (!code && !window.ignoreWCS)
    {
      fits_read_key_log(task->ifptr, "CMPCNTMN", &scheme, NULL, &status);
      fits_read_key_dbl(task->ifptr, "CMPOTHRS", &overflow, NULL, &status);
      fits_read_key_dbl(task->ifptr, "CMPUTHRS", &underflow, NULL, &status);
      if (status)
        {
          scheme = 0;
          report_warning("unable to read %s compression keywords\n"
                "\t=> assuming no compression damage\n", extname);
        }
      status = 0;
    }

  if (!code)
    {
      met = 1e12;
      fits_read_key_dbl(task->ifptr, "TSTOP", &met, NULL, &status);
      if (status)
          report_warning("unable to read TSTOP keyword\n"
                "\t=> using TSTOP %e\n", met);
      status = 0;
    }

  if (!code)
    {
      quality.null = -1;
      quality.x0 = raw.x0;
      quality.y0 = raw.y0;
      code = simage_allocate(&quality, raw.width, raw.height);
      if (code)
        {
          code = TASK_INPUT_ERROR;
          report_error("unable to allocate quality map\n");
        }
      else
        simage_set_constant(&quality, 0);
    }

  if (!code)
    {
      badpixels.x0 = raw.x0;
      badpixels.y0 = raw.y0;
      badpixels.binx = window.binx;
      badpixels.biny = window.biny;
      code = simage_allocate(&badpixels, raw.width, raw.height);
      if (code)
        {
          code = TASK_INPUT_ERROR;
          report_error("unable to allocate bad pixel map\n");
        }
      else
        simage_set_constant(&badpixels, 0);
    }

  if (!code)
    code = apply_bad_pixel_list(task->badpixels, &badpixels, met);

  if (!code)
    {
      /*
       * update quality map 
       */
      IIState state = { 0 };
      UpdateQuality qualifier = { 0 };

      qualifier.badpixels = &badpixels;
      qualifier.quality = &quality;
      qualifier.underflow = underflow;
      qualifier.overflow = overflow;
      qualifier.y = -1;
      qualifier.checkForDamage = par->compress && scheme;

      state.user = &qualifier;
      raw.offset = 1;
      badpixels.offset = 1;
      quality.offset = 1;

      code = iimage_iterate(&raw, iterate_update_quality, &state);
      if (code)
        report_error("unable to update quality map\n");
    }

  if (!code)
    {
      io.header = &header;
      io.checksum = 1;
      code = simage_append(&quality, task->ofptr, &io);
      if (code)
        report_error("unable to write quality map extension\n");
    }

  if (!code)
    code = save_raw_window(task->ofptr, &window);

  if (!code && (code = fits_write_chksum(task->ofptr, &status) != 0))
    report_warning("unable to write checksum for HDU %s\n", extname);

  iimage_release(&raw);
  simage_release(&quality);
  simage_release(&badpixels);

  return code;
}


int
create_quality_maps (const Parameters * par)
{
  int code = 0;
  int status = 0;
  Task task = { 0 };
  BadPixelList badpixels = { 0 };
  FileSpec spec = { 0 };

  task.badpixels = &badpixels;

  if (!code)
    {
      char * in = par->infile;
      if (fits_open_file(&task.ifptr, in, READONLY, &status))
        {
          code = TASK_INPUT_ERROR;
          report_error("unable to open %s [%d]\n", in, status);
        }
      else
        report_verbose("opened %s\n", in);
    }

  if (!code)
    {
      if (file_parse_path(par->infile, &spec))
        {
          code = TASK_INPUT_ERROR;
          report_error("invalid input '%s'\n", par->infile);
        }
      else if (spec.hdu1)
        {
          if (file_resolve_hdu(&spec, task.ifptr))
            {
              code = TASK_INPUT_ERROR;
              report_error("invalid input HDU '%s'\n", par->infile);
            }
          else
            report_verbose("user specified extension '%s' => %d\n",
                  spec.ext, spec.hdu1);
        }
    }

  if (!code)
    {
      char * out = par->outfile;
      if (fits_create_file(&task.ofptr, out, &status))
        {
          code = TASK_OUTPUT_ERROR;
          report_error("unable to create %s [%d]\n", out, status);
        }
      else
        report_verbose("created %s\n", out);
    }

  /*
   * load the bad pixel list
   */
  if (!code)
    {
      char path[QUERY_MAXPATH];
      if (!strncasecmp(par->badfile, "CALDB", 5))
        {
          CALDBQuery query = { 0 };
          strcpy(query.codename, "BADPIX");
		  set_caldb_query_qualifiers(&query, par->badfile);
          code = simple_caldb_query(&query, task.ifptr, path);
          if (!code)
            HDpar_note(PAR_BADPIX, path);
        }
      else
        strcpy(path, par->badfile);

      if (!code)
        code = load_bad_pixel_list(&badpixels, path);
    }

  /*
   * output the primary header 
   */
  if (!code)
    {
      file_create_primary(task.ifptr, task.ofptr, &spec);

      /*
       * modify DATE and CREATOR keywords in the primary header 
       */
      fits_write_date(task.ofptr, &status);
      fits_update_key(task.ofptr, TSTRING, "CREATOR", CREATOR,
                      "Software that created this file", &status);
      fits_update_key(task.ofptr, TSTRING, "ORIGIN", ORIGIN,
                      "Processing Site", &status);

      if (par->history)
        HDpar_stamp(task.ofptr, 1, &status);

      if (status)
        {
          code = TASK_OUTPUT_ERROR;
          report_error("unable to write history\n");
        }

      if (fits_write_chksum(task.ofptr, &status))
        {
          code = TASK_OUTPUT_ERROR;
          report_error("unable to write checksums\n");
        }
    }

  if (!code)
    {
      if (spec.hdu1)
        {
          /*
           * just process the specified HDU 
           */
          code = create_one_quality_map(&task, par);
        }
      else
        {
          /*
           * process all extensions 
           */
          int i;
          int hdus = 0;

          fits_get_num_hdus(task.ifptr, &hdus, &status);

          if (hdus == 1)
            report_warning("no extensions to process\n");

          for (i = 1; !code && i < hdus; ++i)
            {
              int hdu = i + 1;
              int hduType;
              if (fits_movabs_hdu(task.ifptr, hdu, &hduType, &status))
                {
                  code = TASK_FITS_ERROR;
                  report_error("unable to move to extension %d [%d]\n",
                                hdu, status);
                }
              else if (hduType != IMAGE_HDU)
                {
                  code = TASK_INPUT_ERROR;
                  report_error("input extension %d is not an image\n", hdu);
                }
              else
                code = create_one_quality_map(&task, par);
            }
        }
    }

  if (task.ifptr)
    {
      status = 0;
      if (fits_close_file(task.ifptr, &status))
        {
          code = TASK_INPUT_ERROR;
          report_error("unable to close input [%d]\n", status);
        }
    }

  if (task.ofptr)
    {
      status = 0;
      if (fits_close_file(task.ofptr, &status))
        {
          code = TASK_OUTPUT_ERROR;
          report_error("unable to close output [%d]\n", status);
        }
    }

  return code;
}



int
uvotbadpix ()
{
  int code = 0;
  Parameters p = { 0 };

  set_toolname(_STRINGIFY(TOOLSUB));
  set_toolversion(_STRINGIFY(VERSION));

  add_report_function(&report_headas);

  if (!code)
    code = get_parameters(&p);

  if (!code)
    headas_clobberfile(p.outfile);

  if (!code)
    code = create_quality_maps(&p);

  remove_report_function(&report_headas);

  return code;
}
