/** \file coordpntlib.c
 *  \brief Data structure and function definitions for coordpnt
 *  \author Robert Hill
 *  \date $Date: 2016/07/25 20:01:31 $
 */

#include "coordpntlib.h"

#include "hdcal.h"

#include "ahlog/cahlog.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include "headas_utils.h"
#include "fitsio.h"

#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

/* ====================================================================== */

Param* createParam () {
  Param* param = (Param*)calloc(1, sizeof(Param));
  param->input = 0;
  param->outfile = 0;
  param->telescop = 0;
  param->instrume = 0;
  param->ra = 0.0;
  param->dec = 0.0;
  param->roll = 0.0;
  param->ranom = 0.0;
  param->decnom = 0.0;
  param->rollnom = 0.0;
  param->teldeffile = 0;
  param->startsysinput = 0;
  param->stopsysinput = 0;
  param->multisegpar = 0;
  param->pixeltest = 0;
  param->startsys_is_radec = 0;
  param->startsys_is_telpol = 0;
  param->startsys_is_telxy = 0;
  param->stopsys_is_radec = 0;
  param->stopsys_is_telpol = 0;
  param->stopsys_is_telxy = 0;
  param->rawtodetseg = 0;
  param->winoffsetx = 0;
  param->winoffsety = 0;
  param->outx = 0.0;
  param->outy = 0.0;
  param->clobber = 0;
  param->ra_undef = 0;
  param->dec_undef = 0;
  param->roll_undef = 0;
  param->ranom_undef = 0;
  param->decnom_undef = 0;
  param->rollnom_undef = 0;
  param->outx_undef = 0;
  param->outy_undef = 0;
  strcpy(param->startsysname, "");
  strcpy(param->stopsysname, "");
  strcpy(param->in_file, "");
  param->startsysnum = 0;
  param->stopsysnum = 0;
  param->lowsysnum = 0;
  param->highsysnum = 0;
  param->in_pixel = 0;
  param->in_x = 0.0;
  param->in_y = 0.0;
  param->input_is_reg_file = 0;
  param->input_is_pixel = 0;
  param->input_is_point = 0;
  return param;
}

/* -------------------------------------------------------------------------- */

void destroyParam (Param* param) {
  if (0 != param->input) free(param->input);
  if (0 != param->outfile) free(param->outfile);
  if (0 != param->telescop) free(param->telescop);
  if (0 != param->instrume) free(param->instrume);
  if (0 != param->teldeffile) free(param->teldeffile);
  if (0 != param->startsysinput) free(param->startsysinput);
  if (0 != param->stopsysinput) free(param->stopsysinput);
  if (0 != param->multisegpar) free(param->multisegpar);
  if (0 != param->pixeltest) free(param->pixeltest);
  free(param);
}

/* -------------------------------------------------------------------------- */

/* Get the actual filename for the TelDef file. */
int resolveTeldefFilename(char* actual_filename, char* telescop, char* instrume) {

  int status = 0;  /* HDgtcalf status */

  /* Query parameters that never change for this query. */
  const char* codenam="TELDEF";
  const char* detnam="-";
  const char* filt="-";
  const char* strtdate="-";
  const char* strttime="-";
  const char* stpdate="-";
  const char* stptime="-";
  const char* expr="-";

  /* Variable query parameters. */
  char tele[FLEN_VALUE];   /* TELESCOP (mission) */
  char instr[FLEN_VALUE];  /* INSTRUME */

  int maxret = 1;  /* Maximum number of returned values */
  char teldef_filename[FLEN_FILENAME];  /* Buffer for found filename */
  int fnamesize = FLEN_FILENAME;  /* Length of filename buffer */
  char* fnptr = teldef_filename;  /* Alias for filename buffer */
  long extno = 0;  /* Extension number */
  char online[10];  /* Online indicator */
  char* onptr = online;  /* Alias for online indicator */
  int nret = 0;   /* Number of entries returned */
  int nfound = 0;   /* Total number of datasets matching selection criteria */

  strcpy(tele, telescop);
  strcpy(instr, instrume);
  strcpy(teldef_filename, "");

  ahlog_info(HIGH, __func__, "Querying CALDB for TelDef file with these parameters:\n");
  ahlog_info(HIGH, __func__, "   TELESCOPE:  %s\n", telescop);
  ahlog_info(HIGH, __func__, "   INSTRUMENT: %s\n", instrume);
  ahlog_info(HIGH, __func__, "   DETNAM:     %s\n", detnam);
  ahlog_info(HIGH, __func__, "   FILTER:     %s\n", filt);
  ahlog_info(HIGH, __func__, "   CODENAME:   %s\n", codenam);
  ahlog_info(HIGH, __func__, "   START DATE: %s\n", strtdate);
  ahlog_info(HIGH, __func__, "   START TIME: %s\n", strttime);
  ahlog_info(HIGH, __func__, "   STOP DATE:  %s\n", stpdate);
  ahlog_info(HIGH, __func__, "   STOP TIME:  %s\n", stptime);
  ahlog_info(HIGH, __func__, "   EXPRESSION: %s\n", expr);

  status = 0;
  HDgtcalf(tele,instr,detnam,filt,codenam,strtdate,strttime,
           stpdate,stptime,expr,maxret,fnamesize,&fnptr,&extno,
           &onptr,&nret,&nfound,&status);

  if (0 != status) {
    ahlog_err(__func__, "Could not get TelDef file from CALDB; HDgtcalf status = %d\n", 
      status);
    return 1;
  }

  if (0 == nfound) {
    ahlog_err(__func__, "No TelDef file found.\n");
    return 1;
  }

  strcpy(actual_filename, "");
  strncat(actual_filename, teldef_filename, FLEN_FILENAME-1);
  ahlog_info(HIGH, __func__, "TelDef file = %s\n", actual_filename);

  return 0;
}

/* -------------------------------------------------------------------------- */

WCSdata* fillWCSstructureFromParams (
  double ra,
  double dec,
  double roll,
  COORDDEF* coorddef,
  TR_SKYATT* skyatt
) {

/* -------------------------------------------------------------------------- */

/* There are two ways to fill the WCS data structure, depending on whether
 * the refpoint parameter is a filename or a comma-separated list of ra, dec, roll.
 * The WCSdata structure is defined in region.h. */

WCSdata* wcs;
wcs = (WCSdata*)calloc(1, sizeof(WCSdata));

wcs->exists = 1;
wcs->xrefval = ra;
wcs->yrefval = dec;
wcs->xrefpix = coorddef->center_x;
wcs->yrefpix = coorddef->center_y;

/* Note minus sign since RA is opposite to X. */
wcs->xinc = -(180.0/M_PI)/skyatt->sky_pix_per_radian;
wcs->yinc =  (180.0/M_PI)/skyatt->sky_pix_per_radian;
wcs->rot = 0.0; /* WCS oriented with  Y north */
strcpy(wcs->type, "-TAN"); 

return wcs;
}

/* -------------------------------------------------------------------------- */

int convertCoordRADEC (
  int conv_to_higher,
  double* lowx,
  double* lowy,
  double* highx,
  double* highy,
  WCSdata* wcs
) {

double xpix = 0.0;
double ypix = 0.0;
double xpos = 0.0;
double ypos = 0.0;
int cf_status = 0;

if (0 != conv_to_higher) {
  xpix = *lowx;  /* SKY X coordinate */
  ypix = *lowy;  /* SKY Y coordinate */

  /* Call a wcsutil function from cfitsio. */
  /* NOTE:  THESE ARE DEPRECATED IN THE CFITSIO DOCUMENTATION. */

  ahlog_info(LOW, __func__, "xpix = %.8g\n", xpix);
  ahlog_info(LOW, __func__, "ypix = %.8g\n", ypix);
  ahlog_info(LOW, __func__, "wcs->xrefval = %.8g\n", wcs->xrefval);
  ahlog_info(LOW, __func__, "wcs->yrefval = %.8g\n", wcs->yrefval);
  ahlog_info(LOW, __func__, "wcs->xrefpix = %.8g\n", wcs->xrefpix);
  ahlog_info(LOW, __func__, "wcs->yrefpix = %.8g\n", wcs->yrefpix);
  ahlog_info(LOW, __func__, "wcs->xinc = %.8g\n", wcs->xinc);
  ahlog_info(LOW, __func__, "wcs->yinc = %.8g\n", wcs->yinc);
  ahlog_info(LOW, __func__, "wcs->rot = %.8g\n", wcs->rot);

  fits_pix_to_world(xpix, ypix, wcs->xrefval, wcs->yrefval, wcs->xrefpix, wcs->yrefpix, 
    wcs->xinc, wcs->yinc, wcs->rot, wcs->type, &xpos, &ypos, &cf_status);

  ahlog_info(LOW, __func__, "xpos = %.8g\n", xpos);
  ahlog_info(LOW, __func__, "ypos = %.8g\n", ypos);
  ahlog_info(LOW, __func__, "status = %d\n", cf_status);

  *highx = xpos; /* RA */
  *highy = ypos; /* Dec */

} else {

  xpos = *highx; /* RA */
  ypos = *highy; /* Dec */

  /* Call a wcsutil function from cfitsio. */
  /* NOTE:  THESE ARE DEPRECATED IN THE CFITSIO DOCUMENTATION. */

  ahlog_info(LOW, __func__, "xpix = %.8g\n", xpix);
  ahlog_info(LOW, __func__, "ypix = %.8g\n", ypix);
  ahlog_info(LOW, __func__, "wcs->xrefval = %.8g\n", wcs->xrefval);
  ahlog_info(LOW, __func__, "wcs->yrefval = %.8g\n", wcs->yrefval);
  ahlog_info(LOW, __func__, "wcs->xrefpix = %.8g\n", wcs->xrefpix);
  ahlog_info(LOW, __func__, "wcs->yrefpix = %.8g\n", wcs->yrefpix);
  ahlog_info(LOW, __func__, "wcs->xinc %.8g\n= ", wcs->xinc);
  ahlog_info(LOW, __func__, "wcs->yinc %.8g\n= ", wcs->yinc);
  ahlog_info(LOW, __func__, "wcs->rot %.8g\n= ", wcs->rot);

  fits_world_to_pix(xpos, ypos, wcs->xrefval, wcs->yrefval, wcs->xrefpix, wcs->yrefpix, 
    wcs->xinc, wcs->yinc, wcs->rot, wcs->type, &xpix, &ypix, &cf_status);

  ahlog_info(LOW, __func__, "xpix = %.8g\n", xpix);
  ahlog_info(LOW, __func__, "ypix = %.8g\n", ypix);
  ahlog_info(LOW, __func__, "status = %d\n", cf_status);

  *lowx = xpix; /* X */
  *lowy = ypix; /* Y */

}
return cf_status;
}

/* -------------------------------------------------------------------------- */

/* Convert from RAW coordinates to PIXEL using the PIXEL_MAP lookup table in
 * reverse.  Not done in coordevt. */

int convertToLowerCoordRawToPixel
(
  TELDEF2* teldef,  /* teldef structure */
  double* lowx, /* X coordinate of lower-level system */
  double* lowy, /* Y coordinate of lower-level system */
  double highx, /* X coordinate of higher-level system */
  double highy, /* Y coordinate of higher-level system */
  int lowsys /* Number of lower-level system */
) {

  int highsys = 0;
  int n_segs = 0;
  double pixelx = 0.0;
  double pixely = 0.0;
  int i = 0;
  int found_pixel = 0;
  double tmp_lowx = 0.0;
  double tmp_lowy = 0.0;
  double tmp_highx = 0.0;
  double tmp_highy = 0.0;
  int* pixel_num = NULL;
  double** pixelx_table = NULL;
  double** pixely_table = NULL;

  double act_scal = 0.0;
  double actxoff = 0.0;
  double actyoff = 0.0;
  double actxflip = 0.0;
  double actyflip = 0.0;
  double actxcen = 0.0;
  double actycen = 0.0;
  double in0_xcen = 0.0;
  double in0_ycen = 0.0;

  actxcen = teldef->coordsys[highsys][0]->center_x;
  actycen = teldef->coordsys[highsys][0]->center_y;

  /* Fetch the needed teldef elements. */

  highsys = teldef->basicparam[lowsys]->high_sys;

  act_scal = teldef->basicparam[lowsys]->scale;
  actxoff = teldef->basicparam[lowsys]->xoffset;
  actyoff = teldef->basicparam[lowsys]->yoffset;
  actxflip = teldef->basicparam[lowsys]->xflip;
  actyflip = teldef->basicparam[lowsys]->yflip;

  in0_xcen = teldef->rawtodetparam[lowsys]->int_cen_x;
  in0_ycen = teldef->rawtodetparam[lowsys]->int_cen_y;

  actxcen = teldef->coordsys[highsys][0]->center_x;
  actycen = teldef->coordsys[highsys][0]->center_y;

  /* Transform the starting highx, highy to physical (mm). */

  pixelx = in0_xcen + actxoff + (highx - actxcen)*(act_scal/actxflip);
  pixely = in0_ycen + actyoff + (highy - actycen)*(act_scal/actyflip);
  ahlog_info(LOW, __func__, "Converting RAW to pixel.\n");
  ahlog_info(LOW, __func__, "pixelx=%.8g, pixely=%.8g\n", pixelx, pixely);

  /* Get the pixel_map from the teldef structure. */

  n_segs = teldef->rawtodetparam[lowsys]->n_segs;
  pixel_num = teldef->rawtodetparam[lowsys]->corner_seg;
  pixelx_table = teldef->rawtodetparam[lowsys]->corner_x;
  pixely_table = teldef->rawtodetparam[lowsys]->corner_y;

  /* Loop through the pixelx_table and compare highx
   * to the values. */

  found_pixel = 0;
  for (i=0; i<n_segs; i++) {
    if (0 == found_pixel) {
      if (pixelx_table[i][0] < pixelx_table[i][3]) {
        tmp_lowx = pixelx_table[i][0];
      } else {
        tmp_lowx = pixelx_table[i][3];
      }
      if (pixelx_table[i][1] > pixelx_table[i][2]) {
        tmp_highx = pixelx_table[i][1];
      } else {
        tmp_highx = pixelx_table[i][2];
      }
      if (pixely_table[i][0] < pixely_table[i][1]) {
        tmp_lowy = pixely_table[i][0];
      } else {
        tmp_lowy = pixely_table[i][1];
      }
      if (pixely_table[i][2] > pixely_table[i][3]) {
        tmp_highy = pixely_table[i][2];
      } else {
        tmp_highy = pixely_table[i][3];
      }
      ahlog_info(LOW, __func__, "lowx=%.8g, highx=%.8g, lowy=%.8g, highy=%.8g\n", 
        tmp_lowx, tmp_highx, tmp_lowy, tmp_highy);
      if (
        pixelx >= tmp_lowx && pixelx <= tmp_highx &&
        pixely >= tmp_lowy && pixely <= tmp_highy) {
        *lowx = pixel_num[i];
        found_pixel = 1;
      }
    }
  }

  *lowy = 0.0;
  if (0 == found_pixel) {
    *lowx = DOUBLENULLVALUE;
    return 1;
  } else {
    return 0;
  }

}

/* -------------------------------------------------------------------------- */

int convertRegionLength (
  int conv_to_higher,
  TELDEF2* teldef,
  double length_in,
  double* length_out,
  int sys
) {
  
  double scaling = 0.0;

  /* Length parameters are transformed by the ratio of the scales of
   * the lower and upper coordinate systems.  This is found in
   * the TELDEF2 structure as px_scale_factor, which is defined as
   * the ratio between the scale of the given system and that of the 
   * bottom-level system. */

  if (0 != conv_to_higher) {
    scaling = teldef->px_scale_factor[sys]/teldef->px_scale_factor[sys+1];
  } else {
    scaling = teldef->px_scale_factor[sys+1]/teldef->px_scale_factor[sys];
  }

  if (0.0 == scaling) return 1;

  *length_out = length_in*scaling;
  return 0;
}

/* -------------------------------------------------------------------------- */

int convertRegionLengthRADec (
  int conv_to_higher,
  double length_in,
  double* length_out,
  WCSdata* wcs
) {
  
  double scaling = 0.0;

  if (fabs(wcs->xinc) != fabs(wcs->yinc)) {
    ahlog_err(__func__, "Scaling must be the same in both dimensions\n");
    return 1;
  }

  if (0 != conv_to_higher) {
    scaling = fabs(wcs->xinc);
  } else {
    scaling = 1.0/fabs(wcs->xinc);
  }

  ahlog_info(LOW, __func__, "scaling = %.8g\n", scaling);

  if (0.0 == scaling) {
    return 1;
  }

  *length_out = length_in*scaling;

  return 0;
}

/* -------------------------------------------------------------------------- */

/* Convert an angle parameter for a region based on the relative
 * rotation between the two coordinate systems.  Limitation: this routine
 * cannot be used for the MULTISEG conversion, since it assumes the
 * lowest segment number. */

int convertRegionAngle (
  int conv_to_higher,
  TELDEF2* teldef,
  double in_angle,
  double* out_angle,
  int sys
) {

  double rotangle = 0.0;
  double tmp_angle = 0.0;
  double sinang = 0.0;
  double cosang = 0.0;
  int xflip = 0;
  int yflip = 0;
  XFORM2D* t_trans;

  if (0 != conv_to_higher) {
    t_trans = teldef->coordsys[sys][teldef->min_segment[sys]]->trans;
  } else {
    t_trans = teldef->coordsys[sys][teldef->min_segment[sys]]->inverse_trans;
  }

  errno = 0;

  sinang = t_trans->rot[1][0];
  cosang = t_trans->rot[0][0];
  rotangle = atan2(sinang, cosang);

  ahlog_info(LOW, __func__, "rotangle = %.8g\n", rotangle);
  if (EDOM == errno) {
    errno = 0;
    return 1;
  }
  rotangle *= 180.0/M_PI;
  tmp_angle = in_angle + rotangle;
  xflip = teldef->basicparam[sys]->xflip;
  yflip = teldef->basicparam[sys]->yflip;
  tmp_angle *= xflip;
  if (0 > xflip) {
    tmp_angle -= 180.0;
  }
  tmp_angle *= yflip;

  while (tmp_angle < 0.0) {
    tmp_angle += 360.0;
  }
  while (tmp_angle >= 360.0) {
    tmp_angle -= 360.0;
  }

  *out_angle = tmp_angle;
  return 0;
}

/* -------------------------------------------------------------------------- */

int convertRegionAngleMultiseg (
  int conv_to_higher,
  TELDEF2* teldef,
  double in_angle,
  double* out_angle,
  int sys,
  long seg
) {
  int i=0, j=0;  /* Loop indexes */
  double rot[2][2];
  double tmp_angle = 0.0;
  double sinang = 0.0;
  double cosang = 0.0;
  double rotangle = 0.0;
  int xflip = 0;
  int yflip = 0;

  /* Angle parameters are transformed by the rotation angle between the
   * lower and upper systems.  This is encoded in the XFORM2D.rot structure
   * member. */
  for (i=0; i<2; i++) {
    for (j=0; j<2; j++) {
      rot[i][j] = 0.0;
    }
  }
  if (0 != conv_to_higher) {
    for (i=0; i<2; i++) {
      for (j=0; j<2; j++) {
        rot[i][j] = teldef->coordsys[sys][seg]->trans->rot[i][j];
      }
    }
  } else {
    for (i=0; i<2; i++) {
      for (j=0; j<2; j++) {
        rot[i][j] = teldef->coordsys[sys][seg]->inverse_trans->rot[i][j];
      }
    }
  }

  /* Compute angle from sine and cosine elements of rot matrix. */
  errno = 0;
  sinang = rot[1][0];
  cosang = rot[0][0];
  rotangle = atan2(sinang, cosang);
  if (EDOM == errno) {
    errno = 0;
    return 1;
  }
  rotangle *= 180.0/M_PI;

  tmp_angle = in_angle +  rotangle;

  /* Account for changed sense between coordinate systems. */

  xflip = teldef->basicparam[sys]->xflip;
  yflip = teldef->basicparam[sys]->yflip;
  tmp_angle *= xflip;
  if (0 > xflip) {
    tmp_angle -= 180.0;
  }
  tmp_angle *= yflip;

  while (tmp_angle < 0.0) {
    tmp_angle += 360.0;
  }
  while (tmp_angle >= 360.0) {
    tmp_angle -= 360.0;
  }

  *out_angle = tmp_angle;
  return 0;
}

/* -------------------------------------------------------------------------- */

int convertRegionAngleSkyAtt (
  int conv_to_higher,
  TELDEF2* teldef,
  double in_angle,
  double* out_angle,
  int sys,
  QUAT* q
) {
  
  double conv_factor = 1.0;
  double rotangle = 0.0;
  double tmp_angle = 0.0;
  double sinang = 0.0;
  double cosang = 0.0;

  TR_SKYATT* skyattparam = teldef->skyattparam[sys];
  COORDDEF* incoordsys = 0;
  COORDDEF* outcoordsys = 0;

  if (0 != conv_to_higher) {
    incoordsys = teldef->coordsys[sys][teldef->min_segment[sys]];
    outcoordsys = teldef->coordsys[sys+1][teldef->min_segment[sys+1]];
  } else {
    incoordsys = teldef->coordsys[sys+1][teldef->min_segment[sys+1]];
    outcoordsys = teldef->coordsys[sys][teldef->min_segment[sys]];
  }

  /* Adjust for any misalignment between the telescope and spacecraft axes. */

  productOfQuats(skyattparam->xrt, q, skyattparam->alignment->q_inverse);

  /* Calculate the rotation quat from nominal pointing to XRT pointing. */

  getQuatOfChange(skyattparam->delta_q, skyattparam->q0, skyattparam->xrt);

  /* Calculate the conversion factor. */

  if (sys == teldef->n_coordsys-2) {
    /* Sky coordinates */
    conv_factor = skyattparam->sky_pix_per_radian;
  } else {
    /* Other SKYATT transformations */
    conv_factor = teldef->skyattparam[sys]->focal_length /
      teldef->coordsys[sys][teldef->min_segment[sys]]->scale_x;
  }

  /* Calculate the transformation and fill elements of transformation struct. */

  convertQuatToXform2d(
    skyattparam->det2sky, skyattparam->delta_q,
    incoordsys->center_x, incoordsys->center_y,
    outcoordsys->center_x, outcoordsys->center_y,
    conv_factor);

  /* Angle parameters are transformed by the rotation angle between the lower and
   * upper systems.  This is encoded in the XFORM2D.rot structure member. */

  errno = 0;
  if (0 != conv_to_higher) {
    sinang = skyattparam->det2sky->rot[1][0];
  } else {
    sinang = skyattparam->det2sky->rot[0][1];
  }
  cosang = skyattparam->det2sky->rot[0][0];
  rotangle = atan2(sinang, cosang);
  ahlog_info(LOW, __func__, "rotangle = %.8g\n", rotangle);
  if (EDOM == errno) {
    errno = 0;
    return 1;
  }
  rotangle *= 180.0/M_PI;
  tmp_angle = in_angle + rotangle;

  while (tmp_angle < 0.0) {
    tmp_angle += 360.0;
  }
  while (tmp_angle >= 360.0) {
    tmp_angle -= 360.0;
  }

  *out_angle = tmp_angle;
  return 0;
}

/* -------------------------------------------------------------------------- */

/* Convert an angle parameter for a region based on the relative rotation between
 * the two coordinate systems.  This is the routine for converting from SKy to 
 * RA and Dec. */

int convertRegionAngleRADec (
  int conv_to_higher,
  double in_angle,
  double* out_angle,
  int sys,
  WCSdata* wcs
) {
  
  double rotangle = 0.0;
  double tmp_angle = 0.0;

  /* Angle parameters are transformed by the rotation angle between the lower and
   * upper systems. This is encoded in the WCSdata.rot structure member, which
   * must be converted to degrees. */

  rotangle = wcs->rot*180.0/M_PI;

  ahlog_info(LOW, __func__, "rotangle = %.8g\n", rotangle);
  if (0 == conv_to_higher) {
    rotangle *= -1.0;
  }

  tmp_angle = in_angle + rotangle;

  while (tmp_angle < 0.0) {
    tmp_angle += 360.0;
  }
  while (tmp_angle >= 360.0) {
    tmp_angle -= 360.0;
  }

  *out_angle = tmp_angle;
  return 0;
}

/* -------------------------------------------------------------------------- */

int convertCoordPolar(
  int conv_to_higher_tel,
  double* lowx,  /* Lower coordinates */
  double* lowy,
  double* highx, /* Higher coordinates */
  double* highy,
  TELDEF2* teldef /* Information from TelDef file */
) {
  double RAD2DEG = 180.0/M_PI;
  double DEG2ARCMIN = 60.0;

  double optx = 0.0;
  double opty = 0.0;
  double inputx = 0.0;
  double inputy = 0.0;
  double outx = 0.0;
  double outy = 0.0;
  double optaxisx = 0.0; /* Optical axis X location in SYS system. */
  double optaxisy = 0.0; /* Optical axis Y location in SYS system. */
  double phizero = 0.0;  /* Clockwise angle in degrees between  phi=0 and Y axis */
  double pixels_per_radian = 0.0;  /* Plate scale */
  double theta = 0.0;  /* Colatitude */
  double phi = 0.0;  /* Longitude */
  long xflip = 0;
  long yflip = 0;
  int status = 0;
  int fsys = 0;  /* Coordinate system where focal length is found in teldef */

  optaxisx = teldef->telescopeparam->xoffset;
  optaxisy = teldef->telescopeparam->yoffset;
  phizero = teldef->telescopeparam->rotangle;

  if (NO_SKY_SYS == teldef->sky_sys) {  /* Const defined in teldef2.h */
    ahlog_err(__func__, "No SKY coordinate system defined, so focal length unavailable\n");
    return 1;
  }

  fsys = teldef->sky_sys - 1;
  pixels_per_radian = (teldef->skyattparam[fsys]->focal_length) / (teldef->telescopeparam->scale);

  xflip = teldef->telescopeparam->xflip;
  yflip = teldef->telescopeparam->yflip;

/* For this routine, the convention is as follows:
 * 
 * conv_to_higher_tel          from          to
 *        1                   TELPOL       OPTCOORD        
 *        0                  OPTCOORD       TELPOL         
 *
 * The parameter conv_to_higher_tel is maintained separately from
 * the general parameter conv_to_higher because they need not be same.
 * (See logical flow in Appendix A of the TRF.) */

  if (conv_to_higher_tel) {
    /*  Required are
    *   1) Scale factor (plate scale is mm->arcmin another piece arcmin->pixel)
    *   2) optx, opty in FOC system (output x,y = 0,0 at optical axis).
    *   3) rotation phizero between FOC and raytracing output x,y.
    *      phi=0 is the x-axis.  */

    theta = *lowx;
    phi = *lowy;

    /* First we convert from the input (theta, phi) to (optx, opty)
       Convert platescale from arcsec/pixel to rad/pixel. */

    phi += phizero;
    /*  If we want the output in degrees, convert here to degrees. */
    phi /= RAD2DEG;
    theta /= (RAD2DEG * DEG2ARCMIN);
    optx = theta * (cos(phi)*pixels_per_radian);
    opty = theta * (sin(phi)*pixels_per_radian);
    outx = -1.0*optx + optaxisx;
    outy = -1.0*(yflip/xflip)*opty + optaxisy;

    *highx = outx;
    *highy = outy;
    status = 0;
  } else {
    inputx = *highx; /* Will be DETX, DETY if OPTCOORD==DET (for example). */
    inputy = *highy;

    /* FIRST transform unrolled DETX/DETY to OPTX/OPTY (offset & flip). */
    optx = -1.0*(inputx - optaxisx);
    opty = -1.0*(xflip/yflip)*(inputy - optaxisy);

    /* SECOND transform from OPTX/OPTX to θ, φ. */
    phi = atan2(opty, optx); /* radians */
    phi -= phizero/RAD2DEG; /* Rotate based on definition of phi=0 */
    theta = sqrt(optx*optx + opty*opty) / pixels_per_radian;

    /* Check to make sure that the angles are in range */
    if (phi < -M_PI || phi > M_PI || theta < 0.0 || theta > M_PI/2.0) {
      phi = DOUBLENULLVALUE;
      theta = DOUBLENULLVALUE;
      status = 1;
    } else {
      /* Theta is output in arcminutes and phi in degrees. */
      if (phi < 0.0) phi += 2*M_PI;
      theta *= (RAD2DEG*DEG2ARCMIN);
      phi *= RAD2DEG;
      *lowx = theta;
      *lowy = phi;
      status = 0;
    }
  }

  return status;
}

/* -------------------------------------------------------------------------- */

int convertCoordCartesian(
  int conv_to_higher_tel,
  int sys, /* Number of the origin system */
  double* lowx,  /* Lower coordinates */
  double* lowy,
  double* highx,  /* Higher coordinates */
  double* highy,
  TELDEF2* teldef /* teldef structure */
) {
  double inputx = 0.0;
  double inputy = 0.0;
  double outx = 0.0;
  double outy = 0.0;
  int seg=teldef->min_segment[sys];
  COORDDEF* origcoord = teldef->coordsys[sys][seg];
  TR_TELESCOPE* telescopeparam = teldef->telescopeparam;
  XFORM2D* trans=0;
  XFORM2D* inverse_trans=0;
  trans = getXform2dFromTrTelescopeParameters(origcoord,telescopeparam);
  inverse_trans = allocateXform2d();

  /* Now for this routine, the convention is that conv_to_higher_local==TRUE means convert
   * FROM telescope Cartesian to OPTCOORD system and conv_to_higher_local==FALSE means
   * convert TO telescope Cartesian from OPTCOORD. The parameter is called
   * ‘conv_to_higher_local’ since it is not always the same as the general parameter
   * ‘conv_to_higher’ (See logical flow in Appendix A!) */
 
  if (conv_to_higher_tel) { /* Converting FROM telescope */
    inputx = *lowx;  /* Input in telescope coordinates (mm) */
    inputy = *lowy;  /* Input in telescope coordinates (mm) */
    invertXform2d(inverse_trans, trans);
    applyXform2dToContinuousCoords(inverse_trans, &outx, &outy, inputx, inputy);
    *highx = outx;  /* Output in destination coordinates (pixel) */
    *highy = outy;  /* Output in destination coordinates (pixel) */
  } else { /* Converting TO telescope */
    inputx = *highx;  /* Input in origin coordinates (pixel) */
    inputy = *highy;  /* Input in origin coordinates (pixel) */
    invertXform2d(inverse_trans, trans);
    applyXform2dToContinuousCoords(trans, &outx, &outy, inputx, inputy);
    *lowx = outx;  /* Output in telescope coordinates (mm) */
    *lowy = outy;  /* Output in telescope coordinates (mm) */
  }
  /* printXform2d(trans, stdout);
     printXform2d(inverse_trans, stdout); */
  destroyXform2d(trans);
  destroyXform2d(inverse_trans);
  return 0;
}

/* -------------------------------------------------------------------------- */

int convertRegionLengthTelescope (
  int conv_to_higher_tel,
  TELDEF2* teldef, /* teldef structure */
  double length_in, /* In units of pixels */
  double* length_out /* In units of mm */
) {
  double scaling = 0.0;
  /* Length parameters are transformed by the ratio of the scales of
   * the lower and upper coordinate systems. This is found in
   * the TELDEF2 structure as telescopeparam->scale, which is defined as
   * the scale of the FOC system in mm/pixel. */
  if (conv_to_higher_tel) { /* Converting FROM telescope */
    scaling = 1.0 / teldef->telescopeparam->scale; /* Units pixel/mm */
  } else { /* Converting TO telescope */
    scaling = teldef->telescopeparam->scale; /* Units mm/pixel */
  }
  if (scaling == 0.0) return 1;
  *length_out = length_in*scaling;
  return 0;
}

/* -------------------------------------------------------------------------- */

int convertRegionAngleTelescope (
  /* Convert an angle parameter for a region based on the relative
   * rotation between the two coordinate systems. */
  int conv_to_higher_tel,
  TELDEF2* teldef, /* teldef structure */
  double in_angle,
  int sys, /* Number of the origin system */
  double* out_angle
) {
  double rotangle = 0.0;
  double tmp_angle = 0.0;
  double sinang = 0.0;
  double cosang = 0.0;
  int xflip = 0;
  int yflip = 0;
  int seg=teldef->min_segment[sys];
  COORDDEF* origcoord = teldef->coordsys[sys][seg];
  TR_TELESCOPE* telescopeparam = teldef->telescopeparam;
  XFORM2D* t_trans=0;
  XFORM2D* inv_trans=0;
  
  t_trans = getXform2dFromTrTelescopeParameters(origcoord, telescopeparam);
  if (conv_to_higher_tel) { /* Converting FROM telescope */
    invertXform2d(inv_trans, t_trans);
    destroyXform2d(t_trans);  /* Avoid memory leak */
    t_trans = inv_trans;
  }
  sinang = t_trans->rot[1][0];
  cosang = t_trans->rot[0][0];
  rotangle = atan2(sinang, cosang);
  rotangle *= 180.0/M_PI;
  tmp_angle = in_angle + rotangle;
  xflip = telescopeparam ->xflip;
  yflip = telescopeparam ->yflip;
  tmp_angle *= xflip;
  if (xflip < 0) {
    tmp_angle -= 180.0;
  }
  tmp_angle *= yflip;
  while (tmp_angle < 0.0) {
    tmp_angle += 360.0;
  }
  while (tmp_angle >= 360.0) {
    tmp_angle -= 360.0;
  }
  *out_angle = tmp_angle;
  return 0;
}

/* -------------------------------------------------------------------------- */

/* Clone one SAORegion structure into another. */

void cloneSAORegion (
  SAORegion* SAORegion_src,
  SAORegion** SAORegion_dest
) {

  int i = 0, j = 0;  /* Loop indexes */
  int nshapes = 0;  /* Number of shapes in structure */
  int npts = 0;  /* Number of points in polygon */

  (*SAORegion_dest) = (SAORegion*)calloc(1, sizeof(SAORegion));

  nshapes = SAORegion_src->nShapes;
  (*SAORegion_dest)->nShapes = nshapes;
  (*SAORegion_dest)->Shapes = (RgnShape*)calloc(nshapes, sizeof(RgnShape));

  for (i=0; i<nshapes; i++) {
    (*SAORegion_dest)->Shapes[i].sign = SAORegion_src->Shapes[i].sign;
    (*SAORegion_dest)->Shapes[i].shape = SAORegion_src->Shapes[i].shape;
    (*SAORegion_dest)->Shapes[i].comp = SAORegion_src->Shapes[i].comp;
    (*SAORegion_dest)->Shapes[i].xmin = SAORegion_src->Shapes[i].xmin;
    (*SAORegion_dest)->Shapes[i].xmax = SAORegion_src->Shapes[i].xmax;
    (*SAORegion_dest)->Shapes[i].ymin = SAORegion_src->Shapes[i].ymin;
    (*SAORegion_dest)->Shapes[i].ymax = SAORegion_src->Shapes[i].ymax;

    if (poly_rgn == SAORegion_src->Shapes[i].shape) {
      npts = SAORegion_src->Shapes[i].param.poly.nPts;
      (*SAORegion_dest)->Shapes[i].param.poly.nPts = npts;
      (*SAORegion_dest)->Shapes[i].param.poly.Pts = (double*)calloc(npts, sizeof(double));
      for (j=0; j<npts; j++) {        
        (*SAORegion_dest)->Shapes[i].param.poly.Pts[j] = SAORegion_src->Shapes[i].param.poly.Pts[j];
      }
    } else {
      for (j=0; j<MAX_TRANS; j++) {  /* MAX_TRANS is a global constant declared above */
        (*SAORegion_dest)->Shapes[i].param.gen.p[j] = SAORegion_src->Shapes[i].param.gen.p[j];
      }
      (*SAORegion_dest)->Shapes[i].param.gen.sinT = SAORegion_src->Shapes[i].param.gen.sinT;
      (*SAORegion_dest)->Shapes[i].param.gen.cosT = SAORegion_src->Shapes[i].param.gen.cosT;
      (*SAORegion_dest)->Shapes[i].param.gen.a = SAORegion_src->Shapes[i].param.gen.a;
      (*SAORegion_dest)->Shapes[i].param.gen.b = SAORegion_src->Shapes[i].param.gen.b;
    }
  }

  (*SAORegion_dest)->wcs.exists = SAORegion_src->wcs.exists;
  (*SAORegion_dest)->wcs.xrefval = SAORegion_src->wcs.xrefval;
  (*SAORegion_dest)->wcs.yrefval = SAORegion_src->wcs.yrefval;
  (*SAORegion_dest)->wcs.xrefpix = SAORegion_src->wcs.xrefpix;
  (*SAORegion_dest)->wcs.yrefpix = SAORegion_src->wcs.yrefpix;
  (*SAORegion_dest)->wcs.xinc = SAORegion_src->wcs.xinc;
  (*SAORegion_dest)->wcs.yinc = SAORegion_src->wcs.yinc;
  (*SAORegion_dest)->wcs.rot = SAORegion_src->wcs.rot;
  for (i=0; i<6; i++) {
    (*SAORegion_dest)->wcs.type[i] = SAORegion_src->wcs.type[i];
  }
}

/* -------------------------------------------------------------------------- */

void findPixelsWithinRegion (
  SAORegion* region,  /* Region structure */
  int sys,  /* Current coordinate system number */
  TELDEF2* teldef,   /* Structure containing information from TELDEF file */
  char* pixeltest,  /*  Method to test if a pixel should count in a region */  
  PixelSet** pixel_set) {   /*  List of pixels for one region */

/* Routine to find the pixel numbers for SXS within the area of a region.
 * Calls the existing routine fits_in_region (heacore/cfitsio/region.c) to find the ACT pixels in
 * the region and then the convertToLowerCoordRawtoPixel to find a list of pixels in the SXS. */

  /* Internal variables. */
  int minx=0, maxx=0, miny=0, maxy=0;  /* Coordinate range for ACT system */
  int ix=0, iy=0;  /* Loop variables, integer pixel coordinates */
  double xp=0.0, yp=0.0;  /* Coordinates of a pixel, converted to double */
  double x=0.0, y=0.0;  /* Output pixel coordinates from convertToLowerCoordinateRawToPixel */
  int convertstatus=0;   /*  Flag for conversion proceding correctly */

  /* Find the minimum and maximum values of the destination coordinates. */
  minx = (int)(teldef->coordsys[sys+1][teldef->min_segment[sys]]->min_x + 0.5);
  maxx = (int)(teldef->coordsys[sys+1][teldef->min_segment[sys]]->max_x + 0.5);
  miny = (int)(teldef->coordsys[sys+1][teldef->min_segment[sys]]->min_y + 0.5);
  maxy = (int)(teldef->coordsys[sys+1][teldef->min_segment[sys]]->max_y + 0.5);

  ahlog_info(LOW, __func__, "Finding pixels in region:  sys=%d, minx=%d, maxx=%d, miny=%d, maxy=%d\n",
    sys, minx, maxx, miny, maxy);

  /* Fill the pixel set if any pixels lie within the region. */
  *pixel_set = createPixelSet();
  for (ix=minx; ix<=maxx; ix++) {
    for (iy=miny; iy<=maxy; iy++) {
      xp=(double)ix;
      yp=(double)iy;
      
      if ( /* Test only the centers */
           ( (0 == strcasecmp(pixeltest, "CENTER")) && 
             (0 != fits_in_region(xp, yp, region)) ) ||  
              
           /* Any corner is included */
           ( (0 == strcasecmp(pixeltest, "PARTIAL")) && 
             ( (0 != fits_in_region(xp, yp, region)) ||
               (0 != fits_in_region(xp+0.5, yp+0.5, region)) ||
               (0 != fits_in_region(xp+0.5, yp-0.5, region)) ||
               (0 != fits_in_region(xp-0.5, yp+0.5, region)) ||
               (0 != fits_in_region(xp-0.5, yp-0.5, region)) ) ) || 

            /* All corners are included */ 
            ( (0 == strcasecmp(pixeltest, "TOTAL")) && 
              ( (0 != fits_in_region(xp, yp, region)) &&
                (0 != fits_in_region(xp+0.5, yp+0.5, region)) &&
                (0 != fits_in_region(xp+0.5, yp-0.5, region)) &&
                (0 != fits_in_region(xp-0.5, yp+0.5, region)) &&
                (0 != fits_in_region(xp-0.5, yp-0.5, region)) ) ) ) {
        
        ahlog_info(LOW, __func__, "xp = %.8g     yp = %.8g\n", xp, yp);
        convertstatus = convertToLowerCoordRawToPixel(teldef, &x, &y, xp, yp, sys);
        if (0 == convertstatus) {
          ahlog_info(LOW, __func__, "x = %.8g     y = %.8g\n", x, y);
          appendPixelToPixelSet(*pixel_set, (long)x);
        }
        
      }
    } /* End loop in x */
  } /* End loop in y */
}

/* -------------------------------------------------------------------------- */

int writeSAORegion (
  TransEnum** trans_type_table,
  SAORegion* SAORegion_output,
  int stopsys_is_radec,
  PixelList* pixel_list,
  char* outfile, int clobber
) {
  
  int write_pixel_list = 0;  /* If true, pixel list needs to be processed */
  int write_file = 0;  /* File doesn't exist or is clobbered */
  char actual_filename[FLEN_FILENAME];  /* After stripping '!' */

  FILE* reg;  /* Output region file descriptor */

  RgnShape* shp;  /* Alias for shape:  struct type is in region.h */
  int i=0;  /* Loop index */
  int j=0;  /* Loop index */
  int num_shapes = SAORegion_output->nShapes;
  int num_shapes_effective = 0;
  long ip=0;  /* Loop index */
  int c1=0;  /* Character subscript */
  int bang=0;  /* Flag for filename preceded by exclamation point (bang) */
  int clobbered=0;  /* Flag for output file that exists and is to be clobbered */
  int rm_status=0;  /* Status flag returned by remove function */

  const char* shape_names[MAX_SHAPES];
  const char* added_comment[MAX_SHAPES];

  /* Subscripts are an enum found in region.h */
  shape_names[point_rgn] = "point";
  shape_names[line_rgn] = "line";
  shape_names[circle_rgn] = "circle";
  shape_names[annulus_rgn] = "annulus";
  shape_names[ellipse_rgn] = "ellipse";
  shape_names[elliptannulus_rgn] = "ellipse";
  shape_names[box_rgn] = "box";
  shape_names[boxannulus_rgn] = "box";
  shape_names[rectangle_rgn] = "rectangle";
  shape_names[diamond_rgn] = "diamond";
  shape_names[sector_rgn] = "sector";
  shape_names[poly_rgn] = "polygon";
  shape_names[panda_rgn] = "panda";
  shape_names[epanda_rgn] = "epanda";
  shape_names[bpanda_rgn] = "bpanda";

  added_comment[point_rgn] = " # point=circle";
  added_comment[line_rgn] = " # line 0 0";
  added_comment[circle_rgn] = "";
  added_comment[annulus_rgn] = "";
  added_comment[ellipse_rgn] = "";
  added_comment[elliptannulus_rgn] = "";
  added_comment[box_rgn] = "";
  added_comment[boxannulus_rgn] = "";
  added_comment[rectangle_rgn] = "";
  added_comment[diamond_rgn] = "";
  added_comment[sector_rgn] = "";
  added_comment[poly_rgn] = "";
  added_comment[panda_rgn] = "";
  added_comment[epanda_rgn] = "";
  added_comment[bpanda_rgn] = "";

  /* Process CLOBBER parameter; obtained via a global clobber state. */
  if ('!' == outfile[0]) {
    c1 = 1;
    bang = 1;
  } else {
    c1 = 0;
    bang = 0;
  }
  strcpy(actual_filename, outfile+c1);

  if (0 == HDfile_check(actual_filename, "r")) {
    /* Output file exists */
    if (0 != clobber || 0 != bang) {
      clobbered = 1;
      write_file = 1;
    } else {
      clobbered = 0;
      write_file = 0;
    }
  } else {
    /* Output file does not exist */
    clobbered = 0;
    write_file = 1;
  }

  if (0 == write_file) {
    ahlog_err(__func__, "Output region file exists but clobber not enabled\n");
    return 1;
  }

  /* Check if a pixel list needs to be written. */
  if (0 == pixel_list) {
    write_pixel_list = 0;
  } else {
    write_pixel_list = 1;
  }

  if (clobbered) {
    rm_status = remove(actual_filename);
    if (0 != rm_status) {
      ahlog_err(__func__, "Could not delete clobbered file %s; remove function returned %d\n", 
        actual_filename, rm_status);
      return 1;
    }
  }

  reg = fopen(actual_filename, "w");
  fprintf(reg, "# Region file written by coordpnt.\n");
  fprintf(reg, "# Comments and global records not copied from input.\n");
  if (0 != write_pixel_list) {
    /* Canonicalize the pixel_list for writing. */
    sortUniquePixelSetsInList(pixel_list);
    fprintf(reg, "# The keyword 'pixel' is non-standard.\n");
    fprintf(reg, "# A pixel region gives a list of individually\n");
    fprintf(reg, "# numbered pixels contained in the input region.\n");
  }
  if (0 != stopsys_is_radec) {
    fprintf(reg, "fk5\n");
  } else {
    fprintf(reg, "physical\n");
  }
  if (0 != write_pixel_list) {
    num_shapes_effective = 1;  /* Pixel list is from region file as a whole */
  } else {
    num_shapes_effective = num_shapes;
  }
  for (i=0; i<num_shapes_effective; i++) {
    shp = &SAORegion_output->Shapes[i];
    fprintf(reg, "%c", (shp->sign?'+':'-'));
    if (0 != write_pixel_list) {
      fprintf(reg, "pixel(");
    } else {
      fprintf(reg, "%s(", shape_names[shp->shape]);
    }
    if (0 != write_pixel_list) {
      for (ip=0; ip<pixel_list->pixel_sets[i]->n_pixels; ip++) {
        fprintf(reg, "%ld", pixel_list->pixel_sets[i]->pixel_nums[ip]);
        if (ip < pixel_list->pixel_sets[i]->n_pixels - 1) fprintf(reg, ",");
      } 
    } else {
      if (poly_rgn == shp->shape) {
        for (j=0; j<shp->param.poly.nPts; j++) {
          if (0<j) fprintf(reg, ",");
          fprintf(reg, "%.8f", shp->param.poly.Pts[j]);
          if (0 != stopsys_is_radec) {
            fprintf(reg, "d");
          }
        }
      } else {
        for (j=0; j<MAX_TRANS; j++) {  /* MAX_TRANS is a global constant above */
          if (e_nullTrans != trans_type_table[shp->shape][j]) {
            if (0<j) fprintf(reg, ",");
            if (e_numberTrans == trans_type_table[shp->shape][j]) {
              fprintf(reg, "%lld", (LONGLONG)shp->param.gen.p[j]);
            } else {
              fprintf(reg, "%.8f", shp->param.gen.p[j]);
              if (0 != stopsys_is_radec && e_angleTrans != trans_type_table[shp->shape][j]) {
                fprintf(reg, "d");
              }
            }
          }
        }
      }
    }
    fprintf(reg, ")%s\n", added_comment[shp->shape]);
  }
  fclose(reg);
  return 0;
}

/* -------------------------------------------------------------------------- */
  
int coordpnt_read_ascii_region( const char *filename,
			    WCSdata    *wcs,
			    SAORegion  **Rgn,
			    int        *status )
/*  Read regions from a SAO-style region file and return the information     */
/*  in the "SAORegion" structure.  If it is nonNULL, use wcs to convert the  */
/*  region coordinates to pixels.  Return an error if region is in degrees   */
/*  but no WCS data is provided.                                             */
/*                                                                           */
/*  fits_read_ascii_region converted to coordpnt_read_ascii_region:          */
/*  omit call to fits_set_region_components.                                 */
/*                                                                           */
/*  Mostly kept programming style of cfitsio for this routine.               */
/*  However, change malloc to calloc.                                        */
/*---------------------------------------------------------------------------*/
{
   char     *currLine;
   char     *namePtr, *paramPtr, *currLoc;
   char     *pX, *pY, *endp;
   long     allocLen, lineLen, hh, mm, dd;
   double   *coords, X, Y, x, y, ss, div, xsave= 0., ysave= 0.;
   int      nParams, nCoords, negdec;
   int      i, done;
   FILE     *rgnFile;
   coordFmt cFmt;
   SAORegion *aRgn;
   RgnShape *newShape, *tmpShape;

   if( *status ) return( *status );

   aRgn = (SAORegion *)calloc( 1, sizeof(SAORegion) );
   if( ! aRgn ) {
      ffpmsg("Couldn't allocate memory to hold Region file contents.");
      return(*status = MEMORY_ALLOCATION );
   }
   aRgn->nShapes    =    0;
   aRgn->Shapes     = NULL;
   if( wcs && wcs->exists )
      aRgn->wcs = *wcs;
   else
      aRgn->wcs.exists = 0;

   cFmt = pixel_fmt; /* set default format */

   /*  Allocate Line Buffer  */

   allocLen = 512;
   currLine = (char *)calloc( allocLen, sizeof(char) );
   if( !currLine ) {
      free( aRgn );
      ffpmsg("Couldn't allocate memory to hold Region file contents.");
      return(*status = MEMORY_ALLOCATION );
   }

   /*  Open Region File  */

   if( (rgnFile = fopen( filename, "r" ))==NULL ) {
      sprintf(currLine,"Could not open Region file %s.",filename);
      ffpmsg( currLine );
      free( currLine );
      free( aRgn );
      return( *status = FILE_NOT_OPENED );
   }
   
   /*  Read in file, line by line  */
   /*  First, set error status in case file is empty */ 
   *status = FILE_NOT_OPENED;

   while( fgets(currLine,allocLen,rgnFile) != NULL ) {

      /* reset status if we got here */
      *status = 0;

      /*  Make sure we have a full line of text  */

      lineLen = strlen(currLine);
      while( lineLen==allocLen-1 && currLine[lineLen-1]!='\n' ) {
         currLoc = (char *)realloc( currLine, 2 * allocLen * sizeof(char) );
         if( !currLoc ) {
            ffpmsg("Couldn't allocate memory to hold Region file contents.");
            *status = MEMORY_ALLOCATION;
            goto error;
         } else {
            currLine = currLoc;
         }
         fgets( currLine+lineLen, allocLen+1, rgnFile );
         allocLen += allocLen;
         lineLen  += strlen(currLine+lineLen);
      }

      currLoc = currLine;
      if( *currLoc == '#' ) {

         /*  Look to see if it is followed by a format statement...  */
         /*  if not skip line                                        */

         currLoc++;
         while( isspace((int) *currLoc) ) currLoc++;
         if( !strncasecmp( currLoc, "format:", 7 ) ) {
            if( aRgn->nShapes ) {
               ffpmsg("Format code encountered after reading 1 or more shapes.");
               *status = PARSE_SYNTAX_ERR;
               goto error;
            }
            currLoc += 7;
            while( isspace((int) *currLoc) ) currLoc++;
            if( !strncasecmp( currLoc, "pixel", 5 ) ) {
               cFmt = pixel_fmt;
            } else if( !strncasecmp( currLoc, "degree", 6 ) ) {
               cFmt = degree_fmt;
            } else if( !strncasecmp( currLoc, "hhmmss", 6 ) ) {
               cFmt = hhmmss_fmt;
            } else if( !strncasecmp( currLoc, "hms", 3 ) ) {
               cFmt = hhmmss_fmt;
            } else {
               ffpmsg("Unknown format code encountered in region file.");
               *status = PARSE_SYNTAX_ERR;
               goto error;
            }
         }

      } else if( !strncasecmp( currLoc, "glob", 4 ) ) {
		  /* skip lines that begin with the word 'global' */

      } else {

         while( *currLoc != '\0' ) {

            namePtr  = currLoc;
            paramPtr = NULL;
            nParams  = 1;

            /*  Search for closing parenthesis  */

            done = 0;
            while( !done && !*status && *currLoc ) {
               switch (*currLoc) {
               case '(':
                  *currLoc = '\0';
                  currLoc++;
                  if( paramPtr )   /* Can't have two '(' in a region! */
                     *status = 1;
                  else
                     paramPtr = currLoc;
                  break;
               case ')':
                  *currLoc = '\0';
                  currLoc++;
                  if( !paramPtr )  /* Can't have a ')' without a '(' first */
                     *status = 1;
                  else
                     done = 1;
                  break;
               case '#':
               case '\n':
                  *currLoc = '\0';
                  if( !paramPtr )  /* Allow for a blank line */
                     done = 1;
                  break;
               case ':':  
                  currLoc++;
                  if ( paramPtr ) cFmt = hhmmss_fmt; /* set format if parameter has : */
                  break;
               case 'd':
                  currLoc++;
                  if ( paramPtr ) cFmt = degree_fmt; /* set format if parameter has d */  
                  break;
               case ',':
                  nParams++;  /* Fall through to default */
               default:
                  currLoc++;
                  break;
               }
            }
            if( *status || !done ) {
               ffpmsg( "Error reading Region file" );
               *status = PARSE_SYNTAX_ERR;
               goto error;
            }

            /*  Skip white space in region name  */

            while( isspace((int) *namePtr) ) namePtr++;

            /*  Was this a blank line? Or the end of the current one  */

            if( ! *namePtr && ! paramPtr ) continue;

            /*  Check for format code at beginning of the line */

            /* printf("%20s   %10d\n", namePtr, aRgn->nShapes); */

            if( !strncasecmp( namePtr, "image;", 6 ) ) {
				namePtr += 6;
				cFmt = pixel_fmt;
            } else if( !strncasecmp( namePtr, "physical;", 9 ) ) {
                                namePtr += 9;
                                cFmt = pixel_fmt;
            } else if( !strncasecmp( namePtr, "linear;", 7 ) ) {
                                namePtr += 7;
                                cFmt = pixel_fmt;
            } else if( !strncasecmp( namePtr, "fk4;", 4 ) ) {
				namePtr += 4;
				cFmt = degree_fmt;
            } else if( !strncasecmp( namePtr, "fk5;", 4 ) ) {
				namePtr += 4;
				cFmt = degree_fmt;
            } else if( !strncasecmp( namePtr, "icrs;", 5 ) ) {
				namePtr += 5;
				cFmt = degree_fmt;

            /* the following 5 cases support region files created by POW 
	       (or ds9 Version 4.x) which
               may have lines containing  only a format code, not followed
               by a ';' (and with no region specifier on the line).  We use
               the 'continue' statement to jump to the end of the loop and
               then continue reading the next line of the region file. */

            } else if( !strncasecmp( namePtr, "fk5", 3 ) ) {
				cFmt = degree_fmt;
                                continue;  /* supports POW region file format */
            } else if( !strncasecmp( namePtr, "fk4", 3 ) ) {
				cFmt = degree_fmt;
                                continue;  /* supports POW region file format */
            } else if( !strncasecmp( namePtr, "icrs", 4 ) ) {
				cFmt = degree_fmt;
                                continue;  /* supports POW region file format */
            } else if( !strncasecmp( namePtr, "image", 5 ) ) {
				cFmt = pixel_fmt;
                                continue;  /* supports POW region file format */
            } else if( !strncasecmp( namePtr, "physical", 8 ) ) {
				cFmt = pixel_fmt;
                                continue;  /* supports POW region file format */


            } else if( !strncasecmp( namePtr, "galactic;", 9 ) ) {
               ffpmsg( "Galactic region coordinates not supported" );
               ffpmsg( namePtr );
               *status = PARSE_SYNTAX_ERR;
               goto error;
            } else if( !strncasecmp( namePtr, "ecliptic;", 9 ) ) {
               ffpmsg( "ecliptic region coordinates not supported" );
               ffpmsg( namePtr );
               *status = PARSE_SYNTAX_ERR;
               goto error;
            }

            /**************************************************/
            /*  We've apparently found a region... Set it up  */
            /**************************************************/

            if( !(aRgn->nShapes % 10) ) {
               if( aRgn->Shapes )
                  tmpShape = (RgnShape *)realloc( aRgn->Shapes,
                                                  (10+aRgn->nShapes)
                                                  * sizeof(RgnShape) );
               else
                  tmpShape = (RgnShape *) calloc( 10, sizeof(RgnShape) );
               if( tmpShape ) {
                  aRgn->Shapes = tmpShape;
               } else {
                  ffpmsg( "Failed to allocate memory for Region data");
                  *status = MEMORY_ALLOCATION;
                  goto error;
               }

            }
            newShape        = &aRgn->Shapes[aRgn->nShapes++];
            newShape->sign  = 1;
            newShape->shape = point_rgn;
	    for (i=0; i<8; i++) newShape->param.gen.p[i] = 0.0;
	    newShape->param.gen.a = 0.0;
	    newShape->param.gen.b = 0.0;
	    newShape->param.gen.sinT = 0.0;
	    newShape->param.gen.cosT = 0.0;

            while( isspace((int) *namePtr) ) namePtr++;
            
			/*  Check for the shape's sign  */

            if( *namePtr=='+' ) {
               namePtr++;
            } else if( *namePtr=='-' ) {
               namePtr++;
               newShape->sign = 0;
            }

            /* Skip white space in region name */

            while( isspace((int) *namePtr) ) namePtr++;
            if( *namePtr=='\0' ) {
               ffpmsg( "Error reading Region file" );
               *status = PARSE_SYNTAX_ERR;
               goto error;
            }
            lineLen = strlen( namePtr ) - 1;
            while( isspace((int) namePtr[lineLen]) ) namePtr[lineLen--] = '\0';

            /*  Now identify the region  */

            if(        !strcasecmp( namePtr, "circle"  ) ) {
               newShape->shape = circle_rgn;
               if( nParams != 3 )
                  *status = PARSE_SYNTAX_ERR;
               nCoords = 2;
            } else if( !strcasecmp( namePtr, "annulus" ) ) {
               newShape->shape = annulus_rgn;
               if( nParams != 4 )
                  *status = PARSE_SYNTAX_ERR;
               nCoords = 2;
            } else if( !strcasecmp( namePtr, "ellipse" ) ) {
               if( nParams < 4 || nParams > 8 ) {
                  *status = PARSE_SYNTAX_ERR;
	       } else if ( nParams < 6 ) {
		 newShape->shape = ellipse_rgn;
		 newShape->param.gen.p[4] = 0.0;
	       } else {
		 newShape->shape = elliptannulus_rgn;
		 newShape->param.gen.p[6] = 0.0;
		 newShape->param.gen.p[7] = 0.0;
	       }
               nCoords = 2;
            } else if( !strcasecmp( namePtr, "elliptannulus" ) ) {
               newShape->shape = elliptannulus_rgn;
               if( !( nParams==8 || nParams==6 ) )
                  *status = PARSE_SYNTAX_ERR;
               newShape->param.gen.p[6] = 0.0;
               newShape->param.gen.p[7] = 0.0;
               nCoords = 2;
            } else if( !strcasecmp( namePtr, "box"    ) 
                    || !strcasecmp( namePtr, "rotbox" ) ) {
	       if( nParams < 4 || nParams > 8 ) {
		 *status = PARSE_SYNTAX_ERR;
	       } else if ( nParams < 6 ) {
		 newShape->shape = box_rgn;
		 newShape->param.gen.p[4] = 0.0;
	       } else {
		  newShape->shape = boxannulus_rgn;
		  newShape->param.gen.p[6] = 0.0;
		  newShape->param.gen.p[7] = 0.0;
	       }
	       nCoords = 2;
            } else if( !strcasecmp( namePtr, "rectangle"    )
                    || !strcasecmp( namePtr, "rotrectangle" ) ) {
               newShape->shape = rectangle_rgn;
               if( nParams < 4 || nParams > 5 )
                  *status = PARSE_SYNTAX_ERR;
               newShape->param.gen.p[4] = 0.0;
               nCoords = 4;
            } else if( !strcasecmp( namePtr, "diamond"    )
                    || !strcasecmp( namePtr, "rotdiamond" )
                    || !strcasecmp( namePtr, "rhombus"    )
                    || !strcasecmp( namePtr, "rotrhombus" ) ) {
               newShape->shape = diamond_rgn;
               if( nParams < 4 || nParams > 5 )
                  *status = PARSE_SYNTAX_ERR;
               newShape->param.gen.p[4] = 0.0;
               nCoords = 2;
            } else if( !strcasecmp( namePtr, "sector"  )
                    || !strcasecmp( namePtr, "pie"     ) ) {
               newShape->shape = sector_rgn;
               if( nParams != 4 )
                  *status = PARSE_SYNTAX_ERR;
               nCoords = 2;
            } else if( !strcasecmp( namePtr, "point"   ) ) {
               newShape->shape = point_rgn;
               if( nParams != 2 )
                  *status = PARSE_SYNTAX_ERR;
               nCoords = 2;
            } else if( !strcasecmp( namePtr, "line"    ) ) {
               newShape->shape = line_rgn;
               if( nParams != 4 )
                  *status = PARSE_SYNTAX_ERR;
               nCoords = 4;
            } else if( !strcasecmp( namePtr, "polygon" ) ) {
               newShape->shape = poly_rgn;
               if( nParams < 6 || (nParams&1) )
                  *status = PARSE_SYNTAX_ERR;
               nCoords = nParams;
            } else if( !strcasecmp( namePtr, "panda" ) ) {
               newShape->shape = panda_rgn;
               if( nParams != 8 )
                  *status = PARSE_SYNTAX_ERR;
               nCoords = 2;
            } else if( !strcasecmp( namePtr, "epanda" ) ) {
               newShape->shape = epanda_rgn;
               if( nParams < 10 || nParams > 11 )
                  *status = PARSE_SYNTAX_ERR;
               newShape->param.gen.p[10] = 0.0;
               nCoords = 2;
            } else if( !strcasecmp( namePtr, "bpanda" ) ) {
               newShape->shape = bpanda_rgn;
               if( nParams < 10 || nParams > 11 )
                  *status = PARSE_SYNTAX_ERR;
               newShape->param.gen.p[10] = 0.0;
               nCoords = 2;
            } else {
               ffpmsg( "Unrecognized region found in region file:" );
               ffpmsg( namePtr );
               *status = PARSE_SYNTAX_ERR;
               goto error;
            }
            if( *status ) {
               ffpmsg( "Wrong number of parameters found for region" );
               ffpmsg( namePtr );
               goto error;
            }

            /*  Parse Parameter string... convert to pixels if necessary  */

            if( newShape->shape==poly_rgn ) {
               newShape->param.poly.Pts = (double *)calloc( nParams,
                                                            sizeof(double) );
               if( !newShape->param.poly.Pts ) {
                  ffpmsg(
                      "Could not allocate memory to hold polygon parameters" );
                  *status = MEMORY_ALLOCATION;
                  goto error;
               }
               newShape->param.poly.nPts = nParams;
               coords = newShape->param.poly.Pts;
            } else
               coords = newShape->param.gen.p;

            /*  Parse the initial "WCS?" coordinates  */
            for( i=0; i<nCoords; i+=2 ) {

               pX = paramPtr;
               while( *paramPtr!=',' ) paramPtr++;
               *(paramPtr++) = '\0';

               pY = paramPtr;
               while( *paramPtr!=',' && *paramPtr != '\0' ) paramPtr++;
               *(paramPtr++) = '\0';

               if( strchr(pX, ':' ) ) {
                  /*  Read in special format & convert to decimal degrees  */
                  cFmt = hhmmss_fmt;
                  mm = 0;
                  ss = 0.;
                  hh = strtol(pX, &endp, 10);
                  if (endp && *endp==':') {
                      pX = endp + 1;
                      mm = strtol(pX, &endp, 10);
                      if (endp && *endp==':') {
                          pX = endp + 1;
                          ss = atof( pX );
                      }
                  }
                  X = 15. * (hh + mm/60. + ss/3600.); /* convert to degrees */

                  mm = 0;
                  ss = 0.;
                  negdec = 0;

                  while( isspace((int) *pY) ) pY++;
                  if (*pY=='-') {
                      negdec = 1;
                      pY++;
                  }
                  dd = strtol(pY, &endp, 10);
                  if (endp && *endp==':') {
                      pY = endp + 1;
                      mm = strtol(pY, &endp, 10);
                      if (endp && *endp==':') {
                          pY = endp + 1;
                          ss = atof( pY );
                      }
                  }
                  if (negdec)
                     Y = -dd - mm/60. - ss/3600.; /* convert to degrees */
                  else
                     Y = dd + mm/60. + ss/3600.;

               } else {
                  X = atof( pX );
                  Y = atof( pY );
               }
               if (i==0) {   /* save 1st coord. in case needed later */
                   xsave = X;
                   ysave = Y;
               }

               if( cFmt!=pixel_fmt ) {
                  /*  Convert to pixels  */
                  if( wcs==NULL || ! wcs->exists ) {
                     ffpmsg("WCS information needed to convert region coordinates.");
                     *status = NO_WCS_KEY;
                     goto error;
                  }
                  
                  if( ffxypx(  X,  Y, wcs->xrefval, wcs->yrefval,
                                      wcs->xrefpix, wcs->yrefpix,
                                      wcs->xinc,    wcs->yinc,
                                      wcs->rot,     wcs->type,
                              &x, &y, status ) ) {
                     ffpmsg("Error converting region to pixel coordinates.");
                     goto error;
                  }
                  X = x; Y = y;
               }
               coords[i]   = X;
               coords[i+1] = Y;

            }

            /* printf ("i = %d\n", i); */

            /*  Read in remaining parameters...  */

            for( ; i<nParams; i++ ) {
               pX = paramPtr;
               while( *paramPtr!=',' && *paramPtr != '\0' ) paramPtr++;
               *(paramPtr++) = '\0';
               coords[i] = strtod( pX, &endp );

	       if (endp && (*endp=='"' || *endp=='\'' || *endp=='d') ) {
		  div = 1.0;
		  if ( *endp=='"' ) div = 3600.0;
		  if ( *endp=='\'' ) div = 60.0;
		  
		  /* Deletion with respect to cfitsio read_ascii_region
                     starts here. */
                  
		  /* parameter given in arcsec so convert to pixels. */
		  /* Increment first Y coordinate by this amount then calc */
		  /* the distance in pixels from the original coordinate. */
		  /* NOTE: This assumes the pixels are square!! */
                  
                  /*
		  if (ysave < 0.)
		     Y = ysave + coords[i]/div;  -- don't exceed -90
		  else
		     Y = ysave - coords[i]/div;  -- don't exceed +90

		  X = xsave;
		  if( ffxypx(  X,  Y, wcs->xrefval, wcs->yrefval,
			       wcs->xrefpix, wcs->yrefpix,
			       wcs->xinc,    wcs->yinc,
			       wcs->rot,     wcs->type,
                               &x, &y, status ) ) {
		     ffpmsg("Error converting region to pixel coordinates.");
		     goto error;
		  }
		 
		  coords[i] = sqrt( pow(x-coords[0],2) + pow(y-coords[1],2) );
                  */
                  
                  /* Deletion with respect to cfitsio read_ascii_region
                     ends here. */

                  coords[i] /= div;

               }
            }

	    /* special case for elliptannulus and boxannulus if only one angle
	       was given */

	    if ( (newShape->shape == elliptannulus_rgn || 
		  newShape->shape == boxannulus_rgn ) && nParams == 7 ) {
	      coords[7] = coords[6];
	    }

            /* Also, correct the position angle for any WCS rotation:  */
            /*    If regions are specified in WCS coordintes, then the angles */
            /*    are relative to the WCS system, not the pixel X,Y system */

	    if( cFmt!=pixel_fmt ) {	    
	      switch( newShape->shape ) {
	      case sector_rgn:
	      case panda_rgn:
		coords[2] += (wcs->rot);
		coords[3] += (wcs->rot);
		break;
	      case box_rgn:
	      case rectangle_rgn:
	      case diamond_rgn:
	      case ellipse_rgn:
		coords[4] += (wcs->rot);
		break;
	      case boxannulus_rgn:
	      case elliptannulus_rgn:
		coords[6] += (wcs->rot);
		coords[7] += (wcs->rot);
		break;
	      case epanda_rgn:
	      case bpanda_rgn:
		coords[2] += (wcs->rot);
		coords[3] += (wcs->rot);
		coords[10] += (wcs->rot);
              default:
                break;
	      }
	    }

	    /* do some precalculations to speed up tests */

	    fits_setup_shape(newShape);

         }  /* End of while( *currLoc ) */
/*
  if (coords)printf("%.8f %.8f %.8f %.8f %.8f\n",
   coords[0],coords[1],coords[2],coords[3],coords[4]); 
*/
      }  /* End of if...else parse line */
   }   /* End of while( fgets(rgnFile) ) */

   /* set up component numbers */
   /* fits_set_region_components( aRgn ); */

error:
   
   /* printf ("aRgn->nShapes = %d\n", aRgn->nShapes); */

   if( *status ) {
      fits_free_region( aRgn );
   } else {
      *Rgn = aRgn;
   }

   fclose( rgnFile );
   free( currLine );
   

   return( *status );
}

/* -------------------------------------------------------------------------- */

PixelSet* createPixelSet(void) {
  int i=0;  /* Loop index */
  PixelSet* new = calloc(1, sizeof(PixelSet));
  for (i=0; i<MAX_PIXELS; i++) new->pixel_nums[i] = -1;
  new->n_pixels = 0;
  return new;
}

/* -------------------------------------------------------------------------- */

void destroyPixelSet(PixelSet* pixel_set) {
  free(pixel_set);
}

/* -------------------------------------------------------------------------- */

int appendPixelToPixelSet(PixelSet* pixel_set, long pixnum) {
  long n = 0;  /* Working variable, number of pixels */
  n = pixel_set->n_pixels;
  if (MAX_PIXELS <= n) return 1;
  pixel_set->pixel_nums[n++] = pixnum;
  pixel_set->n_pixels = n;
  return 0;
}

/* -------------------------------------------------------------------------- */

int cfPixel(const void* px1, const void* px2) {
  if (*((long*)px1) < *((long*)px2)) return -1;
  if (*((long*)px1) > *((long*)px2)) return  1;
  return 0;
}

/* -------------------------------------------------------------------------- */

void sortUniquePixelSet(PixelSet* pixel_set) {
  void* qptr = 0;  /* Array base pointer for qsort */
  size_t count = 0;  /* Array count for qsort */
  size_t elem_size = 0;  /* Array element size for qsort */

  long i=0, j=0;  /* Loop indices for deletion of duplicates */
  long npix=0;  /* Loop limit (varying) */

  /* Is sorting needed? */
  if (2 > pixel_set->n_pixels) return;

  /* Set up and run qsort. */
  qptr = (void*) &(pixel_set->pixel_nums[0]);
  count = (size_t) pixel_set->n_pixels;
  elem_size = sizeof(pixel_set->pixel_nums[0]);
  qsort(qptr, count, elem_size, cfPixel);

  /* Delete duplicates. */
  npix = pixel_set->n_pixels;
  for (i=0; i<npix-1; i++) {
    while ((i < npix-1) && (pixel_set->pixel_nums[i+1] == pixel_set->pixel_nums[i])) {
      for (j=i+1; j<npix; j++) {
        pixel_set->pixel_nums[j-1] = pixel_set->pixel_nums[j];
      }
      npix--;
    }
  }
  pixel_set->n_pixels = npix;
}

/* -------------------------------------------------------------------------- */

PixelList* createPixelList(void) {
  int i=0;  /* Loop index */
  PixelList* new = calloc(1, sizeof(PixelList));
  for (i=0; i<MAX_PIXEL_SETS; i++) new->pixel_sets[i] = 0;
  new->n_sets = 0;
  return new;
}

/* -------------------------------------------------------------------------- */

void destroyPixelList(PixelList* pixel_list) {
  int i=0;  /* Loop index */
  for (i=0; i<pixel_list->n_sets; i++) {
    destroyPixelSet(pixel_list->pixel_sets[i]);
  }
  free(pixel_list);
}

/* -------------------------------------------------------------------------- */

int appendPixelSetToPixelList(PixelList* pixel_list, PixelSet* pixel_set) {
  long n = 0;  /* Working variable, number of pixels */
  n = pixel_list->n_sets;
  if (MAX_PIXEL_SETS <= n) return 1;
  pixel_list->pixel_sets[n++] = pixel_set;
  pixel_list->n_sets = n;
  pixel_set = 0;
  return 0;
}

/* -------------------------------------------------------------------------- */

int appendPixelSetToPixelSet(PixelSet* pixel_set_dest, PixelSet* pixel_set_src) {
  long n=0;  /* Working variable, number of pixels */
  long i=0;  /* Loop variable */
  n = pixel_set_src->n_pixels;
  for (i=0; i<n; i++) {
    if (0 != appendPixelToPixelSet(pixel_set_dest, pixel_set_src->pixel_nums[i])) return 1;
  }
  return 0;
}

/* -------------------------------------------------------------------------- */

void sortUniquePixelSetsInList(PixelList* pixel_list) {
  int i=0;  /* Loop index */
  for (i=0; i<pixel_list->n_sets; i++) {
    sortUniquePixelSet(pixel_list->pixel_sets[i]);
  }
}

/*
  $Log: coordpntlib.c,v $
  Revision 1.23  2016/07/25 20:01:31  rshill
  Add degree units (small d) to RADEC output region files
  for point and length quantities, for compatibility with ds9 and ximage.

  Revision 1.22  2016/04/20 17:47:28  rshill
  Corrected TELPOL calculation to agree with raytracing.

  Revision 1.21  2016/03/22 19:58:32  rshill
  Added CVS log to bottom.

*/

/* Older log entries:

----------------------------
revision 1.20
date: 2016/03/21 20:51:17;  author: rshill;  state: Exp;  lines: +4 -4
Fixed failure to check for error in appendPixelSetToPixelSet.
----------------------------
revision 1.19
date: 2016/03/12 19:21:35;  author: rshill;  state: Exp;  lines: +22 -4
For output pixel lists, merge all regions.
----------------------------
revision 1.18
date: 2016/01/13 03:55:37;  author: klrutkow;  state: Exp;  lines: +26 -2
added new param pixeltest ; findPixelsWithinRegion: added new logic for testing if a pixel should be included in a region, using new param pixeltest
----------------------------
revision 1.17
date: 2015/10/23 00:38:49;  author: rshill;  state: Exp;  lines: +15 -3
Change coordpnt_read_ascii_region to avoid double
transformation of lengths when one coord sys is WCS (RADEC).
----------------------------
revision 1.16
date: 2015/08/21 18:10:23;  author: rshill;  state: Exp;  lines: +2 -2
Corrected a log output statement for missing data item.
----------------------------
revision 1.15
date: 2015/08/21 16:42:15;  author: rshill;  state: Exp;  lines: +2 -2
Corrected an ahlog_info call.
----------------------------
revision 1.14
date: 2015/08/21 16:34:00;  author: rshill;  state: Exp;  lines: +4 -4
Fixed WCS print formats for misplaced equals sign.
----------------------------
revision 1.13
date: 2015/08/21 15:58:13;  author: rshill;  state: Exp;  lines: +31 -31
Change 15-digit to 8-digit formats.
----------------------------
revision 1.12
date: 2015/08/04 03:27:38;  author: rshill;  state: Exp;  lines: +2 -2
Corrected typos and trivial errors.
----------------------------
revision 1.11
date: 2015/08/04 03:22:04;  author: rshill;  state: Exp;  lines: +39 -47
Finished coordpnt cleanup.
----------------------------
revision 1.10
date: 2015/08/03 23:39:16;  author: rshill;  state: Exp;  lines: +24 -13
Partway through code cleanup; more log statements remain to be added.
----------------------------
revision 1.9
date: 2015/07/16 15:56:05;  author: rshill;  state: Exp;  lines: +16 -22
Made rotations in angle transforms consistent; added cast to isspace arg.
----------------------------
revision 1.8
date: 2015/07/08 17:02:01;  author: rshill;  state: Exp;  lines: +2 -5
Tweaked comments.
----------------------------
revision 1.7
date: 2015/06/26 18:41:14;  author: rshill;  state: Exp;  lines: +2 -2
Fixed a degrees to radians conversion in convertCoordPolar.
----------------------------
revision 1.6
date: 2015/06/26 17:58:29;  author: rshill;  state: Exp;  lines: +62 -2
Implemented resolution of TelDef filename via CALDB.
----------------------------
revision 1.5
date: 2015/06/17 20:12:30;  author: rshill;  state: Exp;  lines: +5 -5
Fixed opt[xy] to out[xy] transform in convertCoordPolar;
fixed conversion direction reversal in convertRegionLengthTelescope.
----------------------------
revision 1.4
date: 2015/06/15 21:22:34;  author: rshill;  state: Exp;  lines: +26 -19
Debugged the simple cases of TELXY or TELPOL to and from OPTCOORD.
----------------------------
revision 1.3
date: 2015/06/11 22:20:35;  author: rshill;  state: Exp;  lines: +6 -1
Fixed allocation problems for Xform2d structures in convertCoordCartesian.
----------------------------
revision 1.2
date: 2015/06/10 00:10:28;  author: rshill;  state: Exp;  lines: +228 -1
Added telescope coordinate TELPOL and TELXY.
----------------------------
revision 1.1
date: 2015/05/22 19:58:36;  author: rshill;  state: Exp;
Converted to straight C.  Split into main and lib files.  Moved from astroh/gen/tasks.

*/
