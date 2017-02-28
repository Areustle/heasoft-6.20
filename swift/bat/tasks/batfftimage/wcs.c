#include <fitsio.h>
#include <math.h>
#include <string.h>   /* Only source detection */
#include "imageutils.h"
#include "pil.h"
#include "headas.h"
#include "batmask.h"
#include "batdet.h"
#include "bat_gswdev.h"

#include "coordfits.h"
#include "batfftimage.h"

/* 
 * Task to reconstruct sky image from BAT detector plane image
 *   - routines to compute sky coordinates and write proper WCS
 *   keywords to the output file
 *
 *   C. Markwardt
 *   Dec 2003
 *
 * $Id: wcs.c,v 1.14 2005/03/26 06:10:57 craigm Exp $
 *
 * 02 Dec 2003 - split out from batfftimage.c
 * */

/* ============================================================= */

/* Open, read, interpolate attitude file, and determine proper
   coordinate axes for image */
int compute_sky_coords(struct parm_struct *parms,
		     struct image_struct *focal, struct image_struct *aperture,
		     struct batmaskplane_struct *mask, 
		     struct batdetplane_struct *detplane,
		     struct wcscoords_struct *wcscoords,
		     int *status)
{
  TELDEF *teldef;
  ATTFILE *attfile;
  QUAT att;

  QUAT corrected;
  ROTMATRIX rot;

  double ra, dec;   /* RA and DEC of BAT pointing dir (+BAT_Z) in radian */

  /* Components of BAT unit vectors in the BAT instrument coord system */
  double batxunit[3] = {1, 0, 0}; /* BAT_X */
  double batyunit[3] = {0, 1, 0}; /* BAT_Y */
  double batzunit[3] = {0, 0, 1}; /* BAT_Z */

  /* These unit vectors have components expressed in the sky coordinate syst */
  double skyzunit[3];  /* BAT_Z = pointing direction */
  double imxunit[3], imyunit[3]; /* BAT_X/Y = BAT tangent plane axes */
  double raunit[3], decunit[3];  /* RA/DEC = sky tangent plane axes */

  double sr, cr, sd, cd;         /* sin(ra), cos(ra), sin(dec), cos(dec) */
  
  double pc11, pc12, pc21, pc22; /* WCS coordinate matrix */
  double tcent;

  if (*status != 0) return *status;
  
  /* Open and initialize the teldef and attitude files */
  headas_chat(5, "   ... opening teldef file ...\n");
  teldef = readTelDef(parms->teldef);
  headas_chat(5, "   ... opening attitude file ...\n");
  attfile = openAttFile(parms->attitude);
  if ((teldef == 0) || (attfile == 0)) {
    fprintf(stderr, "ERROR: could not open teldef and attitude files\n");
    *status = FILE_NOT_OPENED;
    return *status;
  }

  /* Find a sane time to read the attitude file */
  if (parms->time > 0) {
    /* User-specified time */
    findQuatInAttFile(attfile, &att, parms->time);
  } else if ((parms->tstart == 0) && (parms->tstop == 0)) {
    fprintf(stderr, "WARNING: TSTART and TSTOP were 0\n");
    fprintf(stderr, "         (using first row of attitude file to construct image coordinates)\n");
    readQuatFromAttFile(attfile, &att, 1);
    
  } else if (parms->tstart == 0) {
    /* Only TSTOP is good */
    findQuatInAttFile(attfile, &att, parms->tstop);
  } else if (parms->tstop == 0) {
    /* Only TSTART is good */
    findQuatInAttFile(attfile, &att, parms->tstart);
  } else {
    tcent = 0.5*(parms->tstart + parms->tstop);
    findQuatInAttFile(attfile, &att, tcent);
  }
  closeAttFile(attfile);

  /* There is probably a more explicit way to do this, but this
     approach is pretty simple.  The goal is to do several things.

     1. Estimate RA and DEC of image at reference pixel location. 

     2. Estimate PCj_i coefficients, which represent a rotation of the
     tangent plane sky coordinates with respect to the tangent plane
     image coordinates.

     Item #1 is easy, just take a instrument BAT_Z unit vector,
     components expressed in the instrument frame, and rotate the
     components into the sky coordinate frame.  This uses the teldef
     file for instrument misalignment, and the spacecraft attitude
     quaternion.

     Item #2 is harder, but still straightforward.  Here I take BAT_X
     and BAT_Y unit vectors, components expressed in the instrument
     frame, and rotate the components into the sky frame.  These two
     unit vectors will represent the tangent plane coordinate system.

     From the other angle, it is possible to compute the unit vectors
     which lie in the tangent plane on the sky, along the directions
     of increasing RA and increasing DEC.  

     Once both sets of unit vectors have been computed, it is a matter
     of forming the 4 dot products to construct the PCj_i matrix.

 */

  /* Construct the instrument-to-sky transformation */
  productOfQuats(&corrected, &att, teldef->alignment->q_inverse);

  invertQuatInPlace(&corrected);
  convertQuatToRotMatrix(&rot, &corrected);

  headas_chat(5, "   inverted sensor-to-sky quat = (%f,%f,%f,%f)\n",
	      corrected.p[0], corrected.p[1], corrected.p[2], corrected.p[3]);

  /* Apply transformation to BAT_Z, _X, and _Y unit vectors */
  applyRotMatrixToVector(&rot, skyzunit, batzunit);
  /* These are the components of the IMX- and IMY-aligned vectors, now
     expressed in the inertial sky frame. */
  applyRotMatrixToVector(&rot, imxunit, batxunit);
  applyRotMatrixToVector(&rot, imyunit, batyunit);
  /* No longer need teldef definitions */
  destroyTelDef(teldef);

  /* Debugging output */
  headas_chat(5, "   ... transformed instrument unit vectors to sky ...\n");
  headas_chat(5, "     BATZ dir = (%f,%f,%f)\n", 
	      skyzunit[0], skyzunit[1], skyzunit[2]);
  headas_chat(5, "     IMX  dir = (%f,%f,%f)\n", 
	      imxunit[0], imxunit[1], imxunit[2]);
  headas_chat(5, "     IMY  dir = (%f,%f,%f)\n", 
	      imyunit[0], imyunit[1], imyunit[2]);

  /* Use the BAT_Z axis to define the sky pointing direction in
     RA/DEC.  This is a direct conversion of a unit vector into
     spherical polar longitude and declination */
  ra = 0;
  dec = asin(skyzunit[2]);
  if (fabs(skyzunit[2]) > 0) {
    ra = atan2(skyzunit[1], skyzunit[0]);
  }
  /* Range reduction on the RA */
  while (ra < 0)        ra += (360/RTOD);
  while (ra > 360/RTOD) ra -= (360/RTOD);
  
  headas_chat(5,"   BAT pointing direction (ra,dec) = (%f deg,%f deg)\n", 
	      ra*RTOD, dec*RTOD);

  /* Precompute some values */
  sr = sin(ra);  cr = cos(ra);
  sd = sin(dec); cd = cos(dec);

  /* Compute the tangent plane vectors along increasing RA and DEC,
     components expressed in the sky frame.  */
  raunit[0]  = -sr;    raunit[1]  = +cr;    raunit[2]  = 0;
  decunit[0] = -cr*sd; decunit[1] = -sr*sd; decunit[2] = +cd;
  headas_chat(5, "   ... Computed RA/DEC gradient vectors in sky coordinates ...\n");
  headas_chat(5, "     Tangent Plane RA   dir = (%f,%f,%f)\n", 
	      raunit[0], raunit[1], raunit[2]);
  headas_chat(5, "     Tangent Plane DEC  dir = (%f,%f,%f)\n", 
	      decunit[0], decunit[1], decunit[2]);

  /* Now compute the PCj_i matrix by simple vector dot products of the
     unit vectors.  In PCj_i, j refers to the RA/DEC coordinate
     system, and i refers to the IMX/Y coordinate system. */
  pc11 = raunit[0] *imxunit[0] + raunit[1] *imxunit[1] + raunit[2] *imxunit[2];
  pc12 = raunit[0] *imyunit[0] + raunit[1] *imyunit[1] + raunit[2] *imyunit[2];
  pc21 = decunit[0]*imxunit[0] + decunit[1]*imxunit[1] + decunit[2]*imxunit[2];
  pc22 = decunit[0]*imyunit[0] + decunit[1]*imyunit[1] + decunit[2]*imyunit[2];
  headas_chat(5, "   ... Computed WCS rotation matrix ...\n");
  headas_chat(5,"     PC1_1 = %f   PC1_2 = %f\n", pc11, pc12);
  headas_chat(5,"     PC2_1 = %f   PC2_2 = %f\n", pc21, pc22);
  
  wcscoords->ra    = ra*RTOD;
  wcscoords->dec   = dec*RTOD;
  wcscoords->pc1_1 = pc11;
  wcscoords->pc1_2 = pc12;
  wcscoords->pc2_1 = pc21;
  wcscoords->pc2_2 = pc22;

  /* This ordering and sign of parameters verified by passing through
     a batmaskwtimg -> batfftimage chain */
  wcscoords->rota  = atan2(-pc12,pc11)*RTOD;

  return 0;
}

/* ============================================================= */
/* Compute pixel scaling factors */
int compute_crpixdelt(long *naxes,
		      struct parm_struct *parms,
		      struct image_struct *focal, struct image_struct *aperture,
		      struct batmaskplane_struct *mask, 
		      struct batdetplane_struct *detplane,
		      int handedness,
		      struct wcs_coord *wcs)
{
  double crpix1, crpix2;
  double crval1, crval2;
  double cdelt1, cdelt2;
  
  double xrescale, yrescale, zfact;  /* Scaling factors */
  double xm, ym, zm;  /* Position of mask plane */
  double xd, yd, zd;  /* Position of detector plane */
  double zs;          /* Position of source w.r.t. detector plane */
  double h;           /* Detector plane to mask plane distance */
  double zcorrs=1.0;  /* Corrections to apply */
  double zauto;       /* Autocollimation correction */
  int noverx, novery; /* Oversampling factors in X and Y */

  xm = mask->centpos[0]     + mask->meanpos[0];
  ym = mask->centpos[1]     + mask->meanpos[1];
  zm = mask->centpos[2]     + mask->meanpos[2];
  xd = detplane->centpos[0] + detplane->meanpos[0];
  yd = detplane->centpos[1] + detplane->meanpos[1];
  zd = detplane->centpos[2] + detplane->meanpos[2];

  h = zm - zd;

  noverx = parms->oversampx;
  novery = parms->oversampy;

  /* SEE BELOW COMMENTS on rescaling.  Here we recompute the scaling
     factors, except that now the angles should be measured with
     respect to BAT_Z = 0, which is 3.5 mm below the detector top
     plane. */
  
  xrescale = mask->cellsize[0] / detplane->cellsize[0];
  yrescale = mask->cellsize[1] / detplane->cellsize[1];

  headas_chat(5, "   mask cell x/y=%f/%f\n   det  cell x/y=%f/%f\n",
	      mask->cellsize[0], mask->cellsize[1],
	      detplane->cellsize[0], detplane->cellsize[1]);
  headas_chat(5, "   x/yrescale=%f/%f\n", xrescale, yrescale);

  /* Scaling factors - for case where source is near-field */
  zs = parms->srcpos[2] - zd;
  zfact = 1.0;
  if ( (zs-h) > 0) zfact = zs / (zs - h);

  xrescale *= zfact;
  yrescale *= zfact;

  headas_chat(5, "   zs=%f  height of source\n", zs);
  headas_chat(5, "   h=%f x/yrescale=%f/%f\n", h, xrescale, yrescale);

  /* Center pixel of image */
  if (focal) {
    crpix1 = noverx*(focal->axes[0] + aperture->axes[0]*xrescale+2)/2 + 1;
    crpix2 = novery*(focal->axes[1] + aperture->axes[1]*yrescale+2)/2 + 1;
  } else {
    crpix1 = 0;
    crpix2 = 0;
  }

  /* Center position of image 
       = (MASKCENTX - DETCENTX)/(MASK_SEP) */
  crval1 = (xm - xd) / h;
  crval2 = (ym - yd) / h;

  /* Grid spacing of image - NOTE dependence on near-field distance */
  cdelt1 = detplane->cellsize[0] / h / parms->oversampx / zfact;
  cdelt2 = detplane->cellsize[1] / h / parms->oversampy / zfact;

  /* This rescaling takes into account that officially, the angles are
     measured with respect to the origin of BAT coordinates, which is
     3.5 mm below the CZT top-plane.  The above calculations must be
     done with respect to the top plane, so the below scaling converts
     to the BAT_Z=0 plane.  */
  zcorrs = 1.0;
  zauto  = 1.0;
  if (parms->srcpos[2] > 0) {
    /* CASE: Source at finite distance */

    /* Shift to origin at BAT_Z = 0 */
    zcorrs = (zs/(parms->srcpos[2]-parms->origin[2]));
    headas_chat(5, "   (correcting angle for BAT_Z=origin_z origin)\n");
  }

  if (parms->cautocollim) {
    /* Apply autocollimation correction for case of source at any
       distance.  The z/(z-h) part of the correction appears in CDELT
       above, so the expression below applies to both finite and
       infinite distances. */

    zauto = 1.0/(1.0 - 0.5 * (mask->cellsize[2] / h));
  }

  if (zauto != 1.0) {
    headas_chat(5, "   (correcting for autocollimation; factor=%f)\n", zauto);
  }

  /* Apply corrections to X/Y image scale sizes */
  cdelt1 *= zcorrs*zauto;
  cdelt2 *= zcorrs*zauto;

  headas_chat(5, "   crpix1/2=%f/%f\n   cdelt1/2=%f/%f\n   crval1/2=%f/%f\n",
	      crpix1, crpix2, cdelt1, cdelt2, crval1, crval2);

  /* Reset CRVAL and CDELT, so that CRVAL is zero.  This is because
     the point of IMX=IMY=0 corresponds to the pointing direction, so
     that's the pixel position which we attach to the instrument
     RA/DEC pointing direction */
  crpix1 -= crval1/cdelt1;        /* Move reference pixel positions... */
  crpix2 -= crval2/cdelt2;
  crval1  = 0;                    /* ... so we can set CRVAL to 0 */
  crval2  = 0;

  /* If the image was flipped, we need to flip the coordinates now */
  if ((handedness == HANDEDNESS_LEFT) && naxes) {
    crpix1 = naxes[0] + 1 - crpix1;
    cdelt1 = -cdelt1;
  }
  
  wcs->crval1 = crval1;   wcs->crpix1 = crpix1;   wcs->cdelt1 = cdelt1;
  wcs->crval2 = crval2;   wcs->crpix2 = crpix2;   wcs->cdelt2 = cdelt2;
  wcs->ctype1[0] = 0;
  wcs->ctype2[0] = 0;
  wcs->coordtype[0] = 0;
  wcs->rescale_factor[0] = xrescale;
  wcs->rescale_factor[1] = xrescale;

  return 0;
}


/* ============================================================= */
/* Write image keywords */
int write_imgkeys(fitsfile *imgfile, long naxes[2], 
		  struct parm_struct *parms,
		  struct image_struct *focal, struct image_struct *aperture,
		  struct batmaskplane_struct *mask, 
		  struct batdetplane_struct *detplane,
		  int *status)
{
  char creator[FLEN_CARD];           /* CREATOR keyword value */

  /* These are the WCS coordinate keywords for normal "sky" and
     "tangent" coordinate systems.  The difference is that tangent
     coordinates are always right handed, however see the NOTE below
     regarding the CRPIX values. */
  struct wcs_coord wcs_sky;


  if (status == 0) return NULL_INPUT_PTR;
  if (*status != 0) return (*status);
  if (imgfile == 0) return (*status = NULL_INPUT_PTR);

  compute_crpixdelt(naxes, parms, focal, aperture, mask, detplane, 
		    parms->handedness, &wcs_sky);
  
  sprintf(creator, "%s %s", parms->taskname, parms->taskver);
  fits_update_key(imgfile, TSTRING, "CREATOR", creator,
		 "Program that created this FITS file", status);
  fits_update_key(imgfile, TINT, "OVERSMPX", &parms->oversampx,
		 "Image oversampling in IMX direction", status);
  fits_update_key(imgfile, TINT, "OVERSMPY", &parms->oversampy,
		 "Image oversampling in IMY direction", status);
  if (parms->origin[2] != 0) {
    fits_update_key(imgfile, TDOUBLE, "ANGORIGZ", &parms->origin[2],
		    "[cm] Origin (Z) for angular coords", status);
  }

  /* Eventually the default axis will be the sky coordinate axis RA/DEC */
  fits_write_comment(imgfile, 
    "---------------------------------- Default Coordinates ", status);
  if (*status) return *status;
  
  if (parms->attitude[0]) {
    double cdelt1s, cdelt2s;
    struct wcscoords_struct wcscoords;

    cdelt1s = atan(wcs_sky.cdelt1)*RTOD;  /* Convert to angle, in degrees */
    cdelt2s = atan(wcs_sky.cdelt2)*RTOD;

    compute_sky_coords(parms, focal, aperture, mask, detplane, 
		       &wcscoords, status);
    if (*status) return *status;

    write_wcsaxis(imgfile, 1, "",  "", "", 
		  "RA---TAN", wcs_sky.crpix1, cdelt1s, wcscoords.ra, "deg", status);
    write_wcsaxis(imgfile, 2, "",  "", "", 
		  "DEC--TAN", wcs_sky.crpix2, cdelt2s, wcscoords.dec, "deg", status);
    
    fits_update_key(imgfile, TDOUBLE, "CROTA2", &wcscoords.rota,
		    "[deg] WCS Roll of coordinates w.r.t. sky", status);

#if 0
    /* NOTE: 
       Most viewers do not support these keywords, so I don't write them. 
       Use the CROTA2 keyword instead.
    */
    fits_update_key(imgfile, TDOUBLE, "PC1_1", &wcscoords.pc1_1,
		    "WCS Image transformation matrix", status);
    fits_update_key(imgfile, TDOUBLE, "PC1_2", &wcscoords.pc1_2,
		    "WCS Image transformation matrix", status);
    fits_update_key(imgfile, TDOUBLE, "PC2_1", &wcscoords.pc2_1,
		    "WCS Image transformation matrix", status);
    fits_update_key(imgfile, TDOUBLE, "PC2_2", &wcscoords.pc2_2,
		    "WCS Image transformation matrix", status);
#endif

  } else {
    /* If no attitude file, then output IMX/Y coordinates for the
       primary coordinate system */
    /* NOTE: the reference pixel is always defined by the "sky"
       coordinates, since we may have flipped the image */
    write_wcsaxis(imgfile, 1, "",  "", "", 
		  "IMX", wcs_sky.crpix1, wcs_sky.cdelt1, wcs_sky.crval1, "", status);
    write_wcsaxis(imgfile, 2, "",  "", "", 
		  "IMY", wcs_sky.crpix2, wcs_sky.cdelt2, wcs_sky.crval2, "", status);
  }

  /* Redundant axes describe coordinates in tangent plane */
  /* NOTE: the reference pixel is always defined by the "sky"
     coordinates, since we may have flipped the image */
  fits_write_comment(imgfile, 
    "---------------------------------- Tangent Plane Coordinates ", status);
  write_wcsaxis(imgfile, 1, "T", "TANGENT_PLANE", "TANGENT", 
		"IMX", wcs_sky.crpix1, wcs_sky.cdelt1, wcs_sky.crval1, "", status);
  write_wcsaxis(imgfile, 2, "T", "",              "TANGENT", 
		"IMY", wcs_sky.crpix2, wcs_sky.cdelt2, wcs_sky.crval2, "", status);

  return (*status);
}


