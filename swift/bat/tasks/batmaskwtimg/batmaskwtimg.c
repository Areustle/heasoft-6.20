#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "batmask.h"
#include "batdet.h"
#include "bat_gswdev.h"
#include "imageutils.h"

#include "coordfits.h"

static char taskname[] = "batmaskwtimg";
static char taskver[]  = "1.21";

/* 
 * Mask weighting for binned (image) data
 * 
 * C. Markwardt
 *
 * $Id: batmaskwtimg.c,v 1.82 2010/12/16 06:26:44 craigm Exp $ 
 *
 *  17 Dec 2002 - correct some output-related errors (CM)
 *  02 Jan 2003 - Change call to maskwtimg() (CM)
 *  30 Jan 2003 - Change object position keywords to BAT_RA/DEC
 *
 */


#define TOOLSUB batmaskwtimg
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"



#define NX (BLOCKXCELLS*8)     /* Number of "X" detector cells to process */
#define NY (BLOCKYCELLS*2)     /* Number of "Y" detector cells to process */

#define COMBNONE 0
#define COMBMAX 1
#define COMBMIN 2
#define COMBSUM 3

#define OUTTYPE_WEIGHTS 0
#define OUTTYPE_NONZERO 1
#define OUTTYPE_ZERO    2

struct parm_struct 
{
  char *taskname;               /* Name of this task */
  char *taskver;                /* Version number string */
  char outfile[PIL_PATH_MAX];   /* Output image file name */
  char attitude[PIL_PATH_MAX];  /* Attitude file name */
  char aperture[PIL_PATH_MAX];  /* Mask aperture file name */
  char teldef[PIL_PATH_MAX];    /* Telescope definition file name */
  char detmask[PIL_PATH_MAX];   /* Detector quality mask file name */
  char incatalog[PIL_PATH_MAX]; /* Name of input source catalog */
  char infile[PIL_PATH_MAX];    /* Name of input map */
  char racol[PIL_LINESIZE];     /* Name of RA/POS1 column in catalog */
  char deccol[PIL_LINESIZE];    /* Name of RA/POS1 column in catalog */
  char namecol[PIL_LINESIZE];   /* Name of source name column in catalog */
  char catnumcol[PIL_LINESIZE]; /* Name of source number column in catalog */
  char corrections[PIL_LINESIZE]; /* Corrections string */
  char maskwtcol[PIL_LINESIZE]; /* Mask weight column name */
  double time, time0;           /* Calculate time of the image */
  double tstart, tstop;         /* Start/stop time of the image */
  int coord_type;               /* Integer code for coordinate type */
  char coord_name[20];          /* Coordinate type string name */
  double bat_z, origin_z;       /* Source z-position and origin of coords [cm] */
  double pos1, pos2;            /* Raw user coordinates */
  double srcpos[3];             /* Cartesian source position [cm] */
  double lonlat[2];             /* Angular position, if needed [cm] */
  double distance;              /* Derived, source distance [cm] */
  struct batmaskcorrections_struct corrs;  /* Correction structure */
  double maskoff[3], maskthickness; /* Mask parameters */
  double gapval;                /* Value to insert for gaps */
  int rebalance;                /* Rebalance image? 1=yes, 0=no */
  int outtype;                  /* Output type? 0=weights, 1=footprint */
  int combtype;                 /* Combine type? 0=none, 1=max */
  char distfile[PIL_PATH_MAX];  /* (optional) distortion map file */
};

/* Forward declarations */
int read_coords(struct parm_struct *parms,
		double pos1, double pos2, double *distance);

void print_coords(struct parm_struct *parms);


/* Read parameter file */
int batmaskwtimg_getpar(struct parm_struct *parms) 
{
  int status = 0;
  char coords[FLEN_CARD];
  char maskthickness[FLEN_CARD];
  char outtypestr[FLEN_CARD];
  char combtypestr[FLEN_CARD];
  double distance = 0.0;
  double maskwtswgain = 0.0;

  parms->outfile[0] = 0;
  parms->attitude[0] = 0;
  parms->aperture[0] = 0;
  parms->teldef[0] = 0;
  parms->detmask[0] = 0;
  parms->incatalog[0] = 0;
  parms->infile[0] = 0;
  parms->corrections[0] = 0;
  parms->coord_type = 0;
  parms->lonlat[0] = 0;
  parms->lonlat[1] = -999;
  parms->maskoff[0] = 0;
  parms->maskoff[1] = 0;
  parms->maskoff[2] = 0;
  parms->maskthickness = -1;  /* Default thickness */
  parms->time = 0;
  parms->time0 = 0;
  parms->gapval = 0;          /* Value to fill in gaps */
  parms->rebalance = 1;
  parms->bat_z = 0;
  parms->origin_z = 0;
  parms->pos1 = 0;
  parms->pos2 = 0;
  parms->outtype = OUTTYPE_WEIGHTS;
  parms->combtype = COMBNONE;
  strcpy(parms->racol,"RA_OBJ");
  strcpy(parms->deccol,"DEC_OBJ");
  strcpy(parms->namecol,"NAME");
  strcpy(parms->catnumcol,"CATNUM");
  strcat(parms->maskwtcol, "MASK_WEIGHT");
  parms->distfile[0] = 0;

  if ((status = PILGetFname("outfile", parms->outfile)))
    fprintf(stderr, "Error reading the 'outfile' parameter.\n");
  else if ((status = PILGetString("attitude", parms->attitude)))
    fprintf(stderr, "Error reading the 'attitude' parameter.\n");
  else if ((status = PILGetReal("ra", &parms->pos1)))
    fprintf(stderr, "Error reading the 'ra' parameter.\n");
  else if ((status = PILGetReal("dec", &parms->pos2)))
    fprintf(stderr, "Error reading the 'dec' parameter.\n");
  else if ((status = PILGetString("aperture", parms->aperture)))
    fprintf(stderr, "Error reading the 'aperture' parameter.\n");
  else if ((status = PILGetString("detmask", parms->detmask)))
    fprintf(stderr, "Error reading the 'detmask' parameter.\n");
  else if ((status = PILGetString("corrections", parms->corrections)))
    fprintf(stderr, "Error reading the 'corrections' parameter.\n");
  else if ((status = PILGetReal("maskoffx", &parms->maskoff[0])))
    fprintf(stderr, "Error reading the 'maskxoff' parameter.\n");
  else if ((status = PILGetReal("maskoffy", &parms->maskoff[1])))
    fprintf(stderr, "Error reading the 'maskyoff' parameter.\n");
  else if ((status = PILGetReal("maskoffz", &parms->maskoff[2])))
    fprintf(stderr, "Error reading the 'maskzoff' parameter.\n");
  else if ((status = PILGetString("maskthickness", maskthickness)))
    fprintf(stderr, "Error reading the 'maskthickness' parameter.\n");
  else if ((status = PILGetString("coord_type", coords)))
    fprintf(stderr, "Error reading the 'coord_type' parameter.\n");
  else if ((status = PILGetBool("rebalance", &parms->rebalance)))
    fprintf(stderr, "Error reading the 'rebalance' parameter.\n");
  else if ((status = PILGetReal("subpixsize", &parms->corrs.subpixsize)))
    fprintf(stderr, "Error reading the 'subpixsize' parameter.\n");
  else if ((status = PILGetReal("eff_edge", &parms->corrs.eff_edge)))
    fprintf(stderr, "Error reading the 'eff_edge' parameter.\n");
  else if ((status = PILGetString("teldef", parms->teldef)))
    fprintf(stderr, "Error reading the 'teldef' parameter.\n");
  else if ((status = PILGetReal("time", &parms->time)))
    fprintf(stderr, "Error reading the 'time' parameter.\n");
  else if ((status = PILGetReal("gapval", &parms->gapval)))
    fprintf(stderr, "Error reading the 'gapval' parameter.\n");
  else if ((status = PILGetReal("distance", &distance)))
    fprintf(stderr, "Error reading the 'distance' parameter.\n");
  else if ((status = PILGetReal("bat_z", &parms->bat_z)))
    fprintf(stderr, "Error reading the 'bat_z' parameter.\n");
  else if ((status = PILGetReal("origin_z", &parms->origin_z)))
    fprintf(stderr, "Error reading the 'origin_z' parameter.\n");
  else if ((status = PILGetReal("maskwtswgain", &maskwtswgain)))
    fprintf(stderr, "Error reading the 'maskwtswgain' parameter.\n");
  else if ((status = PILGetString("incatalog", parms->incatalog))) 
    fprintf(stderr, "Error reading the 'incatalog' parameter.\n");    
  else if ((status = PILGetString("infile", parms->infile)))
    fprintf(stderr, "Error reading the 'infile' parameter.\n");    
  else if ((status = PILGetString("racol", parms->racol))) 
    fprintf(stderr, "Error reading the 'racol' parameter.\n");    
  else if ((status = PILGetString("deccol", parms->deccol))) 
    fprintf(stderr, "Error reading the 'deccol' parameter.\n");    
  else if ((status = PILGetString("namecol", parms->namecol))) 
    fprintf(stderr, "Error reading the 'namecol' parameter.\n");    
  else if ((status = PILGetString("catnumcol", parms->catnumcol))) 
    fprintf(stderr, "Error reading the 'catnumcol' parameter.\n");    
  else if ((status = PILGetString("maskwtcol", parms->maskwtcol)))
    fprintf(stderr, "Error reading the 'maskwtcol' parameter.\n");
  else if ((status = PILGetString("outtype", outtypestr)))
    fprintf(stderr, "Error reading the 'outtype' parameter.\n");
  else if ((status = PILGetString("combmeth", combtypestr)))
    fprintf(stderr, "Error reading the 'combmeth' parameter.\n");
  else if ((status = PILGetString("distfile", parms->distfile)))
    fprintf(stderr, "Error reading the 'distfile' parameter.\n");

  if (status) {
    return status;
  }

  parms->time0 = parms->time;

  /* Default value of catalog file and "input" file */
  if (strcasecmp(parms->incatalog,"none") == 0) {
    parms->incatalog[0] = 0;
  }
  if (strcasecmp(parms->infile,"none") == 0) {
    /* ***  Interface change: ***
       After teldef file changes of August-September 2007, it is
       no longer safe to allow the user to specify the default,
       which means "now".  This results in erroneous values, when
       new software is used to analyze old observations.  Thus,
       the infile parameter is now required.
    */
    fprintf(stderr, 
	    "ERROR: 'infile' is a required parameter.\n"
	    "       Please specify an input file that has the correct observation date.\n");
    return -1;
  }

  if (strcasecmp(parms->namecol,"none") == 0) {
    parms->namecol[0] = 0;
  }
  if (strcasecmp(parms->catnumcol,"none") == 0) {
    parms->catnumcol[0] = 0;
  }

  /* Output data type */
  if (strcasecmp(outtypestr, "WEIGHTS") == 0) {
    parms->outtype = OUTTYPE_WEIGHTS;
  } else if (strcasecmp(outtypestr, "NONZERO") == 0) {
    parms->outtype = OUTTYPE_NONZERO;
  } else if (strcasecmp(outtypestr, "ZERO") == 0) {
    parms->outtype = OUTTYPE_ZERO;
  } else {
    fprintf(stderr, "ERROR: outtype must be one of 'WEIGHTS', 'ZERO' or 'NONZERO'\n");
    return -1;
  }

  /* Method of combining output images */
  if (strcasecmp(combtypestr, "NONE") == 0) {
    parms->combtype = COMBNONE;
  } else if (strcasecmp(combtypestr, "MAX") == 0) {
    parms->combtype = COMBMAX;
  } else if (strcasecmp(combtypestr, "MIN") == 0) {
    parms->combtype = COMBMIN;
  } else if (strcasecmp(combtypestr, "SUM") == 0) {
    parms->combtype = COMBSUM;
  } else {
    fprintf(stderr, "ERROR: combtype must be one of 'NONE', 'MAX', 'MIN' or 'SUM'.\n");
    return -1;
  }

  /* Parse mask thickness */
  if (strcasecmp(maskthickness, "INDEF") == 0) {
    parms->maskthickness = -1;
  } else {
    char *endptr = 0;
    parms->maskthickness = strtod(maskthickness, &endptr);
    if (endptr == maskthickness) {
      fprintf(stderr, "ERROR: the maskthickness parameter must be a number\n");
      return -1;
    }
    if (parms->maskthickness < 0) parms->maskthickness = 0;
  }

  /* Parse the coordinate type */
  parms->coord_type = bat_coordtype(coords, parms->coord_name);
  if (parms->coord_type == 0) {
    fprintf(stderr, "Parameter 'coord_type' must be one of:\n"
	    "   sky, cartesian, unit, fswlonlat, grmclonlat, tanxy\n");
    return -1;
  }

  /* Check for default value of attitude file */
  if (strcasecmp(parms->attitude,"none") == 0) {
    parms->attitude[0] = 0;
  }
  if ((parms->attitude[0] == 0) && (parms->coord_type == BCSKY)) {
    fprintf(stderr, 
	    "ERROR: attitude file name cannot be NONE if sky coordinates "
	    "are used.\n");
    return -1;
  }

  /* Check for default value of teldef file */
  if (strcasecmp(parms->teldef,"none") == 0) {
    parms->teldef[0] = 0;
  }
  if ((parms->teldef[0] == 0) && (parms->coord_type == BCSKY)) {
    fprintf(stderr, 
	    "ERROR: teldef file name cannot be NONE if sky coordinates "
	    "are used.\n");
    return -1;
  }

  /* Check for default value of detmask file */
  if (strcasecmp(parms->detmask,"none") == 0) {
    parms->detmask[0] = 0;
  }

  /* Parameter checking on distance */
  if (distance <= 100) {
    fprintf(stderr, "ERROR: distance must be greater than 100 [cm]\n");
    return -1;
  }

  /* Default value for bat_z */
  parms->distance = distance;
  if (parms->bat_z == 0) { 
    parms->bat_z = parms->distance;
  }

  if (strcasecmp(parms->distfile, "none") == 0) {
    parms->distfile[0] = 0;
  }

  /* Parse the corrections */
  parms->corrs.ccosine = 0;
  parms->corrs.cside = 0;
  parms->corrs.copaque = 0;
  parms->corrs.opaquethresh = 0.98;
  parms->corrs.rebalance = parms->rebalance;
  parms->corrs.cpcode = 0;
  parms->corrs.crsquare = 0;
  parms->corrs.cflatfield = 0;
  parms->corrs.cnbatdets = 0;
  parms->corrs.direction = RAYTRACE_BACK;
  parms->corrs.cunbalanced = 0;
  parms->corrs.cinc_edge = 0;
  parms->corrs.cfast = 1;
  parms->corrs.cnearfield = 0;
  parms->corrs.cmaskwt = 0;
  parms->corrs.maskwtswgain = maskwtswgain;
  parms->corrs.cbadcosine = 0;

  if ((status == 0) && (parms->corrections[0] != 0)) {
    char **corrlist = 0;
    int ncorrs, i;

    corrlist = expand_item_list(parms->corrections, &ncorrs, 
				',',1,1,0,&status);
    if (status) { 
      fprintf(stderr, "ERROR: failed to parse 'corrections' string\n");
      return status;
    }

    for (i=0; i<ncorrs; i++) {
      if (strcasecmp(corrlist[i], "default") == 0) {
	/* Default: correct for cosine effects, partial coding, and
	   number of detectors */
	parms->corrs.cflatfield = 1;
	parms->corrs.cpcode = 1;
	parms->corrs.cnbatdets = 1;
	parms->corrs.cmaskwt = 1;
      }
      if (strcasecmp(corrlist[i], "flatfield") == 0) {
      parms->corrs.cflatfield = 1;
      }
      if (strcasecmp(corrlist[i], "nearfield") == 0) {
	parms->corrs.cnearfield = 1;
      }
      if (strcasecmp(corrlist[i], "maskwt") == 0) {
	parms->corrs.cmaskwt = 1;
      }
      if (strcasecmp(corrlist[i], "pcode") == 0) {
	parms->corrs.cpcode = 1;
      }
      if (strcasecmp(corrlist[i], "ndets") == 0) {
	parms->corrs.cnbatdets = 1;
      }
      if (strcasecmp(corrlist[i], "forward") == 0) {
	parms->corrs.direction = RAYTRACE_FORW;
      }
      if (strcasecmp(corrlist[i], "unbalanced") == 0) {
	parms->corrs.cunbalanced = 1;
      }
      if (strcasecmp(corrlist[i], "subpixelate") == 0) {
	/* The "old" subpixelating raytrace method */
	parms->corrs.cfast = 0;
      }
      
      if (strcasecmp(corrlist[i], "opaque") == 0) {
	/* JAY's CODE: NOT DOCUMENTED */
	parms->corrs.copaque = 1;
      }
      if (strcasecmp(corrlist[i], "inc_edge") == 0) {
	/* JAY's CODE: NOT DOCUMENTED */
	parms->corrs.cinc_edge = 1;
      }
      if (strcasecmp(corrlist[i], "cosine") == 0) {
	/* OBSOLETE */
	parms->corrs.ccosine = 1;
      }
      if (strcasecmp(corrlist[i], "rsquare") == 0) {
	/* OBSOLETE */
	parms->corrs.crsquare = 1;
      }
      if (strcasecmp(corrlist[i], "badcosine") == 0) {
	/* TESTING ONLY */
	parms->corrs.cbadcosine = 1;
      }
    }

    free(corrlist);
  }

  return status;
}


void banner(struct parm_struct *parms)
{
  
  headas_chat(2, "******************************************\n");
  headas_chat(1, "         %s v%s\n", parms->taskname, parms->taskver);
  headas_chat(2, "------------------------------------------\n");
  headas_chat(2, "    Output Image: %s\n", parms->outfile);
  if (parms->coord_type == BCSKY) {
    if (parms->attitude[0]) 
      headas_chat(2, "   Attitude File: %s\n", parms->attitude);
    else
      headas_chat(2, "   Attitude File: NONE\n");
    if (parms->teldef[0]) 
      headas_chat(2, "     TelDef File: %s\n", parms->teldef);
    else
      headas_chat(2, "     TelDef File: NONE\n");
    if (parms->time != 0) 
      headas_chat(2, "    Interp. Time: %f\n", parms->time);
    else
      headas_chat(2, "    Interp. Time: (First time in attitude file)\n");
  }
  if (parms->incatalog[0]) {
    headas_chat(2,     "   Input Catalog: %s\n", parms->incatalog);
  }
  if (parms->combtype != COMBNONE) {
    headas_chat(2,     "      Combine By: %s\n",
		(parms->combtype == COMBMAX)?"MAX":
		((parms->combtype == COMBMIN)?"MIN":
		 ((parms->combtype == COMBSUM)?"SUM":"UNKNOWN")));
  }
  headas_chat(2, "  Aperture Image: %s\n", parms->aperture);
  if (parms->detmask[0])
    headas_chat(2, "   Detector Mask: %s\n", parms->detmask); 
  else
    headas_chat(2, "   Detector Mask: NONE\n");
  headas_chat(2,   "     Corrections: %s%s%s%s%s%s%s%s%s%s%s%s%s\n",
	      parms->corrs.cflatfield ? "flatfield " : "", 
	      parms->corrs.cnearfield ? "nearfield " : "", 
	      parms->corrs.cpcode ? "pcode " : "", 
	      parms->corrs.cnbatdets ? "ndets " : "",
	      (parms->corrs.direction == RAYTRACE_FORW) ? "forward " : "",
	      parms->corrs.cunbalanced ? "unbalanced " : "",
	      parms->corrs.cfast ? "" : "subpixelate",
	      parms->corrs.copaque ? "opaque " : "",
	      parms->corrs.cinc_edge ? "inc_edge " : "",
	      parms->corrs.ccosine ? "cosine " : "", 
	      parms->corrs.crsquare ? "rsquare " : "",
	      parms->corrs.cmaskwt ? "maskwt " : "",
	      parms->corrs.cbadcosine ? "badcosine " : "");
  if (parms->corrs.cfast == 0) 
    headas_chat(2, "   Subpixel Size: %f\n", parms->corrs.subpixsize);
  if (parms->corrs.copaque) 
    headas_chat(2, "   Effective edge depth: %f\n", parms->corrs.eff_edge);
  headas_chat(2, "     Coordinates: %s\n", parms->coord_name);
  headas_chat(2, "------------------------------------------\n");

  return;
}

void summary(struct parm_struct *parms)
{
  headas_chat(2, "    Done\n");
  headas_chat(2, "------------------------------------------\n");
}

/* Write source position keywords */
int src_writekey(fitsfile *file, struct parm_struct *parms, 
		 char *objname, int catnum, 
		 double minwt, double maxwt,
		 int ncatrows, int bdistapp,
		 int *status)
{
  char card[FLEN_CARD];
  static int extnum = 0;
  int i;

  if (status == 0) return NULL_INPUT_PTR;
  if (*status) return (*status);
  if (parms == 0) return (*status = NULL_INPUT_PTR);

  /* Determine a unique extension identification */
  if (catnum >= 0) {
    i = catnum; 
  } else {
    i = extnum;
    extnum ++;
  }

  /* Write EXTNAME */
  sprintf(card, "MASK_WEIGHT_%d", i);
  if (ncatrows > 0) {
    /* FITS standard used to forbid EXTNAME in the primary HDU but now
       allows it (version 3.0; released 2008) */
    char *extkey = "EXTNAME";

    /* ... but don't write if there is only one extension */
    fits_update_key(file, TSTRING, extkey, card,
		    "name of this binary table extension", status);
  }

  if (parms->coord_type == BCSKY) {
    /* If the coordinates were sky coordinates */
    fits_update_key(file, TDOUBLE, "BAT_RA", &parms->lonlat[0], 
		    "[deg] Right ascension of source", status);
    fits_update_key(file, TDOUBLE, "BAT_DEC", &parms->lonlat[1], 
		    "[deg] Declination of source", status);
  }
  /* If the coordinates were instrument coordinates */
  fits_update_key(file, TDOUBLE, "BAT_XOBJ", &parms->srcpos[0], 
		  "[cm] Position of source in BAT_X", status);
  fits_update_key(file, TDOUBLE, "BAT_YOBJ", &parms->srcpos[1], 
		  "[cm] Position of source in BAT_Y", status);
  fits_update_key(file, TDOUBLE, "BAT_ZOBJ", &parms->srcpos[2], 
		  "[cm] Position of source in BAT_Z", status);
  fits_update_key(file, TSTRING, "COORTYPE", parms->coord_name, 
		  "Type of coordinates specified for weighting", status);

  /* Optional keywords for source management */
  if (objname[0]) {
    fits_update_key(file, TSTRING, "OBJECT", objname,
		    "Name of raytraced object", status);
  }
  if (catnum >= 0) {
    fits_update_key(file, TINT, "CATNUM", &catnum,
		    "Source I.D. of raytraced object", status);
    fits_update_key(file, TINT, "SOURCEID", &catnum,
		    "Source I.D. of raytraced object", status);
  }
  
  /* Write keywords which indicate which corrections were applied */
  fits_update_key(file, TLOGICAL, "FFAPP", &(parms->corrs.cflatfield),
		  "Projection correction applied?", status);
  fits_update_key(file, TLOGICAL, "NFAPP", &(parms->corrs.cnearfield),
		  "Near-field correction applied? ~(COS+RSQ)", status);
  fits_update_key(file, TLOGICAL, "PCODEAPP", &(parms->corrs.cpcode),
		  "Partial coding correction applied?", status);
  fits_update_key(file, TDOUBLE, "PCODEFR", &(parms->corrs.pcodefr),
		  "Partial coding fraction of target", status);
  fits_update_key(file, TLOGICAL, "NGPIXAPP", &(parms->corrs.cnbatdets),
		  "Normalized by number of detectors?", status);
  fits_update_key(file, TINT, "NGOODPIX", &(parms->corrs.ngoodpix),
		  "Number of enabled detectors", status);
  fits_update_key(file, TDOUBLE, "MSKWTSQF", &(parms->corrs.maskwtsqf),
		  "Half-variance of mask weight map", status);
  fits_update_key(file, TLOGICAL, "MSKWTAPP", &(parms->corrs.cmaskwt),
		  "Correction for mask weight technique applied?", status);

  /* Write DATAMIN/MAX keywords */
  fits_update_key(file, TDOUBLE, "DATAMIN", &minwt, 
		  "Minimum mask weight factor", status);
  fits_update_key(file, TDOUBLE, "DATAMAX", &maxwt, 
		  "Maximum mask weight factor", status);
    

  /* OBSOLETE */
  if (parms->corrs.ccosine) 
    fits_update_key(file, TLOGICAL, "COSAPP", &(parms->corrs.ccosine),
		    "Cosine correction applied?", status);
  if (parms->corrs.crsquare) 
    fits_update_key(file, TLOGICAL, "RSQAPP", &(parms->corrs.crsquare),
		    "R-square correction applied?", status);

  /* Indicate whether distortion correction has been applied */
  /*   1 = image is in "true" coordinates */
  /*   0 = image is in "apparent" coordinates */
  fits_update_key(file, TLOGICAL, "BDISTAPP", &bdistapp, 
		  " BAT image corrected for distortions?", status);

  fits_set_hdustruc(file, status);
  return (*status);
}


/***************************************************************************
* calculate the position of a source with respect to the detector axes
*
* det is a quaternion describing the direction of the source with respect
*     to the detector axes. The answer is returned in this structure.
* 
* teldef is the "telescope definition" calibration file structure
* 
* source is the quaternion giving the direction of the source in
*        celestial coordinates
*
* att is the quaternion giving the current orientation of the satellite
***************************************************************************/
void convertSkyToSensor(double det[3], TELDEF* teldef, 
                        double sky[3], QUAT* att) {

    QUAT corrected;
    ROTMATRIX rot;

    /******************************************************
    * correct for misalignment between the instrument 
    * and the spacecraft axes
    ******************************************************/
    productOfQuats(&corrected, att, teldef->alignment->q_inverse);

    /**************************************************
    * rotate the sky unit vector to get the detector
    * coordinate unit vector
    **************************************************/
    convertQuatToRotMatrix(&rot, &corrected);
    applyRotMatrixToVector(&rot, det, sky);

} /* end of position_of_source function */

/* Do full conversion of sky RA/Dec coordinates to sensor coords.
   Opens all files, etc.

   parms - structure containing task parameters
     required values in parms
       double parms->srcpos[3];
             - upon input, the sky unit vector in J2000 coordinates
             - upon output, the instrument unit vector
       char *parms->teldef;
             - upon input, the name of the teldef file
       char *parms->attitude;
             - upon input, the name of the spacecraft attitude file
       char *parms->time;
             - upon input, the time of interest (MET seconds)
       double parms->distance;
             - upon input, the distance of the source.


  RETURNS: CFITSIO status
*/
int convertRADecToSensor(struct parm_struct *parms)
{
  TELDEF *teldef;
  ATTFILE *attfile;
  QUAT att;
  double skyunit[3];
  double *srcpos;

  srcpos = parms->srcpos;

  /* Get sky unit vector... actually, we already have it from the
     parameter validation step */
  skyunit[0] = srcpos[0];
  skyunit[1] = srcpos[1];
  skyunit[2] = srcpos[2];
  headas_chat(5, "   ... sky unit vector = [%f,%f,%f]...\n",
	      skyunit[0], skyunit[1], skyunit[2]);

  /* Open and initialize the teldef and attitude files */
  headas_chat(5, "   ... opening teldef file ...\n");
  teldef = readTelDef(parms->teldef);
  headas_chat(5, "   ... opening attitude file ...\n");
  attfile = openAttFile(parms->attitude);
  if ((teldef == 0) || (attfile == 0)) {
    fprintf(stderr, "ERROR: could not open teldef and attitude files\n");
    return FILE_NOT_OPENED;
  }
  
  /* Locate proper quaternion and interpolate.  If no time parameter
     has been specified, then take first quaternion from attitude
     file. */
  headas_chat(5, "   ... reading quaternion data ...\n");
  if (parms->time == 0) {
    readQuatFromAttFile(attfile, &att, 1);
  } else {
    findQuatInAttFile(attfile, &att, parms->time);
  }
  headas_chat(5, "   ... converting sky to sensor coordinates ...\n");
  convertSkyToSensor(parms->srcpos, teldef, skyunit, &att);
  
  headas_chat(5, "   ... closing attitude and teldef files ...\n");
  destroyTelDef(teldef);
  closeAttFile(attfile);
  return 0;
}

/* Output array of full mask */
double maskimg[NY][NX];
double outmaskimg[NY-3][NX-2];

/* Main work routine */
int batmaskwtimg_work(struct parm_struct *parms)
{
  struct batmaskplane_struct mask;
  struct batdetplane_struct detplane;
  fitsfile *imgfile = 0;
  fitsfile *incat = 0;
  fitsfile *inptr = 0;

  int status = 0;
  char *outfile, *aperture;
  double *srcpos, srcpos_app[3];
  long naxes[2] = {NX-2, NY-3};
  long fpixel[2] = {1,1};
  long int axesd[2];
  char creator[FLEN_CARD];
  int *detmask = 0;   /* Detector quality mask data */
  int i, j;
  double maxwt = 0, minwt = 0;
  double distance = 0.0;
  long int catrow, ncatrows;
  int racol, deccol, namecol = 0, catnumcol = 0;
  char objname[PIL_LINESIZE];
  int catnum;
  int firstout = 1;
  int firstcomb = 1;
  struct distortmap_struct *distmap = 0; /* Distortion map structure (or 0 for none) */

  if (parms == 0) return NULL_INPUT_PTR;

  outfile = parms->outfile;
  aperture = parms->aperture;
  srcpos = parms->srcpos;
  
  /* "True" position */
  srcpos_app[0] = srcpos[0];  srcpos_app[1] = srcpos[1];  srcpos_app[2] = srcpos[2];

  parms->tstart = 0;
  parms->tstop = 0;

  if (parms->infile[0]) {
    int safestatus = 0;
    fits_open_data(&inptr, parms->infile, READONLY, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not open %s\n", parms->infile);
      return status;
    }
    
    /* Read the time from the file if possible */
    fits_write_errmark();
    fits_read_key(inptr, TDOUBLE, "TSTART", &(parms->tstart), 0, &safestatus);
    fits_read_key(inptr, TDOUBLE, "TSTOP", &(parms->tstop), 0, &safestatus);
    headas_chat(5,"   (tstart=%f tstop=%f)\n", parms->tstart, parms->tstop);
    safestatus = 0;
    fits_clear_errmark();

    /* Compute reasonable center time if possible. */
    if (parms->time == 0) {
      int n = 0;
      if (parms->tstart > 0) {
	parms->time += parms->tstart;
	n++;
      }
      if (parms->tstop > 0) {
	parms->time += parms->tstop;
	n++;
      }
      if (n > 0) {
	parms->time /= n;
      }

    }
    headas_chat(5,"   (time=%f)\n", parms->time);
  }

  /* Opening banner */
  banner(parms);

  /* Fill in parameters given by CALDB */
  if ((strncasecmp(parms->aperture, "CALDB",5) == 0) ||
      (strcasecmp(parms->teldef, "CALDB") == 0) ||
      (strcasecmp(parms->distfile, "CALDB") == 0)) {

    struct caldbparms_struct caldb;

    /* XXX: Fill in default values - worry about date later ??? */
    batkw_to_caldb_parms(inptr, &caldb, 1, &status);

    /* Query CALDB database for aperture */
    if (strncasecmp(parms->aperture, "CALDB",5) == 0) {
      char expr[80];
      char *codenam = "CODED_MASK";
      char *pfile = parms->aperture;
      char online[80], *ponline = online;
      long int extno[1];
      int maxret = 1;
      int nret = 0, nfound = 0;

      if (strcasecmp(parms->aperture, "CALDB") == 0) {
	headas_chat(1, "NOTE: Using the 'FLUX' aperture type\n");
	strcpy(expr, "APERTYPE.eq.\"FLUX\"");
      } else if (strncasecmp(parms->aperture, "CALDB:",6) == 0) {
	sprintf(expr, "APERTYPE.eq.\"%s\"", parms->aperture+6);
      } else {
	fprintf(stderr, "ERROR: aperture must be either CALDB or CALDB:apertype\n");
	return -1;
      }
      
      bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
		       &pfile, extno, &ponline, &nret, &nfound, &status);
      if ((status != 0) || (nret == 0) || (nfound == 0)) {
	fprintf(stderr, "ERROR: could not locate aperture CALDB file\n");
	return status;
      }
    }	       
	
    /* Query CALDB database for teldef file */
    if (strcasecmp(parms->teldef, "CALDB") == 0) {
      char *expr = "-";
      char *codenam = "TELDEF";
      char *pfile = parms->teldef;
      char online[80], *ponline = online;
      long int extno[1];
      int maxret = 1;
      int nret = 0, nfound = 0;

      bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
		       &pfile, extno, &ponline, &nret, &nfound, &status);
      if ((status != 0) || (nret == 0) || (nfound == 0)) {
	fprintf(stderr, "ERROR: could not locate teldef CALDB file\n");
	return status;
      }
    }	       

    /* Read the distortion map if requested - check for CALDB */
    if (strcasecmp(parms->distfile, "CALDB") == 0) {
      char *expr = "-";
      char *codenam = "DET_POSCOR";
      char *pfile = parms->distfile;
      char online[80], *ponline = online;
      long int extno[1];
      int maxret = 1;
      int nret = 0, nfound = 0;
      
      bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX, 
		       &pfile, extno, &ponline, &nret, &nfound, &status);
      if ((status != 0) || (nret == 0) || (nfound == 0)) {
	fprintf(stderr, "ERROR: could not locate the BAT distortion map in CALDB\n");
	return status;
      }
    }	       
    
  }

  /* Read distortion map */
  if (parms->distfile[0] && distmap == 0) {
    if (read_distortmap(parms->distfile, &(distmap), &status)) {
      fprintf(stderr, "ERROR: could not read distortion map %s\n",
	      parms->distfile);
      return status;
    }
  }

  /* Read aperture image */
  fits_open_image(&imgfile, aperture, READONLY, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not open %s\n", aperture);
    return status;
  }
  mask_image(imgfile, &mask, &status);            /* Image */
  mask_readkey(imgfile, &mask, &status);          /* Mask keywords */
  detplane_readkey(imgfile, &detplane, &status);  /* Detector keywords */
  fits_close_file(imgfile, &status);
  imgfile = 0;
  if (status) {
    fprintf(stderr, "ERROR: could not read data from %s\n", aperture);
    return status;
  }

  /* Offset the mask by any amounts specified on the command line */
  mask.meanpos[0] += parms->maskoff[0];
  mask.meanpos[1] += parms->maskoff[1];
  mask.meanpos[2] += parms->maskoff[2];
  if (parms->maskthickness >= 0) {
    mask.cellsize[2] = parms->maskthickness;
  }

  /* Read and apply any detector masks */
  axesd[0] = NX;
  axesd[1] = NY;
  detmask = (int *) malloc(sizeof(int)*NX*NY);
  if (detmask == 0) {
    fprintf(stderr, "ERROR: could not allocate memory for quality mask\n");
    return (status = MEMORY_ALLOCATION);
  }
  for (i=0; i<(NX*NY); i++) detmask[i] = 1;
  
  /* Check if correction for number of detectors is requested, but
     quality map was not supplied */
  if (parms->corrs.cnbatdets && parms->detmask[0] == 0) {
    fprintf(stderr, "WARNING: if correcting for number of detectors ('ndets'),\n");
    fprintf(stderr, "         the detector quality map ('detmask') should also\n");
    fprintf(stderr, "         be specified.  Assuming all detectors are enabled.\n");
  }

  if (parms->detmask[0]) {
    fitsfile *maskfile;
    struct image_struct *detmask1 = 0;
    int safestatus = 0;
    int goodval = 0;
    int nenabled = 0;

    headas_chat(5, "  ...Reading detector mask file...\n");
    fits_open_file(&maskfile, parms->detmask, READONLY, &status);
    detmask1 = image_read(maskfile, &status);
    if (status != 0) {
      fprintf(stderr, "ERROR: could not read quality mask %s\n", 
	      parms->detmask);
      return status;
    }

    fits_write_errmark();
    fits_read_key(maskfile, TINT, "GOODVAL", &goodval, 0, &safestatus);
    if (safestatus) goodval = 0;
    fits_clear_errmark();
    fits_close_file(maskfile, &status);

    headas_chat(5, "    (%dx%d image)\n", detmask1->axes[0], detmask1->axes[1]);
    if (status) {
      fprintf(stderr, "ERROR: could not read detector mask data from %s\n", 
	      parms->detmask);
      return status;
    }

    headas_chat(5, "...checking detector mask image...\n");
    if ( (detmask1->axes[0] > NX) || (detmask1->axes[1] > NY) ) {
      fprintf(stderr, 
	      "ERROR: Detector mask is too large (should be < %dx%d)\n",NX,NY);
      return BAD_DIMEN;
    }

    for (j=0; j<detmask1->axes[1]; j++) {
      for (i=0; i<detmask1->axes[0]; i++) {
	int val = 0;
	if (detmask1->datap[j][i] == goodval) val = 1;
	if (val) nenabled ++;
	detmask[i+j*NX] = val;
      }
    }

    headas_chat(5, "   (enabled detectors %d; goodval=%d)\n", nenabled, goodval);
    image_free(detmask1);
  }

  /* Apply detector gaps to detector mask */
  mkdetgaps_intval(detmask, NX, NY, 0);
  parms->corrs.detmask = detmask;
  parms->corrs.dmx     = NX;
  parms->corrs.dmy     = NY;

  /* Open input catalog if it is present */
  if (parms->incatalog[0]) {
    fits_open_table(&incat,parms->incatalog,READONLY,&status);
    if (status) {
      fprintf(stderr, "ERROR: Unable to open %s catalog for reading\n",
	      parms->incatalog);
      return status;
    }

    fits_get_num_rows(incat, &ncatrows, &status);
    if (status) {
      fprintf(stderr, "ERROR: Unable to determine number of catalog rows\n");
      return status;
    }
    if (ncatrows == 0) {
      fprintf(stderr, "WARNING: no catalog rows were found. No output will be made.\n");
      headas_clobberfile(parms->outfile);
      goto DONE;
    }

    fits_get_colnum(incat, CASEINSEN, parms->racol, &racol, &status);
    fits_get_colnum(incat, CASEINSEN, parms->deccol, &deccol, &status);
    if (parms->namecol[0]) 
      fits_get_colnum(incat, CASEINSEN, parms->namecol, &namecol, &status);
    if (parms->catnumcol[0]) 
      fits_get_colnum(incat, CASEINSEN, parms->catnumcol, &catnumcol, &status);
    headas_chat(5, "  ...columns: RA=%d DEC=%d NAME=%d CATNUM=%d\n",
		racol, deccol, namecol, catnumcol);

    if (status) {
      fprintf(stderr, "ERROR: Could not find %s/%s catalog columns\n",
	      parms->racol, parms->deccol);
      return status;
    }
  }

  if (incat == 0) {
    ncatrows = 1;
  }

  /* Initialize the output image */
  for (j=0; j<NY-3; j++) {
    for (i=0; i<NX-2; i++) {
      outmaskimg[j][i] = 0;
    }
  }

  for (catrow = 1; catrow <= ncatrows; catrow++) {

    objname[0] = 0;
    catnum = -1;

    /* ======  Read catalog data if present */
    if (incat) {
      double nulval = -9999999;
      int anynul1 = 0, anynul2 = 0;

      headas_chat(2,"  Source %d ", catrow);
      fits_read_col(incat, TDOUBLE, racol, catrow, 1, 1, 
		    &nulval, &parms->pos1, &anynul1, &status);
      fits_read_col(incat, TDOUBLE, deccol, catrow, 1, 1, 
		    &nulval, &parms->pos2, &anynul2, &status);
      if (namecol) {
	char *p = objname;
	fits_read_col(incat, TSTRING, namecol, catrow, 1, 1, 
		      0, &p, 0, &status);
	if (status == 0) headas_chat(2, "= %s ", objname);
      }
      if (catnumcol) {
	fits_read_col(incat, TINT, catnumcol, catrow, 1, 1, 
		      0, &catnum, 0, &status);
	if (status == 0) headas_chat(2, "= CATNUM %d ", catnum);
      }
      headas_chat(2, "\n");
      headas_chat(5, "  ...RA=%f DEC=%f NAME=%s CATNUM=%d\n",
		  parms->pos1, parms->pos2, objname, catnum);
	
      if (status) {
	fprintf(stderr, "ERROR: could not read catalog row %ld\n",
		catrow);
	return status;
      }
      if (anynul1 || anynul2) {
	fprintf(stderr, "WARNING: NULL position value found in row %ld\n"
		"         Ignoring this row.\n", catrow);
      }
    }		
      

    /* Convert coordinates into usable cartesian values ... */
    status = read_coords(parms, parms->pos1, parms->pos2, &distance);
    
    if (status) {
      fprintf(stderr, "ERROR: coordinate type '%s' not handled yet\n",
	      parms->coord_name);
      return status;
    }

    /* Account for flight software origin, but not for sky coordinates,
       which is handled below */
    if (parms->coord_type != BCSKY) {
      srcpos[2]     += parms->origin_z;
    }
    
    print_coords(parms);
    
    /* Transform from RA/DEC to instrument coordinates */
    if (parms->coord_type == BCSKY) {
      
      status = convertRADecToSensor(parms);
      if (status != 0) return status;
      
      /* Scale according to Z height */
      if ((parms->bat_z > 0) && (srcpos[2] > 0)) {
	srcpos[0] *= (parms->bat_z-parms->origin_z)/srcpos[2];
	srcpos[1] *= (parms->bat_z-parms->origin_z)/srcpos[2];
	srcpos[2]  = (parms->bat_z-parms->origin_z);
	
	srcpos[2] += parms->origin_z;
      }

    }

    /* "Apparent" position */
    srcpos_app[0] = srcpos[0];  srcpos_app[1] = srcpos[1];  srcpos_app[2] = srcpos[2];

    headas_chat(5, "   ... instrument position = [%f,%f,%f] cm   (IMX,IMY) = (%f,%f)\n",
		srcpos[0], srcpos[1], srcpos[2], 
		(srcpos[2]>0)?(srcpos[0]/srcpos[2]):(100),
		(srcpos[2]>0)?(srcpos[1]/srcpos[2]):(100));
    
    /* Make conversion from "true" to "apparent" tangent plane coords */
    if (distmap && srcpos[2] > 0) {
      double imx_new = -999, imy_new = -999;
      distortmap_coco1(distmap, 
		       srcpos[0]/srcpos[2], srcpos[1]/srcpos[2], &imx_new, &imy_new,
		       TRUE_TO_APP);
      
      /* Convert from tangent plane coordinates to cartesian */
      if ((imx_new != -999) && (imy_new != -999)) {
	srcpos_app[0] = imx_new*srcpos[2];
	srcpos_app[1] = imy_new*srcpos[2];
	srcpos_app[2] = srcpos[2];
      }

      headas_chat(5, "        DISTORTED POSITION   [%f,%f,%f] cm   (IMX,IMY) = (%f,%f)\n",
		  srcpos_app[0], srcpos_app[1], srcpos_app[2], 
		  (srcpos_app[2]>0)?(srcpos_app[0]/srcpos_app[2]):(100),
		  (srcpos_app[2]>0)?(srcpos_app[1]/srcpos_app[2]):(100));
    }
    
    
    /* Initialize the weights to zero */
    for (i=0; i<(NX*NY); i++) { maskimg[i/NX][i%NX] = 0; }

    /* Compute the weights via ray tracing */
    if (srcpos_app[2] > 0) {
      maskwtimg(srcpos_app, &mask, &detplane, &(parms->corrs), &(maskimg[0][0]), 
		NX, NY);
    }
    
    /* Fill gaps with a known value */
    mkdetgaps_val(&(maskimg[0][0]), NX, NY, parms->gapval);


    /* When output type is "footprint", i.e. zero or non-zero
       pixels */
    if (parms->outtype == OUTTYPE_NONZERO) {  /* Non-zero */
      for (j=0; j<NY; j++) {
	for (i=0; i<NX; i++) {
	  if (maskimg[j][i] != 0) maskimg[j][i] = 1;
	}
      }
    } else if (parms->outtype == OUTTYPE_ZERO) {
      for (j=0; j<NY; j++) {
	for (i=0; i<NX; i++) {  /* Zero */
	  if (maskimg[j][i] == 0) maskimg[j][i] = 1; 
	  else maskimg[j][i] = 0;
	}
      }
    }


    /* Method of combination of sub-images */
    if (parms->combtype == COMBNONE || firstcomb) {
      /* Either NONE, or this is the first combination */
      firstcomb = 0;
      for (j=0; j<NY-3; j++) {
	for (i=0; i<NX-2; i++) {  
	  outmaskimg[j][i] = maskimg[j][i];
	}
      }
    } else if (parms->combtype == COMBMAX) {      /* MAX */
      for (j=0; j<NY-3; j++) {
	for (i=0; i<NX-2; i++) {
	  if (maskimg[j][i] > outmaskimg[j][i]) {
	    outmaskimg[j][i] = maskimg[j][i];
	  }
	}
      }
    } else if (parms->combtype == COMBMIN) {      /* MIN */
      for (j=0; j<NY-3; j++) {
	for (i=0; i<NX-2; i++) {
	  if (maskimg[j][i] < outmaskimg[j][i]) {
	    outmaskimg[j][i] = maskimg[j][i];
	  }
	}
      }
    } else if (parms->combtype == COMBSUM) {      /* SUM */
      for (j=0; j<NY-3; j++) {
	for (i=0; i<NX-2; i++) {
	  outmaskimg[j][i] += maskimg[j][i];
	}
      }
    }

    if (parms->combtype == 0 || catrow == ncatrows) {
      /* Find min/max values */
      for (j=0; j<NY; j++) {
	for (i=0; i<NX; i++) {
	  if (maskimg[j][i] > maxwt) maxwt = maskimg[j][i];
	  if (maskimg[j][i] < minwt) minwt = maskimg[j][i];
	}
      }
      
      /* Create the image file on the first pass */
      if (firstout) {
	firstout = 0;
	headas_clobberfile(parms->outfile);
	fits_create_file(&imgfile, parms->outfile, &status);
      }
      
      /* Create an image to hold this map */
      fits_create_img(imgfile, DOUBLE_IMG, 2, naxes, &status);
      fits_write_pix(imgfile, TDOUBLE, fpixel, (NX-2)*(NY-3),
		     &outmaskimg[0][0], &status);
    
      if (inptr && (status == 0)) image_copykeys(imgfile, inptr, &status);
      sprintf(creator, "%s %s", parms->taskname, parms->taskver);
      fits_update_key(imgfile, TSTRING, "CREATOR", creator,
		      "Program that created this FITS file", &status);
      fits_update_key(imgfile, TSTRING, "BATCREAT", creator,
		      "BAT Program that modified this FITS file", &status);
      fits_update_key(imgfile, TDOUBLE, "TSTART", 
		      &(parms->time),
		      "[s] MET Time corresponding to image", &status);
      fits_update_key(imgfile, TDOUBLE, "TSTOP", 
		      &(parms->time),
		      "[s] MET Time corresponding to image", &status);
      
      /* Write mask, detector and source keywords */
      src_writekey(imgfile, parms, objname, catnum, minwt, maxwt, 
		   (incat)?(ncatrows):(0), (distmap)?(1):(0), &status);
      mask_writekey(imgfile, &mask, &status);
      detplane_writekey(imgfile, &detplane, &status);
      
      /* Output teldef and aperture file names as keywords */
      {
	/* Remove path components */
	char *p = rindex(parms->aperture,'/');
	if (p == 0) p = parms->aperture; else p++;
	fits_update_key(imgfile, TSTRING, "APERTURE", p, 
			"BAT aperture file name", &status);
	
	p = rindex(parms->teldef,'/');
	if (p == 0) p = parms->teldef; else p++;
	fits_update_key(imgfile, TSTRING, "BTELDEF", p, 
			"BAT teldef file name", &status);
      }
      
      status = HDpar_stamp(imgfile, 0, &status);

      /* Write the row number in the history section */
      {
	char card[FLEN_CARD];
	sprintf(card, "Catalog row number %ld", catrow);
	fits_write_history(imgfile, card, &status);
      }
      fits_set_hdustruc(imgfile, &status);
    } /* End of output to file */


  } /* End of catalog loop */
    
 DONE:
  if (imgfile) fits_close_file(imgfile, &status);
  if (status == 0)
    headas_chat(1, "  Mask weight map written to %s\n", parms->outfile);
  if (detmask) free(detmask);
  detmask = 0;
  if (inptr) {
    int safestatus = 0;
    fits_close_file(inptr, &safestatus);
    inptr = 0;
  }

  if (incat) {
    int mystatus = 0;
    fits_close_file(incat, &mystatus);
  }

  summary(parms);
  return status;
}

int batmaskwtimg(void)
{
  int status = 0;

  struct parm_struct parms;
  memset(&(parms.corrs), 0, sizeof(parms.corrs));

  /* Register taskname and version. */

  set_toolname(taskname);
  set_toolversion(taskver);

  if ((status = batmaskwtimg_getpar(&parms)) != 0) {
    fprintf(stderr, "Could not read parameter file\n");
    return status;
  }

  parms.taskname = &taskname[0];
  parms.taskver  = &taskver[0];

  return batmaskwtimg_work(&parms);

}

int read_coords(struct parm_struct *parms,
		double pos1, double pos2, double *distance) 
{
  int status = 0;

  parms->srcpos[2] = parms->bat_z - parms->origin_z;
  
  /* Read parameters appropriate for coordinate type */
  switch (parms->coord_type) {
  case BCCARTESIAN: /* Cartesian coordinates (BAT_X/Y/Z) */
    parms->srcpos[0] = pos1;
    parms->srcpos[1] = pos2;
    break;
  case BCTANGENT: /* Tangent plane coordinates */
  case BCUNIT:    /* Cartesian unit vectors */
    parms->srcpos[0] = pos1;
    parms->srcpos[1] = pos2;
    break;
  case BCFSWLONLAT: /* Flight software longitude latitude */
  case BCGRMC:      /* GRMC longitude latitude */
  case BCSKY:       /* RA / DEC */
    parms->lonlat[0] = pos1;
    parms->lonlat[1] = pos2;
    break;
  }

  /* Transform coordinates */
  status = bat_coordver(parms->coord_type, 
			parms->srcpos, parms->lonlat, distance);

  return status;
}

void print_coords(struct parm_struct *parms)
{
  double distance;
  double unit[3];

  unit[0] = parms->srcpos[0];
  unit[1] = parms->srcpos[1];
  unit[2] = parms->srcpos[2];
  distance = sqrt(unit[0]*unit[0] + unit[1]*unit[1] + unit[2]*unit[2]);

  switch (parms->coord_type) {
  case BCFSWLONLAT:
  case BCGRMC:
  case BCSKY:
    headas_chat(2, 
		"       Longitude: %12.6f (deg) Latitude: %12.6f (deg; %s)\n",
		parms->lonlat[0], parms->lonlat[1], parms->coord_name);
  }
  if (parms->coord_type != BCSKY) {
    headas_chat(2, "            %19s %19s %18s\n", "BAT_X", "BAT_Y", "BAT_Z");
    headas_chat(2, " Cartesian  %+19.6f %+19.6f %18.6f  (cm)\n",
		parms->srcpos[0], parms->srcpos[1], parms->srcpos[2]);
    headas_chat(2, " Unit Vect. %+19.6f %+19.6f %18.6f\n",
		parms->srcpos[0]/distance, parms->srcpos[1]/distance, 
		parms->srcpos[2]/distance);
    headas_chat(2, " Tangent    %+19.6f %+19.6f\n", 
		unit[0]/unit[2], unit[1]/unit[2]);
  }
  if (parms->origin_z != 0) {
    headas_chat(2, "        Z Origin: %+10.3f cm\n", parms->origin_z);
  }
  headas_chat(2, "------------------------------------------\n");

}
