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

static char taskname[] = "batmaskwtevt";
static char taskver[]  = "1.22";

/* 
 * Mask weighting for event data
 * 
 * C. Markwardt
 *
 *  02 Jan 2003 - Change call to maskwtimg() (CM)
 *  30 Jan 2003 - Change object position keywords to BAT_RA/DEC
 *
 * $Id: batmaskwtevt.c,v 1.82 2010/02/25 02:58:28 craigm Exp $ 
 */


#define TOOLSUB batmaskwtevt
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

#define NX (BLOCKXCELLS*8)     /* Number of "X" detector cells to process */
#define NY (BLOCKYCELLS*2)     /* Number of "Y" detector cells to process */
#define MAXWARNS 10            /* Maximum number of out-of-bounds warnings */

struct parm_struct 
{
  char *taskname;               /* Name of this task */
  char *taskver;                /* Version number string */
  char infile[PIL_PATH_MAX];    /* Input event file name */
  char attitude[PIL_PATH_MAX];  /* Attitude file name */
  char aperture[PIL_PATH_MAX];  /* Mask aperture file name */
  char teldef[PIL_PATH_MAX];    /* Telescope definition file name */
  char detmask[PIL_PATH_MAX];   /* Detector quality mask file name */
  char corrections[PIL_LINESIZE]; /* Corrections string */
  char auxfile[PIL_PATH_MAX];   /* Auxiliary output file name */
  char maskwtcol[PIL_LINESIZE]; /* Mask weight column name */
  int coord_type;               /* Integer code for coordinate type */
  char coord_name[20];          /* Coordinate type string name */
  int buffersize;               /* Input event buffer size [rows] */
  double bat_z, origin_z;       /* Source z-position and origin of coords [cm] */
  double pos1, pos2;            /* Raw user coordinates */
  double srcpos[3];             /* Cartesian source position [cm] */
  double lonlat[2];             /* Angular position, if needed [cm] */
  double distance;              /* Derived, source distance [cm] */
  struct batmaskcorrections_struct corrs;  /* Correction structure */
  double maskoff[3], maskthickness; /* Mask parameters */
  double gapval;                /* Value to insert for gaps */
  int rebalance;                /* Rebalance image? 1=yes, 0=no */
  double angtoler;              /* Angular tolerance [tangent] */
  double pcodethresh;           /* Partial coding threshold (0.0-1.0) */
  char distfile[PIL_PATH_MAX];  /* (optional) distortion map file */
  char filtexpr[1000];          /* (optional) filtering expression */
};

/* Forward declarations */
int read_coords(struct parm_struct *parms,
		double pos1, double pos2, double *distance);

void print_coords(struct parm_struct *parms);

/* Read parameter file */
int batmaskwtevt_getpar(struct parm_struct *parms) 
{
  int status = 0;
  char coords[FLEN_CARD];
  char maskthickness[FLEN_CARD];
  double distance = 0.0;
  double maskwtswgain = 0.0;

  parms->infile[0] = 0;
  parms->attitude[0] = 0;
  parms->aperture[0] = 0;
  parms->teldef[0] = 0;
  parms->detmask[0] = 0;
  parms->corrections[0] = 0;
  parms->auxfile[0] = 0;
  parms->coord_type = 0;
  parms->lonlat[0] = 0;
  parms->lonlat[1] = -999;
  parms->maskoff[0] = 0;
  parms->maskoff[1] = 0;
  parms->maskoff[2] = 0;
  parms->maskthickness = -1;  /* Default thickness */
  parms->angtoler = 0;
  parms->rebalance = 1;
  parms->bat_z = 0;
  parms->origin_z = 0;
  parms->pos1 = 0;
  parms->pos2 = 0;
  strcat(parms->maskwtcol, "MASK_WEIGHT");
  parms->distfile[0] = 0;
  parms->filtexpr[0] = 0;

  if ((status = PILGetFname("infile", parms->infile)))
    fprintf(stderr, "Error reading the 'infile' parameter.\n");
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
  else if ((status = PILGetString("auxfile", parms->auxfile)))
    fprintf(stderr, "Error reading the 'auxfile' parameter.\n");
  else if ((status = PILGetString("corrections", parms->corrections)))
    fprintf(stderr, "Error reading the 'corrections' parameter.\n");
  else if ((status = PILGetReal("pcodethresh", &parms->pcodethresh)))
    fprintf(stderr, "Error reading the 'pcodethresh' parameter.\n");
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
  else if ((status = PILGetInt("buffersize", &parms->buffersize)))
    fprintf(stderr, "Error reading the 'buffersize' parameter.\n");
  else if ((status = PILGetReal("subpixsize", &parms->corrs.subpixsize)))
    fprintf(stderr, "Error reading the 'subpixsize' parameter.\n");
  else if ((status = PILGetReal("eff_edge", &parms->corrs.eff_edge)))
    fprintf(stderr, "Error reading the 'eff_edge' parameter.\n");
  else if ((status = PILGetString("teldef", parms->teldef)))
    fprintf(stderr, "Error reading the 'teldef' parameter.\n");
  else if ((status = PILGetReal("distance", &distance)))
    fprintf(stderr, "Error reading the 'distance' parameter.\n");
  else if ((status = PILGetReal("bat_z", &parms->bat_z)))
    fprintf(stderr, "Error reading the 'bat_z' parameter.\n");
  else if ((status = PILGetReal("origin_z", &parms->origin_z)))
    fprintf(stderr, "Error reading the 'origin_z' parameter.\n");
  else if ((status = PILGetReal("angtoler", &parms->angtoler)))
    fprintf(stderr, "Error reading the 'angtoler' parameter.\n");
  else if ((status = PILGetReal("maskwtswgain", &maskwtswgain)))
    fprintf(stderr, "Error reading the 'maskwtswgain' parameter.\n");
  else if ((status = PILGetString("maskwtcol", parms->maskwtcol)))
    fprintf(stderr, "Error reading the 'maskwtcol' parameter.\n");
  else if ((status = PILGetString("distfile", parms->distfile)))
    fprintf(stderr, "Error reading the 'distfile' parameter.\n");
  else if ((status = PILGetString("filtexpr", parms->filtexpr)))
    fprintf(stderr, "Error reading the 'filtexpr' parameter.\n");


  if (status) {
    return status;
  }

  if (strcasecmp(parms->filtexpr,"NONE") == 0) {
    /* Clear filtering expression */
    parms->filtexpr[0] = 0;
  }

  if (parms->buffersize > 4*1024*1024) {
    parms->buffersize = 4*1024*1024;
  } else if (parms->buffersize < 4) {
    parms->buffersize = 4;
  }

  /* Check for default value of auxfile */
  if (strcasecmp(parms->auxfile,"none") == 0) {
    parms->auxfile[0] = 0;
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
  headas_chat(2, "    Input Events: %s\n", parms->infile);
  if (parms->coord_type == BCSKY) {
    if (parms->attitude[0]) 
      headas_chat(2, "   Attitude File: %s\n", parms->attitude);
    else
      headas_chat(2, "   Attitude File: NONE\n");
    if (parms->teldef[0]) 
      headas_chat(2, "     TelDef File: %s\n", parms->teldef);
    else
      headas_chat(2, "     TelDef File: NONE\n");
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

void summary(struct parm_struct *parms, int nevt, int nrecalcs)
{
  headas_chat(1, "  Number of Events Processed: %d\n", nevt);
  headas_chat(5, "  Number of Mask Weight Calculations: %d\n", nrecalcs);
  headas_chat(2, "------------------------------------------\n");
}

/* Write source position keywords */
int src_writekey(fitsfile *file, struct parm_struct *parms, 
		 int bdistapp, int *status)
{
  if (status == 0) return NULL_INPUT_PTR;
  if (*status) return (*status);
  if (parms == 0) return (*status = NULL_INPUT_PTR);

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
    
  return (*status);
}


/* 
 * create_aux_file - create "auxiliary ray-trace" file for writing
 *
 * Creates auxiliary ray-trace file as a binary table with fixed columns.
 * 
 * char *auxfile - name of auxfile
 * fitsfile **fptr - upon return (*fptr) is output CFITSIO file pointer
 * int *status0 - upon return (*status0) contains CFITSIO status
 *
 * RETURNS: CFITSIO status
 */
int create_aux_file(char *auxfile, fitsfile **fptr, int *status0)
{
  fitsfile *auxptr = 0;
  int status = 0;
  int i;
  char *ttype[] = {"TIME", 
		   "BAT_XOBJ", "BAT_YOBJ", "BAT_ZOBJ",
		   "BAT_XAPP", "BAT_YAPP", "BAT_ZAPP",
		   "IMX", "IMY",
		   "IMX_APP", "IMY_APP",
		   "PCODEFR", "NGOODPIX", "MSKWTSQF"};
  char *tform[] = {"1D", 
		   "1D", "1D", "1D", 
		   "1D", "1D", "1D", 
		   "1D", "1D", 
		   "1D", "1D", 
		   "1D", "1J", "1D"};
  char *tunit[] = {"s", 
		   "cm", "cm", "cm", 
		   "cm", "cm", "cm", 
		   "", "", 
		   "", "", 
		   "", "", ""};
  char *comments[] = {"Time of estimate",
		      "Position of source in BAT_X",
		      "Position of source in BAT_Y",
		      "Position of source in BAT_Z",
		      "Apparent position in BAT_X",
		      "Apparent position in BAT_Y",
		      "Apparent position in BAT_Z",
		      "IMX Tangent plane coordinate",
		      "IMY Tangent plane coordinate",
		      "IMX Tangent plane apparent coordinate",
		      "IMY Tangent plane apparent coordinate",
		      "Partial coding fraction",
		      "Number of enabled detectors",
		      "Half-variance of mask weight map"};
  int ncols = sizeof(ttype)/sizeof(ttype[0]);
  char ttypen[FLEN_CARD];

  /* Initialize with null value */
  (*fptr) = 0;
  /* Return if an error condition already exists */
  if (*status0 != 0) return (*status0);

  headas_clobberfile(auxfile);
  fits_create_file(&auxptr, auxfile, &status);
  if (status) {
    fprintf(stderr, "ERROR: could not open %s for writing\n", auxfile);
    return (*status0 = status);
  }
  fits_create_tbl(auxptr, BINARY_TBL, 0, ncols, ttype, tform, tunit, 
		  "BAT_OBJ_POS", &status);
  if (status) {
    int safestatus = 0;
    fprintf(stderr, "ERROR: could not open %s for writing\n", auxfile);
    fits_close_file(auxptr, &safestatus);
    return (*status0 = status);
  }
  /* Apply descriptive column comments */
  for (i=0; i<ncols; i++) {
    if (comments && comments[i] && comments[i][0]) {
      fits_make_keyn("TTYPE", i+1, ttypen, &status);
      fits_modify_comment(auxptr, ttypen, comments[i], &status);
    }
  }
  fits_set_hdustruc(auxptr, &status);

  /* Return auxptr... */
  *fptr = auxptr;
  /* ... and CFITSIO status */
  return (*status0 = status);
}

int write_aux_row(fitsfile **auxptr0, int auxrow, 
		  double tev, double srcpos[3], double srcpos_app[3],
		  double imx, double imy, double imx_app, double imy_app,
		  double pcodefr, int ngoodpix, double maskwtsqf,
		  int *status)
		  
{
  fitsfile *auxptr;

  if (*status) return *status;

  auxptr = *auxptr0;
  if (auxptr == 0) return 0;

  /* Write a new row to the output auxiliary file  */
  fits_write_col(auxptr, TDOUBLE, 1, auxrow, 1, 1, &(tev), status);
  fits_write_col(auxptr, TDOUBLE, 2, auxrow, 1, 1, &(srcpos[0]), status);
  fits_write_col(auxptr, TDOUBLE, 3, auxrow, 1, 1, &(srcpos[1]), status);
  fits_write_col(auxptr, TDOUBLE, 4, auxrow, 1, 1, &(srcpos[2]), status);
  fits_write_col(auxptr, TDOUBLE, 5, auxrow, 1, 1, &(srcpos_app[0]), status);
  fits_write_col(auxptr, TDOUBLE, 6, auxrow, 1, 1, &(srcpos_app[1]), status);
  fits_write_col(auxptr, TDOUBLE, 7, auxrow, 1, 1, &(srcpos_app[2]), status);
  fits_write_col(auxptr, TDOUBLE, 8, auxrow, 1, 1, &imx, status);
  fits_write_col(auxptr, TDOUBLE, 9, auxrow, 1, 1, &imy, status);
  fits_write_col(auxptr, TDOUBLE, 10,auxrow, 1, 1, &imx_app, status);
  fits_write_col(auxptr, TDOUBLE, 11,auxrow, 1, 1, &imy_app, status);
  fits_write_col(auxptr, TDOUBLE, 12,auxrow, 1, 1, &(pcodefr),status);
  fits_write_col(auxptr, TINT,    13,auxrow, 1, 1, &(ngoodpix),status);
  fits_write_col(auxptr, TDOUBLE, 14,auxrow, 1, 1, &(maskwtsqf),status);
  
  if (*status) {
    /* Disable writing to auxil file if it failed one time */
    int mystatus = 0;
    fprintf(stderr, "ERROR: could not write to auxiliary raytrace file\n");
    fprintf(stderr, "ERROR: ceasing to write to auxiliary file\n");
    fits_close_file(auxptr, &mystatus);
    *status = 0;
    *auxptr0 = 0;
  }

  return *status;
}


/* 
 * Check for MASK_WEIGHT column, and if it doesn't exist then add it
 *
 * fitsfile *infptr - pointer to open FITS file
 * char *maskwtcol - name of MASK_WEIGHT column
 * int after_col - MASK_WEIGHT should appear after column number after_col
 * int *mtwcol - upon return, *mtwcol contains column number of MASK_WEIGHT
 * int *status0 - upon return, *status0 contains CFITSIO status
 *
 * RETURNS: CFITSIO status
 */
int check_add_maskwt_col(fitsfile *infptr, char *maskwtcol,  
			 int after_col, int *mtwcol, int *status0)
{
  int status = 0;

  (*mtwcol) = 0;
  /* Return if an error condition already exists */
  if (*status0 != 0) return (*status0);

  fits_write_errmark();
  fits_get_colnum(infptr,CASEINSEN,maskwtcol,mtwcol,&status);
  fits_clear_errmark();

  /* Add the MASK_WEIGHT column if it does not exist */
  if (status) {
    int ncol = 0;
    char ttypen[FLEN_CARD];

    headas_chat(5, " ...creating %s column...\n", maskwtcol);
    status = 0;
    fits_get_num_cols(infptr, &ncol, &status);
    fits_insert_col(infptr, after_col, maskwtcol, "1E", &status);
    if (status) {
      fprintf(stderr, "ERROR: could not create %s column\n", maskwtcol);
      return (*status0 = status);
    }
    fits_set_hdustruc(infptr, &status);

    fits_get_colnum(infptr, CASEINSEN, maskwtcol, mtwcol, &status);
    fits_make_keyn("TTYPE", *mtwcol, ttypen, &status);
    fits_modify_comment(infptr, ttypen, "Ray-traced weighting factor", &status);
  }

  return (*status0 = status);
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


/* 
 * bat_coord_true_to_app - convert "true" to "apparent" coordinates
 *
 * This routine converts a unit vector of "true" coordinates into a
 * "apparent" unit vector as well as tangent plane coordinates.  Here
 * "true" refers to the actual coordinates on the sky as would be seen
 * if the BAT field of view was *not* distorted, and the "apparent" 
 * coordinates are those observed by the BAT with distortion.  The 
 * distortion information is kept in the "distmap" structure.
 *
 * Note that upon failure, *im{x|y}{_app} take a default value of 100.
 *
 * double srcpos[3] - input unit vector true coords, instrument coords
 * double srcpos_app[3] - output unit vector apparent coords, instrument coord
 * double *imx, *imy - upon output, true tangent plane coords
 * double *imx_app, *imy_app - upon output, apparent tangent plane coords
 * double bat_z - BAT_Z position of source
 * double origin_z - BAT_Z origin of coordinates (use when comparing
 *      with flight software)
 * struct distortmap_struct *distmap - pointer to distortion map structure,
 *      or 0 if no distortion is applied.
 *
 * RETURNS: 0 if the caller can proceed
 *          1 if the stream of coordinates has gone out of bounds and
 *          the caller should reset its state variables
 */
int bat_coord_true_to_app(double srcpos[3], double srcpos_app[3],
			  double *imx, double *imy, 
			  double *imx_app, double *imy_app,
			  double bat_z, double origin_z,
			  struct distortmap_struct *distmap)
{
  /* Initialize the outputs to 'undefined' values */
  *imx = 100.0;
  *imy = 100.0;
  *imx_app = 100.0;
  *imy_app = 100.0;

  /* Default "apparent" position is same as "true" position */
  srcpos_app[0] = srcpos[0]; srcpos_app[1] = srcpos[1]; srcpos_app[2] = srcpos[2];

  /* Scale according to Z height and correct for distortions */
  if ((bat_z > 0) && (srcpos[2] > 0)) {
    srcpos[0] *= (bat_z-origin_z)/srcpos[2];
    srcpos[1] *= (bat_z-origin_z)/srcpos[2];
    srcpos[2]  = (bat_z-origin_z);

    /* Account for flight software origin of coordinates */
    srcpos[2] += origin_z;

    srcpos_app[0] = srcpos[0]; 
    srcpos_app[1] = srcpos[1]; 
    srcpos_app[2] = srcpos[2];

    /* Make conversion from "true" to "apparent" tangent plane coords */
    if (distmap && srcpos[2] > 0) {
      distortmap_coco1(distmap, 
		       srcpos[0]/srcpos[2], srcpos[1]/srcpos[2], 
		       imx_app, imy_app,
		       TRUE_TO_APP);
	    
      /* Convert from tangent plane coordinates to cartesian */
      if ((*imx_app != 100) && (*imy_app != 100)) {
	srcpos_app[0] = (*imx_app)*srcpos[2];
	srcpos_app[1] = (*imy_app)*srcpos[2];
	srcpos_app[2] = srcpos[2];
      }
    }

    /* Compute image coordinates, see if source position has changed
       by more than the tolerance.  If yes, then the mask weights need
       to be recalculated again. */
    *imx = srcpos[0]/srcpos[2];
    *imy = srcpos[1]/srcpos[2];
  } else {

    /* Indicate that caller must reset its state because the input
       coordinates went out of bounds */
    return 1;
  }


  /* Indicate input coordinates are in bounds so that caller can
     proceed as usual. */
  return 0;
}

/* Output array of full mask */
double maskimg[NY][NX];

/* Main work routine */
int batmaskwtevt_work(struct parm_struct *parms)
{
  struct batmaskplane_struct mask;
  struct batdetplane_struct detplane;
  fitsfile *infptr = 0, *imgfile = 0, *auxptr = 0;
  int status = 0, mystatus = 0;
  long int auxrow = 0;
  char *infile, *aperture;
  double *srcpos, srcpos_app[3];
  int buffersize;
  int i, j, ievt;
  long int nrows, ndone;
  int nevt = 0;
  int timecol, mtwcol, detxcol, detycol;
  int needrecalc;
  int nrecalcs = 0;
  int *detmask = 0;   /* Detector quality mask data */
  long int axesd[2];  /* Dimensions of detmask */

  double *tev = 0;    /* Event TIME */
  int *detx = 0, *dety = 0; /* Event DETX & DETY */
  float *mweight = 0; /* Computed event mask weight value */
  float maxwt = 0,    /* Computed minimum and maximum mask weight values */
    minwt = 0;
  double tstart, tstop; /* Overall TSTART/TSTOP */
  char *filt = 0;     /* Results of event filtering expression */
  int numwarns = 0;   /* Number of out of bounds warnings */
  long ngoodrows = 0; /* Result of event filtering expression */

  /* Variables related to attitude reconstruction */
  TELDEF *teldef = 0;
  ATTFILE *attfile = 0;
  QUAT att;
  double skyunit[3];
  double imx = 0, imy = 0;       /* Calculated image coordinates per evt ... */
  double imx_app = 0, imy_app = 0; /* Same in "apparent" coordinates */
  double oimx, oimy;             /* ... and stored for current mask wt img */
  double tandist, maxdist2;      /* Angular tolerances for calcing mwt img */
  double distance = 0.0;

  struct distortmap_struct *distmap = 0; /* Distortion map structure (or 0 for none) */

  if (parms == 0) return NULL_INPUT_PTR;

  banner(parms);

  /* Open and read mask and detector-related data */
  infile = parms->infile;
  aperture = parms->aperture;
  srcpos = parms->srcpos;

  /* "True" position */
  srcpos_app[0] = srcpos[0];  srcpos_app[1] = srcpos[1];  srcpos_app[2] = srcpos[2];

  buffersize = parms->buffersize;
  maxdist2 = parms->angtoler * parms->angtoler;

  /* Open input/output file */
  fits_open_file(&infptr,infile,READWRITE,&status); 
  if (status) {
    fprintf(stderr, "Unable to open %s for read/write access\n", 
	    infile);
    return status;
  }

  /* Move to the first extension containing BAT events.  */
  fits_movnam_hdu(infptr, BINARY_TBL, "EVENTS", 0, &status);
  if (status != 0) {
    fprintf(stderr, "The input file does not contain an EVENTS extension\n");
    return status;
  }

  /* Fill in parameters given by CALDB */
  if ((strncasecmp(parms->aperture, "CALDB",5) == 0) ||
      (strcasecmp(parms->teldef, "CALDB") == 0) ||
      (strcasecmp(parms->distfile, "CALDB") == 0)) {

    struct caldbparms_struct caldb;

    batkw_to_caldb_parms(infptr, &caldb, 1, &status);
    if (status) {
      fprintf(stderr, "ERROR: could not determine CALDB parameters from %s\n",
	      parms->infile);
      return status;
    }

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
    srcpos_app[2] += parms->origin_z;

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
    }
  }

  print_coords(parms);

  /* Special processing for sky coordinates: open and interpolate
     attitude file */
  if (parms->coord_type == BCSKY) {
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

    /* NOTE: final processing for sky coordinates occurs below */
  }

  if (fits_get_num_rows(infptr, &nrows, &status)) {
    fprintf(stderr, "The input file is not well-formed\n");
    return status;
  }

  status = 0;

  fits_get_colnum(infptr,CASEINSEN,"TIME",&timecol,&status);
  fits_get_colnum(infptr,CASEINSEN,"DETX",&detxcol,&status);
  fits_get_colnum(infptr,CASEINSEN,"DETY",&detycol,&status); 

  if (status) {
    fprintf(stderr, "The input file does not contain the required columns:\n"
	    "   TIME, DETX, and DETY\n");
    return status;
  }

  /* Create MASK_WEIGHT column if it does not already exist */
  check_add_maskwt_col(infptr, parms->maskwtcol, detxcol, &mtwcol, &status);

  if (status) {
    fprintf(stderr, "The input file does not contain the %s column\n",
	    parms->maskwtcol);
    return status;
  }

  fits_get_colnum(infptr,CASEINSEN,"TIME",&timecol,&status);
  fits_get_colnum(infptr,CASEINSEN,"DETX",&detxcol,&status);
  fits_get_colnum(infptr,CASEINSEN,"DETY",&detycol,&status); 

  if (status) {
    fprintf(stderr, "The input file does not contain the required columns:\n"
	    "   TIME, DETX, and DETY\n");
    return status;
  }

  /* Open the output auxiliary file */
  if (parms->auxfile[0]) {
    create_aux_file(parms->auxfile, &auxptr, &status);
    if (status) { 
      return status;
    }
  }

  tev     = (double *) malloc(sizeof(double)*buffersize);
  detx    = (int *) malloc(sizeof(int)*buffersize);
  dety    = (int *) malloc(sizeof(int)*buffersize);
  mweight = (float *) malloc(sizeof(float)*buffersize);
  filt    = (char *) malloc(sizeof(char)*buffersize);

  if (tev == 0 || detx == 0 || dety == 0 || mweight == 0) {
    fprintf(stderr, "Unable to allocate memory for buffers\n");
    if (tev) free(tev);
    if (detx) free(detx);
    if (dety) free(dety);
    if (mweight) free(mweight);
    if (filt) free(filt);
    return MEMORY_ALLOCATION;
  }

  /* Default value for filter is 1; this will get overwritten if the
     user specified a filter expression */
  for (i=0; i<buffersize; i++) { filt[i] = 1; }
  ngoodrows = buffersize;
  /* NOTE: filt[] and ngoodrows may be changed in the call to
     fits_find_rows() */

  /* ==================================== */
  /* MASTER LOOP: loop over chunks of events */
  ndone = 0;
  needrecalc = 1;
  oimx = 100; oimy = 100;
  tstart = +1e307; tstop = -1e307;
  while (ndone < nrows) { 
    int nbuf = (nrows - ndone);
    int off = ndone + 1;
    
    if (nbuf > buffersize) {
      nbuf = buffersize;
    }

    /* Read a chunk of events */
    fits_read_col(infptr,TDOUBLE,timecol,off,1,nbuf,0,tev, 0,&status);
    fits_read_col(infptr,TINT,   detxcol,off,1,nbuf,0,detx,0,&status);
    fits_read_col(infptr,TINT,   detycol,off,1,nbuf,0,dety,0,&status);

    /* Scan the input data for the requested filtering expression */
    if (parms->filtexpr[0]) {
      fits_find_rows(infptr, parms->filtexpr, off, nbuf, 
		     &ngoodrows, filt, &status);
      /* NOTE: filt[] and ngoodrows are updated here */
    }

    if (status) {
      fprintf(stderr, "Error while reading from input events file\n");
      break;
    }

    /* ... Mask weighting happens here ... */
    for (ievt=0; ievt<nbuf; ievt++) {
      
      /* For sky coordinates, we need to do the coordinate transform
         every time ... */
      if (parms->coord_type == BCSKY && ngoodrows > 0) {
	findQuatInAttFile(attfile, &att, tev[ievt]);
	convertSkyToSensor(srcpos, teldef, skyunit, &att);
	/* Convert to instrument coordinates -- "True" position */

	if (bat_coord_true_to_app(srcpos, srcpos_app, 
				  &imx, &imy, &imx_app, &imy_app, 
				  parms->bat_z, parms->origin_z, distmap)) {
	  /* Reset old values if the current value is wacked */
	  oimx = 100;  
	  oimy = 100;
	}

	/* Compute the distance between the previously ray-traced
	   position and the new position.  If we have moved more than
	   maxdist, then we should recompute the ray-trace. */
	/* Note: this is the squared distance */
	tandist = (imx-oimx)*(imx-oimx) + (imy-oimy)*(imy-oimy);

	if (tandist > maxdist2 || oimx == 100 || oimy == 100) {
	  needrecalc = 1;

	  headas_chat(5, " ..t[%d]=%f imx=%f imy=%f cos(theta)=%f tandist=%g..\n",
		      ndone+ievt, tev[ievt], imx, imy, srcpos[2], sqrt(tandist));

	}
      }

      if (imx_app == 100 && imx != 100) imx_app = imx;
      if (imy_app == 100 && imy != 100) imy_app = imy;

      /* -------------------- */
      /* By this point we have decided whether or not we need to
	 recalculate the ray-trace, either because this is the first
	 time to reach this point in the code, or if the spacecraft
	 pointing direction has moved enough that the previous
	 ray-trace has become invalid. So, we go ahead and do the
	 ray-trace calculation.

	 Special note here.  If 'ngoodrows' is zero, this means that
	 no good-filtered events were found, and we can skip this
	 whole chunk.  In this case, a ray-trace calculation does not
	 occur.  However, we do not reset 'needrecalc' and do not
	 reset oimx or oimy, so if good-filtered events are found in
	 a later chunk, a ray-trace is immediately triggered. 
      */
      if (needrecalc && ngoodrows > 0) {
	int ii, jj;

	/* Pre-zero the weight array since maskwtimg() is cumulative */
	for (jj=0; jj<NY; jj++) 
	  for (ii=0; ii<NX; ii++) 
	    maskimg[jj][ii] = 0;

	/* Explicitly discard rays coming from behind the instrument */
	if (srcpos_app[2] > 0) {
	  /* Here is where the actual ray-tracing calculation is done! */
	  maskwtimg(srcpos_app, &mask, &detplane, 
		    &(parms->corrs), &(maskimg[0][0]),
		    NX, NY);
	  nrecalcs ++;
	}

	/* Store image coordinates for next pass */
	needrecalc = 0;
	oimx = imx;
	oimy = imy;

	/* Write the next row of the auxiliary file */
	write_aux_row(&auxptr, ++auxrow, tev[ievt], srcpos, srcpos_app,
		      imx, imy, imx_app, imy_app, 
		      parms->corrs.pcodefr, parms->corrs.ngoodpix,
		      parms->corrs.maskwtsqf, &status);
	    
      }

      /* ------- */
      /* Assign MASK_WEIGHT value to each individual event, and perform
	 per-event filtering */
      /*   1. Event must be in-bounds */
      if (detx[ievt] >= 0 && detx[ievt] < NX &&
	  dety[ievt] >= 0 && dety[ievt] < NY) {

	/* 2. Minimum partial coding threshold must be satisfied */
	if (parms->corrs.pcodefr < parms->pcodethresh) {
	  mweight[ievt] = 0;
	} else {
	  mweight[ievt] = maskimg[dety[ievt]][detx[ievt]];
	}

	/* 3. Filtering expression must be satisfied (defaults to 1) */
	if (filt[ievt] == 0) { mweight[ievt] = 0; }

	/* Finally, tally some statistics on the mask weight values */
	if (mweight[ievt] > maxwt) maxwt = mweight[ievt];
	if (mweight[ievt] < minwt) minwt = mweight[ievt];
	if (tev[ievt] < tstart)    tstart = tev[ievt];
	if (tev[ievt] > tstop)     tstop  = tev[ievt];

      } else {
	if (numwarns < MAXWARNS) {
	  fprintf(stderr, "WARNING: event %d at position (DETX,DETY) = (%d,%d) is "
		  "out of bounds.\n", off+ievt, detx[ievt], dety[ievt]);
	  numwarns ++;
	  if (numwarns == MAXWARNS) {
	    fprintf(stderr, "         (suppressing further warnings)\n");
	  }
	}
	mweight[ievt] = 0;
      }
    }

    /* Write this chunk of MASK_WEIGHT values to the output events file */
    fits_write_col(infptr,TFLOAT,mtwcol,off,1,nbuf,mweight,&status);
    if (status) {
      fprintf(stderr, "Error while writing weights to events file\n");
      break;
    }

    /* Reset state variables for next call */
    ndone += nbuf;
    nevt += nbuf;
    off += nbuf;
  }

  /* Write out keywords and close file */
  if (status == 0) {
    char keyname[FLEN_CARD];
    char creator[FLEN_CARD];
    char *p;

    sprintf(creator, "%s %s", parms->taskname, parms->taskver);
    fits_update_key(infptr, TSTRING, "BATCREAT", creator,
		    "BAT Program that modified this FITS file", &status);

    mask_writekey(infptr, &mask, &status);
    detplane_writekey(infptr, &detplane, &status);
    src_writekey(infptr, parms, (distmap)?(1):(0), &status);

    /* Write TDMIN/TDMAX keywords */
    fits_make_keyn("TDMIN", mtwcol, keyname, &status);
    fits_update_key(infptr, TFLOAT, keyname, &minwt, 
		    "Minimum column data value", &status);
    fits_make_keyn("TDMAX", mtwcol, keyname, &status);
    fits_update_key(infptr, TFLOAT, keyname, &maxwt, 
		    "Maximum column data value", &status);


    /* Output teldef and aperture file names as keywords */
    /* Remove path components */
    p = rindex(parms->aperture,'/');
    if (p == 0) p = parms->aperture; else p++;
    fits_update_key(infptr, TSTRING, "APERTURE", p, 
		    "BAT aperture file name", &status);

    /* Now do teldef file */
    p = rindex(parms->teldef,'/');
    if (p == 0) p = parms->teldef; else p++;
    fits_update_key(infptr, TSTRING, "BTELDEF", p, 
		    "BAT teldef file name", &status);
    fits_set_hdustruc(infptr, &status);
  }

  /* Save TSTART/TSTOP to auxiliary raytrace file */
  if (auxptr && tstart < 1e307) {
    fits_update_key(auxptr, TDOUBLE, "TSTART", &tstart,
		    "[s] MET Start time of file", &status);
    fits_update_key(auxptr, TDOUBLE, "TSTOP",  &tstop,
		    "[s] MET Start time of file", &status);
  }
  
  /* Make sure we can close the file, even if the status variable is munged */
  mystatus = 0;
  fits_close_file(infptr, &mystatus);
  mystatus = 0;
  if (auxptr) fits_close_file(auxptr, &mystatus);

  /* De-allocate memory */
  if (tev)     free(tev);    tev = 0;
  if (detx)    free(detx);   detx = 0;
  if (dety)    free(dety);   dety = 0;
  if (mweight) free(mweight);mweight = 0;
  if (filt)    free(filt);   filt = 0;

  /* De-allocate attitude file information */  
  if ((parms->coord_type == BCSKY) && teldef && attfile) {
    destroyTelDef(teldef); teldef = 0;
    closeAttFile(attfile); attfile = 0;
  }

  summary(parms, nevt, nrecalcs);
  return status;
}

int batmaskwtevt(void)
{
  int status = 0;

  struct parm_struct parms;
  memset(&(parms.corrs), 0, sizeof(parms.corrs));

  /* Register taskname and version. */

  set_toolname(taskname);
  set_toolversion(taskver);

  if ((status = batmaskwtevt_getpar(&parms)) != 0) {
    fprintf(stderr, "Could not read parameter file\n");
    return status;
  }

  parms.taskname = &taskname[0];
  parms.taskver  = &taskver[0];

  return batmaskwtevt_work(&parms);

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
