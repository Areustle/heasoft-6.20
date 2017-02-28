#include <string.h>
#include <fitsio.h>
#include <pil.h>
#include <math.h>
#include <ctype.h>
#include "headas.h"
#include "headas_utils.h"
#include "imageutils.h"
#include "batcelldetect.h"


/* 
 * init - initialization / shutdown functions for batcelldetect 
 *
 */

/* =================================================================== */
int batcelldetect_getpar(struct parm_struct *parms) 
{
  int status = 0;
  char wtype[PIL_PATH_MAX];
  char psfshapestr[FLEN_CARD];  /* PSF shape string */
  char keepkeywordstr[PIL_PATH_MAX];  /* List of keywords to keep */
  char hduclassesstr[PIL_PATH_MAX]; /* List of HDUCLASn filters */
  char psffwhmstr[FLEN_CARD]; 
  char psftopwidthstr[FLEN_CARD]; 
  char keepbitstr[FLEN_CARD];
  char statstr[FLEN_CARD];
  char ptcorrelstr[FLEN_CARD];
  char vectposstr[FLEN_CARD];

  parms->infile[0] = 0;
  parms->outfile[0] = 0;
  parms->pcodefile[0] = 0;
  parms->pcodethresh = 0.01;
  parms->bkgpcodethresh = 0.01;
  parms->incatalog[0] = 0;
  parms->regionfile[0] = 0;
  parms->bkgwindowtype = CIRCLE;
  parms->bkgwindowrad = 30;
  parms->srcwindowrad = 3;
  parms->npixthresh = 50;
  parms->niter = 2;
  parms->snrthresh = 6.0;
  parms->signifmap[0] = 0;
  parms->bkgmap[0] = 0;
  parms->bkgvarmap[0] = 0;
  parms->outcat = 0;
  parms->pospeaks = 1;
  parms->nadjpix = 4;
  parms->nonullborder = 1;
  parms->newsrcname[0] = 0;
  parms->newsrcind = -1;
  parms->srcdetect = 1;
  parms->srcfit = 1;
  parms->fitpos = 0;
  parms->psfdebugfile[0] = 0;
  parms->inbkgmap[0] = 0;
  parms->inbkgmap_zero = 0;
  parms->inbkgvarmap[0] = 0;
  parms->distfile[0] = 0;

  parms->exposure = 0;
  parms->tstart = 0;
  parms->tstop = 0;
  parms->batz = 0;
  parms->oversampx = 0;
  parms->oversampy = 0;
  parms->nbatdets = 0;
  parms->statistics = STATGAUSS;
  parms->keep_bad_sources = 0;
  parms->pt_correl = 0;
  parms->posfluxfit = 0;

  if ((status = PILGetFname("infile", parms->infile)))
    fprintf(stderr, "Error reading the 'infile' parameter.\n");

  else if ((status = PILGetFname("outfile", parms->outfile)))
    fprintf(stderr, "Error reading the 'outfile' parameter.\n");

  else if ((status = PILGetString("pcodefile", parms->pcodefile)))
    fprintf(stderr, "Error reading the 'pcodefile' parameter.\n");
  
  else if ((status = PILGetReal("pcodethresh", &parms->pcodethresh)))
    fprintf(stderr, "Error reading the 'pcodethresh' parameter.\n");

  else if ((status = PILGetReal("bkgpcodethresh", &parms->bkgpcodethresh)))
    fprintf(stderr, "Error reading the 'bkgpcodethresh' parameter.\n");

  else if ((status = PILGetString("incatalog", parms->incatalog)))
    fprintf(stderr, "Error reading the 'incatalog' parameter.\n");

  else if ((status = PILGetString("regionfile", parms->regionfile)))
    fprintf(stderr, "Error reading the 'regionfile' parameter.\n");

  else if ((status = PILGetReal("snrthresh", &parms->snrthresh)))
    fprintf(stderr, "Error reading the 'snrthresh' parameter.\n");

  else if ((status = PILGetString("bkgwindowtype", wtype)))
    fprintf(stderr, "Error reading the 'bkgwindowtype' parameter.\n");

  else if ((status = PILGetInt("bkgradius", &parms->bkgwindowrad)))
    fprintf(stderr, "Error reading the 'bkgradius' parameter.\n");

  else if ((status = PILGetInt("srcradius", &parms->srcwindowrad)))
    fprintf(stderr, "Error reading the 'srcradius' parameter.\n");

  else if ((status = PILGetInt("npixthresh", &parms->npixthresh)))
    fprintf(stderr, "Error reading the 'npixthresh' parameter.\n");

  else if ((status = PILGetInt("niter", &parms->niter)))
    fprintf(stderr, "Error reading the 'niter' parameter.\n");

  else if ((status = PILGetBool("pospeaks", &parms->pospeaks)))
    fprintf(stderr, "Error reading the 'pospeaks' parameter.\n");

  else if ((status = PILGetBool("vectorflux", &parms->outvector)))
    fprintf(stderr, "Error reading the 'vectorflux' parameter.\n");

  else if ((status = PILGetBool("carryover", &parms->carryover)))
    fprintf(stderr, "Error reading the 'carryover' parameter.\n");

  else if ((status = PILGetString("keepbits", keepbitstr)))
    fprintf(stderr, "Error reading the 'keepbits' parameter.\n");

  else if ((status = PILGetString("rows", parms->rowstr)))
    fprintf(stderr, "Error reading the 'rows' parameter.\n");

  else if ((status = PILGetString("signifmap", parms->signifmap)))
    fprintf(stderr, "Error reading the 'signifmap' parameter.\n");

  else if ((status = PILGetString("bkgmap", parms->bkgmap)))
    fprintf(stderr, "Error reading the 'bkgmap' parameter.\n");

  else if ((status = PILGetString("bkgvarmap", parms->bkgvarmap)))
    fprintf(stderr, "Error reading the 'bkgvarmap' parameter.\n");

  else if ((status = PILGetString("inbkgmap", parms->inbkgmap)))
    fprintf(stderr, "Error reading the 'inbkgmap' parameter.\n");

  else if ((status = PILGetString("inbkgvarmap", parms->inbkgvarmap)))
    fprintf(stderr, "Error reading the 'inbkgvarmap' parameter.\n");

  else if ((status = PILGetInt("nadjpix", &parms->nadjpix)))
    fprintf(stderr, "Error reading the 'nadjpix' parameter.\n");

  else if ((status = PILGetBool("nullborder", &parms->nonullborder)))
    fprintf(stderr, "Error reading the 'nullborder' parameter.\n");

  else if ((status = PILGetString("newsrcname", parms->newsrcname)))
    fprintf(stderr, "Error reading the 'newsrcname' parameter.\n");

  else if ((status = PILGetInt("newsrcind", &parms->newsrcind)))
    fprintf(stderr, "Error reading the 'newsrcind' parameter.\n");

  else if ((status = PILGetBool("srcdetect", &parms->srcdetect)))
    fprintf(stderr, "Error reading the 'srcdetect' parameter.\n");

  else if ((status = PILGetBool("srcfit", &parms->srcfit)))
    fprintf(stderr, "Error reading the 'srcfit' parameter.\n");

  else if ((status = PILGetBool("posfit", &parms->fitpos)))
    fprintf(stderr, "Error reading the 'posfit' parameter.\n");

  else if ((status = PILGetBool("bkgfit", &parms->bkgfit)))
    fprintf(stderr, "Error reading the 'bkgfit' parameter.\n");

  else if ((status = PILGetString("psfshape", psfshapestr)))
    fprintf(stderr, "Error reading the 'psfshape' parameter.\n");

  else if ((status = PILGetString("psffwhm", psffwhmstr)))
    fprintf(stderr, "Error reading the 'psffwhm' parameter.\n");

  else if ((status = PILGetString("psftopwidth", psftopwidthstr)))
    fprintf(stderr, "Error reading the 'psftopwidth' parameter.\n");

  else if ((status = PILGetReal("posfitwindow", &parms->posfitwindow)))
    fprintf(stderr, "Error reading the 'posfitwindow' parameter.\n");

  else if ((status = PILGetReal("possyserr", &parms->possyserr)))
    fprintf(stderr, "Error reading the 'possyserr' parameter.\n");

  else if ((status = PILGetString("distfile", parms->distfile)))
    fprintf(stderr, "Error reading the 'distfile' parameter.\n");

  else if ((status = PILGetString("imagestatistics", statstr)))
    fprintf(stderr, "Error reading the 'imagestatistics' parameter.\n");

  else if ((status = PILGetString("keepkeywords", keepkeywordstr)))
    fprintf(stderr, "Error reading the 'keepkeywordstr' parameter.\n");

  else if ((status = PILGetString("hduclasses", hduclassesstr)))
    fprintf(stderr, "Error reading the 'hduclasses' parameter.\n");

  else if ((status = PILGetString("psfdebugfile", parms->psfdebugfile)))
    fprintf(stderr, "Error reading the 'psfdebugfile' parameter.\n");

  else if ((status = PILGetString("sortcolumns", parms->sortcolumns)))
    fprintf(stderr, "Error reading the 'sortcolumns' parameter.\n");

  else if ((status = PILGetBool("keepbadsources", &(parms->keep_bad_sources))))
    fprintf(stderr, "Error reading the 'keepbadsources' parameter.\n");

  else if ((status = PILGetString("ptcorrel", ptcorrelstr)))
    fprintf(stderr, "Error reading the 'ptcorrel' parameter.\n");

  else if ((status = PILGetReal("psf_chitol", &parms->psf_chitol)))
    fprintf(stderr, "Error reading the 'psf_chitol' parameter.\n");

  else if ((status = PILGetReal("psf_partol", &parms->psf_partol)))
    fprintf(stderr, "Error reading the 'psf_partol' parameter.\n");

  else if ((status = PILGetBool("posfluxfit", &parms->posfluxfit)))
    fprintf(stderr, "Error reading the 'posfluxfit' parameter.\n");

  else if ((status = PILGetString("vectorposmeth", vectposstr)))
    fprintf(stderr, "Error reading the 'vectorposmeth' parameter.\n");

  if (status) return status;

  /* ------ */
  /* NOTE: task parameter is nullborder but internall it's *NO*nullborder */
  /*       so we invert the sense here */
  parms->nonullborder = 1 - parms->nonullborder;

  /* ------ */
  /* Default value of parms->pcodefile */
  if (strcasecmp(parms->pcodefile, "none") == 0) {
    parms->pcodefile[0] = 0;
  }

  /* ------ */
  /* Use the default value of bkgpcodethresh if it's -1, default =
     pcodethresh */
  if (parms->bkgpcodethresh == -1) {
    parms->bkgpcodethresh = parms->pcodethresh;
  }
  /* Limit checking */
  if (parms->bkgpcodethresh < 0) {
    parms->bkgpcodethresh = 0;
  }

  /* ------ */
  /* Parse keepbits */
  parms->keepbits = 0;
  if (strcasecmp(keepbitstr,"ALL") == 0) {
    parms->keepbits = 0;
  } else {
    parms->keepbits = atoi(keepbitstr);
    if (parms->keepbits <= 0 || parms->keepbits > 64) {
      fprintf(stderr, "WARNING: 'keepbits' value of '%s' was not parseable \n",
	      keepbitstr);
      fprintf(stderr, "         into an integer in the range 1-64.  Assuming\n");
      fprintf(stderr, "         keepbits=ALL\n");
      parms->keepbits = 0;
    }
  }

  /* ------ */
  /* Special case of NONE for many file names */
  if (strcasecmp(parms->signifmap, "none") == 0) {
    parms->signifmap[0] = 0;
  }
  if (strcasecmp(parms->bkgmap, "none") == 0) {
    parms->bkgmap[0] = 0;
  }
  if (strcasecmp(parms->bkgvarmap, "none") == 0) {
    parms->bkgvarmap[0] = 0;
  }
  if (strcasecmp(parms->inbkgmap, "none") == 0) {
    parms->inbkgmap[0] = 0;
  }
  if (strcasecmp(parms->inbkgmap, "zero") == 0) {
    parms->inbkgmap[0] = 0;
    parms->inbkgmap_zero = 1;
  }
  if (strcasecmp(parms->inbkgvarmap, "none") == 0) {
    parms->inbkgvarmap[0] = 0;
  }
  if (strcasecmp(parms->incatalog, "none") == 0) {
    parms->incatalog[0] = 0;
  }
  if (strcasecmp(parms->regionfile, "none") == 0) {
    parms->regionfile[0] = 0;
  }
  if (strcasecmp(parms->distfile, "none") == 0) {
    parms->distfile[0] = 0;
  }
  if (strcasecmp(parms->psfdebugfile, "none") == 0) {
    parms->psfdebugfile[0] = 0;
  }
  if (strcasecmp(parms->sortcolumns, "none") == 0) {
    parms->sortcolumns[0] = 0;
  }


  /* ------ */
  /* Parse the image statistics: GAUSSIAN or POISSON */
  if (toupper(statstr[0]) == 'G') {
    parms->statistics = STATGAUSS;
  } else if (toupper(statstr[0]) == 'P') {
    parms->statistics = STATPOISS;
  } else {
    fprintf(stderr,
	    "ERROR: imagestatistics must be either GAUSSIAN or POISSON\n");
    return -1;
  }
  
  /* ------ */
  /* Background window type: square or circle */
  if (strncasecmp(wtype, "square", 6) == 0) {
    parms->bkgwindowtype = SQUARE;
  } else if (strncasecmp(wtype, "circle", 6) == 0) {
    parms->bkgwindowtype = CIRCLE;
  } else if (strncasecmp(wtype, "smooth_circle", 13) == 0) {
    parms->bkgwindowtype = SMOOTH_CIRCLE;
  } else {
    fprintf(stderr, 
	    "ERROR: bkgwindowtype must be one of 'circle' or 'square'\n");
    return -1;
  }

  /* ------ */
  /* Source and background window sizes */
  if (parms->srcwindowrad >= parms->bkgwindowrad) {
    fprintf(stderr, "ERROR: source window radius must be smaller than \n");
    fprintf(stderr, "       background window radius\n");
    return -1;
  }

  /* ------ */
  /* Default value for new source names */
  if (parms->newsrcname[0] == 0) {
    strcpy(parms->newsrcname, "UNKNOWN");
  }

  /* ------ */
  /* PSF shape */
  if (strcasecmp(psfshapestr, "gaussian") == 0) {
    parms->psfshape = GAUSSIAN;
  } else if (strcasecmp(psfshapestr, "pyramid") == 0) {
    parms->psfshape = PYRAMID;
  } else if (strcasecmp(psfshapestr, "truncone") == 0) {
    parms->psfshape = TRUNCONE;
  } else {
    fprintf(stderr, "ERROR: psfshape must be one of GAUSSIAN, PYRAMID or TRUNCONE\n");
    return -1;
  }

  /* ------ */
  /* PSF size */
  parms->fit_fwhm = 0;
  parms->fit_topwidth = 0;
  parms->psffwhm = atof(psffwhmstr) / 57.295780;
  if (parms->psffwhm <= 0) {
    fprintf(stderr, "ERROR: 'psffwhm' parameter must be positive\n");
    return -1;
  }
  if (strstr(psffwhmstr,":FIT")) {
    parms->fit_fwhm = 1;
  }
  parms->psftopwidth = atof(psftopwidthstr) / 57.295780;
  if (parms->psftopwidth <= 0) {
    fprintf(stderr, "ERROR: 'psftopwidth' parameter must be positive\n");
    return -1;
  }
  if (strstr(psftopwidthstr,":FIT")) {
    parms->fit_topwidth = 1;
  }

  /* ------ */
  /* Keywords to keep */
  parms->nkeepkeywords = 0;
  parms->keepkeywords = expand_item_list(keepkeywordstr, 
					 &parms->nkeepkeywords, ',',
					 1, 1, 0, &status);
  if (parms->keepkeywords == 0) {
    fprintf(stderr,"WARNING: could not parse 'keepkeywords' parameter\n");
  }

  /* ------ */
  /* HDUCLASSn keywords to filter on */
  parms->nhduclasses = 0;
  if (strcasecmp(hduclassesstr,"NONE") != 0) {
    parms->hduclasses = expand_item_list(hduclassesstr,
					 &parms->nhduclasses, ',',
					 1, 1, 0, &status);
    if (parms->hduclasses == 0) {
      fprintf(stderr,"WARNING: could not parse 'hduclasses' parameter\n");
    }
  }

  /* ------ */
  /* Constraint checking of carryover & outvector; can't ask for
     vector output, but no carry-over between images. */
  if (parms->carryover == 0 && parms->outvector == 1) {
    fprintf(stderr, "ERROR: vectorflux=YES and carryover=NO are mutually exclusive\n");
    return -1;
  }

  /* ------ */
  /* Point to point correlations */
  if (strcasecmp(ptcorrelstr, "NONE") == 0) {
    parms->pt_correl = 0;
  } else if (strcasecmp(ptcorrelstr, "PSF") == 0) {
    parms->pt_correl = 1;
  } else if (strncasecmp(ptcorrelstr, "GAUSSIAN[", 9) == 0) {
    parms->pt_correl = 2;
    parms->pt_correl_parms[0] = atof(ptcorrelstr+9);
    if (parms->pt_correl_parms[0] <= 0) {
      fprintf(stderr, "ERROR: for GAUSSIAN[x], x must be positive but was not\n");
      return -1;
    }
    parms->pt_correl_parms[0] = tan(parms->pt_correl_parms[0]*0.0174532925199433);
  } else {
    fprintf(stderr, "ERROR: ptcorrel must be either NONE, PSF or GAUSSIAN[x]\n");
    return -1;
  }
  if (parms->pt_correl != 0) {
    fprintf(stderr, 
	    "WARNING: use of the ptcorrel parameter is not fully tested!\n"
	    "         Use it at your own risk!\n");
    
  }
    
  /* ------ */
  /* How to determine positions for stack of images */
  parms->vectpos = POS_LAST;  /* Default to last image */
  if (strcasecmp(vectposstr, "FIRST") == 0) {
    parms->vectpos = POS_FIRST;
  } else if (strcasecmp(vectposstr, "FIXFIRST") == 0) {
    parms->vectpos = POS_FIXFIRST;
  } else if (strcasecmp(vectposstr, "LAST") == 0) {
    parms->vectpos = POS_LAST;
  } else if (strcasecmp(vectposstr, "AVERAGE") == 0) {
    parms->vectpos = POS_AVG;
  } else if (strcasecmp(vectposstr, "MAX_SNR") == 0) {
    parms->vectpos = POS_MAX_SNR;
  } else if (strcasecmp(vectposstr, "INDEF") == 0) {
    parms->vectpos = POS_LAST;
  } else {
    fprintf(stderr, "ERROR: vectposmeth=%s is not recognized\n",
	    vectposstr);
    return -1;
  }

  return status;
}

/* =================================================================== */
void banner(struct parm_struct *parms)
{
  char *psfshapestr = 0;

  headas_chat(2, "******************************************\n");
  headas_chat(1, "#        %s v%s\n", parms->taskname, parms->taskver);
  headas_chat(2, "------------------------------------------\n");
  headas_chat(2, "     Input Image: %s\n", parms->infile);
  if (strcmp(parms->rowstr,"-") != 0)
    headas_chat(2, " Image Selection: %s\n", parms->rowstr);
  headas_chat(2, "  Output Catalog: %s  %s\n", parms->outfile,
	      parms->outvector ? "(vector flux)":"");
  if (parms->incatalog[0])
    headas_chat(2, "   Input Catalog: %s\n", parms->incatalog);
  else
    headas_chat(2, "   Input Catalog: NONE\n");
  if (parms->pcodefile[0])
    headas_chat(2, "Part. Coding Map: %s   (threshold=%f)\n", 
		parms->pcodefile, parms->pcodethresh);
  else
    headas_chat(2, "Part. Coding Map: NONE\n");
  if (parms->bkgwindowtype == CIRCLE) {
    headas_chat(2, "    Back. Window: CIRCLE    Radius: %d\n",
		parms->bkgwindowrad);
  } else if (parms->bkgwindowtype == SMOOTH_CIRCLE) {
    headas_chat(2, "    Back. Window: SMOOTH_CIRCLE    Radius: %d\n",
		parms->bkgwindowrad);
  } else {
    headas_chat(2, "    Back. Window: SQUARE      Side: %d\n",
		parms->bkgwindowrad*2+1);
  }
  headas_chat(2, "   Source Window: CIRCLE    Radius: %d\n",
	      parms->srcwindowrad);
  headas_chat(2, "   SNR Threshold: %f\n", parms->snrthresh);
  headas_chat(2, " Number of Iter.: %d\n", parms->niter);
  headas_chat(2, "Min. Num. Pixels: %d\n", parms->npixthresh);
  headas_chat(2, " Fit background?: %s\n",
	      parms->bkgfit ? "YES" : "NO");
  switch(parms->psfshape) {
  case GAUSSIAN: psfshapestr = "GAUSSIAN"; break;
  case PYRAMID:  psfshapestr = "PYRAMID"; break;
  case TRUNCONE: psfshapestr = "TRUNCONE"; break;
  default: psfshapestr = "UNKNOWN"; break;
  }
  headas_chat(2, "       PSF Shape: %s\n", psfshapestr);

  if (parms->bkgmap[0])
    headas_chat(2, "     Back. Image: %s\n", parms->bkgmap);
  if (parms->bkgvarmap[0])
    headas_chat(2, "Back. Var. Image: %s\n", parms->bkgvarmap);
  if (parms->signifmap[0])
    headas_chat(2, "   Signif. Image: %s\n", parms->signifmap);

  headas_chat(2, "------------------------------------------\n");
}

/* =================================================================== */
void summary(struct parm_struct *parms)
{
  headas_chat(2, "------------------------------------------\n");
}  
