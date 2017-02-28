#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_gti.h"
#include "bat_gswdev.h" /* "bat_read_calfiles" and GAIN_STRUCTURE */
#include "batgse2dph.h"

/* HISTORY
 * ------
 *   Version 1.0 written by Derek Hullinger, UMCP, June 2004
 */

#define TOOLSUB batgse2dph
#include "headas_main.c"

/* -------------------------------------------------------------------*/
/* check_binedges makes sure the energy bin edges are sensible */
/* -------------------------------------------------------------------*/

int check_binedges(spectrum *spec)
{
  int i;
  int status=0;

  headas_chat(5,"Inside check_binedges...\n");

  /* Make sure e_min[i] < e_max[i] */ 

  for (i=0;i<spec->size;i++) {
    if (spec->e_min[i]>=spec->e_max[i]) {
      headas_chat(5,"e_min[%d]: %f    e_max[%d]: %f\n");
      fprintf(stderr,"ERROR: e_min[%d] >= e_max[%d]\n",i,i);
      status=1;
    }
  }
  headas_chat(4,"e_min[i] < e_max[i] for all i\n");

  /* Make sure e_min[i+1] = e_max[i] */ 

  for (i=0;i<spec->size-1;i++) {
    if (spec->e_min[i+1]!=spec->e_max[i]) {
      fprintf(stderr,"ERROR: e_min[%d] != e_max[%d]\n",i+1,i);
      status=1;
    }
  }
  headas_chat(4,"e_min[i+1] == e_max[i] for all i\n");

  headas_chat(5,"...leaving check_binedges\n");
  return status;
}

/* -------------------------------------------------------------------*/
/* get_ebins finds the new bin edges */
/* -------------------------------------------------------------------*/

int get_ebins(batgse2dph_parms *parms, spectrum *newspec)
{
  int status;
  int count;

  headas_chat(5,"Inside get_ebins...\n");

  /* Extract e_min and e_max using Craig's read_ebins function */

  count=read_ebins(parms->binfile,newspec->e_min,newspec->e_max,NGSEBINS);
  newspec->size=count;

  /* check bin edges to make sure they are valid */

  status = check_binedges(newspec);
  if (status) return status;

  headas_chat(5,"...leaving get_ebins\n");
  return status;
}


/* -------------------------------------------------------------------
 * read_filenames reads file names from infile
 *
 * adapted from batgse2dpi
 * -------------------------------------------------------------------*/
char **read_infile (char *infile, int *file_count)
{
  FILE *fp;
  int i,j,status;
  /* char **filenames;  */
  char **filenames;

  headas_chat(5,"Inside read_infile...\n");

  /* allocate memory */

  filenames = (char **) malloc (sizeof(char *)*64);
  for (i=0;i<64;i++)
    filenames[i] = (char *) malloc (sizeof(char)*FLEN_FILENAME);

  /* open infile for reading */

  if ((fp = fopen(infile,"r")) == NULL) {
    headas_chat(5,"Unable to open ascii file %s \n",infile);
    status = 0;
    *file_count = status;
    return filenames;
  }

  i=0;
    
  while (feof(fp) == 0) {
    if (fgets(filenames[i], 255, fp) == 0) break;
    filenames[i][255] = 0;    /* Null terminate */
    for (j=0; j<255; j++)
      if (filenames[i][j] == '\n') filenames[i][j] = 0; /* Remove C/Rs */
    if (strlen(filenames[i]) == 0) continue; /* Skip the blanks */
    for (j=0; filenames[i][j] && isspace(filenames[i][j]); j++);
    if (strlen(filenames[i]) == j) continue;
    headas_chat(5,"  Filename %d: '%s'\n",i,filenames[i]);
    i++;
  }
  headas_chat(5,"Number of files listed is %d \n",i);
  *file_count = i;

  headas_chat(5,"...leaving read_infiles\n");

  return filenames;
}


/* -------------------------------------------------------------------
 * make_gse_energy finds e_cent for a gse spectrum 
 *
 * adapted from bateconvert
 * -------------------------------------------------------------------*/

int make_gse_energy(GAIN_STRUCTURE *gs, int calmode, int detid, 
    spectrum *gsespec)
{
  double energy[NGSEBINS];
  double resid;
  int i;
  int status=0;

  headas_chat(5,"Inside make_gse_energy...\n");
  headas_chat(5,"calmode: %d\n",calmode);

  /* For each gsespec bin */

  for (i=0;i<NGSEBINS;i++) {

    /* Convert the the ADU to energy using the flight code algorithm
     * (energy is in units of 0.1 keV)
     *
     * Basic formula: energy=gain*(offset-adu) */

    /* NOTE: THIS CALCULATION IS OBSOLETE.  USE THE ONE IN 
       bat_pha_to_keV(), batutils/econv.c */
    energy[i]=
      (((gs->flight_gain[detid])     /* gain/8192 in units of keV/ADU */
	*(gs->flight_offset[detid]   /* offset/8 in units of ADU */
	  -(i << 3))) >> 16);        /* units of ADU */
    /*if ((detid == 17538)&&(i==2589)) 
      headas_chat(1,"linear energy for detid %d, bin %d: %f\n",
	  detid, i, energy[i]/10);*/

    /* add in residual values if quadratic conversion is desired */

    if (calmode==QUAD_METH) {   
      resid=
	gs->resid_offset[detid]+
	gs->resid_gain1[detid]*(double)(i) +
	gs->resid_gain2[detid]*(double)(i)*(double)(i);

      energy[i] += resid;
      /*if ((detid == 17538)&&(i==2589)) {
        headas_chat(1,"quadratic energy for detid %d, bin %d: %f\n",
	    detid, i, energy[i]/10);
        headas_chat(1,"resid offset for detid %d: %f\n",
	    detid, gs->resid_offset[detid]);
        headas_chat(1,"resid gain1 for detid %d: %f\n",
	    detid, gs->resid_gain1[detid]);
        headas_chat(1,"resid gain2 for detid %d: %f\n",
	    detid, gs->resid_gain2[detid]);
        headas_chat(1,"resid for detid %d: %f\n",
	    detid, resid);
      }*/

    } else if (calmode==CUBIC_METH) {

      resid=
	gs->resid_offset[detid]+
	gs->resid_gain1[detid]*(double)(i) +
	gs->resid_gain2[detid]*(double)(i)*(double)(i)+
	gs->resid_cubic[detid]*(double)(i)*(double)(i)*(double)(i);

      energy[i] += resid;

    } 

    /* place the energy in e_cent (in float format) */

    gsespec->e_cent[i]=(float)(energy[i])/10.;
    /*headas_chat(4,"energy for bin %d: %f\n",i,gsespec->e_cent[i]);*/
  }
  headas_chat(5,"...leaving make_gse_energies\n");

  return status;
}

 
/* -------------------------------------------------------------------*/
/* batgse2dph_getpar gets the parameters from the command line */
/* -------------------------------------------------------------------*/
int batgse2dph_getpar(batgse2dph_parms *parms)
{
  int status;

  headas_chat(5,"Inside batgse2dph_getpar...\n");

  if ((status = PILGetFname("infile", parms->infile)))
    fprintf(stderr, "Error reading the 'infile' parameter.\n");

  else if ((status = PILGetFname("outfile", parms->outfile)))
    fprintf(stderr, "Error reading the 'outfile' parameter.\n");

  else if ((status = PILGetFname("calfile", parms->calfile)))
    fprintf(stderr, "Error reading the 'calfile' parameter.\n");

  else if ((status = PILGetFname("residfile", parms->residfile)))
    fprintf(stderr, "Error reading the 'residfile' parameter.\n");

  else if ((status = PILGetFname("pulserfile", parms->pulserfile)))
    fprintf(stderr, "Error reading the 'pulserfile' parameter.\n");

  else if ((status = PILGetFname("binfile", parms->binfile)))
    fprintf(stderr, "Error reading the 'binfile' parameter.\n");

  else if ((status = PILGetString("calmode", parms->calmode)))
    fprintf(stderr, "Error reading the 'calmode' parameter.\n");

  else if ((status = PILGetBool("rebin",&parms->rebin)))
    fprintf(stderr, "Error reading the 'rebin' parameter.\n");

  else if ((status = PILGetReal("deadpercount",&parms->deadpercount)))
    fprintf(stderr, "Error reading the 'deadpercount' parameter.\n");

  else if ((status = PILGetBool("deadapp",&parms->deadapp)))
    fprintf(stderr, "Error reading the 'deadapp' parameter.\n");
  
  if (status) return (status);

  if ((strcasecmp(parms->calmode,"QUADRATIC")==0)&&
     ((strcasecmp(parms->residfile,"NONE")==0)||
      (strcasecmp(parms->pulserfile,"NONE")==0))) {
    fprintf(stderr,
	"residfile and pulserfile must be specified if calmode=QUADRATIC\n");
    return status = -1;
  }

  if ((strcasecmp(parms->calmode,"CUBIC")==0)&&
     ((strcasecmp(parms->residfile,"NONE")==0)||
      (strcasecmp(parms->pulserfile,"NONE")==0))) {
    fprintf(stderr,
	"residfile and pulserfile must be specified if calmode=CUBIC\n");
    return status = -1;
  }

  /* if any of the appropriate values is CALDB */

  if ((strncasecmp(parms->calfile,"CALDB",5)==0)||
      (strncasecmp(parms->residfile,"CALDB",5)==0)||
      (strncasecmp(parms->pulserfile,"CALDB",5)==0)||
      (strncasecmp(parms->binfile,"CALDB",5)==0)) {

    struct caldbparms_struct caldb;
    int nebins = 0;
    char expr[PIL_PATH_MAX];
    char codenam[PIL_PATH_MAX];
    int maxret = 1;
    char *pfile = (char *)malloc(sizeof(char)*PIL_PATH_MAX);
    long int extno[1];
    char online[PIL_PATH_MAX], *ponline = online;
    int nret = 0, nfound = 0;

    /* Initialize the caldb sturcture */

    batkw_to_caldb_parms(0,&caldb,1,&status);
    if (status) exit(status);

    /* query CALDB: calfile */

    if ((strncasecmp(parms->calfile,"CALDB",5)==0)) {

      headas_chat(3,"Using CALDB to locate calfile\n");

      strcpy(codenam,"GSE_GAIN");
      strcpy(expr,"-");

      bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX,
 	  &pfile, extno, &ponline, &nret, &nfound, &status);
      if ((status != 0) || (nret == 0) || (nfound == 0)) return status;

      strcpy(parms->calfile,pfile);

    } 

    /* query CALDB: residfile */

    if ((strncasecmp(parms->residfile,"CALDB",5)==0)) {

      headas_chat(3,"Using CALDB to locate residfile\n");

      strcpy(codenam,"DET_GAIN");
      strcpy(expr,"-");

      bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX,
 	  &pfile, extno, &ponline, &nret, &nfound, &status);
      if ((status != 0) || (nret == 0) || (nfound == 0)) return status;

      strcpy(parms->residfile,pfile);
    }

    /* query CALDB: pulserfile */

    if ((strncasecmp(parms->pulserfile,"CALDB",5)==0)) {

      headas_chat(3,"Using CALDB to locate pulserfile\n");

      strcpy(codenam,"PULSER_GAIN");
      strcpy(expr,"SOURCE.EQ.\"GROUND\"");

      bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX,
 	  &pfile, extno, &ponline, &nret, &nfound, &status);
      if ((status != 0) || (nret == 0) || (nfound == 0)) return status;

      strcpy(parms->pulserfile,pfile);
    }

    /* query CALDB: binfile */

    if ((strncasecmp(parms->binfile,"CALDB",5)==0)) {

      headas_chat(3,"Using CALDB to locate binfile\n");

      strcpy(codenam,"EBOUNDS");

      /* Attempt to read number of bins from CALDB expression,
       * otherwise default to 80 bins */

      status = sscanf(parms->binfile+5, ":%d", &nebins);
      if (status <=0) {
        nebins=80;
        headas_chat(5,"number of ebins not found in CALDB expression\n");
      }
      headas_chat(4,"number of ebins: %d",nebins);
      status=0;

      /* Create expression for number of energy bins */

      sprintf(expr, "MODE.eq.%d", nebins);                      

      /* Query CALDB */

      bat_caldb_search(&caldb, codenam, expr, maxret, PIL_PATH_MAX,
	  &pfile, extno, &ponline, &nret, &nfound, &status);
      if ((status != 0) || (nret == 0) || (nfound == 0)) return status;

      strcpy(parms->binfile,pfile);
    }
    free(pfile);
  }

  headas_chat(4,"infile: %s\n",parms->infile);
  headas_chat(4,"outfile: %s\n",parms->outfile);
  headas_chat(4,"calfile: %s\n",parms->calfile);
  headas_chat(4,"residfile: %s\n",parms->residfile);
  headas_chat(4,"pulserfile: %s\n",parms->pulserfile);
  headas_chat(4,"binfile: %s\n",parms->binfile);
  headas_chat(4,"calmode: %s\n",parms->calmode);
  headas_chat(4,"rebin: %d\n",parms->rebin);
  headas_chat(4,"deadpercount: %d\n",parms->deadpercount);
  headas_chat(4,"deadapp: %d\n",parms->deadapp);

  headas_chat(5,"...leaving batgse2dph_getpar\n");

  return status;
}


/* -------------------------------------------------------------------
 * write_dph writes the new dph FITS file, with all of its extensions.
 *
 * the FITS file is created before this function is called
 *
 * Some of the BAT_DPH and LIVETIME columns are initialized to zero
 * and are filled outside of this function.
 * -------------------------------------------------------------------*/
int write_dph(fitsfile *infits, fitsfile *outfits, spectrum *newspec, 
    double exposure, char *gainmeth, char *creator)
{
  int i, status=0;
  int nkeys=0;
  long primary_naxes=0;
  char key_val[FLEN_VALUE];
  char card[FLEN_CARD];
  int istrue=1;
  float zero=0.0;
  double zero_array[1]={0.0};
  double exp_array[1];
  int block, dm, side;
/*float *dph_array;*/

  /* dph extension */

  char *dph_ttypes[] = {"TIME", "TIME_STOP", "EXPOSURE", "DPH_COUNTS",
    "TOTCOUNTS"};
  char *dph_tforms[] = {"D", "D", "D", "E", "J"};
  char *dph_tunits[] = {"s", "s", "s", "counts", "counts"};
  long int dph_tdim[3]={0,DAP_COLS,DAP_ROWS};

  /* ebounds extension */

  char *eb_ttypes[] = {"CHANNEL", "E_MIN", "E_MAX"};
  char *eb_tforms[] = {"I", "E", "E"};
  char *eb_tunits[] = {"", "keV", "keV"};

  /* gti extension */

  char *gti_ttypes[] = {"START", "STOP"};
  char *gti_tforms[] = {"D", "D"};
  char *gti_tunits[] = {"s", "s"};

  /* livetime extension */

  char *lt_ttypes[] = {"BLOCK", "DM", "SIDE", "EXPOSURE", "LIVE_TIME",
    "DEAD_TIME", "TOTCOUNTS"};
  char *lt_tforms[] = {"I", "I", "I", "E", "E", "E", "J"};
  char *lt_tunits[] = {"", "", "", "s", "s", "s", "count"};

  headas_chat(5,"Inside write_dph...\n");

  /* prepare some variables */

  exp_array[0] = exposure;
  
  /* Write the primary HDU */

  headas_chat(5,"Writing Primary HDU...\n");
  fits_create_img(outfits,8,0,&primary_naxes,&status);
  fits_update_key(outfits,TSTRING,"EXTNAME","Primary","Primary HDU",
      &status);
  headas_chat(5,"Primary HDU written\n");

  /* Create the BAT_DPH extension */

  headas_chat(5,"Writing BAT_DPH HDU...\n");
  sprintf(key_val,"%dD",DAP_COLS*DAP_ROWS*newspec->size);
  dph_tforms[3] = key_val;
  if (fits_create_tbl(outfits, BINARY_TBL, 1, 5, dph_ttypes, dph_tforms,
        dph_tunits,"BAT_DPH",&status)) {
    fits_report_error(stderr,status);
    exit(status);
  }

  /* Write the BAT_DPH keywords */

  /* get the number of keywords in the infits header */

  fits_get_hdrspace(infits, &nkeys, NULL, &status);
  headas_chat(4,"number of cards in infile: %d\n",nkeys);

  /* copy all of the keywords that aren't structural or comments */

  headas_chat(4,"Copying cards from infile...\n");
  for (i=1;i<=nkeys;i++) {
    fits_read_record(infits, i, card, &status);
    if ((fits_get_keyclass(card) >= TYP_REFSYS_KEY) &&
	(strncmp(card, "COMMENT", 7) != 0)) {
      fits_write_record(outfits, card, &status);
      headas_chat(5,"%s\n",card);
    }
  }

  status = write_keywords(infits, outfits, exposure, gainmeth, creator);
  if (status) {
    fprintf(stderr,"ERROR: write_keywords failed\n");
    fits_report_error(stderr,status);
    exit(status);
  }

  dph_tdim[0]=newspec->size;
  fits_write_tdim(outfits,4,3,dph_tdim,&status);
  fits_update_key(outfits,TSTRING,"HDUNAME","BAT_DPH",
      "Detector Plane Histogram",&status);
  fits_update_key(outfits,TSTRING,"HDUCLAS1","ARRAY",
      "Contains array data",&status);
  fits_update_key(outfits,TSTRING,"HDUCLAS2","TOTAL",
      "Histogram is unweighted",&status);
  fits_update_key(outfits,TLOGICAL,"BACKAPP",&istrue,
      "Was background correction applied?",&status); 
  fits_update_key(outfits,TFLOAT,"TNULL",&zero,
      "Missing data is given this value",&status);

  /* Include HISTORY keywords */

  if (HDpar_stamp(outfits,0,&status)) {
    fits_report_error(stderr,status);
    exit(status);
  }

  /* Write the BAT_DPH columns (all null values for now) */

/*fits_write_col(outfits,TDOUBLE,1,1,1,1,zero_array,&status);*/
  fits_write_col(outfits,TDOUBLE,2,1,1,1,exp_array,&status);
  fits_write_col(outfits,TDOUBLE,3,1,1,1,exp_array,&status);
/*dph_array = (float *)malloc(sizeof(float)*DAP_ROWS*DAP_COLS*newspec->size);
  for (i=0;i<DAP_ROWS*DAP_COLS*newspec->size;i++) dph_array[i]=0;
  fits_write_col(outfits,TFLOAT,4,1,1,1,dph_array,&status);
  free(dph_array);
  fits_write_col_null(outfits,4,1,1,(DAP_ROWS*DAP_COLS*newspec->size),&status);
  */
/*fits_write_col_null(outfits,5,1,1,1,&status);*/

  headas_chat(5,"...finished writing BAT_DPH HDU\n");

  /* Create the EBOUNDS extension */

  headas_chat(5,"Writing EBOUNDS HDU\n");

  if (fits_create_tbl(outfits,BINARY_TBL,newspec->size,3,eb_ttypes,
      eb_tforms,eb_tunits,"EBOUNDS",&status)) {
    fits_report_error(stderr,status);
    exit(status);
  }
  
  /* Write the EBOUNDS keywords */

  status = write_keywords(infits, outfits, exposure, gainmeth, creator);
  if (status) {
    fprintf(stderr,"ERROR: write_keywords failed\n");
    fits_report_error(stderr,status);
    exit(status);
  }

  fits_update_key(outfits,TSTRING,"HDUNAME","EBOUNDS",
      "Energy Bin Edges",&status);
  fits_update_key(outfits,TSTRING,"HDUCLAS1","RESPONSE",
      "Contains spectra",&status);
  fits_update_key(outfits,TSTRING,"HDUCLAS2","EBOUNDS",
      "Spectra are gain corrected",&status);
  fits_update_key(outfits, TSTRING, "RMFVERSN", "1992a",      
      "Version of EBOUNDS format (OBSOLETE)", &status);
  fits_update_key(outfits, TSTRING, "HDUVERS", "1.2.0",       
      "Version of EBOUNDS header", &status);      
  fits_update_key(outfits, TSTRING, "HDUVERS1", "1.0.0",      
      "Version of EBOUNDS header", &status);      
  fits_update_key(outfits, TSTRING, "HDUVERS2", "1.1.0",      
      "Version of EBOUNDS header", &status);      
  fits_update_key(outfits,TINT,"DETCHANS",&(newspec->size),
      "Total number of detector channels available", &status);

  if (HDpar_stamp(outfits,0,&status)) {
    fits_report_error(stderr,status);
    exit(status);
  }

  /* Write the EBOUNDS columns */

  for (i=0; i<newspec->size; i++) {
    fits_write_col(outfits, TINT, 1, i+1, 1, 1, &i, &status);
    fits_write_col(outfits, TFLOAT, 2, i+1, 1, 1, &(newspec->e_min[i]), 
	&status);
    fits_write_col(outfits, TFLOAT, 3, i+1, 1, 1, &(newspec->e_max[i]), 
	&status);
  }

  headas_chat(5,"...finished writing EBOUNDS HDU\n");

  /* Create the STDGTI extension */

  headas_chat(5,"Writing GTI HDU\n");

  if (fits_create_tbl(outfits,BINARY_TBL,1,2,gti_ttypes,
      gti_tforms,gti_tunits,"STDGTI",&status)) {
    fits_report_error(stderr,status);
    exit(status);
  }

  /* Write the STDGTI keywords */

  status = write_keywords(infits, outfits, exposure, gainmeth, creator);
  if (status) {
    fprintf(stderr,"ERROR: write_keywords failed\n");
    fits_report_error(stderr,status);
    exit(status);
  }

  fits_update_key(outfits,TSTRING,"HDUNAME","STDGTI",
      "Standard Good Time Interval",&status);
  fits_update_key(outfits,TSTRING,"HDUCLAS1","TEMPORALDATA",
      "Extension contains time ordered HK",&status);
  fits_update_key(outfits,TSTRING,"HDUCLAS2","GTI",
      "Extension contains HK data",&status);

  if (HDpar_stamp(outfits,0,&status)) {
    fits_report_error(stderr,status);
    exit(status);
  }

  /* Write the STDGTI columns */

  fits_write_col(outfits,TDOUBLE,1,1,1,1,zero_array,&status);
  fits_write_col(outfits,TDOUBLE,2,1,1,1,exp_array,&status);

  headas_chat(5,"...finished writing GTI HDU\n");

  /* Create the LIVETIME extension */

  headas_chat(5,"Writing LIVETIME HDU\n");

  if (fits_create_tbl(outfits,BINARY_TBL,256,7,lt_ttypes,
      lt_tforms,lt_tunits,"LIVETIME",&status)) {
    fits_report_error(stderr,status);
    exit(status);
  }

  /* Write the LIVETIME columns (no additional keywords are written) */

  for (i=0; i<256; i++) {
    block = (int)(i/16);
    dm = ((int)(i/2) % 8);
    side = (i % 2);
    fits_write_col(outfits, TINT, 1, i+1, 1, 1, &block, &status);
    fits_write_col(outfits, TINT, 2, i+1, 1, 1, &dm, &status);
    fits_write_col(outfits, TINT, 3, i+1, 1, 1, &side, &status);
  }
  fits_write_col_null(outfits,4,1,1,256,&status);
  fits_write_col_null(outfits,5,1,1,256,&status);
  fits_write_col_null(outfits,6,1,1,256,&status);
  /* fits_write_col_null(outfits,7,1,1,256,&status); */
  
  headas_chat(5,"...finished writing LIVETIME HDU\n");

  /* That'll do it */

  headas_chat(5,"...leaving write_dph\n");

  return status;

}


/* -------------------------------------------------------------------*/
/* write_keywords writes keywords that are common among extensions    */
/* -------------------------------------------------------------------*/
int write_keywords (fitsfile *infits, fitsfile *outfits, double exposure,
    char *gainmeth, char *creator)
{
  int status=0;
  int istrue=1;
  int isfalse=0;
  float mjdref=51910.; /* I'm not sure why */
  float floatone=1.0;
  double dblzero=0.0;
  double dblone=1.0;

  headas_chat(5,"Inside write_keywords...\n");
  headas_chat(5,"Values passed to write_keywords:\n");
  headas_chat(5,"  exposure: %g\n",exposure);
  headas_chat(5,"  gainmeth: %s\n",gainmeth);
  headas_chat(5,"  creator:  %s\n",creator);

  fits_update_key(outfits,TSTRING,"ORIGIN","GSFC",
        "Source of FITS file", &status);
  fits_update_key(outfits,TSTRING,"CREATOR",creator,
        "Program that created this FITS file", &status);
  fits_update_key(outfits,TSTRING,"HDUCLASS","OGIP",
      "Conforms to OGIP/GSFC standards",&status);
  fits_update_key(outfits,TSTRING,"TELESCOP","SWIFT",
      "Telescope (mission) name",&status);
  fits_update_key(outfits,TSTRING,"INSTRUME","BAT",
      "Instrument name",&status);
  fits_update_key(outfits,TSTRING,"FILTER","",
      "BAT has no filters",&status);
  fits_update_key(outfits,TSTRING,"OBS_MODE","POINTING",
      "POINTING, SCAN, SLEW, RASTER",&status);
  fits_update_key(outfits,TSTRING,"DATAMODE","GSE",
      "Instrument data mode",&status);
  fits_update_key(outfits,TSTRING,"TIMESYS","TT",
      "Time System",&status);
  fits_update_key(outfits,TFLOAT,"MJDREF",&mjdref,
      "MJD 01 Jan 2001 00:00:00",&status);
  fits_update_key(outfits,TDOUBLE,"TIMEZERO",&dblzero,
      "Zero-point offset for TIME column",&status);
  fits_update_key(outfits,TSTRING,"TIMEUNIT","s",
      "Time unit is seconds",&status);
  fits_update_key(outfits,TLOGICAL,"CLOCKAPP",&isfalse,
      "Clock corrections not applied",&status);
  fits_update_key(outfits,TDOUBLE,"TSTART",&dblzero,
      "Obs start time (taken to be zero)",&status);
  fits_update_key(outfits,TDOUBLE,"TSTOP",&exposure,
      "Obs end time (taken to be ONTIME)",&status);
  fits_update_key(outfits,TSTRING,"TIMEREF","LOCAL",
      "Time reference (barycenter/local)",&status);
  fits_update_key(outfits,TSTRING,"TASSIGN","GSE",
      "Time assigned by clock",&status);
  fits_update_key(outfits,TDOUBLE,"TIMEPIXR",&dblzero,
      "Time bin alignment",&status);
  fits_update_key(outfits,TDOUBLE,"TIMEDEL",&dblone,
      "Time resolution of data (in seconds)",&status);
  fits_update_key(outfits,TDOUBLE,"TELAPSE",&exposure,
      "TSTOP - TSTART",&status);
  fits_update_key(outfits,TDOUBLE,"ONTIME",&exposure,
      "Sum of GTIs",&status);
  fits_update_key(outfits,TDOUBLE,"LIVETIME",&exposure,
      "Ontime multiplied by DEADC",&status);
  fits_update_key(outfits,TDOUBLE,"EXPOSURE",&exposure,
      "Total exposure, with all known corrections",&status);
  fits_update_key(outfits,TDOUBLE,"DEADC",&floatone,
      "Dead Time",&status);
  fits_write_comment(outfits,
      "Dead Time correction was done sandwich by sandwich",&status);
  fits_write_comment(outfits,
      "and applied directly to the DPH counts",&status);
  fits_update_key(outfits,TLOGICAL,"GAINAPP",&istrue,
      "Gain correction has been applied",&status);
  fits_update_key(outfits,TSTRING,"GAINMETH",gainmeth,
      "Gain correction method",&status);
  fits_write_date(outfits,&status);

  /* Keywords that may be required but are not yet implemented:
   * General on origin and checking:
   *   PROCVER, SEQPNUM, CHECKSUM, DATASUM, TLM2FITS,
   * Observation ID:
   *   OBS_ID, TARG_ID, SEG_NUM, EXP_ID
   * General on instrument:
   *   OBJECT, OPTICm 
   * General on coordinates:
   *   RA_NOM, DEC_NOM, ROLL_NOM, RA_OBJ, DEC_OBJ, RA_SCX, DEC_SCX,
   *   RA_SCY, DEC_SCY, RA_SCZ, DE_SCZ, EQUINOX, RADECSYS
   * Timing:
   *   DATE-OBS, DATE-END (these are probably part of GSE header already
   *     and are therefore copied to the BAT_DPH header)
   *   TIERRELA, TIERABSO, TIMEDEL */

  headas_chat(5,"...leaving write_keywords\n");
  return status;
}


/* -------------------------------------------------------------------*/
/* batgse2dph is the main workhorse of the program */
/* -------------------------------------------------------------------*/
int batgse2dph (void)
{
  batgse2dph_parms parms;
  int status, i, j, m, file_count, n_hdus, hdu_type;
  int blockid, dmid, sideid, colnum, det_ct;
  int firstfile = 1;
  int calmode = 0;
  double exposure, livetime, deadtime;
  long int side_total;
  long int totcounts=0;
  long int totcounts_array[1];
  long int dph_index;
  GAIN_STRUCTURE gs;
  spectrum newspec;
  spectrum gsespec;
  char **filenames;
  char key_name[FLEN_VALUE];
  char creator[FLEN_VALUE];
  fitsfile *infits;
  fitsfile *outfits;
  short int block, dm, det, row, col;
  unsigned short int detid;

  static char taskname[80] = "batgse2dph";
  static char version[8] = "2.0";

  sprintf(creator,"%s %s",taskname,version);
 
  headas_chat(4,"version: %s\n",version);

  /* Register taskname and version */

  set_toolname(taskname);
  set_toolversion(version);

  /* initialize gsespec.size */

  gsespec.size=4096;

  /* Get the parameters from the command line */

  status = batgse2dph_getpar(&parms);
  if (status) {
    fprintf(stderr,"ERROR: batgse2dph_getpar failed\n");
    return status;
  }

  if (strcasecmp(parms.calmode,"QUADRATIC") == 0) calmode=QUAD_METH;
  if (strcasecmp(parms.calmode,"CUBIC") == 0) calmode=CUBIC_METH;

  /* create the gain structure */

  /* NOTE: THIS CALL IS NOW OBSOLETE.  It will need to be modified to
     use bat_read_cal_coeff().  Also, since the output of that routine
     is the calibrationd data in "geographic" format, a little extra
     care will need to be taken to look up by detid. */
  status = bat_read_calfiles(parms.calfile,parms.residfile,0,
      parms.pulserfile,0,parms.pulserfile,0,
      &calmode,&gs);
  if (status) {
    fprintf(stderr,"ERROR: bat_read_calfiles failed\n");
    return status;
  }

  /* get the new bin edges and check them */

  status = get_ebins(&parms, &newspec);
  if (status) {
    fprintf(stderr,"ERROR: get_ebins failed\n");
    return status;
  }

  /* read in gse FITS file names */

  filenames = read_infile(parms.infile,&file_count);
  if (file_count < 1) {
    fprintf(stderr,"no file names found in %s\n",parms.infile);
    status = 1;
    return status;
  }

  /* protect against those silly files that list the text file itself at
   * the very end */

  if (file_count==17) file_count=16;
  
  /* delete existing outfile if outfile already exists */

  headas_clobberfile(parms.outfile);

  /* create the new outfile FITS file */

  if (fits_create_file(&outfits, parms.outfile, &status)) {
    fits_report_error(stderr,status);
    exit(status);
  }

  /* for each gse FITS file... */

  for (i=0; i<file_count; i++) {

    headas_chat(1,"Reading %s ...\n",filenames[i]);

    /* open the gse FITS file */

    status = 0;
    if (fits_open_file(&infits, filenames[i], READONLY, &status)) {
      fits_report_error(stderr,status);
      exit(status);
    }
    headas_chat(5,"Opened file %s\n",filenames[i]);

    /* get number of HDUs */

    if (fits_get_num_hdus(infits, &n_hdus, &status)) {
      fits_report_error(stderr,status);
      fits_close_file(infits,&status);
      exit(status);
    }
    if (n_hdus <= 1) {
      fprintf(stderr,"WARNING: %s has no extensions\n",filenames[i]);
      fits_close_file(infits,&status);
      exit(status);
    }

    headas_chat(5,"Found %d extensions in %s\n",n_hdus,filenames[i]);

    /* For each HDU... */

    for (m=1; m<= n_hdus; m++) {

      /* move to the HDU */

      headas_chat(5, "Moving to HDU %d \n",m);

      if (fits_movabs_hdu(infits, m, &hdu_type, &status)) {
	fits_report_error(stderr,status);
	status=0;
	continue; /* Go to next extension */
      }

      /* Get the extension name */

      if ((fits_read_key(infits,TSTRING,"EXTNAME",key_name,0,&status))&&
	 (m>1)) {
	fprintf(stderr,"WARNING: HDU %d has no EXTNAME keyword\n",m);
	status=0;
	continue; /* Go to next extension */
      }
      status=0;
      headas_chat(5,"EXTNAME = %s\n", key_name);

      /* Make sure it is a SPECTRUM extenstion */

      if (strncmp(key_name,"SPECTRUM",8) != 0) {
	continue; /* Go to next extension */
      }

      /* Get the exposure */

      exposure = 0.0;
      fits_read_key(infits,TDOUBLE,"EXPOSURE",&exposure,NULL,&status);
      headas_chat(4,"EXPOSURE = %g\n",exposure);

      /* If this is the first gse file,
       * Create the outfits FITS file */

      if (firstfile) {

	headas_chat(5,"This is the first SPECTRUM HDU found\n");
	firstfile = 0;     /* reset firstfile so this is only done once */
	status=write_dph(infits, outfits, &newspec, exposure, parms.calmode,
	    creator);
	if (status) {
	  fprintf(stderr,"ERROR: write_dph failed\n");
	  return status;
	}
      }

      /* Get BLOCK_ID, DM_ID, SIDE_ID */

      fits_read_key(infits,TINT,"BLOCK_ID",&blockid,0,&status);
      fits_read_key(infits,TINT,"DM_ID",&dmid,0,&status);
      fits_read_key(infits,TINT,"SIDE_ID",&sideid,0,&status);
      headas_chat(4,"Block ID: %d\n", blockid);
      headas_chat(4,"DM ID: %d\n", dmid);
      headas_chat(4,"Side ID: %d\n", sideid);

      /* Get column number of "COUNTS" column */

      if (fits_get_colnum(infits,CASEINSEN,"counts",&colnum,&status)) {
	fits_report_error(stderr,status);
        fits_close_file(infits,&status);
	exit(status);
      }
      headas_chat(5,"Column of COUNTS: %d\n",colnum);

      /* Find the total number of counts in the sandwich */

      side_total=0;
      for (det_ct = 0; det_ct < 128; det_ct++)
      {
	fits_read_col(infits,TFLOAT,colnum,(det_ct+1),1,NGSEBINS,NULL,
	    &(gsespec.hist[0]),NULL,&status);
	for (j=0;j<NGSEBINS;j++) side_total += gsespec.hist[j];
      }
      totcounts += side_total;

      headas_chat(4,"Total Side counts: %d\n",side_total);

      /* calculate the live time */

      deadtime = (parms.deadpercount)*side_total*1e-6;
      livetime = exposure - deadtime;
      headas_chat(4,"dead time: %f\n",deadtime);
      headas_chat(4,"live time: %f\n",livetime);

      /* move to the BAT_DPH extension */

      fits_movnam_hdu(outfits,BINARY_TBL,"BAT_DPH",0,&status);

      /* For each detector in this HDU... */

      for (det_ct = 0; det_ct < 128; det_ct++)
      {

	/* Find the detector ID (0 to 32767) */

	detid=det_ct+128*sideid+256*dmid+2048*blockid;
	headas_chat(4,"detid: %d\n",detid);

	/* read in the spectrum */

	fits_read_col(infits,TFLOAT,colnum,(det_ct+1),1,NGSEBINS,NULL,
	    &(gsespec.hist[0]),NULL,&status);

	/* find the energy scale for the spectrum */

	status=make_gse_energy(&gs,calmode,detid,&gsespec);
	if (status) {
	  fprintf(stderr,"ERROR: make_gse_energy failed");
          fits_close_file(infits,&status);
	  return status;
	}

	/* rebin the spectrum */

	status=bin_spectrum(&gsespec, &newspec);
	if (status) {
	  fprintf(stderr,"ERROR: bin_spectrum failed");
          fits_close_file(infits,&status);
	  return status;
	}

	/* correct the spectrum for dead time */

	if ((parms.deadapp)&&(exposure>0)) {
	  headas_chat(4,"correcting for dead time\n");
	  headas_chat(4,"  dead time correction factor: %f\n",
	      (exposure/livetime));
	  for (j=0;j<newspec.size;j++) 
	    newspec.hist[j] *= (exposure/livetime);
	}

	/* find row, column, and dph index of the detector */

	batidconvert(1,&detid,&block,&dm,&det,&row,&col);
	dph_index=(row*DAP_COLS+col)*newspec.size+1;
	headas_chat(4,"dph index: %d\n",dph_index);

	/* write the new spectrum to the dph */

	fits_write_col(outfits,TFLOAT,4,1,dph_index,newspec.size,
	    &(newspec.hist),&status);
	
      } /* go on to the next detector */

      /* move to the LIVETIME extension */

      headas_chat(5,"writing values to LIVETIME extension\n");
      fits_movnam_hdu(outfits,BINARY_TBL,"LIVETIME",0,&status);

      /* write exposure, live time, dead time and total counts */

      fits_write_col(outfits,TDOUBLE,4,(blockid*16+dmid*2+sideid+1),1,1,
	  &exposure,&status);
      fits_write_col(outfits,TDOUBLE,5,(blockid*16+dmid*2+sideid+1),1,1,
	  &livetime,&status);
      fits_write_col(outfits,TDOUBLE,6,(blockid*16+dmid*2+sideid+1),1,1,
	  &deadtime,&status);
      fits_write_col(outfits,TLONG,7,(blockid*16+dmid*2+sideid+1),1,1,
	  &side_total,&status);

    } /* go on to the next HDU */
	
    fits_close_file(infits,&status);

  } /* go on to the next GSE FITS file */

  /* free memory of filenames */

  for (i=0;i<64;i++)
    free(filenames[i]);
  free(filenames);

  /* write total counts */

  totcounts_array[0]=totcounts;
  fits_movnam_hdu(outfits,BINARY_TBL,"BAT_DPH",0,&status);
  fits_write_col(outfits,TLONG,5,1,1,1,totcounts_array,&status);

  /* close the dph file */

  fits_close_file(outfits,&status);
  return status;

}
