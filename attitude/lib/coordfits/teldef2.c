#include "teldef2.h"

#include "fitsio.h"
#include "longnam.h"

#include <math.h>
#include <string.h>

/* #define DEBUG 1 */

/* Create a new TELDEF2 structure and read its contents from a file. */
int readTelDef2 /* Returns CFITSIO status */
(
  const char* filename, /* TelDef filename */
  TELDEF2** p_teldef    /* Pointer to TelDef2 structure pointer */
  )
{
  int length = 0;
  fitsfile *fp = NULL;
  int hdutype = 0;
  int status = 0;
  long sys = 0; 
  char keyname[FLEN_KEYWORD];
  char tempstring[1000];
  int seg = 0;
  TELDEF2* teldef = NULL;

  /* Allocate space for the teldef structure and initialize it. */

  *p_teldef = (TELDEF2*) malloc(sizeof(TELDEF2));
  teldef = *p_teldef;

  strcpy(teldef->filename, "");
  strcpy(teldef->mission, "");
  strcpy(teldef->instrument, "");
  teldef->td_version = 0.;
  teldef->n_coordsys = 0;
  teldef->coordsysnames = NULL;
  teldef->coordsys = NULL;
  teldef->n_segments = NULL;
  teldef->min_segment = NULL;
  teldef->trtypenames = NULL;
  teldef->optcoord = NULL;
  teldef->optaxisx = 0.;
  teldef->optaxisy = 0.;
  teldef->randsys = -1;
  strcpy(teldef->randsysname, "");
  teldef->randscalesys = -1;
  strcpy(teldef->randscalesysname, "");
  teldef->px_scale_factor = NULL;
  teldef->basicparam = NULL;
  teldef->rawtodetparam = NULL;
  teldef->multisegparam = NULL;
  teldef->skyattparam = NULL;
  teldef->telescopeparam = NULL;

  /* Set the filename in the structure. */

  length = strlen(filename) + 1;
  strncpy(teldef->filename, filename, length);

  /* Open the teldef file and move to the primary extension. */
  
  fits_open_file(&fp, filename, READONLY, &status);
  checkFITSerrors(status, "opening", teldef->filename); 
  if(status)
    return status;

  fits_movabs_hdu(fp, 1, &hdutype, &status);
  checkFITSerrors(status, "moving to primary extension of", teldef->filename); 
  if(status)
    return status;


  /* Read the mission from the TELESCOP keyword. */

  fits_read_key_str(fp, "TELESCOP", teldef->mission, NULL, &status);
  checkFITSerrors(status, "reading TELESCOP from", teldef->filename);
  if(status)
    return status;


  /* Read the instrument from the INSTRUME keyword. */

  fits_read_key_str(fp, "INSTRUME", teldef->instrument, NULL, &status);
  checkFITSerrors(status, "reading INSTRUME from", teldef->filename);
  if(status)
    return status;


  /* Read the Teldef file format version number from TD_VERS keyword. 
   *  If the keyword is not found, then the version is 0.1. */
  /* Possibly this could be replaced by a read-teldef-version function. */

  fits_read_key_dbl(fp, "TD_VERS", &teldef->td_version, NULL, &status);
  if(status == KEY_NO_EXIST) /* Keyword not found */
  {
    teldef->td_version = 0.1;
    status = 0;
#ifdef DEBUG
    fprintf(stdout, "TelDef keyword TD_VERS was not found.  Assuming the TelDef format version is 0.1.\n");
#endif
  }
  checkFITSerrors(status, "reading TD_VERS from", teldef->filename);
  if(status)
    return status;

  /* Read the number of coordinate systems from the NCOORDS keyword. */

  fits_read_key_lng(fp, "NCOORDS", &teldef->n_coordsys, NULL, &status);
  checkFITSerrors(status, "reading NCOORDS from", teldef->filename);
  if(teldef->n_coordsys <= 1)
  {
    fprintf(stderr, "NCOORDS keyword value is %ld, but valid values are at least 2 to allow a coordinate transformation.\n", teldef->n_coordsys);
    return status;
  }
  
  /* Allocate several arrays. 
   * - coordsys, rawtodetparam, multisegparam, skyattparam are only
   *   partially allocated. More info is needed to finish allocating them. 
   * - basicparam is completely allocated.  It is needed for all transformations. 
   */

  teldef->coordsysnames = malloc(teldef->n_coordsys * sizeof(char*));
  teldef->trtypenames = malloc((teldef->n_coordsys - 1) * sizeof(char*));
  teldef->trtypes = malloc((teldef->n_coordsys - 1) * sizeof(TransformationTypeEnum));
  teldef->coordsys = malloc(teldef->n_coordsys * sizeof(COORDDEF**));
  teldef->n_segments = malloc(teldef->n_coordsys * sizeof(int));
  teldef->min_segment = malloc(teldef->n_coordsys * sizeof(int));
  teldef->px_scale_factor = malloc(teldef->n_coordsys * sizeof(double));
  teldef->basicparam = malloc((teldef->n_coordsys - 1) * sizeof(TR_BASIC*));
  teldef->rawtodetparam = malloc((teldef->n_coordsys - 1) * sizeof(TR_RAWTODET*));
  teldef->multisegparam = malloc((teldef->n_coordsys - 1) * sizeof(TR_MULTISEG*));
  teldef->skyattparam = malloc((teldef->n_coordsys - 1) * sizeof(TR_SKYATT*));

  for(sys = 0; sys < teldef->n_coordsys; sys++)
  {
    teldef->coordsysnames[sys] = malloc(FLEN_VALUE * sizeof(char));

    if(sys < teldef->n_coordsys - 1)
    {
      teldef->trtypenames[sys] = malloc(FLEN_VALUE * sizeof(char));
    }

  }


  /* Read the names of the coordinate systems from the COORDn keywords. */

  for(sys = 0; sys < teldef->n_coordsys; sys++)
  {
    sprintf(keyname, "COORD%ld", sys);
    fits_read_key_str(fp, keyname, tempstring, NULL, &status);
    strcpy(teldef->coordsysnames[sys], tempstring);

    sprintf(tempstring, "reading %s from", keyname);
    checkFITSerrors(status, tempstring, teldef->filename);
    if(status)
      return status;
  }


  /* Read the names of the transformation types from the TRTYPEn keywords. 
   * These are required if TD_VERS >= 0.1. These are determined if TD_VERS < 0.1. */

  status = readTransformationTypesFromTelDef2(fp, teldef);
  if(status)
    return status;

  /* Allocate and fill the transformation parameter structures. */

  status = setTransformationParametersFromTelDef2(fp, teldef);
  if(status)
    return status;

  /* Determine the number of segments in each coordinate system. */

  status = setSegmentCountInTelDef2(fp, teldef);
  if(status)
    return status;

  /* Set pixel scale factors for potential use in randomization. */
  
  for(sys = 0; sys < teldef->n_coordsys; sys++)
  {
    if(sys == 0)
      teldef->px_scale_factor[sys] = 1.0;
    else if(sys == teldef->n_coordsys - 1)
      teldef->px_scale_factor[sys] = teldef->px_scale_factor[sys - 1];
    else
      teldef->px_scale_factor[sys] = teldef->px_scale_factor[sys - 1] * teldef->basicparam[sys - 1]->scale;
  }  

  /* Allocate COORDDEF structures for each segment of each coordinate system.  */

  for(sys = 0; sys < teldef->n_coordsys; sys++)
  {
    teldef->coordsys[sys] = (COORDDEF**) malloc((teldef->n_segments[sys] + teldef->min_segment[sys])* sizeof(COORDDEF*));
				     
    for(seg = teldef->min_segment[sys]; 
	seg < teldef->min_segment[sys] + teldef->n_segments[sys]; seg++)
    {
      teldef->coordsys[sys][seg] = allocateCoordDef();

      /* Initialize the transformation with some dummy values for now. */
	  
      teldef->coordsys[sys][seg]->trans->rot[0][0] = 1;
      teldef->coordsys[sys][seg]->trans->rot[0][1] = 0;
      teldef->coordsys[sys][seg]->trans->rot[1][0] = 0;
      teldef->coordsys[sys][seg]->trans->rot[1][1] = 1;
      teldef->coordsys[sys][seg]->trans->xshift = 0.;
      teldef->coordsys[sys][seg]->trans->yshift = 0.;
      teldef->coordsys[sys][seg]->min_x = 0.;
      teldef->coordsys[sys][seg]->max_x = 1.;
      teldef->coordsys[sys][seg]->min_y = 0.;
      teldef->coordsys[sys][seg]->max_y = 1.;
      setCoordDefTransformInfo(teldef->coordsys[sys][seg]);

      /* Set the coord. sys. name and other coordinate properties. */

      teldef->coordsys[sys][seg]->name = teldef->coordsysnames[sys];
      status = setCoordinatesFromKeywordsInTeldef2(teldef, teldef->coordsys[sys][seg], fp, sys, 
						   seg == teldef->min_segment[sys]);
      if(status)
	return status;
    }
  }


  /* For RAWTODET transformations, fix the internal coordinate system
   * center if the relevant keywords were not found. */

  for(sys = 0; sys < teldef->n_coordsys - 1; sys++)
  {
    if(teldef->trtypes[sys] == e_TT_RAWTODET)
    {
      /* Fix internal coordinate system center if the relevant keywords were not found. */
	  
      if(!teldef->rawtodetparam[sys]->int_cen_found)
      {
	teldef->rawtodetparam[sys]->int_cen_x = teldef->coordsys[sys+1][teldef->min_segment[sys+1]]->center_x;
	teldef->rawtodetparam[sys]->int_cen_y = teldef->coordsys[sys+1][teldef->min_segment[sys+1]]->center_y;
	teldef->rawtodetparam[sys]->int_cen_found = 1;
      }
    }
  }

  /* Fill in coordinate system keyword values and set 2D transformations. */

  for(sys = 0; sys < teldef->n_coordsys - 2; sys++)
  {
    for(seg = teldef->min_segment[sys]; 
	seg < teldef->min_segment[sys] + teldef->n_segments[sys]; seg++)
    {
      setXform2dFromTransformationParameters(teldef, teldef->coordsys[sys][seg], 
					     teldef->coordsys[sys + 1][teldef->min_segment[sys + 1]], sys, seg);
      setCoordDefTransformInfo(teldef->coordsys[sys][seg]);
    }
  }
  
  /* Determine if nonlinear corrections for any RAWTODET
   * transformation are needed by looking for a TelDef NONLINEAR
   * extension. */
  
  for(sys = 0; sys < teldef->n_coordsys - 1; sys++)
  {
    if(teldef->trtypes[sys] == e_TT_RAWTODET)
    {
      readNonLinearCorrectionsInTeldef2(fp, teldef->rawtodetparam[sys],
					teldef->coordsys[sys][teldef->min_segment[sys]], 
                                        teldef->filename);
    }
  }

  /* Read intrapixel event location randomization parameters. */

  status = readRandomizationFromTeldef2(fp, teldef);
  if(status)
    return status;

  /* Read optical information for telescope. */

  status = readTrTelescope(fp, &teldef->telescopeparam, teldef->filename); 
  if(status)
    return status;

  teldef->telescopeparam->low_sys = 
    getCoordSystemNumberFromName(teldef, teldef->telescopeparam->lowcoordsysname);

  /* Clean up. */

  fits_close_file(fp, &status);
  checkFITSerrors(status, "closing", teldef->filename);
  if(status)
    return status;

  return 0;

}

/* Print a whole TELDEF2 structure to a stream. */

void printTelDef2
(
  TELDEF2* teldef, /* TelDef structure */
  FILE* stream /* stream */
  )
{
  long sys, seg;

  /* Print the structure element by element and substructure by substructure. */

  fprintf(stream, "\nTelDef2 structure contents:\n");
  fprintf(stream, "  filename: %s\n", teldef->filename);
  fprintf(stream, "  mission: %s\n", teldef->mission);
  fprintf(stream, "  instrument: %s\n", teldef->instrument);
  fprintf(stream, "  td_version: %f\n", teldef->td_version);
  fprintf(stream, "  n_coordsys: %ld\n", teldef->n_coordsys);
  fprintf(stream, "  sky_sys: %ld\n", teldef->sky_sys);
  fprintf(stream, "  randsysname: %s\n", teldef->randsysname);
  fprintf(stream, "  randsys: %d\n", teldef->randsys);
  fprintf(stream, "  randscalesysname: %s\n", teldef->randscalesysname);
  fprintf(stream, "  randscalesys: %d\n", teldef->randscalesys);

  for(sys = 0; sys < teldef->n_coordsys; sys++)
  {
    fprintf(stream, "  coordsysnames[sys=%ld]: %s\n", sys, teldef->coordsysnames[sys]);
  }

  for(sys = 0; sys < teldef->n_coordsys - 1; sys++)
  {
    fprintf(stream, "  trtypenames[sys=%ld]: %s\n", sys, teldef->trtypenames[sys]);
  }

  for(sys = 0; sys < teldef->n_coordsys - 1; sys++)
  {
    fprintf(stream, "  trtypes[sys=%ld]: %d\n", sys, teldef->trtypes[sys]);
  }

  for(sys = 0; sys < teldef->n_coordsys; sys++)
  {
    fprintf(stream, "  n_segments[sys=%ld]: %d\n", sys, teldef->n_segments[sys]);
  }

  for(sys = 0; sys < teldef->n_coordsys; sys++)
  {
    fprintf(stream, "  min_segment[sys=%ld]: %d\n", sys, teldef->min_segment[sys]);
  }

  for(sys = 0; sys < teldef->n_coordsys; sys++)
  {
    fprintf(stream, "  px_scale_factor[sys=%ld]: %f\n", sys, teldef->px_scale_factor[sys]);
  }

  for(sys = 0; sys < teldef->n_coordsys; sys++)
  {
    if(sys < teldef->n_coordsys - 2)
    {
      fprintf(stream, "  basicparam[sys=%ld]:\n", sys);
      printTrBasic(teldef->basicparam[sys], stream);
    }

    if(sys < teldef->n_coordsys - 1 && teldef->multisegparam[sys] != NULL)
    {
      fprintf(stream, "  multisegparam[sys=%ld]:\n", sys);
      printTrMultiseg(teldef->multisegparam[sys], stream);
    }

    if(sys < teldef->n_coordsys - 1 && teldef->skyattparam[sys] != NULL)
    {
      fprintf(stream, "  skyattparam[sys=%ld]:\n", sys);
      printTrSkyAtt(teldef->skyattparam[sys], stream);
    }

    if(sys < teldef->n_coordsys - 1 && teldef->rawtodetparam[sys] != NULL)
    {
      fprintf(stream, "  rawtodetparam[sys=%ld]:\n", sys);
      printTrRawtodet(teldef->rawtodetparam[sys], stream);
    }

    for(seg = teldef->min_segment[sys]; seg < teldef->min_segment[sys] + teldef->n_segments[sys]; seg++)
    {
      fprintf(stream, "  coordsys[sys=%ld][seg=%ld]:\n", sys, seg);
      printCoordDef(teldef->coordsys[sys][seg], stream);
    }
  }
  
  printTrTelescope(teldef->telescopeparam, stream);

}


/* Free all memory associated with a TELDEF2 structure. */
void destroyTelDef2
(
  TELDEF2* teldef /* TELDEF2 structure to destroy */
  )
{
  int sys = 0;
  int seg = 0;

  /* If teldef is a null pointer, the structure cannot be freed. */

  if(teldef == NULL)
    return;

#ifdef DEBUG
  printf("Freeing teldef\n");
#endif

  /* Free the COORDDEF structures. */

  for(sys = 0; sys < teldef->n_coordsys; sys++)
  {
    for(seg = teldef->min_segment[sys]; 
	seg < teldef->min_segment[sys] + teldef->n_segments[sys]; seg++)
    {
      destroyCoordDef(teldef->coordsys[sys][seg]);
    }

    free(teldef->coordsys[sys]);
  }

  /* Free the transformation type structures. */

  for(sys = 0; sys < teldef->n_coordsys; sys++)
  {
    free(teldef->coordsysnames[sys]);

    if(sys < teldef->n_coordsys - 1)
    {
      free(teldef->trtypenames[sys]);
	  
      /* These destroy functions free memory only when the pointers are 
       * non-null. Several of the pointers will be null, but this is not 
       * a problem. */

      destroyTrBasic(teldef->basicparam[sys]);
      destroyTrSkyAtt(teldef->skyattparam[sys]);
      destroyTrMultiseg(teldef->multisegparam[sys]);
      destroyTrRawtodet(teldef->rawtodetparam[sys]);
    }
  }
  destroyTrTelescope(teldef->telescopeparam);

  /* Free the arrays and then the whole structure. */

  free(teldef->basicparam);
  free(teldef->rawtodetparam);
  free(teldef->skyattparam);
  free(teldef->multisegparam);
  free(teldef->coordsysnames);
  free(teldef->trtypenames);
  free(teldef->trtypes);
  free(teldef->coordsys);
  free(teldef->n_segments);
  free(teldef->min_segment);
  free(teldef->px_scale_factor);

  free(teldef);

#ifdef DEBUG
  printf("Freed teldef\n");
#endif
}

TransformationTypeEnum getTransformationTypeNumberFromName(char* name)
{
  TransformationTypeEnum number = e_TT_UNKNOWN;

  if(!strcasecmp(name, "BASIC"))
    number = e_TT_BASIC;
  else if(!strcasecmp(name, "RAWTODET"))
    number = e_TT_RAWTODET;
  else if(!strcasecmp(name, "MULTISEG"))
    number = e_TT_MULTISEG;
  else if(!strcasecmp(name, "SKYATT"))
    number = e_TT_SKYATT;

  return number;
}


/* Read the transformation types into the teldef structure from the
   TRTYPE keywords in the header of the TelDef file. */

int readTransformationTypesFromTelDef2 /* Returns CFITSIO status */
(
  fitsfile* fp, /* TelDef file pointer */
  TELDEF2* teldef /* TelDef structure */
  )
{
  int sys = 0;
  char keyname[FLEN_KEYWORD] = "";
  char tempstring[1000] = "";
  int status = 0;
  
  for(sys = 0; sys < teldef->n_coordsys - 1; sys++)
  {
    sprintf(keyname, "TRTYPE%d", sys);
    fits_read_key_str(fp, keyname, tempstring, NULL, &status);
      
    /* If the TelDef version is earlier than 0.2, the TRTYPEn 
     * keywords are absent and must be inferred. */
      
    if(teldef->td_version < 0.2 && status == KEY_NO_EXIST)
    {
      status = 0;
      if(sys == 0)
      {
	strcpy(tempstring, "RAWTODET");
      }
      else if(sys == teldef->n_coordsys - 2)
      {
	strcpy(tempstring, "SKYATT");
      }
      else
      {
	strcpy(tempstring, "BASIC");
      }

      /* Suzaku XIS0 - XIS3 are exceptions without the TRTYPE keywords. */
	  
      if(!strcasecmp(teldef->mission, "SUZAKU") && !strncmp(teldef->instrument,"XIS", 3))
      {
	if(sys == 0)
	  strcpy(teldef->trtypenames[sys], "BASIC");
	else if(sys == 1)
	  strcpy(teldef->trtypenames[sys], "RAWTODET");
      }
    }

    strcpy(teldef->trtypenames[sys], tempstring);

    sprintf(tempstring, "reading %s from", keyname);
    checkFITSerrors(status, tempstring, teldef->filename);
    if(status)
      return status;

    /* Check if the transformation type is a recognized one. Explain
       and quit if not. */

    teldef->trtypes[sys] = getTransformationTypeNumberFromName(teldef->trtypenames[sys]);
      
    if(teldef->trtypes[sys] == e_TT_UNKNOWN)
    {
      sprintf(tempstring, "Transformation type %s = '%s' does not match a known type.\nValid transformation types are: BASIC RAWTODET MULTISEG SKYATT.\n", keyname, teldef->trtypenames[sys]);
      return 1;
    }
  }

  /* Set the sky coordinate system index to the final coordinate
   * system if it is the destination system of a transformation of
   * type SKYATT. Otherwise set it to NO_SKY_SYS. */

  if(teldef->trtypes[teldef->n_coordsys - 2] == e_TT_SKYATT)
    teldef->sky_sys = teldef->n_coordsys - 1;
  else
    teldef->sky_sys = NO_SKY_SYS;
  
  return 0;
}

/* Read the transformation parameters of all the transformations from
   the TelDef file and store them in the teldef structure. */

int setTransformationParametersFromTelDef2 /* Returns CFITSIO status */
(
  fitsfile* fp, /* TelDef file pointer */
  TELDEF2* teldef /* teldef structure */
  )
{
  int sys = 0;
  int status = 0;
    
  /* Loop over each transformation. */

  for(sys = 0; sys < teldef->n_coordsys - 1; sys++)
  {

    /* Branch by transformation type and allocate a BASIC structure
       and a structure for the transformation type if it's not BASIC. */

    if(sys < teldef->n_coordsys - 2)
    {
      status = readTrBasic(fp, &(teldef->basicparam[sys]), 
			   teldef->trtypes[sys] == e_TT_RAWTODET,
			   teldef->coordsysnames[sys], 
			   teldef->coordsysnames[sys + 1], 
			   sys, teldef->filename);
      if(status)
	return status;

    }
    else
    {
      teldef->basicparam[sys] = NULL;
    }
	
    if(teldef->trtypes[sys] == e_TT_SKYATT)
    {
      status = readTrSkyAtt(fp, &(teldef->skyattparam[sys]), 
			    teldef->coordsysnames[sys], 
			    teldef->coordsysnames[sys + 1], 
			    sys, teldef->filename);
      if(status)
	return status;
    }
    else
    {
      teldef->skyattparam[sys] = NULL;
    }
	
    if(teldef->trtypes[sys] == e_TT_MULTISEG)
    {
      status = readTrMultiseg(fp, &(teldef->multisegparam[sys]), 
			      teldef->coordsysnames[sys], 
			      teldef->coordsysnames[sys + 1], 
			      sys, teldef->filename);
      if(status)
	return status;
    }
    else
    {
      teldef->multisegparam[sys] = NULL;
    }
	
    if(teldef->trtypes[sys] == e_TT_RAWTODET)
    {
      status = readTrRawtodet(fp, &(teldef->rawtodetparam[sys]), 
			      teldef->coordsysnames[sys], 
			      teldef->coordsysnames[sys + 1], 
			      sys, teldef->filename);
      if(status)
	return status;
    }
    else
    {
      teldef->rawtodetparam[sys] = NULL;
    }

  }
    
  return 0;
}

/* Determine the number of segments in each coordinate system and
   store them in teldef->n_segments. */

int setSegmentCountInTelDef2 /* Returns CFITSIO status */
(
  fitsfile* fp, /* TelDef file pointer */
  TELDEF2* teldef /* teldef structure */
  )
{
  int sys = 0;
  long n_segs = 0;
  char keyname[FLEN_KEYWORD] = "";
  char tempstring[1000] = "";
  int status = 0;

  /* Get the number of segments in each coord sys.  The number of
   * segments is 1 for systems undergoing BASIC and SKYATT
   * transformation types. 
   *
   * The number of segments for systems undergoing MULTISEG
   * transformations is determined by the number of coefficients found
   * in the MULTISEGn_COEFF table.  
   *
   * The number of segments for systems
   * undergoing a RAWTODET transformation is already given by the
   * number of coefficient keywords in a coefficients RAWTODET
   * transformation, or by the number of rows in a PIXEL_MAP table of
   * a corners RAWTODET transformation.  
   *
   * The following is a check
   * against existing keywords specifically giving the number of
   * keywords. */

  /* !!!This check might not be needed!!! */

  for(sys = 0; sys < teldef->n_coordsys; sys++)
  {
    teldef->n_segments[sys] = 1;
    teldef->min_segment[sys] = 0;

    if(sys < teldef->n_coordsys - 1 && teldef->trtypes[sys] == e_TT_RAWTODET)
    {
      sprintf(keyname, "%s_NSEG", teldef->coordsysnames[sys]);
      fits_read_key_lng(fp, keyname, &n_segs, NULL, &status);
      if(status == KEY_NO_EXIST && teldef->td_version < 0.2)
      {
	status = 0;

	strcpy(keyname, "SEG_NUM");
	fits_read_key_lng(fp, keyname, &n_segs, NULL, &status);
	if(status == KEY_NO_EXIST)
	{
	  status = 0;
	  n_segs = 1;
#ifdef DEBUG
	  fprintf(stdout, "Neither TelDef keyword %s_NSEG nor SEG_NUM was found. Assuming the number of segments for coordinate system %s is %d.\n", teldef->coordsysnames[sys], teldef->coordsysnames[sys], 1);
#endif
	}
	else
	{
	  sprintf(tempstring, "reading %s from", keyname);
	  checkFITSerrors(status, tempstring, teldef->filename);
	}
      }
      else
      {
	sprintf(tempstring, "reading %s from", keyname);
	checkFITSerrors(status, tempstring, teldef->filename);
      }

      /* At this point, n_seg is the value of the keyword
       * explicitly giving the number of segments or is 1 if no
       * such keyword was found.  The number of segments from the
       * other methods of getting this value is given in the
       * multisegparam->n_rows to rawtodetparam->n_segs
       * variables.  These should match. Swift XRT seems to be an exception. */

      teldef->n_segments[sys] = teldef->rawtodetparam[sys]->n_segs;
	  
	  
      if(teldef->td_version >= 0.2 && n_segs != teldef->n_segments[sys])
      {
	fprintf(stderr, "Mismatch in the number of segments in %s system between keyword %s = %ld and\nnumber of segments (%d) deduced from transformation parameters read from the\nTelDef file %s.\nThe TelDef file likely has an error.\n", teldef->coordsysnames[sys], keyname, n_segs, teldef->n_segments[sys], teldef->filename);
	return 1;
      }
	  
      /* Set minimum segment number for RAWTODET systems. */
	  
      teldef->min_segment[sys] = teldef->rawtodetparam[sys]->min_seg;
    }

    if(sys < teldef->n_coordsys - 1 &&
       teldef->trtypes[sys] == e_TT_MULTISEG)
    {
      teldef->n_segments[sys] = teldef->multisegparam[sys]->n_rows;
    }
  }

  return 0;
}

/* Read the coordinate system properties from various TelDef keywords
   and store them in the coord structure. */

int setCoordinatesFromKeywordsInTeldef2 /* Returns CFITSIO status */
(
  TELDEF2* teldef, /* teldef structure */ 
  COORDDEF* coord, /* coord structure */
  fitsfile* fp,  /* TelDef file pointer */
  long sys, /* system number of lower-level system */
  int is_lowest_seg /* 1 if coord represents the lowest-indexed segment for system sys; 0 otherwise */
  )
{
  int status=0;
  
  char key[FLEN_KEYWORD] = "";
  char* key_end = NULL;
  
  double first_pixel_x = 0.;
  double first_pixel_y = 0.;
  long npixels_x = 0;
  long npixels_y = 0;
  
  char units[FLEN_VALUE] = "";
  char colname[FLEN_VALUE] = "";
  char tempstring[1000] = "";

  /* Copy the root name into the beginning of the keyword string. */

  strncpy(key,coord->name,FLEN_KEYWORD);
  key_end=key+strlen(coord->name);
  
  
  /* Read the address space limits keywords. */

  strcpy(key_end,"XPIX1");
  fits_read_key_dbl(fp,key,&first_pixel_x,NULL,&status);
  
  strcpy(key_end,"YPIX1");
  fits_read_key_dbl(fp,key,&first_pixel_y,NULL,&status);
  
  strcpy(key_end,"_XSIZ");
  fits_read_key_lng(fp,key,&npixels_x,NULL,&status);
  
  strcpy(key_end,"_YSIZ");
  fits_read_key_lng(fp,key,&npixels_y,NULL,&status);
  
  if(status==KEY_NO_EXIST && !strcasecmp(coord->name,"RAW") &&   
     !strcasecmp(teldef->mission,"ASCA") ) {

    /* We have some special defaults because the original ASCA 
     * TelDef files omitted these keywords for the raw coordinates. */
    
    status=0;
    
    if(!strcasecmp(teldef->instrument,"SIS0") ||
       !strcasecmp(teldef->instrument,"SIS1")) {
      /***********
       * ASCA SIS *
       ***********/
      first_pixel_x=6.;
      first_pixel_y=2.;
      
      npixels_x=419;
      npixels_y=416;
      
    } else if(!strcasecmp(teldef->instrument,"GIS2") ||
              !strcasecmp(teldef->instrument,"GIS3")) {
      /***********
       * ASCA GIS *
       ***********/
    } else {
      /**********
       * unknown *
       **********/
      fprintf(stderr,"Unknown ASCA instrument %s\n",teldef->instrument);
      return 1;
    }
    
  } /*end of ASCA defaults */
  
  if(status==KEY_NO_EXIST && sys == teldef->n_coordsys - 1 ) {

    /* Sky coordinate dimensions default to the originating
     * detector coordinate dimensions. */

    status=0;
    
    first_pixel_x = teldef->coordsys[sys - 1][teldef->min_segment[sys - 1]]->first_pixel_x;
    first_pixel_y = teldef->coordsys[sys - 1][teldef->min_segment[sys - 1]]->first_pixel_y;
    npixels_x     = teldef->coordsys[sys - 1][teldef->min_segment[sys - 1]]->npixels_x;
    npixels_y     = teldef->coordsys[sys - 1][teldef->min_segment[sys - 1]]->npixels_y;
  }

  sprintf(tempstring, "reading %sXPIX1, %sYPIX1, %s_XSIZ, and %s_YSIZ keywords from", coord->name, coord->name, coord->name, coord->name);
  checkFITSerrors(status, tempstring, teldef->filename);
  if(status)
    return status;
  
  
  
  /* Set the values in the COORDDEF structure. */
  
  setCoordDefLimits(coord,first_pixel_x,(int)npixels_x,
		    first_pixel_y,(int)npixels_y  );
  
  /* Determine the scale values. */
  
  if(sys == teldef->n_coordsys - 1) 
  {
      
    /* SKY coordinates have their scale determined from the focal length
     * and the scale of the originating detector coordinates.
     * Note that the physical pixel size of the sky coordinates must
     * be the same as the size of the originating detector coordinates.
     * Note that the FOCALLEN value must be in the same units as
     * the scale of the originating detector coordinates. */ 
      
    double from_radians;
    COORDDEF* det;
      
      
    /* Determine preferred physical units. */
      
    strcpy(key_end,"_UNIT");
    fits_read_key_str(fp,key,units,NULL,&status);
      
    if(status==KEY_NO_EXIST) 
    {
      /* Sky units default to arc minutes. */
	  
      status=0;
      strcpy(units,"arcmin");
#ifdef DEBUG
      if(is_lowest_seg)
	fprintf(stdout, "TelDef keyword %s was not found. Using default sky units: %s.\n", key, units);
#endif
    }
    if(status)
      return status;
      
    setStringInCoordDef(&(coord->scale_unit),units);
      
    /* Interpret the name of the units and set the conversion factor. */
      
    if(     !strcasecmp(units,"arcmin")     ) from_radians=180.*60./M_PI;
    else if(!strcasecmp(units,"arc minutes")) from_radians=180.*60./M_PI;
    else if(!strcasecmp(units,"arcsec")     ) from_radians=180.*3600./M_PI;
    else if(!strcasecmp(units,"arc seconds")) from_radians=180.*3600./M_PI;
    else if(!strcasecmp(units,"deg")        ) from_radians=180./M_PI;
    else if(!strcasecmp(units,"degrees")    ) from_radians=180./M_PI;
    else if(!strcasecmp(units,"rad"    )    ) from_radians=1.;
    else if(!strcasecmp(units,"radians")    ) from_radians=1.;
    else 
    {
      fprintf(stderr,"SKY units '%s' from TelDef keyword %s are not supported.\n", units, key);
      return 1;
    }
      
    teldef->skyattparam[sys - 1]->deg_per_sky_unit = 180./(M_PI * from_radians);

    /* The X and Y scales of the originating detector coordinates
     * must be the same. */
      
    det = teldef->coordsys[teldef->n_coordsys - 2][teldef->min_segment[teldef->n_coordsys - 2]];
    if(det->scale_x != det->scale_y) 
    {
      fprintf(stderr,"Sky pixels are not square since:\n");
      fprintf(stderr,"%s scale=%g %s/pixel and %s scale=%g %s/pixel\n",
	      det->name_x, det->scale_x, det->scale_unit,
	      det->name_y, det->scale_y, det->scale_unit );
      return 1;
    }
      
      
    /* Set the scale. */
      
    coord->scale_x = det->scale_x * from_radians / teldef->skyattparam[sys - 1]->focal_length;
    coord->scale_y = det->scale_y * from_radians / teldef->skyattparam[sys - 1]->focal_length;
      
      
    /* To-sky transformations require the pixel scale in radians.
     * Note that the X and Y scales are required to be the same
     * so we arbitrarily pick the X scale here. */
      
    teldef->skyattparam[sys - 1]->sky_pix_per_radian = teldef->skyattparam[sys - 1]->focal_length / det->scale_x;
      
  } 
  else 
  {
      
    /* For non-sky coordinates, we try to read the scale keywords
     * though in the usual case the raw coordinates default to being
     * unscaled. */
      
    strcpy(key_end,"_XSCL");
    fits_read_key_dbl(fp,key,&(coord->scale_x),NULL,&status);
      
    strcpy(key_end,"_YSCL");
    fits_read_key_dbl(fp,key,&(coord->scale_y),NULL,&status);
      
    if(status==KEY_NO_EXIST && !strcasecmp(coord->name,"RAW") ) 
    {
	  
      /* By default raw pixels are unscaled. */
	  
      status=0;
	  
      coord->scale_x=1.;
      coord->scale_y=1.;
	  
      setStringInCoordDef(&(coord->scale_unit),"pixels"  );
    }
    else
    {
	  
      /* This is not the RAW scale default case, so
       * determine the scale unit. */
	  
      strcpy(key_end,"_UNIT");
      fits_read_key_str(fp,key,units,NULL,&status);
	  
      if(status==KEY_NO_EXIST) 
      {
	      
	/* Reset status and set default value to millimeters. */
	status=0;
	strcpy(units,"mm");
#ifdef DEBUG
	if(is_lowest_seg)
	  fprintf(stdout, "TelDef keyword %s was not found. Using default units: %s.\n", key, units);
#endif
      }
	  
      setStringInCoordDef(&(coord->scale_unit),units);
	  
    } /* end if not raw default scale */
      
  } /* end if setting raw or detector scales */
  
  
  /* Determine column names. */
  
  /* X column name */
  
  strcpy(key_end,"_XCOL");
  fits_read_key_str(fp,key,colname,NULL,&status);
  if(status==KEY_NO_EXIST) 
  {

    /* Use the default name. */

    status=0;
    if(sys == teldef->n_coordsys - 1) 
    {
      /* The default name for sky x coordinates is just "X". */
	  
      strcpy(colname,"X");
    } 
    else 
    {
      /* For everything else, the default is "nameX". */

      strcpy(colname,coord->name);
      strcat(colname,"X");
    }
      
  } 

  setStringInCoordDef(&(coord->name_x),colname);

  /* Y column name */

  strcpy(key_end,"_YCOL");
  fits_read_key_str(fp,key,colname,NULL,&status);
  if(status==KEY_NO_EXIST) {

    /* Use the default namen */
    
    status=0;
    if(sys == teldef->n_coordsys - 1) 
    {
      /* The default name for sky coordinates is just "Y". */
	
      strcpy(colname,"Y");
    } 
    else 
    {
      /* For everything else, the default is "nameY". */
	
      strcpy(colname,coord->name);
      strcat(colname,"Y");
    }
  }

  setStringInCoordDef(&(coord->name_y),colname);

  /* Check for stray FITS errors. */

  sprintf(tempstring, "reading %s_XCOL, %s_YCOL, %s_UNIT, %s_XSCL, and %s_YSCL keywords from", coord->name, coord->name, coord->name, coord->name, coord->name);
  checkFITSerrors(status, tempstring, teldef->filename);
  if(status)
    return status;

  return 0;
}



/* Read randomization parameters from TelDef keywords. */

int readRandomizationFromTeldef2 /* Returns CFITSIO status */
(
  fitsfile* fp, /* TelDef file pointer */
  TELDEF2* teldef /* teldef structure */
  )
{
  int sysnamematches = 0;
  int sys = 0;
  int status = 0;

  /* Read the randomization coordinate system from the RANCOORD keyword. */

  fits_read_key_str(fp, "RANCOORD", teldef->randsysname, NULL, &status);

  if(status == KEY_NO_EXIST)
  {
    /* Use RAW if the keyword is not found. */

    status = 0;
    strcpy(teldef->randsysname, "RAW");
  }
  else
  {
    checkFITSerrors(status, "reading RANCOORD from", teldef->filename);
    if(status)
      return status;
  }

  /* Make sure the RANCOORD value is a real coordinate system or NONE. 
   * If it not a real system, set to NONE. */
  
  if(!strcasecmp(teldef->randsysname, "NONE"))
  {
    sysnamematches = 1;
    teldef->randsys = -1;
  }
  else
  {
      
    for(sys = 0; sys < teldef->n_coordsys; sys++)
    {
      if(!strcasecmp(teldef->randsysname, teldef->coordsysnames[sys]))
      {
	sysnamematches = 1;
	teldef->randsys = sys;
	break;
      }
    }
  }

  if(!sysnamematches)
  {
    fprintf(stdout, "Keyword value RANCOORD = %s is not a recognized coordinate system for %s %s.\nSetting RANCOORD = NONE to suppress any TelDef recommendation for event location randomization.\n", teldef->randsysname, teldef->mission, teldef->instrument);
    strcpy(teldef->randsysname, "NONE");
    teldef->randsys = -1;
  }

  /* If RANCOORD is not NONE (randsys >= 0) at this point, read the
     RAN_SCAL keyword to get the name of the system whose pixel size
     is to be used for the event location randomization. If it is not
     found or refers to an invalid system name, set it to the value of
     the RANCOORD keyword. */
  
  sysnamematches = 0;
  
  if(teldef->randsys >= 0)
  {
    fits_read_key_str(fp, "RAN_SCAL", teldef->randscalesysname, NULL, &status);
    if(status == KEY_NO_EXIST)
    {
      status = 0;
      strcpy(teldef->randscalesysname, teldef->randsysname);
    }
    else
    {
      checkFITSerrors(status, "reading RAN_SCAL from", teldef->filename);
      if(status)
	return status;
    }
      
    for(sys = 0; sys < teldef->n_coordsys; sys++)
    {
      if(!strcasecmp(teldef->randscalesysname, teldef->coordsysnames[sys]))
      {
	sysnamematches = 1;
	teldef->randscalesys = sys;
	break;
      }
    }
      
    if(!sysnamematches)
    {
      fprintf(stdout, "Keyword RAN_SCAL = %s is not a recognized coordinate system for %s %s. Setting RAN_SCAL = %s.\n", teldef->randscalesysname, teldef->mission, teldef->instrument, teldef->randsysname);
      strcpy(teldef->randsysname, teldef->randsysname);
      teldef->randscalesys = teldef->randsys;
    }
  } 
  else /* randsysname == NONE, so set randscalesysname = NONE showing it won't be used. */
  {
    strcpy(teldef->randscalesysname, "NONE");
    teldef->randscalesys = -1;
  }

  return 0;
}


/* Retrieve the coordinate system number from the name. */

int getCoordSystemNumberFromName /* Returns coordinate system number */
(
  TELDEF2* teldef,  /* TelDef structure */
  const char* name /* Coordinate system number */
  )
{
  int sys = 0;
  
  /* Scan the array of coordinate system names until the desired one is found. */

  while(sys < teldef->n_coordsys)
  {
    if(!strcasecmp(teldef->coordsysnames[sys], name))
    {
      return sys;
    }
	
    sys++;
  }

  /* Return -1 if the name wasn't found. */
  
  return -1;
}



/* Set the orientation of the sky coordinate tangent plane.
 * This is used for to-sky SKYATT conversions.
 * R.A. and Dec. are the "nominal pointing" in decimal degrees.
 * When roll = 0, the tangent plane will have zhat pointing toward (ra,dec).
 * xhat will point along -RA and yhat will point along +Dec. */
void setSkyCoordCenterInTelDef2
(
  TELDEF2* teldef, /* TelDef structure */
  double ra, /* right ascension of nominal pointing */
  double dec, /* Declination of nominal pointing */
  double roll /* Roll angle from positive SKY Y axis to positive declination (North) 
	       *  normally set to 0 */
  )
{
  TR_SKYATT* skyattparam = NULL;
  ALIGN* align;
  int lowsys = -1;
  int sys = 0;

  /* Search for the system number of final SKYATT transformation. */

  for(sys = teldef->n_coordsys - 2; sys >= 0; sys--)
  {
    if(teldef->trtypes[sys] == e_TT_SKYATT)
    {
      lowsys = sys;
      break;
    }
  }
  
  if(lowsys == -1)
  {
    fprintf(stderr, "Cannot find a SKYATT transformation with which to associate RA, dec, and roll.\n");
    return;
  }

  skyattparam = teldef->skyattparam[lowsys];


  /* To convert to Quat, use the roll convention (sign and offset)
   * from the align structure read from the TelDef file, but use an
   * identity alignment matrix rather than one read from the TelDef
   * file. The correction from the alignment matrix does not apply to
   * this.  Typically roll should be set to 0 by the user to align the
   * image with north up and east left, but other values could be used
   * in testing.
   */

  align = allocateDefaultAlign();
  align->roll_sign = skyattparam->alignment->roll_sign;
  align->roll_offset = skyattparam->alignment->roll_offset;

  convertRADecRollToQuat(align, skyattparam->q0, ra, dec, roll);

  destroyAlign(align);
  
  /* Calculate the equivalent rotation matrix. */
  
  convertQuatToRotMatrix(skyattparam->rot0, skyattparam->q0);
} 

/* Call the various routines for setting the various types of 2D
   transformations for all the coordinate systems and segments. */

void setXform2dFromTransformationParameters
(
  TELDEF2* teldef, /* teldef structure */
  COORDDEF* origcoord, /* originating coordinate system structure */
  COORDDEF* destcoord, /* destination coordinate system structure */
  int sys, /* system number of originating coordinate system */
  int seg  /* segment number */
  )
{
  XFORM2D* basictrans = NULL;
  XFORM2D* fulltrans = NULL;

  /* Get the basic transformation. */

  basictrans = getXform2dFromTrBasicParameters(teldef, origcoord, destcoord, 
					       teldef->basicparam[sys], sys);
  
#ifdef DEBUG
  printf("sys=%d basictrans:\n", sys);
  printXform2d(basictrans, stdout);
#endif
  
  if(teldef->trtypes[sys] == e_TT_BASIC)
  {
    /* We want the basic 2D transformation for a BASIC transformation. */

    copyXform2d(origcoord->trans, basictrans);
  }
  else if(teldef->trtypes[sys] == e_TT_RAWTODET && 
	  teldef->rawtodetparam[sys]->rawmethod == RM_LINEAR_COEFF)
  {
    /* Use the transformation defined by the linear coefficients. */

    fulltrans = getXform2dFromTrRawtodetLinearCoeffParameters(teldef, basictrans,
							      teldef->rawtodetparam[sys], 
							      sys, seg);
    copyXform2d(origcoord->trans, fulltrans);

    destroyXform2d(fulltrans);
  }
  else if(teldef->trtypes[sys] == e_TT_RAWTODET && 
	  teldef->rawtodetparam[sys]->rawmethod == RM_CORNER_LIST)
  {
    /* Use the transformation defined by the pixel corners. */

    fulltrans = getXform2dFromTrRawtodetCornerMapParameters(teldef, basictrans,
							    origcoord,
							    teldef->rawtodetparam[sys], 
							    sys, seg);
    copyXform2d(origcoord->trans, fulltrans);

#ifdef DEBUG
    printf("sys=%d seg=%d fulltrans:\n", sys, seg);
    printXform2d(origcoord->trans, stdout);
#endif

    destroyXform2d(fulltrans);
  }
  else if(teldef->trtypes[sys] == e_TT_MULTISEG)
  {
    /* Use the transformation defined by the linear coefficients. */
    fulltrans = getXform2dFromTrMultisegLinearCoeffParameters(teldef, basictrans,
							      teldef->multisegparam[sys], 
							      sys, seg);
    copyXform2d(origcoord->trans, fulltrans);

    destroyXform2d(fulltrans);
  }

  /* Clean up. */

  destroyXform2d(basictrans);
  
  return;
}


/* Set the basic transformation parameters. 
 * Return the 2D transformation for the segment */

XFORM2D* getXform2dFromTrBasicParameters /* Returns 2D transformation */
(
  TELDEF2* teldef, /* teldef structure */
  COORDDEF* origcoord, /* originating coordinate system structure */
  COORDDEF* destcoord, /* destination coordinate system structure */ 
  TR_BASIC* basicparam, /* basic transformation structure */
  int sys               /* system number of originating corodinate system */
  )
{
  XFORM2D* translation = NULL;
  XFORM2D* scaling = NULL;
  XFORM2D* rotation = NULL;
  XFORM2D* temp = NULL;
  XFORM2D* trans = NULL;
  int is_rawtodet = (teldef->trtypes[sys] == e_TT_RAWTODET);

  /* Translate the center of the orig coordinates (0., 0.), and then apply an offset. 
   *
   * If the corresponding transformation type is RAWTODET, then the internal coordinate
   * system center should be used instead of that of the lower-level center. */

  translation = allocateXform2d();

  if(is_rawtodet)
  {
    setXform2dToTranslation(translation, 
			    -teldef->rawtodetparam[sys]->int_cen_x - basicparam->xoffset,
			    -teldef->rawtodetparam[sys]->int_cen_y - basicparam->yoffset
      );
      
  }
  else
  {
    setXform2dToTranslation(translation, 
			    -origcoord->center_x - basicparam->xoffset,
			    -origcoord->center_y - basicparam->yoffset
      );
  }

  /* Set the transformation for scaling and inversion. */

  scaling = allocateXform2d();
  setXform2dToScaling(scaling, 
		      basicparam->xflip/basicparam->scale, 
		      basicparam->yflip/basicparam->scale, 0., 0.
    );

  /* Set the transformation for rotation. */

  rotation = allocateXform2d();
  setXform2dToRotation(rotation, 
		       sin(basicparam->rotangle * M_PI / 180.), 
		       cos(basicparam->rotangle * M_PI / 180.), 0., 0.
    );

  /* Combine the translation, scaling, and rotation in this order. */
  temp = allocateXform2d();
  trans = allocateXform2d();
  combineXform2ds(temp, translation, scaling);
  combineXform2ds(trans, temp, rotation);

#ifdef DEBUG
  printf("translation:\n");
  printXform2d(translation, stdout);
  printf("scaling:\n");
  printXform2d(scaling, stdout);
  printf("rotation:\n");
  printXform2d(rotation, stdout);
  printf("trans:\n");
  printXform2d(trans, stdout);
#endif

  /* Translate the origin of the center of the destination coordinate system. 
     Store the result in trans. */

  applyTranslationToXform2d(trans, destcoord->center_x, destcoord->center_y);

#ifdef DEBUG
  printf("dest-transl trans:\n");
  printXform2d(trans, stdout);
#endif

  /* Clean up. */

  destroyXform2d(temp);
  destroyXform2d(translation);
  destroyXform2d(scaling);
  destroyXform2d(rotation);

  return trans;
}


/* Set the parameters for a RAWTODET linear coefficients transformation. 
 * Return the 2D transformation for the segment */

XFORM2D* getXform2dFromTrRawtodetLinearCoeffParameters /* Returns 2D transformation */
(
  TELDEF2* teldef, /* teldef structure */
  XFORM2D* int2dettrans,  /* 2D transformation structure for the 
			     internal-to-upper coordinate system */
  TR_RAWTODET* rawtodetparam, /* Structure holding transformation parameters */
  int sys, /* system number of originating coordinate structure */
  int seg /* segment number of originating coordinate system */
  )
{
  XFORM2D* raw2inttrans = allocateXform2d();
  XFORM2D* fulltrans = allocateXform2d();
  
  /* Set the RAW-to-INT transformation from the linear coefficients. */

  raw2inttrans->xshift = rawtodetparam->coeff_x_a[seg];
  raw2inttrans->rot[0][0] = rawtodetparam->coeff_x_b[seg];
  raw2inttrans->rot[0][1] = rawtodetparam->coeff_x_c[seg];
  
  raw2inttrans->yshift = rawtodetparam->coeff_y_a[seg];
  raw2inttrans->rot[1][0] = rawtodetparam->coeff_y_b[seg];
  raw2inttrans->rot[1][1] = rawtodetparam->coeff_y_c[seg];

  /* Combine RAW-to-INT and INT-to-DET transformations for the full
   * RAW-to-DET transformation. */

  combineXform2ds(fulltrans, raw2inttrans, int2dettrans);

#ifdef DEBUG
  printf("sys=%d seg=%d raw-to-int trans:\n", sys, seg);
  printXform2d(raw2inttrans, stdout);
  printf("sys=%d seg=%d int-to-det trans:\n", sys, seg);
  printXform2d(int2dettrans, stdout);
  printf("sys=%d seg=%d full raw-to-det trans:\n", sys, seg);
  printXform2d(fulltrans, stdout);
#endif

  /* Clean up. */

  destroyXform2d(raw2inttrans);
  
  return fulltrans;
}


/* Set the parameters for a RAWTODET pixel corner map transformation.
 * Return the 2D transformation for the segment */

XFORM2D* getXform2dFromTrRawtodetCornerMapParameters /* Returns 2D transformation */
(
  TELDEF2* teldef, /* teldef structure */
  XFORM2D* basictrans, /* 2D transformation structure for the 
			  internal-to-upper coordinate system */
  COORDDEF* origcoord, /* originating coordinate system structure */
  TR_RAWTODET* rawtodetparam, /* structure holding transformation parameters */
  int sys, /* system number of originating coordinate structure */
  int seg  /* segment number of originating coordinate system */
  )
{
  XFORM2D* fulltrans = allocateXform2d();
  XFORM2D* pretrans = allocateXform2d();
  XFORM2D* middletrans = allocateXform2d();
  XFORM2D* temptrans = allocateXform2d();
  
  /* Set a translation from the raw address space specified in the
   * basic transformation parameters to coordinates with a single
   * pixel whose center is at (0.5, 0.5). This is because
   * setXform2dFromCornerPixels returns a transform from the latter
   * coordinates. */

  setXform2dToScaling(pretrans, 1./(double)origcoord->npixels_x,
		      1./(double)origcoord->npixels_y,
		      origcoord->center_x, origcoord->center_y);

  applyTranslationToXform2d(pretrans, 0.5 - origcoord->center_x,
			    0.5 - origcoord->center_y);

#ifdef DEBUG
  printf("sys=%d seg=%d pretrans:\n", sys, seg);
  printXform2d(pretrans, stdout);
#endif

  /* Get the transformation from the pixel corner map. */

  setXform2dFromCornerPixels(middletrans, 
			     rawtodetparam->corner_x[seg], rawtodetparam->corner_y[seg]);

  /* Combine the transformations. */

  combineXform2ds(temptrans, pretrans, middletrans);
  combineXform2ds(fulltrans, temptrans, basictrans);

  /* Clean up. */

  destroyXform2d(pretrans);
  destroyXform2d(temptrans);
  destroyXform2d(middletrans);
  
  return fulltrans;
}



/* Set the parameters for a MULTISEG linear coefficients transformation.
 * Return the 2D transformation for the segment. */
				  
XFORM2D* getXform2dFromTrMultisegLinearCoeffParameters /* Returns 2D transformation */
(
  TELDEF2* teldef, /* teldef structure */
  XFORM2D* int2hightrans, /* 2D transformation structure for the 
			     internal-to-upper coordinate system */
  TR_MULTISEG* multisegparam, /* structure holding transformation parameters */
  int sys, /* system number of originating coordinate structure */
  int seg  /* segment number of originating coordinate system */
  )
{
  XFORM2D* trans = allocateXform2d();
  
  /* Set the transformation from the linear coefficients. */

  trans->xshift = multisegparam->coeff_x_a[seg];
  trans->rot[0][0] = multisegparam->coeff_x_b[seg];
  trans->rot[0][1] = multisegparam->coeff_x_c[seg];
  
  trans->yshift = multisegparam->coeff_y_a[seg];
  trans->rot[1][0] = multisegparam->coeff_y_b[seg];
  trans->rot[1][1] = multisegparam->coeff_y_c[seg];

  return trans;
}

/* Convert coordinates from lower- to higher-level coord. system using the RAWTODET transformation. */

int convertToHigherCoordRawtodet /* Returns 0 for success, 1 for failure */
(
  TELDEF2* teldef, /* teldef structure */
  double lowx, /* x coordinate of lower-level system */
  double lowy, /* y coordinate of lower-level system */
  double* highx, /* x coordinate of higher-level system */
  double* highy, /* y coordinate of higher-level system */
  int lowsys, /* number of lower-level system */
  int lowseg  /* segment number of lower-level system */
  )
{
  /* Check the segment number for validity. */
  
  if(lowseg < teldef->min_segment[lowsys] || lowseg >= teldef->min_segment[lowsys] + teldef->n_segments[lowsys])
  {
    /*      fprintf(stderr, "Invalid segment number %d in %s coordinate system, which has valid segment numbers %d to %d\n", lowseg, teldef->coordsysnames[lowsys], teldef->min_segment[lowsys], teldef->min_segment[lowsys] + teldef->n_segments[lowsys] - 1); */
    return 1;
  }

  /* Apply the transformation to the coordinates. */

  applyXform2dToContinuousCoords(teldef->coordsys[lowsys][lowseg]->trans, 
				 highx, highy, lowx, lowy);

  /* Apply nonlinear correction if needed. */

  if(teldef->rawtodetparam[lowsys]->use_nonlinear_corr)
  {
    /* Copy the highx,y coordinates to temporary variables so that highx,y can be output 
     * variables in applyMapXform. */

    double tempx, tempy;
      
    tempx = *highx;
    tempy = *highy;
    applyMapXform(teldef->rawtodetparam[lowsys]->corr_map, highx, highy, tempx, tempy);
  }

  return 0;
}


/* Convert coordinates from higher- to lower-level coord. system using the RAWTODET transformation. */

int convertToLowerCoordRawtodet /* Returns 0 for success, 1 for failure */
(
  TELDEF2* teldef, /* teldef structure */
  double* lowx, /* x coordinate of lower-level system */
  double* lowy, /* y coordinate of lower-level system */
  double highx, /* x coordinate of higher-level system */
  double highy, /* y coordinate of higher-level system */
  int lowsys, /* number of lower-level system */
  int lowseg  /* segment number of lower-level system */
  )
{
  double corrected_x = highx;
  double corrected_y = highy;

  /* Check the segment number for validity. */
  
  if(lowseg < teldef->min_segment[lowsys] || lowseg >= teldef->min_segment[lowsys] + teldef->n_segments[lowsys])
  {
    /*      fprintf(stderr, "Invalid segment number %d in %s coordinate system, which has valid segment numbers %d to %d\n", lowseg, teldef->coordsysnames[lowsys], teldef->min_segment[lowsys], teldef->min_segment[lowsys] + teldef->n_segments[lowsys] - 1); */
    return 1;
  }

  /* Apply the inverse nonlinear correction if needed. */

  if(teldef->rawtodetparam[lowsys]->use_nonlinear_corr)
  {
    applyMapXform(teldef->rawtodetparam[lowsys]->inv_corr_map, 
		  &corrected_x, &corrected_y, highx, highy);
  }
  
  /* Apply the transformation to the coordinates. */
  
  applyXform2dToContinuousCoords(teldef->coordsys[lowsys][lowseg]->inverse_trans, 
				 lowx, lowy, corrected_x, corrected_y);

  return 0;
}


/* Convert coordinates from lower- to higher-level coord. system using the BASIC transformation. */

int convertToHigherCoordBasic /* Returns 0 for success, 1 for failure */
(
  TELDEF2* teldef, /* teldef structure */
  double lowx, /* x coordinate of lower-level system */
  double lowy, /* y coordinate of lower-level system */
  double* highx, /* x coordinate of higher-level system */
  double* highy, /* y coordinate of higher-level system */
  int lowsys /* number of lower-level system */
  )
{
  /* Apply the transformation to the coordinates. */

  applyXform2dToContinuousCoords(teldef->coordsys[lowsys][teldef->min_segment[lowsys]]->trans, 
				 highx, highy, lowx, lowy);

  return 0;
}

/* Convert coordinates from higher- to lower-level coord. system using the BASIC transformation. */

int convertToLowerCoordBasic /* Returns 0 for success, 1 for failure */
(
  TELDEF2* teldef, /* teldef structure */
  double* lowx, /* x coordinate of lower-level system */
  double* lowy, /* y coordinate of lower-level system */
  double highx, /* x coordinate of higher-level system */
  double highy, /* y coordinate of higher-level system */
  int lowsys /* number of lower-level system */
  )
{
  /* Apply the transformation to the coordinates. */

  applyXform2dToContinuousCoords(teldef->coordsys[lowsys][teldef->min_segment[lowsys]]->inverse_trans, 
				 lowx, lowy, highx, highy);

  return 0;
}

/* Convert coordinates from lower- to higher-level coord. system using the MULTISEG transformation. */

int convertToHigherCoordMultiseg /* Returns 0 for success, 1 for failure */
(
  TELDEF2* teldef, /* teldef structure */
  double lowx, /* x coordinate of lower-level system */
  double lowy, /* y coordinate of lower-level system */
  double* highx, /* x coordinate of higher-level system */
  double* highy, /* y coordinate of higher-level system */
  int lowsys, /* number of lower-level system */
  int* lowprops,  /* segment properties of lower-level system */
  long window_offset_x, /* x offset for windowing */
  long window_offset_y  /* y offset for windowing */
  )

{
  int propsfound = 0;
  int usethisrow = 1;
  int prop = 0;
  int coeffrow = 0;
  long tempx = 0;
  long tempy = 0;
  
  /* Check for correct set of segment properties. */

  for(coeffrow = 0; coeffrow < teldef->multisegparam[lowsys]->n_rows; coeffrow++)
  {
    usethisrow = 1;
    for(prop = 0; prop < teldef->multisegparam[lowsys]->n_properties; prop++)
    {
      if(lowprops[prop] != teldef->multisegparam[lowsys]->properties[prop][coeffrow])
      {
	usethisrow = 0;
      }

      if(!usethisrow)
	break;
    }
      
    if(usethisrow)
    {
      propsfound = 1;
      break;
    }
  }

  /* Transformation failed if the matching row wasn't found. */

  if(!propsfound)
    return 1;

  /* The correct set of properties were found, so convert the
     coordinates with its transformation and add the windowing offset. */
  
  tempx = (int) (lowx + 0.5 - teldef->multisegparam[lowsys]->coeff_x_e[coeffrow]) 
    % (int) teldef->multisegparam[lowsys]->coeff_x_d[coeffrow];
  tempy = (int) (lowy + 0.5 - teldef->multisegparam[lowsys]->coeff_y_e[coeffrow]) 
    % (int) teldef->multisegparam[lowsys]->coeff_y_d[coeffrow];

  applyXform2dToContinuousCoords(teldef->coordsys[lowsys][coeffrow]->trans, 
				 highx, highy, tempx, tempy);

  *highx += window_offset_x;
  *highy += window_offset_y;

  return 0;
}

/* Convert coordinates from higher- to lower-level coord. system using the MULTISEG transformation. */

int convertToLowerCoordMultiseg /* Returns 0 for success, 1 for failure */
(
  TELDEF2* teldef, /* teldef structure */
  double* lowx, /* x coordinate of lower-level system */
  double* lowy, /* y coordinate of lower-level system */
  double highx, /* x coordinate of higher-level system */
  double highy, /* y coordinate of higher-level system */
  int lowsys, /* number of lower-level system */
  int* lowprops,  /* segment properties of lower-level system */
  long window_offset_x, /* x offset for windowing */
  long window_offset_y  /* y offset for windowing */
  )
{
  int propsfound = 0;
  int usethisrow = 1;
  int prop = 0;
  int coeffrow = 0;
  
  /* Check for correct set of segment properties. */

  for(coeffrow = 0; coeffrow < teldef->multisegparam[lowsys]->n_rows; coeffrow++)
  {
    usethisrow = 1;
    for(prop = 0; prop < teldef->multisegparam[lowsys]->n_properties; prop++)
    {
      if(lowprops[prop] != teldef->multisegparam[lowsys]->properties[prop][coeffrow])
      {
	usethisrow = 0;
      }

      if(!usethisrow)
	break;
    }
      
    if(usethisrow)
    {
      propsfound = 1;
      break;
    }
  }

  /* Transformation failed if the matching row wasn't found. */

  if(!propsfound)
    return 1;

  /* The correct set of properties were found, so convert the
     coordinates with its transformation. At the end, add the coeff_*_e offset. */
  
  applyXform2dToContinuousCoords(teldef->coordsys[lowsys][coeffrow]->inverse_trans,
				 lowx, lowy, highx - window_offset_x, highy - window_offset_y);

  *lowx += teldef->multisegparam[lowsys]->coeff_x_e[coeffrow];
  *lowy += teldef->multisegparam[lowsys]->coeff_y_e[coeffrow];

  return 0;
}

/* Convert coordinates from lower- to higher-level coord. system using
   the SKYATT transformation. This function will calculate the
   transformation for a new attitude quaternion and earth velocity. */

int convertToHigherCoordSkyAtt /* Returns 0 for success, 1 for failure */
(
  TELDEF2* teldef, /* TelDef structure */
  double lowx, /* x coordinate of lower-level system */
  double lowy, /* y coordinate of lower-level system */
  double* highx, /* x coordinate of higher-level system */
  double* highy, /* y coordinate of higher-level system */
  int lowsys, /* number of lower-level system */
  QUAT* q,    /* attitude quaternion  */
  double v,   /* earth speed */
  double vhat[3] /* earth velocity vector */
  )
{
  TR_SKYATT* skyattparam = teldef->skyattparam[lowsys];
  COORDDEF* lowcoordsys = teldef->coordsys[lowsys][teldef->min_segment[lowsys]];
  COORDDEF* highcoordsys = teldef->coordsys[lowsys + 1][teldef->min_segment[lowsys + 1]];
  double conv_factor = 1.;
  
  /* Adjust for misalignment between the telescope and spacecraft axes. */

  productOfQuats(skyattparam->xrt, q, skyattparam->alignment->q_inverse);
#ifdef DEBUG
  printf("att q               = "); printQuat(q, stdout); printf("\n");
  printf("inverse alignment q = "); printQuat(skyattparam->alignment->q_inverse, stdout); printf("\n");
  printf("xrt product q       = "); printQuat(skyattparam->xrt, stdout); printf("\n");
#endif

  /* Calculate the rotation quat from nominal pointing to XRT pointing. */

  getQuatOfChange(skyattparam->delta_q, skyattparam->q0, skyattparam->xrt);
#ifdef DEBUG
  printf("nom. pointing q0    = "); printQuat(skyattparam->q0, stdout); printf("\n");
  printf("nom.-XRT delta q    = "); printQuat(skyattparam->delta_q, stdout); printf("\n");
#endif

  /* Calculate the conversion factor. */

  if(lowsys == teldef->n_coordsys - 2)
  {
    /* Sky coordinates. */

    conv_factor = skyattparam->sky_pix_per_radian;
  }
  else
  {
    /* Other SKYATT transformations. */
      
    conv_factor = teldef->skyattparam[lowsys]->focal_length / 
      teldef->coordsys[lowsys][teldef->min_segment[lowsys]]->scale_x;
  }

  /* Calculate the transformation structure. */

#ifdef DEBUG
  printf("convertQuatToXform2d:\n");
  printf("  (before) det2sky = \n"); printXform2d(skyattparam->det2sky, stdout);
  printf("  delta_q              = "); printQuat(skyattparam->delta_q, stdout); printf("\n");
  printf("  lowcoordsys->center  = %f,%f\n", lowcoordsys->center_x, lowcoordsys->center_y);
  printf("  highcoordsys->center = %f,%f\n", highcoordsys->center_x, highcoordsys->center_y);
  printf("  conv_factor          = %f\n", conv_factor);
#endif

  convertQuatToXform2d(skyattparam->det2sky, skyattparam->delta_q, 
		       lowcoordsys->center_x, lowcoordsys->center_y,
		       highcoordsys->center_x, highcoordsys->center_y,
		       conv_factor);
#ifdef DEBUG
  printf("(after)  det2sky =\n"); printXform2d(skyattparam->det2sky, stdout);
#endif

  /* Correct for aberration if necessary. v is nonzero when the correction is enabled. */

#ifdef DEBUG
  printf("lowsys=%d v=%f vhat=[%f,%f,%f] conv_factor=%f\n", lowsys, v, vhat[0], vhat[1], vhat[2], conv_factor);
#endif
  if(v != 0.)
  {
    addAberrationToXform2d(skyattparam->det2sky, skyattparam->rot0, v, vhat, skyattparam->sky_pix_per_radian);
  }

  /* Convert the coordinates. */

  applyXform2dToContinuousCoords(skyattparam->det2sky, highx, highy, lowx, lowy);

#ifdef DEBUG
  printf("aberr    det2sky =\n"); printXform2d(skyattparam->det2sky, stdout);
#endif
  return 0;
}
			       

/* Convert coordinates from higher- to lower-level coord. system using
   the SKYATT transformation. This function will calculate the
   transformation for a new attitude quaternion and earth velocity. */

int convertToLowerCoordSkyAtt /* Returns 0 for success, 1 for failure */
(
  TELDEF2* teldef, /* TelDef structure */
  double* lowx, /* x coordinate of lower-level system */
  double* lowy, /* y coordinate of lower-level system */
  double highx, /* x coordinate of higher-level system */
  double highy, /* y coordinate of higher-level system */
  int lowsys, /* number of lower-level system */
  QUAT* q,    /* attitude quaternion  */
  double v,   /* earth speed */
  double vhat[3] /* earth velocity vector */
  )
{
  TR_SKYATT* skyattparam = teldef->skyattparam[lowsys];
  COORDDEF* lowcoordsys = teldef->coordsys[lowsys][teldef->min_segment[lowsys]];
  COORDDEF* highcoordsys = teldef->coordsys[lowsys + 1][teldef->min_segment[lowsys + 1]];
  double conv_factor = 1.;
    
  /* Calculate the transformation from lower to higher coordinates.
   * Later, invert the transformation before applying it to the higher
   * coordinates to calculate the lower coordinates. */

  /* Adjust for misalignment between the telescope and spacecraft axes. */

  productOfQuats(skyattparam->xrt, q, skyattparam->alignment->q_inverse);
#ifdef DEBUG
  printf("att q               = "); printQuat(q, stdout); printf("\n");
  printf("inverse alignment q = "); printQuat(skyattparam->alignment->q_inverse, stdout); printf("\n");
  printf("xrt product q       = "); printQuat(skyattparam->xrt, stdout); printf("\n");
#endif

  /* Calculate the rotation quat from nominal pointing to XRT pointing. */

  getQuatOfChange(skyattparam->delta_q, skyattparam->q0, skyattparam->xrt);
#ifdef DEBUG
  printf("nom. pointing q0    = "); printQuat(skyattparam->q0, stdout); printf("\n");
  printf("nom.-XRT delta q    = "); printQuat(skyattparam->delta_q, stdout); printf("\n");
#endif

  /* Calculate the conversion factor. */

  if(lowsys == teldef->n_coordsys - 2)
  {
    /* Sky coordinates. */

    conv_factor = skyattparam->sky_pix_per_radian;
  }
  else
  {
    /* Other SKYATT transformations. */
      
    conv_factor = teldef->skyattparam[lowsys]->focal_length / 
      teldef->coordsys[lowsys][teldef->min_segment[lowsys]]->scale_x;
  }

  /* Calculate the transformation structure. */

#ifdef DEBUG
  printf("convertQuatToXform2d:\n");
  printf("  (before) det2sky = \n"); printXform2d(skyattparam->det2sky, stdout);
  printf("  delta_q              = "); printQuat(skyattparam->delta_q, stdout); printf("\n");
  printf("  lowcoordsys->center  = %f,%f\n", lowcoordsys->center_x, lowcoordsys->center_y);
  printf("  highcoordsys->center = %f,%f\n", highcoordsys->center_x, highcoordsys->center_y);
  printf("  conv_factor          = %f\n", conv_factor);
#endif

  convertQuatToXform2d(skyattparam->det2sky, skyattparam->delta_q, 
		       lowcoordsys->center_x, lowcoordsys->center_y,
		       highcoordsys->center_x, highcoordsys->center_y,
		       conv_factor);
#ifdef DEBUG
  printf("(after)  det2sky =\n"); printXform2d(skyattparam->det2sky, stdout);
#endif

  /* Correct for aberration if necessary. v is nonzero when the correction is enabled. */

#ifdef DEBUG
  printf("lowsys=%d v=%f vhat=[%f,%f,%f] conv_factor=%f\n", lowsys, v, vhat[0], vhat[1], vhat[2], conv_factor);
#endif
  if(v != 0.)
  {
    addAberrationToXform2d(skyattparam->det2sky, skyattparam->rot0, v, vhat, skyattparam->sky_pix_per_radian);
  }

  /* Invert the transformation */

  invertXform2d(skyattparam->sky2det, skyattparam->det2sky);

  /* Convert the coordinates. */

  applyXform2dToContinuousCoords(skyattparam->sky2det, lowx, lowy, highx, highy);

#ifdef DEBUG
  printf("aberr    det2sky =\n"); printXform2d(skyattparam->det2sky, stdout);
  printf("aberr    sky2det =\n"); printXform2d(skyattparam->sky2det, stdout);
#endif
  return 0;
}
			       
/* Convert coordinates from lower- to higher-level coord. system using
   the SKYATT transformation. This function will use the previously
   calculated transformation, which is a time-saver when the attitude
   quaternion and earth velocity have not changed. */

int repeatConvertToHigherCoordSkyAtt /* Returns 0 for success, 1 for failure */
(
  TELDEF2* teldef, /* TelDef structure */
  double lowx, /* x coordinate of lower-level system */
  double lowy, /* y coordinate of lower-level system */
  double* highx, /* x coordinate of higher-level system */
  double* highy, /* y coordinate of higher-level system */
  int lowsys /* number of lower-level system */
  )
{
  /* Convert the coordinates with the previously calculated transformation. */

  applyXform2dToContinuousCoords(teldef->skyattparam[lowsys]->det2sky, highx, highy, lowx, lowy);

#ifdef DEBUG
  printXform2d(teldef->skyattparam[lowsys]->det2sky, stdout);
#endif

  return 0;
}

/* Convert coordinates from higher- to lower-level coord. system using
   the SKYATT transformation. This function will use the previously
   calculated transformation, which is a time-saver when the attitude
   quaternion and earth velocity have not changed. */

int repeatConvertToLowerCoordSkyAtt /* Returns 0 for success, 1 for failure */
(
  TELDEF2* teldef, /* TelDef structure */
  double* lowx, /* x coordinate of lower-level system */
  double* lowy, /* y coordinate of lower-level system */
  double highx, /* x coordinate of higher-level system */
  double highy, /* y coordinate of higher-level system */
  int lowsys /* number of lower-level system */
  )
{
  /* Convert the coordinates with the previously calculated transformation. */

  applyXform2dToContinuousCoords(teldef->skyattparam[lowsys]->sky2det, lowx, lowy, highx, highy);

#ifdef DEBUG
  printXform2d(teldef->skyattparam[lowsys]->det2sky, stdout);
  printXform2d(teldef->skyattparam[lowsys]->sky2det, stdout);
#endif

  return 0;
}

/* Determine the TelDef Format Specification version of a TelDef file. */

double getTelDefFormatVersion /* Returns TelDef Format Spec version */
(
  char* filename /* Teldef filename */
  )
{
  int status = 0;
  fitsfile* fp = NULL;
  int hdutype = 0;
  double td_version = 0;

  /* Open the FITS file. */

  fits_open_file(&fp, filename, READONLY, &status);
  if(status)
  {
    fprintf(stderr, "Cannot open TelDef file %s. Quitting.\n", filename);
    return -1;
  }

  /* Move to primary HDU. */

  fits_movabs_hdu(fp, 1, &hdutype, &status);
  if(status)
  {
    fprintf(stderr, "Cannot find primary extension of %s.\n", filename);
    return -1;
  }

  /* Read TD_VERS keyword. If it does not exist, then set the version
   * to 0.1.  If it does work, set the version to the keyword
   * value. */

  fits_read_key_dbl(fp, "TD_VERS", &td_version, NULL, &status);
  if(status == KEY_NO_EXIST) /* Keyword not found */
  {
    td_version = 0.1;
    status = 0;
  }

  fits_close_file(fp, &status);

  if(status)
  {
    fprintf(stderr, "Could not determine TelDef format specification version of %s.\n", filename);
    return -1;
  }

  return td_version;
}


/* Determine if nonlinear corrections are needed, and if they are, read them
 * from the TelDef file. */
int readNonLinearCorrectionsInTeldef2
(
  fitsfile* fp, /* TelDef file pointer */
  TR_RAWTODET* rawtodetparam, /* TR_RAWTODET structure */
  COORDDEF* origcoord, /* COORDDEF structure for lowest-numbered segment of originating coordinate system */
  char* filename /* TelDef filename */
)
{
  int status = 0;
  int hdutype = ANY_HDU;
  int original_hdu = ANY_HDU;
  int inv_map = 0;
  
#ifdef DEBUG
  printf("readNonLinearCorrectionsInTeldef2: start\n");
#endif
  
  /* Remember the original HDU so we can return there when we are done. */

  fits_get_hdu_num(fp, &original_hdu);
  
  /* Look for a NONLINEAR extension. Its existence determines if nonlinear corrections will be used. */
  
  fits_movnam_hdu(fp, ANY_HDU, "NONLINEAR", 0/*any version*/, &status);
  if(status == BAD_HDU_NUM) 
  {
    /* No such HDU, so there are no nonlinear corrections. */
      
    rawtodetparam->use_nonlinear_corr = 0;
    status = 0;
    return status;
  }
  
#ifdef DEBUG
  printf("readNonLinearCorrectionsInTeldef2: found NONLINEAR extension\n");
#endif
  
  /* If we get here, then we have and will use standard-format non-linear
   * corrections. */
  
  rawtodetparam->use_nonlinear_corr = 1;
  
  /* The non-linear corrections can be stored as an image or as a list
   * of points in a table. 
   *
   * Only the image format is currently supported here.
   */
  
  fits_get_hdu_type(fp, &hdutype, &status);
  
  if(hdutype == IMAGE_HDU) 
    status = readImageMapXformFromTeldef2(fp, rawtodetparam, origcoord, filename, inv_map);
  else                     
  {
    fprintf(stderr, "Table format of nonlinear corrections in the TelDef file is not yet supported.\n");
    rawtodetparam->use_nonlinear_corr = 0;
    status = 1;
  }
  /*readTableMapXformFromTeldef2(fp, rawtodetparam[sys]);*/
  
  if(status)
    return status;

  inv_map = 1;
  fits_movnam_hdu(fp, ANY_HDU, "INVERSE_NONLINEAR", 0, &status);  /* 0 means any version */
  if(status == BAD_HDU_NUM) 
  {
    /* No such HDU, so have to calculate the inverse matrix in the code.
     * Calculate the inverse correction map.  This is a call to a subroutine
     * in coord/xform2d.c */
    invertMapXform(rawtodetparam->inv_corr_map, rawtodetparam->corr_map);
  }
  else
  {
    fits_get_hdu_type(fp, &hdutype, &status);
    if (hdutype == IMAGE_HDU)
    {
      status = readImageMapXformFromTeldef2(fp, rawtodetparam, origcoord,
        filename, inv_map);
    }
    else
    {
      /* Image extension not found, so invert in the code. */
      invertMapXform(rawtodetparam->inv_corr_map, rawtodetparam->corr_map);
    }
  }

  /* Return to primary HDU. */
  
  fits_movabs_hdu(fp, original_hdu, &hdutype, &status);
  
  if(status)
    return status;

  /* Check for any stray FITS errors before we leave this function. */
  
  checkFITSerrors(status,"reading non-linear corrections from", filename);
  
  return status;
}

/* Read nonlinear distortion corrections tables from the TelDef file. */
int readImageMapXformFromTeldef2
(
  fitsfile* fp, /* Pointer to TelDef file */
  TR_RAWTODET* rawtodetparam, /* Pointer to TR_RAWTODET structure */
  COORDDEF* origcoord, /* COORDDEF structure for lowest-numbered segment of originating coordinate system */
  char* filename, /* TelDef filename */
  int inv_map /* Flag for transformation direction */
  ) 
{
  int status=0;
  int anynull=0;
  
  long dimen1;
  long dimen2;
  long npixels;
  
  int mapxform_fits_type;
  MAPXFORM_TYPE null_value = 0.;
  
  double scale_x, scale_y;
  double origin_x, origin_y;
  
  /* Determine the dimensions of the map arrays
   * and allocate the map transform. */
  
  fits_read_key_lng(fp,"NAXIS1",&dimen1,NULL,&status);
  fits_read_key_lng(fp,"NAXIS2",&dimen2,NULL,&status);
  
  if (!inv_map)
  {
    /* If inv_map is true, then this is the second pass through the routine.
     * In that case, don't allocate the maps since they were already allocated
     * on the first pass. */

    rawtodetparam->corr_map = allocateMapXform(dimen1,dimen2);
    rawtodetparam->inv_corr_map = allocateMapXform(dimen1,dimen2);
  }

  /* Read the WCS keywords. These give the mapping from map pixels
   * to detector coordinates. */

  readDetToMapPixelScaling(&scale_x, &origin_x, 1, fp);
  readDetToMapPixelScaling(&scale_y, &origin_y, 2, fp);
  
  if (inv_map)
  {
    setMapXformAxes(rawtodetparam->inv_corr_map, origin_x, scale_x, origin_y, scale_y);
  }
  else
  {
    setMapXformAxes(rawtodetparam->corr_map, origin_x, scale_x, origin_y, scale_y);
  }
  
  /******************************************************
   * get FITS data type code for MAPXFORM arraydata type *
   ******************************************************/
  if(sizeof(MAPXFORM_TYPE) == sizeof(float)) 
    mapxform_fits_type = TFLOAT;
  else if(sizeof(MAPXFORM_TYPE) == sizeof(double)) 
    mapxform_fits_type = TDOUBLE;
  else 
  {
    fprintf(stderr,"Error: MAPXFORM_TYPE not float or double\n");
    return 1;
  }

  /* Now read the correction values from the image
   * the image has two planes, the first is the deltax values and
   * the second is the deltay values. */
  
  npixels = dimen1*dimen2;

  if (inv_map)
  {
    fits_read_img(fp, mapxform_fits_type, 1l, npixels, 
                  &null_value, 
                  getMapXformDeltaxBlock(rawtodetparam->inv_corr_map), 
                  &anynull, &status);

    fits_read_img(fp, mapxform_fits_type, 1l+npixels, npixels, 
                  &null_value,
                  getMapXformDeltayBlock(rawtodetparam->inv_corr_map), 
                  &anynull, &status);
  }
  else
  {
    fits_read_img(fp,mapxform_fits_type, 1l, npixels,
                  &null_value,
                  getMapXformDeltaxBlock(rawtodetparam->corr_map),
                  &anynull, &status);
    
    fits_read_img(fp,mapxform_fits_type, 1l+npixels, npixels,
                  &null_value,
                  getMapXformDeltayBlock(rawtodetparam->corr_map),
                  &anynull, &status);
  }
  
  checkFITSerrors(status,"reading nonlinear correction array from", filename);
  if(status)
    return status;
  

  /* Merge these transforms with the raw->det transform */

  if(isDistortionInOrigCoords(fp, rawtodetparam, filename) ) 
  {
    /* Transform the corrections to the dest. coord. sys. */

    if (inv_map)
    {
      applyXform2dToMapXform(rawtodetparam->inv_corr_map, origcoord->trans);
    }
    else
    {
      applyXform2dToMapXform(rawtodetparam->corr_map, origcoord->trans);
    }
  }

  return 0;
}


/* Determine if the nonlinear corrections are expressed in the originating or destination coordinates of
 * the transformation.  If they are expressed in orig. coordinates, the corrections need to
 * be transformed to the dest. coordinates, since the corrections are applied after the linear
 * part of the RAWTODET transformation. */
int isDistortionInOrigCoords
(
  fitsfile* fp, /* Pointer to TelDef file */
  TR_RAWTODET* rawtodetparam, /* TR_RAWTODET structure */
  char* filename /* TelDef filename */
  )
{
  int status = 0;

  /* Read the CORINRAW keyword from the NONLINEAR TelDef extension. */

  fits_read_key_log(fp, "CORINRAW", &(rawtodetparam->distortion_in_orig_coord), NULL, &status);
  checkFITSerrors(status, "reading CORINRAW keyword from", filename);

  if(rawtodetparam->distortion_in_orig_coord)
  {
    /* Check if there is more than one segment in the orig. coordinate system. 
     * If there is, this is probably a mistake, so give a warning. */
    if(rawtodetparam->n_segs > 1)
    {
      fprintf(stderr, "Warning: Nonlinear corrections will be applied in segment %d coordinates.\n",
	      rawtodetparam->min_seg);
    }
  }

  return rawtodetparam->distortion_in_orig_coord;
}



/* Handle FITSIO errors while reading Teldef Files. Prints error
   messages but don't terminate execution if there is an error. */
void checkFITSerrors
(
  int status, /* CFITSIO status */
  char* doing, /* FITS action that was just performed, given as a present participle */
  char* filename /* TelDef filename */
  )
{
#ifdef DEBUG
  if(!status) 
    fprintf(stderr,"FITSIO success while %s file %s:\n",doing,filename);
#endif

  if(!status) return;
  
  fprintf(stderr,"FITSIO error while %s file %s:\n",doing,filename);
  fits_report_error(stderr,status);
}
