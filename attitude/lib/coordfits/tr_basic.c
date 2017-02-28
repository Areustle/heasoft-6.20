#include <math.h>
#include <string.h>
#include "fitsio.h"
#include "longnam.h"
#include "teldef2.h"

/* Create a new TR_BASIC structure and read its contents from a TelDef file. */
int readTrBasic /* Returns CFITSIO status */
(
 fitsfile* fp, /* TelDef file pointer */
 TR_BASIC** p_basicparam,  /* Pointer to TR_BASIC structure pointer */
 int is_rawtodet, /* Is the transformation type actually RAWTODET? */
 char* lowcoordsysname,  /* Name of orig. coord. sys. */
 char* highcoordsysname, /* Name of dest. coord. sys. */
 int low_sys, /* Number of orig. coord. sys. */
 char* filename /* TelDef filename */
 )
{
  
  char keyname[FLEN_KEYWORD];
  char tempstring[1000];
  int status = 0;
  int hdutype = 0;
  TR_BASIC* basicparam = NULL;

  /* Allocate and initialize the structure. */

  *p_basicparam = (TR_BASIC*) malloc(sizeof(TR_BASIC));
  basicparam = *p_basicparam;

  strcpy(basicparam->lowcoordsysname, lowcoordsysname);
  strcpy(basicparam->highcoordsysname, highcoordsysname);
  basicparam->low_sys = low_sys;
  basicparam->high_sys = low_sys + 1;
  basicparam->xoffset = 0.;
  basicparam->yoffset = 0.;
  basicparam->scale = 0.;
  basicparam->xflip = 0;
  basicparam->yflip = 0;
  basicparam->rotangle = 0.;


  /* Move to primary extension. */

  fits_movabs_hdu(fp, 1, &hdutype, &status);
  checkFITSerrors(status, "moving to primary extension of", filename); 
  if(status)
    return status;

  /* Read the x offset from the xxx_XOFF keyword. */

  strcpy(keyname, highcoordsysname);
  strcat(keyname, "_XOFF");
  fits_read_key_dbl(fp, keyname, &basicparam->xoffset, NULL, &status);
  if(status == KEY_NO_EXIST) /* Keyword not found */
    {
      basicparam->xoffset = 0.0;
      status = 0;
#ifdef DEBUG
      fprintf(stdout, "TelDef keyword %s was not found. Setting the %s->%s X offset to %f.\n", keyname, lowcoordsysname, highcoordsysname, basicparam->xoffset);
#endif
    }
  sprintf(tempstring, "reading %s from", keyname);
  checkFITSerrors(status, tempstring, filename);
  if(status)
    return status;

  /* Read the y offset from the xxx_YOFF keyword. */

  strcpy(keyname, highcoordsysname);
  strcat(keyname, "_YOFF");
  fits_read_key_dbl(fp, keyname, &basicparam->yoffset, NULL, &status);
  if(status == KEY_NO_EXIST) /* Keyword not found */
    {
      basicparam->yoffset = 0.0;
      status = 0;
#ifdef DEBUG
      fprintf(stdout, "TelDef keyword %s was not found. Setting the %s->%s Y offset to %f.\n", keyname, lowcoordsysname, highcoordsysname, basicparam->yoffset);
#endif
    }
  sprintf(tempstring, "reading %s from", keyname);
  checkFITSerrors(status, tempstring, filename);
  if(status)
    return status;

  /* Read the scaling factor from the xxx_SCAL keyword. */

  strcpy(keyname, highcoordsysname);
  strcat(keyname, "_SCAL");
  fits_read_key_dbl(fp, keyname, &basicparam->scale, NULL, &status);
  if(status == KEY_NO_EXIST) /* Keyword not found */
    {
      basicparam->scale = 1.0;
      status = 0;
#ifdef DEBUG
      fprintf(stdout, "TelDef keyword %s was not found. Setting the %s->%s scale factor to %f.\n", keyname, lowcoordsysname, highcoordsysname, basicparam->scale);
#endif

    }
  sprintf(tempstring, "reading %s from", keyname);
  checkFITSerrors(status, tempstring, filename);
  if(status)
    return status;

  /* Read the X flip factor from the xxxFLIPX keyword. */

  strcpy(keyname, highcoordsysname);
  strcat(keyname, "XFLIP");
  fits_read_key_lng(fp, keyname, &basicparam->xflip, NULL, &status);
  if(status == KEY_NO_EXIST) /* Keyword not found */
    {
      basicparam->xflip = 1;
      status = 0;
#ifdef DEBUG
      fprintf(stdout, "TelDef keyword %s was not found. Disabling the %s->%s X flip.\n", keyname, lowcoordsysname, highcoordsysname);
#endif
    }
  sprintf(tempstring, "reading %s from", keyname);
  checkFITSerrors(status, tempstring, filename);
  if(status)
    return status;

  /* Read the Y flip factor from the xxxFLIPY keyword. */

  strcpy(keyname, highcoordsysname);
  strcat(keyname, "YFLIP");
  fits_read_key_lng(fp, keyname, &basicparam->yflip, NULL, &status);
  if(status == KEY_NO_EXIST) /* Keyword not found */
    {
      basicparam->yflip = 1;
#ifdef DEBUG
      fprintf(stdout, "TelDef keyword %s was not found. Disabling the %s->%s Y flip.\n", keyname, lowcoordsysname, highcoordsysname);
#endif

      if(is_rawtodet)
	{
	  basicparam->yflip = -1;
#ifdef DEBUG
	  fprintf(stdout, "TelDef keyword %s was not found. Enabling the %s->%s Y flip.\n", keyname, lowcoordsysname, highcoordsysname);
#endif
	}
      status = 0;
    }
  sprintf(tempstring, "reading %s from", keyname);
  checkFITSerrors(status, tempstring, filename);
  if(status)
    return status;

  /* Read the rotation angle (degrees) from the xxx_ROTD keyword. */

  strcpy(keyname, highcoordsysname);
  strcat(keyname, "_ROTD");
  fits_read_key_dbl(fp, keyname, &basicparam->rotangle, NULL, &status);
  if(status == KEY_NO_EXIST) /* Keyword not found */
    {
      basicparam->rotangle = 0.0;
      status = 0;
#ifdef DEBUG
      fprintf(stdout, "TelDef keyword %s was not found. Setting the %s->%s rotation angle to %f.\n", keyname, lowcoordsysname, highcoordsysname, basicparam->rotangle);
#endif
    }
  sprintf(tempstring, "reading %s from", keyname);
  checkFITSerrors(status, tempstring, filename);
  if(status)
    return status;


  return 0;
}



/* Print a TR_BASIC structure to a stream */
void printTrBasic
(
 TR_BASIC* basicparam, /* TR_BASIC structure to print */
 FILE* stream /* destination stream */
 )
{
  /* Print the structure element by element. */

  fprintf(stream, "    Tr_Basic structure contents:\n");
  
  fprintf(stream, "      lowcoordsysname: %s\n", basicparam->lowcoordsysname);
  fprintf(stream, "      highcoordsysname: %s\n", basicparam->highcoordsysname);
  fprintf(stream, "      low_sys: %d\n", basicparam->low_sys);
  fprintf(stream, "      high_sys: %d\n", basicparam->high_sys);
  fprintf(stream, "      xoffset: %f\n", basicparam->xoffset);
  fprintf(stream, "      yoffset: %f\n", basicparam->yoffset);
  fprintf(stream, "      scale: %f\n", basicparam->scale);
  fprintf(stream, "      xflip: %ld\n", basicparam->xflip);
  fprintf(stream, "      yflip: %ld\n", basicparam->yflip);
  fprintf(stream, "      rotangle: %f\n", basicparam->rotangle);
}


/* Destroy a TR_BASIC structure. */
void destroyTrBasic
(
 TR_BASIC* basicparam /* TR_BASIC structure to destroy */
 )
{
#ifdef DEBUG
  int low_sys = -1;
#endif

  /* If basicparam is a null pointer, no memory can be freed, so return. */

  if(basicparam == NULL)
    return;

#ifdef DEBUG
  low_sys = basicparam->low_sys;
  printf("Freeing basicparam[%d]\n", low_sys);
#endif

  /* Free the whole structure. */

  free(basicparam);

#ifdef DEBUG
  printf("Freed basicparam[%d]\n", low_sys);
#endif
}

