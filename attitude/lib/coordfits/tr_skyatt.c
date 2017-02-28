#include <math.h>
#include <string.h>
#include "fitsio.h"
#include "longnam.h"
#include "teldef2.h"

/* Create a new TR_SKYATT structure and read its contents from a TelDef file. */
int readTrSkyAtt /* Returns CFITSIO status */
(
 fitsfile* fp, /* TelDef file pointer */
 TR_SKYATT** p_skyattparam, /* Pointer to TR_SKYATT structure pointer */
 char* lowcoordsysname, /* Name of orig. coord. sys. */
 char* highcoordsysname, /* Name of dest. coord. sys. */
 int low_sys,  /* Number of orig. coord. sys. */
 char* filename /* TelDef filename */
)
{
  char keyname[FLEN_KEYWORD];
  char tempstring[1000];
  int status = 0;
  int hdutype = ANY_HDU;
  TR_SKYATT* skyattparam = NULL;

  /* Allocate and initialize the structure. */
  
  *p_skyattparam = (TR_SKYATT*) malloc(sizeof(TR_SKYATT));
  skyattparam = *p_skyattparam;

  strcpy(skyattparam->lowcoordsysname, lowcoordsysname);
  strcpy(skyattparam->highcoordsysname, highcoordsysname);
  skyattparam->low_sys = low_sys;
  skyattparam->high_sys = low_sys + 1;
  skyattparam->sky_pix_per_radian = 1.; 
  skyattparam->deg_per_sky_unit = 1./60.;  /* arcmin by default */

  /* Move to primary extension. */

  fits_movabs_hdu(fp, 1, &hdutype, &status);
  checkFITSerrors(status, "moving to primary extension of", filename); 
  if(status)
    return status;

  /* Read the focal length from the FOCALLEN keyword. */

  strcpy(keyname, "FOCALLEN");
  fits_read_key_dbl(fp, keyname, &skyattparam->focal_length, NULL, &status);
  if(status == KEY_NO_EXIST) 
    {
      /* Use default value of 1.0 if keyword wasn't found. */

      skyattparam->focal_length = 1.0;
      status = 0;
    }
  sprintf(tempstring, "reading %s from", keyname);
  checkFITSerrors(status, tempstring, filename);
  if(status)
    return status;

  /* Allocate the alignment matrix. */

  status = readTelDef2Alignment(fp, &(skyattparam->alignment), skyattparam->lowcoordsysname, filename);
  
  /* Allocate and initialize the extra structure elements. */

  skyattparam->xrt = allocateQuat();
  setQuatToIdentity(skyattparam->xrt);
  skyattparam->q0 = allocateQuat();
  setQuatToIdentity(skyattparam->q0);
  skyattparam->delta_q = allocateQuat();
  setQuatToIdentity(skyattparam->delta_q);
  skyattparam->det2sky = allocateXform2d();
  setXform2dToTranslation(skyattparam->det2sky, 0., 0.);
  skyattparam->sky2det = allocateXform2d();
  setXform2dToTranslation(skyattparam->sky2det, 0., 0.);
  skyattparam->rot0 = allocateRotMatrix();
  setRotMatrixToIdentity(skyattparam->rot0);

  return status;
}

/* Read TelDef alignment matrix into an ALIGN structure */
int readTelDef2Alignment /* Return CFITSIO status */
(
 fitsfile* fp, /* TelDef file pointer */
 ALIGN** p_align, /* Pointer to ALIGN structure pointer */
 char* lowcoordsysname, /* Number of orig. coord. sys. */
 char* filename /* TelDef filename */
)
{
  ALIGN* align;
  ROTMATRIX* rot;
  int status = 0;

  char keyname[FLEN_KEYWORD];
  char tempstring[1000];
  int i, j;
  long long_value;

  /* Allocate the ALIGN structure. */

  *p_align = allocateDefaultAlign();
  align = *p_align;

  /* Allocate the ROTMATRIX and open the TelDef file */

  rot = allocateRotMatrix();

  /* Read alignment matrix from TelDef keywords. */

  for(j = 0; j < 3; j++)
    {
      for(i = 0; i < 3; i++)
	{
	  /* Read matrix element from xxx_Mij keyword. */
	  
	  sprintf(keyname, "%s_M%d%d", lowcoordsysname, i + 1, j + 1);
	  fits_read_key_dbl(fp, keyname, &(rot->m[j][i]), NULL, &status);

	  /* Try ALIGNMij keyword if xxx_Mij wasn't found. */

	  if(status == KEY_NO_EXIST)
	    {
	      status = 0;
	      sprintf(keyname, "ALIGNM%d%d", i + 1, j + 1);
	      fits_read_key_dbl(fp, keyname, &(rot->m[j][i]), NULL, &status);
	      sprintf(tempstring, "reading %s from", keyname);
	      checkFITSerrors(status, tempstring, filename);
	      if(status)
		return status;
	    }
	  else
	    {
	      sprintf(tempstring, "reading %s from", keyname);
	      checkFITSerrors(status, tempstring, filename);
	      if(status)
		return status;
	    }
	}
    }

  /* Convert rotation matrix to quaternion. */

  convertRotMatrixToQuat(align->q, rot);
  
  /* Check integrity of alignment matrix. */

  if(fabs(normOfQuat(align->q) - 1.0) > ALIGN_ROUNDOFF_ERROR) 
    {
      fprintf(stderr, "Ill-formed alignment matrix for transforming from %s.\n", lowcoordsysname);
      align = NULL;
      return 1;
    }
  
  /* Calculate the inverse quaternion. */

  invertQuat(align->q_inverse, align->q);

  /* Free the rotation matrix. */

  destroyRotMatrix(rot);

  /* Read the roll angle keywords. Default values are ROLLOFF=0 and
     ROLLSIGN=-1 if the keywords are not present in the TelDef
     file. */

  fits_read_key_dbl(fp, "ROLLOFF", &align->roll_offset, NULL, &status);
  if(status == KEY_NO_EXIST) 
    {
      align->roll_offset = 0.;
      status=0;
    }
  
  fits_read_key_lng(fp, "ROLLSIGN", &long_value, NULL, &status);
  if(status == KEY_NO_EXIST) 
    {
      long_value = -1;
      status=0;
    }
  align->roll_sign = long_value;
  if(abs(align->roll_sign) != 1)
    {
      fprintf(stderr, "Invalid ROLLSIGN=%d\n", align->roll_sign);
      align = NULL;
      return 1;
    }
  
  /* Check for stray errors. */

  checkFITSerrors(status, "reading alignment structure from", filename);
  if(status)
    return status;

  /* No problems were encountered, so return 0. */

  return 0;
}



/* Print a TR_SKYATT structure to a stream */
void printTrSkyAtt
(
 TR_SKYATT* skyattparam, /* TR_SKYATT structure to print */
 FILE* stream /* Destination stream */
 )
{
  /* Print the structure element by element. */

  fprintf(stream, "    Tr_SkyAtt structure contents:\n");
  fprintf(stream, "      lowcoordsysname: %s\n", skyattparam->lowcoordsysname);
  fprintf(stream, "      highcoordsysname: %s\n", skyattparam->highcoordsysname);
  fprintf(stream, "      low_sys: %d\n", skyattparam->low_sys);
  fprintf(stream, "      high_sys: %d\n", skyattparam->high_sys);
  fprintf(stream, "      focal_length: %f\n", skyattparam->focal_length);
  fprintf(stream, "      sky_pix_per_radian: %f\n", skyattparam->sky_pix_per_radian);
  fprintf(stream, "      deg_per_sky_unit: %f\n", skyattparam->deg_per_sky_unit);
  fprintf(stream, "      alignment:\n");
  printAlign(skyattparam->alignment, stream);
  fprintf(stream, "      xrt: ");
  printQuat(skyattparam->xrt, stream);
  fprintf(stream, "\n");
  fprintf(stream, "      q0: ");
  printQuat(skyattparam->q0, stream);
  fprintf(stream, "\n");
  fprintf(stream, "      delta_q: ");
  printQuat(skyattparam->delta_q, stream);
  fprintf(stream, "\n");
}

/* Destroy a TR_SKYATT structure. */
void destroyTrSkyAtt
(
 TR_SKYATT* skyattparam /* TR_SKYATT structure to destroy */
 )
{
#ifdef DEBUG
  int low_sys = -1;
#endif

  /* If skyattparam is a null pointer, no memory can be freed, so return. */

  if(skyattparam == NULL)
    return;
  
#ifdef DEBUG
  low_sys = skyattparam->low_sys;
  printf("Freeing skyattparam[%d]\n", low_sys);
#endif

  if(skyattparam->rot0 != NULL)
    destroyRotMatrix(skyattparam->rot0);
  if(skyattparam->det2sky != NULL)
    destroyXform2d(skyattparam->det2sky);
  if(skyattparam->sky2det != NULL)
    destroyXform2d(skyattparam->sky2det);
  if(skyattparam->delta_q != NULL)
    destroyQuat(skyattparam->delta_q);
  if(skyattparam->q0 != NULL)
    destroyQuat(skyattparam->q0);
  if(skyattparam->xrt != NULL)
    destroyQuat(skyattparam->xrt); 

  /* Free the alignment and TR_SKYATT structures. */

  destroyAlign(skyattparam->alignment);
  free(skyattparam);

#ifdef DEBUG
  printf("Freed skyattparam[%d]\n", low_sys);
#endif
}

