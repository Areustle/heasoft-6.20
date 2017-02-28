/* Create a new TR_TELESCOPE structure and read its contents from a TelDef
 * file. This routine is based on readTrBasic(). */

/* This program is to be added to coordfits library within a new program tr_telescope.c. This
program will include this subroutine readTrTelescope and the other routines
printTrTelescope() and destroyTrTelescope() modeled on the parallel routines in tr_basic.c
This routine must be called from the subroutine
setTransformationParametersFromTelDef2() in the program coordfits/teldef2.c
*/

#include "tr_telescope.h"
#include "coorddef.h"
#include "teldef2.h"

#include "fitsio.h"
#include <math.h>
#include <string.h>
#include <stdlib.h>

/* #define DEBUG 1 */

int readTrTelescope (
  fitsfile* fp, /* TelDef file pointer */
  TR_TELESCOPE** p_telescopeparam, /* Pointer to TR_TELESCOPE structure pointer */
  char* filename /* TelDef filename */
) {
  char keyname[FLEN_KEYWORD];
  char ykeyname[FLEN_KEYWORD];
  char tempstring[1000];
  int status = 0;
  int hdutype = 0;
  TR_TELESCOPE* telescopeparam = NULL;
  double xscale, yscale;

  /* Allocate and initialize the structure. */

  *p_telescopeparam = (TR_TELESCOPE*) calloc(1, sizeof(TR_TELESCOPE));
  telescopeparam = *p_telescopeparam;

  /* Set these to flag values, which can be tested if the OPTAXIS keywords are not set. */

  telescopeparam->low_sys = -999;
  telescopeparam->high_sys = -999;

  /* Since the destination system is the Telescope, it does not have a number within the TelDef.
  Thus set this to the system in which the Optical Axis is defined. */

  telescopeparam->xoffset = 0.;
  telescopeparam->yoffset = 0.;
  telescopeparam->scale = 0.;
  telescopeparam->xflip = 0;
  telescopeparam->yflip = 0;
  telescopeparam->rotangle = 0.;

  /* Move to primary extension. */

  fits_movabs_hdu(fp, 1, &hdutype, &status);
  checkFITSerrors(status, "moving to primary extension of", filename);

  if (0 != status) return status;

  /* Read the coordsysname from the OPTCOORD keyword. */

  strcpy(keyname, "OPTCOORD");
  fits_read_key_str(fp, keyname, telescopeparam->lowcoordsysname, NULL, &status);

  if (status == KEY_NO_EXIST) { /* Keyword not found */
    strcpy( telescopeparam->lowcoordsysname,"NULL");
    status = 0;

    #ifdef DEBUG
    fprintf(stdout, "TelDef keyword %s was not found. Setting OPTCOORD to NULL.\n",
      keyname);
    #endif
  }
  sprintf(tempstring, "reading %s from", keyname);
  checkFITSerrors(status, tempstring, filename);
  if (0 != status) return status;
  strcpy(telescopeparam->highcoordsysname, telescopeparam->lowcoordsysname);

  /* Read the x offset from the OPTAXISX keyword. */
  strcpy(keyname, "OPTAXISX");
  fits_read_key_dbl(fp, keyname, &telescopeparam->xoffset, NULL, &status);
  if (KEY_NO_EXIST == status) { /* Keyword not found */
    telescopeparam->xoffset = 0.0;
    status = 0;

    #ifdef DEBUG
    fprintf(stdout, "TelDef keyword %s was not found. Setting the %s->%s X offset to %f.\n",
      keyname, telescopeparam->lowcoordsysname, telescopeparam->highcoordsysname, 
        telescopeparam->xoffset);
    #endif
  }
  sprintf(tempstring, "reading %s from", keyname);
  checkFITSerrors(status, tempstring, filename);
  if (0 != status) return status;

  /* Read the y offset from the OPTAXISY keyword. */

  strcpy(keyname, "OPTAXISY");
  fits_read_key_dbl(fp, keyname, &telescopeparam->yoffset, NULL, &status);

  if (KEY_NO_EXIST == status) { /* Keyword not found */
    telescopeparam->yoffset = 0.0;
    status = 0;

    #ifdef DEBUG
    fprintf(stdout, "TelDef keyword %s was not found. Setting the %s->%s Y offset to %f.\n",
      keyname, telescopeparam->lowcoordsysname, telescopeparam->highcoordsysname, 
      telescopeparam->yoffset);
    #endif
  }
  sprintf(tempstring, "reading %s from", keyname);
  checkFITSerrors(status, tempstring, filename);
  if (0 != status) return status;

  /* Read the scaling factor from the xxx_XSCL keyword. 
   * Make sure that the X and Y scales are the same.*/

  strcpy(keyname, telescopeparam->lowcoordsysname);
  strcat(keyname, "_XSCL");
  fits_read_key_dbl(fp, keyname, &xscale, NULL, &status);
  if (KEY_NO_EXIST == status) { /* Keyword not found */
    xscale = 1.0;
    status = 0;

    #ifdef DEBUG
    fprintf(stdout, "TelDef keyword %s was not found. Setting the %s->%s scale factor to %f.\n",
      keyname, telescopeparam->lowcoordsysname, telescopeparam->highcoordsysname, 
        telescopeparam->scale);
    #endif
  }
  sprintf(tempstring, "reading %s from", keyname);
  checkFITSerrors(status, tempstring, filename);
  if (0 != status) return status;

  strcpy(keyname, telescopeparam->lowcoordsysname);
  strcat(keyname, "_YSCL");
  fits_read_key_dbl(fp, keyname, &yscale, NULL, &status);
  if (KEY_NO_EXIST == status) { /* Keyword not found */
    yscale = 1.0;
    status = 0;

    #ifdef DEBUG
    fprintf(stdout, "TelDef keyword %s was not found. Setting the %s->%s scale factor to %f.\n",
      keyname, telescopeparam->lowcoordsysname, telescopeparam->highcoordsysname, 
        telescopeparam->scale);
    #endif
  }
  sprintf(tempstring, "reading %s from", keyname);
  checkFITSerrors(status, tempstring, filename);
  if (0 != status) return status;

  telescopeparam->scale = xscale;

  if (xscale != yscale) {
    fprintf(stdout, "WARNING keyword %s has a different value from %s.\n", keyname, ykeyname);
    fprintf(stdout, "Setting the telescope scale factor to %f.\n", telescopeparam->scale);
  }

  /* Read the X flip factor from the OPTXFLIP keyword. */

  strcpy(keyname, "OPTXFLIP");
  fits_read_key_lng(fp, keyname, &telescopeparam->xflip, NULL, &status);
  if (KEY_NO_EXIST == status) { /* Keyword not found */
    telescopeparam->xflip = 1;
    status = 0;

    #ifdef DEBUG
    fprintf(stdout, "TelDef keyword %s was not found. Disabling the %s->%s X flip.\n",
      keyname, telescopeparam->lowcoordsysname, telescopeparam->highcoordsysname);
    #endif
  }
  sprintf(tempstring, "reading %s from", keyname);
  checkFITSerrors(status, tempstring, filename);
  if (0 != status) return status;

  /* Read the Y flip factor from the OPTYFLIP keyword. */

  strcpy(keyname, "OPTYFLIP");
  fits_read_key_lng(fp, keyname, &telescopeparam->yflip, NULL, &status);
  if (KEY_NO_EXIST == status) { /* Keyword not found */
    telescopeparam->yflip = 1;
    status = 0;

    #ifdef DEBUG
    fprintf(stdout, "TelDef keyword %s was not found. Disabling the %s->%s Y flip.\n",
      keyname, telescopeparam->lowcoordsysname, telescopeparam->highcoordsysname);
    #endif
  }
  sprintf(tempstring, "reading %s from", keyname);
  checkFITSerrors(status, tempstring, filename);
  if (0 != status) return status;

  /* Read the rotation angle (degrees) from the OPT_ROTD keyword. */

  strcpy(keyname, "OPT_ROTD");
  fits_read_key_dbl(fp, keyname, &telescopeparam->rotangle, NULL, &status);
  if (KEY_NO_EXIST == status) { /* Keyword not found */
    telescopeparam->rotangle = 0.0;
    status = 0;

    #ifdef DEBUG
    fprintf(stdout, "TelDef keyword %s was not found.\n", keyname); 
    fprintf(stdout, "Setting the %s->%s rotation angle to %f.\n", 
      telescopeparam->lowcoordsysname, telescopeparam->highcoordsysname, 
      telescopeparam->rotangle);
    #endif
  }
  sprintf(tempstring, "reading %s from", keyname);
  checkFITSerrors(status, tempstring, filename);
  if (0 != status) return status;
  return 0;
}

/* The transformation from a normal system to the raytracing output (DET2) is a
 * simple translation plus rotation, so can be defined as an Xform2d structure.
 * But the way to fill the structure is not standard. The subroutine was
 * modified from getXform2dFromTrBasicParameters.  Note that in the
 * descriptions the origin system is called ‘SYS’, since it could be any system
 * defined in the TelDef. */

XFORM2D* getXform2dFromTrTelescopeParameters (
  COORDDEF* origcoord, /* originating coordinate system structure */
  TR_TELESCOPE* telescopeparam) { /* basic transformation structure */

  XFORM2D* translation = NULL;
  XFORM2D* scaling = NULL;
  XFORM2D* rotation = NULL;
  XFORM2D* temp = NULL;
  XFORM2D* trans = NULL;
  translation = allocateXform2d();

  /* Translate the center of the orig coordinates to (0., 0.), and then apply an offset. */

  setXform2dToTranslation(translation,
  -telescopeparam->xoffset, -telescopeparam->yoffset);

  /* Set the transformation for scaling and inversion. */

  scaling = allocateXform2d();
  setXform2dToScaling(scaling,
  telescopeparam->xflip*telescopeparam->scale,
  telescopeparam->yflip*telescopeparam->scale, 0., 0.);

  /* Set the transformation for rotation. */

  rotation = allocateXform2d();
  setXform2dToRotation(rotation,
  sin(telescopeparam->rotangle * M_PI / 180.),
  cos(telescopeparam->rotangle * M_PI / 180.), 0., 0.);

  /* Combine the translation, scaling, and rotation in this order. */

  temp = allocateXform2d();
  trans = allocateXform2d();
  combineXform2ds(temp, translation, scaling);
  combineXform2ds(trans, temp, rotation);

  /* There is no need to translate the origin to the center of the 
   * destination coordinate system since the destination telescope system 
   * is centered at (0, 0). */

  /* Clean up. */

  destroyXform2d(temp);
  destroyXform2d(translation);
  destroyXform2d(scaling);
  destroyXform2d(rotation);
  return trans;
}


/* Print a TR_BASIC structure to a stream */
void printTrTelescope (
  TR_TELESCOPE* telescopeparam, /* TR_TELESCOPE structure to print */
  FILE* stream) { /* destination stream */

  /* Print the structure element by element. */

  fprintf(stream, "    Tr_Telescope structure contents:\n");
  
  fprintf(stream, "      lowcoordsysname: %s\n", telescopeparam->lowcoordsysname);
  fprintf(stream, "      highcoordsysname: %s\n", telescopeparam->highcoordsysname);
  fprintf(stream, "      low_sys: %d\n", telescopeparam->low_sys);
  fprintf(stream, "      high_sys: %d\n", telescopeparam->high_sys);
  fprintf(stream, "      xoffset: %f\n", telescopeparam->xoffset);
  fprintf(stream, "      yoffset: %f\n", telescopeparam->yoffset);
  fprintf(stream, "      scale: %f\n", telescopeparam->scale);
  fprintf(stream, "      xflip: %ld\n", telescopeparam->xflip);
  fprintf(stream, "      yflip: %ld\n", telescopeparam->yflip);
  fprintf(stream, "      rotangle: %f\n", telescopeparam->rotangle);
}


/* Destroy a TR_TELESCOPE structure. */
void destroyTrTelescope(
 TR_TELESCOPE* telescopeparam) { /* TR_BASIC structure to destroy */
#ifdef DEBUG
  int low_sys = -1;
#endif

  /* If telescopeparam is a null pointer, no memory can be freed, so return. */

  if(telescopeparam == NULL)
    return;

#ifdef DEBUG
  low_sys = telescopeparam->low_sys;
  printf("Freeing telescopeparam[%d]\n", low_sys);
#endif

  /* Free the whole structure. */

  free(telescopeparam);

#ifdef DEBUG
  printf("Freed telescopeparam[%d]\n", low_sys);
#endif
}

