/*
 * $Source: /headas/headas/attitude/tasks/det2att2/det2att2.c,v $
 * $Revision: 1.15 $
 * $Date: 2016/04/15 20:04:46 $
 *
 * This tool accepts as input a file of translations and rotations
 * in a rectilinear coordinate system attached to a detector or instrument.
 * It converts these individual transformation into an equivalent set
 * of quaternions.  This approximation is valid for small angles.
 *
 * Source files:
 *   det2att2.c
 *   ascii_infile.c, .h
 *   infile.c, .h
 *   param.c, .h
 *
 * Library dependencies
 *   attitude/lib/rew
 *   attitude/lib/coordfits
 *   attitude/lib/coord
 *   heacore/heautils
 * 
 * Modificaton history:
 *   v1.0    2015-07-30  RSH    Code cleanup; tstart, tstop, telapse added.
 */

#include "headas.h"
#define TOOLSUB det2att2
#include "att_fatal.h"
#include "headas_main.c"

#include "pil.h"
#include "param.h"
#include "infile.h"
#include "caldbquery.h"
#include "coordfits.h"
#include "coordfits2.h"

#include "string.h"

static void applyQuatToVector (QUAT * quat,
			       double out[3], double in[3], ROTMATRIX * rm)
{
  convertQuatToRotMatrix(rm, quat);
  applyRotMatrixToVector(rm, out, in);
  
}

/* Given an old-style TelDef structure (format version < 0.2) and name of a detector coordinate system, 
 * return the detector system number. 
 */
static int getCoordSystemNumberFromTelDef(TELDEF* teldef, char* sys_name)
{
  int sys_number = -1;
  int sys = 0;

  for(sys = 0; sys < teldef->n_det_levels; sys++)
    {
      if(!strcasecmp(sys_name, teldef->det[sys]->name))
	{
	  sys_number = sys;
	  break;
	}
    }

  return sys_number;
}


/* The tool's "main" function.  How does this function form
 * part of the actual main program that is to be executed?
 * The answer has several parts:
 *
 * (1) TOOLSUB is defined above as det2att2.
 * (2) A prototype for a TOOLSUB() function is given in headas_main.c.
 * (3) Again in headas_main.c, the real main() is defined as a
 *     wrapper that calls TOOLSUB(), as well as initializaion and
 *     termination functions.
 * (4) The code body of TOOLSUB() is given in att_fatal.h, such that
 *     TOOLSUB() calls TOOLSUB_aux() after using setjmp() to
 *     initialize error handling.
 * (5) The code body of TOOLSUB_aux is here, below. */

void det2att2_aux (void) {

  PARAM* param;

  INFILE* infile;
  double time;
  double deltax, deltay;
  double cos_angle;
  double sin_angle;

  TELDEF* teldef = NULL;
  TELDEF2* teldef2 = NULL;
  char* mission = NULL;
  double td_version = 0.;
  int orig_sys_number = -1;
  char orig_sys_name[FLEN_VALUE] = "";
  char teldef_path[QUERY_MAXPATH];

  double centerx, centery;

  XFORM2D* xf_final = NULL;
  XFORM2D* xf_trans = NULL;
  XFORM2D* xf_rot = NULL;
   
  QUAT* delta_q = NULL;
  QUAT* alignment_q = NULL;
  QUAT* q_final = NULL;
   
  double conv_factor = 1.;
  ATTOUT* out;

  int status = 0;
  long rowno = 0;
  double tstart = 0.0;
  double tstop = 0.0;
  double telapse = 0.0;
   
  /****************************
   * read the input paramaters *
   ****************************/
  param = readParam();

  /**********************
   * open the input file *
   **********************/
  infile = openInfile(param->infile);
  if(infile==NULL) {
    fprintf(stderr, "Can't open infile %s\n", param->infile);
    att_fatal(1);
  }

  /**********************
   * resolve the teldef file
   **********************/
  if (!strncasecmp(param->teldeffile, "CALDB", 5)) {

    CALDBQuery query = { 0 };
    strcpy(query.codename, "TELDEF");
    query.infile = param->infile;

    /* set_caldb_query_qualifiers(&query, param->teldeffile); */

    /* The function simple_caldb_query automatically picks up
     * TELESCOP and INSTRUME from query.infile. */

    if (simple_caldb_query(&query, 0, teldef_path)) {
      fprintf(stderr, "teldef_path=%s\n", teldef_path);
      fprintf(stderr, "unable to resolve teldeffile=%s\n", param->teldeffile);
      att_fatal(1);
    } else {
      PILPutString("teldeffile", teldef_path);
      HDpar_note("teldeffile", teldef_path);
    }
  } else {
    strcpy(teldef_path, param->teldeffile);
  }

  /***********************
   * Check the TelDef file format version and read the teldef file
   * into the correct structure.
   ***********************/

  /* Get TelDef file format version. */

  td_version = getTelDefFormatVersion(teldef_path);
  if(td_version < 0)
    att_fatal(1);

  /* Read the TelDef file into the correct structure. */
  
  if(td_version < 0.2) 
    {
      teldef = readTelDef(teldef_path);
      mission = teldef->mission;
    }
  else
    {
      readTelDef2(teldef_path, &teldef2);
      
      if(param->debug)
	printTelDef2(teldef2, stdout);
      
      mission = teldef2->mission;
    }

  if((td_version < 0.2 && teldef == NULL) 
     || (td_version >= 0.2 && teldef2 == NULL) 
     ) 
    {
      fprintf(stderr, "Can't open teldef %s\n", teldef_path);
      att_fatal(1);
    }

  /***********************
   * open the output file *
   ***********************/
  headas_clobberfile(param->outfile);
  out = createAttOut(param->outfile);
  setAttOutMission(out, mission);
  if(out==NULL) {
    fprintf(stderr, "Can't open output file %s\n", param->outfile);
    att_fatal(1);
  }

  /* Allocate quaternions and transformation structures. */

  xf_trans = allocateXform2d();
  xf_rot = allocateXform2d();
  xf_final = allocateXform2d();
  delta_q = allocateQuat();
  q_final = allocateQuat();

  /* Get centerx, centery values from TelDef file.  Looking up this
   * value in the TelDef structures requires knowing the number of the
   * originating coordinate system.  This number is the sky_from value
   * in a v0-0.1 TelDef file.  For v0.2+, the user must supply it in
   * the startsys parameter.
   */
  
  if(td_version < 0.2)
    {
      /* In old-style TelDef files, orig_sys_number refers to a specific det system, never raw or sky. 
       * It ranges from 0 to teldef->n_det_levels - 1. */
      
      if(!strcasecmp(param->startsys, "SKY_FROM"))
	orig_sys_number = teldef->sky_from;
      else
	{
	  strcpy(orig_sys_name, param->startsys);
	  orig_sys_number = getCoordSystemNumberFromTelDef(teldef, orig_sys_name);
	  if(orig_sys_number == -1)
	    {
	      fprintf(stderr, "startsys '%s' is not a coordinate system for %s %s found in TelDef file %s.\n", orig_sys_name, teldef->mission, teldef->instrument, teldef_path);
	      att_fatal(1);
	    }
	  else if(orig_sys_number > teldef->n_det_levels - 1)
	    {
	      fprintf(stderr, "startsys '%s' is not a possible originating coordinate system for %s %s found in TelDef file %s.\n", orig_sys_name, teldef->mission, teldef->instrument, teldef_path);
	      att_fatal(1);
	    }
	}

      centerx = teldef->det[orig_sys_number]->center_x;
      centery = teldef->det[orig_sys_number]->center_y;
      conv_factor = teldef->sky_pix_per_radian;
      alignment_q = teldef->alignment->q;
    }
  else
    {
      /* In the new-style TelDef files, orig_sys_number refers to any coordinate system. 
       * It ranges from 0 to teldef2->n_coordsys - 1. */
      
      strcpy(orig_sys_name, param->startsys);
      if(!strcasecmp(orig_sys_name, "SKY_FROM"))
	{
	  fprintf(stderr, "Please specify an originating coordinate system name in the startsys parameter.  'SKY_FROM' is not compatible with the TelDef file.\n");
	  att_fatal(1);
	}
      orig_sys_number = getCoordSystemNumberFromName(teldef2, orig_sys_name);
      if(orig_sys_number == -1)
	{
	  fprintf(stderr, "startsys '%s' is not a coordinate system for %s %s found in TelDef file %s.\n", orig_sys_name, teldef2->mission, teldef2->instrument, teldef_path);
	  att_fatal(1);
	}
      else if(orig_sys_number >= teldef2->n_coordsys - 1)
	{
	  fprintf(stderr, "startsys '%s' is not a possible originating coordinate system for %s %s found in TelDef file %s.\n", orig_sys_name, teldef2->mission, teldef2->instrument, teldef_path);
	  att_fatal(1);
	}
      else if(teldef2->trtypes[orig_sys_number] != e_TT_SKYATT)
	{
	  fprintf(stderr, "startsys '%s' is not associated with a subsequent coordinate transformation involving an attitude file.\n", orig_sys_name);
	  att_fatal(1);
	}
      
      centerx = teldef2->coordsys[orig_sys_number][teldef2->min_segment[orig_sys_number]]->center_x;
      centery = teldef2->coordsys[orig_sys_number][teldef2->min_segment[orig_sys_number]]->center_y;
      
      if(orig_sys_number == teldef2->n_coordsys - 2)
	conv_factor = teldef2->skyattparam[orig_sys_number]->sky_pix_per_radian;
      else
	conv_factor = teldef2->skyattparam[orig_sys_number]->focal_length / 
teldef2->coordsys[orig_sys_number][teldef2->min_segment[orig_sys_number]]->scale_x;

      alignment_q = teldef2->skyattparam[orig_sys_number]->alignment->q;
    }


  /***************************************
   * loop over the rows of the input file *
   ****************************************/
  while(readInfileValues(infile, &time, &deltax, &deltay, &sin_angle, &cos_angle) ) {
    
    headas_chat(5,"time=%.17g deltax=%.17g deltay=%.17g sin_angle=%.17g cos_angle=%.17g\n", time, deltax, deltay, sin_angle, cos_angle);

    rowno++;
    if (1 == rowno)
      {
        tstart = time;
      }
    
    setXform2dToTranslation(xf_trans, -deltax, -deltay);

    if(param->debug)
      {
	printf("\ntime=%f\n", time);
	printf("xf_trans:\n");
	printXform2d(xf_trans, stdout);
      }

    if(infile->use_angle)
      {
	setXform2dToRotation(xf_rot, sin_angle, cos_angle, centerx, centery);

	if(param->debug)
	  {
	    printf("xf_rot:\n");
	    printXform2d(xf_rot, stdout);
	  }
    
	combineXform2ds(xf_final, xf_trans, xf_rot);
      }
    else
      {
	copyXform2d(xf_final, xf_trans);
      }

    if(param->debug)
      {
	printf("xf_final:\n");
	printXform2d(xf_final, stdout);
      }
    
    convertXform2dToQuat(delta_q, xf_final, centerx, centery,
			 centerx, centery,
			 conv_factor);

    if(param->debug)
      {
	printf("delta_q     = (");
	printQuat(delta_q, stdout);
	printf(")\n");
      }
    
    if(param->debug)
      {
	EULER* e;
	e = allocateEuler();
	convertQuatToEuler(e, delta_q);
	printf("phi=%g theta=%g psi=%g\n", e->phi, e->theta, e->psi);
	destroyEuler(e);
      }

    /* Quat arithmetic. */

    {
      ROTMATRIX rm;
      double q_out[3];
      
      applyQuatToVector(alignment_q, q_out, delta_q->p, &rm);
      
      if(param->debug)
	{
	  printf("alignment_q = (");
	  printQuat(alignment_q, stdout);
	  printf(")\n");
      
	  printf("delta_q     = (");
	  printQuat(delta_q, stdout);
	  printf(")\n");
      
	  printf("q_out       = [%f %f %f]\n", q_out[0], q_out[1], q_out[2]);
	}
      
      delta_q->p[0] = q_out[0];
      delta_q->p[1] = q_out[1];
      delta_q->p[2] = q_out[2];
      
      addAttOutRow(out, time, delta_q);
    }
    tstop = time;
  }
  telapse = tstop - tstart;

  headas_chat(0, "det2att2 finished processing %ld rows.\n", rowno);
  headas_chat(1, "det2att2 tstart = %.15g; tstop = %.15g; telapse = %.15g\n", tstart, tstop, telapse);

  /* Add ORIGSYS and DESTSYS keywords for missions with new-style TelDef formats. */
  if(td_version >= 0.2)
    {
      fits_update_key_str(out->fp, "ORIGSYS", teldef2->coordsys[orig_sys_number][teldef2->min_segment[orig_sys_number]]->name, "Originating coordinate system", &status);
      fits_update_key_str(out->fp, "DESTSYS", teldef2->coordsys[orig_sys_number + 1][teldef2->min_segment[orig_sys_number + 1]]->name, "Destination coordinate system", &status);
      fits_update_key(out->fp, TDOUBLE, "TSTART", &tstart, "Start time", &status);
      fits_update_key(out->fp, TDOUBLE, "TSTOP", &tstop, "Stop time", &status);
      fits_update_key(out->fp, TDOUBLE, "TELAPSE", &telapse, "Elapsed time", &status);
      if(status)
	{
	  fprintf(stderr, "Could not write ORIGSYS, DESTSYS, TSTART, TSTOP, and TELAPSE keywords to extension header.\n");
	  att_fatal(1);
	}
    }

  /*************************
   * HEADAS parameter stamp *
   *************************/
  {
    int status=0;
    HDpar_stamp(out->fp, 0, &status);
    if(status) fits_report_error(stderr, status);
  }

  /* Clean up files and allocated memory. */

  closeInfile(infile);
  closeAttOut(out);

  if(td_version < 0.2)
    destroyTelDef(teldef);
  else
    destroyTelDef2(teldef2);

  destroyXform2d(xf_trans);
  destroyXform2d(xf_rot);
  destroyXform2d(xf_final);
  destroyQuat(delta_q);
  destroyQuat(q_final);

} /* end of attdump function */



/* $Log: det2att2.c,v $
/* Revision 1.15  2016/04/15 20:04:46  rshill
/* Store teldef file name back into parameter if CALDB.
/*
/* Revision 1.14  2015/07/31 01:52:32  rshill
/* Code cleanup; tstart, tstop, telapse added.
/*
 * Revision 1.13  2015/07/01 01:14:44  rshill
 * Added TelDef CALDB query.
 *
 * Revision 1.12  2015/06/15 18:12:13  irby
 * det2att_aux should now be det2att2_aux.
 *
 * Revision 1.11  2015/06/15 16:59:04  irby
 * Rename ahdet2att as "det2att2".
 *
 * Revision 1.10  2015/04/28 15:24:52  rshill
 * Added call to setAttOutMission to set TELESCOP keyword in both
 * current and primary HDUs.
 *
 * Revision 1.9  2015/01/08 21:28:10  rshill
 * Changed user parameter teldef to teldeffile.
 *
 * Revision 1.8  2014/02/20 19:23:44  treichar
 * Updated to use the enumerated TelDef transformation types instead of the type names.
 *
 * Revision 1.7  2013/08/02 20:12:55  treichar
 * Reversed the signs of the sine and cosine of the twist angle in the rotation transformation structure to coordinate with corrections to the twist angle in the preceding cams2det tool.  This change affects Astro-H
 * but not Swift.
 *
 * Revision 1.6  2013/01/17 15:46:37  treichar
 * Changed signs of cosangle and sinangle in attitude calculation to match the sign flip in cams2det. This affects Astro-H but not Swift.
 *
 * Revision 1.5  2013/01/14 18:51:14  treichar
 * Fixed bug where the the cases were flipped for assigning the conv_factor from new-style TelDef values.
 *
 * Revision 1.4  2012/11/13 19:55:47  treichar
 * Corrected quaternion arithmetic, added debug parameter, updated comments and help file.
 *
 * Revision 1.3  2012/10/17 18:17:03  treichar
 * Added more debugging output.
 *
 * Revision 1.2  2012/10/02 21:16:07  treichar
 * Added/edited error cases and messages.
 *
 * Revision 1.1  2012/09/27 21:34:05  treichar
 * Added "new" tool ahdet2att, a modification of det2att.  ahdet2att allows both time-dependent translations (old) and rotations (new) of a coordinate system to be converted to an attitude file.  When development and testing are completed, ahdet2att will replace det2att and will be renamed to det2att.
 *
 * Revision 1.5  2005/10/11 20:32:17  rwiegand
 * Updated to produce quaternions consistent with the telescope definition.
 *
 * Update for Astro-H by Timothy Reichard Oct. 2012:
 * - Added clobber and debug parameter.
 * - Added support for rotation+translation (rather than translation alone)
 *   when generating quaternion.  The input file does not need to hold 
 *   rotation angle information (for backwards compatibility with Swift XRT
 *   files).  A rotation angle of 0 is used when if rotation angle information
 *   is not in the input file.
 * - Added sincol, coscol parameters for specifying rotation angle columns names
 *   of the input file.
 */
