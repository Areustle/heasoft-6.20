#include "genorbfile.h"
#include "headas_gti.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* define DEBUG */

/* Create a GENORBFILE structure and open the orbit file. */
GENORBFILE* openGenOrbFile( /* Returns a pointer to a new GENORBFILE struct. The pointer is NULL when the file cannot be read. */
  char* filename, /* Orbit filename (input) */
  char* ext_name, /* Extension containing orbit data (input)  */
  char** orb_col_names, /* Array of velocity column names (input) */
  ORBFORMAT orb_format, /* Enumerated orbit format (input) */
  ORBINTERP interp_method /* Enumerated interpolation method (input) */
  )
{
  GENORBFILE* file = NULL; /* Structure pointer */
  int status = 0;  /* CFITSIO status */
  int col = 0, col2 = 0; /* Column indexes */
  int i = 0; /* Component index */
  char tempstring[2000] = ""; /* Reusable string for messages */
  char comment[FLEN_COMMENT] = ""; /* CFITSIO keyword comment */
  long n_elements = 0; /* Number of elements of velocity or position column */
  long col_bytes = 0; /* Number of bytes in column */
  int type_code = 0; /* Data type of column */
  ORB_FORMAT_INFO** orbinfo = getOrbFormatInfo(); /* Info about velocity formats. Array of structures: orbinfo[orb_format] */
  int format = 0; /* Format index */
  char* tunit_pos[3];     /* Pointers to potential TUNIT strings */
  char* tunit_vel[3];     /* Pointers to potential TUNIT strings */
  char* tunit_kep[6];     /* Pointer to potential TUNIT string */
  double mjdrefi = 0.0;   /* value of MJDREFI keyword */
  double mjdreff = 0.0;   /* value of MJDREFF keyword */

  for (i=0; i<3; i++) {
    tunit_pos[i] = (char*)malloc(FLEN_VALUE);
    tunit_vel[i] = (char*)malloc(FLEN_VALUE);
  }

  for (i=0; i<6; i++) {
    tunit_kep[i] = (char*)malloc(FLEN_VALUE);
  }
    
  /* Allocate memory for the structure. */
  file = (GENORBFILE*) calloc(1, sizeof(GENORBFILE));

  /* Check if the file can be opened and orbit extension exists. */
  fits_open_file(&(file->fp), filename, READONLY, &status);
  checkGenOrbFileFITSErrors(status, "opening orbit file", file);
  if(status)
    return NULL;


  fits_movnam_hdu(file->fp, BINARY_TBL, ext_name, 0, &status);
  sprintf(tempstring, "finding orbit extension '%s' in", ext_name);
  checkGenOrbFileFITSErrors(status, tempstring, file);
  if(status)
    return NULL;

  /* Allocate memory for structure members. */
  file->filename = (char*) malloc((strlen(filename) + 1) * sizeof(char));
  strcpy(file->filename, filename);
  file->ext_name = (char*) malloc((strlen(ext_name) + 1) * sizeof(char));
  strcpy(file->ext_name, ext_name);
  file->telescope = (char*) malloc(FLEN_VALUE * sizeof(char));
  strcpy(file->telescope, "UNKNOWN");
  file->time_col_name = (char*) malloc(FLEN_VALUE * sizeof(char));
  strcpy(file->time_col_name, "TIME");
  file->time_col_num = -1;
  file->pos_col_names = (char**) malloc(MAX_NUM_POS_COLS * sizeof(char*));
  file->pos_col_nums = (int*) malloc(MAX_NUM_POS_COLS * sizeof(int));
  file->num_pos_col_elements = (int*) malloc(MAX_NUM_POS_COLS * sizeof(int));
  file->num_pos_cols = orbinfo[orb_format]->num_pos_cols;
  file->vel_col_names = (char**) malloc(MAX_NUM_VEL_COLS * sizeof(char*));
  file->vel_col_nums = (int*) malloc(MAX_NUM_VEL_COLS * sizeof(int));
  file->num_vel_col_elements = (int*) malloc(MAX_NUM_VEL_COLS * sizeof(int));
  file->num_vel_cols = orbinfo[orb_format]->num_vel_cols;
  file->kep_col_names = (char**) malloc(MAX_NUM_KEP_COLS * sizeof(char*));
  file->kep_col_nums = (int*) malloc(MAX_NUM_KEP_COLS * sizeof(int));
  file->num_kep_col_elements = (int*) malloc(MAX_NUM_KEP_COLS * sizeof(int));
  file->num_kep_cols = orbinfo[orb_format]->num_kep_cols;


  /* Initialize structure members. */
  file->orb_format_num = ORBF_UNKNOWN;
  file->interp_method_num = ORBI_UNKNOWN;
  file->tstart = 0.0;
  file->tstop = 0.0;
  file->duration = 0.0;
  /* Orbit extrapolation is not implemented.
     The parts of this library referring to extrapolation are
     only a hook for future implementation, if needed. */
  file->min_extrapolated_time = 0.0;
  file->max_extrapolated_time = 0.0;
  file->num_rows = 0;
  file->search_row = UNINITIALIZED_ORBFILE_ROW;
  file->search_time0 = 0.0;
  file->search_time1 = 0.0;
  file->search_timem1 = 0.0;
  file->search_timep1 = 0.0;
  file->search_pos0 = allocateOrbPosition();
  setOrbPositionToZero(file->search_pos0);
  file->search_pos1 = allocateOrbPosition();
  setOrbPositionToZero(file->search_pos1);
  file->search_posm1 = allocateOrbPosition();
  setOrbPositionToZero(file->search_posm1);
  file->search_velm1 = allocateOrbVelocity();
  setOrbVelocityToZero(file->search_velm1);
  file->search_velm2 = allocateOrbVelocity();
  setOrbVelocityToZero(file->search_velm2);
  file->search_vel0 = allocateOrbVelocity();
  setOrbVelocityToZero(file->search_vel0);
  file->search_vel1 = allocateOrbVelocity();
  setOrbVelocityToZero(file->search_vel1);
  file->search_kep0 = allocateOrbKeplerian();
  setOrbKeplerianToZero(file->search_kep0);
  file->search_kep1 = allocateOrbKeplerian();
  setOrbKeplerianToZero(file->search_kep1);

  file->last_search_is_reliable = 0;
  file->orb_len_unit_num = ORBU_LEN_UNKNOWN;
  file->orb_ang_unit_num = ORBU_ANG_UNKNOWN;

  /* Sort out column configuration for position, velocity, position and velocity,
   * or Keplerian elements. */

  if (ORBF_COMPONENTS == orb_format ||
      ORBF_VECTOR == orb_format) {
    /* Both position and velocity (position first). */
    for(col = 0; col < MAX_NUM_POS_COLS && col < file->num_pos_cols; col++) {
      file->pos_col_names[col] = (char*) malloc(FLEN_VALUE * sizeof(char));
      strcpy(file->pos_col_names[col], orb_col_names[col]);
      file->pos_col_nums[col] = -1;
      file->num_pos_col_elements[col] = orbinfo[orb_format]->num_pos_elements;
    }
    for(col2 = 0; col2 < MAX_NUM_VEL_COLS && col2 < file->num_vel_cols; col2++) {
      file->vel_col_names[col2] = (char*) malloc(FLEN_VALUE * sizeof(char));
      strcpy(file->vel_col_names[col2], orb_col_names[col+col2]);
      file->vel_col_nums[col2] = -1;
      file->num_vel_col_elements[col2] = orbinfo[orb_format]->num_vel_elements;
    }
  } else if (
      ORBF_COMPONENTS_POS == orb_format ||
      ORBF_VECTOR_POS == orb_format) {
    for(col = 0; col < MAX_NUM_POS_COLS && col < file->num_pos_cols; col++) {
    /* Position only. */
      file->pos_col_names[col] = (char*) malloc(FLEN_VALUE * sizeof(char));
      strcpy(file->pos_col_names[col], orb_col_names[col]);
      file->pos_col_nums[col] = -1;
      file->num_pos_col_elements[col] = orbinfo[orb_format]->num_pos_elements;
    }
  } else if (
      ORBF_COMPONENTS_VEL == orb_format ||
      ORBF_VECTOR_VEL == orb_format) {
    /* Velocity only. */
    for(col = 0; col < MAX_NUM_VEL_COLS && col < file->num_vel_cols; col++) {
      file->vel_col_names[col] = (char*) malloc(FLEN_VALUE * sizeof(char));
      strcpy(file->vel_col_names[col], orb_col_names[col]);
      file->vel_col_nums[col] = -1;
      file->num_vel_col_elements[col] = orbinfo[orb_format]->num_vel_elements;
    }
  } else if (
      ORBF_KEPLERIAN == orb_format ||
      ORBF_KEPLERIAN_NODUP == orb_format) {
    /* Keplerian elements. */
    for(col = 0; col < MAX_NUM_KEP_COLS && col < file->num_kep_cols; col++) {
      file->kep_col_names[col] = (char*) malloc(FLEN_VALUE * sizeof(char));
      strcpy(file->kep_col_names[col], orb_col_names[col]);
      file->kep_col_nums[col] = -1;
      file->num_kep_col_elements[col] = orbinfo[orb_format]->num_kep_elements;
    }
  } else {
    fprintf(stderr, "Invalid orbit file format = %d\n", orb_format);
    return NULL;
  }

  file->orb_format_num = orb_format;

  /* Store interpolation method. */
  if (ORBI_TAYLOR == interp_method) {
    if (ORBF_COMPONENTS == orb_format || ORBF_VECTOR == orb_format) {
      file->interp_method_num = interp_method;
    } else {
      fprintf(stderr, "Orbit file format invalid for Taylor interpolation method.\n");
      fprintf(stderr, "File must have both position and velocity.\n");
      return NULL;
    }
  } else {
      file->interp_method_num = interp_method;
  }

  /* Read the telescope from the TELESCOP keyword. */
  fits_read_key_str(file->fp, "TELESCOP", file->telescope, comment, &status);
  if(status == KEY_NO_EXIST) {
    status = 0;
    strcpy(file->telescope, "UNKNOWN");
  }
  checkGenOrbFileFITSErrors(status, "reading TELESCOP keyword from", file);
  if(status)
    return NULL;

  /* Read the number of rows in the orbit table. */

  fits_read_key_lng(file->fp, "NAXIS2", &(file->num_rows), comment, &status);
  checkGenOrbFileFITSErrors(status, "reading NAXIS2 keyword from", file);
  if(status) 
    return NULL;

  if(0 >= file->num_rows)
    {
      fprintf(stderr, "Orbit file %s has no data\n", file->filename);
      return NULL;
    }

  /* Read the column number of the time column. */

  fits_get_colnum(file->fp, CASESEN, file->time_col_name, &(file->time_col_num), &status);
  sprintf(tempstring, "locating %s column in", file->time_col_name);
  checkGenOrbFileFITSErrors(status, tempstring, file);
  if(status)
    return NULL;

  /* Get MJDREF from the orbit file. */

  file->mjdref = HDget_frac_time(file->fp, "MJDREF", &mjdrefi, &mjdreff, &status);
  checkGenOrbFileFITSErrors(status, "reading MJDREF keywords from", file);
  if (status) 
    return NULL;

  /* Read the column numbers of the orbit columns and check both the
   * physical units and the vector length of each. */

  for(col = 0; col < file->num_pos_cols; col++) {
    fits_get_colnum(file->fp, CASESEN, file->pos_col_names[col], &(file->pos_col_nums[col]), &status);
    sprintf(tempstring, "locating position column '%s' in", file->pos_col_names[col]);
    checkGenOrbFileFITSErrors(status, tempstring, file);
    if(status)
      return NULL;

    fits_get_coltype(file->fp, file->pos_col_nums[col], &type_code, &n_elements,
		     &col_bytes, &status);
    sprintf(tempstring, "determining the number of vector elements in position column '%s' in", file->pos_col_names[col]);
    checkGenOrbFileFITSErrors(status, tempstring, file);
    if(status)
      return NULL;

    if(n_elements != file->num_pos_col_elements[col]) {
      fprintf(stderr, "Column '%s' of orbit file %s\nhas %ld elements, but %d elements are expected.\n", 
	     file->pos_col_names[col], file->filename, n_elements, file->num_pos_col_elements[col]);
      return NULL;
    }

    fits_get_bcolparms(file->fp, file->pos_col_nums[col], NULL, tunit_pos[col], 
      NULL, NULL, NULL, NULL, NULL, NULL, &status);
    sprintf(tempstring, "determining the physical units of position column '%s' in", file->pos_col_names[col]);
    checkGenOrbFileFITSErrors(status, tempstring, file);
    if(status)
      return NULL;
  }

  for(col = 0; col < file->num_vel_cols; col++) {
    fits_get_colnum(file->fp, CASESEN, file->vel_col_names[col], &(file->vel_col_nums[col]), &status);
    sprintf(tempstring, "locating velocity column '%s' in", file->vel_col_names[col]);
    checkGenOrbFileFITSErrors(status, tempstring, file);
    if(status)
      return NULL;

    fits_get_coltype(file->fp, file->vel_col_nums[col], &type_code, &n_elements,
		     &col_bytes, &status);
    sprintf(tempstring, "determining the number of vector elements in velocity column '%s' in", file->vel_col_names[col]);
    checkGenOrbFileFITSErrors(status, tempstring, file);
    if(status)
      return NULL;

    if(n_elements != file->num_vel_col_elements[col]) {
      fprintf(stderr, "Column '%s' of orbit file %s\nhas %ld elements, but %d elements are expected.\n", 
	     file->vel_col_names[col], file->filename, n_elements, file->num_vel_col_elements[col]);
      return NULL;
    }

    fits_get_bcolparms(file->fp, file->vel_col_nums[col], NULL, tunit_vel[col], 
      NULL, NULL, NULL, NULL, NULL, NULL, &status);
    sprintf(tempstring, "determining the physical units of velocity column '%s' in", file->vel_col_names[col]);
    checkGenOrbFileFITSErrors(status, tempstring, file);
    if(status)
      return NULL;
  }

  for(col = 0; col < file->num_kep_cols; col++) {
    fits_get_colnum(file->fp, CASESEN, file->kep_col_names[col], &(file->kep_col_nums[col]), &status);
    sprintf(tempstring, "locating Keplerian column '%s' in", file->kep_col_names[col]);
    checkGenOrbFileFITSErrors(status, tempstring, file);
    if(status)
      return NULL;

    fits_get_coltype(file->fp, file->kep_col_nums[col], &type_code, &n_elements,
		     &col_bytes, &status);
    sprintf(tempstring, "determining the number of vector elements in Keplerian column '%s' in", file->kep_col_names[col]);
    checkGenOrbFileFITSErrors(status, tempstring, file);
    if(status)
      return NULL;

    if(n_elements != file->num_kep_col_elements[col]) {
      fprintf(stderr, "Column '%s' of orbit file %s\nhas %ld elements, but %d elements are expected.\n", 
	     file->kep_col_names[col], file->filename, n_elements, file->num_kep_col_elements[col]);
      return NULL;
    }

    fits_get_bcolparms(file->fp, file->kep_col_nums[col], NULL, tunit_kep[col],
      NULL, NULL, NULL, NULL, NULL, NULL, &status);
    sprintf(tempstring, "determining the physical units of Keplerian column '%s' in", file->kep_col_names[0]);
    checkGenOrbFileFITSErrors(status, tempstring, file);
    if(status)
      return NULL;

  }
  
  setGenOrbFileUnits(file, tunit_pos, tunit_vel, tunit_kep);

  if ((ORBF_VECTOR == orb_format || ORBF_COMPONENTS_POS == orb_format || ORBF_VECTOR_POS == orb_format) &&
    (ORBU_LEN_UNKNOWN == file->orb_len_unit_num)) {
    fprintf(stderr, "Positional units not specified in orbit file.");
    return NULL;
  }

  if ( ( (ORBF_KEPLERIAN == orb_format) || (ORBF_KEPLERIAN_NODUP == orb_format) ) && ORBU_ANG_UNKNOWN == file->orb_ang_unit_num) {
    fprintf(stderr, "Angular units not specified for Keplerian elements in orbit file.");
    return NULL;
  }

  /* Mark the last search results as unreliable because no searches
   * have been done. */
  
  file->search_row = -1;
  file->last_search_is_reliable = 0;
  
  /* Read the last and first times from the orbit table.  Read the
   * first time after reading the last time, in order to position the 
   * FITSIO buffers at the beginning of the file. */

  file->tstop = readTimeFromGenOrbFile(file, file->num_rows);
  file->tstart = readTimeFromGenOrbFile(file, 1L);
  file->duration = file->tstop - file->tstart;

  /* Check that the orbit file has a positive duration. */
  
  if(file->duration <= 0) {
    printf("Warning! Orbit extension '%s' has nonpositive duration %g\n", file->ext_name, file->duration);
    file->duration = 1.;
  }

  /* Set the extrapolated time range to be the same as the actual time range. */

  /* Orbit extrapolation is not implemented.
     The parts of this library referring to extrapolation are
     only a hook for future implementation, if needed. */
  file->min_extrapolated_time = file->tstart;
  file->max_extrapolated_time = file->tstop; 

  /* Free the orbit column info. */
  for(format = 0; format < NUM_ORB_FORMATS; format++)
    free(orbinfo[format]);
  free(orbinfo);

  /* Free the TUNIT info. */
  for (i=0; i<3; i++) {
    free(tunit_pos[i]);
    free(tunit_vel[i]);
  }
  for (i=0; i<6; i++) {
    free(tunit_kep[i]);
  }

  return file;
}

/* ---------------------------------------------------------------------- */

void closeGenOrbFile(GENORBFILE* file)
{
  int col = 0; /* Column index */
  int status = 0; /* CFITSIO status */

  /* Do nothing if the pointer is already null. */
  if(file == NULL)
    return;

  /* Close the FITS file. */
  fits_close_file(file->fp, &status);
  checkGenOrbFileFITSErrors(status, "closing", file);

  /* Free the arrays and structures. */
  for(col = 0; col < MAX_NUM_POS_COLS && col < file->num_pos_cols; col++) {
    free(file->pos_col_names[col]);
  }
  for(col = 0; col < MAX_NUM_VEL_COLS && col < file->num_vel_cols; col++) {
    free(file->vel_col_names[col]);
  }
  for(col = 0; col < MAX_NUM_KEP_COLS && col < file->num_kep_cols; col++) {
    free(file->kep_col_names[col]);
  }
  free(file->num_pos_col_elements);
  free(file->pos_col_nums);
  free(file->pos_col_names);
  free(file->num_vel_col_elements);
  free(file->vel_col_nums);
  free(file->vel_col_names);
  free(file->num_kep_col_elements);
  free(file->kep_col_nums);
  free(file->kep_col_names);
  free(file->time_col_name);
  free(file->telescope);
  free(file->ext_name);
  free(file->filename);
  free(file->search_pos0);
  free(file->search_pos1);
  free(file->search_vel0);
  free(file->search_vel1);
  free(file->search_velm1);
  free(file->search_kep0);
  free(file->search_kep1);
  free(file->search_posm1);
  free(file->search_velm2);

  /* Free the structure. */
  free(file);
}

/* ---------------------------------------------------------------------- */

/* Print the contents of a GENORBFILE structure to a stream. */
void printGenOrbFile(GENORBFILE* file, FILE* stream)
{
  int col = 0; /* Column index */

  /* Print a message if the structure pointer is NULL. */
  if(file == NULL) {
    fprintf(stream, "  Null GENORBFILE structure\n");
    return;
  }

  /* Print the contents of the structure */
  fprintf(stream, "  filename: %s\n", file->filename);
  fprintf(stream, "  ext_name: %s\n", file->ext_name);
  fprintf(stream, "  telescope: %s\n", file->telescope);
  fprintf(stream, "  num_rows: %ld\n", file->num_rows);
  fprintf(stream, "  time_col_name: %s\n", file->time_col_name);
  fprintf(stream, "  time_col_num: %d\n", file->time_col_num);
  fprintf(stream, "  num_pos_cols: %d\n", file->num_pos_cols);
  for(col = 0; col < file->num_pos_cols; col++) {
    fprintf(stream, "  pos_col_names[%d]: %s\n", col, file->pos_col_names[col]);
    fprintf(stream, "  pos_col_nums[%d]: %d\n", col, file->pos_col_nums[col]);
    fprintf(stream, "  num_pos_col_elements[%d]: %d\n", col, file->num_pos_col_elements[col]);
  }
  fprintf(stream, "  num_vel_cols: %d\n", file->num_vel_cols);
  for(col = 0; col < file->num_vel_cols; col++) {
    fprintf(stream, "  vel_col_names[%d]: %s\n", col, file->vel_col_names[col]);
    fprintf(stream, "  vel_col_nums[%d]: %d\n", col, file->vel_col_nums[col]);
    fprintf(stream, "  num_vel_col_elements[%d]: %d\n", col, file->num_vel_col_elements[col]);
  }
  fprintf(stream, "  num_kep_cols: %d\n", file->num_kep_cols);
  for(col = 0; col < file->num_kep_cols; col++) {
    fprintf(stream, "  kep_col_names[%d]: %s\n", col, file->kep_col_names[col]);
    fprintf(stream, "  kep_col_nums[%d]: %d\n", col, file->kep_col_nums[col]);
    fprintf(stream, "  num_kep_col_elements[%d]: %d\n", col, file->num_kep_col_elements[col]);
  }
  fprintf(stream, "  orb_format_num: %d\n", file->orb_format_num);
  fprintf(stream, "  interp_method_num: %d\n", file->interp_method_num);
  fprintf(stream, "  tstart: %.3f\n", file->tstart);
  fprintf(stream, "  tstop: %.3f\n", file->tstop);
  fprintf(stream, "  duration: %.3f\n", file->duration);
  /* Orbit extrapolation is not implemented.
     The parts of this library referring to extrapolation are
     only a hook for future implementation, if needed. */
  fprintf(stream, "  min_extrapolated_time: %.3f\n", file->min_extrapolated_time);
  fprintf(stream, "  max_extrapolated_time: %.3f\n", file->max_extrapolated_time);
  fprintf(stream, "  search_row: %ld\n", file->search_row);
  fprintf(stream, "  search_time0: %g\n", file->search_time0);
  fprintf(stream, "  search_time1: %g\n", file->search_time1);
  fprintf(stream, "  search_pos0: "); printOrbPosition(file->search_pos0, stream); fprintf(stream, "\n");
  fprintf(stream, "  search_pos1: "); printOrbPosition(file->search_pos1, stream); fprintf(stream, "\n");
  fprintf(stream, "  search_vel0: "); printOrbVelocity(file->search_vel0, stream); fprintf(stream, "\n");
  fprintf(stream, "  search_vel1: "); printOrbVelocity(file->search_vel1, stream); fprintf(stream, "\n");
  fprintf(stream, "  search_kep0: "); printOrbKeplerian(file->search_kep0, stream); fprintf(stream, "\n");
  fprintf(stream, "  search_kep1: "); printOrbKeplerian(file->search_kep1, stream); fprintf(stream, "\n");
  fprintf(stream, "  last_search_is_reliable: %d\n", file->last_search_is_reliable);
}

/* ---------------------------------------------------------------------- */

void setGenOrbFileUnits(GENORBFILE* file, 
  char* tunit_pos[3], char* tunit_vel[3], char *tunit_kep[6]) {
  
  ORBLENUNIT orb_len_unit_pos = ORBU_LEN_UNKNOWN;
  ORBLENUNIT orb_len_unit_vel = ORBU_LEN_UNKNOWN;
  ORBLENUNIT orb_len_unit_kep = ORBU_LEN_UNKNOWN;
  ORBANGUNIT orb_ang_unit_kep = ORBU_ANG_UNKNOWN;
  ORBFORMAT ofn = ORBF_UNKNOWN;

  int units_same_pos = 1;      /* Flag for all position TUNITs being equal */
  int units_same_vel = 1;      /* Flag for all velocity TUNITs being equal */
  int units_same_ang = 1;      /* Flag for all Keplerian angle TUNITs being equal */
  int col = 0;                 /* Loop index */

  file->orb_len_unit_num = ORBU_LEN_UNKNOWN;

  if (1 < file->num_pos_cols) {
    for(col = 1; col < file->num_pos_cols; col++) { 
      if (0 != strcmp(tunit_pos[0], tunit_pos[col])) units_same_pos = 0;
    }
  }

  if (1 != units_same_pos) {
    fprintf(stderr, "TUNITS of position columns are inconsistent in %s\n",file->filename);
    return;
  }

  if (1 < file->num_vel_cols) {
    for(col = 1; col < file->num_vel_cols; col++) { 
      if (0 != strcmp(tunit_vel[0], tunit_vel[col])) units_same_vel = 0;
    }
  }

  if (1 != units_same_vel) {
    fprintf(stderr, "TUNITS of velocity columns are inconsistent in %s\n",file->filename);
    return;
  }

  if (1 < file->num_kep_cols) {
    for(col = 3; col < file->num_kep_cols; col++) { 
      if (0 != strcmp(tunit_kep[2], tunit_kep[col])) units_same_ang = 0;
    }
  }

  if (1 != units_same_ang) {
    fprintf(stderr, "TUNITS of Keplerian angular columns are inconsistent in %s\n",file->filename);
    return;
  }
  
  if (0 < file->num_pos_cols) {
    if (0 == strcmp(tunit_pos[0], "m")) {
      orb_len_unit_pos = ORBU_LEN_M;
    } else if (0 == strcmp(tunit_pos[0], "km")) {
      orb_len_unit_pos = ORBU_LEN_KM;
    }
  }
  
  if (0 < file->num_vel_cols) {
    if (0 == strcmp(tunit_vel[0], "m/s")) {
      orb_len_unit_vel = ORBU_LEN_M;
    } else if (0 == strcmp(tunit_vel[0], "km/s")) {
      orb_len_unit_vel = ORBU_LEN_KM;
    }
  }
  
  if (0 < file->num_kep_cols) {
    if (0 == strcmp(tunit_kep[0], "m")) {
      orb_len_unit_kep = ORBU_LEN_M;
    } else if (0 == strcmp(tunit_kep[0], "km")) {
      orb_len_unit_kep = ORBU_LEN_KM;
    }
    if (0 == strcmp(tunit_kep[2], "deg")) {
      orb_ang_unit_kep = ORBU_ANG_DEG;
    } else if (0 == strcmp(tunit_kep[0], "rad")) {
      orb_ang_unit_kep = ORBU_ANG_RAD;
    }
  }
  
  ofn = file->orb_format_num;

  if (ORBF_COMPONENTS == ofn || ORBF_VECTOR == ofn) {
    if (
      (orb_len_unit_pos == ORBU_LEN_M && orb_len_unit_vel == ORBU_LEN_KM) ||
      (orb_len_unit_pos == ORBU_LEN_KM && orb_len_unit_vel == ORBU_LEN_M) ) {
      fprintf(stderr, "TUNITS of position and velocity columns are inconsistent in %s",file->filename);
      return;
    } 
    file->orb_len_unit_num = orb_len_unit_pos;
  } else if (ORBF_COMPONENTS_POS == ofn || ORBF_VECTOR_POS == ofn) {
    file->orb_len_unit_num = orb_len_unit_pos;
  } else if (ORBF_COMPONENTS_VEL == ofn || ORBF_VECTOR_VEL == ofn) {
    file->orb_len_unit_num = orb_len_unit_vel;
  } else if ( (ORBF_KEPLERIAN == ofn) ||  (ORBF_KEPLERIAN_NODUP == ofn)) {
    file->orb_len_unit_num = orb_len_unit_kep;
    file->orb_ang_unit_num = orb_ang_unit_kep;
  } else {
    fprintf(stderr, "orbit format invalid in %s while processing TUNITs",file->filename);
  }

  return;

}
  
/* ---------------------------------------------------------------------- */

void checkGenOrbFileFITSErrors(int status, char* doing, GENORBFILE* file)
{
  /* Do nothing if status is 0. */
  if(0 == status) 
    return;

  /* Print error messages. */
  fprintf(stderr, "FITSIO error while %s orbit file %s:", doing, file->filename);
  fits_report_error(stderr,status);
}

/* ---------------------------------------------------------------------- */

double readTimeFromGenOrbFile(GENORBFILE* file, long row)
{
  int any_null = 0.; /* CFITSIO any_null flag */
  double null_value = 0.; /* CFITSIO null value */
  double time = 0.; /* Orbit time */
  int status = 0; /* CFITSIO status */
  char tempstring[2000] = ""; /* Reusable string for messages */

  /* Read the time. */
  fits_read_col_dbl(file->fp, file->time_col_num, row, 1L, 1L, null_value, &time,
		&any_null, &status);

  /* Check for errors. */
  sprintf(tempstring, "reading time value from row %ld of", row);
  checkGenOrbFileFITSErrors(status, tempstring, file);
  if (status == 0) {
    /* Status is OK:  return the orbit time */
    return time;
  } else {
    /* Status is no good:  exit */
    exit(status);
  }
}

/* ---------------------------------------------------------------------- */

long findTimeInGenOrbFile(GENORBFILE* file, double time)
{
  long row; /* The first of the two rows whose orbit times bracket the input time. */
  double current_time = 0.; /* Current time during time search */
  double last_time = 0.; /* Previous time during time search */
  long row0 = 0; /* Earlier row during time search */
  long row1 = 0; /* Later row during time search */
  double time0 = 0.0; /* Earlier time during bisection time search */
  double time1 = 0.0; /* Later time during bisection time search */

  if(!isTimeWithinGenOrbFile(file, time)) {
    /* Need to extrapolate. */
    /* Orbit extrapolation is not implemented.
       The parts of this library referring to extrapolation are
       only a hook for future implementation, if needed. */
    if(isInExtrapolatedGenOrbFile(file, time)) {
      if(time < file->tstart || file->num_rows == 1) {
        /* If the input time precedes the orbit file time range, or if
         * there is only one row in the orbit table, use the first
         * row. */
        row = 1L;
      } else {
        /* Input time is after the time range. */
        row = file->num_rows - 1;
      }

      /* If row has changed from search_row, update the search times. */
      if(file->search_row != row) {
        file->search_time0 = readTimeFromGenOrbFile(file, row);

        if(file->num_rows > 1)
          file->search_time1 = readTimeFromGenOrbFile(file, row + 1);

        file->search_row = row;
      }

      return file->search_row;
    } else {
      fprintf(stderr, "ERROR:  time %.15g outside of orbit file extrapolated time range %.15g - %.15g\n",
        time, file->min_extrapolated_time, file->max_extrapolated_time);
      exit(1);
    }
  }

/* If current search yields the same bracketing times as the previous
 * search, return the row. */

  if(file->search_row != UNINITIALIZED_ORBFILE_ROW &&
     time >= file->search_time0 && time < file->search_time1
    ) {
    return file->search_row;
  }
  
  /* Searching the orbit table is necessary, so mark the previous
   * search as unreliable. */

  file->last_search_is_reliable = 0;

  if(file->search_row == UNINITIALIZED_ORBFILE_ROW) {
    /* This is the first search, so calculate the start position based
     * on tstart and tstop. */
    row = (time - file->tstart) / file->duration * file->num_rows + 1;
  } else {
    /* Calculate the start position from the current position.  This
     * will work better if the current search is near the last search
     * which we hope would usually be the case. */
    double delta_time = file->search_time1 - file->search_time0;
    if(delta_time <= ORBFILE_TIME_EPSILON) 
      delta_time = file->duration/file->num_rows;
    row = (time - file->search_time0)/delta_time + file->search_row;
  }

  /* Ensure the row is within the table. */
  if(row < 1) 
    row = 1;
  if(row > file->num_rows)
    row = file->num_rows; 

  /* Try a linear search for the current time value. */

  current_time = readTimeFromGenOrbFile(file, row);

  if(time == current_time) {
    /* Update the search row and times. */
    file->search_time0 = current_time;

    if (row+1 <= file->num_rows){
      file->search_time1 = readTimeFromGenOrbFile(file, row + 1);
    } else {
      file->search_time1 = current_time;
    }

    if (row-1 > 0){
	file->search_timem1 = readTimeFromGenOrbFile(file, row - 1);
    } else {
	file->search_timem1 = -1.0;
    }

    if (row+2 <= file->num_rows){
	file->search_timep1 = readTimeFromGenOrbFile(file, row + 2);
    } else {
	file->search_timep1 = -1.0;
    }
    file->search_row = row;
    
    return file->search_row;
  } else if(time > current_time) {
    /* Search forward. */

    long row_limit = row + ORBFILE_LOCAL_SEARCH_LIMIT;
    while(time >= current_time && row < row_limit && row < file->num_rows) { 
      row++;
      last_time = current_time;
      current_time = readTimeFromGenOrbFile(file, row);
    }
    
    /* Check if the time was found. */
    if(time < current_time) {
      /* We passed the point we want. */
      file->search_row = row - 1;
      file->search_time0 = last_time;
      file->search_time1 = current_time;

      if (row-2 > 0){
	  file->search_timem1 = readTimeFromGenOrbFile(file, row - 2);
      } else {
	  file->search_timem1 = -1.0;
      }

      if (row+1 <= file->num_rows){
	  file->search_timep1 = readTimeFromGenOrbFile(file, row + 1);
      } else {
	  file->search_timep1 = -1.0;
      }

      return file->search_row;
    }
  } else if(time < current_time) {
    /* Need to search backwards. */

    long row_limit = row - ORBFILE_LOCAL_SEARCH_LIMIT;
    while(time < current_time && row > row_limit && row > 1) { 
      row--;
      last_time = current_time;
      current_time = readTimeFromGenOrbFile(file, row);
    }

    /* Check if the time was found. */
    if(time >= current_time) {
      /* We found the time. */
      file->search_row = row;;
      file->search_time0 = current_time;
      file->search_time1 = last_time;

      if (row-1 > 0){
	  file->search_timem1 = readTimeFromGenOrbFile(file, row - 1);
      } else {
	  file->search_timem1 = -1.0;
      }

      if (row+2 <= file->num_rows){
	  file->search_timep1 = readTimeFromGenOrbFile(file, row + 2);
      } else {
	  file->search_timep1 = -1.0;
      }

      return file->search_row;
    }
  }

  /* The time wasn't located in a linear search, so try a bisection search. */

  if(time > current_time) {
    /* Search between the current position and the end of the table. */
    row0 = row;
    time0 = current_time;
    row1 = file->num_rows;
    time1 = file->tstop;
  } else {
    /* Search between the current position and the beginning of the table. */
    row0 = 1;
    time0 = file->tstart;
    row1 = row;
    time1 = current_time;
  }

  while(row1 - row0 > 1) {
    row = (row0 + row1)/2;
    current_time = readTimeFromGenOrbFile(file, row);

    if(current_time == time) {
      /* Found it! */
      file->search_row = row;
      file->search_time0 = current_time;
      file->search_time1 = readTimeFromGenOrbFile(file, row + 1);

      if (row+1 <= file->num_rows){
	  file->search_timep1 = readTimeFromGenOrbFile(file, row + 1);
      } else {
	  file->search_timep1 = -1.0;
      }
      return file->search_row;
    } else if (current_time > time) {
      /* Time not found.  Look earlier. */
      row1 = row;
      time1 = current_time;

      if (row+1 <= file->num_rows){
	file->search_timep1 = readTimeFromGenOrbFile(file, row + 1);
      } else {
	file->search_timep1 = -1.0;
      } 

    } else {
      /* Time not found. Look later. */
      row0 = row;
      time0 = current_time;

      if (row+1 <= file->num_rows){
	file->search_timep1 = readTimeFromGenOrbFile(file, row + 1);
      } else {
	file->search_timep1 = -1.0;
      } 

    }
  }

  /* The time has been found by the bisection method.  Update the
   * search row and times. */
  file->search_row = row0;
  file->search_time0 = time0;
  file->search_time1 = time1;
  
  /* Return the search row. */
  return file->search_row;
}

/* ---------------------------------------------------------------------- */

int isTimeWithinGenOrbFile(GENORBFILE* file, double time)
{
  /* Return if the time is between the tstart and tstop times. */
  return (time >= file->tstart && time <= file->tstop);
}

/* ---------------------------------------------------------------------- */

int readOrbPositionFromGenOrbFile(GENORBFILE* file, ORBPOSITION* orbpos, long row)
{
  double null_value = 0.; /* CFITSIO null value */
  int any_null = 0; /* CFITSIO any_null value */
  int col = 0; /* Column index */
  int comp = 0; /* Component index */
  int status = 0; /* CFITSTIO status */
  char tempstring[2000]; /* Reusable string for messages. */

  /* Loop over each velocity column that should be read and read the
   * right number of elements from each. */

  for(col = 0; col < file->num_pos_cols; col++)
  {
    /* Read the position in km. */
    fits_read_col_dbl(file->fp, file->pos_col_nums[col], row, 1L, file->num_pos_col_elements[col], 
		      null_value, &(orbpos->p[col]), &any_null, &status);
    sprintf(tempstring, "reading data from column '%s' in", file->pos_col_names[col]);
    checkGenOrbFileFITSErrors(status, tempstring, file);
    if(status)
      return status;
  }

  /* Convert the velocity to km/s if necessary. */
  if (ORBU_LEN_M == file->orb_len_unit_num) {
    for(comp = 0; comp < 3; comp++)  {
      orbpos->p[comp] /= 1000.0;
    }
  }
  
  /* Return the CFITSIO status */
  return status;
}

/* ---------------------------------------------------------------------- */

int readOrbVelocityFromGenOrbFile(GENORBFILE* file, ORBVELOCITY* orbvel, long row)
{
  double null_value = 0.; /* CFITSIO null value */
  int any_null = 0; /* CFITSIO any_null value */
  int col = 0; /* Column index */
  int comp = 0; /* Velocity component index */
  const int num_comp = 3; /* Number of velocity components */
  int status = 0; /* CFITSTIO status */
  char tempstring[2000]; /* Reusable string for messages. */

  /* Loop over each velocity column that should be read and read the
   * right number of elements from each. */

  for(col = 0; col < file->num_vel_cols; col++)
  {
    /* Read the velocity in km/s or m/s. */
    fits_read_col_dbl(file->fp, file->vel_col_nums[col], row, 1L, file->num_vel_col_elements[col], 
		      null_value, &(orbvel->v[col]), &any_null, &status);
    sprintf(tempstring, "reading data from column '%s' in", file->vel_col_names[col]);
    checkGenOrbFileFITSErrors(status, tempstring, file);
    if(status)
      return status;
  }

  /* Convert the velocity to units of the speed of light. */

  /*
  for(comp = 0; comp < num_comp; comp++)  {
    orbvel->v[comp] /= LIGHT_SPEED_KM_PER_S;
  }
  */

  /* Convert the velocity to km/s if necessary. */
  if (ORBU_LEN_M == file->orb_len_unit_num) {
    for(comp = 0; comp < num_comp; comp++)  {
      orbvel->v[comp] /= 1000.0;
    }
  }
  
  /* Return the CFITSIO status */
  return status;
}

/* ---------------------------------------------------------------------- */

int readOrbKeplerianFromGenOrbFile(GENORBFILE* file, ORBKEPLERIAN* orbkep, long row)
{
  const double MATH_PI = 3.14159265358979323846;  /* CRC Math Tables, 16th Ed. */
  const double DEG2RAD=MATH_PI/180.0;

  double null_value = 0.; /* CFITSIO null value */
  int any_null = 0; /* CFITSIO any_null value */
  int col = 0; /* Column index */
  int status = 0; /* CFITSTIO status */
  char tempstring[2000]; /* Reusable string for messages. */
  double kep_ele[6];  /* Vector of the Keplerian elements */
  double kep_ele0[6];  /* Vector of the Keplerian elements */
  double kep_ele1[6];  /* Vector of the Keplerian elements */
  double time0=0.0,timem1=0.0,timep1=0.0; /* Time values associated with Keplerian elements */
  long row_delta=0;
  float weight0=0.0, weight1=0.0;
  float weights=0.0;
  int i=0;
  int weight_column=0;
  double timetest = -1.0;

  time0 = readTimeFromGenOrbFile(file, row);

  timem1 = -1.0;
  if (row > 1) timem1 = readTimeFromGenOrbFile(file, row-1);

  timep1 = -1.0;
  if (row < file->num_rows) timep1 = readTimeFromGenOrbFile(file, row+1);
  
  if (time0 == timem1 && row > 2) {
    timetest = readTimeFromGenOrbFile(file, row-2);
    if (time0 == timetest){
      fprintf(stderr, "ERROR: More than two duplicated rows in orbit file at row %ld\n",row);
      exit(1);
    }
  }

  if (time0 == timep1 && row < file->num_rows-1) {
    timetest = readTimeFromGenOrbFile(file, row+2);
    if (time0 == timetest){
      fprintf(stderr, "ERROR: More than two duplicated rows in orbit file at row %ld\n",row);
      exit(1);
    }
  }

  row_delta = 0;
  if (time0 == timem1) row_delta = -1;
  if (time0 == timep1) row_delta = +1;

  if (file->orb_format_num == ORBF_KEPLERIAN_NODUP){
    /* If we're in "no duplicate rows allowed" mode, then set all weights = 1.0 */
    weight0 = weight1 = weights = 1.0;
    row_delta = 0;

  } else {
    /* Otherwise, try to locate the WEIGHT column; if not found, set all weight to 1.0 */
    fits_get_colnum(file->fp,CASEINSEN,"WEIGHT",&weight_column,&status);

    if (status != 0){
      /* If there was a problem reading weights, set them all to 1.0 */
      status = 0;
      weight0 = weight1 = weights = 1.0;
    } else {
      /* Otherwise, use the weights */
      fits_read_col_flt(file->fp, weight_column, row, 1, 1, 0, &weight0, &any_null, &status);
      fits_read_col_flt(file->fp, weight_column, row+row_delta, 1, 1, 0, &weight1, &any_null, &status);
      if (row_delta == -1) weights=weight0/(weight0+weight1);
      if (row_delta == 1) weights=weight1/(weight0+weight1);
    }
  }


  /* Loop over each velocity column that should be read and read the
   * right number of elements from each. */

  for(col = 0; col < file->num_kep_cols; col++){
    /* Read the Keplerian elements. */
    fits_read_col_dbl(file->fp, file->kep_col_nums[col], row, 1L, file->num_kep_col_elements[col], 
		      null_value, &(kep_ele0[col]), &any_null, &status);
    sprintf(tempstring, "reading data from column '%s' in", file->kep_col_names[col]);
    checkGenOrbFileFITSErrors(status, tempstring, file);
    if(status)
      return status;
  }


  /* To read the next elements when there are duplicate times. */
  if (row_delta != 0) {
      for(col = 0; col < file->num_kep_cols; col++){
	  /* Read the Keplerian elements. */
	  fits_read_col_dbl(file->fp, file->kep_col_nums[col], row+row_delta, 1L, file->num_kep_col_elements[col], 
			    null_value, &(kep_ele1[col]), &any_null, &status);
	  sprintf(tempstring, "reading data from column '%s' in", file->kep_col_names[col]);
	  checkGenOrbFileFITSErrors(status, tempstring, file);
	  if(status)
	      return status;
      }
  } else {
      kep_ele1[0] = kep_ele0[0];
      kep_ele1[1] = kep_ele0[1];
      kep_ele1[2] = kep_ele0[2];
      kep_ele1[3] = kep_ele0[3];
      kep_ele1[4] = kep_ele0[4];
      kep_ele1[5] = kep_ele0[5];
  }

  /* Different behavior depending on whether the nearest time in the orbit file is before or after the event time. */
  if (row_delta == -1) {
      for (i=0;i<6;i++) kep_ele[i] = kep_ele1[i] + weights*(kep_ele0[i]-kep_ele1[i]);
  } else {
      for (i=0;i<6;i++) kep_ele[i] = kep_ele0[i] + weights*(kep_ele1[i]-kep_ele0[i]);
  }


  orbkep->a = kep_ele[0];
  orbkep->e = kep_ele[1];
  orbkep->i = kep_ele[2];
  orbkep->an = kep_ele[3];
  orbkep->ap = kep_ele[4];
  orbkep->ma = kep_ele[5];

  /* Convert the semimajor axis to km if necessary. */
  if (ORBU_LEN_M == file->orb_len_unit_num) {
    orbkep->a /= 1000.0;
  }

  /* Convert the angular units to rad if necessary. */
  if (ORBU_ANG_DEG == file->orb_ang_unit_num) {
    orbkep->i *= DEG2RAD;
    orbkep->an *= DEG2RAD;
    orbkep->ap *= DEG2RAD;
    orbkep->ma *= DEG2RAD;
  }
  
  /* Return the CFITSIO status */
  return status;
}


/* ---------------------------------------------------------------------- */

void computePosVelFromKeplerian(ORBPOSITION* pos, ORBVELOCITY* vel, 
  ORBKEPLERIAN* kep) {

  /* Gravitational constant times mass of Earth */
  /* #define GM      3.986005e5 - used in atFunctions */

  /* G is from CODATA */
  /* M is from http://solarsystem.nasa.gov/planets/profile.cfm?Object=Earth&Display=Facts */
  /* const double GM=6.67384*5.9722e24;  (units?) */
  const double GM=3.986005e5;   /* atFunctions value */
  const int imax=50;   /* Maximum number of iterations to solve Kepler equation */
  const double eps=10e-15;  /* Tolerance for terminating Kepler iteration */

  int i=0;    /* Loop index */
  double error=0.0, deltau=0.0, d__1=0.0;  /* Intermediate variable in solution to Kepler equation */

  double a = 0.0;       /* semi-major axis (km) */
  double b = 0.0;       /* semi-minor axis (km) */
  double e = 0.0;       /* eccentricity */
  double u = 0.0;       /* eccentric anomaly (rad) */
  double cosu = 0.0, sinu = 0.0;
  double M_dot = 0.0;   /* 2 pi / T, where T = 2 pi a**1.5 / sqrt(GM) */
  double u_dot = 0.0;   /* u_dot * (1 - e cos(u)) = M_dot, from Kepler equation */

  ORBPOSITION xys;
  ORBVELOCITY vxys;

  double g = 0.0;
  double ai = 0.0;      /* I:  Inclination (rad) */
  double ob = 0.0;      /* AN: Right Ascension of Ascending Node (rad) */
  double ol = 0.0;      /* AP: Argument of Perigee, angle from ascending node (rad) */
  double q1=0.0, q2=0.0, q3=0.0, q4=0.0, q5=0.0, q6=0.0, qq1=0.0, qq2=0.0;  /* Intermediate values */

  int converged = 0;

  setOrbPositionToZero(&xys);
  setOrbVelocityToZero(&vxys);

  a = kep->a;
  e = kep->e;
  b = a * sqrt(1 - e*e);

  /* Equivalent to atKepler(kp->MA * DEG2RAD, e, &u); */
  g = kep->ma;
  u = g;
  if (0 != g) {
    for (i=0; i<imax; i++) {
      deltau = (g - u + e * sin(u)) / (1. - e * cos(u));
      u += deltau;
      error = (d__1 = deltau / u, fabs(d__1));
      if (error < eps) {
        converged = 1;
        break;
      }
    }
    if (0 == converged) {
      /* Not converged */
    }
  }

  cosu = cos(u);
  sinu = sin(u);

  ai = kep->i;
  ob = kep->an;
  ol = kep->ap;

  xys.p[0] = a * (cosu - e);
  xys.p[1] = b * sinu;
  xys.p[2] = 0.0;
/*      xys[2] = 0.0;*/

/* copied from atOrbPlane() */
  q1 = cos(ob);
  q2 = sin(ob);
  q3 = cos(ai);
  q4 = sin(ai);
  q5 = cos(ol);
  q6 = sin(ol);
  qq1 = xys.p[0] * q5 - xys.p[1] * q6;
  qq2 = xys.p[0] * q6 + xys.p[1] * q5;
  pos->p[0] = q1 * qq1 - q2 * (qq2 * q3 /*- q4 * xys->p[2]*/);
  pos->p[1] = q2 * qq1 + q1 * (qq2 * q3 /*- q4 * xys->p[2]*/);
  pos->p[2] = q4 * qq2 /*+ q3 * xys[2]*/;
/* end of atOrbPlane() */

#ifdef DEBUG
  printf ("%25.15e %25.15e %25.15e\n", pos->p[0], pos->p[1], pos->p[2]);
#endif

  M_dot = sqrt(GM / a) / a;
  u_dot = M_dot / (1 - e*cosu);
  vxys.v[0] = - a * sinu * u_dot;
  vxys.v[1] = b * cosu * u_dot;
  vxys.v[2] = 0.0;
  qq1 = vxys.v[0] * q5 - vxys.v[1] * q6;
  qq2 = vxys.v[0] * q6 + vxys.v[1] * q5;
  vel->v[0] = q1 * qq1 - q2 * (qq2 * q3 /*- q4 * vxys->v[2]*/);
  vel->v[1] = q2 * qq1 + q1 * (qq2 * q3 /*- q4 * vxys->v[2]*/);
  vel->v[2] = q4 * qq2 /*+ q3 * vxys->v[2]*/;
}

/* ---------------------------------------------------------------------- */

int findOrbPositionInGenOrbFile(GENORBFILE* file, ORBPOSITION* orbpos, double time)
{
  int found = 0; /* Flag if the orbital position at the input time is found. */
  double dt = 0.0; /* Time since previous orbit file record. */
  double t_interval = 0.0; /* Time interval between two successive orbit file records. */
  double tfrac = 0.0; /* Fraction of t_interval since previous orbit file record. */
  double dtacc = 0.0; /* Time interval containing three successive orbit file records. */
  ORBVELOCITY orbvel;
  ORBKEPLERIAN orbkep;
  

  /* Search for the orbit table rows to use for interpolation. */
  findTimeInGenOrbFile(file, time);

  /* Check if a new pair of velocities needs to be read. */
  if(0 == file->last_search_is_reliable) {
    /* Read or compute the two velocity vectors and mark the search as reliable. */
    if ((ORBF_KEPLERIAN == file->orb_format_num) ||
	(ORBF_KEPLERIAN_NODUP == file->orb_format_num)) {
      readOrbKeplerianFromGenOrbFile(file, file->search_kep0, file->search_row);
    } else {
      readOrbPositionFromGenOrbFile(file, file->search_pos0, file->search_row);
      if (ORBI_TAYLOR == file->interp_method_num) {
	if ( (file->search_row > 1) && (file->search_row < file->num_rows) ){
	  readOrbPositionFromGenOrbFile(file, file->search_pos0, file->search_row+1);
	  readOrbVelocityFromGenOrbFile(file, file->search_vel0, file->search_row+1);
	} else {
	  readOrbPositionFromGenOrbFile(file, file->search_pos0, file->search_row);
	  readOrbVelocityFromGenOrbFile(file, file->search_vel0, file->search_row);
	} 

      }
    }
    if(file->num_rows > 1) {
      /* Get the orbital velocity from the next row if there is another row. */
      if (file->search_row < file->num_rows) {
	if ( (ORBF_KEPLERIAN == file->orb_format_num) || (ORBF_KEPLERIAN_NODUP == file->orb_format_num)) {
          readOrbKeplerianFromGenOrbFile(file, file->search_kep1, file->search_row + 1);
        } else {
	  if (file->search_row > 1) {
	    readOrbPositionFromGenOrbFile(file, file->search_pos1, file->search_row + 1);
	  } else {
	    readOrbPositionFromGenOrbFile(file, file->search_pos1, file->search_row);
	  } 

	  if ( (ORBI_TAYLOR == file->interp_method_num) && (file->search_row+1 < file->num_rows) ) { 
            readOrbVelocityFromGenOrbFile(file, file->search_vel1, file->search_row + 2); 
          }
        }
      }
      if (ORBI_TAYLOR == file->interp_method_num) {
        if (1 != file->search_row) {
          readOrbVelocityFromGenOrbFile(file, file->search_velm1, file->search_row);
	  readOrbVelocityFromGenOrbFile(file, file->search_velm2, file->search_row-1);
          readOrbPositionFromGenOrbFile(file, file->search_posm1, file->search_row);
        }
      }
    }
    
    file->last_search_is_reliable = 1;
  }

  /* Use position for previous time point.  There are two cases in which this
   * method is used even if another method is specified:  (1) file is positioned
   * at the last row, and (2) file only has one row.  Actually (2) is a subset
   * of (1), but it doesn't hurt to check.  */

  found = 0;
  if (ORBI_NEAREST == file->interp_method_num || file->num_rows == file->search_row 
    || 1 == file->num_rows) {
    if ( (ORBF_KEPLERIAN == file->orb_format_num) || (ORBF_KEPLERIAN_NODUP == file->orb_format_num)){
      computePosVelFromKeplerian(orbpos, &orbvel, file->search_kep0);
    } else {
      *orbpos = *(file->search_pos0);
    }
    found = 1;
  }

  /* Linearly interpolate to obtain the position.  There is one case in which
   * this method is used even if the Taylor expansion method is specified:
   * file positioned at the first row. */

  if (0 == found) {
    if (ORBI_WEIGHTED == file->interp_method_num || 
	1 == file->search_row || 
	ORBF_KEPLERIAN == file->orb_format_num ||
	ORBF_KEPLERIAN_NODUP == file->orb_format_num) {

      t_interval = file->search_time1 - file->search_time0;
      dt = (time - file->search_time0);

      if (0 < t_interval) {
        tfrac = dt / t_interval;
        if ( (ORBF_KEPLERIAN == file->orb_format_num) || (ORBF_KEPLERIAN_NODUP == file->orb_format_num)){
          interpolateOrbKeplerian(&orbkep, file->search_kep0, file->search_kep1, tfrac);
          computePosVelFromKeplerian(orbpos, &orbvel, &orbkep);
        } else {
          interpolateOrbPosition(orbpos, file->search_pos0, file->search_pos1, tfrac);
        }
      } else {  /* 0 >= dt, so fail over to ORBI_NEAREST */
        if ( (ORBF_KEPLERIAN == file->orb_format_num) || (ORBF_KEPLERIAN_NODUP == file->orb_format_num)){
          computePosVelFromKeplerian(orbpos, &orbvel, file->search_kep0);
        } else {
          *orbpos = *(file->search_pos0);
        }
      }
      found = 1;
    }
  }

  /* Use Taylor expansion method to interpolate position. */

  if (0 == found) {
    if (ORBI_TAYLOR == file->interp_method_num) {
      
      if (file->search_timep1 != -1.0 && file->search_timem1 != -1.0) { 
	dt = (time - file->search_time1); 
	dtacc = file->search_timep1 - file->search_time0; 
	interpolateOrbPositionTaylor(orbpos, file->search_pos0, file->search_velm1, 
				     file->search_vel0, file->search_vel1, dt, dtacc); 
      } else { 
	if (file->search_timep1 == -1.0) { 
	  dt = (time - file->search_time1); 
	  dtacc = file->search_time1 - file->search_time0; 
	  interpolateOrbPositionTaylor(orbpos, file->search_pos0, file->search_velm1, 
				       file->search_vel0, file->search_vel1, dt, dtacc); 
	} 
	if (file->search_timem1 == -1.0) { 
	  dt = (time - file->search_time1); 
	  dtacc = file->search_timep1 - file->search_time0; 
	  interpolateOrbPositionTaylor(orbpos, file->search_pos0, file->search_vel0, 
				       file->search_vel0, file->search_vel1, dt, dtacc); 
	} 
      } 
      
      found = 1;
    }
  }
  
  return found;

}

/* ---------------------------------------------------------------------- */

int findOrbVelocityInGenOrbFile(GENORBFILE* file, ORBVELOCITY* orbvel, double time)
{
  int found = 1; /* Flag if the orbital velocity at the input time is found. */
  double dt = 0.0; /* Time since previous orbit file record. */
  double t_interval = 0.0; /* Time interval between two successive orbit file records. */
  double tfrac = 0.0; /* Fraction of t_interval since previous orbit file record. */
  ORBPOSITION orbpos;
  ORBKEPLERIAN orbkep;

  /* Search for the orbit table rows to use for interpolation. */
  findTimeInGenOrbFile(file, time);

  /* Check if a new pair of velocities needs to be read. */
  if(!file->last_search_is_reliable) {
    /* Read or compute the two velocity vectors and mark the search as reliable. */
    if ( (ORBF_KEPLERIAN == file->orb_format_num) || (ORBF_KEPLERIAN_NODUP == file->orb_format_num)){
      readOrbKeplerianFromGenOrbFile(file, file->search_kep0, file->search_row);
    } else {
      readOrbVelocityFromGenOrbFile(file, file->search_vel0, file->search_row);
    }
    if(file->num_rows > 1) {
      /* Get the orbital velocity from the next row if there is another row. */
      if ( (ORBF_KEPLERIAN == file->orb_format_num) || (ORBF_KEPLERIAN_NODUP == file->orb_format_num)){
        readOrbKeplerianFromGenOrbFile(file, file->search_kep1, file->search_row + 1);
      } else {
        readOrbVelocityFromGenOrbFile(file, file->search_vel1, file->search_row + 1);
      }
    }
    
    file->last_search_is_reliable = 1;
  }

  /* Use position for previous time point.  There are two cases in which this
   * method is used even if another method is specified:  (1) file is positioned
   * at the last row, and (2) file only has one row.  Actually (2) is a subset
   * of (1), but it doesn't hurt to check.  */

  found = 0;
  if (ORBI_NEAREST == file->interp_method_num || file->num_rows == file->search_row 
    || 1 == file->num_rows) {
    if ( (ORBF_KEPLERIAN == file->orb_format_num) || (ORBF_KEPLERIAN_NODUP == file->orb_format_num)){
      computePosVelFromKeplerian(&orbpos, orbvel, file->search_kep0);
    } else {
      *orbvel = *(file->search_vel0);
    }
    found = 1;
  }

  /* Linearly interpolate the velocities. */
  if (0 == found) {
    t_interval = file->search_time1 - file->search_time0;
    if(t_interval > 0) {
      dt = time - file->search_time0;
      tfrac = dt/t_interval;
      if ( (ORBF_KEPLERIAN == file->orb_format_num) || (ORBF_KEPLERIAN_NODUP == file->orb_format_num)){
        interpolateOrbKeplerian(&orbkep, file->search_kep0, file->search_kep1, tfrac);
        computePosVelFromKeplerian(&orbpos, orbvel, &orbkep);
      } else {
        interpolateOrbVelocity(orbvel, file->search_vel0, file->search_vel1, tfrac);
      }
    } else {
      *orbvel = *(file->search_vel0);
    }
    found = 1;
  }

  /* Return if the orbital velocity was found in the orbit file. */
  return found;
}

/* ---------------------------------------------------------------------- */

int findSplitOrbVelocityInGenOrbFile(GENORBFILE* file, double* v_mag, double* v_unit, double time)
{
  ORBVELOCITY* orbvel = allocateOrbVelocity();
  int found = findOrbVelocityInGenOrbFile(file, orbvel, time);
  
  if(found) {
    *v_mag = getOrbVelocityMagnitude(orbvel);
    getOrbVelocityUnitVector(orbvel, v_unit);
  } else {
    *v_mag = 0.;
    v_unit[0] = 0.;
    v_unit[1] = 0.;
    v_unit[2] = 0.;
  }

#ifdef DEBUG
  printf("v_mag=%25.15e v_unit=%25.15e %25.15e %25.15e\n", *v_mag, v_unit[0], v_unit[1], v_unit[2]);
#endif
  
  destroyOrbVelocity(orbvel);

  return found;
}

/* ---------------------------------------------------------------------- */

void interpolateOrbPosition(ORBPOSITION* pos_out, ORBPOSITION* pos_start, 
			    ORBPOSITION* pos_stop, double tfrac)
{
  int comp = 0; /* Velocity component index */

  /* Linearly interpolate the velocity components separately. */
  for(comp = 0; comp < 3; comp++) {
    pos_out->p[comp] = pos_start->p[comp] + tfrac * (pos_stop->p[comp] - pos_start->p[comp]);
  }
#ifdef DEBUG
  printf("interpolateOrbPosition: tfrac=%.3f\n", tfrac);
  printf("p start="); printOrbPosition(pos_start, stdout); printf("\n");
  printf("p out  ="); printOrbPosition(pos_out, stdout); printf("\n");
  printf("p stop ="); printOrbPosition(pos_stop, stdout); printf("\n");
#endif
}

/* ---------------------------------------------------------------------- */

void interpolateOrbPositionTaylor(ORBPOSITION* pos_out,
  ORBPOSITION* pos_middle, 
  ORBVELOCITY* vel_start, ORBVELOCITY* vel_middle, ORBVELOCITY* vel_stop,
  double dt, double dtacc) {

  int comp = 0;  /* Velocity component index */
  double scacc = 0.0;  /* Estimate of spacecraft acceleration */
  double dt2 = 0.5*dt*dt;  /* Quadratic term in time */

  /* Interpolate each component separately. */

  for (comp=0; comp<3; comp++) {
    scacc = (vel_stop->v[comp] - vel_start->v[comp])/dtacc;
    pos_out->p[comp] = pos_middle->p[comp] + dt*vel_middle->v[comp] + dt2*scacc;
  }
#ifdef DEBUG
  printf("interpolateOrbPositionTaylor: dt=%.5f dtacc=%.5f\n", dt, dtacc);
  printf("p out  ="); printOrbPosition(pos_out, stdout); printf("\n");
  printf("p mid  ="); printOrbPosition(pos_middle, stdout); printf("\n");
  printf("v start="); printOrbVelocity(vel_start, stdout); printf("\n");
  printf("v mid  ="); printOrbVelocity(vel_middle, stdout); printf("\n");
  printf("v stop ="); printOrbVelocity(vel_stop, stdout); printf("\n");
#endif

}

/* ---------------------------------------------------------------------- */

void interpolateOrbVelocity(ORBVELOCITY* vel_out, ORBVELOCITY* vel_start, 
			    ORBVELOCITY* vel_stop, double tfrac)
{
  int comp = 0; /* Velocity component index */

  /* Linearly interpolate the velocity components separately. */
  for(comp = 0; comp < 3; comp++) {
    vel_out->v[comp] = vel_start->v[comp] + tfrac * (vel_stop->v[comp] - vel_start->v[comp]);
  }
#ifdef DEBUG
  printf("interpolateOrbVelocity: tfrac=%.3f\n", tfrac);
  printf("v start="); printOrbVelocity(vel_start, stdout); printf("\n");
  printf("v out  ="); printOrbVelocity(vel_out, stdout); printf("\n");
  printf("v stop ="); printOrbVelocity(vel_stop, stdout); printf("\n");
#endif
}

/* ---------------------------------------------------------------------- */

/* Translated from weight_ang in Suzaku code. */
double interpolateAngle(double tfrac, double ang_start, double ang_stop) {

  const double MATH_PI = 3.14159265358979323846;  /* CRC Math Tables, 16th Ed. */

  double da = 0, ang_out = 0;

  if ( 0.0 == tfrac ) return ang_start;
  if ( 1.0 == tfrac ) return ang_stop;

  da = ang_stop - ang_start;
  while ( da < -MATH_PI) {     /* e.g. ang_start=359, ang_stop=1 */
    da = da + 2*MATH_PI;
  }
  while ( MATH_PI < da ) {      /* e.g. ang_start=1, ang_stop=359 */
    da = da - 2*MATH_PI;
  }

  ang_out = ang_start + tfrac * da;
  if ( 2*MATH_PI < ang_out ) {
    ang_out -= 2*MATH_PI;
  } else if ( ang_out < 0.0 ) {
    ang_out += 2*MATH_PI;
  }

  return ang_out;
}

/* ---------------------------------------------------------------------- */

/* Translated from weight_kepler in Suzaku code. */
void interpolateOrbKeplerian(ORBKEPLERIAN* kep_out, ORBKEPLERIAN* kep_start, 
			     ORBKEPLERIAN* kep_stop, double tfrac) {
{
  if ( 0.0 == tfrac ) {
    *kep_out = *kep_start;
  } else if ( 1.0 == tfrac ) {
    *kep_out = *kep_stop;
  } else {
    kep_out->a = kep_start->a + tfrac * (kep_stop->a - kep_start->a);
    kep_out->e = kep_start->e + tfrac * (kep_stop->e - kep_start->e);
    kep_out->i = interpolateAngle(tfrac, kep_start->i, kep_stop->i);
    kep_out->an = interpolateAngle(tfrac, kep_start->an, kep_stop->an);
    kep_out->ap = interpolateAngle(tfrac, kep_start->ap, kep_stop->ap);
    kep_out->ma = 
      interpolateAngle( tfrac, kep_start->ap+kep_start->ma, 
        kep_stop->ap+kep_stop->ma) - kep_out->ap;
  }
}

}

/* ---------------------------------------------------------------------- */

void setGenOrbFileExtrapolationLimits(GENORBFILE* file, double margin)
{
  /* Set the extrapolation limits to an amount margin outside the
     tstart and tstop times. */

  /* Currently, expanding the valid search range using margin is
     the only part of extrapolation that is implemented. 
     The parts of this library referring to extrapolation are
     only a hook for future implementation, if needed. */
  file->min_extrapolated_time = file->tstart - margin; 
  file->max_extrapolated_time = file->tstop + margin; 
}

/* ---------------------------------------------------------------------- */

int isInExtrapolatedGenOrbFile(GENORBFILE* file, double time)
{
#ifdef DEBUG
  printf("time=%25.15f\n", time);
  printf("file->min_extrapolated_time=%25.15f\n", file->min_extrapolated_time);
  printf("file->max_extrapolated_time=%25.15f\n", file->max_extrapolated_time);
#endif
  /* Orbit extrapolation is not implemented.
     The parts of this library referring to extrapolation are
     only a hook for future implementation, if needed. */

  /* Return if the input time is between the extrapolation limits. */
  return (time >= file->min_extrapolated_time && time <= file->max_extrapolated_time);
}

/* ====================================================================== */

ORBKEPLERIAN* allocateOrbKeplerian()
{
  /* Return a pointer to an allocated ORBPOSITION structure. */
  return (ORBKEPLERIAN*) malloc(sizeof(ORBKEPLERIAN));
}


/* ---------------------------------------------------------------------- */
void destroyOrbKeplerian(ORBKEPLERIAN* orbkep)
{
  /* Deallocate the structure. */
  free(orbkep);
}

/* ---------------------------------------------------------------------- */

void setOrbKeplerian(ORBKEPLERIAN* orbkep, 
  double kep_a, double kep_e, double kep_i, double kep_an, 
  double kep_ap, double kep_ma)
{
  /* Set the p vector components to the input values. */
  orbkep->a = kep_a;
  orbkep->e = kep_e;
  orbkep->i = kep_i;
  orbkep->an = kep_an;
  orbkep->ap = kep_ap;
  orbkep->ma = kep_ma;
}

/* ---------------------------------------------------------------------- */

void setOrbKeplerianToZero(ORBKEPLERIAN* orbkep)
{
  /* Set the Keplerian orbital elements to zero. */
  setOrbKeplerian(orbkep, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
}

/* ---------------------------------------------------------------------- */

ORBPOSITION* allocateOrbPosition()
{
  /* Return a pointer to an allocated ORBPOSITION structure. */
  return (ORBPOSITION*) malloc(sizeof(ORBPOSITION));
}


/* ---------------------------------------------------------------------- */
void destroyOrbPosition(ORBPOSITION* orbpos)
{
  /* Deallocate the structure. */
  free(orbpos);
}

/* ---------------------------------------------------------------------- */

void setOrbPosition(ORBPOSITION* orbpos, double px, double py, double pz)
{
  /* Set the p vector components to the input values. */
  orbpos->p[0] = px;
  orbpos->p[1] = py;
  orbpos->p[2] = pz;
}

/* ---------------------------------------------------------------------- */

void setOrbPositionToZero(ORBPOSITION* orbpos)
{
  /* Set the orbital velocity vector to zero. */
  setOrbPosition(orbpos, 0.0, 0.0, 0.0);
}

/* ---------------------------------------------------------------------- */

ORBVELOCITY* allocateOrbVelocity()
{
  /* Return a pointer to an allocated ORBVELOCITY structure. */
  return (ORBVELOCITY*) malloc(sizeof(ORBVELOCITY));
}


/* ---------------------------------------------------------------------- */
void destroyOrbVelocity(ORBVELOCITY* orbvel)
{
  /* Deallocate the structure. */
  free(orbvel);
}

/* ---------------------------------------------------------------------- */

void setOrbVelocity(ORBVELOCITY* orbvel, double vx, double vy, double vz)
{
  /* Set the v vector components to the input values. */
  orbvel->v[0] = vx;
  orbvel->v[1] = vy;
  orbvel->v[2] = vz;
}

/* ---------------------------------------------------------------------- */

void setOrbVelocityToZero(ORBVELOCITY* orbvel)
{
  /* Set the orbital velocity vector to zero. */
  setOrbVelocity(orbvel, 0.0, 0.0, 0.0);
}

/* ---------------------------------------------------------------------- */

double getOrbVelocityMagnitude(ORBVELOCITY* orbvel)
{
  /* Return the magnitude of the velocity vector. */
  double* v = orbvel->v;
  return sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
}

/* ---------------------------------------------------------------------- */

int getOrbVelocityUnitVector(ORBVELOCITY* orbvel, double* unit_vector)
{
  /* Calculate the velocity magnitude. */
  double magnitude = getOrbVelocityMagnitude(orbvel);

  /* If the magnitude is zero, the unit vector cannot be calculated,
  so return a nonzero error value. */
  if(magnitude == 0.)
    return 1;

  /* Calculate the unit vector and return zero. */
  unit_vector[0] = orbvel->v[0]/magnitude;
  unit_vector[1] = orbvel->v[1]/magnitude;
  unit_vector[2] = orbvel->v[2]/magnitude;
  return 0;
}

/* ---------------------------------------------------------------------- */

void printOrbPosition(ORBPOSITION* orbpos, FILE* stream)
{
  /* Print the position vector to a stream. */
  fprintf(stream, "[%.15g %.15g %.15g]", orbpos->p[0], orbpos->p[1], orbpos->p[2]);
}

/* ---------------------------------------------------------------------- */

/*
int isOrbVelocityNull(ORBVELOCITY* orbvel)
{
  return (orbvel->v[0] == 0. && orbvel->v[1] == 0. && orbvel->v[2] == 0.);
}
*/

/* ---------------------------------------------------------------------- */

void printOrbVelocity(ORBVELOCITY* orbvel, FILE* stream)
{
  /* Print the velocity vector to a stream. */
  fprintf(stream, "[%.15g %.15g %.15g]", orbvel->v[0], orbvel->v[1], orbvel->v[2]);
}

/* ---------------------------------------------------------------------- */

void printOrbKeplerian(ORBKEPLERIAN* orbkep, FILE* stream)
{
  /* Print the velocity vector to a stream. */
  fprintf(stream, "[a=%.15g e=%.15g i=%.15g an=%.15g ap=%.15g ma=%.15g]", 
    orbkep->a, orbkep->e, orbkep->i, orbkep->an, orbkep->ap, orbkep->ma);
}

/* ====================================================================== */

ORB_FORMAT_INFO** getOrbFormatInfo()
{
  /* Allocate an array of VEL_FORMAT_INFO structure pointers. */
  ORB_FORMAT_INFO** orbinfo = (ORB_FORMAT_INFO**) malloc(NUM_ORB_FORMATS * sizeof(ORB_FORMAT_INFO*));
  ORBFORMAT format = 0; /* Velocity format index. */

  /* Allocate the structures in the array. */
  for(format = 0; format < NUM_ORB_FORMATS; format++)
    orbinfo[format] = (ORB_FORMAT_INFO*) malloc(sizeof(ORB_FORMAT_INFO));
  
  /* Set zeros for the unknown velocity format. */
  orbinfo[ORBF_UNKNOWN]->num_pos_cols = 0;
  orbinfo[ORBF_UNKNOWN]->num_pos_elements = 0;
  orbinfo[ORBF_UNKNOWN]->num_vel_cols = 0;
  orbinfo[ORBF_UNKNOWN]->num_vel_elements = 0;
  orbinfo[ORBF_UNKNOWN]->num_kep_cols = 0;
  orbinfo[ORBF_UNKNOWN]->num_kep_elements = 0;

  /* Format ORBF_COMPONENTS has 6 columns, each with one element. */
  orbinfo[ORBF_COMPONENTS]->num_pos_cols = 3;
  orbinfo[ORBF_COMPONENTS]->num_pos_elements = 1;
  orbinfo[ORBF_COMPONENTS]->num_vel_cols = 3;
  orbinfo[ORBF_COMPONENTS]->num_vel_elements = 1;
  orbinfo[ORBF_COMPONENTS]->num_kep_cols = 0;
  orbinfo[ORBF_COMPONENTS]->num_kep_elements = 0;

  /* Format ORBF_VECTOR has 2 columns, each with 3 elements. */
  orbinfo[ORBF_VECTOR]->num_pos_cols = 1;
  orbinfo[ORBF_VECTOR]->num_pos_elements = 3;
  orbinfo[ORBF_VECTOR]->num_vel_cols = 1;
  orbinfo[ORBF_VECTOR]->num_vel_elements = 3;
  orbinfo[ORBF_VECTOR]->num_kep_cols = 0;
  orbinfo[ORBF_VECTOR]->num_kep_elements = 0;

  /* Format ORBF_COMPONENTS_POS has 3 columns, each with one element. */
  orbinfo[ORBF_COMPONENTS_POS]->num_pos_cols = 3;
  orbinfo[ORBF_COMPONENTS_POS]->num_pos_elements = 1;
  orbinfo[ORBF_COMPONENTS_POS]->num_vel_cols = 0;
  orbinfo[ORBF_COMPONENTS_POS]->num_vel_elements = 0;
  orbinfo[ORBF_COMPONENTS_POS]->num_kep_cols = 0;
  orbinfo[ORBF_COMPONENTS_POS]->num_kep_elements = 0;

  /* Format ORBF_VECTOR_POS has 1 columns with 3 elements. */
  orbinfo[ORBF_VECTOR_POS]->num_pos_cols = 1;
  orbinfo[ORBF_VECTOR_POS]->num_pos_elements = 3;
  orbinfo[ORBF_VECTOR_POS]->num_vel_cols = 0;
  orbinfo[ORBF_VECTOR_POS]->num_vel_elements = 0;
  orbinfo[ORBF_VECTOR_POS]->num_kep_cols = 0;
  orbinfo[ORBF_VECTOR_POS]->num_kep_elements = 0;

  /* Format ORBF_COMPONENTS_VEL has 3 columns, each with one element. */
  orbinfo[ORBF_COMPONENTS_VEL]->num_pos_cols = 0;
  orbinfo[ORBF_COMPONENTS_VEL]->num_pos_elements = 0;
  orbinfo[ORBF_COMPONENTS_VEL]->num_vel_cols = 3;
  orbinfo[ORBF_COMPONENTS_VEL]->num_vel_elements = 1;
  orbinfo[ORBF_COMPONENTS_VEL]->num_kep_cols = 0;
  orbinfo[ORBF_COMPONENTS_VEL]->num_kep_elements = 0;

  /* Format ORBF_VECTOR_VEL has 1 columns with 3 elements. */
  orbinfo[ORBF_VECTOR_VEL]->num_pos_cols = 0;
  orbinfo[ORBF_VECTOR_VEL]->num_pos_elements = 0;
  orbinfo[ORBF_VECTOR_VEL]->num_vel_cols = 1;
  orbinfo[ORBF_VECTOR_VEL]->num_vel_elements = 3;
  orbinfo[ORBF_VECTOR_VEL]->num_kep_cols = 0;
  orbinfo[ORBF_VECTOR_VEL]->num_kep_elements = 0;

  /* Format ORBF_KEPLERIAN has 6 columns, each with one element. */
  orbinfo[ORBF_KEPLERIAN]->num_pos_cols = 0;
  orbinfo[ORBF_KEPLERIAN]->num_pos_elements = 0;
  orbinfo[ORBF_KEPLERIAN]->num_vel_cols = 0;
  orbinfo[ORBF_KEPLERIAN]->num_vel_elements = 0;
  orbinfo[ORBF_KEPLERIAN]->num_kep_cols = 6;
  orbinfo[ORBF_KEPLERIAN]->num_kep_elements = 1;

  /* Format ORBF_KEPLERIAN_NODUP has 6 columns, each with one element. */
  orbinfo[ORBF_KEPLERIAN_NODUP]->num_pos_cols = 0;
  orbinfo[ORBF_KEPLERIAN_NODUP]->num_pos_elements = 0;
  orbinfo[ORBF_KEPLERIAN_NODUP]->num_vel_cols = 0;
  orbinfo[ORBF_KEPLERIAN_NODUP]->num_vel_elements = 0;
  orbinfo[ORBF_KEPLERIAN_NODUP]->num_kep_cols = 6;
  orbinfo[ORBF_KEPLERIAN_NODUP]->num_kep_elements = 1;

  
  /* Return the array of structures. */
  return orbinfo;
}

/* ====================================================================== */
/* Revision log
   $Log: genorbfile.c,v $
   Revision 1.26  2016/08/02 15:17:14  rshill
   Added comments about the fact that extrapolation of the orbit
   is not actually implemented.

   Revision 1.25  2016/03/22 20:35:54  driethmi
   Added lines:

     free(file->search_posm1);
     free(file->search_velm2);

   at the end of closeGenOrbFile(), to correct memory leak.

   Revision 1.24  2015/12/29 17:32:21  rshill
   Added check for empty input file.

   Revision 1.23  2015/12/14 21:52:09  rshill
   Improved error handling for time search outside time range
   of orbit file.  Use extrapolated time range, which was ignored before (extrapolated
   time range is initialized equal to the actual time range).

   Revision 1.22  2015/12/08 19:13:01  driethmi
   Corrected additional minor bugs related to reading past the end of the orbit
   file during the TAYLOR interpolation method.

   Revision 1.21  2015/11/19 16:00:37  driethmi
   Corrected additional issues reading near the beginning or end of file when
   in TAYLOR mode.

   Revision 1.20  2015/11/19 15:43:42  driethmi
   Corrected an error in reading the orbit files when a time in the event file
   matches exactly a row in the orbit file, by adding an additional check to
   make sure that the routine doesn't search before the beginning or after
   the end of the orbit file.

   Revision 1.19  2015/11/18 16:30:06  driethmi
   In findOrbPositionInGenOrbFile(), replaced ORBI_TAYLOR conditional with conditional
   to avoid FITSIO error when reading past end of file.

   Revision 1.18  2015/11/17 21:51:34  driethmi
   If we find more than two consecutive duplicate times in the orbit file, within
   the bounds of TSTART and TSTOP that we use, then print an error message and
   fatally exit the code.

   Revision 1.17  2015/11/17 20:16:32  driethmi
   Added allowances for ORBF_KEPLERIAN_NODUP wherever ORBF_KEPLERIAN already
   exists.  Only difference occurs where weights are concerned.

   Revision 1.16  2015/11/13 18:59:29  driethmi
   If the orbit format is ORBF_KEPLERIAN_NODUP, then we're in a mode that forbids
   duplicate rows in the orbit file.  In this case, automatically set all weights
   to 1.0, and don't bother looking for the weights column.

   Revision 1.15  2015/10/30 18:09:20  driethmi
   Removed errant printf statements.

   Revision 1.14  2015/10/30 17:55:35  driethmi
   Modified to handle duplicate time rows.  Also modified so that if interpolation
   search extends beyond the bounds of the orbit file, then set a flag value to -1.

   Revision 1.13  2015/10/08 21:53:13  rshill
   Added meaningful error messages for file opening failures.

   Revision 1.12  2015/10/02 14:05:18  driethmi
   Corrected typo: Line 1111, +1 should be +2.

   Revision 1.11  2015/10/01 19:11:00  driethmi
   Implemented genorbfile changes as prescribed by HAK in the coordevt TRF,
   15-09-23.

   Revision 1.10  2015/09/15 15:27:05  driethmi
   Added allocation and free for structure member GENORBFILE file->search_velm1

   Revision 1.9  2015/09/11 16:10:41  driethmi
   Added "else" block to handle case where interpolation method is not TAYLOR.

   Revision 1.8  2015/08/05 18:12:48  rshill
   Improved error reporting form MJDREF keywords.

   Revision 1.7  2015/06/19 23:38:26  rshill
   Added Taylor interpolation method; fixed some inconsistencies in
   variable names and also between findOrbPositionInGenOrbFile and findOrbVelocityInGenOrbFile.

   Revision 1.6  2015/05/11 20:03:25  rshill
   Previous fix to handling of last endpoint of time interval was incorrect - reverting that.
   Fixing call in DEBUG printf statements.

   Revision 1.5  2015/04/13 18:07:01  rshill
   Fixed endpoint problem with time range.

   Revision 1.4  2015/04/03 22:55:36  rshill
   Automatic processing of orbital units: m vs. km, rad vs. deg.

   Revision 1.3  2015/03/18 23:04:42  rshill
   Support orbital position and Keplerian elements.

   Revision 1.2  2014/07/16 23:00:35  rshill
   Added some DEBUG output.

   Revision 1.1  2014/07/14 19:56:05  rshill
   General orbit file handling

*/
