/// \file gtiinvert.cxx
/// \brief Create a new extension in a GTI FITS file that covers time spans NOT covered by the input GTI.
/// \author David Riethmiller                                                                                                                       
/// \date $Date: 2016/04/13 14:28:36 $                                                                                                           

/* This tool takes an input FITS file specifying Good Time Invervals (GTI), and produces an output
   FITS GTI file giving time intervals in between those from the input file, i.e. the output FITS
   file accounts for all the time not covered in the input FITS file.
*/

#define AHLABEL tool_gtiinvert
#define AHCVSID "$Id: gtiinvert.cxx,v 1.17 2016/04/13 14:28:36 mwitthoe Exp $"
#define TOOLTAG "$Name: heasoft6_20_20170113 $"
#define NUM_PARAM 13  // Number of parameters in par file

#include "gtiinvert.h"  // other required includes found within gtiinvert.h, to avoid redundant include statements

#include "sys/stat.h"       // lstat, stat
#include <string.h>

/* ----------------------------------------------------------------------------------- */
int main(int argc, char ** argv){

  PARAMS par;                  // structure to hold input parameters
  double tstart = 0.0;         // start time
  double tstop = 0.0;          // stop time

  fitsfile * ahffp_in = 0;     // GTI in
  fitsfile * ahffp_out = 0;    // GTI out

  int errstatus = 0;           // Error status flag

  errstatus = getPars(par, argc, argv);
  if (errstatus){
    printf("Error in getPars, status %d, exiting.\n",errstatus);
    return errstatus;
  }

  errstatus = initialize(&par, tstart, tstop, &ahffp_in, &ahffp_out);
  if (errstatus){
    printf("Error in initialize, status %d, exiting.\n",errstatus);
    cleanup_error(&ahffp_in,&ahffp_out);
    return errstatus;
  }

  errstatus = doWork(&par, tstart, tstop, &ahffp_in, &ahffp_out);
  if (errstatus){
    printf("Error in doWork, status %d, exiting.\n",errstatus);
    cleanup_error(&ahffp_in,&ahffp_out);
    return errstatus;
  }

  errstatus = finalize(par, &ahffp_in, &ahffp_out);
  if (errstatus){
    printf("Error in finalize, status %d, exiting.\n",errstatus);
    return errstatus;
  }

  if (par.debug){
    // If debug is set, execute a routine that dumps the input GTI and output GTI inversion
    // into a gnuplot script, to see a visual representation of the results.  The output gnuscript
    // is named vis.gnu, and is executed via "gnuplot vis.gnu".  Executing this gnuplot script
    // produces a postscript file called vis.ps.
    visualize(&par);
  }
  
  return 0;
}


/* ----------------------------------------------------------------------------------- */

int getPars(PARAMS & par, int argc, char ** argv){

  // temp strings
  char * temp_infile = 0;
  char * temp_outfile = 0;
  char * temp_outext = 0;
  char * temp_tstart = 0;
  char * temp_tstop = 0;
  char * temp_logfile = 0;
  char * temp_mode = 0;

  // temp booleans
  char temp_margingti = 0;
  char temp_clobber = 0;
  char temp_debug = 0;
  char temp_history = 0;

  // Initialize APE
  int status = ape_trad_init(argc, argv);
  if (eOK != status) {
    printf("ERROR: ape_trad_init failed!\n");
    return status;
  }

  // Begin reading parameters with APE
  if (ape_trad_query_string("infile",&temp_infile) != 0){
      printf("getPars(): problem reading infile.\n"); status=-1; }
  if (ape_trad_query_string("outfile",&temp_outfile) != 0){
      printf("getPars(): problem reading outfile.\n"); status=-1; }
  if (ape_trad_query_string("outext",&temp_outext) != 0){
      printf("getPars(): problem reading outext.\n"); status=-1; }
  if (ape_trad_query_bool("margingti",&temp_margingti) != 0){
      printf("getPars(): problem reading margingti.\n"); status=-1; }
  if (ape_trad_query_string("tstart",&temp_tstart) != 0){
      printf("getPars(): problem reading tstart.\n"); status=-1; }
  if (ape_trad_query_string("tstop",&temp_tstop) != 0){
      printf("getPars(): problem reading tstop.\n"); status=-1; }
  if (ape_trad_query_double("dt",&par.dt) != 0){ 
    printf("getPars(): problem reading dt.\n"); status=-1; }
  if (ape_trad_query_bool("clobber",&temp_clobber) != 0){ 
    printf("getPars(): problem reading clobber.\n"); status=-1; }
  if (ape_trad_query_int("chatter",&par.chatter) != 0) {
    printf("getPars(): problem reading chatter.\n"); status=-1; }
  if (ape_trad_query_string("logfile",&temp_logfile) != 0) {
    printf("getPars(): problem reading logfile.\n"); status=-1; }
  if (ape_trad_query_bool("debug",&temp_debug) != 0) {
    printf("getPars(): problem reading debug.\n"); status=-1; }
  if (ape_trad_query_bool("history",&temp_history) != 0) {
    printf("getPars(): problem reading history.\n"); status=-1; }
  if (ape_trad_query_string("mode",&temp_mode) != 0) {
    printf("getPars(): problem reading mode.\n"); status=-1; }
  
  if (status != 0) return status;

  // Copy the APE strings into the par structure
  par.infile = temp_infile;
  par.outfile = temp_outfile;
  par.outext = temp_outext;
  par.tstart = temp_tstart;
  par.tstop = temp_tstop;
  par.logfile = temp_logfile;
  par.mode = temp_mode;
  
  // Convert temp strings to par boolean
  par.margingti = temp_margingti;
  par.clobber = temp_clobber;
  par.debug = temp_debug;
  par.history = temp_history;

  // if outfile is preceded by an exclamation point, remove it and set clobber
  if (temp_infile[0] == '!') {
    par.infile.erase(0,1);
    temp_clobber=1;
  }

  // Clean up allocated strings
  free(temp_infile);
  free(temp_outfile);
  free(temp_outext);
  free(temp_tstart);
  free(temp_tstop);
  free(temp_logfile);
  free(temp_mode);
  
  // Print out some diagnostic info if debug is set
  if (par.debug){
    printf("  infile = %s\n",par.infile.c_str());
    printf("  outfile = %s\n",par.outfile.c_str());
    printf("  outext = %s\n",par.outext.c_str());
    if (par.margingti){
      printf("  margingti = YES\n");
    } else {
      printf("  margingti = NO\n");
    }
    printf("  tstart = %s\n",par.tstart.c_str());
    printf("  tstop = %s\n",par.tstop.c_str());
    printf("  dt = %f\n",par.dt);
  }

  return 0;
}


/* ------------------------------------------------------------------------------------- */

int initialize(PARAMS * par,
               double & tstart,
               double & tstop,
               fitsfile ** ahffp_in, 
               fitsfile ** ahffp_out){

  int fstat = 0;           // cfitsio status
  bool okay=true;
  char* comment=0;

  // for checking/comparing input/output files
  char rootinfile[FLEN_FILENAME];        // hold infile w/o extended syntax
  char rootoutfile[FLEN_FILENAME];       // hold outfile w/o extended syntax
  int exists=0;                          // output of fits_file_exists

  // the output file cannot be the same as the input
  if (0 != fits_parse_rootname((char*)par->infile.c_str(),rootinfile,&fstat)) {
    printf("ERROR: could not understand infile: %s\n",par->infile.c_str());
    return fstat;
  }
  if (0 != fits_parse_rootname((char*)par->outfile.c_str(),rootoutfile,&fstat)) {
    printf("ERROR: could not understand outfile: %s\n",par->outfile.c_str());
    return fstat;
  }
  if (0 != fits_file_exists(rootoutfile,&exists,&fstat)) {
    printf("ERROR: failed to check if outfile exists: %s\n",par->outfile.c_str());
    return fstat;
  }
  if (1 == exists) {     // if outfile exists, check if it is the same as infile
    struct stat fileStat1, fileStat2;
    stat(rootinfile,&fileStat1);
    stat(rootoutfile,&fileStat2);
    if (fileStat1.st_ino == fileStat2.st_ino) {
      printf("ERROR: cannot have outfile=infile\n");
      return 1;
    }
  }

  // open input file
  if (0 != fits_open_file(ahffp_in, par->infile.c_str(), READONLY, &fstat)){
    printf("ERROR: could not open file %s; fstat = %d\n",par->infile.c_str(),fstat);
    return fstat;
  }

  // if opened into the primary extension, move to extension GTI
  int hdunum=0;
  if (1 == fits_get_hdu_num(*ahffp_in,&hdunum)) {
    printf("moving to GTI extension of input file\n");
    if (0 != fits_movnam_hdu(*ahffp_in, BINARY_TBL, (char *)"GTI", 0, &fstat)){
      printf("ERROR: could not move to GTI extension; fstat = %d\n",fstat);
      return fstat;
    }
  }

  // if outfile exists and clobber is set, then remove existing outfile
  if (file_exists(par->outfile.c_str())) {
    if (par->clobber) {
      remove(par->outfile.c_str());
    } else {
      printf("ERROR: outfile already exists; set clobber to overwrite\n");
      return 1;
    }
  }

  // create output file
  printf("Creating output file %s\n",par->outfile.c_str());
  if (0 != fits_create_file(ahffp_out, par->outfile.c_str(), &fstat)) {
    printf("ERROR: cannot create output file, %s; fstat = %d\n",par->outfile.c_str(),fstat);
    return fstat;
  }

  // create output extension with two columns: START (1D) & STOP (1D)
  int tfields=2;
  char* ttype[2];
  ttype[0]=(char*)"START";
  ttype[1]=(char*)"STOP";
  char* tform[3];
  tform[0]=(char*)"1D";
  tform[1]=(char*)"1D";
  char* tunit[3];
  tunit[0]=(char*)"sec";
  tunit[1]=(char*)"sec";
  char* extname=(char*)par->outext.c_str();
  if (0 != fits_create_tbl(*ahffp_out,BINARY_TBL,1,tfields,ttype,tform,tunit,extname,&fstat)) {
    printf("ERROR: failed to create output extension; fstat = %d\n",fstat);
    return fstat;
  }

  // copy timing keywords from infile
  okay=true;
  if (0 != copy_keyword(*ahffp_in,*ahffp_out,(char*)"HDUCLAS1")) okay=false;
  if (0 != copy_keyword(*ahffp_in,*ahffp_out,(char*)"TIMESYS")) okay=false;
  if (0 != copy_keyword(*ahffp_in,*ahffp_out,(char*)"MJDREFI")) okay=false;
  if (0 != copy_keyword(*ahffp_in,*ahffp_out,(char*)"MJDREFF")) okay=false;
  if (0 != copy_keyword(*ahffp_in,*ahffp_out,(char*)"TIMEUNIT")) okay=false;
  if (0 != copy_keyword(*ahffp_in,*ahffp_out,(char*)"TIMEREF")) okay=false;
  if (0 != copy_keyword(*ahffp_in,*ahffp_out,(char*)"TASSIGN")) okay=false;
  if (0 != copy_keyword(*ahffp_in,*ahffp_out,(char*)"GPSOFFET")) okay=false;
  if (0 != copy_keyword(*ahffp_in,*ahffp_out,(char*)"CLOCKAPP")) okay=false;
  if (0 != copy_keyword(*ahffp_in,*ahffp_out,(char*)"TSTART")) okay=false;
  if (0 != copy_keyword(*ahffp_in,*ahffp_out,(char*)"TSTOP")) okay=false;
  if (0 != copy_keyword(*ahffp_in,*ahffp_out,(char*)"TELAPSE")) okay=false;
  if (0 != copy_keyword(*ahffp_in,*ahffp_out,(char*)"ONTIME")) okay=false;
  if (0 != copy_keyword(*ahffp_in,*ahffp_out,(char*)"DATE-OBS")) okay=false;
  if (0 != copy_keyword(*ahffp_in,*ahffp_out,(char*)"DATE-END")) okay=false;
  if (0 != copy_keyword(*ahffp_in,*ahffp_out,(char*)"TIME-OBS")) okay=false;
  if (0 != copy_keyword(*ahffp_in,*ahffp_out,(char*)"TIME-END")) okay=false;
  if (!okay) {
    printf("ERROR: failed to copy keywords\n");
    return 1;
  }

  // determine start and stop for margin GTI in output, if applicable
  if (par->margingti) {

    if (par->tstart == "DEFAULT") {
      if ( fits_read_key_dbl(*ahffp_in, "TSTART", &tstart, comment, &fstat) != 0) {
        printf("No TSTART keyword in input file header, will use first start time.\n");
        tstart = -1;
        fstat = 0;
      }
    } else {
      tstart=atof(par->tstart.c_str());
    }

    if (par->tstop == "DEFAULT") {
      if ( fits_read_key_dbl(*ahffp_in, "TSTOP", &tstop, comment, &fstat) != 0) {
        printf("No TSTOP keyword in input file header, will use last stop time.\n");
        tstop=-1;      // will not make last margin GTI
        fstat = 0;
      }
    } else {
      tstop=atof(par->tstop.c_str());
    }

  } else {     // par->margingti = no
    tstart=-1;
    tstop=-1;
  }

  return fstat;
}


/* ------------------------------------------------------------------------------ */

int doWork(PARAMS * par,          // parameter structure
      double tstart,              // start time
      double tstop,               // stop time
      fitsfile ** ahffp_in,
      fitsfile ** ahffp_out){

  // Internal variables
  int fstat = 0;        // FITS error status
  long nrows_in = 0;    // Number of rows in input GTI
  long nrows_out = 0;   // Number of row sin output GTI
  int colnum_start=0;   // Column number of "START"
  int colnum_stop=0;    // Column number of "STOP"
  int anynull = 0;      // A null value
  char * comment = 0;   // FITS comment
  int ii = 0;           // Loop index

  // Initialize the keyword values
  double tstart_out=tstart;
  double tstop_out=tstop;

  // Initialize the column value arrays
  double * start_in = 0;
  char * start_in_null = 0;
  double * stop_in = 0;
  char * stop_in_null = 0;
  double * start_out = 0;
  double * stop_out = 0;

  // Get the number of rows to be read in
  if (fits_read_key_lng(*ahffp_in, "NAXIS2", &nrows_in, comment, &fstat) != 0){
    printf("ERROR: could not read NAXIS2 keyword: %d.\n",fstat);
    return fstat;
  }

  nrows_out = nrows_in + 1;    // max output rows

  // Allocate the arrays
  start_in = (double *)calloc(nrows_in, sizeof(double));
  start_in_null = (char *)calloc(nrows_in, sizeof(char));
  stop_in = (double *)calloc(nrows_in, sizeof(double));
  stop_in_null = (char *)calloc(nrows_in, sizeof(char));
  start_out = (double *)calloc(nrows_out, sizeof(double));
  stop_out = (double *)calloc(nrows_out, sizeof(double));

  // Get the input column numbers
  if (0 == fstat) {
    if (fits_get_colnum(*ahffp_in, CASEINSEN, (char *)"START", &colnum_start, &fstat)){
      printf("ERROR: input START column may not exist.\n");
    }
  }
  if (0 == fstat) {
    if (fits_get_colnum(*ahffp_in, CASEINSEN, (char *)"STOP", &colnum_stop, &fstat)){
      printf("ERROR: input STOP column may not exist.\n");
    }
  }

  // Read the columns
  if (0 == fstat) {  
    if (fits_read_colnull_dbl(*ahffp_in, colnum_start, 1, 1, nrows_in, start_in, start_in_null, &anynull, &fstat)){
      printf("ERROR: could not read input START column.\n");
    }
  }
  if (0 == fstat) {
    if (fits_read_colnull_dbl(*ahffp_in, colnum_stop, 1, 1, nrows_in, stop_in, stop_in_null, &anynull, &fstat)){
      printf("ERROR: could not read input STOP column\n");
    }
  }

  // Check that the input GTI is valid, i.e. no negative intervals
  if (0 == fstat) {
    for (ii=0; ii<nrows_in; ii++){
      if (start_in_null[ii] == 1 || stop_in_null[ii] == 1) continue;
      double interval = stop_in[ii] - start_in[ii];
      if (interval < 0.0){
        printf("ERROR: input GTI file %s has negative time interval at row %d.\n",par->infile.c_str(),(ii+1));
        printf("   Please correct input file and re-run gtiinvert.\n");
        fstat=-1;
      }
    }
  }

  // Compute all elements for output arrays; we'll remove negative or zero output intervals later
  int jj=0;
  if (0 == fstat) {
    start_out[jj] = tstart;
    for (ii=0; ii<nrows_in; ii++){
      if (start_in_null[ii] == 1 || stop_in_null[ii] == 1) {
        printf("Row %d: skipping input row with NULL START or STOP\n",(ii+1));
        continue;
      }
      stop_out[jj] = start_in[ii] - par->dt;
      jj++;
      start_out[jj] = stop_in[ii] + par->dt;
    }
    stop_out[jj] = tstop;  // Final margin GTI
    nrows_out=jj+1;
  }

  // Loop over output invervals and remove negative or zero intervals
  if (0 == fstat) {
    for (ii=nrows_out-1; ii>=0; ii--){
      double interval = stop_out[ii] - start_out[ii];
      if (interval <= 0.0 || start_out[ii] < 0.){
        remove_array_element(&start_out, nrows_out, ii);
        remove_array_element(&stop_out, nrows_out, ii);
        nrows_out--;
      }
    }
  }

  // Write the output arrays to file
  if (0 == fstat) {
    printf("Writing %ld output GTI rows\n",nrows_out);
    if (fits_write_col_dbl(*ahffp_out, 1, 1, 1, nrows_out, start_out, &fstat)){
      printf("ERROR: could not write output START data.\n");
    }
  }
  if (0 == fstat) {
    if (fits_write_col_dbl(*ahffp_out, 2, 1, 1, nrows_out, stop_out, &fstat)){
      printf("ERROR: could not write output STOP data.\n");
    }
  }

  // update output TSTART & TSTOP 
  // !!! Note that this may lose information, can't invert the inversion to recover the original!
  if (0 == fstat) {
    if (nrows_out == 0) {
      tstart_out=0.;
      tstop_out=0.;
    } else {
      tstart_out = start_out[0];
      tstop_out = stop_out[nrows_out-1];
    }
  }

  // Write output keywords
  if (0 == fstat) {
    if (fits_modify_key_dbl(*ahffp_out, "TSTART", tstart_out, 14, "Start Time", &fstat)){
      fstat = 0;
      if (fits_write_key_dbl(*ahffp_out, "TSTART", tstart_out, 14, "Start Time", &fstat)){
        printf("ERROR: could not update TSTART keyword.\n");
      }
    }
  }
  if (0 == fstat) {
    if (fits_modify_key_dbl(*ahffp_out, "TSTOP", tstop_out, 14, "Stop Time", &fstat)){
      fstat = 0;
      if (fits_write_key_dbl(*ahffp_out, "TSTOP", tstop_out, 14, "Stop Time", &fstat)){
        printf("ERROR: could not update TSTOP keyword.\n");
      }
    }
  }

  // If TSTART and TSTOP are the same, then output fits file should have no rows
  if (0 == fstat) {
    if (tstart_out == tstop_out){
      printf("Output TSTART and TSTOP are equal; expect output file to have zero rows.\n");
      fits_modify_key_lng(*ahffp_out, "NAXIS2", 0, "number of rows in table", &fstat);
      if (0 != fits_delete_rows(*ahffp_out, 1, nrows_out, &fstat)) {
        printf("ERROR: failed to delete rows after finding TSTART=TSTOP\n");
      }
    }
  }

  // Delete columns that are not named START or STOP.
  // Note 1: this is done after writing the rows rather than before, because
  // fits_delete_col returns an error if trying to delete columns from an
  // empty table (status 107).
  // Note 2: deleting rows backwards (from ncols to 1) in order to preserve
  // the column index.
  if (0 == fstat) {
    int ncols=0;
    fits_get_num_cols(*ahffp_out,&ncols,&fstat);
    for (int icol=ncols; icol >= 1; icol--) {
      char colname[FLEN_CARD]="";
      char templ[FLEN_CARD]="";
      int colnum=0;
      sprintf(templ,"%d",icol);
      fits_get_colname(*ahffp_out,CASEINSEN,templ,colname,&colnum,&fstat);
      if (0 != strcmp("START",colname) && 0 != strcmp("STOP",colname)) {
        printf("Deleting column %s\n",colname);
        if (0 != fits_delete_col(*ahffp_out,icol,&fstat)) {
          printf("ERROR: Failed to delete column %s\n",colname);
          break;
        }
      }
    }
  }

  // Clean up
  free(start_in);
  free(stop_in);
  free(start_out);
  free(stop_out);

  return fstat;
}


/* ------------------------------------------------------------------------------ */

int finalize(PARAMS & par,
              fitsfile ** ahffp_in,
              fitsfile ** ahffp_out){

  int fstat = 0;

  fits_close_file(*ahffp_in, &fstat);
  fits_write_chksum(*ahffp_out,&fstat);
  fits_close_file(*ahffp_out, &fstat);

  printf("\nGTI inversion complete.  Output GTI FITS file: %s\n\n",par.outfile.c_str());

  return fstat;
}


/* ------------------------------------------------------------------------------ */

void cleanup_error(fitsfile ** ahffp_in,
                   fitsfile ** ahffp_out) {
  int fstat=0;
  fits_close_file(*ahffp_in, &fstat);
  fits_delete_file(*ahffp_out, &fstat);
}

/* ------------------------------------------------------------------------------ */

bool file_exists(const char *fileName){
  int status = 0;    /* tracks the status, 0 to continue, anything else to stop */
  struct stat stbuf; /* status buffer */

  status = stat(fileName, &stbuf);
  return (0 == status);
}


/* ------------------------------------------------------------------------------ */

void remove_array_element(double ** array, long n_array, int index_to_remove){

  double * temp_array = 0;
  int ii=0, jj=0;

  // Allocate a temp array one element less than input array
  temp_array = (double *)calloc(n_array-1, sizeof(double));

  // Copy all but excluded element to temp array
  for (ii=0; ii<n_array; ii++)
      if (ii != index_to_remove)  temp_array[jj++] = (*array)[ii];

  // Free and reallocate original array
  free( (*array) );
  *array = (double *)calloc(n_array-1, sizeof(double));

  // Copy temp array to original array
  for (ii=0; ii<n_array-1; ii++)  (*array)[ii] = temp_array[ii];

  // Clean up
  free(temp_array);
}


/* ------------------------------------------------------------------------------ */


void visualize(PARAMS * par){

  // Filter the input GTI and output GTI inversion into a gnuplot script, in order to
  // verify visually that the inversion is correct.

  // Initialize the column values
  double * start_out=0;
  double * stop_out=0;
  double * start_in=0;
  double * stop_in=0;

  // Initialize the FITS pointers
  fitsfile * ahffp_in = 0;
  fitsfile * ahffp_out = 0;

  int fstat = 0;
  long nrows_in = 0;
  long nrows_out = 0;
  int colnum_start=0, colnum_stop=0;
  int anynull = 0;
  char * comment = 0;
  int ii = 0;

  double tstart = 0.0;
  double tstop = 0.0;

  // Re-open the fits files and move to GTI extension
  fits_open_file(&ahffp_in, par->infile.c_str(), READONLY, &fstat);
  fits_movnam_hdu(ahffp_in, BINARY_TBL, (char *)"GTI", 0, &fstat);
  fits_open_file(&ahffp_out, par->outfile.c_str(), READONLY, &fstat);
  fits_movnam_hdu(ahffp_out, BINARY_TBL, (char *)"GTI", 0, &fstat);

  // Get number of rows to read
  fits_read_key_lng(ahffp_in, "NAXIS2", &nrows_in, comment, &fstat);
  fits_read_key_lng(ahffp_out, "NAXIS2", &nrows_out, comment, &fstat);

  // Allocate arrays
  start_in = (double *)calloc(nrows_in, sizeof(double));
  stop_in = (double *)calloc(nrows_in, sizeof(double));
  start_out = (double *)calloc(nrows_out, sizeof(double));
  stop_out = (double *)calloc(nrows_out, sizeof(double));

  // Get column numbers and read arrays
  fits_get_colnum(ahffp_in, CASEINSEN, (char *)"START", &colnum_start, &fstat);
  fits_get_colnum(ahffp_in, CASEINSEN, (char *)"STOP", &colnum_stop, &fstat);
  fits_read_col_dbl(ahffp_in, colnum_start, 1, 1, nrows_in, 0, start_in, &anynull, &fstat);
  fits_read_col_dbl(ahffp_in, colnum_stop, 1, 1, nrows_in, 0, stop_in, &anynull, &fstat);

  fits_get_colnum(ahffp_out, CASEINSEN, (char *)"START", &colnum_start, &fstat);
  fits_get_colnum(ahffp_out, CASEINSEN, (char *)"STOP", &colnum_stop, &fstat);
  fits_read_col_dbl(ahffp_out, colnum_start, 1, 1, nrows_out, 0, start_out, &anynull, &fstat);
  fits_read_col_dbl(ahffp_out, colnum_stop, 1, 1, nrows_out, 0, stop_out, &anynull, &fstat);

  // If the output gnuplot and postscript files already exist, remove them
  if (file_exists("vis.gnu")) remove("vis.txt");
  if (file_exists("vis.ps")) remove("vis.ps");

  // Open the gnuplot file for writing
  FILE *fp = fopen("vis.gnu","w");

  // Get the tstart and tstop keyword values
  fits_read_key_dbl(ahffp_in, "TSTART", &tstart, comment, &fstat);
  fits_read_key_dbl(ahffp_in, "TSTOP", &tstop, comment, &fstat);

  // Write the setup commands
  fprintf(fp,"set output \"vis.ps\"\n");
  fprintf(fp,"set xrange[%f:%f]\n",tstart,tstop);
  fprintf(fp,"set yrange[-10:10]\n");
  fprintf(fp,"set xlabel 'Time (s)'\n");
  fprintf(fp,"set style arrow 1 nohead\n");
  fprintf(fp,"set terminal postscript landscape\n");
  fprintf(fp,"set title 'GTI Inversion'\n");
  fprintf(fp,"set label \"Red: Input GTI\" at graph 0.2, graph 0.7 tc rgb \"red\"\n");
  fprintf(fp,"set label \"Blue: GTI Inversion\" at graph 0.2, graph 0.4 tc rgb \"blue\"\n");
  fprintf(fp,"\n");

  // Set line segments for the input GTI in red
  for (ii=0; ii<nrows_in; ii++){
    fprintf(fp,"set arrow from %f,2 to %f,2 as 1 lt rgb \"red\"\n",start_in[ii],stop_in[ii]);
  }
  fprintf(fp,"\n");

  // Set line segments for output GTI inversion in blue
  for (ii=0; ii<nrows_out; ii++){
    fprintf(fp,"set arrow from %f,1 to %f,1 as 1 lt rgb \"blue\"\n",start_out[ii],stop_out[ii]);
  }

  // Give command to plot all line segments
  fprintf(fp,"\nplot NaN notitle\n");

  fclose(fp);

  // Clean up
  fits_close_file(ahffp_in,&fstat);
  fits_close_file(ahffp_out,&fstat);

  // Tell the user what we've done and how to use it
  printf("\nVisualization script written to vis.gnu.\n");
  printf("Execute \"gnuplot vis.gnu\" to create a plot file of the GTI inversion, vis.ps\n");

}


/* ------------------------------------------------------------------------------ */


int copy_keyword(fitsfile* fpin, fitsfile* fpout, char* keyword) {
  int fstat=0;
  char card[FLEN_CARD];
  fits_read_card(fpin,keyword,card,&fstat);
  if (fstat == KEY_NO_EXIST) return 0;     // skip keywords that do not exist in input file
  if (0 == fstat) fits_update_card(fpout,keyword,card,&fstat);
  return fstat;
}


/* ------------------------------------------------------------------------------ */



/* Revision Log
 * $Log: gtiinvert.cxx,v $
 * Revision 1.17  2016/04/13 14:28:36  mwitthoe
 * gtiinvert: copy HDUCLAS1 keyword from input file
 *
 * Revision 1.16  2016/03/28 08:41:55  mwitthoe
 * skip input rows where START or STOP are NULL
 *
 * Revision 1.15  2015/12/23 17:32:36  mwitthoe
 * gtiinvert: fix bug in checking for outfile=infile
 *
 * Revision 1.14  2015/12/23 16:47:10  mwitthoe
 * gtiinvert: 1) do not copy input header into output extension; instead copy only timing keywords; 2) make sure that allocated memory is freed upon error; 3) delete output file upon error
 *
 * Revision 1.13  2015/12/17 08:36:37  mwitthoe
 * gtiinvert bug-fixes: 1) for output, code assumed START was always column 1 and STOP was always column 2, now use the START/STOP column numbers; 2) if no output GTI, explicitly set TSTART=TSTOP=0
 *
 * Revision 1.12  2015/12/15 01:59:08  mwitthoe
 * gtiinvert: 1) fix bug where extra GTI was being created when the tstart parameter was DEFAULT or no TSTART was in the input file; 2) remove tab characters from source
 *
 * Revision 1.11  2015/12/04 19:31:27  driethmi
 * Corrected some erroneous behavior when TSTART or TSTOP keywords are not present
 * in the input fits header.
 *
 * Revision 1.10  2015/10/13 19:26:50  driethmi
 * If output TSTART and TSTOP are equal, then output fits file now has zero rows.
 *
 * Revision 1.9  2015/10/09 14:49:40  driethmi
 * If ape_trad_init fails it reports the APE error code, instead of status=-1.
 *
 * Revision 1.8  2015/10/09 14:40:27  driethmi
 * Added explicit check for extended bracket syntax, i.e. inputfile[extname]
 *
 * Revision 1.7  2015/10/09 14:31:41  driethmi
 * Modified to always use the output extension name as given.
 *
 * Revision 1.6  2015/10/01 18:04:43  driethmi
 * Set TSTART and TSTOP equal to the output GTI endpoints of the start and stop
 * arrays, respectively.  This may lose information, as we can no longer invert
 * the inversion to recover the original GTI.
 *
 * Revision 1.5  2015/09/30 22:10:14  driethmi
 * Enabled futher support for extended syntax in file names.
 *
 * Revision 1.4  2015/09/30 21:06:04  driethmi
 * Changed to exit upon fits error.  Also changed to modify TSTART and TSTOP
 * keyword values instead of adding new ones.
 *
 * Revision 1.3  2015/09/18 17:42:16  driethmi
 * Added check for negative GTI intervals in the input file.  If found, code
 * should now inform the user and exit.
 *
 * Revision 1.2  2015/01/08 16:27:19  driethmi
 * Updated parameters to conform to AstroH standards.
 *
 * Revision 1.1  2014/12/10 15:09:33  driethmi
 * Modified gtiinvert to remove all ahftis and ahapp, replaced with direct
 * cfitsio calls, moved to ftools/time.
 *
 * Revision 1.3  2014/12/01 21:46:14  driethmi
 * Removed unused variables.
 *
 * Revision 1.2  2014/12/01 20:54:02  driethmi
 * Cleaned up and simplified the getPars routine.
 *
 * Revision 1.1  2014/11/21 20:18:08  driethmi
 * Initial commit
 *
 *
 */
