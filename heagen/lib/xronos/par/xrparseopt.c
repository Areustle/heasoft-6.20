/*
 * xrparseopt.c
 */
#include <stdlib.h>
#include <string.h>
#include "xronos.h"

/*
 *  Local helper function prototypes (implemented at end)
 */
bool xrmatchopt_bool(char *optstr, char *optcode, XROptBool *value, 
                     int *status);
bool xrmatchopt_int(char *optstr, char *optcode, XROptInt *value, int *status);
bool xrmatchopt_dbl(char *optstr, char *optcode, XROptDbl *value, int *status);

int xrparseopt(char *filestr, XRFile *file, int *status) {
/*
 *  Parse file options and store in XRFile struct
 *
 * I/O  filestr - String containing files and options (clobbered by strtok)
 * I/O  file    - XRFile in which to store files and options
 *  O   status  - Error flag (0=OK)
 */
   char errmsg[MAXLEN_ERR];
   char *curstr;

   /* Options are space and/or comma-delimited */
   curstr = fits_split_names(filestr);
   /* First item encountered is filename */
   file->name = stralloc(curstr);
   while ( NULL != (curstr = fits_split_names(NULL)) ) {
      /* Remaining items are options */
      if ( xrmatchopt_bool(curstr, "BT", &file->BT, status) ) continue;
      if ( xrmatchopt_int (curstr, "FE", &file->FE, status) ) continue;
      if ( xrmatchopt_int (curstr, "FR", &file->FR, status) ) continue;
      if ( xrmatchopt_int (curstr, "LR", &file->LR, status) ) continue;
      if ( xrmatchopt_bool(curstr, "DN", &file->DN, status) ) continue;
      if ( xrmatchopt_int (curstr, "LE", &file->LE, status) ) continue;
      if ( xrmatchopt_int (curstr, "LE", &file->LE, status) ) continue;
      if ( xrmatchopt_int (curstr, "VX", &file->VX, status) ) continue;
      if ( xrmatchopt_int (curstr, "VY", &file->VY, status) ) continue;
      if ( xrmatchopt_int (curstr, "VC", &file->VC, status) ) continue;
      if ( xrmatchopt_int (curstr, "VE", &file->VE, status) ) continue;
      if ( xrmatchopt_int (curstr, "VS", &file->VS, status) ) continue;
      if ( xrmatchopt_int (curstr, "RT", &file->RT, status) ) continue;
      if ( xrmatchopt_bool(curstr, "OF", &file->OF, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "MU", &file->MU, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "MD", &file->MD, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "ME", &file->ME, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "MA", &file->MA, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "DI", &file->DI, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "DD", &file->DD, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "DE", &file->DE, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "DA", &file->DA, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "AA", &file->AA, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "AD", &file->AD, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "AE", &file->AE, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "SA", &file->SA, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "SD", &file->SD, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "SE", &file->SE, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "QA", &file->QA, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "QD", &file->QD, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "QE", &file->QE, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "ST", &file->ST, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "SS", &file->SS, status) ) continue;
      if ( xrmatchopt_dbl (curstr, "DV", &file->DV, status) ) continue;
      *status = -1;
      sprintf(errmsg, "Unknown file option: %s\n", curstr);
      HD_ERROR_THROW(errmsg, *status);
   }
   return(*status);
}

bool xrmatchopt_bool(char *optstr, char *optcode, XROptBool *opt, 
                     int *status) {
/*
 *  Compares first two characters of optstr to optcode
 *  If a match, sets value to TRUE
 *
 *  I optstr  - File option to be parsed
 *  I optcode - Two character optcode to compare
 *  O value   - Value to set
 *  O status  - Error flag (0=OK)
 *  
 *  Returns TRUE if a match, FALSE otherwise
 */
   bool match, exact;

   fits_compare_str(optcode, optstr, CASEINSEN, &match, &exact);
   if ( match ) {
      opt->set = TRUE;
      opt->value = TRUE;
   }
   return(match);
}

bool xrmatchopt_int(char *optstr, char *optcode, XROptInt *opt, int *status) {
/*
 *  Compares first two characters of optstr to optcode
 *  If a match, sets value to integer that follows code
 *
 *  I optstr  - File option to be parsed
 *  I optcode - Two character optcode to compare
 *  O value   - Value to set
 *  O status  - Error flag (0=OK)
 *  
 *  Returns TRUE if a match, FALSE otherwise
 */
   bool match, exact;
   char optpatt[4];

   optpatt[0] = optcode[0];
   optpatt[1] = optcode[1];
   optpatt[2] = '*';
   optpatt[3] = '\0';

   fits_compare_str(optpatt, optstr, CASEINSEN, &match, &exact);
   if ( match ) {
      opt->set = TRUE;
      opt->value = atoi(optstr+2);
   }
   return(match);
}

bool xrmatchopt_dbl(char *optstr, char *optcode, XROptDbl *opt, int *status) {
/*
 *  Compares first two characters of optstr to optcode
 *  If a match, sets value to double that follows code
 *
 *  I optstr  - File option to be parsed
 *  I optcode - Two character optcode to compare
 *  O value   - Value to set
 *  O status  - Error flag (0=OK)
 *  
 *  Returns TRUE if a match, FALSE otherwise
 */
   bool match, exact;
   char optpatt[4];

   optpatt[0] = optcode[0];
   optpatt[1] = optcode[1];
   optpatt[2] = '*';
   optpatt[3] = '\0';

   fits_compare_str(optpatt, optstr, CASEINSEN, &match, &exact);
   if ( match ) { 
      opt->set = TRUE;
      opt->value = atof(optstr+2);
   }
   return(match);
}
