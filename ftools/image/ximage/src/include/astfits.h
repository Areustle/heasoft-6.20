/*
 *  Routines to simplify working with AST FitsChan 
 */

/*
 * Modify keyword in fitschan that matches "keypatt" and has value 
 *   "matchval" with "replaceval"
 */
int astfitsmod(AstFitsChan *fitschan, char *keypatt, 
               char *matchval, char *replaceval, char *keyword);

/*
 * Find and return double value from fits channel based on keyword
 */
int astfitsgetd(AstFitsChan *fitschan, char *keyword, double *value);

/*
 * Assign keyword value in fits channel.  If exists, replace. If not,
 *    create a new entry
 */
int astfitssetd(AstFitsChan *fitschan, char *keyword, double value, 
                char *comment);

/* Number of significant digits to write in FITS card */
#define SIGDIG 10
