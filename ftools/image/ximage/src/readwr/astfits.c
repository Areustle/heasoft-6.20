/*
 *  FORTRAN Usage:
 *  call wr_fithdr(lun, mapid, template)
 */
#include <stdlib.h>
#include <stdio.h>
#include "ast.h"
#include "fitsio2.h"
#include "cfortran.h"
#include "../include/maxvals.h"
#include "../include/xcommon.h"
#include "../include/map.h"
#include "../include/wcsmanager.h"
#include "../include/astfits.h"
#include "../include/null.h"

int astfitsmod(AstFitsChan *fitschan, char *keypatt, 
                char *matchval, char *replaceval, char *keyword) {
/*
 *  Modify keyword in fitschan that matches "keypatt" and has value 
 *   "matchval" with "replaceval"
 *
 *  I  fitschan   - AST Fits Channel
 *  I  keypatt    - Key name pattern (see astFindFits)
 *  I  matchval   - String value to match
 *  I  replaceval - String value to replace if there is a match
 *  O  keyword    - Keyword value matched
 *
 *  Returns 1 if match found and replacement made, zero otherwise
 *
 *  Note: Replacements appear at end of channel not in place of original
 */

   int replaced = 0;
   int status = 0;
   char card[FLEN_CARD];
   char value[FLEN_VALUE], valbuff[FLEN_VALUE], comment[FLEN_COMMENT];
   int keylen, ncard;

   strcpy(value, "");
   astClear(fitschan, "Card");
   if ( astFindFits(fitschan, keypatt, card, 0) ) {
      ffgknm(card, keyword, &keylen, &status);
      ffpsvc(card, value, comment, &status);
      ffc2s(value, valbuff, &status);

      if ( strcmp(valbuff, matchval) == 0 ) {
         ffs2c(replaceval, value, &status);
         ffmkky(keyword, value, comment, card, &status);
         astDelFits(fitschan);
         ncard = astGetI(fitschan, "Ncard");
         astSetI(fitschan, "Card", ncard);
         astPutFits(fitschan, card, 0);
         if ( astOK ) replaced = 1;
      }
   }
   return replaced;
}

int astfitsgetd(AstFitsChan *fitschan, char *keyword, double *value) {
/*
 *  Find and return double value from fits channel based on keyword
 *
 *  I  fitschan   - AST Fits Channel
 *  I  keyword    - Keyword to get value of
 *  O  value      - Value to return
 *
 *  Returns 1 if match found and value returned, zero otherwise
 */

   int found = 0;
   int status = 0;
   char card[FLEN_CARD];
   char valstr[FLEN_VALUE], comment[FLEN_COMMENT], keybuff[FLEN_KEYWORD];
   int keylen;

   strcpy(valstr, "");
   astClear(fitschan, "Card");
   if ( astFindFits(fitschan, keyword, card, 0) ) {
      ffgknm(card, keybuff, &keylen, &status);
      ffpsvc(card, valstr, comment, &status);
      ffc2d(valstr, value, &status);
      if ( status == 0 ) found = 1;
   }
   return found;
}

int astfitssetd(AstFitsChan *fitschan, char *keyword, double value, char *comment) {
/*
 *  Assign keyword value in fits channel.  If exists, replace. If not,
 *    create a new entry
 *
 *  I  fitschan   - AST Fits Channel
 *  I  keyword    - Keyword to set
 *  I  value      - Value to return
 *  I  comment    - Comment
 *
 *  Returns 1 if new, 2 if replaced, zero if failed
 *
 *  Note: Replacements appear at end of channel not in place of original
 */

   int overwrite = 0;
   int status = 0;
   char card[FLEN_CARD], dumcard[FLEN_CARD];
   char valstr[FLEN_VALUE];
   int ncard;

   ffd2e(value, SIGDIG, valstr, &status);
   ffmkky(keyword, valstr, comment, card, &status);
   if ( status != 0 ) return 0;

   astClear(fitschan, "Card");
   if ( astFindFits(fitschan, keyword, dumcard, 0) ) astDelFits(fitschan);
   ncard = astGetI(fitschan, "Ncard");
   astSetI(fitschan, "Card", ncard);
   astPutFits(fitschan, card, 0);
   if ( !astOK ) return 0;
   if ( overwrite ) return 2;
   return 1;
}
