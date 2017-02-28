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

#define EVALTMPL(A,B,C,D,E,F,G,H,I) CCALLSFSUB9(EVALTMPL,evaltmpl,STRING,STRING,PSTRING,PINT,PDOUBLE,PSTRING,PSTRING,PSTRING,PINT, A,B,C,D,E,F,G,H,I)

#define NUMSVKEYS 1
/* N_WCSKEYS must match the length of the initialized wcskeys array. */
#define N_WCSKEYS 9 

extern fitsfile *gFitsFiles[]; /* file ptr corresponding to Fortran lun */

int XImage_MoveKeywordBeforeWCSKeywords(fitsfile *fitsptr, char *badkeyword, int *status)
{
  /* Checks for the existence of badkeyword in FITS header of fitsptr,
   * then searches for first WCS keyword in header.  If both
   * badkeyword and any WCS keyword are found in the header, and
   * badkeyword appears after any WCS keyword, this function deletes
   * badkeyword from the header and inserts it immediately before the
   * first WCS keyword. Returns CFITSIO status.
   *
   * This function was written to eliminate a bug in ximage that
   * places the WCSAXES and WCSAXESa keywords after the other WCS
   * keywords.  This behavior violated the FITS standard and caused an
   * error when the FITS file was checked with fverify.
   *
   * Timothy Reichard Sept. 2011
   */

  int n_keywords, morekeys, firstwcskeypos, badkeypos, i, k;
   char card[FLEN_CARD], keyword[FLEN_KEYWORD];
   char shortkeyword[FLEN_KEYWORD];
   char comment[FLEN_COMMENT];
   char valstr[FLEN_VALUE];

   /* List of full or truncated WCS keywords. */
   char *wcskeys[]= {"CTYPE", /* Will match CTYPE and anything 
				 beginning with "CTYPE". */
		     "CUNIT", /* Similarly for all keywords in this list. */
		     "CRVAL", 
		     "CDELT",
		     "CRPIX",
		     "CROTA",
		     "PC",
		     "CD",
		     "PV",
		     "PS",
		     "WCSNAME",
		     "CNAME",
		     "CRDER",
		     "CSYER",
		     "LONPOLE",
		     "LATPOLE",
		     "EQUINOX",
		     "EPOCH",
		     "MJD-OBS",
		     "MJD-AVG",
		     "DATE-OBS",
		     "DATE-AVG",
		     "RADESYS",
		     "RADECSYS",
		     "RESTFREQ",
		     "RESTFRQ",
		     "RESTWAV",
		     "SPECSYS",
		     "SSYSOBS",
		     "SSYSSRC",
		     "OBSGEO-",
		     "VELOSYS",
		     "ZSOURCE",
		     "VELANGL"
   }; /* WCS keyword names from Table 22 of the FITS Definition v3.0 */

   int n_wcskeys = 34;
  
   fits_read_card(fitsptr, badkeyword, card, status);
   if(*status != 0) /* badkeyword not found in header. Nothing to do. */
     {
       *status = 0;
       return *status;
     }

   fits_get_hdrspace(fitsptr, &n_keywords, &morekeys, status);
   if(*status != 0)
     {
       cxwrite("Cannot obtain the number of keywords in header.", 20);
       return *status;
     }

   /* Search for first occurrence of a WCS keyword in the wcskeys list 
    * and note its position in the header. 
    *
    * The WCS keyword list has either full or truncated keyword names.
    * In the comparison with header keyword names below, the WCS
    * keyword names act as though they have the wildcard (*) at their
    * ends, so that, e.g., header keyword "CTYPE1" will match WCS
    * keyword "CTYPE" (interpreted as "CTYPE*").
    */

   firstwcskeypos = -1; /* -1 means "not found" */
   badkeypos = -1;      /* -1 means "not found" */
   i = 1;
   while(i <= n_keywords && (firstwcskeypos == -1 || badkeypos == -1))
     {
       fits_read_keyn(fitsptr, i, keyword, valstr, comment, status);
       if(*status != 0)
	 {
	   cxwrite("Suddenly cannot read keyword from header.\n", 20);
	   return *status;
	 }
       
       if(badkeyword != -1 && strcmp(keyword, badkeyword) == 0)
	 {
	   badkeypos = i;
	 }

       for(k = 0; k < n_wcskeys; k++)
	 {
	   if(firstwcskeypos != -1)
	     break;
	   
	   if(strlen(wcskeys[k]) > strlen(keyword))
	     continue;
	   
	   strncpy(shortkeyword, keyword, strlen(wcskeys[k]));
	   shortkeyword[strlen(wcskeys[k])] = '\0';

	   if((strcmp(shortkeyword, wcskeys[k]) == 0) && (strcmp(keyword, "PCOUNT") != 0)) /* WCS key found. */
	     {
	       firstwcskeypos = i;
	       break;
	     }
	 }
       i++;
     }

   if(firstwcskeypos == -1 || badkeypos == -1) 
     /* If WCS keywords or badkeyword aren't found, nothing to do. */
     return *status;

   if(badkeypos < firstwcskeypos) 
     /* If badkeyword appears before WCS keywords, nothing to do. */
     return *status;

   /* Remove badkeyword and insert it before first WCS keyword. The
      position of the first WCS keyword (for insertion) does not
      change after deleting badkeyword because the deleted key appeared
      after the first WCS keyword if execution reaches here.
    */
   
   fits_delete_key(fitsptr, badkeyword, status);
   if(*status != 0)
     {
       cxwrite("Cannot delete keyword", 20);
       return *status;
     }
   
   fits_insert_record(fitsptr, firstwcskeypos, card, status);
   if(*status != 0)
     {
       cxwrite("Cannot insert keyword", 20);
       return *status;
     }
   
   return *status;
}

void wr_fithdr(int lun, char *mapid, char *template) {
/*
 *  Writes contents of header to FITS file based on template
 *
 *  I  lun       (i)  Logical unit of open FITS file
 *  I  mapid     (c)  Which header to print
 *  I  template  (c)  Template to base output on
 */

   AstFitsChan *fitschan, *nowcschan;
   AstFrameSet *wcsinfo;
   char card[FLEN_CARD], keyword[FLEN_KEYWORD], ds[FLEN_VALUE];
   char comment[FLEN_COMMENT], type[2], wcsid[MAX_IDSTR];
   char valstr[FLEN_VALUE], keypatt[FLEN_CARD], *msg;
   char sysiden, c;
   char badkeyword[FLEN_KEYWORD];
   int i, isskew, delcd, len, di, status;
   double cd1, cd2, dd;
   FILE *fp;
   fitsfile *fitsptr;
   int (*wrwcskey)(fitsfile *fptr, const char *card, int *status) = &ffprec;

   static char * svkey[NUMSVKEYS] = {"DATE-OBS"};

   if ( (fp = fopen(template, "r")) == NULL ) {
      msg = strcatalloc(" Could not open template file: ", template);
      cxwrite(msg, 10);
      free(msg);
      return;
   }
   msg = strcatalloc(" Using template file: ", template);
   cxwrite(msg, 20);
   free(msg);

   status = 0;

   astBegin;
/*
 *  Fill FitsChan with internal header values from template
 */
   fitschan = astFitsChan( NULL, NULL, "" );
   while ( fgets(card, FLEN_CARD, fp) ) {
      len = strlen(card);
      if ( card[len-1] == '\n' ) card[len-1] = '\0';
      strcpy(valstr, "");
      EVALTMPL(card, mapid, keyword, di, dd, ds, type, comment, status);
      if ( *type == 'M' ) {
	 ffupch(keyword);
	 ffupch(ds);
	 if ( strcmp(keyword, "WCS") == 0 ) {
	    if ( strcmp(ds, "COMMENT") == 0 ) {
	       wrwcskey = &ffpcom;
	    } else if ( strcmp(ds, "HISTORY") == 0 ) {
	       wrwcskey = &ffphis;
	    } else if ( strcmp(ds, "NOWRITE") == 0 ) {
	       wrwcskey = NULL;
	    }
	 }
      } else if ( *type == 'I' ) {
         ffi2c(di, valstr, &status);
      } else if ( *type == 'D' ) {
         if ( ! ISDNULL(dd) ) ffd2e(dd, SIGDIG, valstr, &status);
      } else if ( *type == 'S' ) {
         if ( strcmp(ds,"") == 0 ) {
            strcpy(valstr,"");
         } else {
            ffs2c(ds, valstr, &status);
         }
      }
      if ( strcmp(valstr,"") != 0 ) {
         ffmkky(keyword, valstr, comment, card, &status); /* construct card */
         astPutFits(fitschan, card, 0);
      }
   }
/*
 *  Save keys from AST meddling.  For example, AST insists on
 *  using the old dd/mm/yy format for DATE-OBS < year 2000
 *  losing time information in the process.
 */
   nowcschan = astCopy(fitschan);
/*
 *  Write wcs data structure as FITS keywords
 */
   GHEADS(mapid, "WCSID", wcsid, 0, status);
   if ( strcmp(wcsid,"") != 0 )  {
      wcsinfo = getwcs(wcsid);
      astSet(fitschan, "Encoding=FITS-WCS,CDMatrix=1");
      astWrite(fitschan, wcsinfo);
   }
/*
 *  Restore saved keys (DATE-OBS)
 */
   for ( i = 0; i < NUMSVKEYS; i++ ) {
      astClear(nowcschan, "Card");
      if ( astFindFits(nowcschan, svkey[i], card, 0) ) {
         astClear(fitschan, "Card");
         if ( astFindFits(fitschan, svkey[i], NULL, 0) ) {
            astPutFits(fitschan, card, 1);
         } else {
            astPutFits(fitschan, card, 0);
         }
      }
   }
/*
 *   If axis 1 is Dec, ignore wcs structure output
 */
   astClear(fitschan, "Card");
   if ( astFindFits(fitschan, "CTYPE1", card, 0) ) {
      ffpsvc(card, valstr, comment, &status);
      if ( strncmp(valstr, "'DEC", 4) == 0 ) {
         cxwrite(" Omitting wcsid info as it inverts RA/Dec", 20);
         astAnnul(fitschan);
         fitschan = astCopy(nowcschan);
      }
   }
/*
 *  If CD1_2 or CD2_1 exists, delete CDELT1 and CDELT2
 */
   isskew = 0;
   astClear(fitschan, "Card");
   if ( astFindFits(fitschan, "CD1_2", card, 0) ) isskew = 1;
   astClear(fitschan, "Card");
   if ( !isskew && astFindFits(fitschan, "CD2_1", card, 0) ) isskew = 1;
   if ( isskew ) {
      if ( astfitsgetd(fitschan, "CDELT1", &cd1) ) astDelFits(fitschan);
      if ( astfitsgetd(fitschan, "CDELT2", &cd2) ) astDelFits(fitschan);
   }
/*
 *  If CD1_1, CD2_2, CDELT1, and CDELT2 exist, delete CD1_1 and CD2_2
 */
   delcd = 1;
   astClear(fitschan, "Card");
   if ( delcd && !astFindFits(fitschan, "CD1_1", card, 0) ) delcd = 0;
   astClear(fitschan, "Card");
   if ( delcd && !astFindFits(fitschan, "CD2_2", card, 0) ) delcd = 0;
   astClear(fitschan, "Card");
   if ( delcd && !astFindFits(fitschan, "CDELT1", card, 0) ) delcd = 0;
   astClear(fitschan, "Card");
   if ( delcd && !astFindFits(fitschan, "CDELT2", card, 0) ) delcd = 0;
   if ( delcd ) {
      if ( astfitsgetd(fitschan, "CD1_1", &cd1) ) astDelFits(fitschan);
      if ( astfitsgetd(fitschan, "CD2_2", &cd2) ) astDelFits(fitschan);
   }
/*
 *  Use standard respresentation of PHYSICAL coordinates
 */
   if ( astfitsmod(fitschan, "WCSNAME%1c", "PIXEL", "PHYSICAL", keyword)) {
      sysiden = keyword[strlen(keyword)-1];
      sprintf(keypatt, "CTYPE1%c", sysiden);
      astfitsmod(fitschan, keypatt, "PIXEL1", "x", keyword);
      sprintf(keypatt, "CTYPE2%c", sysiden);
      astfitsmod(fitschan, keypatt, "PIXEL2", "y", keyword);
   }
/*
 *  Use CDELT1P instead of CD1_1P
 */
   cd1 = 1.0;
   cd2 = 1.0;
   if ( astfitsgetd(fitschan, "CD1_1P", &cd1) ) astDelFits(fitschan);
   astfitssetd(fitschan, "CDELT1P", cd1, "Rebin factor in x");
   if ( astfitsgetd(fitschan, "CD2_2P", &cd2) ) astDelFits(fitschan);
   astfitssetd(fitschan, "CDELT2P", cd2, "Rebin factor in y");

   fitsptr = gFitsFiles[lun];
   astClear(fitschan, "Card");
   while ( astFindFits(fitschan, "%f", card, 1) ) {
      ffgknm(card, keyword, &len, &status);
      astClear(nowcschan, "Card");
      if ( astFindFits(nowcschan, keyword, NULL, 0) ) {
         ffprec(fitsptr, card, &status);      /* write keyword*/
      } else {
	 if ( wrwcskey ) {
	    i = strlen(card) - 1;
	    while ( i > 0 && card[i] == ' ' ) {
	       card[i] = '\0';
	       i--;
	    }
            (*wrwcskey)(fitsptr, card, &status); /* write wcs keyword*/
	 }
      }
   }

   /* Check for WCSAXES and WCSAXESA to WCSAXESZ keywords appearing
    * illegally after other WCS keys.  Move the keyword to before the
    * first WCS keyword if necessary.
    */

   XImage_MoveKeywordBeforeWCSKeywords(fitsptr, "WCSAXES", &status);
   for(c = 'A'; c <= 'Z'; c++)
     {
       if(status != 0)
	 {
	   cxwrite("Cannot check existence/position of WCSAXES* keyword.", 20);
	   break;
	 }
       strcpy(badkeyword, "WCSAXES_");
       badkeyword[7] = c;
       XImage_MoveKeywordBeforeWCSKeywords(fitsptr, badkeyword, &status);
     }

   astEnd;
}
FCALLSCSUB3(wr_fithdr,WR_FITHDR,wr_fithdr,INT,STRING,STRING)
