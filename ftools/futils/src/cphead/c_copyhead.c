#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "fitsio.h"
#include "cftools.h"
#include "cphead.h"


/***************************************************************************
*
*	c_copyhead 
*
*  Selectively copy keywords from the input fits file to the output fits  file.
*  Inspired by Larry Brown's fortran routine copyhead. 
*
*  The user can provided a  list  of the wanted keywords or unwanted
*  keywords (preceded by !) with variable length argument.
*  Every keyword,  which is not in the list and not a "basic" keyword,
*  is copied to the output file in default.   
*
*  It is user's resposibility to position the current hdu's of 
*  the input and output fits files.
*  
*
*  parameter:
*     infits: input fits file.
*     oufits: ouput fits file. 
*     scale:  if !=0, copy the related scale keywords.
*     ...:    variable argument list of names of the wanted keywords or
*             unwanted keywords. The name is enclosed by " ". It is ended 
*             with "0". The wanted keywords in this list 
*	      will be copied  to the outfits file regardlessly
*             (even it is a scale-related  keyword). 
*             The unwanted keywords, whose name is preceded by !, will not be   
*             copied to the output fitsfile. The name of keywords
*             in this list is case-insensitive. 
*             The maximum number of keywords in this list is MAXCOPYKEYS
*     "0":    End of the argument list. 
*
* Author:   Ning Gan	6/2/98
*
*************************************************************************/

int c_copyhead(fitsfile* infits,	/* input fits file */
               fitsfile* outfits,	/* output fits file*/
               int scale,		/* scale keywords flag */
                   ...)
{
    char errmsg[FLEN_ERRMSG];
    char* basic_keywords[] = {  "SIMPLE",
        	        "BITPIX",
        		"NAXIS",
        		"EXTEND",
        		"XTENSION",
        		"PCOUNT",
        		"GCOUNT",
        		"TFIELDS",
        		"TTYPE",
        		"TBCOL",
        		"TFORM",
        		"TUNIT",
        		"THEAP",
        		"TDIM",
        		"GROUPS",
        		"DATASUM",
        		"CHECKSUM",
        		"END"};
    char* last_basic = "END";		/* last keywords in the basic_keyword list */	
    char* scale_keywords[] = {  "TSCAL",
        		"TZERO",
        		"TNULL",
        		"TDISP",
        		"BSCALE",
        		"BZERO",
        		"BUNIT",
        		"BLANK",
        		"CTYPE",
        		"CRPIX",
        		"CROTA",
        		"CRVAL",
        		"CDELT",
        		"TLMIN",
        		"TLMAX",
        		"OPTIC",
        		"TCRPX",
        		"TCRVL",
        		"TCDLT",
        		"TCTYP",
        		"TCD",
        		"TCROT",
        		"PLTSCL"
                   };
    char* last_scale = "PLTSCL";	/* last keywords in the scale_keyword list */

    char* accept_keys[MAXCOPYKEYS];
    char* reject_keys[MAXCOPYKEYS];
    int naccept = 0;
    int nreject = 0;

    char keyname[FLEN_KEYWORD];
    char card[FLEN_CARD];

    int n = 0;
    int i = 0;
    int knlen;

    int nkeys, morekeys;
    
    va_list argptr;		/* argument pointer*/
    int arg_len;
    char* p;
    int status = 0;

    
    /* handle the argument list */ 
    va_start(argptr, scale);
    while(1) { 
         p = va_arg(argptr, char*);
	 if(p[0] == '0') break;
         fits_uppercase(p);
         if(p[0] == '!') {		/* rejected keyword */
             arg_len = strlen(p)-1;           
             reject_keys[nreject] = (char *) malloc(arg_len+1);
             strcpy(reject_keys[nreject],&(p[1]));
             nreject++;
         }
         else{
             arg_len = strlen(p);           
             accept_keys[naccept] = (char *) malloc(arg_len+1);
             strcpy(accept_keys[naccept],p);
             naccept++;
         }
         if(naccept >=  MAXCOPYKEYS || nreject >= MAXCOPYKEYS) {
             strcpy(errmsg,"too many arguments in the c_copyhead");
             c_fcerr(errmsg);
             return 1;
         } 
    }
    va_end(argptr);
      
    /* get the total number of keywords */
    if(fits_get_hdrspace(infits, &nkeys, &morekeys, &status)) { 
        fits_report_error(stderr, status);
        return 1;
    }
    nkeys += 1;
    for ( n = 1 ; n < nkeys ; n++ ) { 
	/* read in the keyword */
        if(fits_read_record(infits, n, card, &status)) { 
            fits_report_error(stderr, status);
            return 1;
        }
	/* parse out the keyname and strip the trailing blanks */
        if(ffgknm(card,keyname,&knlen,&status)) {  
            fits_report_error(stderr, status);
            return 1;
        }     

        /* treat null keyword name as comment */
        if(strlen(keyname) == 0) strcpy(keyname,"COMMENT");




	/* Never copy the basic keywords */
        for( i=0; strcmp(basic_keywords[i],last_basic) != 0 ; i++){
	    if(strncmp(keyname, basic_keywords[i], strlen(basic_keywords[i]))==0)
                 goto done;
        }

	/* is it a accepted keyword? */
        for (i = 0; i < naccept; i++) { 
	    if(strcmp(keyname, accept_keys[i])==0 ) {
                goto cpgo1; 
            } 
        }

	/* is it a rejected keyword? */
        for (i = 0; i< nreject; i++) { 
	    if(strcmp(keyname, reject_keys[i])==0 ) goto done; 
        }
   
 
        /* deal with scale related keywords */
        if( scale == 0)  { 
	    for( i=0; strcmp(scale_keywords[i],last_scale) != 0 ; i++){
	        if(strncmp(keyname, scale_keywords[i], 
                      strlen(scale_keywords[i]))==0) goto done;
            }
        }

	/*  copy the keyword ... */
        cpgo1: 
	if(  (strstr(keyname,"COMMENT") == NULL) 
            && (strstr(keyname,"HISTORY") == NULL)
            && (strlen(keyname) != 0 ) ) {
            if(fits_update_card(outfits,keyname, card, &status)) { 
                fits_report_error(stderr, status);
                return 1;
            }
        }
        else if (strstr(keyname,"COMMENT") != NULL) {
            /* excluding the common fits comments */
            if(fit_comment(card)) continue;
            if(fits_write_record(outfits, card, &status)){
                fits_report_error(stderr, status);
                return 1;
            }
        }
        else {
            if(fits_write_record(outfits, card, &status)){
                fits_report_error(stderr, status);
                return 1;
            }
        }
        done:              
        ;
    }   	
    for (i = 0; i< nreject; i++) free(reject_keys[i]);
    for (i = 0; i< naccept; i++) free(accept_keys[i]);
    return 0;
}
