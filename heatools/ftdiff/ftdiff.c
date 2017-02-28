#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <fitsio.h>
#include "pil.h"
#include "headas.h"
#include "headas_error.h"
#include "ftdiff.h"

#define MAXMSG 256

/*
HISTORY
-------
Version 1.0 written by William Pence and Ziqin Pan, NASA/GSFC, August 2002
Version 1.1 - added tolerance parameter; suggested by Bob Wiegand
Version 1.2 - added reltol parameter to look for fractional differences
Version 1.3 - added HDerror_dump_silence
Version 1.31 - detect row by row differences in variable length array
Version 1.32 - properly handle long keyword value and continue keyword
Version 1.33 - properly handle multiple copies of the same keyword
Version 1.34 - fix a memory leaking
Version 1.35 - display more precision in floating point differences
Version 1.36 - if tolerances in numerical comparisions are not zero,
               then ignore any differences in the DATASUM keyword value
Version 1.37 - do not perform numerical comparisons of TFORM or TDISP
               keyword values
Version 1.38 - If hdumaxdiff <=0, report all differences.
*/

#define TOOLSUB ftdiff
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int ftdiff (void);
int ftdiff_getpar(char * infile1, char * infile2, char* exclude);
int ftdiff_work (char * infile1, char * infile2, char *exclude);

void max_mess(void);

/* routines on comparing headers */
int cmp_head(fitsfile *infptr1, fitsfile *infptr2, char **elist, int nelist);
void get_keylist( char *exclude,	char **elist, int  *nelist);
int isinlist(char *keyname, char **elist, int nelist);
int compnum (char *value1, char *value2 );
int compkeys (const void *key1, const void *key2);
void print_keys(FitsKey *p1, FitsKey *p2); 

/* routines on comparing data */
int cmp_datasum(fitsfile *fits1, fitsfile *fits2);
int cmp_data(fitsfile *fits1, fitsfile *fits2,int hdutype);
int cmp_img(fitsfile *fits1, fitsfile *fits2);
int cmp_tbl(fitsfile *fits1, fitsfile *fits2);
int cmp_row(fitsfile *fits1,  fitsfile *fits2, int irow , 
            FitsCol *fits1_col, FitsCol *fits2_col, int ncols);          
int cmp_col(fitsfile *fits1, int col1, int row1, fitsfile *fits2,int
            col2, int row2, int repeat, int coltype, double *data, 
            unsigned char *cdata, int *idata );
int print_col(int coltype,  int repeat,
              double *data, unsigned char* cdata, int *idata, char *prompt);
int datacode(int datatype); 


int ftdiff(void)
{ 
    char infile1[PIL_LINESIZE] = "";  	/* Input Fits file */
    char infile2[PIL_LINESIZE] = "";  	/* Input Fits file */
    char exclude[FDFCOM];
    int status;

    static char taskname[80] = "ftdiff";
    static char version[8] = "1.38";

    /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);
   

    /* get input parameters */
    status = ftdiff_getpar(infile1,infile2,exclude);

    if(!status)
        status = ftdiff_work(infile1,infile2, exclude);
    return(status);

}

int ftdiff_getpar(
    char *infile1,       /* I - Input file name */
    char *infile2,       /* I - Input file name */
    char *exclude)       /* I - Excludes keywords */

/*  read input parameters for the ftdiff task from the .par file */
{
    int status;
    char msg[MAXMSG];

    if ((status = PILGetFname("infile1", infile1))) {
      sprintf(msg, "Error reading the 'infile1' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetFname("infile2", infile2))) {
      sprintf(msg, "Error reading the 'infile2' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("exclude", exclude))) {
      sprintf(msg, "Error reading the 'exclude' parameter.");
      HD_ERROR_THROW(msg,status);
    }
    else if ((status = PILGetBool("cmpnum", &cmpnum))) {
      sprintf(msg, "Error reading the 'cmpnum' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetReal("tolerance", &tolerance))) {
      sprintf(msg, "Error reading the 'tolerance' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetReal("reltol", &reltol))) {
      sprintf(msg, "Error reading the 'reltol' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetBool("caldsum", &caldsum))) {
      sprintf(msg, "Error reading the 'caldsum' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetBool("cmpdata", &cmpdata))) {
      sprintf(msg, "Error reading the 'cmpdata' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetInt("hdumaxdiff", &hdumaxdiff))) {
      sprintf(msg, "Error reading the 'hdumaxdiff' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetInt("chatter", &chatter))) {
      sprintf(msg, "Error reading the 'chatter' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    return status;
}



int ftdiff_work(char* infile1, char* infile2, char* exclude) 
{

    char rtnam1[FLEN_FILENAME] = "";
    int hdunum1 = -99;                  /* extension number? */
    int fhdu1  = 1;                     /* first hdu   */
    int thdu1  = 1;                     /* total number of hdu  */
    fitsfile *infptr1;                  /* input fits file pointer */
    int hdutype1;

    char rtnam2[FLEN_FILENAME] = "";    /* Input Fits file root name */
    int hdunum2 = -99;                  /* extension number? */
    int fhdu2  = 1;                     /* first hdu   */
    int thdu2  = 1;                     /* total number of hdu  */
    fitsfile *infptr2;                  /* input fits file pointer */
    int hdutype2;
    int totalhdu = 1;

    char *elist[MAXEXCLUDE];
    int nelist = 0;
    int ndiff = 0;

    int status =0;
    int i;

    get_toolname(task); 
    get_toolversion(version);

    headas_chat(1,"\n              %s V%s\n\n",task,version);

    /* discard the rowfilters etc */
    if(ffrtnm(infile1, rtnam1, &status)) goto cleanup; 

    if(ffrtnm(infile2, rtnam2, &status)) goto cleanup; 

    /* Is the extension number given? */
    if(fits_parse_extnum(infile1, &hdunum1, &status)) goto cleanup;    

    if(fits_parse_extnum(infile2, &hdunum2, &status)) goto cleanup;    
     
    /* open the fits files */
    if(fits_open_file(&infptr1, infile1, READONLY, &status)) goto cleanup;

    if(fits_open_file(&infptr2, infile2, READONLY, &status)) goto cleanup;

    /* If one file hdu number specified and the other has not, 
       make the hdu number of the second file as same 
       as that of the first one */
    if(hdunum1 != -99 && hdunum2 == -99) hdunum2 = hdunum1;
    if(hdunum1 == -99 && hdunum2 != -99) hdunum1 = hdunum2;

    /* If no hdu  number is given,  get the total number of hdu */ 
    if(hdunum1 == -99) { 
	fhdu1 = 1;
        if(fits_get_num_hdus(infptr1, &thdu1, &status)) goto cleanup; 
    } 
    else { 
        fhdu1 = hdunum1;
        thdu1 = 1;
    }

    if(hdunum2 == -99) { 
	fhdu2 = 1;
        if(fits_get_num_hdus(infptr2, &thdu2, &status)) goto cleanup;
    }
    else { 
        fhdu2 = hdunum2;
        thdu2 = 1;
    }

    totalhdu = thdu2 > thdu1? thdu1: thdu2; 

    /* get the keywords we are not interested. */ 
    for (i = 0; i<MAXEXCLUDE; i++) 
	elist[i] = (char *)calloc(FLEN_KEYWORD+1,sizeof(char));
    nelist = 0;  
    get_keylist(exclude, elist, &nelist);

    /* loop over the hdus */
    for (i = 0; i < totalhdu; i++) { 
	hdundiff = 0;
        sprintf(title, "%s[%d] <     %s[%d] > \n\n",
            rtnam1,fhdu1+i-1, rtnam2,fhdu2+i-1);
	headas_chat(1, title);

        /* move to the right hdu */
        if(fits_movabs_hdu(infptr1,fhdu1+i, &hdutype1, &status) ) {
            goto cleanup;
	    break;
        } 
        if(fits_movabs_hdu(infptr2,fhdu2+i, &hdutype2, &status) ) {
            goto cleanup;
	    break;
        }

 
        /* test header */
	if((status = cmp_head(infptr1,infptr2,elist, nelist))) {  
           if(status == DIFFERR)  break;
           if(status == DIFFMAX)  {ndiff += hdumaxdiff; max_mess();}
           if(status == DIFFWRN)  ndiff+=hdundiff; 
           status = 0;
	   headas_chat(1,"=========================================\n\n");
           continue;
        }

        /* test data */ 
	if(hdutype1 != hdutype2) {
	    headas_chat(0,
       "Warning: Ext. %d of %s and  Ext. %d of %s have different types.\n",
		     fhdu1+i-1, rtnam1,fhdu2+i-1,rtnam2);
	    headas_chat(0,"Not performing data comparison.\n");
            continue;
        } 

	if((status = cmp_datasum(infptr1,infptr2))) {     /*  data sum */ 
           if(status == DIFFERR)  break;
           if(status == DIFFMAX)  {ndiff+=hdumaxdiff; max_mess();} 
           if(status == DIFFWRN)  ndiff+=hdundiff; 
           status = 0;
	   headas_chat(1,"=========================================\n\n");
           continue;
        }

        if(cmpdata) { 
            if((status = cmp_data(infptr1,infptr2,hdutype1))) {
                if(status == DIFFERR) break;
                if(status == DIFFMAX) {ndiff+=hdumaxdiff; max_mess();} 
                if(status == DIFFWRN) ndiff+=hdundiff; 
                status = 0;
	        headas_chat(1,"=========================================\n\n");
                continue; 
            }
        }

	if(chatter && !hdundiff) headas_chat(1,"\nNo differences\n");
	if(chatter || hdundiff) headas_chat(0,"=========================================\n\n");
	ndiff += hdundiff;
    }
    if(thdu2 < thdu1) { 
        ndiff += 1;
	if(thdu1 - thdu2 == 1)
	    headas_chat(0,"File %s does not have Ext. #%d\n", 
		 rtnam2, thdu2); 
        else 
	    headas_chat(0,
		"File %s does not have Ext. #%d to Ext. #%d.\n", 
		 rtnam2, thdu2,thdu1-1); 
    } 
    
    if(thdu2 > thdu1) { 
        ndiff += 1;
	if(thdu1 - thdu2 == 1)
	    headas_chat(0,"File %s does not have Ext. #%d\n", 
		 rtnam1, thdu1); 
        else 
	    headas_chat(0,
		"File %s does not have Ext. #%d to Ext. #%d.\n", 
		 rtnam1, thdu1,thdu2-1); 
    } 


    if((status=PILPutInt("numdiffs",ndiff)) !=0)  {
         fprintf(stderr,"Error updating numdiffs keyword.\n");
         status = 0;
    }


    if(chatter || ndiff) {
        if (ndiff == 1)
            headas_chat(0,"**** End of file comparison:  %d difference was found\n", ndiff);
        else
	    headas_chat(0,"**** End of file comparison:  %d differences were found\n", ndiff);
    }
    
    for (i = 0; i < MAXEXCLUDE; i++) free(elist[i]); 

    /* close the fitsfiles  */ 
    fits_close_file(infptr1, &status);
    fits_close_file(infptr2, &status);

cleanup:

    if (status == 0 && ndiff > 0) {
        HDerror_dump_silence(1);
	/* exit status values must range from 0 - 255 */
	if (ndiff > 255)
	   return(255);
	else
           return(ndiff); 
    }
    else
        return(status);
    
}


/* print the maximum diff message */
void max_mess(void)
{ 
    headas_chat(0,
"The # of differences in this HDU exceeds the max. = %d. Comparison stops!\n ",
    hdumaxdiff);
    return;
}

/******************************************************************************
* Function
*      get_keylist 
*
*
* DESCRIPTION:
*      get the excluding keyword list.  
*
*******************************************************************************/
void get_keylist( char *exclude,	/* string of the exclude parameter */
		 char **elist,		/* list of the excluded keywords*/
		 int  *nelist		/* number of the excluding list */
		 )
{
    FILE *list;				/* ASCII list file pointer */
    int i;
    char *p;
    char temp[80];

    strcpy(elist[0],"HISTORY");
    strcpy(elist[1],"COMMENT");
    strcpy(elist[2],"");
    strcpy(elist[3],"DATE");
    strcpy(elist[4],"CHECKSUM");
    strcpy(elist[5],"CONTINUE");
    *nelist = 6;

    if (tolerance || reltol) {
      /* if a tolerance is allowed in the data comparisons, any difference
         in the DATASUM keywords should not be considered significant. */
      strcpy(elist[6],"DATASUM");
      *nelist = 7;
    }

    p = exclude; 
    while(*p == ' ') p++; 
    if(*p == '\0') return ;
    i = *nelist;
    if(*p != '@') { 
	if(strchr(p,',') == NULL) { 
	    while(isspace((int) *p)) p++; 
	    strncpy(elist[i],p,FLEN_KEYWORD);
	    elist[i][FLEN_KEYWORD] = '\0';
	    p = elist[i];
	    while(*p && !isspace((int) *p) ) p++; 
	    *p = '\0';
	    i++;
        } 
	else {
	    p = strtok(exclude,",");
	    while(p) { 
	        while(isspace((int) *p)) p++; 
	        strncpy(elist[i],p,FLEN_KEYWORD);
	        elist[i][FLEN_KEYWORD] = '\0';
	        p = elist[i];
	        while(*p && !isspace((int)*p) ) p++; 
	        *p = '\0';
		i++;
		p = strtok(NULL,","); 
		if(i >= MAXEXCLUDE) break;
            }
        } 
    }
    else { 
	p++; 
        if((list = fopen(p,"r")) == NULL) { 
            sprintf(comm,"Cannot find the list file %s. Ignored. \n", p);
        } 
	else { 
	    while(fgets(temp,80,list)) {  
		p = temp;
	        while(isspace((int) *p)) p++; 
		strncpy(elist[i],p,FLEN_KEYWORD);
	        elist[i][FLEN_KEYWORD] = '\0';
	        p = elist[i];
	        while( *p && !isspace((int) *p) ) p++; 
	        *p = '\0';
		i++;
		if(i >= MAXEXCLUDE) break;
            }
        }
	fclose(list);
    }
    *nelist = i;

    /* Make all the keywords uppercase */ 
    for (i = 0; i < *nelist; i++) { 
	p = elist[i]; 
	while(*p != '\0') { 
	    if( *p >= 'a' && *p <= 'z') *p -= 32;
	    p++;
        }
    }
    return; 
}

    
/******************************************************************************
* Function
*      cmp_head 
*
*
* DESCRIPTION:
*      compare the keywords of two headers.  
*
*******************************************************************************/
int cmp_head(fitsfile *fits1, 
              fitsfile *fits2, 
	      char ** elist, 
	      int  nelist
             ) 
{
    FitsKey *kwds1; 
    FitsKey **ptkwds1; 
    int nkwds1; 

    FitsKey *kwds2; 
    FitsKey **ptkwds2; 
    int nkwds2; 
    char dumstr[FLEN_VALUE];

    int i, i1, i2, j, not_tform, not_tdisp; 
    int morekeys;
    int status = 0; 

    if(fits_get_hdrspace(fits1, &nkwds1, &morekeys, &status)) { 
        fits_report_error(stderr,status);
        return DIFFWRN;
    }
 
    if(fits_get_hdrspace(fits2, &nkwds2, &morekeys, &status)) { 
        fits_report_error(stderr,status);
        return DIFFWRN;
    } 

    kwds1 = (FitsKey *)calloc(nkwds1, sizeof(FitsKey)); 
    kwds2 = (FitsKey *)calloc(nkwds2, sizeof(FitsKey));  
    

    /* read the keywords */
    j = 0;
    for (i = 0; i < nkwds1; i++) { 
        if(fits_read_keyn(fits1, i+1, kwds1[j].kname, dumstr,NULL, 
            &status)) { 
            fits_report_error(stderr,status);
            status = 0; 
        } 
        kwds1[j].kvalue =NULL;
        kwds1[j].kindex = i+1; 
	/* skip the uninterested keywords */
	if(!isinlist(kwds1[j].kname, elist, nelist)) {
            if(fits_read_key_longstr(fits1, kwds1[j].kname, &(kwds1[j].kvalue),NULL, 
                &status)) { 
                fits_report_error(stderr,status);
                status = 0; 
            } 
            j++;
        }
    } 
    nkwds1 = j;
    ptkwds1 = (FitsKey **)malloc(nkwds1*sizeof(FitsKey*)); 
    for (i = 0; i < nkwds1; i++) { 
        ptkwds1[i] = &(kwds1[i]);
    } 

    j = 0;
    for (i = 0; i < nkwds2; i++) { 
        if(fits_read_keyn(fits2, i+1, kwds2[j].kname, dumstr,NULL,
            &status)) { 
            fits_report_error(stderr,status);
            status = 0; 
        } 
        kwds2[j].kvalue =NULL;
        kwds2[j].kindex = i+1; 
	/* skip the uninterested keywords */
	if(!isinlist(kwds2[j].kname, elist, nelist)){
            if(fits_read_key_longstr(fits2,kwds2[j].kname,&(kwds2[j].kvalue),NULL,
                &status)) { 
                fits_report_error(stderr,status);
                status = 0; 
            } 
            j++;
        }
    }
    nkwds2 = j;
    ptkwds2 = (FitsKey **)malloc(nkwds2*sizeof(FitsKey*)); 
    for (i = 0; i < nkwds2; i++) { 
        ptkwds2[i] = &(kwds2[i]);
    } 
     
    /* sort the keywords in the alphabetical ascending order of keyword name*/
    qsort(ptkwds1, nkwds1, sizeof(FitsKey *), compkeys);		
    qsort(ptkwds2, nkwds2, sizeof(FitsKey *), compkeys);		

    i1 = 0; 
    i2 = 0;
    while(i1 < nkwds1 &&  i2 < nkwds2) {
	/* compare the keyword names */
	j = strcmp(ptkwds1[i1]->kname, ptkwds2[i2]->kname); 
	if(j > 0) {
	    if(0 < hdumaxdiff && hdundiff > hdumaxdiff) goto DIFFKWDSEXIT;
            print_keys(NULL, ptkwds2[i2]);  
	    i2++;
        } 
        else if ( j < 0) {
	    if(0 < hdumaxdiff && hdundiff > hdumaxdiff) goto DIFFKWDSEXIT;
	    print_keys(ptkwds1[i1],NULL);  
            i1++;
        } 
	else { 
	    /* compare the values */ 

            /* Are these TFORM or TDISP keywords? */
            not_tform = strncmp(ptkwds1[i1]->kname,"TFORM",5);
            not_tdisp = strncmp(ptkwds1[i1]->kname,"TDISP",5);
              
	    if(strcmp(ptkwds1[i1]->kvalue, ptkwds2[i2]->kvalue)) {  
                /* Only compare as numerical values if requested and 
                   if this is _not_ a set of TFORM or TDISP keywords */ 
                if(cmpnum && not_tform && not_tdisp) { 
                    if(compnum(ptkwds1[i1]->kvalue, ptkwds2[i2]->kvalue)) {
                        if(0 < hdumaxdiff && hdundiff > hdumaxdiff) goto DIFFKWDSEXIT;
                        print_keys(ptkwds1[i1], ptkwds2[i2]);
                    }
                } 
	        else {
	            if(0 < hdumaxdiff && hdundiff > hdumaxdiff) goto DIFFKWDSEXIT;
	            print_keys(ptkwds1[i1], ptkwds2[i2]);  
                }
            }
	    i1++; i2++;
       }
    } 
    if(i1 < nkwds1) { 
	  if(0 < hdumaxdiff && hdundiff > hdumaxdiff) goto DIFFKWDSEXIT;
	  for (i = i1; i < nkwds1; i++ )  
	      print_keys(ptkwds1[i], NULL);
    }
    if(i2 < nkwds2) { 
	  if(0 < hdumaxdiff && hdundiff > hdumaxdiff) goto DIFFKWDSEXIT;
	  for (i = i2; i < nkwds2; i++ )  
	      print_keys(NULL,ptkwds2[i]);
    }

DIFFKWDSEXIT:
    for (i1=0; i1 <nkwds1; i1++) {
         status=0;
         if (ptkwds1[i1]->kvalue) fffree(ptkwds1[i1]->kvalue, &status);
    }
    for (i2=0; i2 <nkwds2; i2++) {
         status=0;
         if (ptkwds2[i2]->kvalue) fffree(ptkwds2[i2]->kvalue, &status);
    }
    free (ptkwds1);
    free (ptkwds2);
    free (kwds1);
    free (kwds2);
    if(0 < hdumaxdiff && hdundiff > hdumaxdiff) return DIFFMAX;
    return 0 ;
}
/*  test whether the keyword is in the list. 1 = yes. 0 = no.  */
    int isinlist( char *keyname,		/* keyword name */ 
	          char **elist,		/* list of the excluding list */
	          int  nelist		/* number of the excluding list */
            )
{ 
    int i; 
    char *p;
    char *q;
    int match, exact;
/*    int len; */
    int flag;

    match = 0;
    for (i = 0; i < nelist; i++) { 

       p = elist[i];
       q = keyname;
       if(*p == '\0' && *q == '\0') return 1;
       if(*p == '\0' && *q != '\0') continue;

/*
       len = strlen(p); 
       if(*(p+len-1) == '*') 
            flag = strncmp(q,p,len-1); 
       else 
            flag = strcmp(q,p);
*/

       ffcmps(p, q, 1, &flag, &exact);

       if(flag) { 
            match = 1;
            break;
       } 
    }
    return match;
}

/*  comparison function for the FitsKey structure array */
    int compkeys (const void *key1, const void *key2)
{
    char *name1;
    char *name2;
    int value1;
    int value2;
    int result=0;

    name1 = (*(FitsKey **)key1)->kname;
    name2 = (*(FitsKey **)key2)->kname;
    value1 = (*(FitsKey **)key1)->kindex;
    value2 = (*(FitsKey **)key2)->kindex;

    result = strncmp(name1,name2,FLEN_KEYWORD);

   if (result ==0 ) {
      if (value1 < value2) return -1;
      else if (value1 == value2) return 0;
      else return 1;
   } else {
      return result;
   }
}

/*  comparison function for the numerical fitskey value */ 
    int compnum (char *value1, char *value2 )
{
    char *p1, *loc1;
    char *p2, *loc2; 
    double x1, x2;
    double y1, y2;
    double delta;
    double tottol;

    /* if one of them is not numerical value, return */
    if(!isdigit((int) *value1) && *value1 != '+' && *value1 != '-' 
      && *value1 != '.' && *value1 != '(') return 1;
    if(!isdigit((int) *value2) && *value2 != '+' && *value2 != '-' 
      && *value2 != '.' && *value2 != '(') return 1; 

    /* only one of them are complex number */
    if(*value1 != '(' && *value2 == '(') return 1;
    if(*value2 != '(' && *value1 == '(') return 1; 

    p1 = value1; 
    p2 = value2; 

    /* replace D with E */
    while(*p1 != '\0') { 
	if(*p1 == 'D') *p1 = 'E';
	p1++;
    }
    while(*p2 != '\0') { 
	if(*p2 == 'D') *p2 = 'E';
	p2++;
    }

    if(*value1 != '(' && *value2 != '(') { 
        loc1 = '\0';
        loc2 = '\0';
        x1 = strtod(value1, &loc1);	
        x2 = strtod(value2, &loc2);
	
	/* test if there are any remaining characters in the string, 
	   following the number that was read */
        if (*loc1 != '\0' && *loc2 != '\0') return 1;
	
	delta = fabs(x1 - x2);
	tottol = tolerance + fabs(x1)*reltol;
    } 
    else { 
       p1 = value1; 
       p1++; 
       x1 = strtod(p1, NULL);
       while(*p1 != ',') p1++;
       p1++;  	/* skip , */
       y1 = strtod(p1,NULL);

       p2 = value2; 
       p2++; 
       x2 = strtod(p2, NULL);
       while(*p2 != ',') p2++;
       p2++;  	/* skip , */
       y2 = strtod(p2,NULL); 

	delta = hypot(x1 - x2, y1 - y2);
	tottol = tolerance + reltol*hypot(x1,y1);
    } 

    if (delta > tottol)
	return 1;
    else
	return 0;
}
    
	
/* print the keywords */
   void print_keys(FitsKey *p1, FitsKey *p2) 
{
    if(!chatter && !hdundiff) headas_chat(0,title); 
    if(hdundiff > 0) headas_chat(0,"---\n");
    hdundiff++;
    if(p1 != NULL) {
        headas_chat(0,"< card %d: %s  = %s \n", p1->kindex, p1->kname, 
	  p1->kvalue,"\n");
    }
    if(p2 != NULL) {
        headas_chat(0,"> card %d: %s  = %s \n", p2->kindex, p2->kname, 
	  p2->kvalue,"\n");
    }
    return;
}
/******************************************************************************
* Function
*      cmp_datasum 
*
*
* DESCRIPTION:
*      Compare the checksum of the data unit.   
*
*******************************************************************************/
int cmp_datasum(fitsfile *fits1, 
                 fitsfile *fits2 ) 

{ 
    unsigned long  datasum1 = 0; 
    unsigned long  datasum2 = 0; 
    unsigned long  hdusum; 
    int status = 0;
    int tstatus1 = 0;
    int tstatus2 = 0;

    /* read the datasum keyword */
    if(fits_read_key(fits1, TULONG, "DATASUM", &datasum1, NULL, &tstatus1)) { 
	if(caldsum) {
            if(fits_get_chksum(fits1, &datasum1, &hdusum, &status)) {  
                if(!chatter && !hdundiff) headas_chat(0,title); 
	        fits_report_error(stderr, status); 
	        status = 0;
            }
        }
    }

    if(fits_read_key(fits2, TULONG, "DATASUM", &datasum2, NULL, &tstatus2)) { 
	if(caldsum) {
            if(fits_get_chksum(fits2, &datasum2, &hdusum, &status)) {  
                if(!chatter && !hdundiff) headas_chat(0,title); 
	        fits_report_error(stderr, status); 
	        status = 0;
            }
        }
    }

    if( chatter && !caldsum && tstatus1 != 0 ) { 
        if(!chatter && !hdundiff) headas_chat(0,title); 
        sprintf(comm,"---\n");
        if(hdundiff > 1) headas_chat(0,comm);
	sprintf(comm, "Warning: DATASUM keyword does not exist in file 1.\n");
	headas_chat(0,comm);
	sprintf(comm, "         Not performing data comparison.\n");
	headas_chat(0,comm);
    }

    if( chatter && !caldsum && tstatus2 != 0 ) { 
        if(!chatter && !hdundiff) headas_chat(0,title); 
        sprintf(comm,"---\n");
        if(hdundiff > 1) headas_chat(0,comm);
	sprintf(comm, "Warning: DATASUM keyword does not exist in file 2.\n");
	headas_chat(0,comm);
	sprintf(comm, "         Not performing data comparison.\n");
	headas_chat(0,comm);
    }

    if(!caldsum && (tstatus1 != 0 || tstatus2 != 0) ) return 0;

    if(datasum1 != datasum2 ) { 
        sprintf(comm,"< calculated DATASUM=%lu\n",datasum1);
	headas_chat(0,comm);
        sprintf(comm,"> calculated DATASUM=%lu\n",datasum2);
	headas_chat(0,comm);
        if(!chatter && !hdundiff) headas_chat(0,title); 
        sprintf(comm,"---\n");
        if(hdundiff > 1) headas_chat(0,comm);
        if(!chatter  && !hdundiff) headas_chat(0,title); 
	sprintf(comm, "These two data units are not identical.\n\n"); 
	headas_chat(0,comm);
 
    } 
    return 0;
}
    

/******************************************************************************
* Function
*      cmp_data 
*
*
* DESCRIPTION:
*      Compare two data units.  
*
*******************************************************************************/
int cmp_data(fitsfile *fits1, 
              fitsfile *fits2,
              int hdutype)
{
    if(hdutype == IMAGE_HDU) { 
         return cmp_img(fits1, fits2);
    }
    if(hdutype == ASCII_TBL || hdutype == BINARY_TBL) {
         return cmp_tbl(fits1,fits2);
         
    }
    if(chatter) { 
        sprintf(comm,"---\n");
        if(hdundiff > 1) headas_chat(0,comm);
	sprintf(comm, "Warning: Unknown HDU type \n");
	strcat(comm, "Cannot perform data comparison.");
	headas_chat(0,comm);
        return DIFFWRN;
    }
    return 0;
}

/******************************************************************************
* Function
*      cmp_img 
*
*
* DESCRIPTION:
*      Compare two image arrays.  
*
*******************************************************************************/
int cmp_img(fitsfile *fits1, 
             fitsfile *fits2)
{     
    int naxis1, naxis2;
    long *naxes1; 
    long *naxes2; 
    int status = 0;
    long i;
    double *img1;
    double *img2;
    long nelem = 1;
    double nullval = -9.11E99;
    int anynul = 0;
    double tottol;
    long * pixpos;
    char str[80]; 
    long j,k;

    /* Read and check the NAXIS and NAXESn keywords */
    if(fits_get_img_dim(fits1, &naxis1, &status)){
         fits_report_error(stderr,status);
         return DIFFWRN;
    }
    if(fits_get_img_dim(fits2, &naxis2, &status)){
         fits_report_error(stderr,status);
         return DIFFWRN;
    }
    if(naxis1 != naxis2) {
	sprintf(comm, 
      "Dimensions (%d and %d) of images are different.\n",
        naxis1,naxis2);
	strcat(comm,  "    Cannot compare image arrays.\n");
	if(headas_chatpar ) headas_chat(1,comm);
        hdundiff++;
        return DIFFWRN;
    }

    if(naxis1 == 0) return 0;    /* zero array */
    naxes1 = (long*) malloc(sizeof(long)*naxis1);
    naxes2 = (long*) malloc(sizeof(long)*naxis2);
    for (i = 0; i < naxis1; i++) {
        if(fits_get_img_size(fits1, naxis1, naxes1, &status)){
            fits_report_error(stderr,status);
            return DIFFWRN;
        } 
        if(fits_get_img_size(fits2, naxis2,naxes2, &status)){
            fits_report_error(stderr,status);
            return DIFFWRN;
        } 
        nelem *= naxes1[i];
        if(naxes1[i] != naxes2[i]) {
            sprintf(comm,"<  : NAXIS%ld = %ld\n\n", i+1,naxes1[i]); 
            headas_chat(0,comm);
            sprintf(comm,">  : NAXIS%ld = %ld\n\n", i+1,naxes2[i]); 
            headas_chat(0,comm);
            hdundiff++;
            status = 1; 
        }
    }
    if(status) {
	sprintf(comm, "Values of NAXESn are different.\n");
	strcat(comm,  "Cannot compare image arrays.");
	headas_chat(1,comm);
        return DIFFWRN;
    }
    if(nelem == 0) return 0; /* zero array */ 

    /* Read and check the images */
    img1 = (double*) calloc(nelem, sizeof(double));
    img2 = (double*) calloc(nelem, sizeof(double));
    pixpos =(long *) calloc(naxis1,sizeof(long));
    if(fits_read_img(fits1, TDOUBLE, 1,nelem,&nullval,img1,&anynul,&status)){
        fits_report_error(stderr,status);
        return DIFFWRN;
    }
    if(fits_read_img(fits2, TDOUBLE, 1,nelem, &nullval,img2,&anynul,&status)){
        fits_report_error(stderr,status);
        return DIFFWRN;
    }
    
    for (i = 0; i < nelem; i++) {
        tottol = tolerance + fabs(img1[i])*reltol;
        if(fabs(img1[i] - img2[i]) > tottol) {
            hdundiff++;
            if(0 < hdumaxdiff && hdundiff > hdumaxdiff) return DIFFMAX;

            if(!chatter  && !hdundiff) headas_chat(0,title);
            sprintf(comm,"---\n");
            if(hdundiff > 1) headas_chat(0,comm);

            k=i;
            for (j=0; j<naxis1; j++) {
               pixpos[j]= k%naxes1[j]; 
               k = (k-pixpos[j])/naxes1[j];
               if ( naxis1 == 1 ) {
                   sprintf(str,"pixel(%ld)",pixpos[j]+1);
               } else if (j ==0 ) {
                   sprintf(str,"pixel(%ld,",pixpos[j]+1);
               } else if ( j==naxis1 -1) {
                   sprintf(str,"%s%ld)",str,pixpos[j]+1);
               } else {
                   sprintf(str,"%s%ld,",str,pixpos[j]+1);
               }
            }

            if(img1[i] != nullval)
                sprintf(comm,"< %s: %g\n",str,img1[i]); 
            else 
                sprintf(comm,"< %s: NULL\n",str); 
            headas_chat(0,comm);
            if(img2[i] != nullval)
                sprintf(comm,"> %s: %g\n",str,img2[i]); 
            else 
                sprintf(comm,"> %s: NULL\n",str); 
            headas_chat(0,comm);
        }
    }
    free(naxes1);
    free(naxes2);
    if(nelem > 0) free(img1);
    if(nelem > 0) free(img2);
    return 0;
}
 
/******************************************************************************
* Function
*      cmp_tbl 
*
*
* DESCRIPTION:
*      Compare two tables.  
*
*******************************************************************************/
int cmp_tbl(fitsfile *fits1, 
             fitsfile *fits2)
{
    FitsCol *fits1_col;
    FitsCol *fits2_col;
    int ncols;          
    long nrows1, nrows2;
    int ncols1, ncols2;
    int status = 0;
    int i, j;
    char keytmplt[8];
    long minrows, resrows;

    /* get the number of columns */ 
    if(fits_get_num_cols(fits1, &ncols1, &status)){
        fits_report_error(stderr,status);
        return DIFFWRN;
    }
    if(fits_get_num_cols(fits2, &ncols2, &status)){
        fits_report_error(stderr,status);
        return DIFFWRN;
    }
    if(ncols1 != ncols2) {
	sprintf(comm, "The number of columns differs (%d and %d).\n",                ncols1, ncols2);
	strcat(comm,  "Not performing detailed comparison of the tables.\n");
	headas_chat(1,comm);
        return DIFFWRN;
    } 
    ncols = ncols1;

    if( !ncols ) return 0;
    fits1_col = (FitsCol*) calloc(ncols1,sizeof(FitsCol));
    fits2_col = (FitsCol*) calloc(ncols2,sizeof(FitsCol));
  
    /* read column information */ 
    j = 0;
    for (i = 0; i < ncols1; i++) {

       /* get the column of the first fits file
          (name/num/type/repeat)  */
       sprintf(keytmplt,"%d",i+1);

       if(fits_get_colname(fits1,CASEINSEN, keytmplt, 
           fits1_col[i].col_name, &(fits1_col[i].col_num), &status)){
            fits_report_error(stderr,status);
            return DIFFWRN;
       }
       
       if(fits_get_coltype(fits1,i+1, &(fits1_col[i].col_type),
             &(fits1_col[i].col_repeat), &(fits1_col[i].col_width),
              &status)) {
            fits_report_error(stderr,status);
            return DIFFWRN;
       } 


       /* get the column of the second fits file
          (name/num/type/repeat/width)  */ 
       fits_get_colname(fits2,CASEINSEN, fits1_col[i].col_name,
           fits2_col[i].col_name, &(fits2_col[i].col_num), &status);
       if(status == COL_NOT_FOUND) { 
	   sprintf(comm, "Column %s is not found in the second file.\n",
                  fits1_col[i].col_name);
           strcat(comm,"No comparison of data will be performed.\n");
	   headas_chat(0,comm);
           return DIFFWRN;
       }
       if(status == COL_NOT_UNIQUE) { 
	   sprintf(comm, 
             "Column %s is found more than once in the second file.\n",
                  fits1_col[i].col_name);
           strcat(comm,"No comparison of data will be performed.");
	   headas_chat(0,comm);
           return DIFFWRN;
       }
       if(status) {
            fits_report_error(stderr,status);
            return DIFFWRN;
       } 

       if(fits_get_coltype(fits2,i+1, &(fits2_col[i].col_type),
             &(fits2_col[i].col_repeat), &(fits2_col[i].col_width), 
              &status)) {
            fits_report_error(stderr,status);
            return DIFFWRN;
       } 
       
       /* test type, repeat etc */
       if(datacode(fits1_col[i].col_type) !=  
          datacode(fits2_col[i].col_type)  ){ 
	   sprintf(comm, 
     "Format (TFORM) of column %s is incompatible.\n",
                  fits1_col[i].col_name);
           strcat(comm,"No comparison of data will be performed.\n");
	   headas_chat(0,comm);
           return DIFFWRN;
       }

       if(fits1_col[i].col_repeat !=  fits2_col[i].col_repeat){ 
	   sprintf(comm, 
     "Different repeat of column %s.\n",
                  fits1_col[i].col_name);
           strcat(comm,"No comparison of data will be performed.\n");
	   headas_chat(0,comm);
           return DIFFWRN;
       }
     
       if (fits1_col[i].col_type == TSTRING) {
         if(fits1_col[i].col_width !=  fits2_col[i].col_width){ 
	   sprintf(comm, 
       "The string column %s has different widths.\n",
                   fits1_col[i].col_name);
           strcat(comm,"No comparison of data will be performed.\n");
	   headas_chat(0,comm);
           return DIFFWRN;
         }
       }
       
       switch (fits1_col[i].col_type) {
          case TCOMPLEX:
          case TDBLCOMPLEX:
             fits1_col[i].col_repeat *= 2;
             break;
          default:
             break;
       }
       fits2_col[i].col_repeat = fits1_col[i].col_repeat;  
    }

    /* get the number of rows */ 
    if(fits_get_num_rows(fits1, &nrows1, &status)){
        fits_report_error(stderr,status);
        return DIFFWRN;
    }
    if(fits_get_num_rows(fits2, &nrows2, &status)){
        fits_report_error(stderr,status);
        return DIFFWRN;
    }
 
    minrows = nrows1 > nrows2? nrows2: nrows1;
    resrows = nrows1 > nrows2? nrows2 - nrows1: nrows1 - nrows2;
    if(minrows == 0) { 
	   sprintf(comm, 
     "One of the two files has 0 number of rows (%ld and %ld).\n",
                  nrows1, nrows2); 
	   headas_chat(0,comm); 
           return DIFFWRN;
    }
    if(nrows1 != nrows2) { 
	   sprintf(comm, 
     "Files have different number of rows (%ld and %ld).\n",
                  nrows1, nrows2);
	   headas_chat(0,comm);
           sprintf(comm,"Only comparing %ld row(s).\n", minrows); 
	   headas_chat(0,comm);
    } 


    
    
    /* row loops */ 
    status = 0;
    for (i = 0; i < minrows; i++) {
        status = cmp_row(fits1, fits2, i+1, fits1_col, fits2_col, ncols);
        if(status) break;   
    } 
    
    if(!status ) {
        if(nrows1 > nrows2) i = 1;
        if(nrows2 > nrows1) i = 2; 
        if(nrows1 != nrows2) {
            sprintf(comm,
            "Not comparing last %ld row(s) of file %d.\n", resrows,i);
            headas_chat(0,comm);
        }
    } 
    if(ncols > 0) free(fits1_col);
    if(ncols > 0) free(fits2_col);
    return status;
} 


/* define the code */ 

int  datacode(int datatype) 
{    
   int code = 0;
   switch (datatype) {
      case TDOUBLE:  
      case TBYTE:
      case TSHORT:  
      case TLONG:  
      case TFLOAT: 
           code = 1;
           break; 
      case TCOMPLEX:  
      case TDBLCOMPLEX: 
           code = 2;
           break; 
      case TBIT:  
           code = 3;
      case TLOGICAL:
           code = 4;
           break; 
      case TSTRING:
           code = 5;
           break; 
      default: 
         break;
   }
   return code;
}

/******************************************************************************
* Function
*      cmp_row 
*
*
* DESCRIPTION:
*      Compare two rows from different FITS file  
*
*******************************************************************************/
int cmp_row(fitsfile *fits1,  fitsfile *fits2, int irow , 
            FitsCol *fits1_col, FitsCol *fits2_col, int ncols)          
{
    int i;
    char temp[512];
    int j;
    double *data=0;
    unsigned char* cdata=0;
    int *idata=0;

    long repeat1=0;
    long repeat2=0;
    long offset1=0;
    long offset2=0;
    int maxrepeat;
    int status =0;
    int ctype;
    

    for (i= 0; i < ncols; i++) {
            ctype = fits1_col[i].col_type;
            if (ctype < 0) {
                   ffgdes(fits1,fits1_col[i].col_num, irow,
                          &repeat1,&offset1,&status);
                   ffgdes(fits2,fits2_col[i].col_num, irow,
                          &repeat2,&offset2,&status);
                   if ( repeat1 != repeat2 ) {
                                  sprintf(comm,
		     "Array lengths in column %s row %d are different.\n",
               			   fits1_col[i].col_name,irow);
                      strcat(comm,"No further comparison of data will be performed.\n");
                       headas_chat(0,comm);
                       return DIFFWRN;
                   } else {
                      fits1_col[i].col_repeat =repeat1;
                      fits2_col[i].col_repeat =repeat2;
                      ctype = - ctype;
                   }
            }

	    if (ctype == TSTRING)
              maxrepeat = fits1_col[i].col_width;
	    else
              maxrepeat = fits1_col[i].col_repeat;

	 
            if ( !maxrepeat ) return 0;
            data = (double*) calloc(2*maxrepeat,sizeof(double));
            cdata = (unsigned char*) calloc(2*(maxrepeat+1),sizeof(unsigned char)); 
            idata = (int *) calloc(2*maxrepeat,sizeof(int));

            if(idata == NULL || cdata == NULL || data == NULL) {
                 strcpy(comm,"Cannot allocate memory for data, exiting!");
                 headas_chat(1,comm);
                 return DIFFWRN;
            }

            if(cmp_col(fits1, fits1_col[i].col_num, irow, 
            fits2, fits2_col[i].col_num, irow, 
            maxrepeat, ctype, 
            data, cdata, idata)) { 
               if(0 < hdumaxdiff && hdundiff > hdumaxdiff) return DIFFMAX;
               hdundiff++;
               if(!chatter  && !hdundiff) headas_chat(0,title); 
               sprintf(comm,"---\n");
               if(hdundiff > 1) headas_chat(0,comm);

               j = maxrepeat;
               sprintf(temp,"< row %d, col %d: ",irow,fits1_col[i].col_num);
               print_col(ctype, fits1_col[i].col_repeat,
                      data,cdata,idata,temp );
               sprintf(temp,"> row %d, col %d: ",irow,fits1_col[i].col_num);
               print_col(ctype, fits2_col[i].col_repeat,
                      &(data[j]),&(cdata[j+1]),&(idata[j]),temp); 
           }
           if (maxrepeat && data) free(data);
           if (maxrepeat && cdata) free(cdata);
           if (maxrepeat && idata) free(idata); 
    } 
    return 0;
}

/******************************************************************************
* Function
*      cmp_col 
*
*
* DESCRIPTION:
*      Compare two columns.  
*
*******************************************************************************/
int cmp_col(fitsfile *fits1, int col1, int row1, 
            fitsfile *fits2,int col2, int row2, 
            int repeat, int coltype, 
            double *data, unsigned char* cdata, int *idata)
{
   double nullval = -9.11e31;
   char cnull = 0;
   double tottol;

   int anynul;
   int status = 0; 
   int i;
   int flag = 0;
   unsigned char *pt;

   /* skip the variable length array for now */ 
   if(coltype < 0) return 0; 
   

   switch (coltype) {
      case TBYTE:  
      case TSHORT:  
      case TLONG:  
      case TFLOAT:  
      case TDOUBLE:  
         fits_read_col(fits1, TDOUBLE, col1, row1, 1, repeat, &nullval,
                       data, &anynul, &status); 
         fits_read_col(fits2, TDOUBLE, col2, row2, 1, repeat, &nullval,
                       &(data[repeat]), &anynul, &status); 
         for ( i = 0; i < repeat; i++) {
	   tottol = tolerance + fabs(data[i])*reltol;
            if(fabs(data[i] - data[i+repeat]) > tottol) {
               flag = 1;
               break;
            }
         }
         break; 
      case TCOMPLEX:  
      case TDBLCOMPLEX:  
         fits_read_col(fits1, TDOUBLE, col1, row1, 1, repeat, &nullval,
                       data, &anynul, &status);
         fits_read_col(fits2, TDOUBLE, col2, row2, 1, repeat, &nullval,
                       &(data[repeat]), &anynul, &status); 
         for ( i = 0; i < repeat; i += 2 ) {
	   tottol = tolerance + hypot(data[i],data[i+1])*reltol;
	    if (hypot(data[i] - data[i+repeat],
			data[i+1] - data[i+1+repeat]) > tottol) {
               flag = 1;
               break;
            }
         }
         break;
      case TLOGICAL:
         fits_read_col(fits1, TLOGICAL, col1, row1, 1, repeat, &cnull,
                       cdata, &anynul, &status);
         fits_read_col(fits2, TLOGICAL, col2, row2, 1, repeat, &cnull,
                       &(cdata[repeat+1]), &anynul, &status); 
         for ( i = 0; i < repeat; i++) {
            if(cdata[i] != cdata[i+repeat+1]) {
               flag = 1;
               break;
            }
         }
         break;
      case TBIT:
         /* because of TBIT is shared the same data array with string.
            the second array has to start from repeat+1  */
         fits_read_col(fits1, TBIT,col1, row1, 1, repeat, NULL,
                           cdata,NULL, &status); 
         fits_read_col(fits2, TBIT,col2, row2, 1, repeat, NULL,
                           &(cdata[repeat+1]),NULL, &status); 
         for ( i = 0; i < repeat; i++) {
            if(cdata[i] != cdata[i+repeat+1]) {
               flag = 1;
               break;
            }
         }
         break; 
      case TSTRING:
         pt = cdata;
         fits_read_col(fits1, TSTRING,col1, row1, 1, 1, NULL, 
                           &pt,NULL, &status); 

         cdata[repeat] = '\0';
         pt = &(cdata[repeat+1]);
         fits_read_col(fits2, TSTRING,col2, row1, 1, 1, NULL,
                           &pt,NULL, &status); 
         cdata[2*repeat+1] = '\0';

         if (strcmp((char *)cdata, (char *)pt)){
               flag = 1;
               break;
         }
         break; 
     default:
         break;
   } 
   if(flag) return 1;
   return 0;    
} 

/******************************************************************************
* Function
*      print_col 
*
*
* DESCRIPTION:
*      print a column data at a row. 
*
*******************************************************************************/
int print_col(int coltype,  int repeat,
              double *data, unsigned char* cdata, int *idata, char *prompt)
{
   double nullval = -9.11e31;
   char cnull = 0;

   long i,j;
   long ltmp;
   unsigned char btmp;
   char tmp[512]="";
   char indent[64];
   int ntodo;

   strcpy(comm,prompt);
   ntodo = repeat;
   j = 0;
   for (i = 0; i < strlen(prompt); i++) indent[i] = ' ';
   indent[i] = '\0';
   switch (coltype) {
      case TBYTE:
      case TSHORT:  
      case TLONG:  
         while(ntodo > 0) {
           for (i = 0; i < 8 && ntodo > 0 ; i++)  { 
              if(data[j] == nullval) { 
                 sprintf(tmp,"NULL");
               } else {
                 ltmp = (long)data[j];
                 sprintf(tmp,"%ld ",ltmp);
              }
              strcat(comm,tmp);
              ntodo--; j++;
           }
           headas_chat(0,comm);
           strcpy(comm,indent);
         }
         break; 
      case TDOUBLE: 
         while(ntodo > 0) {
           for (i = 0; i < 8 && ntodo > 0 ; i++)  { 
              if(data[j] == nullval) { 
                 sprintf(tmp,"NULL ");
              } else {
                 sprintf(tmp,"%.16g ",data[j]);
              }
              strcat(comm,tmp);
              ntodo--; j++;
           }
           headas_chat(0,comm);
           strcpy(comm,indent);
         }
         break; 
      case TFLOAT:  
         while(ntodo > 0) {
           for (i = 0; i < 8 && ntodo > 0 ; i++)  { 
              if(data[j] == nullval) { 
                 sprintf(tmp,"NULL ");
              } else {
                 sprintf(tmp,"%.8g ",data[j]);
              }
              strcat(comm,tmp);
              ntodo--; j++;
           }
           headas_chat(0,comm);
           strcpy(comm,indent);
         }
         break;
      case TLOGICAL: 
         while(ntodo > 0) {
           for (i = 0; i < 8 && ntodo > 0 ; i++)  { 
              if(cdata[j] == cnull) {
                 sprintf(tmp,"NULL ");
              }else {
                  btmp = cdata[j] !=0 ? 'T' :'F';
                  sprintf(tmp,"%c ",btmp);
              }
              strcat(comm,tmp);
              ntodo--; j++;
           }
           headas_chat(0,comm);
           strcpy(comm,indent);
         }
         break;
      case TBIT:
         while(ntodo > 0) {
           for (i = 0; i < 8 && ntodo > 0 ; i++)  {
              sprintf(tmp,"0x%02x ",cdata[j]);
              strcat(comm,tmp);
              ntodo--; j++;
           }
           headas_chat(0,comm);
           strcpy(comm,indent);
         }
         break; 
      case TSTRING:
         if(strlen((char*)cdata)==0)   
            sprintf(tmp,"NULL");
         else if(strlen((char*)cdata)>=80) {
            strncpy(tmp,(char*)cdata,77);
            strcat(tmp,"...");
         }
         else 
            sprintf(tmp,"%s",cdata);
         strcat(comm,tmp);
         headas_chat(0,comm);
         strcpy(comm,indent);
         break; 
      case TCOMPLEX:  
      case TDBLCOMPLEX:  
         while(ntodo > 0) {
           for (i = 0; i < 4 && ntodo > 0 ; i++)  { 
              sprintf(tmp,"(%g, %g)  ",data[j],data[j+1]);
              strcat(comm,tmp);
              ntodo-=2; j+=2;
           }
           headas_chat(0,comm);
           strcpy(comm,indent);
         }
         break;
      default:
         break;
   }
   headas_chat(0,"\n");
   return 0; 
}  
