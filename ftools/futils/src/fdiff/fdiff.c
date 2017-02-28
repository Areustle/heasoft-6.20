/******************************************************************************
* SELECTOR TASK:
*      fdiff
*
* FILE:
*      fdiff.c
*
* DESCRIPTION:
*      
* Compare the two Fits files and  the display differences between them. 
*
* If the extension names/numbers are not given for both files, the
* program will compare all the HDUs.
* If the extension name/number is given in one of the file, 
* the program will compare the specified HDU of two files 
* with the same hdu number.  
*
* The Headers are compared keyword by keyword. The commentary keywords (Histroy,
* Comment, and "") and "DATE" keyword are excluded. Users can also exclude
* additional keywords by supplying a list as the value of "exclude" parameter. 
* The list can be either a string which consists of the keywords 
* separated by commas, or an ascii file in which there is one line per keyword. * For the  second form, the file name is preceded with @, and entered as the 
* value of "excluded" parameter. If the string of excluded keyword is followed 
* by *, the name of the excluded keyword will be matched as with that
* string with any characters after.  For example, 
* the DATE means only excluding the keyword "DATE" but not "DATE-OBS". 
* The "DATE*" will exclude the keywords "DATE", "DATE-OBS" and other 
* DATExxxx but not "MYDATE". 
* 
* If the cmpnum parameter is set, the numerical keywords 
* will be compared as numerical value. Otherwise, they will be compared 
* as strings. 
*
* The checksums of Data Unit are also compared. 
* The default checksum of data is the value of DATASUM keyword. If 
* the DATASUM keyowrd does not exixt, users can choose whether to calculate 
* the datasum by setting the caldsum parameter. 
* If it is set, the datasum will be calculated. Otherwise, the comparison of the
* datasum  will be skipped. 
*
* If the cmpdata is set to "yes", the pixels in image arrays and columns
* in tables will be compared. 
*
* Usage:
*     fdiff file1 file2 
*
* AUTHOR:
*      Ning Gan,  20 Sep. 1998 
*
* MODIFICATION HISTORY:
*      1998-09-20  Ning Gan, v1.0.0:
*          Beta version.
*      1998-10-02  Ning Gan, v1.0.1: 
*          The excluded keywords are  case-insensitive.
*      1998-10-21  Ning Gan, v1.0.2: 
*          Fixed the "isspace" problem. 
*      1998-12-17  Ning Gan, v1.0.3: 
*          Stylish changes.
*      1999-03-04  Ning Gan, v1.0.4: 
*          Correctly handle two files with different number of HDU.
*      1999-03-05  Ning Gan, v1.0.5: 
*          Changed the convention of specifying the excluding keywords.
*      1999-11-15  Ning Gan, v1.1.0: 
*          Added abilities of comparing data units.
*      1999-12-20  Ning Gan, v1.1.1: 
*          Added the parameter numdiffs
*      2000-08-20  Ning Gan, v1.1.2: 
*          fixed a bug when comparing string data.
*      2003-09-08 Ziqi Pan, v1.1.3:
*          add reltol parameter to look for fractional differences
*      2007-05-21 Bryan Irby, v1.1.4:
*          Increase FDFCOM to prevent seg fault with long filenames (PPC Linux)
*
*******************************************************************************/
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <fitsio.h>
#include <xpi.h>
#include <cftools.h>
#define MAXEXCLUDE 100				/* Maximum number of keywords which
						   could be excluded from comparison.*/
#define FDFCOM	1024
#define DIFFERR    1    	/* fdiff error exit */
#define DIFFWRN    2    	/* fdiff warning exit */
#define DIFFMAX    3	  	/* # of difference exceeds the maximum */ 
typedef struct {
    char kname[FLEN_KEYWORD];   /* fits keyword name */
    char kvalue[FLEN_VALUE];    /* fits keyword name */
    int kindex;                 /* position at the header */
}FitsKey;

typedef struct {
    char col_name[FLEN_VALUE];   /* Column  name    */
    int  col_num;                /* Column position */
    int  col_type;		 /* Column type     */
    long col_repeat;		 /* column repeat   */   
    long col_width;		 /* column width    */   
}FitsCol;

/*================= Function prototypes =====================*/

int init_fdiff(char *file1, char *file2, char* exclude); 
void max_mess(void);

/* routines on comparing headers */
int cmp_head(fitsfile *fits1, fitsfile *fits2, char **elist, int nelist);
void get_keylist( char *exclude,	char **elist, int  *nelist);
int isinlist(char *keyname, char **elist, int nelist);
int compnum (char *value1, char *value2 );
int compkeys (const void *key1, const void *key2);
int compindexs (const void *key1, const void *key2);
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

/*================= Global variables ========================*/

static char errmes[FDFCOM];	
static char comm[FDFCOM];
static char title[FDFCOM];
static char task[] = "FDIFF V1.1.4";
static int cmpnum = 1;		/* compare the numerical keyword value? */  
static int cmpdata = 1;		/* compare the table columns */  
static int verbose = 1;		
static int caldsum = 1;		/* calculate the data sum */
static int hdumaxdiff = 999;      /* maximum number of the difference
			             allowed in a HDU. */
static int hdundiff = 0;

static double tolerance = 0;    /* allowed difference in numerical elements */
static double reltol = 0;       /* allowed relative difference */



void fdiff()
{ 
    char file1[FLEN_FILENAME] = "";  	/* Input Fits file */
    char rtnam1[FLEN_FILENAME] = "";  	/* Input Fits file root name */
    int hdunum1 = -99;			/* extension number? */
    int fhdu1  = 1;			/* first hdu   */
    int thdu1  = 1;			/* total number of hdu  */
    fitsfile *fits1;			/* input fits file pointer */
    int hdutype1;

    char file2[FLEN_FILENAME] = "";  	/* Input Fits file */
    char rtnam2[FLEN_FILENAME] = "";  	/* Input Fits file root name */
    int hdunum2 = -99;			/* extension number? */
    int fhdu2  = 1;			/* first hdu   */

    int thdu2  = 1;			/* total number of hdu  */
    fitsfile *fits2;			/* input fits file pointer */
    int hdutype2;


    int totalhdu = 1;
    char exclude[FDFCOM];
    char *elist[MAXEXCLUDE];
    int nelist = 0;
    int ndiff = 0;

    char *p;
    int status = 0;
    int i,j;
    char parname[FLEN_FILENAME] = "";

    /*------------------Initialization  --------------------------------*/

    c_ptaskn(task);

    /* get the input parameters from parameter file*/
    if(init_fdiff(file1, file2, exclude) ) {
        strcpy(errmes,"Errors in init_fdiff.");
        c_fcerr(errmes);
	return ;
    } 

    if(verbose) c_fcecho("");
    sprintf(comm,"               %s",task);
    if(verbose) c_fcecho(comm);
    if(verbose) c_fcecho("");
    
    /* discard the rowfilters etc */
    if(ffrtnm(file1, rtnam1, &status)) { 
        fits_report_error(stderr,status);
	return;
    }

    if(ffrtnm(file2, rtnam2, &status)) { 
        fits_report_error(stderr,status);
	return;
    } 

    /* Is the extension number given? */
    if(fits_parse_extnum(file1, &hdunum1, &status)) {    
	 fits_report_error(stderr,status);
	 return;
    }

    if(fits_parse_extnum(file2, &hdunum2, &status)) {    
	 return;
    }
     
    /* open the fits files */
    if(fits_open_file(&fits1, file1, READONLY, &status)) {
        fits_report_error(stderr,status);
        return ;
    }

    if(fits_open_file(&fits2, file2, READONLY, &status)) {
        fits_report_error(stderr,status);
        return ;
    } 

    /* If one file hdu number specified and the other has not, 
       make the hdu number of the second file as same 
       as that of the first one */
    if(hdunum1 != -99 && hdunum2 == -99) hdunum2 = hdunum1;
    if(hdunum1 == -99 && hdunum2 != -99) hdunum1 = hdunum2;

    /* If no hdu  number is given,  get the total number of hdu */ 
    if(hdunum1 == -99) { 
	fhdu1 = 1;
        if(fits_get_num_hdus(fits1, &thdu1, &status)) { 
            fits_report_error(stderr,status);
            return ;
        }
    } 
    else { 
        fhdu1 = hdunum1;
        thdu1 = 1;
    }

    if(hdunum2 == -99) { 
	fhdu2 = 1;
        if(fits_get_num_hdus(fits2, &thdu2, &status)) { 
            fits_report_error(stderr,status);
            return ;
        }
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
	sprintf(title, "%s[%d] <     %s[%d] > \n ",
	    rtnam1,fhdu1+i-1, rtnam2,fhdu2+i-1);
        if(verbose) c_fcecho(title); 

        /* move to the right hdu */
        if(fits_movabs_hdu(fits1,fhdu1+i, &hdutype1, &status) ) {
	    fits_report_error(stderr,status);
	    break;
        } 
        if(fits_movabs_hdu(fits2,fhdu2+i, &hdutype2, &status) ) {
	    fits_report_error(stderr,status);
	    break;
        }

 
        /* test header */
	if(status = cmp_head(fits1,fits2,elist, nelist)) {  
           if(status == DIFFERR)  break;
           if(status == DIFFMAX)  {ndiff += hdumaxdiff; max_mess();} 
           if(status == DIFFWRN)  ndiff+=hdundiff; 
           status = 0;
	   strcpy(comm,"=========================================\n ");
	   if(verbose)  c_fcecho(comm);
           continue;
        }

        /* test data */ 
	if(hdutype1 != hdutype2) {
	    sprintf(comm,
       "Warning: Ext. %d of %s and  Ext. %d of %s have different types.",
		     fhdu1+i-1, rtnam1,fhdu2+i-1,rtnam2);
	    strcat(comm,"Comparisons of data are skipped");
            c_fcecho(comm);
            continue;
        } 

	if(status = cmp_datasum(fits1, fits2)) {     /*  data sum */ 
           if(status == DIFFERR)  break;
           if(status == DIFFMAX)  {ndiff+=hdumaxdiff; max_mess();} 
           if(status == DIFFWRN)  ndiff+=hdundiff; 
           status = 0;
	   strcpy(comm,"=========================================\n ");
	   if(verbose)  c_fcecho(comm);
           continue;
        }

        if(cmpdata) { 
            if(status = cmp_data(fits1,fits2,hdutype1)) {
                if(status == DIFFERR) break;
                if(status == DIFFMAX) {ndiff+=hdumaxdiff; max_mess();} 
                if(status == DIFFWRN) ndiff+=hdundiff; 
                status = 0;
	        strcpy(comm,"=========================================\n ");
	        if(verbose)  c_fcecho(comm);
                continue; 
            }
        }

	if(verbose && !hdundiff) c_fcecho("No differences");
	strcpy(comm,"=========================================\n ");
	if(verbose || hdundiff) c_fcecho(comm);
	ndiff += hdundiff;
    }
    if(thdu2 < thdu1) { 
        ndiff +=1;
	if(thdu1 - thdu2 == 1)
	    sprintf(comm,"File %s does not have Ext. #%d", 
		 rtnam2, thdu2); 
        else 
	    sprintf(comm,
		"File %s does not have Ext. #%d to Ext. #%d.", 
		 rtnam2, thdu2,thdu1-1); 
        c_fcecho(comm);
    } 
    
    if(thdu2 > thdu1) { 
        ndiff +=1;
	if(thdu1 - thdu2 == 1)
	    sprintf(comm,"File %s does not have Ext. #%d", 
		 rtnam1, thdu1); 
        else 
	    sprintf(comm,
		"File %s does not have Ext. #%d to Ext. #%d.", 
		 rtnam1, thdu1,thdu2-1); 
        c_fcecho(comm);
    } 

    if (ndiff == 1)
       sprintf(comm,"**** End of file comparison:  %d difference was found",
          ndiff);
    else
       sprintf(comm,"**** End of file comparison:  %d differences were found",
          ndiff);
	  
	  
    strcpy(parname, "numdiffs");
    Uclpsi(parname, ndiff, &status);
    if(status) {
       c_fcerr("Error to update the numdiffs keyword.");
       status = 0;
    }

    if(verbose || ndiff) c_fcecho(comm);
    for (i = 0; i < MAXEXCLUDE; i++) free(elist[i]); 

    /* close the fitsfiles  */ 
    fits_close_file(fits1, &status);
    fits_close_file(fits2, &status);
}

/******************************************************************************
* Function
*      init_fdiff
*
* DESCRIPTION:
*      Get parameters from the par file.
*
*******************************************************************************/
int init_fdiff(char *file1, 	
		char *file2, 
		char *exclude
		)
{
    char temp[FLEN_FILENAME];
    char parname[FLEN_FILENAME];
    int status = 0;
    int BufLen_2;

    /* get name of the fits file 1*/
    BufLen_2 = FLEN_FILENAME-1; 
    strcpy(parname,"file1");
    Uclgst(parname, file1, &status);
    if(status) {
        strcpy(errmes,"could not get file1 parameter");
        c_fcerr(errmes);
        return status;
    }

    /* get name of the  fits file 2*/
    strcpy(parname,"file2");
    Uclgst(parname,file2,&status);
    if(status) {
        strcpy(errmes,"could not get file2 parameter");
        c_fcerr(errmes);
        return status;
    }

    /* get name of the excluding keyword list*/
    BufLen_2 = FDFCOM - 1; 
    strcpy(parname,"exclude");
    Uclgst(parname,exclude,&status);
    if(status) {
        strcpy(errmes,"could not get exclude parameter");
        c_fcerr(errmes);
        return status;
    } 

    /* get the cmpnum(compare the numerical value?) parameter */
    strcpy(parname,"cmpnum");
    Uclgsb(parname,&cmpnum,&status);
    if(status) {
        strcpy(errmes,"could not get cmpnum parameter");
        c_fcerr(errmes);
        return status;
    } 

    /* get the tolerance parameter */
    strcpy(parname,"tolerance");
    Uclgsd(parname,&tolerance,&status);
    if(status) {
        strcpy(errmes,"could not get tolerance parameter");
        c_fcerr(errmes);
        return status;
    } 

    /* get the relative tolerance parameter */
    strcpy(parname,"reltol");
    Uclgsd(parname,&reltol,&status);
    if(status) {
        strcpy(errmes,"could not get reltol parameter");
        c_fcerr(errmes);
        return status;
    } 

    /* get the the caldsum parameter */
    strcpy(parname,"caldsum");
    Uclgsb(parname,&caldsum,&status);
    if(status) {
        strcpy(errmes,"could not get caldsum parameter");
        c_fcerr(errmes);
        return status;
    }  

    /* get the cmpdata(compare the numerical value?) parameter */
    strcpy(parname,"cmpdata");
    Uclgsb(parname,&cmpdata,&status);
    if(status) {
        strcpy(errmes,"could not get cmpdata parameter");
        c_fcerr(errmes);
        return status;
    } 

    /* get the the verbose parameter */
    strcpy(parname,"verbose");
    Uclgsb(parname,&verbose,&status);
    if(status) {
        strcpy(errmes,"could not get verbose parameter");
        c_fcerr(errmes);
        return status;
    }  

    /* get the the hdumaxdiff parameter */
    strcpy(parname,"hdumaxdiff");
    Uclgsi(parname,&hdumaxdiff,&status);
    if(status) {
        strcpy(errmes,"could not get hdumaxdiff parameter");
        c_fcerr(errmes);
        return status;
    }  
    return 0;
}

/* print the maximum diff message */
void max_mess(void)
{ 
    sprintf(comm,
"The # of differences in this HDU exceeds the max. = %d. Comparison stops! ",
    hdumaxdiff);
    c_fcecho(comm);
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
    char *p1;
    char temp[80];

    strcpy(elist[0],"HISTORY");
    strcpy(elist[1],"COMMENT");
    strcpy(elist[2],"");
    strcpy(elist[3],"DATE");
    strcpy(elist[4],"CHECKSUM");
    *nelist = 5;

    p = exclude; 
    while(*p == ' ') p++; 
    if(*p == '\0') return ;
    i = *nelist;
    if(*p != '@') { 
	if(strchr(p,',') == NULL) { 
	    while(isspace(*p)) p++; 
	    strncpy(elist[i],p,FLEN_KEYWORD);
	    elist[i][FLEN_KEYWORD+1] = '\0';
	    p = elist[i];
	    while(*p && !isspace(*p) ) p++; 
	    *p = '\0';
	    i++;
        } 
	else {
	    p = strtok(exclude,",");
	    while(p) { 
	        while(isspace(*p)) p++; 
	        strncpy(elist[i],p,FLEN_KEYWORD);
	        elist[i][FLEN_KEYWORD+1] = '\0';
	        p = elist[i];
	        while(*p && !isspace(*p) ) p++; 
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
            sprintf(comm,"Can not find the list file %s. Ignored. \n", p);
        } 
	else { 
	    while(fgets(temp,80,list)) {  
		p = temp;
	        while(isspace(*p)) p++; 
		strncpy(elist[i],p,FLEN_KEYWORD);
	        elist[i][FLEN_KEYWORD+1] = '\0';
	        p = elist[i];
	        while( *p && !isspace(*p) ) p++; 
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

    int i, i1, i2, j; 
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
        if(fits_read_keyn(fits1, i+1, kwds1[j].kname, kwds1[j].kvalue,NULL, 
            &status)) { 
            fits_report_error(stderr,status);
            status = 0; 
        } 
        kwds1[j].kindex = i+1; 
	/* skip the uninterested keywords */
	if(!isinlist(kwds1[j].kname, elist, nelist))j++;
    } 
    nkwds1 = j;
    ptkwds1 = (FitsKey **)malloc(nkwds1*sizeof(FitsKey*)); 
    for (i = 0; i < nkwds1; i++) { 
        ptkwds1[i] = &(kwds1[i]);
    } 

    j = 0;
    for (i = 0; i < nkwds2; i++) { 
        if(fits_read_keyn(fits2, i+1, kwds2[j].kname, kwds2[j].kvalue,NULL, 
            &status)) { 
            fits_report_error(stderr,status);
            status = 0; 
        } 
        kwds2[j].kindex = i+1; 
	/* skip the uninterested keywords */
	if(!isinlist(kwds2[j].kname, elist, nelist))j++;
    }
    nkwds2 = j;
    ptkwds2 = (FitsKey **)malloc(nkwds2*sizeof(FitsKey*)); 
    for (i = 0; i < nkwds2; i++) { 
        ptkwds2[i] = &(kwds2[i]);
    } 
     
    /* sort the keywords in the alphabetical ascending order of keyword name*/
    qsort(ptkwds1, nkwds1, sizeof(FitsKey *), compkeys);		
    qsort(ptkwds2, nkwds2, sizeof(FitsKey *), compkeys);		
   
    /* if there are more than one same keyword, sort them in the  alphabetical ascending order of index */ 
        i=0;
        while(i < nkwds1  ) {
            j=i+1;
            while(j <nkwds1 && strcmp(ptkwds1[i]->kname,ptkwds1[j]->kname)==0){
            j++;
            }
            if( i < j-1) {
	    qsort(ptkwds1+i,j-i,sizeof(FitsKey *),compindexs);
            }
            i=j;
        }
        i=0;
        while(i < nkwds2  ) {
            j=i+1;
            while(j <nkwds2 && strcmp(ptkwds2[i]->kname,ptkwds2[j]->kname)==0){
            j++;
            }
            if( i < j-1) {
	    qsort(ptkwds2+i,j-i,sizeof(FitsKey *),compindexs);
            }
            i=j;
        }

    i1 = 0; 
    i2 = 0;
    while(i1 < nkwds1 &&  i2 < nkwds2) {
	/* compare the keyword names */
	j = strcmp(ptkwds1[i1]->kname, ptkwds2[i2]->kname); 
	if(j > 0) {
	    if(hdundiff > hdumaxdiff) goto DIFFKWDSEXIT;
            print_keys(NULL, ptkwds2[i2]);  
	    i2++;
        } 
        else if ( j < 0) {
	    if(hdundiff > hdumaxdiff) goto DIFFKWDSEXIT;
	    print_keys(ptkwds1[i1],NULL);  
            i1++;
        } 
	else { 
	    /* compare the values */ 
	    if(strcmp(ptkwds1[i1]->kvalue, ptkwds2[i2]->kvalue)) {  
                if(!cmpnum) { 
	            if(hdundiff > hdumaxdiff) goto DIFFKWDSEXIT;
	            print_keys(ptkwds1[i1], ptkwds2[i2]);  
                } 
	        else {
	            if(compnum(ptkwds1[i1]->kvalue, ptkwds2[i2]->kvalue)) {
	                if(hdundiff > hdumaxdiff) goto DIFFKWDSEXIT;
	                print_keys(ptkwds1[i1], ptkwds2[i2]);
                    }
                }
            }
	    i1++; i2++;
       }
    } 
    if(i1 < nkwds1) { 
	  if(hdundiff > hdumaxdiff) goto DIFFKWDSEXIT;
	  for (i = i1; i < nkwds1; i++ )  
	      print_keys(ptkwds1[i], NULL);
    }
    if(i2 < nkwds2) { 
	  if(hdundiff > hdumaxdiff) goto DIFFKWDSEXIT;
	  for (i = i2; i < nkwds2; i++ )  
	      print_keys(NULL,ptkwds2[i]);
    }

DIFFKWDSEXIT:
    free (ptkwds1);
    free (ptkwds2);
    free (kwds1);
    free (kwds2);
    if(hdundiff > hdumaxdiff) return DIFFMAX;
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
    int match;
    int len;
    int flag;

    match = 0;
    for (i = 0; i < nelist; i++) { 
       p = elist[i];
       q = keyname;
       if(*p == '\0' && *q == '\0') return 1;
       if(*p == '\0' && *q != '\0') continue;
       len = strlen(p); 
       if(*(p+len-1) == '*')   /* it is an short hand */ 
            flag = strncmp(q,p,len-1); 
       else 
            flag = strcmp(q,p);
       if(!flag) { 
            match = 1;
            break;
       } 
    }
    return match;
}

/*  comparison function for the FitsKey structure array */
    int compindexs (const void *key1, const void *key2)
{
    int index1;
    int index2;
    index1 = (*(FitsKey **)key1)->kindex;
    index2 = (*(FitsKey **)key2)->kindex;
    if( index1 < index2) {
    return -1;
    } 
    else if (index1 == index2) {
    return 0;
    } 
    else {
    return 1;
    }
}
    int compkeys (const void *key1, const void *key2)
{
    char *name1;
    char *name2;
    name1 = (*(FitsKey **)key1)->kname;
    name2 = (*(FitsKey **)key2)->kname;
    return strncmp(name1,name2,FLEN_KEYWORD);
}

/*  comparison function for the numerical fitskey value */ 
    int compnum (char *value1, char *value2 )
{
    char *p1;
    char *p2; 
    double x1, x2;
    double y1, y2;
    double delta;
    double tottol;

    /* if one of them is not numerical value, return */
    if(!isdigit(*value1) && *value1 != '+' && *value1 != '-' 
      && *value1 != '.' && *value1 != '(') return 1;
    if(!isdigit(*value2) && *value2 != '+' && *value2 != '-' 
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
        x1 = strtod(value1, NULL);	
	x2 = strtod(value2, NULL); 
        delta = fabs(x1-x2);
        tottol = tolerance + fabs(x1)*reltol;
	/*if(x1 == x2) return 0; */
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
       /*if(x1 == x2 && y1 == y2) return 0; */
    } 
       /*return 1;*/ 
    if (delta > tottol)
        return 1;
    else
        return 0;

}
    
	
/* print the keywords */
   void print_keys(FitsKey *p1, FitsKey *p2) 
{
    if(!verbose && !hdundiff) c_fcecho(title); 
    sprintf(comm,"---");
    if(hdundiff > 0) c_fcecho(comm);
    hdundiff++;
    if(p1 != NULL) {
        sprintf(comm,"< card %d: %s  = %s ", p1->kindex, p1->kname, 
	  p1->kvalue);
        c_fcecho(comm);
    }
    if(p2 != NULL) {
        sprintf(comm,"> card %d: %s  = %s ", p2->kindex, p2->kname, 
	  p2->kvalue);
        c_fcecho(comm);
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
    if(fits_read_key(fits1, TLONG, "DATASUM", &datasum1, NULL, &tstatus1)) { 
	if(caldsum) {
            if(fits_get_chksum(fits1, &datasum1, &hdusum, &status)) {  
                if(!verbose && !hdundiff) c_fcecho(title); 
	        fits_report_error(stderr, status); 
	        status = 0;
            }
        }
    }

    if(fits_read_key(fits2, TLONG, "DATASUM", &datasum2, NULL, &tstatus2)) { 
	if(caldsum) {
            if(fits_get_chksum(fits2, &datasum2, &hdusum, &status)) {  
                if(!verbose && !hdundiff) c_fcecho(title); 
	        fits_report_error(stderr, status); 
	        status = 0;
            }
        }
    }

    if( verbose && !caldsum && tstatus1 != 0 ) { 
        if(!verbose && !hdundiff) c_fcecho(title); 
        sprintf(comm,"---");
        if(hdundiff > 1) c_fcecho(comm);
	sprintf(comm, "Warning: DATASUM keyword doesn't exist in file 1");
	c_fcecho(comm);
	sprintf(comm, "         so data comparison not performed.");
	c_fcecho(comm);
    }

    if( verbose && !caldsum && tstatus2 != 0 ) { 
        if(!verbose && !hdundiff) c_fcecho(title); 
        sprintf(comm,"---");
        if(hdundiff > 1) c_fcecho(comm);
	sprintf(comm, "Warning: DATASUM keyword doesn't exist in file 2");
	c_fcecho(comm);
	sprintf(comm, "         data comparison is not performed.");
	c_fcecho(comm);
    }

    if(!caldsum && (tstatus1 != 0 || tstatus2 != 0) ) return 0;

    if(datasum1 != datasum2 ) { 
        if(!verbose && !hdundiff) c_fcecho(title); 
        sprintf(comm,"---");
        if(hdundiff > 1) c_fcecho(comm);
        if(!verbose && !hdundiff) c_fcecho(title); 
	sprintf(comm, "These two data units are not identical.\n"); 
	c_fcecho(comm); 
 
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
    if(verbose) { 
        sprintf(comm,"---");
        if(hdundiff > 1) c_fcecho(comm);
	sprintf(comm, "Warning: Unknown HDU type \n");
	strcat(comm, "Can not performed the data comparison.");
	c_fcecho(comm);
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
    int instatus = 0;
    long i;
    char keyn[8];
    double *img1;
    double *img2;
    long nelem = 1;
    double nullval = -9.11E99;
    int anynul = 0;
    double tottol;

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
      "Dimentions (%d and %d) of two images are different.\n",
        naxis1,naxis2);
	strcat(comm,  "    Can not compared two image arrays.");
	if(verbose)c_fcecho(comm);
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
            sprintf(comm,"<  : NAXIS%d = %d ", i+1,naxes1[i]); 
            c_fcecho(comm);
            sprintf(comm,">  : NAXIS%d = %d ", i+1,naxes2[i]); 
            c_fcecho(comm);
            hdundiff++;
            status = 1; 
        }
    }
    if(status) {
	sprintf(comm, "Values of NAXESn are different.\n");
	strcat(comm,  "Can not compared two image arrays.");
	if(verbose)c_fcecho(comm);
        return DIFFWRN;
    }
    if(nelem == 0) return 0; /* zero array */ 

    /* Read and check the images */
    img1 = (double*) calloc(nelem, sizeof(double));
    img2 = (double*) calloc(nelem, sizeof(double));
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
        /*if(img1[i] != img2[i]) {*/
        if(fabs(img1[i] - img2[i]) > tottol) {
            hdundiff++;
            if(hdundiff >  hdumaxdiff) return DIFFMAX;

            if(!verbose && !hdundiff) c_fcecho(title);
            sprintf(comm,"---");
            if(hdundiff > 1) c_fcecho(comm);

            if(img1[i] != nullval)
                sprintf(comm,"< pixel %d: %g",i+1,img1[i]); 
            else 
                sprintf(comm,"< pixel %d: NULL",i+1); 
            c_fcecho(comm);
            if(img2[i] != nullval)
                sprintf(comm,"> pixel %d: %g",i+1,img2[i]); 
            else 
                sprintf(comm,"> pixel %d: NULL",i+1); 
            c_fcecho(comm);
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
	sprintf(comm, "The number of columns are different(%ld and %ld).\n",                ncols1, ncols2);
	strcat(comm,  "Skip the detailed comparison for the tables.");
	c_fcecho(comm);
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
	   sprintf(comm, "The column %s is not found in the second file.\n",
                  fits1_col[i].col_name);
           strcat(comm,"No comparison of data will be performed.");
	   c_fcecho(comm);
           return DIFFWRN;
       }
       if(status == COL_NOT_UNIQUE) { 
	   sprintf(comm, 
             "The column %s is found more than once in the second file.\n",
                  fits1_col[i].col_name);
           strcat(comm,"No comparison of data will be performed.");
	   c_fcecho(comm);
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
     "The column %s in two files have incompatible format(TFORM)\n",
                  fits1_col[i].col_name);
           strcat(comm,"No comparison of data will be performed.");
	   c_fcecho(comm);
           return DIFFWRN;
       }

       if(fits1_col[i].col_repeat !=  fits2_col[i].col_repeat){ 
	   sprintf(comm, 
     "The column %s in two files have different repeat \n",
                  fits1_col[i].col_name);
           strcat(comm,"No comparison of data will be performed.");
	   c_fcecho(comm);
           return DIFFWRN;
       }
     
       if (fits1_col[i].col_type == TSTRING) {
         if(fits1_col[i].col_width !=  fits2_col[i].col_width){ 
	   sprintf(comm, 
       "The string column %s in two files have different widths \n",
                   fits1_col[i].col_name);
           strcat(comm,"No comparison of data will be performed.\n");
	   c_fcecho(comm);
           return DIFFWRN;
         }
       }

       switch (fits1_col[i].col_type) {
          case TCOMPLEX:
          case TDBLCOMPLEX:
             fits1_col[i].col_repeat *= 2;
             break;
          case TBIT: 
             fits1_col[i].col_repeat = (fits1_col[i].col_repeat + 7)/8;  
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
     "One of the two files has 0 number of rows (%d and %d)",
                  nrows1, nrows2); 
	   c_fcecho(comm); 
           return DIFFWRN;
    }
    if(nrows1 != nrows2) { 
	   sprintf(comm, 
     "Two files has different number of rows (%d and %d)",
                  nrows1, nrows2);
	   c_fcecho(comm);
           sprintf(comm,"Only %d rows are compared. ", minrows); 
	   c_fcecho(comm);
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
            "The last %d rows of the file %d is not compared.", resrows,i);
            c_fcecho(comm);
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
		     "The column %s row %d in two files have different length array \n",
               			   fits1_col[i].col_name,irow);
                      strcat(comm,"No further comparison of data will be performed.\n");
	               c_fcecho(comm);
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
                 strcpy(comm,"Can not allocate memory for data, quit!");
                 c_fcecho(comm);
                 return DIFFWRN;
            }

            if(cmp_col(fits1, fits1_col[i].col_num, irow, 
            fits2, fits2_col[i].col_num, irow, 
            maxrepeat, ctype, 
            data, cdata, idata)) { 
               if(hdundiff > hdumaxdiff) { 
                   return DIFFMAX;
               }
               hdundiff++;
               if(!verbose && !hdundiff) c_fcecho(title); 
               sprintf(comm,"---\n");
               if(hdundiff > 1) c_fcecho(comm);

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
         fits_read_col(fits1, TBYTE,col1, row1, 1, repeat, NULL,
                           cdata,NULL, &status); 
         fits_read_col(fits2, TBYTE,col2, row2, 1, repeat, NULL,
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
                 sprintf(tmp,"NULL ");
               } else {
                 ltmp = (long)data[j];
                 sprintf(tmp,"%d ",ltmp);
              }
              strcat(comm,tmp);
              ntodo--; j++;
           }
           c_fcecho(comm);
           strcpy(comm,indent);
         }
         break; 
      case TDOUBLE:  
      case TFLOAT:  
         while(ntodo > 0) {
           for (i = 0; i < 8 && ntodo > 0 ; i++)  { 
              if(data[j] == nullval) { 
                 sprintf(tmp,"NULL ");
              } else {
                 sprintf(tmp,"%g ",data[j]);
              }
              strcat(comm,tmp);
              ntodo--; j++;
           }
           c_fcecho(comm);
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
           c_fcecho(comm);
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
           c_fcecho(comm);
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
         c_fcecho(comm);
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
           c_fcecho(comm);
           strcpy(comm,indent);
         }
         break;
      default:
         break;
   }
   return 0; 
}  
