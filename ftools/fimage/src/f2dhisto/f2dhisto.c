/******************************************************************************
* SELECTOR TASK:
*      f2dhisto
*
* FILE:
*      f2dhisto.c
*
* DESCRIPTION:
*      Make a 2d histogram or spectrum of the values from two column of a table.
*      The tool accepts both the new extended fitsfile syntax and the old fortran 
*      parameters, but they are excluded to each other. The result is 
*      stored in the primary array of the output file.
*
*      When the (outfile) is given in input filename, the outfil parameter
*      is omitted. 
*
*      When the rowfilter is given in input filename, the rows parameter
*      is ignored.
*
*      When the binning is given in the input filename, the parameters
*      of column, binsz, exposure, minkw, maxkw, lowval and highval are
*      all ignored.
*
*      The parameters of copyall and copyprime are not supported due to
*      the conflicts with the extended syntax of fits file name.
*     
*
* Usage:
*     f2dhisto  infile  
* where infile = filetype://filename(outfile)[extension][rowfilter][binning]
*
* Notes:
*     For the history of f2dhisto, read the fortran codes f2dhisto.f.
*     Both the rg2rf and set_binspec are in libftools.a.
*
* AUTHOR:
*      Ning Gan  8/28/98 v4.0
*
* MODIFICATION HISTORY:
*
*
*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <xpi.h>
#include <cftools.h>
#include <fitsio.h>

/*================= Function prototypes =====================*/
int init_his_2d(char *infile, char *outfile,  int *status);
int set_binspec( char *imagetype, int haxis, char **colname,
char **min, char **max, char **binsz, char *weight, char *binspec,
int *status );
int rg2rf( char* row_range, char* row_filter );


/*================= Global variables ========================*/
char errmes[FLEN_ERRMSG];	
char comm[1024];
void f2dhisto() 
{ 
    char task[] = "f2dhisto v4.0";
    char infile[FLEN_FILENAME] ="";  	/* Input Fits files */
    char outfile[FLEN_FILENAME]="";  	/* output fits file */

    fitsfile *infits;
    fitsfile *outfits;


    int status = 0;


    int i;
    int temp;

    double refpix[2];
    double refval[2];
    double binsz[2];
    long   nbins[2];
    double   highval[2];
    double   lowval[2];

    /* Task defination */
    c_ptaskn(task);


    /* construct the input file and output file from the parameter */
    if(init_his_2d(infile, outfile,  &status) != 0) {
        strcpy(errmes,"Errors in init_hist_2d");
        c_fcerr(errmes);
	return;
    }


    /* ============ Histogram  =============*/
     
    /********************************************************* 
     * When binning option and outfile are specified in input* 
     * filename, the histogram is automatically  write to    *
     * the primary array of the output file.                 *
     *                                                       *
     *********************************************************/

    sprintf(comm,"input file: %s . ", infile);
    c_fcecho(comm);
    /* open the input fitsfile with spec. of binning and output file */  
    if(fits_open_file(&infits, infile, READONLY, &status)) {
	fits_report_error(stderr,status); 
        return;
    } 

    /* close the input fitsfile, the cfitsio also close the output file */ 
    fits_close_file(infits, &status);

    /* open the output file again to write the history keyword */
    /* strip the leading ! */
    if(*outfile == '!') { 
	strcpy(comm, outfile);
	strcpy(outfile, &comm[1]);
    }
    if(fits_open_file(&outfits, outfile, READWRITE,&status)) {
        fits_report_error(stderr,status); 
        return;
    }
    fits_movabs_hdu(outfits, 1, NULL, &status);

    /* get the dimesion */
    if(fits_read_key(outfits,TLONG,"NAXIS1",&nbins[0],NULL, &status)){
        fits_report_error(stderr,status);
        return;
    }

    if(fits_read_key(outfits,TLONG,"NAXIS2",&nbins[1],NULL, &status)){
        fits_report_error(stderr,status);
        return;
    }

    /* get the WCS keywords */
    if(fits_read_key(outfits,TDOUBLE,"CRPIX1",&refpix[0],NULL, &status)){
       fits_report_error(stderr,status);
       return;
    }
    if(fits_read_key(outfits,TDOUBLE,"CRPIX2",&refpix[1],NULL, &status)){
       fits_report_error(stderr,status);
       return;
    }
    if(fits_read_key(outfits,TDOUBLE,"CRVAL1",&refval[0],NULL, &status)){
       fits_report_error(stderr,status);
       return;
    }
    if(fits_read_key(outfits,TDOUBLE,"CRVAL2",&refval[1],NULL, &status)){
       fits_report_error(stderr,status);
       return;
    }
    if(fits_read_key(outfits,TDOUBLE,"CDELT1",&binsz[0],NULL, &status)){
       fits_report_error(stderr,status);
       return;
    }
    if(fits_read_key(outfits,TDOUBLE,"CDELT2",&binsz[1],NULL, &status)){
       fits_report_error(stderr,status);
       return;
    }
    for (i = 0; i < 2; i++) {
       lowval[i] = refval[i]- (refpix[i]-0.5)*binsz[i];
       highval[i] = lowval[i] + nbins[i]*binsz[i];
    } 
    
    /* write the histogram keywords */
    fits_update_key(outfits,TLONG, "XNBINS",&nbins[0], 
        "Number of BINS along X -Axis", &status);

    fits_update_key(outfits,TLONG, "YNBINS",&nbins[1], 
        "Number of BINS along Y -Axis", &status);

    fits_update_key(outfits,TDOUBLE, "X_LOW",&lowval[0], 
        "Lower limit of X - Axis", &status);

    fits_update_key(outfits,TDOUBLE, "X_HIGH",&highval[0], 
        "Upper limit of Y - Axis", &status);

    fits_update_key(outfits,TDOUBLE, "Y_LOW",&lowval[1], 
        "Lower limit of Y - Axis", &status);

    fits_update_key(outfits,TDOUBLE, "Y_HIGH",&highval[1], 
        "Upper limit of Y - Axis", &status);

    /* write the history keywords */
    strcpy(comm,"Task: f2dhisto on ");
    strcat(comm, infile);
    fits_write_history(outfits, comm,&status);
    status = 0;
    c_timestamp(outfits);

    /* close the output fitsfile  */ 
    fits_close_file(outfits, &status);

}	

/******************************************************************************
* Function
*      init_his_2d
*
*
* DESCRIPTION:
*      Get the parameters from the par file and construct the input
* filename  in extended fits syntax.
*
*
*******************************************************************************/
int init_his_2d(char *infile, 		/* input filename+filter */
		char *outfile, 		/* output filename */
		int *status		/* error status */
		)
{
    char temp[FLEN_FILENAME]   	= "" ;
    char urltype[FLEN_FILENAME]	= "" ;
    char file[FLEN_FILENAME]    = "" ;
    char extspec[FLEN_FILENAME]	= "" ;
    char rowfilter[FLEN_FILENAME]= "" ;
    char binspec[FLEN_FILENAME]	= "" ;
    char colspec[FLEN_FILENAME]	= "" ;
    char colname[4][FLEN_VALUE];
    char minname[4][FLEN_VALUE];
    char maxname[4][FLEN_VALUE];
    char binname[4][FLEN_VALUE];
    char wtname[FLEN_VALUE]	= "" ;
    int recip;
    double minin[4];
    double maxin[4];
    double binsizein[4];

    double indef_val = DOUBLENULLVALUE;
    double binsz[2];
    double lowval[2];
    double highval[2];
    char rows[FLEN_FILENAME]	= "" ;
    char expos [FLEN_KEYWORD]	= "" ;
    char *inputcol[2];
    char *range[2];
    char pixel[2];
    char image_str[30];
    char *low_str[2];
    char *high_str[2];
    char *bin_str[2];
    char weight_str[FLEN_VALUE]	= "" ;



    int clobber = 0;
    int BufLen_2;
    int dummy;
    int imagetype, haxis;
    double weight;
    int changed = 0;
    char *p ;
    char *p1;
    int i;

    /*============ Basic parameters   =================*/
    BufLen_2 = FLEN_FILENAME - 1;
    /* get name of the input fits file */
    Uclgst("infile",infile,status);
    if(*status != 0) {
        strcpy(errmes,"could not get INFILE parameter");
        c_fcerr(errmes);
        return *status;
    }

    /* get the clobber parameter */
    Uclgsb("clobber",&clobber,status);
    *status = 0;




    /*============ optional  parameters   =================*/
    /* parse the infile name */
    *outfile = '\0';
    strcpy(temp,infile);
    if(fits_parse_input_url(temp,urltype,file, outfile, extspec,
       rowfilter, binspec, colspec, status)){
        strcpy(errmes,"could not parse the input file");
        c_fcerr(errmes);
        return *status;
    }

    if(strlen(outfile) == 0) {
        /* get name of the output fits file */
        Uclgst("outfil",outfile,status);
        if(*status != 0) {
           strcpy(errmes,"could not get OUTFILE parameter");
           c_fcerr(errmes);
           return *status;
        }
        if(clobber) {
            if(*outfile != '!') {
                strcpy(temp,"!");
                strcat(temp,outfile);
                strcpy(outfile, temp);
            }
            sprintf(temp,"The file %s is overwritten",&outfile[1]);
            c_fcecho(temp);
        }
    } 

    if(strlen(extspec) == 0) {
        /* Default extension spec. is 1 */
	strcpy(extspec,"1");
    }

    if(strlen(binspec) != 0) { 
        if(fits_parse_binspec(binspec, &imagetype, &haxis,
                    colname, minin, maxin, binsizein,
                    minname, maxname, binname,&weight,
                    wtname, &recip , status)) {
            sprintf(errmes,"Invalid binspec. %s ", binspec);
            c_fcerr(errmes);
            return *status;
        }
        if(haxis != 2) {
            sprintf(errmes,"binspec. %s is not 2-d histogram spec.", binspec);
            c_fcerr(errmes);
	    return *status;
        }
    }
    else {
        /* get x binsize */
        Uclgsd("xbinsz",&binsz[0],status);
        if(*status ==3 ) {   /* Undefined xbinsz */
            *status = 0;
            binsz[0] = indef_val;
        }
        if(*status != 0) {
            strcpy(errmes,"could not get XBINSZ  parameter");
            c_fcerr(errmes);
            return *status;
        }

        /* get y binsize */
        Uclgsd("ybinsz",&binsz[1],status);
        if(*status ==3 ) {   /* Undefined ybinsz */
            *status = 0;
            binsz[1] = indef_val;
        }
        if(*status != 0) {
            strcpy(errmes,"could not get YBINSZ parameter");
            c_fcerr(errmes);
            return *status;
        }


        /* get x column name of the histogram */
        BufLen_2 = FLEN_VALUE - 1 ;
        inputcol[0] = (char*)malloc(FLEN_VALUE);
        Uclgst("xcol",inputcol[0],status);
        if(*status != 0) {
            strcpy(errmes,"could not get XCOL parameter");
            c_fcerr(errmes);
            return *status;
        }

        /* get y column name of the histogram */
        inputcol[1] = (char*)malloc(FLEN_VALUE);
        Uclgst("ycol",inputcol[1],status);
        if(*status != 0) {
            strcpy(errmes,"could not get YCOL parameter");
            c_fcerr(errmes);
            return *status;
        }


        /* get x-upper and x-lower ranges of the histogram */
        range[0] = (char*)malloc(FLEN_VALUE);
        Uclgst("xrange",range[0],status);
        if(*status != 0) {
            strcpy(errmes,"could not get XRANGE parameter");
            c_fcerr(errmes);
            return *status;
        }
        p = range[0];
        while(*p == ' ') p++;
        lowval[0] = DOUBLENULLVALUE;
        highval[0] = DOUBLENULLVALUE;
        if(*p  != ',') {
            p1 = strtok(p,",");
	    if(strstr(p1,"INDEF") ==NULL) sscanf(p1, "%lf", &lowval[0]);
            if( (p1 = strtok(NULL,",")) != NULL ) {
	        if(strstr(p1,"INDEF") ==NULL) sscanf(p1, "%lf", &highval[0]); 
            }
        }
        else {
            if( (p1 = strtok(p,",")) != NULL ) {
	        if(strstr(p1,"INDEF") ==NULL) sscanf(p1, "%lf", &highval[0]); 
            }
        }

        /* get y-upper and y-lower ranges of the histogram */
        range[1] = (char*)malloc(FLEN_VALUE);
        Uclgst("yrange",range[1],status);
        if(*status != 0) {
            strcpy(errmes,"could not get YRANGE parameter");
            c_fcerr(errmes);
            return *status;
        }
        p = range[1];
        while(*p == ' ') p++;
        lowval[1] = DOUBLENULLVALUE;
        highval[1] = DOUBLENULLVALUE;
        if(*p  != ',') {
            p1 = strtok(p,",");
	    if(strstr(p1,"INDEF") ==NULL) sscanf(p1, "%lf", &lowval[1]);
            if( (p1 = strtok(NULL,",")) != NULL ) {
	        if(strstr(p1,"INDEF") ==NULL) sscanf(p1, "%lf", &highval[1]); 
            }
        }
        else {
            if( (p1 = strtok(p,",")) != NULL ) {
	        if(strstr(p1,"INDEF") ==NULL) sscanf(p1, "%lf", &highval[1]); 
            }
        }
        /* get the exposure time  */
        BufLen_2 = FLEN_KEYWORD - 1;
        Uclgst ("expos",expos,status);
        if(*status != 0) {
            strcpy(errmes,"could not get EXPOS parameter");
            c_fcerr(errmes);
            return *status;
        }

        /* get pixel size */
        BufLen_2 = 1;
        Uclgst("pixel",pixel,status);
        if(*status != 0) {
            strcpy(errmes,"could not get pixel parameter");
            c_fcerr(errmes);
            return *status;
        }

	/* construct the binning */
        for (i = 0; i< 2; i++) { 
            low_str[i] = (char *)malloc(FLEN_VALUE);
            *low_str[i]='\0';
            if(lowval[i] != indef_val) 
                sprintf(low_str[i],"%lg",lowval[i]);
            

            high_str[i] = (char *)malloc(FLEN_VALUE);
            *high_str[i]='\0';
            if(highval[i] != indef_val) 
                sprintf(high_str[i],"%lg",highval[i]);
        

            bin_str[i] = (char *)malloc(FLEN_VALUE);
            bin_str[i][i]='\0';
            if(binsz[i] != indef_val)  sprintf(bin_str[i],"%lg",binsz[i]);
        }

        weight_str[0] = '\0';
        p = expos;                       /* dealing with exposure */
        while(*p == ' ') p++;
        if(strlen(p) != 0) {
            strcat(weight_str, "/");
            strcat(weight_str, expos);
        }
        set_binspec(pixel, 2, inputcol, low_str, high_str,
                    bin_str, weight_str, binspec, status);
    }

    if(strlen(rowfilter) == 0) { 
        /* get the rows */
        BufLen_2 = FLEN_VALUE - 1;
        Uclgst("rows",rows,status);
        if(*status != 0) {
            strcpy(errmes,"could not get ROWS parameter");
            c_fcerr(errmes);
            return *status;
        }
        /* convert the row range to the row filter  */
        if(strlen(rows)!=0) rg2rf(rows, rowfilter);
    }


    /* constructing the input file name */
    if(strlen(urltype)) {
        strcpy(temp, urltype);
        strcat(temp, file);
    }
    else {
        strcpy(temp,file);
    }

    if(strlen(outfile)) {
        strcat(temp,"(");
        strcat(temp,outfile);
        strcat(temp,")");
    }
    else {
	sprintf("No output file.",errmes);
	c_fcerr(errmes);
	return 1;
    }
    if(strlen(extspec)) {
        strcat(temp,"[");
        strcat(temp,extspec);
        strcat(temp,"]");
    }


    if(strlen(rowfilter) != 0) {
        strcat(temp,"[");
        strcat(temp,rowfilter);
        strcat(temp,"]");
    }

    if(strlen(binspec) != 0) {
        strcat(temp,"[");
        strcat(temp,binspec);
        strcat(temp,"]");
    }
    strcpy (infile, temp);


    for (i = 0; i < 2; i++){
        free(low_str[i]);
        free(high_str[i]);
        free(bin_str[i]);
        free(range[i]);
    }

    return 0;
}

/* The following code is needed to allow IRAF to call cdummyftool.
This extra subroutine layer is needed because of differences in
linkers between vms and unix. */
#ifdef vms
#define F77CALL iraf_f2dhisto
#endif
#ifdef unix
#define F77CALL iraf_f2dhisto_
#endif

void F77CALL()
{
    void f2dhisto();
    f2dhisto();
}


