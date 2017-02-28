/******************************************************************************
* SELECTOR TASK:
*      fhisto
*
* FILE:
*      fhisto.c
*
* DESCRIPTION:
*
*      Makes a histogram or spectrum of the values in a column.  The histogram
*      is written to a file. The tool accepts both the new extended
*      fitsfile syntax and the old fortran parameters, but they 
*      are excluded to each other.
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
*      The histogram extension will be appended at the end of output file
*      if the output file exists. Otherwise, it will be in the extension 1.  
*
* Usage:
*     fhisto  infile outfile 
* where infile = filetype://filename[extension][rowfilter][binning] 
*                The default extension is 1.
*       outfile = filename
*
* Note:
*     The functions set_binspec and rg2rf are in the libftools.a
*
*     For the history of the fhisto tool, see the codes of fhisto.f(v3.0).
*     
*
* AUTHOR:
*      Ning Gan  7/28/98  v4.0
*
* MODIFICATION HISTORY:
*
*      toliver  07/09/99  v4.1  added code to generate bin-centers in raw
*                               coordinates in the first column of the output
*                               file, and, optionally, WCS coordinates in a
*                               fourth column
*
*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "xpi.h"
#include "fitsio.h"
#include "fitsio2.h"
#include "cftools.h"

/*================= Function prototypes =====================*/
int init_his_1d (char *infil, char *infil_nofilt, char *outfile,
                 char *extnam, char *bin_col, char* value_col,
                 char* error_col, char *wcs_col, int *iwcscol,
                 char *inputcol, int *status);
int set_binspec( char *imagetype, int haxis, char **colname,
char **min, char **max, char **binsz, char *weight, char *binspec,
int *status );
int rg2rf( char* row_range, char* row_filter );

/*================= Global variables ========================*/
static char errmes[FLEN_ERRMSG];	
static char comment[1024];

void fhisto() 
{ 
    char task[] = "fhisto v4.1";
    char infile[FLEN_FILENAME] = "";  	    /* Input Fits file */
    char infile_nofilt[FLEN_FILENAME] = ""; /* Input Fits file without
                                               binning specification */
    char outfile[FLEN_FILENAME] = "";  	    /* output fits file */
    int bitpix;

    fitsfile *infits;
    fitsfile *infits_nobin;
    fitsfile *outfits;



    double *histo_bin_raw;	 /* raw values of the histogram centroid */
    double *histo_bin_wcs;	 /* WCS values of the histogram centroid */
    double *histo_value;		/* values of the histogram value */
    double *histo_error;		/* values of the histogram error */

    char extnam[FLEN_VALUE] = "1dhisto";/* name of the histogram extension */
    char bin_col[FLEN_VALUE]   = "";	/* name of the histogram centroid */
    char value_col[FLEN_VALUE] = "";	/* name of the histogram value */
    char error_col[FLEN_VALUE] = "";	/* name of the histogram error */
    char wcs_col[FLEN_VALUE] = "";	/* name of the hist. centroid (WCS) */
    char inputcol[FLEN_VALUE]="";

    char *ttype[4]; 
    char *tform[4] = {"1D", "1D", "1D", "1D"}; 
    char *tunit[4]; 
    int ncol;
    int iwcscol;
    char keyname[FLEN_KEYWORD]="";	/* temp. keyname */ 

    long nbins;
    int i;
    double lowval,binsz;
    double refpix;
    double refval;
    double crpix, crval, cdelt;

    int status = 0;
    int iscreated = 0;
    int anynul;
    int temp;
    int inputcolnum;
    int totalhdus = 0;



    /* Task definition */
    c_ptaskn(task);

    /* get the input file and output file from the parameter */

    if (init_his_1d (infile, infile_nofilt, outfile, extnam, 
	             bin_col, value_col, error_col, wcs_col, &iwcscol,
                     inputcol, &status) != 0)
    {
        strcpy(errmes,"Errors in init_hist_1d");
        c_fcerr(errmes);
	return;
    }

    sprintf(comment, "input file (no binspec): %s", infile_nofilt);
    c_fcecho(comment);  
    sprintf(comment, "input file: %s", infile);
    c_fcecho(comment);  
    /* ============ Histogram  =============*/
     
    /********************************************************* 
     * When binning option is used, the histogram will be    *
     * in the primary array of the memory file. (The original*
     * primary array in the original file is not changed).   *
     *********************************************************/ 

    /* open the input fitsfile with binning */  
    if(fits_open_file(&infits, infile, READONLY, &status)) {
	fits_report_error(stderr,status); 
        return;
    } 

    /* open the input fitsfile without binning, in order to read the WCS
       keyword values */  
    fits_open_file (&infits_nobin, infile_nofilt, READONLY, &status);

    /* get the column number in the table */
    fits_get_colnum (infits_nobin, CASEINSEN, inputcol, &inputcolnum, &status);

    /* CRPIXn - Reference Pixel */
    fits_make_keyn ("TCRPX", inputcolnum, keyname, &status);
    fits_read_key (infits_nobin, TDOUBLE, keyname, &crpix, NULL, &status);
    if (status)
    {
      crpix = 0.0;
      status = 0;
    }

    /* CRVALn - Value at the location of the reference pixel */
    fits_make_keyn ("TCRVL", inputcolnum, keyname, &status);
    fits_read_key (infits_nobin, TDOUBLE, keyname, &crval, NULL, &status);
    if (status)
    {
      crval = 0.0;
      status = 0;
    }

    /* CDELTn - unit size of pixels */
    fits_make_keyn ("TCDLT", inputcolnum, keyname, &status);
    fits_read_key (infits_nobin, TDOUBLE, keyname, &cdelt, NULL, &status);
    if (status)
    {
      cdelt = 1.0;
      status = 0;
    }

    /* close the input fitsfile opened without the binning spec */ 
    fits_close_file (infits_nobin, &status);

    /* move to the primary array and read the binned histogram */  
    fits_movabs_hdu(infits, 1, NULL, &status);
    
    fits_read_key(infits, TLONG, "NAXIS1", &nbins, NULL,&status);
    histo_value = (double *) calloc (nbins, sizeof(double));
    histo_error = (double *) calloc (nbins, sizeof(double));
    histo_bin_raw = (double *) calloc (nbins, sizeof(double));
    histo_bin_wcs = (double *) calloc (nbins, sizeof(double));
      
    if (fits_read_img (infits, TDOUBLE, 1, nbins, 0 ,histo_value, &anynul,
        &status)){
        fits_report_error(stderr,status); 
        return;
    }


    /* ============  Output fits file =============*/

    /* open the output fitsfile */
    if(fits_create_file(&outfits, outfile, &status)) {
	if(status == FILE_NOT_CREATED) { 
	    iscreated = 1;
	    status = 0;
            sprintf(comment, "The file %s has already existed, ",outfile);
	    strcat(comment, "the histogram will be appended at the end");
	    strcat(comment,"of the file.");
	    c_fcecho(comment);  
	    if(fits_open_file(&outfits,outfile,READWRITE,&status)){
	        fits_report_error(stderr,status); 
		return;
            }
        }
	else { 
	     fits_report_error(stderr,status); 
             return;
        }
    }

    /* if the file had been created, move to the last hdu. if not, 
       create the dummy primary array */
    if(!iscreated) { 
	fits_create_img(outfits, LONG_IMG, 0, NULL, &status);
    }
    else {
	if(fits_get_num_hdus(outfits, &totalhdus, &status)){ 
	    fits_report_error(stderr,status); 
	    return;
        }
	if(fits_movabs_hdu(outfits, totalhdus, NULL, &status)){ 
	    fits_report_error(stderr,status); 
	    return;
        }
    }
	

    /* Create the histogram hdus*/
    /* Column name */
    if(strlen(bin_col) == 0) {
        if(fits_read_key(infits,TSTRING,"CTYPE1",bin_col,comment,&status)){
            status = 0;
            strcpy(bin_col, "X"); 
        }
    }
    ttype[0] = bin_col;
    

    tunit[0] = (char*)malloc(FLEN_VALUE);
    if(fits_read_key(infits,TSTRING,"CUNIT1",tunit[0],comment, &status)){
        status = 0;
	strcpy(tunit[0]," ");
    }

    if(strlen(value_col) == 0) strcpy(value_col,"Y"); 
    ttype[1] = value_col;
    tunit[1] = (char*)malloc(FLEN_VALUE);
    strcpy(tunit[1],"");

    /* histogram data  type */
    tform[1] = (char*)malloc(FLEN_VALUE);
    if(fits_read_key(infits,TINT,"BITPIX",&bitpix,comment, &status)){
	fits_report_error(stderr,status); 
	return;
    }
    switch(bitpix) {
	case BYTE_IMG: 
	     strcpy(tform[1],"1B");
	     break;
	case SHORT_IMG: 
	     strcpy(tform[1],"1I");
	     break;
	case LONG_IMG: 
	     strcpy(tform[1],"1J");
	     break;
	case FLOAT_IMG: 
	     strcpy(tform[1],"1E");
	     break;
	case DOUBLE_IMG: 
	     strcpy(tform[1],"1D");
	     break;
        default:
	     strcpy(tform[1],"1D");
             break;
   }

    if(strlen(error_col)) { 
	ttype[2] = error_col;
        tunit[2] = (char*)malloc(FLEN_VALUE);
        strcpy(tunit[2],"");
	ncol = 3;
    }
    else {
	ncol = 2;
    }

    if (iwcscol)
    {
       ttype[ncol] = error_col;
       tunit[ncol] = (char*)malloc(FLEN_VALUE);
       strcpy(tunit[ncol],"");
       ttype[ncol] = wcs_col;
       ncol++;
    }

    fits_create_tbl(outfits, BINARY_TBL, nbins, ncol, ttype, tform,
		    tunit, extnam, &status);
    sprintf(comment, "The histogram extension: %s", extnam);
    c_fcecho(comment);  

    ffpdat(outfits, &status);  /* write file creation DATE keyword */
    /* add some comments */
    strcpy(comment,"Task: fhisto on ");
    strcat(comment, infile);
    fits_write_history(outfits, comment,&status);
    c_timestamp(outfits);
    status = 0;



    /* reconstruct the histogram */
    if(fits_read_key(infits,TDOUBLE,"CRPIX1",&refpix,NULL, &status)){
	fits_report_error(stderr,status); 
	return;
    }
    if(fits_read_key(infits,TDOUBLE,"CRVAL1",&refval,NULL, &status)){
	fits_report_error(stderr,status); 
	return;
    }
    if(fits_read_key(infits,TDOUBLE,"CDELT1",&binsz,NULL, &status)){
	fits_report_error(stderr,status); 
	return;
    } 

    /* printf("refpix %lf refval %lf binsz %lf \n",refpix,refval,binsz); */
    lowval = refval - ((refpix - 0.5) * binsz);
    for (i = 0 ; i < nbins; i++)
    {  
       histo_bin_wcs[i] = lowval + (i * binsz) + (0.5 * binsz);
       histo_bin_raw[i] = ((histo_bin_wcs[i] - crval) / cdelt) + crpix;
    }

    fits_write_col(outfits, TDOUBLE, 1, 1, 1, nbins, histo_bin_raw, &status);
    fits_write_col(outfits, TDOUBLE, 2, 1, 1, nbins, histo_value, &status);
    
    if (strlen (error_col)) 
    {
       for (i=0 ; i< nbins; i++ )
       { 
	  histo_error[i] = 0.0;
	  if(histo_value[i] >= 0) histo_error[i] = sqrt(histo_value[i]);
       }
       fits_write_col (outfits, TDOUBLE, 3, 1, 1, nbins, histo_error, &status);
    }

    if (iwcscol)
    {
       fits_write_col (outfits, TDOUBLE, ncol, 1, 1, nbins, histo_bin_wcs,
                       &status);
    }

    /* close the input fitsfile */ 
    fits_close_file(infits, &status);
   
    /* close the output fitsfile  */ 
    fits_close_file(outfits, &status);

    for (i=1; i < ncol; i++) free(tunit[i]);
    free(tform[1]);
    free(histo_value);
    free(histo_bin_raw);
    free(histo_bin_wcs);
    free(histo_error);

}

/******************************************************************************
* Function
*      init_his_1d
*
*
* DESCRIPTION:
*      Get the parameters from the par file and construct the input
* filename + filter
*
*
*******************************************************************************/
int init_his_1d(char  *infil, 		/* input filename+filter */
		char  *infil_nofilt, 	/* input filename-filter */
		char  *outfile, 	/* output filename */
		char  *extnam,		/* output extension  name */
		char  *bin_col,		/* bin column name */
		char  *value_col,	/* value column name */
		char  *error_col,	/* error column name */
		char  *wcs_col,		/* WCS column name */
		int   *iwcscol,		/* include WCS column flag */
		char  *inputcol, 	/* name of input column */
		int   *status		/* error status */
		)
{
    char temp[FLEN_FILENAME];
    char tempv[FLEN_VALUE];


    /* variables for file names */
    char urltype[FLEN_FILENAME] ="";
    char file[FLEN_FILENAME] ="";
    char ofile[FLEN_FILENAME] ="";
    char extspec[FLEN_FILENAME] ="";
    char rowfilter[FLEN_FILENAME] ="";
    char binspec[FLEN_FILENAME] ="";
    char colspec[FLEN_FILENAME] ="";
    char colname[4][FLEN_VALUE] ;
    char minname[4][FLEN_VALUE] ;
    char maxname[4][FLEN_VALUE];
    char binname[4][FLEN_VALUE];
    char wtname[FLEN_VALUE] ="";
    int recip;
    double minin[4];
    double maxin[4]; 
    double binsizein[4];

    double indef_val = DOUBLENULLVALUE;

    /* varables for parameters */
    double binsz;
    double lowval;
    double highval;
    char rows[FLEN_FILENAME]="";
    char expos [FLEN_KEYWORD]="";
    char minkw [FLEN_KEYWORD]="";
    char maxkw [FLEN_KEYWORD]="";
    char image_str[30]="";
    char *col_str;
    char *low_str;
    char *high_str;
    char *bin_str;
    char weight_str[FLEN_VALUE]="";


    int dummy;
    int imagetype, haxis;
    double weight;
    int changed = 0;
    int BufLen_2;
    char *p;
    int clobber = 0;

    /*============ Basic parameters   =================*/

    /* get name of the input fits file */
    BufLen_2 = FLEN_FILENAME -1 ;
    Uclgst("infile",infil,status);
    if(*status) {
        strcpy(errmes,"could not get infile parameter");
        c_fcerr(errmes);
        return *status;
    }

    /* get name of the output fits file */
    Uclgst("outfile",outfile,status);
    if(*status) {
        strcpy(errmes,"could not get outfile parameter");
        c_fcerr(errmes);
        return *status;
    }

    BufLen_2 = FLEN_VALUE -1 ;
    /* get extension name of the output fits file */
    Uclgst("extname",tempv,status);
    if(*status) {
        strcpy(errmes,"could not get extname parameter");
        c_fcerr(errmes);
        return *status;
    }
    p = tempv;
    while(*p == ' ') p++;
    if(strlen(p) != 0) strcpy(extnam,tempv);

    /* get name of bin centroid column of the output file */
    Uclgst("outcolx",bin_col,status);
    if(*status)  {
        strcpy(errmes,"could not get outcolx parameter");
        c_fcerr(errmes);
        return *status;
    }

    /* get name of value column of the  output file */
    Uclgst("outcoly",value_col,status);
    if(*status)     {
        strcpy(errmes,"could not get outcoly parameter");
        c_fcerr(errmes);
        return *status;
    }

    /* get name of error column of the  output file */
    Uclgst("outcolz",error_col,status);
    if(*status)     {
        strcpy(errmes,"could not get outcolz parameter");
        c_fcerr(errmes);
        return *status;
    }

    /* get name of WCS column of the output file */
    Uclgst("outcolw",wcs_col,status);
    if(*status)     {
        strcpy(errmes,"could not get outcolw parameter");
        c_fcerr(errmes);
        return *status;
    }

    /* get the include WCS column parameter */
    Uclgsb("wcscol",iwcscol,status);
    if(*status)     {
        strcpy(errmes,"could not get wcscol parameter");
        c_fcerr(errmes);
        return *status;
    }

    /* get the clobber parameter */
    Uclgsb("clobber",&clobber,status);
    *status = 0; 
    if(clobber) { 
	if(*outfile != '!') { 
	   strcpy(temp,"!");
	   strcat(temp,outfile);
	   strcpy(outfile, temp);
        }
	sprintf(temp,"The file %s is overwritten",&outfile[1]);
	c_fcecho(temp); 
    }

    /* parse the infile name */ 
    strcpy(temp,infil);
    if(fits_parse_input_url(temp,urltype,file, ofile, extspec, rowfilter,
          binspec, colspec, status)){
        strcpy(errmes,"could not parse the input file");
        c_fcerr(errmes);
        return *status;
    } 
    /* printf ("rowfilter ->%s<-\n", rowfilter); */
    /* default extension number is 1*/
    if(strlen(extspec)==0) strcpy(extspec,"1");

    /*======= Optional parameters ============================*/

    /* These keywords are for the old fortran style keywords. 
       They are not needed if the row filter and bin spec are 
       present in the input file name.                        */ 
     

    /* Only read the row parameters when rowfilter  is not used */
    if(strlen(rowfilter) == 0) {
        Uclgst("rows",rows,status);
        if(*status){
            strcpy(errmes,"could not get rows parameter");
            c_fcerr(errmes);
            return *status;
        }
	/* convert the row range to the row filter  */
	if(strlen(rows)!=0) rg2rf(rows, rowfilter);
    }


    if(strlen(binspec) != 0) {
        if(fits_parse_binspec(binspec, &imagetype, &haxis,
                        colname, minin, maxin, binsizein,
			minname, maxname, binname,&weight,
			wtname, &recip, status)) {
            sprintf(errmes,"Invalid binspec. %s ", binspec);
            c_fcerr(errmes);
            exit (1); 
        } 
        strcpy (inputcol, colname[0]);
        if(haxis != 1) {
             sprintf(errmes,"binspec. %s is not 1-d histogram spec.", binspec);
             c_fcerr(errmes);
             exit (1); 
        }
    }
    else {
        BufLen_2 = FLEN_VALUE - 1 ;
        /* get the column name used to generate the histogram */
        Uclgst("column",inputcol,status);
        if(*status)     {
            strcpy(errmes,"could not get column parameter");
            c_fcerr(errmes);
            return *status;
        }

        /* get the binsize */
        Uclgsd("binsz",&binsz,status);
        if(*status == 3 ){
	    binsz = indef_val;
	    *status = 0; 
        }
        else if (*status != 0) {
            strcpy(errmes,"could not get binsz parameter");
            c_fcerr(errmes);
            return *status;
        }



        BufLen_2 = FLEN_KEYWORD -1 ;
        /* get the exposure  keyword */
        Uclgst("exposure",expos,status);
        if(*status)     {
            strcpy(errmes,"could not get exposure parameter");
            c_fcerr(errmes);
            return *status;
        }

        /* get the minimum  keyword */
        minkw[0]='\0';
        Uclgst("minkw",minkw,status);
        if(*status)     {
            strcpy(errmes,"could not get minkw parameter");
            c_fcerr(errmes);
            return *status;
        }

        /* get the maximum  keyword */
        maxkw[0]='\0';
        Uclgst("maxkw",maxkw,status);
        if(*status)     {
            strcpy(errmes,"could not get maxkw parameter");
            c_fcerr(errmes);
            return *status;
        }

        /* get the histogram lower limit */ 
        Uclgsd("lowval",&lowval,status);
        if(*status == 3 ){
	    lowval = indef_val;
	    *status = 0; 
        }
        else if (*status != 0) {
            strcpy(errmes,"could not get lowval parameter");
            c_fcerr(errmes);
            return *status;
        } 

        /* get the histogram upper limit */ 
        Uclgsd("highval",&highval,status);
        if(*status == 3 ){
	highval = indef_val;
	*status = 0; 
        }
        else if (*status != 0) {
            strcpy(errmes,"could not get highval parameter");
            c_fcerr(errmes);
            return *status;
        } 

    	/* construct the bin spec  from the provided parameters */
	low_str = (char *)malloc(FLEN_VALUE);
	low_str[0]='\0';
        if(lowval != indef_val) { 
	    sprintf(low_str,"%lg",lowval);
        }
	else {
	    p = minkw;
	    while(*p == ' ') p++;
	    if( *p == '-' || strlen(p) == 0) strcat(low_str,p);
        }

	high_str = (char *)malloc(FLEN_VALUE);
	high_str[0]='\0';
        if(highval != indef_val) { 
	    sprintf(high_str,"%lg",highval);
        }
	else {
	    p = maxkw;
	    while(*p == ' ') p++;
	    if( *p == '-' || strlen(p) == 0) strcat(high_str,p);
        }

	bin_str = (char *)malloc(FLEN_VALUE);
	bin_str[0]='\0';
        if(binsz != indef_val)  sprintf(bin_str,"%lg",binsz);

	weight_str[0] = '\0';
	p = expos;			 /* dealing with exposure */
        while(*p == ' ') p++;
	if(strlen(p) != 0) {
	    strcat(weight_str, "/");
	    strcat(weight_str, expos);
        }

	image_str[0] = '\0';
	col_str = inputcol;
	set_binspec(image_str, 1, &col_str, &low_str, &high_str,
	&bin_str, weight_str, binspec, status);
	strcat(temp,binspec);
        free(low_str);
        free(high_str);
        free(bin_str);
    }	  

    /* constructing the input file name */
    if(strlen(urltype)) {
	strcpy(temp, urltype);
        strcat(temp, file);
    } 
    else { 
	strcpy(temp,file);
    }

    if(strlen(extspec)) {
	strcat(temp,"[");
	strcat(temp,extspec);
	strcat(temp,"]");
    }

    strcpy (infil_nofilt, temp);

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
    strcpy (infil, temp); 

    return 0;
}
/* The following code is needed to allow IRAF to call cdummyftool.
This extra subroutine layer is needed because of differences in
linkers between vms and unix. */
#ifdef vms
#define F77CALL iraf_fhisto 
#endif
#ifdef unix
#define F77CALL iraf_fhisto_
#endif

void F77CALL()
{
    void fhisto();
    fhisto();
} 


