/***********************************************************************
File : flst2im.c
 
Description:
    create an output image file from an input file (ASCII or FITS)
    containing a list of the coordinates and value of the pixels.
    e.g. input file looks like this

      X    Y    VALUE
     200  100    2
     210  200   10
      .    .     .
    ... so on
    
Author:
       Banashree M Seifert (July, 1997)V1.0.0
       Banashree M Seifert (Nov,  1997)V1.1.0
         . made nxbin & nybin (no. of bins in x-,y- directions respectively)
           to be INDEF in which case xbinsize=ybinsize=1
         . the writing output image was not right
         . accepts the case when in the list file, 3rd column is not
           present and in that case, 3rd column =1
       Peter D Wilson      (June, 1998)V1.2.0
         . Updated task to support new extended-syntax filenames... required
           moving stuff from "getpar" to "do" function so that files would
           only get opened *once*... allowing piping from stdin.
         . Modified handling of binsizes... if nbin not specified, assume
           inclusive, integral bounds and shift min value to reflect this.
       Ning Gan      	(July, 1998)V1.3.0
	 . Deleted the cfitsio.h.
       Peter D Wilson   (June, 2000) V1.4.0
         . Allow blank lines and lines starting with '#'
         . Add datatype parameter which sets the output image bitpix
         . Absent datatype, detect real weights and set output to float
         . Change weight/output arrays from float to double for greater
           precision
         . Make VALUE column optional for FITS input file
 
Variables used:
    infile         char   input file name
    outfile        char   output file name
    xcol           char   name of the col containing x-data
    ycol           char   name of the col containing y-data
    value          char   name of the col containing value-data
    xrangepresent  int    true, if range has been mentioned
    yrangepresent  int    true, if range has been mentioned
    rowpresent     int    true, if rows has been mentioned
    xrange1        float  minimum x-value
    xrange2        float  maximum x-value
    yrange1        float  minimum y-value
    yrange2        float  maximum y-valu
    nxbin          int    no. of bins in x-direction
    nybin          int    no. of bins in y-direction
    extnum         int    extension no.
    status         int    status at any instant
    clobber        int    whether to overwrite existing file
    FirstRow[]     int    ranges of first rows
    LastRow[]      int    ranges of last rows
    numranges      int    number of ranges

***********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>

#include "ftools.h"        /* for fortran interface */
#include "fitsio.h"        /* cfitsio defined constants */
#include "xpi.h"           /* parameter file functions, e.g. Uclgst */
#include "ftoolstruct.h"   /* C-Fortran common blocks */
#include "cftools.h"       /* standard C library constants */
#include "flst2im.h"      /* task specific definitions */

/* Define interface to FORTRAN FTOOLS library routine... */
/*     It translates range of datatype strings to bitpix */
#define DataType(dtype,bitpix,status) \
  CCALLSFSUB3(DATATY,dataty,STRING,PCINT,PCINT,dtype,bitpix,status)

#ifndef MAX
#define MAX(A,B) (A>B? A:B)
#endif
#ifndef MIN
#define MIN(A,B) (A<B? A:B)
#endif

void flst2im()
{
    char infile[FLEN_BUFFER];
    char outfile[FLEN_BUFFER];
    char subinfo[100];
    char xcol[FLEN_BUFFER], ycol[FLEN_BUFFER], value[FLEN_BUFFER];
    int xrangepresent=0, yrangepresent=0, rowpresent=0;
    int xbinpresent=0, ybinpresent=0;
    float xrange1=0., xrange2=0., yrange1=0., yrange2=0.;
    int nxbin=0, nybin=0, status=0, clobber=0;
    int FirstRow[10], LastRow[10];
    int numranges=0;
    int bitpix;

    c_ptaskn("flst2im1.4.0"); /* method to store task name for c_fcerr */

/************ get parameters from flst2im.par ************/
    flst2imgp(infile, outfile, xcol, ycol, value, 
	      &xrangepresent, &yrangepresent, &numranges, &xrange1, 
	      &xrange2, &yrange1, &yrange2, &rowpresent, FirstRow, 
	      LastRow, &xbinpresent, &ybinpresent, &nxbin, &nybin,  
              &bitpix, &clobber, &status);

    if(status) StatusChk(status, "... Unsuccesfully exiting FLST2IM \n");

    flst2imdo(infile, outfile, xcol, ycol, value, 
	      &xrangepresent, &yrangepresent, &numranges, 
	      &xrange1, &xrange2, &yrange1, &yrange2, &rowpresent, 
	      FirstRow, LastRow, &xbinpresent, &ybinpresent, 
              &nxbin, &nybin, bitpix, clobber, &status);

    if(status) StatusChk(status, "... Unsuccesfully exiting FLST2IM\n");

    DispMsg(0, 0, " ... Successfully completed FLST2IM \n");

    return;
 
}
 
/*******************************************************************
function:
      flst2imgp
 
description:
      gets the parameters for the task.
 
author:
      Banashree M Seifert (July, 1997)1.0.0:
      Banashree M Seifert (Nov , 1997)1.1.0
         . provision to have nxbin & nybin = INDEF

 
modification history:
      Peter D Wilson      (June, 1998)1.2.0
         . Dropped code which opened infile to locate number of rows then
           closed it. (moved to "do" function)
      Peter D Wilson      (June, 2000)1.4.0
         . Add datatype->bitpix parameter
 
variable definitions:
    *fp1        FILE    pointer for ascii file
    *infp1      FILE    pointer for FITS file
    *p          char    pointer needed for STRTOK function
    rows[]      char    rows to be read.  It breaks into 
                        FirstRow[] & LastRow[]
    xrange[]    char    range of xvalues. It breaks into two
                        xrange1 & xrange2
    yrange[]    char    range of yvalues. It breaks into two
                        yrange1 & yrange2
    str[]       char    string to be read as one line
    tmpfil[]    char    name of the temporary input file
    naxis2      long    no. of input data lines
 
usage:
    flst2imgp(char *infile, char *outfile,
              char *xcol, char *ycol, char *value, int *xrangepresent, 
              int *yrangepresent, int *numranges, float *xrange1, 
              float *xrange2, float *yrange1, float *yrange2, 
              int *rowpresent, int *FirstRow, int *LastRow,
              int *xbinpresent, int *ybinpresent,
              int *nxbin, int *nybin, int *bitpix, int *clobber, int *status)

*********************************************************************/
void flst2imgp(char *infile, char *outfile,
               char *xcol, char *ycol, char *value, int *xrangepresent, 
               int *yrangepresent, int *numranges, float *xrange1, 
               float *xrange2, float *yrange1, float *yrange2, 
               int *rowpresent, int *FirstRow, int *LastRow,
               int *xbinpresent, int *ybinpresent,
               int *nxbin, int *nybin, int *bitpix, int *clobber, int *status)
{
    int BufLen_2= FLEN_BUFFER -1;      /* required by cfortran.h*/

    char subinfo[100];
    char rows[FLEN_BUFFER], xrange[FLEN_BUFFER], yrange[FLEN_BUFFER];
    char xbinstr[FLEN_BUFFER], ybinstr[FLEN_BUFFER];
    char bitpixStr[FLEN_BUFFER];
    char *p;

    *status=0;

    /*------- clobber ------*/ 

    Uclgsb("clobber", clobber, status);
    StatusChk(*status,"\n....Error reading clobber from parameter file\n");
 
    /*------ infile ------*/
    Uclgst("infile", infile, status);

    if(infile[0] =='\0'){
       DispMsg(0,0, "\n....Error: filename is required \n");
       *status=1;
       return;
    }

    StatusChk(*status,"\n....Error reading infile from parameter file \n");

    /*------ outfile ------*/
    Uclgst("outfile", outfile, status);

    if(outfile[0] == '\0'){
        *status = 1;
        DispMsg(0,0, "\n....Error: output filename is required\n");
    }

    StatusChk(*status,"\n....Error reading outfile from parameter file\n");

    if( *clobber && outfile[0] != '!') {
       char tmp[FLEN_BUFFER];
       strcpy(tmp,outfile);
       outfile[0]='!';
       strcpy( outfile+1, tmp);
    }
 
    /*------ xcol ------*/
    Uclgst("xcol", xcol, status);
       StatusChk(*status, "\n....Error reading xcol from parameter file\n");

    /*------ ycol ------*/
    Uclgst("ycol", ycol, status);
       StatusChk(*status, "\n....Error reading ycol from parameter file\n");

    /*------ value ------*/
    Uclgst("value", value, status);
       StatusChk(*status, "\n....Error reading value from parameter file\n");

    /*------ xrange ------*/

    Uclgst("xrange", xrange, status);
       StatusChk(*status, "\n....Error reading xrange from parameter file\n");

    /*------ yrange ------*/
    Uclgst("yrange", yrange, status);
       StatusChk(*status, "\n....Error reading yrange from parameter file\n");

    /*------ rows ------*/
    Uclgst("rows", rows, status);
       StatusChk(*status, "\n....Error reading rows from parameter file\n");

    /*------ nxbin ------*/
    Uclgst("nxbin", xbinstr, status);
        StatusChk(*status, "\n....Error reading nxbin from parameter file\n");

    /*------ nybin ------*/
    Uclgst("nybin", ybinstr, status);
        StatusChk(*status, "\n....Error reading nybin from parameter file\n");

    /*------ datatype ------*/
    Uclgst("datatype", bitpixStr, status);
        StatusChk(*status, "\n....Error reading datatype from parameter file\n");

    /* convert the input parameters to meaningful to be used by the task */

    if((xrange[0] == 'I') || (xrange[0] == 'i'))
        *xrangepresent = 0;
     
    else{
        *xrangepresent = 1;
        p = strtok(xrange,",\t ");
        *xrange1 = atof(p);
        p = strtok(NULL,"\t ");
        *xrange2 = atof(p);
    }

    if((yrange[0] == 'I') || (yrange[0] == 'i'))
        *yrangepresent = 0;
     
    else{
        *yrangepresent = 1;
        p = strtok(yrange,",\t ");
        *yrange1 = atof(p);
        p = strtok(NULL,"\t ");
        *yrange2 = atof(p);
    }

    if((xbinstr[0] == 'I') || (xbinstr[0] == 'i'))
        *xbinpresent = 0;
    else{
        *xbinpresent = 1;
        *nxbin = atoi(xbinstr);
    }

    if((ybinstr[0] == 'I') || (ybinstr[0] == 'i'))
        *ybinpresent = 0;
    else{
        *ybinpresent = 1;
        *nybin = atoi(ybinstr);
    }

    if(rows[0] == '-') {
       *rowpresent = 0;
       *numranges = 1;
       FirstRow[0] = 1;
       LastRow[0]  = INT_MAX;
    } else {
       *rowpresent = 1;
       Fcgrgs(rows, INT_MAX, numranges, FirstRow, LastRow);
    }

    *bitpix = 0;  /*  Flag as unspecified  */
    ffupch(bitpixStr);
    DataType(bitpixStr, bitpix, status);
       StatusChk(*status, "\n....Unable to convert datatype to bitpix\n");

    return;
}


/*******************************************************************
function:
      flst2imdo
 
description:
      do required calculations for the tool
 
author:
      Banashree M Seifert (July, 1997)1.0.0:
      Banashree M Seifert (Nov,  1997)1.1.0:
           . output image array was not proper. Now it is fine
           . calculation for default nxbin & nybin= INDEF introduced

 
modification history:
      Peter D Wilson      (June, 1998)1.2.0
         . Integrated code which read data and calculated number of rows
           in file, to allow single-pass processing of infile.
         . Modified handling of binsizes... if nbin not specified, assume
           inclusive, integral bounds and shift min value to reflect this.
      Peter D Wilson      (June, 1998)1.4.0
         . Allow blank lines and add greater control over output datatype
         . Make VALUE column in FITS file optional
 
variable definitions:

   *infp1      File   input file pointer
   *infp2      File   output file pointer
   *p          char   pointer for function STRTOK to parse a string
   str         char   one line read at a time
   naxes       long   array of two defining the no. of axes
   pcount      long   value of pcount keyword (==0) 
   gcount      long   value of gcount keyword (==1) 
   hdutyp      int    image=0, asciitable=1, binarytable=2
   xcolnum     int    col no. containing X
   ycolnum     int    col no. containing Y
   vcolnum     int    col no. containing VALUE
   num         int    counter for no. of ranges of rows to read
   i           int    counter general
   nrow        int    counter for no. of rows done at a time
   n           int    counter for when rows are read from input
   nrows[]     int    (array) of no. of rows to be read at a time
   TotalRows   int    total no. of rows to be read from input
   Matrix      int    total no. of Tmparray required(nxbin*nybin)
   start       int    staring array no. for each ffgcve
   ChartoRead  int    no. of characters to be read into at a time
   Row         int    temporary counter for counting no. of rows
   nskip[]     int    (array) no. of rows to skip at a time
   bitpix      int    bits per pixel
   naxis       int    no. of axes
   simple      int    true if file does conform to FITS standard
   extend      int    true if FITS dataset may contain extensions
                      but always recommended to make = 1
   anynul      int    true if any of the returned data values are undefined
   xbinNo      int    current bin no. in x-direction
   ybinNo      int    current bin no. in y-direction
   InpAscii    int    true, if input file is ascii
   *Xarray     float  pointer to be filled up by input X values
   *Yarray     float  pointer to be filled up by input Y values
   *Varray     double pointer to be filled up by input VALUES values
   *Tmparray   double new output array
    nulval     float  value to represent undefined pixel
    xbinsz     float  size of each bin ((xrange2-xrange1)/nxbin)
    ybinsz     float  size of each bin ((yrange2-yrange1)/nybin)
    crpix1     float  X-axis ref pixel
    crpix2     float  Y-axis ref pixel
    crval1     float  coord. at X-axis ref pixel
    crval2     float  coord. at Y-axis ref pixel
    crdelt1    float  X-axis ref pixel increment
    crdelt2    float  X-axis ref pixel increment


usage:
    flst2imdo(char *infile, char *outfile,
              char *xcol, char *ycol, char *value, int *xrangepresent, 
              int *yrangepresent, int *numranges, float *xrange1, 
              float *xrange2, float *yrange1, float *yrange2, 
              int *rowpresent, int *FirstRow, int *LastRow,
              int *xbinpresent, int *ybinpresent, 
              int *nxbin, int *nybin, int bitpix, int clobber, int *status)

*********************************************************************/
void flst2imdo(char *infile, char *outfile,
               char *xcol, char *ycol, char *value, int *xrangepresent, 
               int *yrangepresent, int *numranges, float *xrange1, 
               float *xrange2, float *yrange1, float *yrange2, 
               int *rowpresent, int *FirstRow, int *LastRow,
               int *xbinpresent, int *ybinpresent,
               int *nxbin, int *nybin, int bitpix, int clobber, int *status)
{
    FILE *fp1;
    fitsfile *infp1=NULL, *infp2=NULL;
  
    char *p;

    char subinfo[100];
    char comment[200], str[80];

    long naxes[2], pcount, gcount;
    long naxis2=0;

    int hdutyp=0, xcolnum=0, ycolnum=0, vcolnum=0;
    int num=0, i=0, n=0;
    int nrows[10], anynul=0, TotalRows=0;
    int Matrix;
    int start, ChartoRead=80, Row;
    int naxis, simple, extend;
    int xbinNo=0, ybinNo=0;
    int InpAscii=0, extnum;

    int typecode;
    long repeat, width;

    float *Xarray=0, *Yarray=0;
    double *Varray=0, *Tmparray;
    float nulval=0.;
    float xbinsz=0., ybinsz=0.;
    float crpix1, crpix2, crval1, crval2, cdelt1, cdelt2;
    long nnelem;
    int isReal=0;

    if( ffopen(&infp1, infile, READONLY, status) ) {
     
                      /****** case when input file is ASCII *******/

       if((fp1 = fopen(infile, "r")) != NULL){
	  InpAscii = 1;
	  *status = 0;
	  Row = 0;
	  TotalRows = 0;

	  n = 0;
	  num = 0;
	  while( fgets(str,ChartoRead,fp1) != NULL ) {
	     Row++;
	     while( Row > LastRow[num] && num < *numranges ) num++;
	     if( num==*numranges ) break;
	     if( Row < FirstRow[num] ) continue;

             /*  Check for lines to skip... # and blank  (PDW) */

             if( str[0] == '#' ) continue;
             for( i=0; str[i] && isspace(str[i]); i++);
             if( !str[i] ) continue;

	     /* Found valid row... check if enough memory allocated */

	     if( n >= TotalRows ) {
		TotalRows += 10000;

		if( n ) {
		   Xarray = realloc( Xarray, TotalRows*sizeof(float)  );
		   Yarray = realloc( Yarray, TotalRows*sizeof(float)  );
		   Varray = realloc( Varray, TotalRows*sizeof(double) );
		} else {
		   Xarray = malloc (         TotalRows*sizeof(float)  );
		   Yarray = malloc (         TotalRows*sizeof(float)  );
		   Varray = malloc (         TotalRows*sizeof(double) );
		}

		if( Xarray==NULL || Yarray==NULL || Varray==NULL ) {
		   if( Xarray ) free(Xarray);
		   if( Yarray ) free(Yarray);
		   if( Varray ) free(Varray);
		   *status=MEMORY_ALLOCATION;
		   StatusChk(*status, "....Error: malloc failed for Arrays\n");
		}
	     }

	     if( (p = strtok(str,",\t ")) )
                Xarray[n] = atof(p);
	     else{
		*status=1; 
		sprintf(subinfo,
			"...error parsing input data: \"%s\"\n", str);
		StatusChk(*status, subinfo);
		return;
	     }
	     
	     if( (p = strtok(NULL,",\t ")) )
                Yarray[n] = atof(p);
	     else{
		*status=1; 
		sprintf(subinfo,
			"...error parsing input data: \"%s\"\n", str);
		StatusChk(*status, subinfo);
		return;
	     }
	     
	     if( (p = strtok(NULL,",\t ")) ) {
                Varray[n] = atof(p);
                if( Varray[n] != (long) Varray[n] ) isReal=1;
             } else
		Varray[n] = 1.;

	     n++; /* Advance array index */
          }
	  fclose(fp1);
	  TotalRows = n;

       } else {

	  Fcerrm(*status);
	  StatusChk(*status, "....Error opening input file\n");

       }

    } else {
                       /****** case when input is FITS file ****/

       InpAscii = 0;
       ffghdn(infp1, &extnum);
       if( extnum==1 ) {
	  extnum=2;
	  if(ffmahd(infp1, extnum, &hdutyp, status)) Printerror(*status);
       }
       if(ffgky(infp1, TINT, "NAXIS2", &naxis2, comment, status))
                                 Printerror(*status); 

/*------------------------------------------------------------------
      find no. of rows to be read at a time and
      total no. of rows (TotalRows) to be read from input file
-------------------------------------------------------------------*/
       TotalRows = 0;
       for( num=0; num < *numranges; num++ ){
	  if( LastRow[num]==INT_MAX ) LastRow[num] = naxis2;
	  nrows[num] = LastRow[num] - FirstRow[num] + 1;
	  TotalRows += nrows[num]; 
       }

    /*************************************************************
      allocate DMA to the arrays to be read
      Xarray = array of the input X
      Yarray = array of the input Y
      Varray = array of the input VALUE
    *************************************************************/
    
       if(!(Xarray = malloc(TotalRows*sizeof(float)))){
          *status=1;
          DispMsg(0,0, "....Error: malloc failed for Xarray\n");
       }

       if(!(Yarray = malloc(TotalRows*sizeof(float)))){
          *status=1;
          DispMsg(0,0, "....Error: malloc failed for Yarray\n");
       }

       if(!(Varray = malloc(TotalRows*sizeof(double)))){
          *status=1;
          DispMsg(0,0, "....Error: malloc failed for Varray\n");
       }

       if( ffgcno(infp1, 0, xcol,  &xcolnum, status) ) Printerror(*status);
       if( ffgcno(infp1, 0, ycol,  &ycolnum, status) ) Printerror(*status);
       if( value[0]=='\0' || !strcmp("-",value) ) {
          vcolnum = 0;
          for( i=0; i<TotalRows; i++ ) Varray[i] = 1.0;
       } else {
          if( ffgcno(infp1, 0, value, &vcolnum, status) ) Printerror(*status);
          ffgtcl( infp1, vcolnum, &typecode, &repeat, &width, status );
          if( typecode > TLONG ) isReal = 1;
       }

       start=0;
       for(num=0; num < *numranges; num++){
             
          if(ffgcve(infp1, xcolnum, (long)FirstRow[num], 1L,
                    (long)nrows[num], nulval, Xarray+start, &anynul, status)) 
             Printerror(*status);
 	
          if(ffgcve(infp1, ycolnum, (long)FirstRow[num], 1L,
                    (long)nrows[num], nulval, Yarray+start, &anynul, status)) 
             Printerror(*status);
 
          if( vcolnum ) {
             if(ffgcvd(infp1, vcolnum, (long)FirstRow[num], 1L,
                       (long)nrows[num], nulval, Varray+start,
                       &anynul, status)) 
                Printerror(*status);
          }

	  start+=nrows[num];
      }

      if(ffclos(infp1, status)) Printerror(*status);
    }


    /*-----------------------------------------------------------------
      end of block {if(ffopen)} where data is read from input file.
            The file is closed after reading 
    ------------------------------------------------------------------*/

    /*-----------------------------------------------------------------
      find minimum and maximum ranges in x & y directions from the data 
           itself if they are not given 
    ------------------------------------------------------------------*/ 
    if(!*xrangepresent){
        *xrange1 =  1.e+30;
        *xrange2 = -1.e+30;
           for(i=0; i< TotalRows; i++){
               *xrange1 = MIN(*xrange1,Xarray[i]);
               *xrange2 = MAX(*xrange2,Xarray[i]);
	   }
    }

    if(!*yrangepresent){
        *yrange1 =  1.e+30;
        *yrange2 = -1.e+30;
           for(i=0; i< TotalRows; i++){
               *yrange1 = MIN(*yrange1,Yarray[i]);
               *yrange2 = MAX(*yrange2,Yarray[i]);
           }
    }

    if(!*xbinpresent) {
        xbinsz = 1 ;
        *nxbin = (*xrange2 - *xrange1) + 1;
	*xrange1 -= 0.5*(*xrange1);
    } else {
        xbinsz = (*xrange2 - *xrange1)/(*nxbin) ;
    }

    if(!*ybinpresent) {
        ybinsz = 1 ;
        *nybin = (*yrange2 - *yrange1) + 1;
	*yrange1 -= 0.5*(*yrange1);
    } else {
        ybinsz = (*yrange2 - *yrange1)/(*nybin) ;
    }


    Matrix = (*nxbin)*(*nybin);

     if(!(Tmparray = malloc(Matrix*sizeof(double)))){
          *status=1;
          DispMsg(0,0, "....Error: malloc failed for Tmparray\n");
     }
  
     /* initialise matrix elements */
     for(i=0; i<Matrix; i++) Tmparray[i] = 0.;

     for(i=0; i<TotalRows; i++) {

         xbinNo = ((Xarray[i] - *xrange1)/xbinsz ) + 1;
         ybinNo = ((Yarray[i] - *yrange1)/ybinsz ) + 1;

         if((xbinNo >= 1) && (xbinNo <= (*nxbin))) {
             if((ybinNo >= 1) && (ybinNo <= (*nybin)))
	       Tmparray[(xbinNo-1)+((ybinNo-1) * (*nxbin))] += Varray[i];
         }
     }


    /* initialise output file */

     *status=0;
     simple   = 1;
     bitpix   = (bitpix==0 ? (isReal?-32:32) : bitpix);
     naxis    = 2;
     naxes[0] = (*nxbin);
     naxes[1] = (*nybin);
     pcount   = 0;
     gcount   = 1;
     extend   = 1;

     if( ffinit(&infp2, outfile, status) ) Printerror(*status);
     if( ffphpr(infp2, simple, bitpix,
		naxis, naxes, pcount,
		gcount, extend, status) )  Printerror(*status);
     if( ffpdat(infp2, status) )           Printerror(*status);

     /* some other required keywords */

     strcpy(comment," Input file name ");
     if(ffpky(infp2, TSTRING, "FILIN001", infile, comment, status))
          Printerror(*status);

     strcpy(comment," Origin of Fits file ");
     if(ffpky(infp2, TSTRING, "ORIGIN", "NASA/GSFC", comment, status))
          Printerror(*status);

     strcpy(comment,"  Format conforms to OGIP/GSFC conventions ");
     if(ffpky(infp2, TSTRING, "HDUCLASS", "OGIP", comment, status))
          Printerror(*status);

     strcpy(comment," Extension contains an image ");
     if(ffpky(infp2, TSTRING, "HDUCLAS1", "IMAGE", comment, status))
          Printerror(*status);

     strcpy(comment," Task that created this image from list ");
     if(ffpky(infp2, TSTRING, "CREATOR", "FLST2IM 1.0.0", comment, status))
          Printerror(*status);

     /* put some CR* keywords */

     crpix1 = crpix2 = 0.5;
     strcpy(comment," X axis reference pixel ");
     if(ffpky(infp2, TFLOAT, "CRPIX1", &crpix1, comment, status))
          Printerror(*status);

     strcpy(comment," Y axis reference pixel ");
     if(ffpky(infp2, TFLOAT, "CRPIX2", &crpix2, comment, status))
          Printerror(*status);

     cdelt1 = xbinsz;
     strcpy(comment," X increment ");
     if(ffpky(infp2, TFLOAT, "CDELT1", &cdelt1, comment, status))
          Printerror(*status);

     cdelt2 = ybinsz;
     strcpy(comment," Y increment ");
     if(ffpky(infp2, TFLOAT, "CDELT2", &cdelt2, comment, status))
          Printerror(*status);

     crval1 = *xrange1 + (xbinsz/2.); 
     strcpy(comment," coord. at X axis ref. pixel ");
     if(ffpky(infp2, TFLOAT, "CRVAL1", &crval1, comment, status))
          Printerror(*status);

     crval2 = *yrange1 + (ybinsz/2.);
     strcpy(comment," coord. at Y axis ref. pixel ");
     if(ffpky(infp2, TFLOAT, "CRVAL2", &crval2, comment, status))
          Printerror(*status);

     /* history keyword */
  
     if(InpAscii)
        strcpy(comment, "This image has been created from an ascii file containing lists of X, Y & Value columns. If one of Value column is missing in input, it is assumed as 1 and all other undefined pixel value = 0 ");
     else
        strcpy(comment,"This image has been created from a FITS file containing lists of X,Y & Value columns. Any undefined pixel value = 0 ");

     if(ffphis(infp2, comment, status))  Printerror(*status);
     
     /* now write the data */

     nnelem = (*nybin)*(*nxbin);
     if(ffppr(infp2, TDOUBLE, 1L, nnelem,Tmparray, status)) Printerror(*status);

     free(Tmparray);
     free(Xarray);
     free(Yarray);
     free(Varray);
 
     if(ffclos(infp2, status)) Printerror(*status);

   return;
}


/* This code is needed by IRAF */
 
#ifdef vms
#define F77CALL flst2im
#endif
#ifdef unix
#define F77CALL flst2im_
#endif
 
void F77CALL()
{
 void flst2im();
 
 flst2im();
}
