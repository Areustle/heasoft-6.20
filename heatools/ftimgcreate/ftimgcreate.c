#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_error.h"

#define MAXMSG 256

/*
HISTORY
-------
  Version 1.0 written by William Pence and Ziqin Pan, NASA/GSFC, January 2003
  Version 1.1 Improved handling of 'naxes' parameter, Bryan Irby, May 2010:
              Reject illegal (non-numeric) naxes values, unless naxes="none",
              in which case create a null image (NAXIS=0).
*/

#define TOOLSUB ftimgcreate
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int ftimgcreate (void);
int ftimgcreate_getpar (int* bitpix, char* caxes, char* datfil, char * outfil,
      char * hdfil, int* nskip, int* nulflg, double * nulval);
int ftimgcreate_work (int bitpix, char* caxes, char* datfil, char * outfil,
      char * hdfil, int nskip, int nulflg, double nulval);
int gtaxes (char * caxes, int * naxis, long * naxes, int * status);
int addhd(fitsfile * fptr, char * hdfile, int * status);
int isblankstr(char* str);
void chop(char* str);
int crimg(fitsfile * fptr, char * datfil,int nulflg, double nulval,int nskip,int npixel,int *status);
int crnullimg(fitsfile * fptr, double nulval,int npixel,int *status);
int parseDouble (char * string, int* n, double * array, char * tokens,int * status);
int parseLong (char * string, int* n, long * array, char * tokens,int * status);

/*---------------------------------------------------------------------------*/
int ftimgcreate (void)
{
/*  Create  a  FITS  primary array image from ASCII template file. */

    int bitpix, nskip, nulflg, status;
    char datfile[PIL_LINESIZE], hdfile[PIL_LINESIZE], outfile[PIL_LINESIZE];
    char caxes[PIL_LINESIZE];
    double nulval;
    char msg[MAXMSG];

    static char taskname[80] = "ftimgcreate";
    static char version[8] = "1.10";

    /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);

    /*  get input parameters */
    if((status = ftimgcreate_getpar (&bitpix, caxes, datfile, outfile,
             hdfile, &nskip, &nulflg, &nulval) )) {
       sprintf(msg,"Error getting input parameters");
       HD_ERROR_THROW(msg,status);
       goto cleanup;
    }


    /* call work function to create the output file */
    if( (status = ftimgcreate_work (bitpix, caxes, datfile, outfile,
                 hdfile, nskip, nulflg, nulval))) {
        sprintf(msg,"Error creating the FITS image");
        HD_ERROR_THROW(msg,status);
        goto cleanup;
    }

cleanup:
    return(status);
}
/*---------------------------------------------------------------------------*/
int ftimgcreate_getpar (
    int* bitpix,      /* O - The value for the FITS BITPIX keyword */
    char* caxes,      /* O - List giving the size of each axis in  the
                             image. */
    char* datfile,     /* O - The name of the input ASCII formatted data
                             file containing the list of pixel values for
                             the image. */
    char* outfile,    /* O - The name of the output FITS file. */
    char* hdfile,     /* O - The name of the optional file of
                             header keywords. */
    int* nskip,       /* O - Number of rows at the beginning of the data
                             template file to skip inclusive of any blank
                             rows or rows beginning with '#'. */
    int* nulflg,      /* O - if NULLTEST = TRUE, than any pixels in the
                             input ASCII data file which have a value equal 
                             to the NULLVAL parameter will be designated
                             as undefined pixels by setting the pixel
                             value in the FITS file equal to the IEEE NaN.  
                             This only applies to FITS files with BITPIX
                             = -32 or -64. */
    double * nulval)  /* O - The floating point value that is used in
                             conjunction with the NULLTEST  parameter to
                             flag undefined pixels in the input image.
                             Only applies if BITPIX = -32 or -64. */


/*  read input parameters for the ftimgcreate task from the .par file */
{
    int status;
    char msg[MAXMSG];

    if ((status = PILGetInt("bitpix", bitpix))) {
      sprintf(msg, "Error reading the 'bitpix' parameter");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("naxes", caxes))) {
      sprintf(msg, "Error reading the 'naxes' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetFname("datafile", datfile))) {
      sprintf(msg, "Error reading the 'datafile' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetFname("outfile", outfile))) {
      sprintf(msg, "Error reading the 'outfile' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("headfile", hdfile))) {
      sprintf(msg, "Error reading the 'headfile' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetInt("nskip", nskip))) {
      sprintf(msg, "Error reading the 'nskip' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetBool("nulltest", nulflg))) {
      sprintf(msg, "Error reading the 'nulltest' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetReal("nullval", nulval))) {
      sprintf(msg, "Error reading the 'nullval' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftimgcreate_work (
    int bitpix, 
    char* caxes, 
    char* datfil, 
    char * outfil,
    char * hdfil, 
    int nskip, 
    int nulflg, 
    double nulval)

{
    int status =0, naxis=0;
    long naxes[PIL_LINESIZE];
    int npixel,i;

    char bnulval;
    short snulval;
    int inulval;

    fitsfile *outfptr = 0;


    if( strcmp(caxes,"none") != 0 ) {
            if ( gtaxes(caxes,&naxis,naxes,&status) ) goto cleanup;
    }

    headas_clobberfile(outfil);  /* delete existing file if clobber=YES */

    if (fits_create_file(&outfptr, outfil, &status)) goto cleanup;
    headas_chat(5,"Created the output file:\n %s\n", outfil);

    if (fits_write_grphdr(outfptr,1,bitpix,naxis,naxes,0,1,1,&status)) goto cleanup;

    if( nulflg )
    {
        if(bitpix == 8 ) {
           bnulval = nulval;
           if(fits_update_key(outfptr,TBYTE,"BLANK",&bnulval," ",&status)) goto cleanup; 
           
        }
	else if (bitpix == 16) {
           snulval = nulval;
           if(fits_update_key(outfptr,TSHORT,"BLANK",&snulval," ",&status)) goto cleanup; 
        }
	else if (bitpix == 32) {
           inulval = nulval;
           if(fits_update_key(outfptr,TINT,"BLANK",&inulval," ",&status)) goto cleanup; 
        }
    }

    headas_chat(5,"Wrote primary header keywords\n");

    if (ffrdef(outfptr,&status))  goto cleanup;
    if (addhd(outfptr,hdfil,&status))  goto cleanup;

    npixel =1;

    if ( naxis != 0 )
    {
         for (i=0; i< naxis; i++) {
	     npixel *=naxes[i];
         }

         if( strcasecmp(datfil,"none") == 0 || strcasecmp(datfil,"n") == 0)
         {
             if (crnullimg(outfptr,nulval,npixel,&status)) goto cleanup;
         }
         else 
         {
             if (crimg(outfptr,datfil,nulflg,nulval,nskip,npixel,&status)) goto cleanup;
         }
         headas_chat(5, "Created primary array\n");
    }
    

    /* write history keywords, depending on HISTORY parameter */
    HDpar_stamp(outfptr, 0, &status); /* write to current HDU */

cleanup:

    if (outfptr) fits_close_file(outfptr,&status);
    return status;

}


int gtaxes (char * caxes, int * naxis, long * naxes, int * status)
{

    return parseLong(caxes,naxis, naxes,",",status);
 
}

int addhd(fitsfile * fptr, char * hdfile, int * status) 
{
    FILE* hdfptr =0;
    char  tmplte[300];
    char  recstr[81];
    int hdtype=0;
    int slen;

    if (*status ) return *status;

    if ( hdfile != NULL && isblankstr(hdfile) !=0 ) {
        hdfptr = fopen(hdfile,"r");
    }
    else {
        return 0;
    }

    if (hdfptr == 0 ) {
       *status =FILE_NOT_OPENED;
       return *status;
    }

    /* hopefully, this will read the whole line, so no    */
    /* characters are left over on the next call to fgets */
    while (fgets(tmplte,300,hdfptr)  !=NULL )
    {
          slen = strlen(tmplte);
          chop(tmplte);
          if( isblankstr(tmplte) == 0 && slen > 8)  /* blank keyword */
          {
                ffprec(fptr,tmplte,status);
          }
          else if (isblankstr(tmplte) != 0 && tmplte[0] !='#')
          {
                    fits_parse_template(tmplte,recstr,&hdtype,status);
                    if( hdtype ==0 || hdtype == 1)
                    {
                        ffprec(fptr,recstr,status);
                    }
          }   
    }

    return *status;
}

int isblankstr(char* str)
{
   int i;
   int len;
   int status=0;

   len =strlen(str);
   for (i=0; i<len; i++)
   {
      status = status || !isspace( (int) str[i]);
   }
   return status;
}


void chop(char* str)
{
   int i;
   if (str == NULL ) return;
   i =strlen(str) -1;
   while(i >= 0 && isspace((int) str[i]) ) i--;
   str[++i]='\0';
}

int crimg(fitsfile * fptr, char * datfil,int nulflg, double nulval,int nskip,int npixel,int *status)

{
     FILE * dtfptr=0;
     char dtline[30000]; 
     double dvalue[3000];
     int fields, fpixel, ntodo,i;


     if (*status) return *status;

     if( datfil == NULL ) return 0;

     chop (datfil);

     if( strcmp(datfil,"-") ==0 ) dtfptr=stdin;
     else  dtfptr=fopen(datfil,"r");
   
     if(dtfptr == NULL ) {
        *status =FILE_NOT_OPENED;
        return *status;
     }

     for (i=0; i<nskip; i++ )
     {
         fgets(dtline,1,dtfptr);
     }

     ntodo = npixel;
     fpixel = 1;


     while ( ntodo > 0 && fgets(dtline,30000,dtfptr))
     {
	if( isblankstr(dtline) !=0 && dtline[0] !='#' ) 
        {
                parseDouble(dtline,&fields,dvalue,", ",status);
		fields = (fields > ntodo ) ? ntodo : fields;
                if (nulflg )
                {
			ffppnd(fptr,1,fpixel,fields,dvalue,nulval,status);
                }
                else 
		{
			ffpprd(fptr,1,fpixel,fields,dvalue,status);
		}
		ntodo -=fields;
		fpixel += fields;
	}

     }

    return *status;         

}

int crnullimg(fitsfile * fptr, double nulval,int npixel,int *status)

{
     double dvalue[3000];
     int fpixel, todo,i;

     fpixel = 1;

     while (npixel > 0)
     {
     	todo = (3000 > npixel) ? npixel : 3000;
     	for (i =0; i <todo; i++ )
     	{
		 dvalue[i] = nulval;
      	}
	if(ffpprd(fptr,1,fpixel,todo,dvalue,status)) return *status;

	fpixel +=todo;
	npixel -=todo;
      }

      return *status;
}
     
