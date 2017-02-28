#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_error.h"

#define MAXFLD 999
#define MAXMSG 256

typedef enum ftcreate_error_code {
     BASE_ERROR = 2000,
     PARSE_ERROR
} ftcreate_error_code;

       

/*
HISTORY
-------
  Version 1.0 written by William Pence and Ziqin Pan, NASA/GSFC, September 2002
*/

#define TOOLSUB ftcreate
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int ftcreate (void);
int ftcreate_getpar (char* cdfile, char* dtfile, char* hdfile, char* outfile,
                     char* tabtyp, int* nskip, int* nrows, 
                     int* morehd, char* extnam, char* aundef, long* iundef);
int ftcreate_work (char* cdfile, char* dtfile, char* hdfile, char* outfile,
                   char* tabtyp, int nskip, int nrows, 
                   int morehd, char* extnam, char* aundef, long iundef);
int getcoldef(char* cdfile, char* tabtyp, int maxdim,
            int* nfield,char** ttype, char** tform, char** tunit,
            int* datacode, long* repeat, int* freefm,
            long* begcol, long* twidth, int* status);
int dohead(fitsfile* fptr, char* hdfile, int * status);
int dodata(fitsfile* fptr, char* dtfile, int nskip, int nrows,
            int freefm, int nfield, long* begcol, long* twidth, 
            int* datacode,long* repeat, int* status);
int gtoken( int nfields, char* line, int start, long* begcol, long* twidth);
void chop(char *str);
int isblankstr(char *str);

/*---------------------------------------------------------------------------*/
int ftcreate (void)
{
/*  Create  a  FITS  table  extension  from  ASCII   files 
    describing the  format and content of the table.
*/

    char cdfile[PIL_LINESIZE], dtfile[PIL_LINESIZE];
    char hdfile[PIL_LINESIZE], outfile[PIL_LINESIZE];
    char tabtyp[PIL_LINESIZE], extnam[PIL_LINESIZE];
    char aundef[PIL_LINESIZE];
    long iundef; 
    int nskip, morehd, nrows, status;

    static char taskname[80] = "ftcreate";
    static char version[8] = "1.00";

    /* Register taskname and version. */

    set_toolname(taskname);
    set_toolversion(version);

    /*  get input parameters */
    status = ftcreate_getpar (cdfile, dtfile, hdfile, outfile,
              tabtyp, &nskip, &nrows,&morehd, extnam, aundef, &iundef);

    /* call work function to create the output file */
    if (!status)
        status = ftcreate_work (cdfile, dtfile, hdfile, outfile,
                    tabtyp, nskip, nrows,morehd, extnam, aundef, iundef);

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftcreate_getpar(
    char* cdfile,     /* O - Name of column descriptor file */ 
    char* dtfile,     /* O - name of ASCII data template file */ 
    char* hdfile,     /* O - name of ASCII header template file */ 
    char* outfile,    /* O - name of output FITS file */
    char* tabtyp,     /* O - ASCII or BINARY type of FITS extension to be
                             created */
    int* nskip,       /* O - number of lines in data templete file to
                             skip over  */
    int* nrows,       /* O - number of rows in the data template file to
                             read  */
    int* morehd,      /* O - reserve room in header for this many more
                             keywords */
    char* extnam,     /* O - value for the FITS EXTNAME keyword */ 
    char* aundef,     /* O - ASCII value to use for undefined values in
                             an ASCII table */
    long* iundef)      /* O - integer value to use for undefined values
                             in a binary table */

/*  read input parameters for the ftcreate task from the .par file */
{
    int status;
    char msg[MAXMSG];

    if ((status = PILGetFname("cdfile", cdfile))) {
      sprintf(msg, "Error reading the 'cdfile' parameter.");
      HD_ERROR_THROW(msg,status);
    }
    else if ((status = PILGetFname("datafile", dtfile))) {
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

    else if ((status = PILGetString("tabtyp", tabtyp))) {
      sprintf(msg, "Error reading the 'tabtyp' parameter.");
      HD_ERROR_THROW(msg,status);
    }
    else if ((status = PILGetInt("nskip", nskip))) {
      sprintf(msg, "Error reading the 'nskip' parameter");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetInt("nrows", nrows))) {
      sprintf(msg, "Error reading the 'nrows' parameter");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetInt("morehdr", morehd))) {
      sprintf(msg, "Error reading the 'morehdr' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("extname", extnam))) {
      sprintf(msg, "Error reading the 'extname' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetString("anull", aundef))) {
      sprintf(msg, "Error reading the 'anull' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetLong("inull", (long*) iundef))) {
      sprintf(msg, "Error reading the 'inull' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftcreate_work(
    char* cdfile,     /* I - Name of column descriptor file */ 
    char* dtfile,     /* I - name of ASCII data template file */ 
    char* hdfile,     /* I - name of ASCII header template file */ 
    char* outfile,    /* I - name of output FITS file */
    char* tabtyp,     /* I - ASCII or BINARY type of FITS extension to be
                             created */
    int nskip,        /* I - number of lines in data templete file to
                             skip over  */
    int nrows,        /* I - number of rows in the data template file to
                             read  */
    int morehd,       /* I - reserve room in header for this many more
                             keywords */
    char* extnam,     /* I - value for the FITS EXTNAME keyword */ 
    char* aundef,     /* I - ASCII value to use for undefined values in
                             an ASCII table */
    long iundef)      /* I - integer value to use for undefined values
                             in a binary table */

{
    char *ttype[MAXFLD], *tform[MAXFLD], *tunit[MAXFLD];
    int nfield;
    long begcol[MAXFLD],twidth[MAXFLD];
    long repeat[MAXFLD];
    int datacode[MAXFLD];
    int freefm;
    char* comment;
    char msg[MAXMSG];

    fitsfile *outfptr = 0;
    int status = 0;
    int i;

    /* get the column definitions */
    if(getcoldef(cdfile,tabtyp,MAXFLD,&nfield,ttype,tform,tunit,datacode,
           repeat,&freefm,begcol,twidth,&status) ) {
        sprintf(msg,"Error to get column definitions");
        HD_ERROR_THROW(msg,status);
        goto cleanup;
    }
    headas_clobberfile(outfile);  /* delete existing file if clobber=YES */

    if (fits_create_file(&outfptr, outfile, &status)) {
        sprintf(msg,"Error to create outfile");
        HD_ERROR_THROW(msg,status);
        goto cleanup;
    }
    headas_chat(5,"Created the output file:\n %s\n", outfile);
   
    if(tabtyp[0] == 'A' || tabtyp[0] == 'a') 
    {
         if(fits_create_tbl(outfptr,ASCII_TBL,0,nfield,ttype,tform,tunit,extnam,&status)) {
            sprintf(msg,"Error to create table");
            HD_ERROR_THROW(msg,status);
            goto cleanup;
         }

         if( strcmp(aundef," ") != 0 ) 
         {
             for( i=0; i<nfield; i++)
             {
                  comment="string representing an undefined value";
                  fits_write_keys_str(outfptr,"TNULL",i+1,1,&aundef,
                      &comment, &status);
             }
         }
    }
    else 
    {

         if(fits_create_tbl(outfptr,BINARY_TBL,0,nfield,ttype,tform,tunit,extnam,&status)) {
             sprintf(msg,"Error to create table");
             HD_ERROR_THROW(msg,status);
             goto cleanup;
         }

         if(iundef != 0) 
         {
              for(i=0; i<nfield; i++)
              {
                  if(datacode[i]== 11 || 
                     datacode[i]== 21 ||
                     datacode[i]== 41)
                  {
                       comment ="integer representing an undefined value";
                       if(fits_write_keys_lng(outfptr,"TNULL",i+1,1,&iundef,
                         &comment, &status)) {
                            sprintf(msg,"Error to write keyword");
                            HD_ERROR_THROW(msg,status);
                            goto cleanup;
                       }
                  }
              }
         }
    }

    if(dohead(outfptr,hdfile,&status) ) {
           sprintf(msg,"Error to create head");
           HD_ERROR_THROW(msg,status);
           goto cleanup;
    }
    HDpar_stamp(outfptr, 0, &status); /* write optional history keywords */


     if( morehd) 
     {
          if(fits_set_hdrsize(outfptr,morehd,&status)) {
                sprintf(msg,"Error to reserve more headfile space");
                HD_ERROR_THROW(msg,status);
                goto cleanup;
          }
     }

     if( dodata(outfptr,dtfile,nskip,nrows,freefm,nfield,
              begcol,twidth,datacode,repeat,&status)) {
                sprintf(msg,"Error to create data");
                HD_ERROR_THROW(msg,status);
                goto cleanup;
     }

cleanup:

    if (outfptr) fits_close_file(outfptr, &status);
    return(status);
}
/*------------------------------------------------------------------------*/
int getcoldef(char* cdfile, char* tabtyp, int maxdim,
            int* nfield, char** ttype, char** tform, char** tunit,
            int* datacode, long* repeat, int* freefm,
            long* begcol, long* twidth, int* status)
{
    FILE* cdfptr;
    char line[80];
    long fields, fmtcol[5],fmtwidth[5];
    char tmpstr[80];
    char msg[MAXMSG];
    long j=0; 

    *nfield =0;
    *freefm =0;

    if (*status) return *status;

    if( (cdfptr = fopen(cdfile,"r")) ==NULL ) 
    {
         *status = FILE_NOT_OPENED;
         sprintf(msg,"error reading column descriptor file");
         HD_ERROR_THROW(msg,*status);
         return *status;
    }

    while ( fgets(line,80,cdfptr) != NULL  && *nfield < maxdim)
    {

        fields = gtoken(5,line,0,fmtcol,fmtwidth);

        if (fields == 0)
        {
             continue;
        }
        else if (fields < 2 )
        {
             *status = READ_ERROR;
             sprintf(msg,"cdfile format error: column name and format required");
             HD_ERROR_THROW(msg,*status);
             return *status;
        } 
        else if( fields < 5)
        {
              *freefm = 1;
        }

        tform[*nfield] = (char*) malloc(80*sizeof(char));
        ttype[*nfield] = (char*) malloc(80*sizeof(char));
        tunit[*nfield] = (char*) malloc(80*sizeof(char));
    
        
        strncpy(ttype[*nfield],line+fmtcol[0],fmtwidth[0]);
        strncpy(tform[*nfield],line+fmtcol[1],fmtwidth[1]);
        *(ttype[*nfield]+fmtwidth[0]) ='\0';
        *(tform[*nfield]+fmtwidth[1]) ='\0';
        
        if ( fields >= 3 && fmtwidth[2] > 0) 
        {
             strncpy(tunit[*nfield],line+fmtcol[2],fmtwidth[2]);
             *(tunit[*nfield]+fmtwidth[2]) ='\0';
        }
        else
        {
             strcpy(tunit[*nfield]," ");
        }

             
             
        if( *freefm != 1 ) 
        {
             strncpy(tmpstr,line+fmtcol[3],fmtwidth[3]);
             *(tmpstr+fmtwidth[3])='\0';
             begcol[*nfield] = strtol(tmpstr, NULL,10);
             strncpy(tmpstr,line+fmtcol[4],fmtwidth[4]);
             *(tmpstr+fmtwidth[4])='\0';
             twidth[*nfield] = strtol (tmpstr,NULL,10);
             if( begcol[*nfield] == LONG_MIN ||
                 begcol[*nfield] == LONG_MAX ||
                 twidth[*nfield] == LONG_MIN ||
                 twidth[*nfield] == LONG_MAX ) 
             {
                 *status = PARSE_ERROR;
                 sprintf(msg,"error parsing column starting position and width");
                 HD_ERROR_THROW(msg,*status);
                 return *status;
             }
        }

        if (strcmp(tabtyp,"ASCII") == 0)
        { 
             fits_ascii_tform(tform[*nfield],
                   &datacode[*nfield],&repeat[*nfield],NULL,status);
             repeat[*nfield] =1;
        }
        else 
        {
             fits_binary_tform(tform[*nfield],
                    &datacode[*nfield],&repeat[*nfield],&j,status);
             if( datacode[*nfield] == 16) repeat[*nfield] /=j;
             if( datacode[*nfield] == 83) repeat[*nfield] *=2;
        }
        
        *nfield = *nfield+1;
     }

     return *status;
}
/*---------------------------------------------------------------------------*/
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
/*---------------------------------------------------------------------------*/
int dohead(fitsfile* fptr, char* hdfile, int * status)
{
     FILE* hdfptr;
     char tmplte[300], recstr[81];
     int hdtype, slen;
     char msg[MAXMSG];

     if(*status) return *status;

     if( isblankstr(hdfile) !=0 )
     {
            hdfptr=fopen(hdfile,"r");
            if( hdfptr == NULL ) 
            {
            sprintf(msg,"Can not open the  header keyword template file:%s",hdfile);
            HD_ERROR_THROW(msg,*status);
            *status = FILE_NOT_OPENED;
            return *status;
            }

            /* hopefully, this will read the whole line, so no    */
            /* characters are left over on the next call to fgets */
            while ( fgets(tmplte,300,hdfptr)  !=NULL ) 
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
     }
     return *status;
}
/*---------------------------------------------------------------------------*/
int dodata(fitsfile* fptr, char* dtfile, int nskip, int nrows,
            int freefm, int nfield, long* begcol, long* twidth, 
            int* datacode,long* repeat, int* status)
{
     FILE* dtfptr;
     char dtline[30001];
     int nelem =0;
     int dtlnum;
     int naxis2;
     int fields;
     long irow;
     long jcol;
     long elem;
     char* svalue;
     char lvalue;
     char *sptr;
     double dvalue[2];     
     unsigned long jvalue;
     char msg[MAXMSG];
     
     long i,j,r;

     if(*status) return *status;

     for (jcol=0; jcol<nfield; jcol++)
     {
          nelem +=repeat[jcol];
     }

     if( !freefm )
     {
          j=nelem-1;
          for (jcol=nfield-1; jcol>=0; jcol--)
          {
                for(r=repeat[jcol]; r>0; r--)
                {
                    twidth[j] = twidth[jcol];
                    begcol[j] = begcol[jcol]+(r-1)*twidth[jcol];
                    j--;
                }
          } 
     }

     if( strcmp(dtfile,"-") ==0 ) dtfptr=stdin;
     else  dtfptr=fopen(dtfile,"r");

     if( dtfptr == NULL ) 
     {
         sprintf(msg,"Can not open the  datafile:%s",dtfile);
         *status = FILE_NOT_OPENED;
         HD_ERROR_THROW(msg,*status);
         return *status;
     }


     for (i=0; i<nskip; i++ ) 
     {
         fgets(dtline,30000,dtfptr);
     }

     dtlnum = nskip;

     if( nrows <= 0 ) nrows =1000000000;
    
     naxis2 =0;
     irow = 0;
     fields =0;
     elem =0;

     
     for (irow = 0; irow<nrows; irow++) 
     {
           for( jcol=0; jcol<nfield; jcol++) 
           {
                for ( r=1; r<=repeat[jcol]; r++)
                {
                    elem =elem+1;
                    if( elem > fields) 
                    {
                          do
                          {
                              if(fgets(dtline,30000,dtfptr) == NULL) goto label1;
                              sptr = dtline;
                              while (*sptr == ' ')sptr++;
                          }
                          while (dtline[0]=='#' || *sptr == '\0' || *sptr == '\n' || *sptr == '\r');

                          elem =1;

                          if(freefm)
                          {
                                fields = gtoken(999,dtline,0,begcol,twidth); 
                          }
                          else 
                          {
                                fields = nelem;
                          }
                     }
                         
                     svalue=dtline+begcol[elem-1];
                     *(svalue+twidth[elem-1])='\0';

                     
                     if( strcmp(svalue,"INDEF") == 0 || strcmp(svalue,"indef") == 0)
                     {
                          ffpclu(fptr,jcol+1,irow+1,r,1,status);
                     }
                     else if (datacode[jcol] == TSTRING)
                     {
                          ffpcls(fptr,jcol+1,irow+1,r,1,&svalue,status);
                     }
                     else if (datacode[jcol] == TLOGICAL)
                     {
                          if(strcmp(svalue,"F") == 0)
                          {
                              lvalue = FALSE;
                              ffpcll(fptr,jcol+1,irow+1,r,1,&lvalue,status);
                          }
                          else 
                          {
                              lvalue = TRUE;
                              ffpcll(fptr,jcol+1,irow+1,r,1,&lvalue,status);
                          }
                     }
                     else if (datacode[jcol] == TBIT)
                     {
			  
			  /* The sscanf code was previously set to %i, but
			  this limits the range to a signed 32-bit int.
			  Tried %u, but this then does not support 0xff notation.
			  Finally used %lx because it seems to support both
			  decimal and octal notation, at least with gcc.
			  This should be verified that it works on machines
			  with sizeof(long) = 8.

			  Update 2015 05/15: Using a hex format specifier ('%lx')
			  to sscanf svalue isn't suitable either, since it can
			  misinterpret non-hex formats.
			  */
                          jvalue = strtoul(svalue,0,0);
	
                          for ( ; r<=repeat[jcol]; r++) {
                          if( ( (jvalue >> (repeat[jcol]-r)) & 1 ) == 0)
                          {	
                              lvalue =FALSE;
                              ffpclx(fptr,jcol+1,irow+1,r,1,&lvalue,status);
                          }
                          else 
                          {
                             lvalue =TRUE;
                             ffpclx(fptr,jcol+1,irow+1,r,1,&lvalue,status);
                          }
                          }
                     }
                     else 
                     { 
                          while (svalue[0] ==' ') svalue++;
                          dvalue[0] =atof(svalue);
                          ffpcld(fptr,jcol+1,irow+1,r,1,dvalue,status);
                     }
                 }
             }
             naxis2 = naxis2 + 1;
        }

label1:
        ffmkyj(fptr,"NAXIS2",naxis2,"&",status);

        return *status;
}
/*---------------------------------------------------------------------------*/
void chop(char* str)
{
   int i;
   if (str == NULL ) return;
   i =strlen(str) -1;
   while(i >= 0 && isspace((int) str[i]) ) i--;
   str[++i]='\0';
} 

