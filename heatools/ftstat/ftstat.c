#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_error.h"
#include "ftstat.h"

#define MAXMSG 256


/*
HISTORY
-------
  Version 1.0 written by William Pence and Ziqin Pan, NASA/GSFC, October 2002
  Version 1.1 Added median output & quickselect routine; Bryan Irby, Feb 2009
  Version 2.0 Fix mean computation for sigma clipping case; added new routine
              cal_col_stat_aux to remove recursion between sigma_clip and
              cal_col_stat; distinguish clipped values from null values in
              output & add # clipped to par file output. Bryan Irby, July 2011
  Version 2.1 Add mode computation.  Replaces the ftstat_median_double
              quickselect routine with new routines ftstat_mode/ftstat_mode_aux.
              Bob Wiegand, September 2011.
  Version 2.2 Add support for fixed-length vector columns; display messages
              notifying user that zero-length vector columns or columns of type
              TBIT, TLOGICAL, or TSTRING will not be computed.
              Bryan Irby, April 2013.
*/

#define TOOLSUB ftstat
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

/* Function Prototypes */
int ftstat (void);
int ftstat_getpar (char *infile, char *outfile, int *centroid, 
                int *clip, double *nsigma,int *maxiter);
int ftstat_work (char *infile, char *outfile, int centroid, int clip, double nsigma,
                 int maxiter); 
int cal_tbl_stat(hduInfo * info, int centroid, int clip, 
                double nsigma, int maxiter,int *status);
int cal_img_stat(fitsfile* infptr, hduInfo * info, int centroid, int clip,
                double nsigma, int maxiter, int *status);
int cal_col_stat(Column * column, int centroid, int clip,
                double nsigma, int maxiter, int *status);
int cal_col_stat_aux(Column *column, int get_all_stats, int *status);
int sigma_clip(Column *column, double nsigma, int maxiter, int *status);
int print_stat(FILE* outfptr, char* infile, 
                hduInfo info, int centroid, int clip);
int read_hdu_data(fitsfile * infptr, hduInfo *info, int * status);
int ftstat_putpar(hduInfo * info, int centroid, int* status);
void ftstat_free(hduInfo * info);
static void ftstat_mode(Column *column);
/*---------------------------------------------------------------------------*/
int ftstat (void)
{
/*  Compute statistics for 2D images and table columns */

    char infile[PIL_LINESIZE], outfile[PIL_LINESIZE];
    int centroid, clip, maxiter, status;
    double nsigma;
    static char taskname[80] = "ftstat";
    static char version[8] = "2.2";

    /* Register taskname and version. */
    set_toolname(taskname);
    set_toolversion(version);

    /* Get input parameters */
    status = ftstat_getpar(infile, outfile, &centroid, &clip, &nsigma, 
                 &maxiter);

    /* Call work function to compute statistics for 2D images 
       and table columns */
    if (!status)
        status = ftstat_work(infile, outfile, centroid, clip, nsigma,
                   maxiter);

    return(status);
}
/*---------------------------------------------------------------------------*/
int ftstat_getpar(
    char *infile,    /* O - Input file name */ 
    char *outfile,   /* O - optional output file name */
    int *centroid,   /* O - Whether to compute image centroid */
    int *clip,       /* O - whether to iteratively reject N*sigma
                            outliers and recompute the statistics */
    double *nsigma,  /* O - if clip = yes, then this is the sigma
                            rejection factor */
    int *maxiter)    /* O - if clip = yes this is the maximum number
                            of iterations */ 

/*  read input parameters for the ftstat task from the .par file */
{
    int status;
    char msg[MAXMSG];

    if ((status = PILGetFname("infile", infile))) {
      sprintf(msg, "Error reading the 'infile' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetFname("outfile", outfile))) {
      sprintf(msg, "Error reading the 'outfile' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetBool("centroid", centroid))) {
      sprintf(msg, "Error reading the 'centroid' parameter.");
      HD_ERROR_THROW(msg,status);
    }


    else if ((status = PILGetBool("clip", clip))) {
      sprintf(msg, "Error reading the 'clip' parameter.");
      HD_ERROR_THROW(msg,status);
    }


    else if ((status = PILGetReal("nsigma", nsigma))) {
      sprintf(msg, "Error reading the 'nsigma' parameter.");
      HD_ERROR_THROW(msg,status);
    }

    else if ((status = PILGetInt("maxiter", maxiter))) {
      sprintf(msg, "Error reading the 'maxiter' parameter.");
      HD_ERROR_THROW(msg,status);
    }


    return(status);
}
/*---------------------------------------------------------------------------*/
int ftstat_work(
    char *infile,   /* I - Input file name */ 
    char *outfile,  /* I - Output file name */ 
    int centroid,   /* I - Whether to compute image centroid */
    int clip,       /* I - whether to iteratively reject N*sigma
                            outliers and recompute the statistics */
    double nsigma,     /* I - if clip = yes, then this is the sigma
                            rejection factor */
    int maxiter)    /* I - if clip = yes this is the maximum number
                            of iterations */ 
{
    FILE *stream;
    fitsfile *infptr = 0;
    int status = 0, len;
    char *cptr;
    char msg[MAXMSG];
    hduInfo info;

    info.column = 0;
    
    /* strip leading and trailing spaces off of 'outfile' */
    cptr = outfile;
    while (*cptr == ' ')cptr++;
    len = strlen(cptr);
    while (len > 0 && *(cptr+len-1) == ' ') {
        len--;
        *(cptr+len) = '\0';
    }

    /* determine output stream (stdout or to a file) */
    if (*cptr == '\0' || !strcmp(cptr, "-") || 
        !strcmp(cptr, "stdout") || !strcmp(cptr, "STDOUT") ) {
        stream = stdout;
    } else {
        if (*cptr == '!') {  /* clobber the output file */
            cptr++;
            remove(cptr);
        } else {
            headas_clobberfile(cptr);  /* delete existing file if clobber=YES */
        }

        stream = fopen(cptr, "r"); /* does file already exist? */

        if (stream)
        {
            fclose(stream);         /* close file and exit with error */
            sprintf(msg,"Error: Output file already exists:\n %s",cptr);
            status = FILE_NOT_CREATED;
            HD_ERROR_THROW(msg,status);
            return(status); 
        }

        if (!(stream = fopen(cptr, "w"))) {
            sprintf(msg,"Error creating output file:\n %s",cptr);
            status = FILE_NOT_CREATED;
            HD_ERROR_THROW(msg,status);
            return(status); 
        }
    }

    /* Open the input file, and move to first 'interesting' HDU */
    if ( fits_open_data(&infptr, infile, READONLY, &status)) goto cleanup;
    headas_chat(5,"Opened the input file:\n %s\n",infile);

    /* get the current HDU data*/
    if ( read_hdu_data(infptr,&info,&status)) goto cleanup;
    headas_chat(5,"Read data from current HDU\n");
   
    if( info.hdutype == IMAGE_HDU)
        cal_img_stat(infptr,&info,centroid,clip,nsigma,maxiter,&status);
    else if( info.hdutype == ASCII_TBL || info.hdutype == BINARY_TBL )
        cal_tbl_stat(&info,0,clip,nsigma,maxiter,&status);

    print_stat(stream,infile,info,centroid,clip);

    ftstat_putpar(&info,centroid,&status);


cleanup:

    if (infptr)  fits_close_file(infptr,  &status);
    if (status) fits_report_error(stderr, status);
    if (info.column)  ftstat_free(&info);
    return(status);
}

/*---------------------------------------------------------------------------*/
int cal_img_stat(fitsfile* infptr, hduInfo * info, int centroid, int clip,
       double nsigma, int maxiter, int *status)
{
    long i;
    long x, y;
    double sumx,sumy,sumx2,sumy2;
    double xrefval=0., yrefval=0., xrefpix=0.;
    double yrefpix=0., xinc=0., yinc=0., rot=0.;
    char coordtype[20];

    info->column[0].wcflg =TRUE;
    if(fits_read_img_coord(infptr,&xrefval,&yrefval,&xrefpix,
           &yrefpix,&xinc,&yinc,&rot,coordtype,status))  
    {
         info->column[0].wcflg=FALSE;
         *status =0;
    }

    
    cal_tbl_stat(info,centroid,clip,nsigma,maxiter,status);
    info->column[0].ymin = info->column[0].xmin/info->naxes[0]+1;
    info->column[0].xmin = info->column[0].xmin%info->naxes[0]+1;
    info->column[0].ymax = info->column[0].xmax/info->naxes[0]+1;
    info->column[0].xmax = info->column[0].xmax%info->naxes[0]+1;


    if(centroid) 
    {
        sumx=0.;
        sumy=0.;
        sumx2=0.;
        sumy2=0.;
        for (i=0; i<info->column[0].total; i++)
        {
           if( !info->column[0].nulflag[i])
           {
              x =i%info->naxes[0]+1;
              y =i/info->naxes[0]+1;
              sumx +=x*info->column[0].dvalue[i];
              sumy +=y*info->column[0].dvalue[i];
              sumx2 +=x*x*info->column[0].dvalue[i];
              sumy2 +=y*y*info->column[0].dvalue[i];
           }
        }

        if( info->column[0].sum > 0.) 
        {
           info->column[0].cx=sumx/info->column[0].sum;
           info->column[0].cy=sumy/info->column[0].sum;
           info->column[0].dx =sqrt(sumx2/info->column[0].sum -
                                   info->column[0].cx*info->column[0].cx);
           info->column[0].dy =sqrt(sumy2/info->column[0].sum -
                                   info->column[0].cy*info->column[0].cy);
        }

    }

    if(info->column[0].wcflg) 
    {

    if(fits_pix_to_world(info->column[0].xmin, info->column[0].ymin,
       xrefval, yrefval,xrefpix, yrefpix, xinc, yinc,rot,
       coordtype, &info->column[0].rxmin,&info->column[0].rymin,status)) 
    {
       info->column[0].wcflg = FALSE;
       *status =0;
       return *status;
    }

    if(fits_pix_to_world(info->column[0].xmax, info->column[0].ymax,
       xrefval, yrefval,xrefpix, yrefpix, xinc, yinc,rot,
       coordtype, &info->column[0].rxmax,&info->column[0].rymax,status))
    {
       info->column[0].wcflg = FALSE;
       *status =0;
       return *status;
    }

    if(fits_pix_to_world(info->column[0].cx, info->column[0].cy,
       xrefval, yrefval,xrefpix, yrefpix, xinc, yinc,rot,
       coordtype, &info->column[0].rcx,&info->column[0].rcy,status)) 
    {
       info->column[0].wcflg = FALSE;
       *status =0;
       return *status;
    }

    if(fits_pix_to_world(info->column[0].dx+info->column[0].cx, 
                         info->column[0].dy+info->column[0].cy,
       xrefval, yrefval,xrefpix, yrefpix, xinc, yinc,rot,
       coordtype, &info->column[0].rdx,&info->column[0].rdy,status)) 
    {
       info->column[0].wcflg = FALSE;
       *status =0;
       return *status;
    }
     info->column[0].rdx -= info->column[0].rcx;
     info->column[0].rdy -= info->column[0].rcy;
    if(info->column[0].rdx <0) info->column[0].rdx=-info->column[0].rdx;
    if(info->column[0].rdy <0) info->column[0].rdy=-info->column[0].rdy;
    }

    return *status;

}

/*---------------------------------------------------------------------------*/
int read_hdu_data(fitsfile * infptr, hduInfo *info, int * status)
{
    int j;
    int anyf;
    long nrows_save;
    char str[20];
    char msg[MAXMSG];

    /* Initialize naxes[] */
    info->naxes[0]=0;
    info->naxes[1]=0;

    if (fits_get_hdu_type(infptr, &(info->hdutype), status)) return(*status);

    if ( info->hdutype == ASCII_TBL || info->hdutype == BINARY_TBL )
    {
          if(fits_get_num_cols(infptr,&(info->ncols),status)) return(*status);
          if(fits_get_num_rows(infptr,&(info->nrows),status)) return(*status);
          info->column = (Column *) calloc(info->ncols,sizeof(Column));

          /* Save value of nrows as it may change below for vector columns */
          nrows_save=info->nrows;

          for (j=0; j<info->ncols; j++)
          {
              sprintf(str,"%d",j+1);

              if(fits_get_colname(infptr,1,str,info->column[j].name,&anyf,status))
                 return(*status);

              if(fits_get_coltype(infptr,j+1,&(info->column[j].dtype),
                 &(info->column[j].repeat),&(info->column[j].width),status))
                 return(*status);

              sprintf(str,"%s%d","TUNIT",j+1);

              if(fits_read_key(infptr,TSTRING,str,info->column[j].unit,NULL,status)) 
              {
                   strcpy(info->column[j].unit," \0");
                   *status =0;
              }

              /* Allow for (fixed-length) vector columns: */
              info->nrows=nrows_save;
              if((info->column[j].repeat > 1) && (info->column[j].dtype != TBIT))
                 info->nrows=(info->nrows)*(info->column[j].repeat);

              /* Allocate memory; this could fail for very large tables, so we
	       * may eventually want to read the data in smaller chunks. */
              (info->column[j]).dvalue =(double*) calloc(info->nrows,sizeof(double));
              (info->column[j]).nulflag =(char*) calloc(info->nrows,sizeof(char));

              info->column[j].total = info->nrows;

              if(info->column[j].repeat > 0) /* Avoid columns with zero elements */
              {
                 if(info->column[j].dtype == TBIT ||
                    info->column[j].dtype == TBYTE ||
                 /* info->column[j].dtype == TLOGICAL || */
                    info->column[j].dtype == TUSHORT ||
                    info->column[j].dtype == TSHORT ||
                    info->column[j].dtype == TUINT ||
                    info->column[j].dtype == TINT ||
                    info->column[j].dtype == TULONG ||
                    info->column[j].dtype == TLONG ||
                    info->column[j].dtype == TFLOAT ||
                    info->column[j].dtype == TDOUBLE )
                 {
                    if(fits_read_colnull_dbl(infptr,j+1,1,1,info->nrows,
                      (info->column[j]).dvalue,(info->column[j]).nulflag,&anyf,status))
                       return(*status);
                 }
              }

          } /* end loop over ncols */
    }
    else if (info->hdutype == IMAGE_HDU)
    {
          if(fits_read_key(infptr,TSTRING,"XTENSION",info->extname,
                  NULL,status)==0) 
          {
               
             if(fits_read_key(infptr,TSTRING,"EXTNAME",info->extname,
                  NULL,status)) 
             {
               strcpy(info->extname," \0");
               *status=0;
             }
          }
          else 
          {
               strcpy(info->extname,"PRIMARY\0");
               *status=0;
          }

          if(fits_get_img_dim(infptr,&(info->naxis),status)) return(*status);

          /* allow 3D or 4D files as long as the 3rd and 4th dimensions = 1 */
          if(info->naxis > 4 || info->naxis < 1) 
          {
              sprintf(msg,"NAXIS = %d; ftstat only supports 1- or 2-D images", 
              info->naxis);
              info->column = 0;
              *status =108;
              HD_ERROR_THROW(msg,*status);
              return *status;
          }
          if(fits_get_img_size(infptr,info->naxis,info->naxes,status)) return(*status);

          if(info->naxis >= 3) 
	  {
	    if(info->naxes[2] != 1)
            {
              sprintf(msg,"NAXIS3 = %ld; ftstat only supports 1- or 2-D images", 
              info->naxes[2]);
              info->column = 0;
              *status =108;
              HD_ERROR_THROW(msg,*status);
              return *status;
            }
          }
 
          if(info->naxis == 4) 
	  {
	    if(info->naxes[3] != 1)
            {
              sprintf(msg,"NAXIS4 = %ld; ftstat only supports 1- or 2-D images", 
              info->naxes[3]);
              info->column = 0;
              *status =108;
              HD_ERROR_THROW(msg,*status);
              return *status;
            }
          }

          info->ncols=1;
          if(info->naxes[1] == 0) /* 1-D array */
          {
            info->nrows=info->naxes[0];
          }
          else
          {
            info->nrows=info->naxes[0]*info->naxes[1];
          }
          info->column = (Column *) calloc(info->ncols,sizeof(Column));
          info->column[0].total =info->nrows;
          info->column[0].dtype =TDOUBLE;
          info->column[0].dvalue =(double*) calloc(info->nrows,sizeof(double));
          info->column[0].nulflag =(char*) calloc(info->nrows,sizeof(char));
          if(fits_read_imgnull(infptr,TDOUBLE,1,info->column[0].total,
                info->column[0].dvalue, info->column[0].nulflag, &anyf,status))
                return *status;
    }
          
    return(*status);
}

/*---------------------------------------------------------------------------*/
int cal_tbl_stat(hduInfo * info, int centroid, int clip, 
      double nsigma, int maxiter,int *status)
{
    int j;

    for (j=0; j<info->ncols; j++) 
    {
         cal_col_stat(info->column+j,centroid,clip,nsigma,maxiter,status);
    }

    return *status;
}

/*---------------------------------------------------------------------------*/
int cal_col_stat(Column * column, int centroid, int clip,
      double nsigma, int maxiter, int *status)
{

    column->median =0.;
    column->max =0.;
    column->min =0.;
    column->std =0.;
    column->good =0;
    column->null =0;
    column->clipped =0;
    column->ymin =1;
    column->ymax =1;
    column->cx =0;
    column->cy =0;
    column->dx =0;
    column->dy =0;
    column->pass =0;

    if(column->dtype == TBIT ||
       column->dtype == TBYTE ||
    /*   column->dtype == TLOGICAL || */
       column->dtype == TUSHORT ||
       column->dtype == TSHORT ||
       column->dtype == TUINT ||
       column->dtype == TINT ||
       column->dtype == TULONG ||
       column->dtype == TLONG ||
       column->dtype == TFLOAT ||
       column->dtype == TDOUBLE )
    {
       if(clip && !centroid) sigma_clip(column,nsigma,maxiter,status);
       cal_col_stat_aux(column,1,status);
    }

    return *status;
}

/*--------------------------------------------------------------------------*/

static int compare_doubles(const void * p1, const void * p2)
{
	double delta;
	delta = *(double*) p1 - *(double*) p2;
	if (delta < 0)
		return -1;
	else if (delta > 0)
		return 1;
	return 0;
}


/*---------------------------------------------------------------------------*/
int cal_col_stat_aux(Column *column, int get_all_stats, int *status) 
{
       int i, j, k, first;

       first = TRUE;

       /* Initialize # good values, sum, and sum^2 each time to allow for
	  iterative calls from sigma_clip */
       column->good = 0.;
       column->sum = 0.;
       column->sumsq = 0.;
        
       for ( i=0; i<column->total; i++)
       {
            if ( column->nulflag[i] == 0) 
            {

                (column->good)++;
                column->sum += column->dvalue[i];
                column->sumsq += column->dvalue[i] * column->dvalue[i];

                /* Initialize min & max at first element */
                if (first)
                {
                    column->max = column->dvalue[i];
                    column->min = column->dvalue[i];
                    column->xmax = i;
                    column->xmin = i;
                    first = FALSE;
                }

                /* Check for new min & max */
                if (get_all_stats)
                {
                    if (column->max < column->dvalue[i])
                    {
                        column->max = column->dvalue[i];
                        column->xmax = i;
                    }

                    if (column->min > column->dvalue[i])
                    {
                        column->min = column->dvalue[i];
                        column->xmin = i;
                    }
                 }

              /* Tally nulls on first call (do not repeat for subsequent calls) */
            } else if (column->pass == 0) (column->null)++;
       }

       if(column->good > 0) 
       {
           column->mean = column->sum/column->good;
           column->std = sqrt(column->sumsq/column->good - column->mean*column->mean);

           /* Remove NULLs before computing median: */
           if (get_all_stats)
           {
               if (column->null > 0)
               {
                   k=0;
                   for (j=0; j<column->total; j++)
                   {
                       /* Shift good values into the lower column->good elements: */
                       if (column->nulflag[j]) k++;
                       else column->dvalue[j-k] = column->dvalue[j];
                   }
               }

               qsort(column->dvalue, column->good, sizeof(column->dvalue[0]), &compare_doubles);

               /* ftstat defines median as the 'lower' median */
               k = column->good / 2;
               column->median = column->dvalue[k];

               ftstat_mode(column);
           }
       }

       (column->pass)++;
       return *status;
}

/*---------------------------------------------------------------------------*/
int sigma_clip(Column *column, double nsigma, int maxiter, int *status) 
{
    int i, iterations;

    iterations = 0;

    if(maxiter == 0 ) return (*status);
    if(column->converge) return (*status);

    /* Reject outliers until convergence or max # of iterations is reached */
    while (!column->converge && iterations < maxiter) {

         column->converge = TRUE;

         /* (Re)compute mean & sigma (but not min/max/median) */
         cal_col_stat_aux(column,0,status);

         for (i=0;i<column->total; i++)
         {
             if(column->nulflag[i] == FALSE) 
             {
                if( column->dvalue[i] >  column->mean + nsigma* column->std ||
                    column->dvalue[i] <  column->mean - nsigma* column->std )
                {
                    column->converge = FALSE;
                    column->nulflag[i] = TRUE;
                    (column->clipped)++;
                    (column->good)--;
                }
             }
         }
         ++iterations;
    }

    return (*status);
}


/*---------------------------------------------------------------------------*/
int print_stat(FILE* outfptr, char* infile, hduInfo info, int centroid, int clip) {


 int i;

    fprintf(outfptr,"====== statistics for %s ======\n",infile);
 for (i=0; i<info.ncols; i++) {

    fprintf(outfptr,"\n");
    if(info.hdutype == IMAGE_HDU)
    {
       if(strcmp(info.extname," ") !=0) {
          fprintf(outfptr,"           extname:  %s\n",info.extname);
       }
       fprintf(outfptr,"     minimum value:  %f\n", info.column[i].min);
       fprintf(outfptr,"       pixel coord:  (%ld,%ld)\n", info.column[i].xmin,info.column[i].ymin);

       if(info.column[i].wcflg) 
       {
          fprintf(outfptr,"       world coord:  (%g,%g)\n",
             info.column[i].rxmin,info.column[i].rymin);
       }

       fprintf(outfptr,"     maximum value:  %g\n", info.column[i].max);
       fprintf(outfptr,"       pixel coord:  (%ld,%ld)\n", info.column[i].xmax,info.column[i].ymax);

       if(info.column[i].wcflg) 
       {
          fprintf(outfptr,"       world coord:  (%g,%g)\n",
             info.column[i].rxmax,info.column[i].rymax);
       }

       fprintf(outfptr,"              mean:  %.10g\n", info.column[i].mean);
       fprintf(outfptr,"            median:  %.10g\n", info.column[i].median);
       fprintf(outfptr,"             sigma:  %.10g\n", info.column[i].std);
       fprintf(outfptr,"               sum:  %.10g\n", info.column[i].sum);
       if(clip && !centroid) {
           if(info.column[i].converge) 
               fprintf(outfptr,"            cnvrgd:  YES\n");
           else
               fprintf(outfptr,"            cnvrgd:  NO\n");
       }

       if(centroid) {
           fprintf(outfptr,"      cntrd[pixel]:  (%g,%g)\n", info.column[i].cx,info.column[i].cy);

           if(info.column[i].wcflg) 
           {
              fprintf(outfptr,"      cntrd[world]:  (%g,%g)\n",
                info.column[i].rcx,info.column[i].rcy);
           }

           fprintf(outfptr,"sigma_cntrd[pixel]:  (%g,%g)\n",
             info.column[i].dx,info.column[i].dy);

           if(info.column[i].wcflg) 
           {
              fprintf(outfptr,"sigma_cntrd[world]:  (%g,%g)\n",
                info.column[i].rdx,info.column[i].rdy);
           }
       }

       fprintf(outfptr,"      #good pixels:  %ld\n", info.column[i].good);
       fprintf(outfptr,"      #null pixels:  %ld\n", info.column[i].null);
       if(clip & !centroid)
       fprintf(outfptr,"      #clip pixels:  %ld\n", info.column[i].clipped);

       fprintf(outfptr,"              mode:  %.10g\n", info.column[i].mode);
       fprintf(outfptr,"             modes:  %d\n", info.column[i].modes);
       fprintf(outfptr,"             modez:  %d\n", info.column[i].mode_depth);
    } 
    else  /* statistics of a table column */
    {
       if( strcmp(info.column[i].unit," ") !=0 ) 
       {
          fprintf(outfptr,"%s[%s]:\n", info.column[i].name,info.column[i].unit);
       }
       else 
       {
          fprintf(outfptr,"%s:\n", info.column[i].name);
       }
       if(info.column[i].dtype < 0 )
       {
         fprintf(outfptr,"Not computing statistics for variable-length array column.\n");
	 continue;
       }
       if(info.column[i].repeat == 0 )
       {
         fprintf(outfptr,"Not computing statistics for zero-element column.\n");
	 continue;
       }
       if (info.column[i].dtype  == TBIT)
       {
         fprintf(outfptr,"Not computing statistics for bit column.\n");
	 continue;
       }
       if (info.column[i].dtype  == TLOGICAL)
       {
         fprintf(outfptr,"Not computing statistics for logical column.\n");
	 continue;
       }
       if (info.column[i].dtype  == TSTRING)
       {
         fprintf(outfptr,"Not computing statistics for string column.\n");
	 continue;
       } else if (info.column[i].dtype  == TFLOAT) {
           fprintf(outfptr,"   min:       %.6g\n", info.column[i].min);
           fprintf(outfptr,"   max:       %.6g\n", info.column[i].max);
           fprintf(outfptr,"  mean:       %.6g\n", info.column[i].mean);
           fprintf(outfptr,"median:       %.6g\n", info.column[i].median);
           fprintf(outfptr," sigma:       %.6g\n", info.column[i].std);
           fprintf(outfptr,"   sum:       %.6g\n", info.column[i].sum);
           fprintf(outfptr,"  mode:       %.6g\n", info.column[i].mode);
           fprintf(outfptr," modes:         %d\n", info.column[i].modes);
           fprintf(outfptr," modez:         %d\n", info.column[i].mode_depth);

       } else {
           fprintf(outfptr,"   min:       %.15g\n", info.column[i].min);
           fprintf(outfptr,"   max:       %.15g\n", info.column[i].max);
           fprintf(outfptr,"  mean:       %.15g\n", info.column[i].mean);
           fprintf(outfptr,"median:       %.15g\n", info.column[i].median);
           fprintf(outfptr," sigma:       %.15g\n", info.column[i].std);
           fprintf(outfptr,"   sum:       %.15g\n", info.column[i].sum);
           fprintf(outfptr,"  mode:       %.15g\n", info.column[i].mode);
           fprintf(outfptr," modes:          %d\n", info.column[i].modes);
           fprintf(outfptr," modez:          %d\n", info.column[i].mode_depth);
       }

       if(clip)
       {
           if(info.column[i].converge) 
               fprintf(outfptr,"cnvrgd:       YES\n");
           else
               fprintf(outfptr,"cnvrgd:       NO\n");
       }
       fprintf(outfptr,"  good:       %ld\n", info.column[i].good);
       fprintf(outfptr,"  null:       %ld\n", info.column[i].null);
       if(clip)
       fprintf(outfptr,"  clip:       %ld\n", info.column[i].clipped);
    }
  } /* loop over all columns */

    return 0;
}

/*---------------------------------------------------------------------------*/
int ftstat_putpar(hduInfo * info, int centroid, int* status)
{
    int n;
    char msg[MAXMSG];

    if(*status) return *status;

    n =info->ncols -1;

    if((*status = PILPutReal("min",info->column[n].min))) 
    {
        sprintf(msg,"Error writing the 'min' parameter");
        HD_ERROR_THROW(msg,*status);
        return *status;
    }

    if((*status = PILPutReal("max",info->column[n].max))) 
    {
        sprintf(msg,"Error writing the 'max' parameter");
        HD_ERROR_THROW(msg,*status);
        return *status;
    }

    if((*status = PILPutInt("xmin",info->column[n].xmin))) 
    {
        sprintf(msg,"Error writing the 'xmin' parameter");
        HD_ERROR_THROW(msg,*status);
        return *status;
    }

    if((*status = PILPutInt("ymin",info->column[n].ymin))) 
    {
        sprintf(msg,"Error writing the 'ymin' parameter");
        HD_ERROR_THROW(msg,*status);
        return *status;
    }

    if((*status = PILPutInt("xmax",info->column[n].xmax))) 
    {
        sprintf(msg,"Error writing the 'xmax' parameter");
        HD_ERROR_THROW(msg,*status);
        return *status;
    }
    if((*status = PILPutInt("ymax",info->column[n].ymax))) 
    {
        sprintf(msg,"Error writing the 'ymax' parameter");
        HD_ERROR_THROW(msg,*status);
        return *status;
    }

    if((*status = PILPutReal("mean",info->column[n].mean))) 
    {
        sprintf(msg,"Error writing the 'mean' parameter");
        HD_ERROR_THROW(msg,*status);
        return *status;
    }

    if((*status = PILPutReal("median",info->column[n].median))) 
    {
        sprintf(msg,"Error writing the 'median' parameter");
        HD_ERROR_THROW(msg,*status);
        return *status;
    }

    if((*status = PILPutReal("sigma",info->column[n].std))) 
    {
        sprintf(msg,"Error writing the 'sigma' parameter");
        HD_ERROR_THROW(msg,*status);
        return *status;
    }

    if((*status = PILPutReal("sum",info->column[n].sum))) 
    {
        sprintf(msg,"Error writing the 'sum' parameter");
        HD_ERROR_THROW(msg,*status);
        return *status;
    }

    if((*status = PILPutInt("null",info->column[n].null))) 
    {
        sprintf(msg,"Error writing the 'null' parameter");
        HD_ERROR_THROW(msg,*status);
        return *status;
    }

    if((*status = PILPutInt("good",info->column[n].good))) 
    {
        sprintf(msg,"Error writing the 'good' parameter");
        HD_ERROR_THROW(msg,*status);
        return *status;
    }

    if((*status = PILPutInt("clipped",info->column[n].clipped))) 
    {
        sprintf(msg,"Error writing the 'clipped' parameter");
        HD_ERROR_THROW(msg,*status);
        return *status;
    }

    if((*status = PILPutReal("xcent",info->column[n].cx))) 
    {
        sprintf(msg,"Error writing the 'xcent' parameter");
        HD_ERROR_THROW(msg,*status);
        return *status;
    }
    if((*status = PILPutReal("ycent",info->column[n].cy))) 
    {
       sprintf(msg,"Error writing the 'ycent' parameter");
       HD_ERROR_THROW(msg,*status);
       return *status;
    }
    if((*status = PILPutReal("xsigma",info->column[n].dx))) 
    {
       sprintf(msg,"Error writing the 'xsigma' parameter");
       HD_ERROR_THROW(msg,*status);
       return *status;
    }
    if((*status = PILPutReal("ysigma",info->column[n].dy))) 
    {
       sprintf(msg,"Error writing the 'ysigma' parameter");
       HD_ERROR_THROW(msg,*status);
       return *status;
    }

    if((*status = PILPutReal("modev",info->column[n].mode))) 
    {
        sprintf(msg,"Error writing the 'modev' parameter");
        HD_ERROR_THROW(msg,*status);
        return *status;
    }

    if((*status = PILPutInt("modes",info->column[n].modes))) 
    {
        sprintf(msg,"Error writing the 'modes' parameter");
        HD_ERROR_THROW(msg,*status);
        return *status;
    }

    if((*status = PILPutInt("modez",info->column[n].mode_depth))) 
    {
        sprintf(msg,"Error writing the 'modez' parameter");
        HD_ERROR_THROW(msg,*status);
        return *status;
    }

    return *status;
}

/*---------------------------------------------------------------------------*/
void ftstat_free(hduInfo * info)
{
    int i;
    for (i=0; i<info->ncols; i++)
    {
       if(info->column[i].nulflag) free(info->column[i].nulflag);
       if(info->column[i].dvalue) free(info->column[i].dvalue);
    }
    free(info->column);
}

/*--------------------------------------------------------------------------*/

/*
	Since the column data is sorted and nulls have been removed, the mode
	can be determined in a single pass over the array by counting how many
	times the same value occurs consecutively.

	ftstat_mode counts the consecutive occurrences and calls
	ftstat_mode_aux to update the mode statistics.

	In the event that there is more than one mode, the minimum is reported.
*/

static void ftstat_mode_aux(Column * column, int count, double value)
{
	if (column->mode_depth < count)
	{
		column->modes = 1;
		column->mode = value;
		column->mode_depth = count;
	}
	else if (column->mode_depth == count)
	{
		++column->modes;
	}
}

static void ftstat_mode(Column * column)
{
	int i;
	int count;
	double last_value;

	column->modes = 0;
	column->mode_depth = 0;
	column->mode = 0;

	if (column->good < 1)
		return;

	count = 1;
	last_value = column->dvalue[0];

	for (i = 1; i < column->good; ++i)
	{
		if (column->dvalue[i] == last_value)
			++count;
		else
		{
			ftstat_mode_aux(column, count, last_value);
			count = 1;
			last_value = column->dvalue[i];
		}
	}

	ftstat_mode_aux(column, count, last_value);
}

