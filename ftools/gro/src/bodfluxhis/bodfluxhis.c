/*  filename : bodfluxhis.c
    author   : Chunhui Pan
    purpose  : read in the #VP and the corresponding light curve data 
               from nhis FITs file and display it  */
  
#include <bodfluxhis.h>

void bodfluxhis()
{
    FILE  *fp;
    int    hdunum, hdutype,ii,j = 0,anynull = 0,binsz = 0, nbin = 0, nbin1 = 0;
    long   longnull, firstrow, noutrows, lastrow, nrows = 0;
    float  *startTime = NULL, tjdFitsta = 0, *bstartTime = NULL, tjdsta = 0;
    float  *stopTime  = NULL, tjdFitstp = 0, *bstopTime = NULL, tjdstp = 0;
    float  tjdsta1 = 0, tjdstp1= 0, tjdsta2 = 0, tjdstp2 = 0;
    float  *Flux = NULL, *FluxErr = NULL, *bFlux = NULL, *bFluxErr = NULL;
    float  *FluxFits = NULL, *FluxErrFits = NULL, *startTimeFits = NULL;
    float  fltnull = 0, *bFluxFits = NULL, *bFluxErrFits = NULL; 
    float  *bstartTimeFits = NULL,*tjde= NULL, *tjdb= NULL;
    int    intnull = 0, status = 0, nvp=0;
    int    BufLen_2 = 80, slen = 0, nfile, fitsWtFlg = 0;
    char   *listfn[10],data_dir[81], outfil_dir[21];
    char   dataFname[40]="",fitsFname[40]=""; 
    char   fitsf[81],msg[100]="";

     /*get input file list */
      getlist(listfn, &nfile, data_dir);
 
     /* get data from list */
      getFitsData(listfn,&nrows,&nfile,&startTime, &stopTime, &FluxFits,
       &FluxErrFits,&startTimeFits);
     
     /* get bin size */
      strcpy(msg,"binsz");
      Uclgsi(msg, &binsz, &status);
     if (status != 0 )
	{
         strcpy(msg,"Problem getting bin size!");
         Fcerr (msg);
         exit (1);
	 }
     
     /* find TJD time span of the data in the current Fits file*/ 
      tjdFitsta = startTime[0];
      tjdFitstp = stopTime[nrows-1];
  
     /* get the selected TJD time */
      status = getTjd(&tjdsta, &tjdstp,&tjdFitsta,&tjdFitstp, &tjdsta1, 
               &tjdstp1, &tjdsta2, &tjdstp2, &tjdb, &tjde, &nvp, data_dir);
       if (status != 0 )
	  {
           strcpy(msg,"Problem getting selected TJD!");
           Fcerr (msg);
           exit (1);
	  } 

     /* get the selected number of #row*/
      getnrows(&nrows,startTime, stopTime, &noutrows,&tjdsta,&tjdstp, 
             &firstrow, &lastrow);
     
     /* get the selected light curve */
      getSelectData( &noutrows, &firstrow, &lastrow,&startTime,&Flux,&FluxErr,
                   & FluxFits, &FluxErrFits, &startTimeFits);
     
     /* check bin size */
      checkBinsz(&nbin, &noutrows, &binsz);
   
     /* get bined data for the selected data */
      getBindata(&binsz,&noutrows,&bstartTime,&bFlux,&bFluxErr,startTime,
      Flux,FluxErr);
     
      /* get bined data for the Fits file*/
       getBindata(&binsz,&nrows,&bstartTimeFits,&bFluxFits,&bFluxErrFits,
       startTimeFits,FluxFits,FluxErrFits);
   
       status=0;
       /* get name of log director */
       strcpy(msg,"fitsf");
       Uclgst(msg, fitsf, &status);
       if (status != 0)
         {
           strcpy(msg,"Problem getting fitsf!");
           Fcerr (msg);
           exit (1);
          }

       nbin1 = (nrows)/(binsz);
       if (strncmp(fitsf,"yes",3) == 0)
	 {
           /* get asiic data file name */     
            getFname(&tjdsta, &tjdstp, fitsFname, data_dir);
     
           /* get fitsWtFlg */
            strcpy(msg,"fitsWtFlg");
            Uclgsi(msg, &fitsWtFlg, &status);
            if (status != 0 )
	      {
               strcpy(msg,"Problem getting fitsWtFlg!");
               Fcerr (msg);
               exit (1);
	      }

          /* write in fits binary file */
           if (fitsWtFlg == 0)
              writefits(&nbin, bstartTime,bFlux, bFluxErr, 
              fitsFname,&binsz);
           else
	    {
             writefits(&nbin1, bstartTimeFits, bFluxFits, 
             bFluxErrFits, fitsFname,&binsz);
	     }
	 }

      /* Write into a log file */  
        writeLog(fitsFname, outfil_dir);
     
      /* Call pgplot subroutine */
        bodfluxhis_plot_(&nbin1,bstartTimeFits,bFluxFits,bFluxErrFits,&nbin,
        bstartTime, bFlux, bFluxErr, &tjdsta1, &tjdstp1, &tjdsta2, &tjdstp2,
        tjdb, tjdb, &nvp);

      
}

