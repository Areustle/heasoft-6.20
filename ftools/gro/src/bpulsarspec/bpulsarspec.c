/*  filename : bpulsarspec.c
    author   : Chunhui Pan
    purpose  : read in the data of pulsar-rotation phase as a function of
    detector energy channel as a functio of time for a given pulsar 
               from a lad_pll.fits file and display it  */
  
#include "bpulsarspec.h"

void bpulsarspec()
{
    fitsfile *infptr;
    int    hdunum, hdutype,ii=0,jj=0,j = 0,anynull = 0,intnull = 0,i=0,irow=0;
    long   longnull, bnrows=0, nrows = 0, snrows=0, nPhas=0;
    double  *bstartTime = NULL, *startTime = NULL, tjdFitsta = 0;
    double  *bstopTime = NULL, *stopTime = NULL, tjdFitstp = 0, texpos = 0.;
    double  tjdsta = 0, tjdstp = 0, tstart=0, tstop=0;
    double  *stime= NULL, *sumCts = NULL, *sdt=NULL,*sumRate=NULL;
    double   **CtsErr=NULL,**binCtsErr=NULL, **sbinCtsErr=NULL,*hardnErr=NULL;
    double  **sbinCtsErrPhas=NULL,*hardnErrPhas= NULL,*sumCtsErr=NULL;
    double  *sumRateErr=NULL,**binPhasErr=NULL,**binPhas=NULL;
    double  fltnull = 0, sum = 0, **sbinCts=NULL,*hardn= NULL;
    double  **sbinCtPhas=NULL,*hardnPhas= NULL,*fluxphs=NULL,*fluxphsErr=NULL;
    int    status = 0,**Cts=NULL,**binCts=NULL,chan[16], decnum=0,nphs=0;
    int    chani,chanj,chanm,chann,chansta,chanstp,*sphas =NULL,binsz=0;
    int    BufLen_2 = 80, slen = 0,fitsWtFlg = 0, phsta=0, phstp=0;
    char   infname[81],data_dir[81], bpulsfn[81];
    char   dataFname[40]="",fitsFname[40]=""; 
    char   fitsf[81],msg[50],asciiwt[81],fitswt[81];

     /* get input file name */
        getpulsfn(infname, data_dir, bpulsfn);
      
     /* get data from the file */
        getFitsData(infname,&nrows,&startTime,&stopTime,&Cts,&CtsErr); 

     /* get bined data */
        getBinData(&nrows,&startTime,&stopTime,&Cts,&CtsErr,
	  &bnrows,&bstartTime,&bstopTime,&binCts,&binCtsErr,&binsz); 

     /* find TJD time span of the data in the current Fits file*/ 
        tjdFitsta = startTime[0];
        tjdFitstp = stopTime[nrows-1];
    
     /* get selected TJD time */
        getTjd(&tjdsta, &tjdstp,&tjdFitsta,&tjdFitstp);  
       
     /* get exposure time */  
        getExpos(&nrows,&tjdsta,&tjdstp,&startTime,&stopTime,&texpos);
    
     /* get selected phase */
        getPhase(&phsta, &phstp);
      
     /* get selected channel */
        getChan(&chani, &chanj, &chanm, &chann, &chansta, &chanstp);
       
     /* get the selected data as function of TJD */
         getSelectData( &bnrows,&tjdsta,&tjdstp,&bstartTime,&bstopTime,
         &phsta,&phstp,&binCts,&stime,&sbinCts,&snrows,&binCtsErr,
         &sbinCtsErr, &sdt); 

     /* get the selected flux as function of 64 phases */
        getPhasData(&bnrows,&tjdsta,&tjdstp,&bstartTime,&bstopTime, 
	&phsta,&phstp,&binCts,&binPhas,&binCtsErr,&binPhasErr);

     /* get the selected counts as function of selected phases */
        getSelectPhas(&bnrows,&tjdsta,&tjdstp,&bstartTime,&bstopTime, 
        &phsta,&phstp,&binCts,&sbinCtPhas,&nPhas,&binCtsErr,&sbinCtsErrPhas);
 
      /* get summed counts and error for each channel */
         getSumData(&snrows,&sbinCts,&sbinCtsErr,&sumCts,&sumCtsErr,
         &sdt, &sumRate, &sumRateErr);
         
      /* get flux */
         getFlux(&binPhas,&binPhasErr,&fluxphs,
                 &fluxphsErr, &chansta, &chanstp, &nphs);

      /* get fits file name */
         getFname(fitsFname, data_dir);
 
      /* write counts/rates in fits file */
         strcpy(msg, "fitswt");
         Uclgst(msg,fitswt,&status);
         if (status != 0)
           {
            printf ("Problem getting fitswt!\n");
            exit (1);
           }
         if (strncmp(fitswt,"c",1) == 0 || 
             strncmp(fitswt,"C",1) == 0)
            writefits(infname,bpulsfn,fitsFname,&snrows,sumCts,sumCtsErr,
            &texpos,&decnum); 
         else
            writeRates(infname,bpulsfn,fitsFname,&snrows,sumRate,sumRateErr,
            &decnum, &texpos); 
      
      /* get hardness as function of TJD */
         getHardn(&snrows,&sbinCts,&hardn,&sbinCtsErr,&hardnErr,
                  &chani, &chanj,&chanm,&chann); 
     
      /* get hardness as function of phase */
         getHardnPhas(&nPhas,&sbinCtPhas,&hardnPhas, 
         &sbinCtsErrPhas,&hardnErrPhas,
         &chani, &chanj,&chanm,&chann); 

      /* save hardness as function of tjd and phase */ 
         strcpy(msg, "asciiwt");
         Uclgst(msg, asciiwt, &status);
         if (status != 0)
           {
            printf ("Problem getting asciiwt!\n");
            exit (1);
           }
         if (strncmp(asciiwt,"y",1) == 0 || 
             strncmp(asciiwt,"Y",1) == 0)
	   {
            tstart = stime[0];
            tstop  = stime[snrows-1];
            saveHtjd(&stime,&hardn,&hardnErr,&snrows,data_dir,bpulsfn,
                     &texpos,&binsz, &tjdsta, &tjdstp);
            sphas = (int *) malloc((int)(nPhas) * sizeof(int));
            for (i=phsta; i<phstp+1; i++) sphas[i-phsta] = i;
            saveHphs(&sphas,&hardnPhas,&hardnErrPhas,&nPhas,data_dir,
                     bpulsfn,&texpos, &binsz,&tjdsta,&tjdstp);
	   }

      /* display hardness vs. TJD */
         pulsplot_(&snrows,stime,hardn,hardnErr,&nPhas,hardnPhas,
         hardnErrPhas,&phsta,&phstp,&decnum,&chani,&chanj,&chanm,&chann,
         &tjdsta, &tjdstp,fluxphs,fluxphsErr,&chansta,&chanstp,&nphs);  
  
}

