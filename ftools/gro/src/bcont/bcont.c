/*  filename : bcont.c
    author   : Chunhui Pan
    purpose  : read in one or more (contiguous) BATSE daily data sets, 
               "CONT" or DISCLA" type data, plot 1-8 light curves,
                flux (or counts) vs time, on screen, and make interactive 
                time cuts from screen graphics for output to simple
                FITS file or ascii output. Also perform statistical
                test on light curve segments */ 

#include "bcont.h"
#define BPlot(snrows,sbinTime, Pcts,PctsErr,det,ndet) \
CCALLSFSUB6(BPLOT, bplot, LONG, DOUBLEV, DOUBLEVV,DOUBLEVV,INTV, PINT, snrows,sbinTime,Pcts,PctsErr,det,ndet)
void bcont()
{
    fitsfile *infptr;
    int      nfile, nchan, binsz,i,j;
    char     *listfn[10], data_dir[81], bcontfil[81], wtFlg[10],rcFlg[10];
    char     outFn[81], msg[100]="";
    int      chansta,chanstp,detsta, detstp;
    double   tjdFitsta, tjdFitstp,tjdsta, tjdstp;
    double   *midTime = NULL, *binTime = NULL,*sbinTime = NULL,**binCts=NULL;
    double   **ctsErr = NULL, **binCtsErr = NULL, **sbinCtsErr = NULL;
    long     **cts=NULL;
    long     nrows=0, ncol=0, binrows = 0,snrows = 0;
    double   **sbinCts = NULL, *sumCts=NULL, *sumCtsErr=NULL;
    float    *timeRes=NULL, *binTimeRes=NULL,*sTimeRes=NULL ;
    double   PctsErr[3000][8], Pcts[3000][8];
    double   *Rate=NULL, *RateErr=NULL;
    int      *det=NULL, ndet, status=0;
    
     /* get input file name */
        getBcontFn(listfn, &nfile, data_dir, &nchan, &ncol);
 
     /* get binsz */
        strcpy(msg,"binsz");
        Uclgsi(msg, &binsz, &status);
        if (status != 0 )
        {
         strcpy(msg,"Problem getting bin size!");
         Fcerr (msg);
         exit (1);
         }
      
     /* get channels */
        getChan(&chansta, &chanstp, &nchan);
       
     /* get fits data */
        getFitsData(listfn,&nrows,&nfile, &midTime, &cts,&ctsErr,&ncol,
                    &timeRes);
       
        tjdFitsta = midTime[0];
        tjdFitstp = midTime[nrows-1];
         
      /* get TJD */
	 getTjd(&tjdsta, &tjdstp, &tjdFitsta, &tjdFitstp);
        
      /* get binned data */
         getBinData(&nrows,&midTime,&cts,&ctsErr,&binrows,&binTime,
         &binCts,&binCtsErr,&binsz,&ncol,&timeRes,&binTimeRes); 
     
      /* get selected data */  
         getSelectData(&binrows,&tjdsta,&tjdstp,&binTime,&sbinTime, 
         &chansta,&chanstp,&binCts,&sbinCts,&snrows,&binCtsErr,&sbinCtsErr,
         &nchan, &ncol, &binTimeRes,&sTimeRes);
             
       /* overflow */
         if (snrows > 3000) 
	   {
	     printf("overflow!!!\n");
             exit(1);
	   }

  
    for (i=0;i<snrows;i++)
      {
       for (j=0;j<8;j++)
	 {
	 PctsErr[i][j]=sbinCtsErr[i][j]; 
         Pcts[i][j]= sbinCts[i][j];
	 }
       }
  

        det = (int *) malloc(8 * sizeof(int));
	BPlot(snrows,sbinTime,Pcts,PctsErr,det,ndet); 
      
        for (i=0;i<snrows;i++)
      {
       for (j=0;j<8;j++)
	 {
	 PctsErr[i][j]=0; 
         Pcts[i][j]=0;
	 }
       }
      
       /* get selected data */  
          getSumData(&snrows,&sbinTime,&sbinCts,&sbinCtsErr,
	    &sumCts, &sumCtsErr,&ndet,&det); 

       /* free memory */
          free(sbinCts);
          free(sbinCtsErr);
          free(binTime);
          free(binCts);
          free(binCtsErr);
   
       /* get rates */ 
         getRates(&snrows,&sumCts, &sumCtsErr,&Rate, &RateErr,&sTimeRes); 
       
       /* write fits or ascii file */
          getwtFlg(wtFlg, rcFlg);
 
       /* get out file name */
          getFname(outFn, data_dir); 
        
          if (strncmp(wtFlg,"A",1) == 0 ||
             strncmp(wtFlg,"a",1) == 0)
	    {
	      if (strncmp(rcFlg,"c",1) == 0 ||
                 strncmp(rcFlg,"C",1) == 0)
              writeAsiic(&snrows,&sbinTime,&sumCts, &sumCtsErr,
                    data_dir,&det,&ndet,&timeRes,&binsz, rcFlg, outFn);
              else
              writeAsiic(&snrows,&sbinTime,&Rate, &RateErr,
                    data_dir,&det,&ndet,&timeRes,&binsz, rcFlg, outFn);
	    }

          if (strncmp(wtFlg,"F",1) == 0 ||
             strncmp(wtFlg,"f",1) == 0)
	    {
             if (strncmp(rcFlg,"c",1) == 0 ||
                 strncmp(rcFlg,"C",1) == 0)
                writeFits(&snrows,sbinTime,sumCts, sumCtsErr,
                data_dir,det,&ndet,timeRes,rcFlg,listfn,&nfile,outFn,
                &chansta,&chanstp,&binsz);
             else
                writeFits(&snrows,sbinTime,Rate, RateErr,
                data_dir,det,&ndet,timeRes,rcFlg,listfn,&nfile,outFn, 
                &chansta,&chanstp,&binsz);
	    }   
}

