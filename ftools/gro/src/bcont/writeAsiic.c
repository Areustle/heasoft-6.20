/*       ----------------------------------------------
       filename: writeAsiic.c
       purpose:  save counts/rates vs. tjd in an ascii file
       author/date: C. Pan, May., 2002
       ---------------------------------------------- */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

void writeAsiic(long *npt, double **x, double **y, double **z, char *data_dir, 
      int **det, int *ndet, float **timeRes, int *binsz, char *flg,
      char *outFn)
{
  FILE  *fp;
  int   i, j;
 
  remove(outFn);
  if ((fp = fopen (outFn,"w")) == NULL)
     {
      printf("cannot open output file %s!\n",outFn);
      exit(0);
     }
    
    fprintf(fp, "%s = %f\n","TimeRes(sec)", (*timeRes)[0]);
    fprintf(fp, "%s","Det:");
    
    for (i=0;i<*ndet;i++)fprintf(fp, "%d", (*det)[i]);
    fprintf(fp, "\n");
 
    fprintf(fp, "%s = %d\n","Bin size", *binsz); 
    if (strncmp(flg,"R",1) == 0 ||
             strncmp(flg,"r",1) == 0)
    fprintf(fp,"%s   %s   %s\n", "       Midtime","   Rates","   Stat_err");
    else
    fprintf(fp,"%s   %s   %s\n", "       Midtime","   Counts","   Stat_err"); 
     
    for (i=0;i<*npt; i++)
      {
       fprintf(fp,"%16.6f %16.6f %16.6f\n", (*x)[i], (*y)[i], (*z)[i]);
      }
    
    fclose(fp);

}
