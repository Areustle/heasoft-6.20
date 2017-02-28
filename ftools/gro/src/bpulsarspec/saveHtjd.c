/*       ----------------------------------------------
       filename: saveHtjd.c
       purpose:  save counts vs. TJD in an asiic data file
       author/date: C. Pan, May., 2002
       ---------------------------------------------- */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

void  saveHtjd(double **x, double **y, double **z, long *npt, 
      char *data_dir, char *infile, double *texpos, int *binsz,
      double *tjdsta, double *tjdstp)
{
  FILE  *fp;
  int   i, j;
  char fname[100];

  for (i=0;i<100;i++)fname[i]='\0';
  strcpy(fname, data_dir);
  strcat(fname,"/hardntjd.dat");
  remove(fname);
  if ((fp = fopen (fname,"w")) == NULL)
     {
      printf("cannot open output files\n");
      exit(0);
     }
    fprintf(fp, "%s = %f\n","Exposure time(sec)", *texpos);
    fprintf(fp, "%s %12.6f -- %12.6f \n","TJD range:",*tjdsta,*tjdstp);
    fprintf(fp, "%s = %d\n","TJD bin size", *binsz);
    fprintf(fp, "%s: %s\n", "input fits file", infile);  
    fprintf(fp,"%s       %s   %s\n", "TJD_start", "Hardness", "Stat_err");
    for (i=0;i<*npt; i++)
    fprintf(fp,"%12.6f %12.6f %12.6f\n", (*x)[i], (*y)[i], (*z)[i]);
    fclose(fp);

}
