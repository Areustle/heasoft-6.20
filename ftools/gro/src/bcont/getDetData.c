
void getDetData(long *snrows,long ***sbinCts,double ***sbinCtsErr,
                long **Cts1, double **CtsErr1, long **Cts2, double **CtsErr2,
                long **Cts3, double **CtsErr3, long **Cts4, double **CtsErr4,
                long **Cts5, double **CtsErr5,long **Cts6, double **CtsErr6,
                long **Cts7, double **CtsErr7,long **Cts8, double **CtsErr8) 
{ 
  int i;

    *Cts1 = (long *) malloc((long)(*snrows) * sizeof(long));
    *CtsErr1 = (double *) malloc((long)(*snrows) * sizeof(double));
    *Cts2 = (long *) malloc((long)(*snrows) * sizeof(long));
    *CtsErr2 = (double *) malloc((long)(*snrows) * sizeof(double));
    *Cts3 = (long *) malloc((long)(*snrows) * sizeof(long));
    *CtsErr3 = (double *) malloc((long)(*snrows) * sizeof(double));
    *Cts4 = (long *) malloc((long)(*snrows) * sizeof(long));
    *CtsErr4 = (double *) malloc((long)(*snrows) * sizeof(double));
    *Cts5 = (long *) malloc((long)(*snrows) * sizeof(long));
    *CtsErr5 = (double *) malloc((long)(*snrows) * sizeof(double));
    *Cts6 = (long *) malloc((long)(*snrows) * sizeof(long));
    *CtsErr6 = (double *) malloc((long)(*snrows) * sizeof(double));
    *Cts7 = (long *) malloc((long)(*snrows) * sizeof(long));
    *CtsErr7 = (double *) malloc((long)(*snrows) * sizeof(double));
    *Cts8 = (long *) malloc((long)(*snrows) * sizeof(long));
    *CtsErr8 = (double *) malloc((long)(*snrows) * sizeof(double));

         for (i=0;i<*snrows;i++)
	   {
	    (*Cts1)[i] = (*sbinCts)[i][0];
            (*CtsErr1)[i] = (*sbinCtsErr)[i][0];
            (*Cts2)[i] = (*sbinCts)[i][1];
            (*CtsErr2)[i] = (*sbinCtsErr)[i][1];
            (*Cts3)[i] = (*sbinCts)[i][2];
            (*CtsErr3)[i] = (*sbinCtsErr)[i][2];
            (*Cts4)[i] = (*sbinCts)[i][3];
            (*CtsErr4)[i] = (*sbinCtsErr)[i][3];
            (*Cts5)[i] = (*sbinCts)[i][4];
            (*CtsErr5)[i] = (*sbinCtsErr)[i][4];
            (*Cts6)[i] = (*sbinCts)[i][5];
            (*CtsErr6)[i] = (*sbinCtsErr)[i][5];
            (*Cts7)[i] = (*sbinCts)[i][6];
            (*CtsErr7)[i] = (*sbinCtsErr)[i][6];
            (*Cts8)[i] = (*sbinCts)[i][7];
            (*CtsErr8)[i] = (*sbinCtsErr)[i][7]; 
	   }
}
