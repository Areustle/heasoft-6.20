#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_error.h"
#define TOOLSUB getocctimes
#include "headas_main.c"

int getocctimes(void);
int quadintzero(long double *x, long double *y, long n, const double x0, double *xzero1, double *xzero2);
long orbocctimes(double sourcera, double sourcedec, char *orbfilename, double atmthickness, double eartheqrad, double flattening, double *occtimes);
int compare(const void *a, const void *b);
int uniq(double *a, long *n);

/*
  Handle input parameters for and collect occultation times 
  calculated from orbocctimes(...).  Sort the occultation 
  times, remove duplicates, and write the times to a file 
  or to STDOUT.
 */ 
int getocctimes(void)
{
  int pilstatus = 0;
  double sourcera = 0.; /* source RA in decimal degrees */
  double sourcedec = 0.; /* source declination in decimal degrees */
  char orbfilename[PIL_LINESIZE]; /* orbit filename or filenames list read from PIL */
  char origoutfilename[PIL_LINESIZE]; /* output filename read from PIL */
  char outfilename[PIL_LINESIZE]; /* output filename for writing */
  int overwrite = 0; /* write to file when ==1 */
  double atmthickness = 0.; /* altitude above Earth surface at which occultations should be calculated */
  double eartheqrad = 6378.136; /* equatorial Earth radius (km) */
  double flattening = 1./298.257; /* R_polar = (1-flattening) * R_equator */
  FILE* outfile;

  const int MAX_FILES = 10000;
  const int MAX_FILENAME_LENGTH = 256;

  char orbfilelist[PIL_LINESIZE]; /* filename of list of orbit filenames */
  char orbfilenames[MAX_FILES][MAX_FILENAME_LENGTH]; /* list of orbit filenames to process */

  int i = 0;
  long j = 0;
  long n_files = 0;
  long n_occtimes = 0;
  double *occtimes = (double*) calloc(1, sizeof(int));
  double *moreocctimes;
  long n_newocctimes = 0;
  double newocctimes[5000];

  /* Set tool properties */
  set_toolname("getocctimes");
  set_toolversion("0.1");

  /* Get necessary parameters if not in command line */
  pilstatus = PILGetReal("ra", &sourcera);
  if(pilstatus) 
    {
      HD_ERROR_THROW("Error reading ra parameter.\n", pilstatus);
      return pilstatus;
    }
  
  pilstatus = PILGetReal("dec", &sourcedec);
  if(pilstatus) 
    {
      HD_ERROR_THROW("Error reading dec parameter.\n", pilstatus);
      return pilstatus;
    }
  
  pilstatus = PILGetString("orbitfile", orbfilename);
  if(pilstatus) 
    {
      HD_ERROR_THROW("Error reading orbitfile parameter.\n", pilstatus);
      return pilstatus;
    }
  
  pilstatus = PILGetReal("atmthickness", &atmthickness);
  if(pilstatus) 
    {
      HD_ERROR_THROW("Error reading atmthickness parameter.\n", pilstatus);
      return pilstatus;
    }

  pilstatus = PILGetString("outfile", origoutfilename);
  if(pilstatus) 
    {
      HD_ERROR_THROW("Error reading outfile parameter.\n", pilstatus);
      return pilstatus;
    }

  pilstatus = PILGetReal("earthrad", &eartheqrad);
  if(pilstatus) 
    {
      HD_ERROR_THROW("Error reading earthrad parameter.\n", pilstatus);
      return pilstatus;
    }

  pilstatus = PILGetReal("flattening", &flattening);
  if(pilstatus) 
    {
      HD_ERROR_THROW("Error reading flattening parameter.\n", pilstatus);
      return pilstatus;
    }

  /* Check for outfile existence */
  strcpy(outfilename, origoutfilename);
  if(origoutfilename[0] == '!')
    {
      overwrite = 1;
      strncpy(outfilename, origoutfilename+1, strlen(origoutfilename)-1);
      outfilename[strlen(origoutfilename)-1] = '\0';
    }

  outfile = fopen(outfilename, "r");
  if(!outfile)
    {
      overwrite = 1;
    }
  else
    {
      fclose(outfile);
      if(overwrite != 1)
	printf("Outfile %s already exists. Prepend '!' to overwrite it.\n", outfilename);
    }


  /* Create array of filenames or load list of filenames */

  if(orbfilename[0] == '@') /* Load list of filenames */
    {
      FILE *f_orblist;
      char line[MAX_FILENAME_LENGTH];
      strncpy(orbfilelist, orbfilename+1, strlen(orbfilename)-1);
      orbfilelist[strlen(orbfilename)-1] = '\0';
      /*printf("%s\n",orbfilelist);*/

      f_orblist = fopen(orbfilelist, "r");
      if(!f_orblist)
	{
	  fprintf(stderr, "File with list of orbit files %s not found. Quitting.\n", orbfilelist);
	  return 1;
	}

      n_files = 0;
      while(!feof(f_orblist) && n_files < MAX_FILES)
	{
	  fgets(line, MAX_FILENAME_LENGTH, f_orblist);
	  strncpy(orbfilenames[n_files], line, strlen(line)-1); 
	  n_files++;
	}
      

    }
  else /* Parse list of filenames from prompt or command line */
    {
      char *ptr;
      n_files = 0;
      ptr = strtok(orbfilename, ",");
      while(ptr != NULL)
	{
	  strcpy(orbfilenames[n_files], ptr);
	  n_files++;
	  ptr = strtok(NULL, ",");
	  
	}
    }

  /* Loop through each orbit file in input order */
  for(i = 0; i < n_files; i++)
    {
      printf("Working on orbit file %s ...\n", orbfilenames[i]);
      n_newocctimes = orbocctimes(sourcera, sourcedec, orbfilenames[i], atmthickness, eartheqrad, flattening, newocctimes);
      if(n_newocctimes > 0)
	{
	  moreocctimes = (double*) realloc(occtimes, (n_occtimes + n_newocctimes)*sizeof(double));
	  if(moreocctimes == NULL)
	    {
	      printf("Out of memory while processing %s. Quitting.\n", orbfilenames[i]);
	    }
	  occtimes = moreocctimes;
	  for(j = 0; j < n_newocctimes; j++)
	    {
	      occtimes[j + n_occtimes] = newocctimes[j];
	      /*printf("%ld %ld %f %f\n", j, j+n_occtimes, occtimes[j+n_occtimes], newocctimes[j]);*/
	    }
	  n_occtimes += n_newocctimes;
	}
      /*for(j = 0; j < n_occtimes; j++)
	{
	  ;//if(i % 100 == 0 || i == n_files-1)
	  //printf("%ld %ld  %s %f\n", j, compare(n_occtimes-2,j), orbfilenames[i], occtimes[j]);
	  }*/
    }

  if(n_occtimes <= 0)
    {
      printf("No occultations are expected during the timeframe of the orbitfile(s).\n");
      return 0;
    }

  if(overwrite == 1)
    {
      outfile = fopen(outfilename, "w");
      if(!outfile)
	{
	  printf("Cannot write to file %s. Quitting.\n", outfilename);
	}
    }

  /* sort list of occultation times and remove duplicates */
  qsort(occtimes, n_occtimes, sizeof(double), compare);
  uniq(occtimes, &n_occtimes);

  /* output the occultation time list */
  for(j = 0; j < n_occtimes; j++)
    {

      if(overwrite == 1)
	{
	  fprintf(outfile, "%.2f\n", occtimes[j]);
	}
      else
	{
	  printf("%.2f\n", occtimes[j]);	  
	}
    }

  if(overwrite == 1)
    {
      fclose(outfile);
      printf("Occultation times written to file %s.\n", outfilename);
    }

  free(occtimes);
  return 0;
}

/* Read a Suzaku orbit file and calculate occultation times 
   for a given (right ascension, declination) source
   during the observation.  
*/
long orbocctimes(double sourcera, double sourcedec, char *orbfilename, double atmthickness, double eartheqrad, double flattening, double *occtimes)
{

  const double PI = M_PI;
  double sourcex; /* position of source in rect. coords. with unit length */
  double sourcey;
  double sourcez;
  int status = 0;
  int anynul = 1;
  double nulval = -99999;
  fitsfile *orbfile;
  long n_orbrows;

  long double *orbtime;
  double *orbx;
  double *orby;
  double *orbz;

  double *orbtime0;
  double *orbwt0;
  double *orbx0;
  double *orby0;
  double *orbz0;
  long double *anglediff;

  /*FILE *outfile;*/

  long i = 0;
  long k = 0;

  long n_occtimes = 0;
  
  double a;
  double b;
  double occtime;
  double t1,t2;
  int q;



  /* Convert source direction into rectangular coordinates */
  sourcex = cos(sourcedec*PI/180) * cos(sourcera*PI/180); /* rectangular unit vector of source direction */
  sourcey = cos(sourcedec*PI/180) * sin(sourcera*PI/180);
  sourcez = sin(sourcedec*PI/180);

  /* printf("Orbit filename: %s\n", orbfilename);*/


  /* Open orbit file and load table */
  fits_open_file(&orbfile, orbfilename, READONLY, &status);
  if(status > 0)
    {
      fits_report_error(stderr, status);
      return status;
    }
  
  /*printf("status %d\n", status);*/
  fits_movnam_hdu(orbfile, BINARY_TBL, "PAR_ORBIT", 1, &status);
  /*printf("status %d\n", status);*/
  fits_get_num_rows(orbfile, &n_orbrows, &status);
  if(status > 0)
    {
      fits_report_error(stderr, status);
      return status;
    }
  /*printf("n_orbrows: %d\n", n_orbrows);*/

  orbtime = malloc(n_orbrows*sizeof(long double));
  orbx = malloc(n_orbrows*sizeof(double));
  orby = malloc(n_orbrows*sizeof(double));
  orbz = malloc(n_orbrows*sizeof(double));

  orbtime0 = malloc(n_orbrows*sizeof(double));
  orbwt0 = malloc(n_orbrows*sizeof(double));
  orbx0 = malloc(n_orbrows*sizeof(double));
  orby0 = malloc(n_orbrows*sizeof(double));
  orbz0 = malloc(n_orbrows*sizeof(double));
  anglediff = malloc(n_orbrows*sizeof(long double)-1);

  if(anglediff == NULL)
    {
      printf("Out of memory before orbit file could be read.\n");
    }
  fits_read_col(orbfile, TDOUBLE, 1, 1, 1, n_orbrows, &nulval, orbtime0, &anynul, &status);
  fits_read_col(orbfile, TDOUBLE, 6, 1, 1, n_orbrows, &nulval, orbx0, &anynul, &status);
  fits_read_col(orbfile, TDOUBLE, 7, 1, 1, n_orbrows, &nulval, orby0, &anynul, &status);
  fits_read_col(orbfile, TDOUBLE, 8, 1, 1, n_orbrows, &nulval, orbz0, &anynul, &status);
  fits_read_col(orbfile, TDOUBLE, 5, 1, 1, n_orbrows, &nulval, orbwt0, &anynul, &status);
  fits_close_file(orbfile, &status);
  

  /* Remake the orbit data arrays to remove the repetitious orbit times */
  k = 0;
  for(i = 0; i < n_orbrows; i++)
    {
      if(orbwt0[i] > 0.5)
	{
	  orbtime[k] = orbtime0[i];
	  orbx[k] = orbx0[i];
	  orby[k] = orby0[i];
	  orbz[k] = orbz0[i];
	  k++;
	}
    }
  n_orbrows = k;
  free(orbtime0);
  free(orbwt0);
  free(orbx0);
  free(orby0);
  free(orbz0);


  /* Calculate angles between source-satellite-Earth center and Earth surface-satellite-Earth center-Earth */
  /* outfile = fopen("junk.txt", "w");*/

  for(i = 0; i < n_orbrows; i++)
    {
      double orbrad = sqrt( orbx[i]*orbx[i] + orby[i]*orby[i] + orbz[i]*orbz[i]/((1 - flattening) * (1 - flattening)) );
      double satsourceangle = acos((-orbx[i]*sourcex - orby[i]*sourcey - orbz[i]*sourcez/(1 - flattening))/orbrad) * 180 / PI;
      double satearthangle = asin((eartheqrad+atmthickness)/orbrad) * 180/PI;
      anglediff[i] = satsourceangle - satearthangle;


      /* printf("%d %f %f: %f %f %f | %f %f %f \n", i, orbtime[i], orbx[i], orby[i], orbz[i], orbrad, orbrad2, satsourceangle, satearthangle);*/
      /*fprintf(outfile, "%f  %f %f %f %f\n", orbtime[i]-orbtime[0],  orbx[i], orbrad, satsourceangle, satearthangle);*/

      
    }
  /*fclose(outfile);*/


  /* linear and quadratic interpolation of where the satellite-source and satellite-earth angles coincide */
  for(i = 0; i < n_orbrows-1; i++)
    {
      /*printf("%d %f %f   %f %f \n", n_occtimes, orbtime[i]-orbtime[0], orbtime[i+1]-orbtime[0], anglediff[i], anglediff[i+1]);*/
      if(anglediff[i] == 0)
	{
	  occtimes[n_occtimes] = anglediff[i];
	  n_occtimes++;
	}
      else if(anglediff[i+1] * anglediff[i] < 0)
	{
	  a = (anglediff[i+1]-anglediff[i])/(orbtime[i+1]-orbtime[i]);
	  b = anglediff[i] - a * orbtime[i];
	  occtime = -b/a;
	  q = quadintzero(orbtime, anglediff, n_orbrows, orbtime[i], &t1, &t2);

	  if(q > 0)
	    {
	      occtime = t1;
	      occtimes[n_occtimes] = occtime;
	      n_occtimes++;

	    }
	  /* printf("%d %f %f %f\n", q, occtime, -b/a, occtime+b/a);*/
	  /*if(i<= 100) printf("%f\n", occtime);*/
	}
      
    }
  
  
  
  /*printf("%ld occultation times found.\n", n_occtimes);*/
  
  free(orbtime);
  free(orbx);
  free(orby);
  free(orbz);

  /* give the resulting occultation times back to the 
     driver getocctimes() function */

  return n_occtimes;
}


/* Fit a local quadratic function to y(x) near x = x0.  
   Returns -1 if x0 is out of range of array x.
   Returns 0, 1, or 2 as the number of found roots.
   If one or two roots are found, they are stored in xzero1 and xzero2. */
int quadintzero(long double *x, long double *y, long n, const double x0, double *xzero1, double *xzero2)
  {
  long i;
  long i0 = -1;
  long double a, b, c; /* quadratic coefficients ax^2 + bx + c */
  long double radical;
  int n_solutions = 0;
  *xzero1 = 0; /* possible zeroes will be stored here */
  *xzero2 = 0;
  
  /* Proceed only for interpolation, not extrapolation. */
  if(x0 <= x[0])
    return -1;
  if(x0 >= x[n-1])
    return -1;


  /* Locate position of x0 in x array */
  for(i = 0; i < n - 1; i++)
    {
      if(x[i] <= x0 && x0 <= x[i+1])
	i0 = i-1;
    }

  /* Select trio of points in x,y arrays to use for quadratic interpolation. */
  if(i0 == -1)
    return -1;
  if(i0 >= n-3)
    i0 = n-3;
  if(i0 <= 0)
    i0 = 0;
  if(x[i0] == x[i0+1] || x[i0+1] == x[i0+2])
    return 0;

  /* printf("found: %f %f %f %f\n", x[0], x[n-1], x0, x[i0]);*/


  /* Calculate quadratic coefficients a,b,c with ax^2 + bx + c as the fit. */
  a = y[i0]/((x[i0]-x[i0+1])*(x[i0]-x[i0+2]))
           + y[i0+1]/((x[i0+1]-x[i0])*(x[i0+1]-x[i0+2]))
           + y[i0+2]/((x[i0+2]-x[i0])*(x[i0+2]-x[i0+1]));
  b = -((x[i0+1]+x[i0+2])*y[i0]/((x[i0]-x[i0+1])*(x[i0]-x[i0+2]))
	       + (x[i0]+x[i0+2])*y[i0+1]/((x[i0+1]-x[i0])*(x[i0+1]-x[i0+2]))
	       + (x[i0]+x[i0+1])*y[i0+2]/((x[i0+2]-x[i0])*(x[i0+2]-x[i0+1]))
	      );
  c = x[i0+1]*x[i0+2]*y[i0]/((x[i0]-x[i0+1])*(x[i0]-x[i0+2]))
           + x[i0]*x[i0+2]*y[i0+1]/((x[i0+1]-x[i0])*(x[i0+1]-x[i0+2]))
           + x[i0]*x[i0+1]*y[i0+2]/((x[i0+2]-x[i0])*(x[i0+2]-x[i0+1]));
 
  /*printf("check: %f %f\n", c + x[i0]*(b + x[i0]*a), y[i0]);*/
  /*printf("check: %f %f\n", c + x[i0+1]*(b + x[i0+1]*a), y[i0+1]); */
  /*printf("check: %f %f\n", c + x[i0+2]*(b + x[i0+2]*a), y[i0+2]); */

  radical = b*b - 4 * a * c;

  /*  printf( "coeff: %f %f %f   %f %f %f    %f\n", y[i0], y[i0+1], y[i0+2], a, b, c, radical); */



  /* Analyze the solutions to ax^2 + bx + c = 0 and 
     return the number of found roots in range of the x array. 
     Put solutions in xzero1 and xzero2 to be returned by reference. */

  if(radical == 0)
    {
      n_solutions = 1;
      *xzero1 = -b/(2 * a);
      *xzero2 = -b/(2 * a);
      if(*xzero1 < x[i0] || *xzero1 > x[i0+2])
	n_solutions = 0;
      return n_solutions;
    }
  if(radical > 0)
    {
      n_solutions = 2;
      *xzero1 = (-b - sqrt(radical))/(2 * a);
      *xzero2 = (-b + sqrt(radical))/(2 * a);
      if(*xzero1 < x[i0] || *xzero1 > x[i0+2])
	{
	  n_solutions--;
	  *xzero1 = *xzero2;
	}
      if(*xzero1 < x[i0] || *xzero1 > x[i0+2])
	n_solutions--;

      if(n_solutions == 2)
	{
	  if(fabs(*xzero1-x0) > fabs(*xzero2-x0))
	    {
	      double temp = *xzero1;
	      *xzero1 = *xzero2;
	      *xzero2 = temp;
	    }
	}

      return n_solutions;
    }
  
  return 0;
}

/*
  Compare two doubles and return 1 (a > b), 0 (a == b), or -1 (a < b)
  for use in qsort()
 */
int compare(const void* a, const void* b)
{
  const double *aa = (const double*) a;
  const double *bb = (const double*) b;
  return (*aa > *bb) - (*aa < *bb);
}

/* 
   Remove duplicates from array a of length n and return a 
   shortened array and array length in their places
 */

int uniq(double *a, long *n)
{
  long i = 0;
  long k = 0;
  
  for(i = 1; i < *n; i++)
    {
      /*      printf("%ld %ld %f %f\n", k, i, a[k], a[i]);*/
      if(a[i] > a[k])
	{
	  k++;
	  a[k] = a[i];
	}
    }
  *n = k+1;
  return 0;
}
