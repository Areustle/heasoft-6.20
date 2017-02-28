/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/oldaspect/oldaspect.c,v 1.5 2002/04/05 15:55:03 irby Exp $   */
/*                   */
/*****************************************************************************

  Filename:    oldaspect.c

  Purpose:     Reads euler angles from the ATTITUDE ftool's output file
	       and recomputes them for the best aspect point.

  Written by Edward A. Pier 12/95, Converted to an FTOOL by Srilal Weera 

  Date:        Apr 1996

  Version: 1.0      
***********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cfitsio.h"
#include "xpi.h"


#define YES 1
#define NO 0
#define WRAPTEST 5.
#define WRAPSPLIT 10.
#define NBINS 200
#define BINWIDTH 0.01
#define FILENMLEN 250

static int Read_Param();
static int Write_Param();
static char *upper();

static FILE *fp_in;
static FILE *fp_out;
static char infile[FILENMLEN];
static char outfile[FILENMLEN];
static char file[FILENMLEN];

void oldAspect()

{
char line[100];

double binwidth;
double time,euler1,euler2,euler3,xoff,yoff;
int sensor;
int status = 0;

double euler10,euler20;
int i,j;

char verbose[80];
double sum1[NBINS][NBINS],sum2[NBINS][NBINS],sum3[NBINS][NBINS];
int count[NBINS][NBINS];
int outside,row;

int count4,countmax;
int imax=0,jmax=0;

int shift1,shift2,shift3;

/*   Call Read_Param to Read the parameter file */

     status=Read_Param(infile,outfile,verbose);
     if(status !=0){
     fprintf(stderr," Error in calling Read_Param routine \n");
     exit(1);
     }

     if ((fp_in =fopen(infile,"r")) == NULL){
     fprintf(stderr,"File Open Error !! (%s)\n",infile);
     exit(1);
     }

/*********************************************
* throw away the first 21 lines of the file 
* This is the QDP header
*********************************************/

i=0;
while(i<21)
     {
     fgets(line,100,fp_in);
     ++i;
     }

/******************************
* initialize all bins to zero *
******************************/

i=0;
while(i<NBINS)
     {
     j=0;
     while(j<NBINS)
          {
          count[i][j]=0;

          sum1[i][j]=0.;
          sum2[i][j]=0.;
          sum3[i][j]=0.;
          
          ++j;
          }
     ++i;
     }

/**************************************************************************
* read the first point and determine if we need to worry about wraparound *
**************************************************************************/
row=0;
fscanf(fp_in, "%lf %lf %lf %lf %lf %lf %d",
      &time,&euler1,&euler2,&euler3,&xoff,&yoff,&sensor);

shift1= euler1<WRAPTEST || euler1>360.-WRAPTEST;
shift2= euler2<WRAPTEST || euler2>360.-WRAPTEST;
shift3= euler3<WRAPTEST || euler3>360.-WRAPTEST;



/******************************************************
* set the center of the bin system to the first point *
******************************************************/
euler10=euler1-BINWIDTH*NBINS*.5;
euler20=euler2-BINWIDTH*NBINS*.5;

/*********************************
* put the first point in its bin *
*********************************/

i=(euler1-euler10)/BINWIDTH;
j=(euler2-euler20)/BINWIDTH;

++count[i][j];
sum1[i][j]=sum1[i][j]+euler1;
sum2[i][j]=sum2[i][j]+euler2;
sum3[i][j]=sum3[i][j]+euler3;

/************************
* read and bin the rest *
************************/
outside=0;
while(fscanf(fp_in, "%lf %lf %lf %lf %lf %lf %d",
      &time,&euler1,&euler2,&euler3,&xoff,&yoff,&sensor)>0)
     {

     /********************
     * wrap if nesessary *
     ********************/
     if(shift1 && euler1<WRAPSPLIT) euler1=euler1+360.;
     if(shift2 && euler2<WRAPSPLIT) euler2=euler2+360.;
     if(shift3 && euler3<WRAPSPLIT) euler3=euler3+360.;

     /****************
     * determine bin *
     ****************/
     i=(euler1-euler10)/BINWIDTH;
     j=(euler2-euler20)/BINWIDTH;
     
     if(i>=0 && i<NBINS && j>=0 && j<NBINS)
          {
          /**********************************
          * point is withing the bin system *
          **********************************/
          ++count[i][j];
          sum1[i][j]=sum1[i][j]+euler1;
          sum2[i][j]=sum2[i][j]+euler2;
          sum3[i][j]=sum3[i][j]+euler3;
          }
     else
          {
          /******************************************************
          * point fell outside the bin system, so just count it *
          ******************************************************/
          ++outside;
          }

     ++row;   
     }


if((verbose[0]=='y')|(verbose[0]=='Y'))
     {
     /*************************
     * list the occupied bins *
     *************************/
     i=0;
     fprintf(fp_out, "\n List of Bin Counts and Sums for Each Euler Angle: \n");

     while(i<NBINS)
          {
          j=0;
          while(j<NBINS)
               {
               if(count[i][j]>0)
                    {
                    fprintf(fp_out,
                            "%d %d count=%d sum1=%g sum2=%g sum3=%g\n",
                            i,j,count[i][j],sum1[i][j],sum2[i][j],sum3[i][j]);
                    }
               ++j;
               }
          ++i;
          }
   fprintf(fp_out,"%d out of %d points outside bin structure\n\n",outside,row);
     }


/**********************************************************
* find the set of four adjacent bins with the most counts *
**********************************************************/
countmax=0;
i=0;
while(i<NBINS-1)
     {
     j=0;
     while(j<NBINS-1)
          {
          count4=count[i][j]+count[i+1][j]+count[i][j+1]+count[i+1][j+1];
          if(count4>countmax)
               {
               countmax=count4;
               imax=i;
               jmax=j;
               }
          ++j;
          }
     ++i;
     }

/******************************************************
* calculate the mean in the most populous set of bins *
******************************************************/

euler1=( sum1[imax][jmax]+sum1[imax+1][jmax]+sum1[imax][jmax+1]
        +sum1[imax+1][jmax+1] )/(double)countmax;

euler2=( sum2[imax][jmax]+sum2[imax+1][jmax]+sum2[imax][jmax+1]
        +sum2[imax+1][jmax+1] )/(double)countmax;

euler3=( sum3[imax][jmax]+sum3[imax+1][jmax]+sum3[imax][jmax+1]
        +sum3[imax+1][jmax+1] )/(double)countmax;

/**************************************************
* make sure Euler angles are in the 0 - 360 range *
**************************************************/
while(euler1>360.) euler1=euler1-360.;
while(euler2>360.) euler2=euler2-360.;
while(euler3>360.) euler3=euler3-360.;

fprintf(fp_out, "%g %g %g\n",euler1,euler2,euler3);

/*   Call Write_Param to Write to the parameter file */
     status=Write_Param(euler1, euler2, euler3);
     if(status !=0){
     fprintf(stderr," Error in calling Write_Param routine \n");
     exit(1);
     }
/*
exit(outside);
*/
}

/**************************************************/
/*   upper: Function to convert character upper   */
/**************************************************/

char *upper(str,  n)
char *str;
int n;
{
  int  i;
  for (i=0;i<n;i++)   *(str+i) = (char)toupper((unsigned char)(*(str+i)));
  return str;
}


/******************************************************/
/*   Read_Param:  Function to read the parameter file */
/******************************************************/

    int Read_Param(infile,outfile,verbose)
    char *infile, *outfile;
    char *verbose;
     {
     int BufLen_2 = 255;
     int status = 0;


/*   Read input filename   */
     Uclgst("infile", infile, &status);
     if(status !=0) {
     fprintf(stderr,"Could not get infile parameter \n");
     exit(1);
     }

/*   Read output filename   */
     Uclgst("outfile", outfile, &status);
     if(status !=0) {
     fprintf(stderr,"Could not get outfile parameter \n");
     exit(1);
     }

/*   Read VERBOSE parameter   */
     Uclgst("verbose", verbose, &status);
     if(status !=0) {
     fprintf(stderr,"Could not get VERBOSE parameter \n");
     exit(1);
     }

/*   set outfile   */
     sprintf(file,"%s",outfile);
     if ( !strcmp(upper(file,strlen(file)),"STDOUT") )   fp_out=stdout;
     else if ( ( fp_out = fopen(outfile,"w") ) == NULL ){
     fprintf(stderr,"File Open Error !! (%s)\n",outfile);
     exit(1);
     }

  return status;
  }


/******************************************************/
/*   Write_Param:  Function to write to the parameter file */
/******************************************************/

    int Write_Param(euler1, euler2, euler3)
    double euler1, euler2, euler3; 
     {
     int BufLen_2 = 255;
     int status = 0;

/*   Write Euler1 value to the parameter file   */
     Uclpsr("euler1", euler1, &status);
     if(status !=0) {
     fprintf(stderr,"Could not copy euler1 parameter \n");
     exit(1);
     }

/*   Write Euler2 value to the parameter file   */
     Uclpsr("euler2", euler2, &status);
     if(status !=0) {
     fprintf(stderr,"Could not copy euler2 parameter \n");
     exit(1);
     }

/*   Write Euler3 value to the parameter file   */
     Uclpsr("euler3", euler3, &status);
     if(status !=0) {
     fprintf(stderr,"Could not copy euler3 parameter \n");
     exit(1);
     }

  return status;
  }
