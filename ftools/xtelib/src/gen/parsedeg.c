/*
  FILENAME:    pars_deg.c
  purpose:     Parse string entered in as Deg.dec or DEG MIN SEC.DEC
               (IFLAG=0) or HOUR MIN SEC.DEC (IFLAG=1)
  author:      Brian K. Elza 
  date:        May 1995  
  status:      First iteration of this code. 
  */ 

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "cfortran.h"
#include "pctype.h"

/* The following are variables defined in CFITSIO.H:
   They are repeated here for convenience.

#define FITS_CLEN_COMMENT   73
#define FITS_FLEN_COMMENT   72
#define FITS_CLEN_KEYVAL    71
#define FITS_FLEN_KEYVAL    70
#define FITS_CLEN_CARD      81
#define FITS_FLEN_CARD      80
#define FITS_CLEN_KEYNAME    9
#define FITS_FLEN_KEYNAME    8
#define FITS_CLEN_HDEFKWDS  25
#define FITS_FLEN_HDEFKWDS  24
#define FITS_CLEN_ERRMSG    31
#define FITS_FLEN_ERRMSG    30
*/

#define BufLen_1 20

/* Notice that in this code the string length is declared as 
   buffer[BufLen_1+1] but that we could use many more elements in 
   preforming the work. This is done just in case the SGI or one of the 
   many operating systems have a problem with this type of definition.
   But the following should work on any system.

   Begin definition of boolean parser routine. See below this 
   routine for the FCALLSCSUBn definition which is required to be
   able to call this C routine from FORTRAN.

   IFLAG is used to determine if the string is hh mm sec.dec -> IFLAG=1
   or IFLAG is used to determine if the string is deg mm sec.dec -> IFLAG=0
   double precisioned value returned is in deg.dec. 
   */

void Pars_deg(buffer, value, iflag, status)
  char buffer[BufLen_1+1];
  double *value;
  int *status,iflag;
{
  double tempval = 0.0, temphold = 0.0, multval = 1.0;
  double factor = 10.0, dec = 0.0, temp = 0.0, sign = 1.0;
  char char_val;
  char compare_val[10];
  int int_val=0, i, ifirst_pos, k, istart=0, ispace=0, itemp_val=0;
  int ideg=0, imin=0, isec=0, isecset=0, ipos=0, idegf=0, imult=0, idecimal, ilast_pos;

  /*  printf("Into parsedeg with %s %f %i %i \n",buffer,*value,iflag,*status); */

  compare_val[0]='0';
  compare_val[1]='1';
  compare_val[2]='2';
  compare_val[3]='3';
  compare_val[4]='4';
  compare_val[5]='5'; 
  compare_val[6]='6';
  compare_val[7]='7';
  compare_val[8]='8';
  compare_val[9]='9';

/* Determine how long the string that was input is */

  k=strlen(buffer);

/* Remove any spaces preceding the values or after the values. */
  ifirst_pos=0;
  while(isspace(buffer[ifirst_pos]))ifirst_pos++;
  ilast_pos=k;
  while(isspace(buffer[ilast_pos]))ilast_pos--;

/* Account for and negative sign and remove any spaces just in case. */
  if(buffer[ifirst_pos] == '-'){
    sign = -1.0;
    ifirst_pos++;
    while(isspace(buffer[ifirst_pos]))ifirst_pos++;
  }

/* (MJT) 16Jun00: Initial '+' confuses this code */
  if(buffer[ifirst_pos] == '+'){
      ifirst_pos++;
      while(isspace(buffer[ifirst_pos]))ifirst_pos++;
  }

/* Initialize starting position to begin moving through the string. */
  ipos=ifirst_pos;

/* Extract the first grouping of digits in the string and convert to 
   a numerical value. */
  while(isdigit(buffer[ipos]) && ipos != ilast_pos){
    char_val=buffer[ipos];
    for(i=0; i <= 9; i++)if(char_val == compare_val[i]) int_val=i;
    ideg *= (int) factor;
    ipos++;
    ideg +=int_val;
  }

  tempval = (double) ideg;
  if(buffer[ipos] == '.'){
    iflag=0;
    idegf=1;
  }

/* Remove any blanks to the next grouping. */
  while(isspace(buffer[ipos]) && ipos != ilast_pos)ipos++;

/* Convert the next grouping digits to numerical values. */
  while(isdigit(buffer[ipos]) && ipos != ilast_pos){
    char_val=buffer[ipos];
    for(i=0; i <= 9; i++)if(char_val == compare_val[i]) int_val=i;
    imin *= (int) factor;
    ipos++;
    imin +=int_val;

/* If the first grouping wasn't in DEG but was in HOURS then convert to
   degrees - this is tested for at several places but only done once by
   resetting the value of IFLAG. */

/*    if(iflag == 1){ 
      tempval *= 15.0;
      iflag = 0; 
    } */
  }

/* Since this grouping (the second) is in Minutes convert it into degrees. */
  if(imin){
    tempval += ((double) imin) /60.0;
  }

/* Remove any blanks to the next grouping. */
  while(isspace(buffer[ipos]) && ipos != ilast_pos)ipos++;
  
/* Convert the next grouping of digits to numerical values. */
  while(isdigit(buffer[ipos]) && ipos != ilast_pos){
    isecset=1;
    char_val=buffer[ipos];
    for(i=0; i <= 9; i++)if(char_val == compare_val[i]) int_val=i;
    isec *= (int) factor;
    ipos++;
    isec += int_val;
  }

  if(isec){
    temphold += ((double) isec);
  }

/* This tests for a decimal point and if found increments past it. */
  if(buffer[ipos] == '.')ipos++;

/* Convert the next grouping digits to numerical values. */
  factor=0.1;
  while(isdigit(buffer[ipos]) && ipos != ilast_pos){
    char_val=buffer[ipos];
    for(i=0; i <= 9; i++)if(char_val == compare_val[i]) int_val=i;
    multval *= factor;
    dec = (double) int_val;
    dec *= multval;
    temp += dec;
    ipos++;
  }

/* The following tests to see if the decimals are associated with the
   seconds or with the degree string and performs the proper operations to 
   ensure that the resulting output is in degrees. */
  if(!idegf){
    if(!isecset){
      if(!temp){
	tempval += temp;
      }
    }
    else { 
      tempval += (temp + temphold)/3600.0;
    }
  }
  else {
    tempval += temp;
  }

    if(iflag == 1){ 
      tempval *= 15.0;
      iflag = 0; 
    } 

/* Change the output to the proper sign. */
  tempval *= sign;
  *value = tempval;

/* Check to see if there were any "strange" characters, i.e., those that
   weren't properly accounted for and, if so set a status - ERROR flag 
   saying that something may be wrong with the output and that the 
   input needs to be checked. */

/* First, check to see if the input was in scientific notation and if so then
   shift the values accordingly. Apparently the manual stating NOT to use
   scientific notation was too difficult for some users to comprehend, so 
   we make the code "smarter" so that the average user doesn't have to be. */
  factor = 10.0;

  if(ipos != ilast_pos){
    if(buffer[ipos] == 'E' | buffer[ipos] == 'D' | buffer[ipos] == 'e' | buffer[ipos] == 'd'){
      ipos++;

      if(buffer[ipos] == '-'){
	factor=0.1;
	ipos++;
      }

      if(buffer[ipos] == '+'){
	factor=10.0;
	ipos++;
      }

      while(isdigit(buffer[ipos]) && ipos != ilast_pos){
	char_val=buffer[ipos];
	for(i=0; i <= 9; i++)if(char_val == compare_val[i]) int_val=i;
	i *= (int) 10;
	ipos++;
	imult += int_val;
      }
      
      for(i=0; i < imult; i++){
	tempval *= factor;
      }

      *value = tempval;

    } else {
      *status=1;
    }

  }

  /*  printf("Out of parsedeg with %s %f %i %i \n",buffer,*value,iflag,*status); */

}

/*######################################################################*/
/******************** Parsedeg ********************/
/* Note that the C subroutine call has to be different from the
   FORTRAN subroutine call or VMS will scream about this. */

FCALLSCSUB4(Pars_deg,PARSEDEG,parsedeg,STRING,PDOUBLE,INT,PINT)
