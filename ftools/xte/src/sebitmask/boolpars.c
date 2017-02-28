/*
  FILENAME:    Boolpars.c
  purpose:     Parse boolean expression to obtain a bit-mask
  author:      Brian K. Elza 
  date:        Februray 1995  
  status:      First iteration of this code. 
  */ 

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include "cfortran.h"
#include "pctype.h"
#include "cfitsio.h"
#include "ftools.h"

/* #define FITS_BLOCKSIZE 2880 Blocksize of FITS file */
                           
/* #define KEYWORD_SIZE 9      Allocate space for all keyword holders */
/* #define KEYCOMMENT 73       Allocate space for all keyword comments */
/* #define HEADERCARD 81       Allocate space for all header cards */
/* #define KEYWORD_VAL 71      Allocate space for all keyword values */
/* #define ERRMSG 31           Allocate space for all error messages */
/* #define MXELEM 20           Allocate for number of columns for files */
/* #define FitsStrBufLen 73    Allocate string buffer length */
                           
/* #define False 0             Define a logical FALSE value */
/* #define True  1             Define a logical TRUE value */

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

   The following are variables defined within ftools.h

#define BufLen_2c 256       Max size of filename string 
#define BufLen_2 255       Max size of filename string 
*/
#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#define DEBUGIT FALSE
/* This is the Beginning of the CFORTRAN macros that define the 
   XPI calls...
 */
/* Define the usual length of the strings returned from xpi calls. */
#define BufLen_2 3000

/* Define a length for all of the bitmask character arrays. */
#define Elems 512

extern void XTE_Fcecho();
int Char_2_Int();
void Generate_Character_Binary();

/* Define a structure which contains information in a usable 
   form with a character string which describes the information 
   in the bit string, and 3 integer values containing the number
   of bits before (bbfr) we come to the bit stream (bstrm) containing
   the information about how many bits are dedicated to this information 
   and finally an integer describing how many bits come after (baftr).*/
struct binfo {
  char *cbinfo;
  int bbfr;
  int bstrm; 
  int baftr;
} ;

/* Define a structure which contains information about the Bitmasks and
   operations to be performed on them and the relationships between them. */
struct string_info {
  char cbitmask[Elems];
  int equiv;
} ;

/* Define a structure which will contain string_info structure and the
   relationship between that and the following string_info and the nesting 
   parenthesis heirarchy. */
struct bit_relations {
  struct string_info Bit_elem;
  char operation;
  int paren_level, paren_no;
} ;
  
/* Begin definition of boolean parser routine. See below this 
 routine for the FCALLSCSUBn definition which is required to be
 able to call a C routine from FORTRAN.*/

void Boolpars(tddes,rcount,column,cexpr,bitmask,status)
  char tddes[BufLen_2+1],cexpr[3001],bitmask[3001],
    column[41];
  int rcount, *status;
{

  char *p, temp[BufLen_2+1], cstore[BufLen_2+1], *c[25],
    cfiltemp[BufLen_2+1],out_val[Elems],chold[20],
    bitval[128],*bstart[25],*bstop[25];
  int i=0,j=0,k=0,l=0,m[8],n=0,iand= -1,ior= -1,ichold,iequiv=0,o=0,
    istart[25],istop[25],pstart[8][8],pstop[8][8],and[25],icol,ipar,
    or[25],equiv[25],itemp=0,value,ibitmask,info_array_num=0,imatch=0,
    paren_no,ibit=0;
  struct binfo info_array[25];
  struct bit_relations bit_count[25];

  XTE_Fcecho(" ");
/*  struct finfo output_array[25] */


/***********************************************************************/
/* This section of the code is dedicated to parsing the DDL into a form
   that contains information which we can use. */
  l=0;
  info_array[l].cbinfo= &temp[0];
  l++;

/* Find out how long the initial string is. */
  k=strlen(tddes);
  if(DEBUGIT == TRUE)printf("Input string is %s and length is %i.\n",tddes,k);

/* In this loop we are going to create several arrays with useful 
   information about how the data is stored. */
  for(i=0; i < k; i++){

/* Remove all blank spaces from the original string and store all 
   non-blank characters in the string temp[] */
    if(tddes[i] != ' '){
      temp[j]=toupper(tddes[i]);
      j++;

/* Now search the string and figure out where the information about 
   how many bits are dedicated to this information and store the 
   information as to where the values start in istart and end in istop. */
      if(tddes[i] == '{'){

/* In order to remove characters of the form {n} we insert a null after 
   we find the opening one, but we continue to process the information
   in a normal manner as we will need to use the "n" from inside the {}
   characters to figure out how many bits of information are dedicated to
   the information that will be contained within "temp". */
	temp[j-1]='\0';

/* Store the array element that is 1 place beyond where the "{" character 
   was found. */
      	istart[l-1]=i+1;
      }
      if(tddes[i] == '}'){
/* Store the array element that is 1 place behind where the "}" character 
   was found. */
	istop[l-1]=i-1;

/* Skip the comma delimiter that always follows the "}" character */
	i++;

/* Add a null character to terminate this string 
	temp[j]='\0'; */

/* Set the pointer array info_array[l].cbinfo to point to the 
   beginning address of the next string. */
	j++;
	info_array[l].cbinfo= &temp[j];
/* Increment the counter that keeps track of how many strings we have. */
	l++;
      }
    }
  }

/* Terminate the final string with a null character just in case, */
  temp[j]='\0';
  
  k=0;

/* Now we are going to figure out the integer equivalent to the ASCII
   character that is stored between the character. So first we must
   create a string which contains ONLY the ASCII numerical information 
   that we wish to convert into integers. */
  
  for(i=0; i <= l-2; i++){
    c[i]= &cstore[k];
    n=0;
    info_array[i].bstrm=0;
    for(o=istart[i]; o <= istop[i]; o++){

/* If we are doing more than 1 loop we will multiply the integer 
   information stored in info_array[i].bstrm by 10 so that it is in 
   its proper place holder - since info_array[i].bstrm is initialized 
   to 0 on the first pass the place holder is 1's. */
      info_array[i].bstrm=(info_array[i].bstrm*10);

/* Store the ASCII character into the cstore array. */
      cstore[k]=tddes[o];

/* Find out the integer equivalent of this character and assign 
   that value to be n. */
      n=Char_2_Int(tddes[o]);

/* Add the integer value to the integer array info_array[i].bstrm. */
      info_array[i].bstrm=info_array[i].bstrm+n;

/* Increment the array pointer by 1. */
      k++;
    } 
    info_array[i].bbfr=itemp;
    info_array[i].baftr=(rcount-itemp-info_array[i].bstrm);
    itemp=itemp+info_array[i].bstrm;

/* Terminate the ASCII character value with a null */
    cstore[k]='\0';

/* Increment the array pointer by 1. */
    k++;

    if(DEBUGIT == TRUE)printf("Information in structure is %s %i %i %i\n",info_array[i].cbinfo, info_array[i].bbfr, info_array[i].bstrm, info_array[i].baftr);
    info_array_num=i;
/* Store all of this information in our structure*/
  }
/***********************************************************************/

/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/
  
/* In this section we are interested in parsing the filtering information
   that has been provided by the user to create a command that is ready
   to go into FSELECT or SEEXTRCT. */

  for(i=0; i < 8; i++){
    m[i]= -1;
    for(j=0; j < 8; j++){
      pstart[i][j]=0;
      pstop[i][j]=0;
    }
  }

/* Initialize holder array which will hold values that allow us to
   perform comparisons and the bitmask conversion.*/
  for(i=0; i < 20; i++)chold[i]=' ';
  for(i=0; i < 128; i++)bitval[i]=' ';

  if(DEBUGIT == TRUE)printf("Before we do anything cexpr is %s \n", cexpr);
  if(DEBUGIT == TRUE)printf("\n");

  k=strlen(cexpr);
  paren_no= -1,ipar=0,ibitmask=0;

  strcpy(cfiltemp,cexpr);
  j=0;

/* Remove all blanks and convert the entire string to uppercase and store
 that in a temporary string.*/

  for(i=0; i < k; i++){
    cfiltemp[j]=' ';
    if(cexpr[i] != ' '){
      cfiltemp[j]=toupper(cexpr[i]);
      j++;
    }
  }
  cfiltemp[j]='\0';

/* Copy the resulting string with all of the spaces removed back into
   cexpr. And find out the length of the new string. */
  strcpy(cexpr,cfiltemp);
  k=strlen(cexpr);
  printf("Expression is %s \n",cexpr);

/* Convert the entire FILTER string into uppercase so that we 
   can do pattern matches easier */

  for(i=0; i < k; i++){

/* Copy cexpr into bitmask one character at a time. */
    bitmask[ibitmask]=cexpr[i];
    ibitmask++;

/* If a relationship operator is found then store it into the structure
   that is defined which stores the bitmask and the equivalence operator 
   that relates it to the Column being searched for. Here we have related
   a specific integer value to the relationship being searched for. */
    if(cexpr[i] == '=' && cexpr[i+1] == '=')bit_count[iequiv].Bit_elem.equiv=1;
    if(cexpr[i] == '<' && cexpr[i+1] == '=')bit_count[iequiv].Bit_elem.equiv=2;
    if(cexpr[i] == '>' && cexpr[i+1] == '=')bit_count[iequiv].Bit_elem.equiv=3;
    if(cexpr[i] == '!' && cexpr[i+1] == '=')bit_count[iequiv].Bit_elem.equiv=4;
    if(cexpr[i] == '<' && cexpr[i+1] != '=')bit_count[iequiv].Bit_elem.equiv=5;
    if(cexpr[i] == '>' && cexpr[i+1] != '=')bit_count[iequiv].Bit_elem.equiv=6;

/* If the & operator is specified than we store the position that it was
   found at and copy it to bitmask a second time so that we always will have
   this translated into "&&" in the bitmask that is passed to FSELECT */
    if(cexpr[i] == '&'){
      bit_count[iequiv].operation = '&';
      iand++;
      and[iand]=i;

/* If the & does not have a space in front of it than insert one
   and increment ibitmask appropriately. */
      if(bitmask[ibitmask-2] != ' '){
	bitmask[ibitmask-1]=' ';
	bitmask[ibitmask]='&';
	ibitmask++;
      }

      bitmask[ibitmask]=cexpr[i];
      ibitmask++;
      bitmask[ibitmask]=' '; /* Add a blank space after the & */
      ibitmask++; /* Increment the counter */

/* If there are two &'s in a row since we have copied the first one twice
   we will ignore the second one that may have been entered. */
      if(cexpr[i+1] == '&')i++;
    }

/* If the | operator is specified than we store the position that it was
   found at and copy it to bitmask a second time so that we always will have
   this translated into "||" in the bitmask that is passed to FSELECT */
    if(cexpr[i] == '|'){
      bit_count[iequiv].operation = '|';
      ior++;
      or[ior]=i;
/* If the | does not have a space in front of it than insert one
   and increment ibitmask appropriately. */
      if(bitmask[ibitmask-2] != ' '){
	bitmask[ibitmask-1]=' ';
	bitmask[ibitmask]='|';
	ibitmask++;
      }

      bitmask[ibitmask]=cexpr[i];
      ibitmask++;
      bitmask[ibitmask]=' '; /* Add a blank space after the | */
      ibitmask++; /* Increment the counter */

/* If there are two |'s in a row since we have copied the first one twice
   we will ignore the second one that may have been entered. */
      if(cexpr[i+1] == '|')i++;
    }
      
/* If the character is a parenthesis then store the position as well as 
   what the nest number is. */ 
    if(cexpr[i] == '('){
      paren_no++;
      m[paren_no]++;
      if(paren_no > ipar)ipar=n++;
      pstart[paren_no][m[paren_no]]=i;
      bit_count[iequiv+1].paren_no=paren_no;
      bit_count[iequiv+1].paren_level=m[paren_no];

      if(DEBUGIT == TRUE)printf("Pstart is %i %i %i\n",paren_no,m[paren_no],pstart[paren_no][m[paren_no]]);
    }

    if(cexpr[i] == ')'){
      pstop[paren_no][m[paren_no]]=i;
      bit_count[iequiv].paren_no=paren_no;
      bit_count[iequiv].paren_level=m[paren_no];

      if(DEBUGIT == TRUE)printf("Pstop is %i %i %i\n",paren_no,m[paren_no],pstop[paren_no][m[paren_no]]);
      paren_no--;
    }

/* This is the section where we try to figure out what type of bit-mask 
   to insert in the output string */
    if(cexpr[i] == '['){
      p=column;
      ibitmask -= 2;
      while(*p){
	bitmask[ibitmask]= *p;
	ibitmask++;
	p++; 
      }
      bitmask[ibitmask]=' '; /* Insert a blank after column name. */
      ibitmask++;  /* Increment bitmask counter */
      chold[0]=cexpr[i-1];
      chold[1]=cexpr[i];
      ichold=1,o=0,i++;
      for(; o < k; i++){
	cexpr[i]=toupper(cexpr[i]);
	ichold++;
	chold[ichold]=cexpr[i];
	if(cexpr[i] == ']'){
	  o=k;
	  chold[ichold+1]= '\0';
	  ichold=ichold+1;
	}
      }
      i--;

      if(DEBUGIT == TRUE)printf("The chold string is %s\n",chold);
      imatch = -1;
      for(o=0; o <= info_array_num; o++){
	if(DEBUGIT == TRUE)printf("Testing info_array %s\n",info_array[o].cbinfo);
	if(strcmp(chold,info_array[o].cbinfo) == 0){
	  imatch=o;
	  o=info_array_num;
	}
      }
      if(imatch == -1){
	printf("Matches are 1 %s 2 %s",chold,info_array[imatch].cbinfo);
	XTE_Fcecho("Could not find matching string in TEVTBnnn!");
	XTE_Fcecho("Cannot continue! Check input! Aborting.");
	*status=1;
      }
    }
    
    if(isdigit(cexpr[i])){
      if(DEBUGIT == TRUE)printf("Ival in is digit for first is %i\n",i);
      n=Char_2_Int(cexpr[i]);
      itemp=0,o=0,ibitmask--,i++;
      if(DEBUGIT == TRUE)printf("Ival in is digit for firstN is %i\n",i);
      itemp=n;
      for(; o < k; i++){
	if(isdigit(cexpr[i])){
	  itemp *= 10;
	  n = Char_2_Int(cexpr[i]);
	  itemp += n;
	}
	else{
/*	  printf("Value is found to be %i\n",itemp); */
	  o=k;

	  Generate_Character_Binary(info_array[imatch],itemp,out_val,Elems);

	  iequiv++; /* Increment counter for number of bitmasks */

/* Copy the string out_val to the element cbitmask of the structure Bit_elem 
   into the string cbitmask. */
	  strcpy(bit_count[iequiv].Bit_elem.cbitmask,out_val);

	  bitmask[ibitmask]=' '; /* Insert blank before bit mask */
	  ibitmask++; /* Increment bitmask counting index */
/*	  printf("Going into for loop %s\n",out_val); */
	  p=out_val;
	  ibit++;
	  bstart[ibit]= &bitmask[ibitmask];
	  bitmask[ibitmask]='b';
	  ibitmask++;
	  while(*p){
	    bitmask[ibitmask]= *p;
	    ibitmask++;
	    p++;
	  }
	  bstop[ibit]= &bitmask[ibitmask];
	  bitmask[ibitmask]=' '; /* Insert blank after bitmask */
	  ibitmask++; /* Increment bitmask count index */

/*	  for(p=bstart[ibit]; p != bstop[ibit]; p++){
	    printf("%c", *p);
	  }
	  printf("\n"); */

	  i -=2;
	}
      }
    }
  }

  if(DEBUGIT == TRUE){
    printf("We are finished searching out bitmask %i \n",iequiv);
    for(i=0; i < iequiv+1; i++) {
    printf("Writing out structures %s %i %c %i %i \n",
    bit_count[i].Bit_elem.cbitmask, bit_count[i].Bit_elem.equiv,
    bit_count[i].operation, bit_count[i].paren_level,
    bit_count[i].paren_no);
    }
    printf("\n");
    printf("Our cexpr is %s max paren is %i \n",cexpr,ipar+1);
  }
  bitmask[ibitmask]='\0';
  if(DEBUGIT == TRUE)printf("Bitmask strings read in is %s\n",bitmask);
}

/*######################################################################*/
/* Let's create a function which will scan an character for a match
   and return the integer that character is represents. */

int Char_2_Int (char_val)
 char char_val;
{
 char compare_val[10];
 int int_val=0,i;
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
 for(i=0; i <= 9; i++)if(char_val == compare_val[i]) int_val=i;
 return int_val;
 }

/*######################################################################*/
void Generate_Character_Binary(info_array,value,out_val,elems)
  struct binfo info_array;
  int value, elems;
  char *out_val;
{

  int i,j,k,l,m,n,itempval;
  if(DEBUGIT == TRUE)printf("Information in structure is %s %i %i %i\n",info_array.cbinfo, info_array.bbfr, info_array.bstrm, info_array.baftr);

  j=0;
  for(i=0; i < elems ; i++)out_val[i]=' ';

  for(i=0; i < info_array.bbfr ; i++){
    out_val[j]='x';
    j++;
  }
  itempval=1;
  for(i=1; i < info_array.bstrm; i++) itempval *= 2;
  if(DEBUGIT == TRUE)printf("Itempval is %i, value is %i \n",itempval,value);

  for(i=info_array.bstrm; i > 0; i--){
    if(itempval <= value){
      if(DEBUGIT == TRUE)printf("Counting VARIABLE IS %i\n",i);
      out_val[j]='1';
      j++;
      value -= itempval;
      itempval /= 2;
      if(DEBUGIT == TRUE)printf("Value is %i, Itempval is %i \n",value,itempval);
    }
    else{
      out_val[j]='0';
      j++;
      itempval /= 2;
    }
  }  
  for(i=0; i < info_array.baftr; i++){
    out_val[j]='x';
    j++;
  }
 
/* Terminate the bitmask string with a null value. */
  out_val[j]='\0';

}

/*######################################################################*/

/******************** Boolparse ********************/
/* Note that the C subroutine call has to be different from the
   FORTRAN subroutine call or VMS will scream about this. */

FCALLSCSUB6(Boolpars,BOOLPARSE,boolparse,STRING,INT,STRING,STRING,PSTRING,PINT)
