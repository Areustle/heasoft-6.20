/*
  FILENAME:    Sebhelp.c
  purpose:     Write out a HELP screen for the user.
  author:      Brian K. Elza 
  date:        December 1995  
  status:      First iteration of this code. 
  */ 

#include <stdio.h>
#include <string.h>
#include <errno.h>
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
extern void XTE_Fcecho();

void Sebithelp(ihelp)
  int ihelp;
{
  if(ihelp == 1){
    XTE_Fcecho(" ");
    XTE_Fcecho("--------------------------------------------------------");
    XTE_Fcecho("**** Welcome to SEBITMASK HELP ****");
    XTE_Fcecho("--------------------------------------------------------");
  }
  if(ihelp == 1 | ihelp == 2){
    XTE_Fcecho(" ");
    XTE_Fcecho("--------------------------------------------------------");
    XTE_Fcecho(" ");

    XTE_Fcecho("      ******THE M[] token****** ");
    XTE_Fcecho(" ");
    XTE_Fcecho("*You MUST specify an M[N]{m} value for SEBITMASK*");
    XTE_Fcecho("This is passed in via the MTOKEN parameter. (Default: M[1]{1})"); 
    XTE_Fcecho(" ");
    XTE_Fcecho("**The M[] token is NOT to be entered in the EXPRESSION to be converted.**");
    XTE_Fcecho("The value in [] specifies the value this token must have for these ");
    XTE_Fcecho("definitions to apply.");
    XTE_Fcecho("There may be more than one series of definitions in each file.");
    XTE_Fcecho("The applicable definition is differentiated by the M[] token value.");
    XTE_Fcecho(" ");
    XTE_Fcecho("The value in {} specifies the number of bits used to express this value.");
    XTE_Fcecho(" ");
    XTE_Fcecho("Thus possible values and translations are:");

    XTE_Fcecho("  M[1]{1} -> 1,");  
    XTE_Fcecho("M[127]{8} -> 01111111, M[1]{8} -> 00000001,");
    XTE_Fcecho("  M[2]{8} -> 00000010, M[4]{8} -> 00000100 ");
    XTE_Fcecho(" ");
    XTE_Fcecho("M[0]{1} is NOT shown, but is special.");
    XTE_Fcecho("If you set M[0]{1} you will get all other values except M[1]{1}!");
    XTE_Fcecho(" ");
    XTE_Fcecho("--------------------------------------------------------");
    XTE_Fcecho(" ");
  }
  if(ihelp == 1 | ihelp == 3){
    XTE_Fcecho(" ");
    XTE_Fcecho("--------------------------------------------------------");
    XTE_Fcecho(" ");
    XTE_Fcecho("      ******THE Z[] token****** ");
    XTE_Fcecho(" ");
    XTE_Fcecho("The Z token is unique in that its definition changes.");
    XTE_Fcecho("For example, in the following Z[] can take on values from 0 -> 5.");
    XTE_Fcecho("And this is expressed using 3 bits - {3}");
    XTE_Fcecho(" ");
    XTE_Fcecho("So if we are given:");
    XTE_Fcecho("Z[E[X3R],E[X3L],E[X2R],E[X2L],E[X1R],E[X1L]]{3}");
    XTE_Fcecho("    0       1      2      3      4      5");
    XTE_Fcecho("The values are relative to the position. So: ");
    XTE_Fcecho(" ");
    XTE_Fcecho("Thus we are using enumeration to specify the values that Z can take on. ");
    XTE_Fcecho("In the above case case:");
    XTE_Fcecho("For E[X3R] you would specify Z[] == 0.");
    XTE_Fcecho("For E[X3L] you would specify Z[] == 1.");
    XTE_Fcecho("For E[X2R] you would specify Z[] == 2.");
    XTE_Fcecho("For E[X2L] you would specify Z[] == 3.");
    XTE_Fcecho("For E[X1R] you would specify Z[] == 4.");
    XTE_Fcecho("For E[X1L] you would specify Z[] == 5.");
    XTE_Fcecho("You can specify Z[] <= 5 for all of them.");
    XTE_Fcecho(" ");
    XTE_Fcecho("Another example of the Z token is: ");
    XTE_Fcecho("      0           1           2           3           4           5");
    XTE_Fcecho("Z[D[0]&E[X1L],D[0]&E[X1R],D[0]&E[X2L],D[0]&E[X2R],D[0]&E[X3L],D[0]&E[X3R],");
    XTE_Fcecho("      6           7           8           9          10          11");
    XTE_Fcecho("  D[1]&E[X1L],D[1]&E[X1R],D[1]&E[X2L],D[1]&E[X2R],D[1]&E[X3L],D[1]&E[X3R]");
    XTE_Fcecho("     12          13          14          15          16          17");
    XTE_Fcecho("  D[2]&E[X1L],D[2]&E[X1R],D[2]&E[X2L],D[2]&E[X2R],D[2]&E[X3L],D[2]&E[X3R]");
    XTE_Fcecho("     18          19          20          21          22          23");
    XTE_Fcecho("  D[3]&E[X1L],D[3]&E[X1R],D[3]&E[X2L],D[3]&E[X2R],D[3]&E[X3L],D[3]&E[X3R]");
    XTE_Fcecho("     24          25          26          27          28          29");
    XTE_Fcecho("  D[4]&E[X1L],D[4]&E[X1R],D[4]&E[X2L],D[4]&E[X2R],D[4]&E[X3L],D[4]&E[X3R]]{5}");
    XTE_Fcecho(" ");
    XTE_Fcecho("Thus to specify D[0] only:       Z[] <= 5.");
    XTE_Fcecho("To specify both D[0]&D[1]:       Z[] <=11.");
    XTE_Fcecho("To specify D[1] only:            Z[] >=6 & Z[] <=11.");
    XTE_Fcecho("To specify all E[X1L] values:    Z[] == 0 | Z[] == 6 | Z[] == 12 |");
    XTE_Fcecho("                                 Z[] == 18 | Z[] == 24, etc.");
    XTE_Fcecho(" ");
    XTE_Fcecho("--------------------------------------------------------");
    XTE_Fcecho(" ");
  }
  if(ihelp == 1 | ihelp == 4 ) {
    XTE_Fcecho(" ");
    XTE_Fcecho("--------------------------------------------------------");
    XTE_Fcecho(" ");
    XTE_Fcecho("      ******THE D[] token****** ");
    XTE_Fcecho(" ");
    XTE_Fcecho("The D token, usually given as D[0:4]{3}, specifies the PCU ID.");
    XTE_Fcecho("The values in [] specifies the values this token can have.");
    XTE_Fcecho("So with 3 bits we can specify values up to 7.");
    XTE_Fcecho("Here we see that only values of 0 -> 4 are meaningful.");
    XTE_Fcecho(" ");
    XTE_Fcecho("Thus to specify D[0] only:       D[] == 0.");
    XTE_Fcecho("To specify both D[0]&D[1]:       D[] <= 1.");
    XTE_Fcecho("To specify D[1],D[2], and D[3]   D[] >= 1 & D[] <= 3");
    XTE_Fcecho("To specify D[4] only:            D[] == 4.");
    XTE_Fcecho("To specify D[1],D[2], and D[4]   D[] == 1 | D[]== 2 | D[] == 4");
    XTE_Fcecho(" ");
    XTE_Fcecho("--------------------------------------------------------");
    XTE_Fcecho(" ");

  }
  if(ihelp == 1 | ihelp == 5){
    XTE_Fcecho(" ");
    XTE_Fcecho("--------------------------------------------------------");
    XTE_Fcecho(" ");
    XTE_Fcecho("      ******THE E[] token****** ");
    XTE_Fcecho(" ");
    XTE_Fcecho("This is a rather special token, due to its complexity.");
    XTE_Fcecho(" ");
    XTE_Fcecho("Sometime in various files you may see such expressions as E[0~2047]");
    XTE_Fcecho("in the TDDES keyword. This means that there are 11 bits dedicated ");
    XTE_Fcecho("describing the various energy configurations. This can be confusing");
    XTE_Fcecho("but is described in the documentation for XFF. To make life a bit ");
    XTE_Fcecho("easier, in TRANS2FITS we break up this into its constituent components ");
    XTE_Fcecho("so that there are E[CAL]{1}, E[VLE]{1}, E[VPR]{1}, E[VXH]{1}, and E[VLH]{1}");
    XTE_Fcecho("bits that can be specified. Thus only the various layers X3R, X3L, ");
    XTE_Fcecho(" X2R, X2L, X1R, and X1L are specified as E[0:63]{6}.");
    XTE_Fcecho(" ");

    XTE_Fcecho("To specify the Calibration bit be set set: E[CAL] == 1.");
    XTE_Fcecho("To specify the Propane bit be set set:     E[VPR] == 1. ");
    XTE_Fcecho("To specify the VX bit be set set: E[VXL] == 1 & E[VXH] == 1.");
    XTE_Fcecho(" ");
    XTE_Fcecho("The E[0:63] specification is different:");
    XTE_Fcecho("E[0:63]{6} is a shorthand way in whish to specify X3R,X3L,X2R,X2L,X1R,X1L ");
    XTE_Fcecho("in which a specific bit represents each layer      5 , 4 , 3 , 2 , 1 , 0");
    XTE_Fcecho("with the associated decimal value of:             32 ,16 , 8 , 4 , 2 , 1");
    XTE_Fcecho("Note that 32+16+8+4+2+1 = 63 thus we have values from 0 -- > 63.");
    XTE_Fcecho("To specify layers X3R, and X2L layers:    E[0:63] == 32 | E[0:63] == 4");
    XTE_Fcecho("To specify layers all X1 and X2 layers:   E[0:63] <= 15 (8+4+2+1)");
    XTE_Fcecho(" ");
    XTE_Fcecho("--------------------------------------------------------");
    XTE_Fcecho(" ");
  }

  if(ihelp == 1 | ihelp == 6){
    XTE_Fcecho(" ");
    XTE_Fcecho("--------------------------------------------------------");
    XTE_Fcecho(" ");
    XTE_Fcecho("      ******THE C[] token****** ");
    XTE_Fcecho(" ");
    XTE_Fcecho("Do not use SEBITMASK and FSELECT to select channels!");
    XTE_Fcecho("FSELECT does not know which absolute channels go with which relative channel.");
    XTE_Fcecho("Use SEEXTRCT to specify channel filtering information. ");
    if(ihelp == 6){
      XTE_Fcecho(" ");
      XTE_Fcecho("*****Will attempt to use C[] supplied in the expression*****"); 
      XTE_Fcecho("*****Check the results carefully.*****");
      XTE_Fcecho("***USE SEEXTRCT for channel filtering***");
    }
  }

}





/*######################################################################*/

/******************** Sebithelp ********************/
/* Note that the C subroutine call has to be different from the
   FORTRAN subroutine call or VMS will scream about this. */

FCALLSCSUB1(Sebithelp,SEBHELP,sebhelp,INT)
