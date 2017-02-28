
#include <stdio.h>
#include <string.h>
#include <errno.h> 
#include "cfortran.h"
#include "pctype.h"
#include "cfitsio.h"
#include "ftools.h"
#include "xte.h"

/*******************************************************************
  This routine adds a row (event) to a FITS file in nX format.
 *******************************************************************/
void Add_Event_VLE(ounit, irow, time, value)
     double time; 
     int value[24];
     int ounit;
     int irow; 
{

  int stat=0,staterr=0;
  int felem=1, nelem=1, nelem2=16, col=0;
  
  col++;
  FCPCLD(ounit,col,irow,felem,nelem,&time,&stat);
  if(stat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not put TIME information into outputfile");
    Fcerrm(stat);
    FCCLOS(ounit,&staterr);
    if(staterr != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not close output file");
      Fcerrm(staterr);
    }    
    exit(1);
  }

  col++;

/*  Since C doesn't have a logical type and since FITSIO writes out logicals
    as an array it is dangerous to write out BYTES and to assume that it
    is being translated properly. To avoid this problem I have a translation
    routine the accecpts as input an array of integers that are either 0 or 1 
    and a 0 is translated to a logical FALSE and a 1 to a logical TRUE.
    */
  Int2log(ounit,col,irow,nelem2,value,&stat);
  if(stat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not put BIT information into outputfile");
    (void) printf("Status returned was %i\n",stat);
    Fcerrm(stat);

  FCMKYJ(ounit,"NAXIS2",irow-1," Number of rows ",&staterr);
  if(staterr != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not update total number of rows output in output file");
    Fcerrm(staterr);
    exit(1);
  }  

  FCRDEF(ounit,&staterr);
  if(staterr != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not update structure of output in output file");
    Fcerrm(staterr);
    exit(1);
  }  

    FCCLOS(ounit,&staterr);
    if(staterr != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not close output file");
      Fcerrm(staterr);
    }    
    exit(1);
  }

}
