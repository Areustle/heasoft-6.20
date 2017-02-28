
#include <stdio.h>
#include <string.h>
#include <errno.h> 
#include "cfortran.h"
#include "pctype.h"
#include "cfitsio.h"
#include "ftools.h"
#include "xte.h"

#ifndef TRUER
#define TRUER  1             /* Define a logical TRUE value */
#endif

#ifndef FALSER
#define FALSER 0             /* Define a logical FALSE value */
#endif

#define DEBUGIT FALSER         /* Set Debug flag for easier debugging */

/*********************************************************************
  This routine reads 1 row of data out of a Transparent mode FITS file
  or a GoodXenon mode FITS file and returns the results to the calling
  routine. 
*********************************************************************/
int Read_Line(iunit, row, rowTime, lostEventCount, triggerFlags, spillageFlag, eventList, rcount, nfield, pstat) 

     int iunit, rcount, *row, nfield, *pstat;
     double *rowTime;
     unsigned int *lostEventCount;
     unsigned char *triggerFlags, *spillageFlag;
     unsigned int *eventList;
{
  
  int numberofEvents[1];
  int lostEvent_Count[1],PartRowSeq[1];
  char trigger_Flags[1],spillage_Flag[1];
  int event_List[16385];

  int colread, eincr=1, nultyp=0;
  double nulvald=0.0, dtemp=0.0, dnumber=0.0, drcount=0.0, dmaxcount=0.0;
  int nulvalj=0,icount=0,maxcount=64;
  int nulvali=0, ilen, val_len, ii;
  int flgval[5], anynul,stat,i,j,one=1;
  int temprow;
  char cval[21], update[90];

  temprow = *row;

  strcpy(update, "Searching data. Reading rows: ");
  ilen=strlen(update);
  sprintf(cval,"%i",(temprow+1));
  val_len=strlen(cval);
  for (ii = 0; ii < val_len; ii++){
    update[ilen]=cval[ii];
    ilen++;
  }
  update[ilen]='\0';
 
  do{
    
    icount++;
    colread=0;

    temprow++;     /* Increment ROW that is being read from the file. */
    stat= *pstat;
    colread++;      /* Increment COLUMN that is read. */
    
/*    strcpy(update, "Searching data. Reading row number: ");
    ilen=strlen(update);
    sprintf(cval,"%i",temprow);
    val_len=strlen(cval);
    for (ii = 0; ii < val_len; ii++){
      update[ilen]=cval[ii];
      ilen++;
    }
    update[ilen]='\0';
    strcat(update,", from input unit:");

    ilen=strlen(update);
    sprintf(cval,"%i",iunit);
    val_len=strlen(cval);
    for (ii = 0; ii < val_len; ii++){
      update[ilen]=cval[ii];
      ilen++;
    }

    update[ilen]='\0';
    XTE_Fcecho(update); */
    
    flgval[0]=anynul=0;

  /*         
        subroutine ftgcfd(iunit,colnum,frow,felem,nelem,array,
     &          flgval,anynul,status)

C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       array   d  returned array of data values that was read from FITS file
C       flgval  l  set .TRUE. if corresponding element undefined
C       anynul  l  set to .TRUE. if any of the returned values are undefined
C       status  i  output error status
C
*/

    FCGCFD(iunit,colread,temprow,1,1,rowTime,flgval,&anynul,&stat);
    if(stat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not get Timestamp");
      Fcerrm(stat);
      exit(1);
    }

/*
        subroutine ftgcvi(iunit,colnum,frow,felem,nelem,nulval,array,
     &          anynul,status)
C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       nulval  i*2  value that undefined pixels will be set to
C       array   i*2 returned array of data values that was read from FITS file
C       anynul  l  set to .TRUE. if any of the returned values are undefined
C       status  i  output error status

        subroutine ftgcvj(iunit,colnum,frow,felem,nelem,nulval,array,
     &          anynul,status)
C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       nulval  i  value that undefined pixels will be set to
C       array   i  returned array of data values that was read from FITS file
C       anynul  l  set to .TRUE. if any of the returned values are undefined
C       status  i  output error status
*/

    colread++;      /* Increment COLUMN that is read. */

    FCGCVJ(iunit,colread,temprow,1,1,nulvali,numberofEvents,&anynul,&stat);
    if(DEBUGIT == TRUER)printf("Number of Events read in was %i %i %i\n",temprow,colread,numberofEvents[0]);
    if(stat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not get number of Events");
      Fcerrm(stat);
      exit(1);
    }

/* Since we now know the number of Events we can calculate how many
   rows of RCOUNT elements it will take to accumulate that many events.
   We set that number to be equal to "maxcounts" which we will test 
   "icount" against at the bottom of the loop to determine if we have
   to read more rows to accumulate that total number of Events. */
    if(icount == 1){
      maxcount = (numberofEvents[0]/rcount);
/*      printf("Maxcount is %i and rcount is %i \n",maxcount,rcount); */
/* Since it is possible to have a numberofEvents which are perfectly
   divisible by 256 we have to perform a test  */
      dnumber = (double) numberofEvents[0];
      drcount = (double) rcount;
      dtemp = (dnumber / drcount);
      dmaxcount = (double) maxcount;

/*      printf("Dtemp value is %f\n",dtemp); */

      dtemp = dtemp - dmaxcount ;

/*      printf("Dtemp value is %f\n",dtemp); */

      if(dtemp >= 0.0 && dtemp < 0.0000001 ){
      } else {
	maxcount++;
      }
/*      printf("After checking for remainder maxcount is %i\n",maxcount); */
    }

    if (nfield > 6) {
      colread++;
  
      FCGCVJ(iunit,colread,temprow,1,1,nulvali,lostEvent_Count,&anynul,&stat);
      lostEventCount[0]=(unsigned int) lostEvent_Count[0];
      if(DEBUGIT == TRUER)printf("LostEvent read in was %i\n",lostEventCount[0]);
      if(stat != 0){
	XTE_Fcecho(" ");
	XTE_Fcecho("Could not get Lost Event Counts");
	Fcerrm(stat);
	exit(1);
      }
    } 
    else {
      lostEventCount[0]=0;
    }

/*        subroutine ftgcfb(iunit,colnum,frow,felem,nelem,array,
     &          flgval,anynul,status)
C       iunit   i  fortran unit number
C       colnum  i  number of the column to read
C       frow    i  first row to read
C       felem   i  first element within the row to read
C       nelem   i  number of elements to read
C       array   b  returned array of data values that was read from FITS file
C       flgval  l  set .TRUE. if corresponding element undefined
C       anynul  l  set to .TRUE. if any of the returned values are undefined
C       status  i  output error status */

    colread++;

    FCGCFB(iunit,colread,temprow,1,1,spillage_Flag,flgval,&anynul,&stat);
    spillageFlag[0]=(unsigned char) spillage_Flag[0];
    if(DEBUGIT == TRUER)printf("spillageFlag read in was %i\n",spillageFlag[0]);
    if(stat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not get read SpillageFlag");
      Fcerrm(stat);
      exit(1);
    }
    
    colread++;
  
    FCGCFB(iunit,colread,temprow,1,1,trigger_Flags,flgval,&anynul,&stat);
    triggerFlags[0]=(unsigned char) trigger_Flags[0];
    if(DEBUGIT == TRUER)printf("triggerFlag read in was %i\n",triggerFlags[0]);
    if(stat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not get read triggerFlags");
      Fcerrm(stat);
      exit(1);
    }

    colread++;
    
    FCGCVJ(iunit,colread,temprow,1,1,nulvali,PartRowSeq,&anynul,&stat); 
    if(stat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not get PartRowSeq value");
      Fcerrm(stat);
      exit(1);
    }

    if(icount != PartRowSeq[0]){
      XTE_Fcecho(" ");
      XTE_Fcecho("ERROR!!! PartRowSeq and icount do not agree!");
      XTE_Fcecho("This indicates that a ROW is out of ORDER!");
      XTE_Fcecho("Cannot continue... Check data.... Aborting!!!!");
      printf("Icount is %i and PartRowSeq[0] is %i\n",icount,PartRowSeq[0]);
      printf("The row that we are reading is %i\n",temprow);
      printf("Max count is %i, number of Events %i, rount is %i \n",maxcount,numberofEvents[0],rcount);
      exit(1);
    }

    colread++;
    
    if(DEBUGIT == TRUER)printf("Going into Eventlist call \n");
    FCGCVJ(iunit,colread,temprow,1,rcount,nulvali,event_List,&anynul,&stat); 
    if(DEBUGIT == TRUER)printf("Coming out of Eventlist call %i\n", eventList[numberofEvents[0]]);
    j=(icount-1)*rcount;
/*    printf("Storing elements from j %i",j); */
    for(i=0; (i <= (rcount -1) && j <= numberofEvents[0]-1); i++){
      eventList[j]=event_List[i]; 
      j++;
    }
/*    printf("up to but not including j %i \n",j); */
    if(stat != 0){
      XTE_Fcecho(" ");
      XTE_Fcecho("Could not get eventList");
      Fcerrm(stat);
      exit(1);
    }

  }while(icount <= maxcount-1);

  strcat(update, " to ");
  ilen=strlen(update);
  sprintf(cval,"%i",(temprow+1));
  val_len=strlen(cval);
  for (ii = 0; ii < val_len; ii++){
    update[ilen]=cval[ii];
    ilen++;
  }
  update[ilen]='\0';


  strcat(update,", from input unit:");
  
  ilen=strlen(update);
  sprintf(cval,"%i",iunit);
  val_len=strlen(cval);
  for (ii = 0; ii < val_len; ii++){
    update[ilen]=cval[ii];
    ilen++;
  }

  update[ilen]='\0';
  XTE_Fcecho(update);

  *pstat=stat;

/* Now that we have finished reading all of the rows with the same
   TIMESTAMP we have to increment the pointer to the proper value before
   returning to the main program. */
  temprow = *row;
  temprow += maxcount-1;
  *row = temprow;

  return (int)(numberofEvents[0]);
}
