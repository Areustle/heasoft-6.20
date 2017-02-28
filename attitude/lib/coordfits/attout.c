#include <string.h>
#include "coord.h"
#include "attout.h"

/*
#define DEBUG
*/

/*****************************************************************************
******************************************************************************
* Create a new ATTOUT output attitude file structure
* This functions creates a structure and opens a corresponding FITS file
* for writing
*****************************************************************************/
ATTOUT* createAttOut(char* filename) {

int status=0;
ATTOUT* att;

int tfields=2;
char* ttype[]={"TIME", "QPARAM" };
char* tform[]={"1D"  , "4D"     };
char* tunit[]={"s"   , ""       };

int time_col=1;
int q_col=2;

int i;

#ifdef DEBUG
printf("createAttOut: start\n");
#endif

/********************************
* allocate memory for structure *
********************************/
att=(ATTOUT*)malloc(sizeof(ATTOUT));

/*******************
* record file name *
*******************/
att->name=(char*)malloc(sizeof(char)*strlen(filename)+1);
strcpy(att->name,filename);

/********************************************
* open the FITS file and create primary HDU *
********************************************/
fits_create_file(&(att->fp),filename,&status);
fits_create_img(att->fp,8,0,NULL,&status);

checkAttOutFITSerrors(status,"creating",att);




/****************************
* create bintable extension *
****************************/
fits_create_tbl(att->fp,BINARY_TBL,0l,tfields, ttype, tform, tunit,
                "ATTITUDE",&status);

att->time_col=time_col;
att->q_col=q_col;



/*******************
* allocate buffers *
*******************/
fits_get_rowsize(att->fp,&(att->nbuffer),&status);

att->time=(double*)malloc(sizeof(double)*att->nbuffer);

att->q=(double**)malloc(sizeof(double*)*att->nbuffer);
*(att->q)=(double*)malloc(sizeof(double)*att->nbuffer*4);
for(i=1;i<att->nbuffer;++i) {
    att->q[i]=att->q[i-1]+4;
}

att->row=0;
att->file_row=1;

/*******************
* check for errors *
*******************/
checkAttOutFITSerrors(status,"creating bintable in",att);

return(att);
}

/*****************************************************************************
******************************************************************************
* write the TELESCOP keyword in the current and primary HDUs of the file.
* This is not a terribly efficient routine, but that doesn't matter 
* if it is called only once or twice.
*****************************************************************************/
void setAttOutMission(ATTOUT* att, char* mission) {

int status=0;

int hdu;
int hdutype;

/**********************
* record mission name *
**********************/
fits_write_key_str(att->fp,"TELESCOP",mission,NULL,&status);
if(status==KEY_NO_EXIST) {
    /************************************************
    * keyword doesn't exist yet, so write a new one *
    ************************************************/
    status=0;
    fits_write_key_str(att->fp,"TELESCOP",mission,"Mission name",&status);
}
checkAttOutFITSerrors(status,"writing TELESCOP keyword to",att);

/*************************************************
* remember current HDU and go to the primary HDU *
*************************************************/
fits_get_hdu_num(att->fp,&hdu);
fits_movabs_hdu(att->fp,1/* primary HDU*/,&hdutype,&status);
checkAttOutFITSerrors(status,"moving to primary HDU in",att);

/**********************
* record mission name *
**********************/
fits_write_key_str(att->fp,"TELESCOP",mission,NULL,&status);
if(status==KEY_NO_EXIST) {
    /************************************************
    * keyword doesn't exist yet, so write a new one *
    ************************************************/
    status=0;
    fits_write_key_str(att->fp,"TELESCOP",mission,"Mission name",&status);
}
checkAttOutFITSerrors(status,"writing TELESCOP keyword to",att);

/****************************
* move back to original HDU *
****************************/
fits_movabs_hdu(att->fp,hdu,&hdutype,&status);
checkAttOutFITSerrors(status,"changing HDUs in",att);

}/* end of setAttOutMission function */





/*****************************************************************************
******************************************************************************
* write a single row to the attitude file via the buffers
*****************************************************************************/
void addAttOutRow(ATTOUT* att, double time, QUAT* q ) {


#ifdef DEBUG
printf("addAttOutRow: start\n");
printf("addAttOutRow: row=%d\n",att->row);
#endif


att->time[att->row]=time;

#ifdef DEBUG
printf("addAttOutRow: time=%g stored\n",att->time[att->row]);
#endif

att->q[att->row][0]=q->p[0];
att->q[att->row][1]=q->p[1];
att->q[att->row][2]=q->p[2];
att->q[att->row][3]=q->p[3];


#ifdef DEBUG
printf("addAttOutRow: q=(%g %g %g%g) stored\n",
att->q[att->row][0],
att->q[att->row][1],
att->q[att->row][2],
att->q[att->row][3]);
#endif


++(att->row);
if(att->row >= att->nbuffer ) flushAttOut(att);

} /* end of addAttOutRow function */


/*****************************************************************************
******************************************************************************
* write the data in the ATTOUT buffers to the file
*****************************************************************************/
void flushAttOut(ATTOUT* att) {

int status=0;
long new_nbuffer;

#ifdef DEBUG
printf("flushAttOut: start\n");
#endif
    


/*****************
* write the data *
*****************/
fits_write_col_dbl(att->fp,att->time_col,att->file_row,1l,att->row,
                   att->time,&status);

fits_write_col_dbl(att->fp,att->q_col   ,att->file_row,1l,4*att->row,
                   att->q[0],&status);

/**********************
* adjust the counters *
**********************/
att->file_row += att->row;
att->row=0;

/********************************************
* check if this is still a good buffer size *
********************************************/
fits_get_rowsize(att->fp,&new_nbuffer,&status);
if(new_nbuffer<att->nbuffer) att->nbuffer=new_nbuffer;

/*******************
* check for errors *
*******************/
checkAttOutFITSerrors(status,"writing data to",att);

#ifdef DEBUG
printf("flushAttOut: done\n");
#endif

} /* end of flushAttOut function */


/*****************************************************************************
******************************************************************************
* do everything to close an output attitude except actually closing the
* FITS file. The ATTOUT buffers are flushed, the FITSIO buffers are flushed,
* the NASXIS2 keyword is updated, and the att structure is destroyed.
* A pointer to the fitsfile structurte is returned so that
* another application can read this file. This is useful for memory
* resident files where closing the fistfile deletes the "file".
* it can also save some overhead of closing and reopening a regular disk file.
*****************************************************************************/
fitsfile* finishAttOut(ATTOUT* att) {

int status=0;

fitsfile* fp;

fp=att->fp;

/******************************************
* flush any remaining data in the buffers *
******************************************/
flushAttOut(att);

/****************************
* update the NAXIS2 keyword *
****************************/
fits_modify_key_lng(fp,"NAXIS2",att->file_row-1l,NULL,&status);

/***************************
* flush the FITSIO buffers *
***************************/
fits_flush_file(fp,&status);
checkAttOutFITSerrors(status,"finishing",att);

/************************
* destroy the structure *
************************/
free(att->name);
free(att->time);
free(att->q[0]);
free(att->q);

free(att);

return(fp);

}



/*****************************************************************************
******************************************************************************
* close an output attitude file and destroy the corresponding ATTOUT structure
* note this function must be called or the FITSIO buffers may not
* be flushed and the file may not be complete.
* Note also that mem:// type FITS files are destroyed when the fitsfile
* structure is closed, so for those you probably want to use finishAttOut
* instead.
*****************************************************************************/
void closeAttOut(ATTOUT* att) {

int status=0;
fitsfile* fp;

/****************************************
* do everything but close the FITS file *
****************************************/
fp=finishAttOut(att);

/*****************
* close the file *
*****************/
fits_close_file(fp,&status);
if(status) {
    fits_report_error(stderr,status);
    exit(status);
}

} /* end of closeAttOut function */

/*****************************************************************************
******************************************************************************
* Handle CFITSIO errors
*****************************************************************************/
void checkAttOutFITSerrors(int status, char* doing, ATTOUT* att)
{

   if(!status) return;

   fprintf(stderr,"FITSIO error while %s file %s:\n",doing,att->name);
   fits_report_error(stderr,status);

   exit(status);

}

