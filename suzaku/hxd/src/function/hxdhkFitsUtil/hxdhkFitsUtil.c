#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "bnk.h"
#include "fitsio.h"

#include "HXD.h"

#include "hxdhkFitsUtil.h"

#define FILELIST_MAXLEN 1024

static char *pname = "hxdhkFitsUtil";

typedef struct {  
  int nrow;
  int filenum;
  int eventtype;
} HDU;

typedef struct {
  double time;
  int irow;
  char filenum;
  short eventtype;
} ROW;

static struct {
  char filenames[HXD_HK_FITS_FILELIST_MAXNUM][FILELIST_MAXLEN];
  ROW *row;
  HDU *hdu;
  int *index;
  int rownum;
  int hdunum;
  int sort_flag;
} com;

static int filelist_to_filenames ( char *filelist, int *filenum ){
  
  char line[FILELIST_MAXLEN];
  FILE *fp;
  int i=0;
  
  *filenum = 0;
  
  fp = fopen ( filelist , "r");
  if (fp == NULL) {
    fprintf (stderr, "%s : cannot open filelist %s\n",
	     pname, filelist);
    return ANL_NG;
  }
  
  while (fgets (line, FILELIST_MAXLEN, fp) != NULL){
    
    (*filenum)++;
    if( *filenum > HXD_HK_FITS_FILELIST_MAXNUM ){
      fprintf (stderr, "%s : over the max number of filelist \n",
	       pname );
      return ANL_NG;
    }
  }
  
  fclose(fp);
  fp = fopen ( filelist , "r");
  if (fp == NULL) {
    fprintf (stderr, "%s : cannot open filelist %s\n",
	     pname, filelist);
    return ANL_NG;
  }
  
  while (fgets (line, FILELIST_MAXLEN, fp) != NULL){
    i++;
    if( line[strlen(line)-1]== '\n'){
      line[strlen(line)-1]='\0';
    }
    if( line[0]=='\0'){
      i--;
      (*filenum)--;
      continue;
    }
    strcpy(com.filenames[i-1], line);
  }
  
  return ANL_OK;
    
}


static int *ivector(long nl, long nh)
/* allocate an int vector with subscript range v[nl..nh] */
{
  int *v;
  
  v=(int *)malloc((size_t) ((nh-nl+1+1)*sizeof(int)));
  if (!v) printf("allocation failure in ivector()");
  return v-nl+1;
}


static void free_ivector(int *v, long nl, long nh)
/* free an int vector allocated with ivector() */
{
  free((char*) (v+nl-1));
}


static int sort_time(){
  
#define SWAP(a,b) itemp=(a);(a)=(b);(b)=itemp;
#define M 7
#define NSTACK 1000
  
  int i,indxt,ir=com.rownum,itemp,j,k,l=1;
  int jstack=0,*istack;
  double a;
  
  istack=ivector(1,NSTACK);
  for (j=1;j<=com.rownum;j++) com.index[j]=j;
  for (;;) {
    if (ir-l < M) {
      for (j=l+1;j<=ir;j++) {
	indxt=com.index[j];
	a=com.row[indxt].time;
	for (i=j-1;i>=1;i--) {
	  if (com.row[com.index[i]].time <= a) break;
	  com.index[i+1]=com.index[i];
	}
	com.index[i+1]=indxt;
      }
      if (jstack == 0) break;
      ir=istack[jstack--];
      l=istack[jstack--];
    } else {
      k=(l+ir) >> 1;
      SWAP(com.index[k],com.index[l+1]);
      if (com.row[com.index[l+1]].time > com.row[com.index[ir]].time) {
	SWAP(com.index[l+1],com.index[ir])
      }
      if (com.row[com.index[l]].time > com.row[com.index[ir]].time) {
	SWAP(com.index[l],com.index[ir])
      }
      if (com.row[com.index[l+1]].time > com.row[com.index[l]].time) {
	SWAP(com.index[l+1],com.index[l])
      }
      i=l+1;
      j=ir;
      indxt=com.index[l];
      a=com.row[indxt].time;
      for (;;) {
	do i++; while (com.row[com.index[i]].time < a);
	do j--; while (com.row[com.index[j]].time > a);
	if (j < i) break;
	SWAP(com.index[i],com.index[j])
      }
      com.index[l]=com.index[j];
      com.index[j]=indxt;
      jstack += 2;
      if (jstack > NSTACK) printf("NSTACK too small in indexx.");
      if (ir-i+1 >= j-l) {
	istack[jstack]=ir;
	istack[jstack-1]=i;
	ir=j-1;
      } else {
	istack[jstack]=j-1;
	istack[jstack-1]=l;
	l=i;
      }
    }
  }
  free_ivector(istack,1,NSTACK);

  return ANL_OK;
  
}


void hxdhkFits_init( char *filelistname, int sort_flag, HxdHkFits *fits,
		    int *istat){
    
  int ifits;
  int ihdu;
  int irow;

  int nhdu;
  long nrow;
  char extname[16];
  int time_colnum;
  int eventtype;
  
  int hdutype;
  int anynul;
  int casesen = TRUE;
  double nulval = 0.0;
  char comment[81];
  long firstelem = 1;
  long nelements = 1;
  
  com.rownum = 0;
  com.hdunum = 0;
  
  if( filelist_to_filenames ( filelistname, &fits->filenum ) ){
    *istat = ANL_NG;
    return;
  }
  
  for( ifits=0;ifits<fits->filenum;ifits++ ){
    
    fits_open_file( &fits->fptr[ifits], com.filenames[ifits], READONLY, istat);
    if( *istat ){
      fprintf(stderr, "%s: fits_open_file('%s') failed (%d)\n",
	      pname, com.filenames[ifits], *istat);
      return;
    } else {
      fits_get_num_hdus( fits->fptr[ifits], &nhdu, istat);
    } if ( *istat ) {
      fprintf(stderr, "%s: fits_get_num_hdus('%s') failed (%d)\n",
	      pname, com.filenames[ifits], *istat);
      return ;
    }
    
    for(ihdu=1;ihdu<nhdu;ihdu++){
      
      fits_movabs_hdu(fits->fptr[ifits], ihdu+1, &hdutype, istat );
      if ( *istat ) {
	fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
		pname, *istat);
	return;
      }
      
      fits_read_key_lng(fits->fptr[ifits], "NAXIS2", &nrow,
			comment, istat);
      if ( *istat ) {
	fprintf(stderr, " %s: fits_read_key_lng 'NAXIS2' failed (%d)\n",
		pname, *istat);
	return;
      }

      if( sort_flag == HXD_HK_FITS_SORT ){
	com.row = realloc ( com.row, (com.rownum + nrow+1) * sizeof(ROW) );
	if( com.row == NULL && nrow!=0){
	  fprintf(stderr, "%s realloc failed \n", pname);
	  *istat = ANL_NG;
	  return;
	}
      } else {
	com.hdu = realloc ( com.hdu, (com.hdunum+1) * sizeof(HDU) );
	if( com.hdu == NULL ){
	  fprintf(stderr, "%s realloc failed \n", pname);
	  *istat = ANL_NG;
	  return;
	}
      }
      
      fits_read_key_str(fits->fptr[ifits], "EXTNAME", extname, comment,
			istat);
      if ( *istat ) {
	fprintf(stderr, " %s: fits_read_key_lng 'EXTNAME' failed (%d)\n",
		pname, *istat);
	return;
      }
      
      fits_get_colnum(fits->fptr[ifits], casesen, "TIME", &time_colnum,
		      istat);
      if ( *istat ) {
	fprintf(stderr, "%s: fits_get_colnum('TIME') failed (%d)\n",
		pname, *istat);
	return;
      }
      
      if(ihdu == 1){
	if(!strcmp("HXD_HK",extname)){
	  eventtype = HXD_EVTYP_HK;
	  fits->filetype[ifits] = HXD_HK_FITS_FILETYPE_HK; }
	else if(!strcmp("HXD_STM",extname)){
	  eventtype = HXD_EVTYP_STM;
	  fits->filetype[ifits] = HXD_HK_FITS_FILETYPE_TBL; }
	else if(!strcmp("HXD_MEM_DMP",extname)){
	  eventtype = HXD_EVTYP_MEM_DMP;
	  fits->filetype[ifits] = HXD_HK_FITS_FILETYPE_DMP; }
	else if(!strcmp("HXD_SFC",extname)){
	  eventtype = HXD_EVTYP_SFC;
	  fits->filetype[ifits] = HXD_HK_FITS_FILETYPE_HST; }
	else if(!strcmp("EVENTS",extname)){

	  char datamode[16];
          
          fits_read_key_str(fits->fptr[ifits], "DATAMODE", datamode, comment,
                            istat);
          if ( *istat ) {
            fprintf(stderr, " %s: fits_read_key_lng 'DATAMODE' failed (%d)\n",
                    pname, *istat);
            return;
          }

	  if(!strcmp("TRANSIENT",datamode)){
	    
	    /*fits_get_colnum(fits->fptr[ifits], casesen, "TRN_AETIME",
			    &time_colnum, istat);
	    if ( *istat ) {
	      fprintf(stderr, 
		      "%s: fits_get_colnum('TRN_AETIME') failed (%d)\n",
		      pname, *istat);
	      return;
	    }*/
	    
	    eventtype = HXD_EVTYP_TRN;
	    fits->filetype[ifits] = HXD_HK_FITS_FILETYPE_TRN;
	    nhdu =2;
	  } else if (!strcmp("WELL",datamode)){
            eventtype = HXD_EVTYP_WEL;
            fits->filetype[ifits] = HXD_HK_FITS_FILETYPE_WEL;
            nhdu =2;
          }	  
	  
	}
	else {
	  fprintf( stderr, "%s: this file is not HXD HK file\n",pname );
	  *istat = ANL_NG; return; }
      } else {
	if(!strcmp("HXD_SYS",extname)){
	  eventtype = HXD_EVTYP_SYS; }
	else if(!strcmp("HXD_SCL",extname)){
	  eventtype = HXD_EVTYP_SCL; }
	else if(!strcmp("HXD_ACU",extname)){
	  eventtype = HXD_EVTYP_ACU; }
	else if(!strcmp("HXD_RHK",extname)){
	  eventtype = HXD_EVTYP_RHK; }
	else if(!strcmp("HXD_PPR",extname)){
	  eventtype = HXD_EVTYP_PPR; }
	else if(!strcmp("HXD_PST",extname)){
	  eventtype = HXD_EVTYP_PST; }
	else if(!strcmp("HXD_AET_HC",extname)){
	  eventtype = HXD_EVTYP_AET_HC; }
	else if(!strcmp("HXD_AET_SC",extname)){
	  eventtype = HXD_EVTYP_AET_SC; }
	else if(!strcmp("HXD_ECC_DMP",extname)){
	  eventtype = HXD_EVTYP_ECC_DMP; }
	else if(!strcmp("HXD_IO_DMP",extname)){
	  eventtype = HXD_EVTYP_IO_DMP; }
	else if(!strcmp("HXD_RECC_DMP",extname)){
	  eventtype = HXD_EVTYP_RECC_DMP; }
	else if(!strcmp("HXD_RIO_DMP",extname)){
	  eventtype = HXD_EVTYP_RIO_DMP; }
	else if(!strcmp("HXD_SFF1",extname)){
	  eventtype = HXD_EVTYP_SFF1; }
	else if(!strcmp("HXD_AFF2",extname)){
	  eventtype = HXD_EVTYP_SFF2; }
	else if(!strcmp("HXD_DLT",extname)){
	  eventtype = HXD_EVTYP_DLT; }
	else if(!strcmp("HXD_SP_PMT",extname)){
	  eventtype = HXD_EVTYP_SP_PMT; }
	else if(!strcmp("HXD_SP_PIN",extname)){
	  eventtype = HXD_EVTYP_SP_PIN; }
      }
      
      if( sort_flag == HXD_HK_FITS_SORT ){
	for(irow =0;irow<nrow;irow++){
	  fits_read_col_dbl(fits->fptr[ifits], time_colnum, irow+1,
			    firstelem, nelements, nulval,
			    &com.row[com.rownum + irow+1].time, &anynul,
			    istat);
	  if(*istat){
	    fprintf(stderr, "%s: fits_read_col_dbl('TIME') failed (%d)\n",
		    pname, *istat);
	    return;
	  }
	  
	  com.row[com.rownum + irow+1].irow = irow+1;
	  com.row[com.rownum + irow+1].eventtype = eventtype;
	  com.row[com.rownum + irow+1].filenum = (char)ifits;
	  
	}
      } else {

	com.hdu[com.hdunum].nrow = nrow;
        com.hdu[com.hdunum].filenum = (char)ifits;
        com.hdu[com.hdunum].eventtype = eventtype;
	
      }
      
      com.rownum+=nrow;
      com.hdunum++;
      
    }
    
  }
  
  if( com.rownum == 0 ){
    fprintf(stderr, "%s no column row found \n", pname);
    *istat = ANL_NG;
    return;
  }

  if( sort_flag == HXD_HK_FITS_SORT ) { 
    com.index = realloc ( com.index, (com.rownum+1) * sizeof(int) );
    if( com.index == NULL ){
      fprintf(stderr, "%s realloc failed \n", pname);
      *istat = ANL_NG;
      return;
    }
    
    if(sort_time()){
      *istat = ANL_NG;
      return;
    }
    
    com.sort_flag = HXD_HK_FITS_SORT;
    
  } else {
    com.sort_flag = HXD_HK_FITS_NO_SORT;
  }
    
  /*
  for(irow=1;irow<=com.rownum;irow++){
    printf("%f\n",com.row[com.index[irow]].time);
  }*/

  /*for(ihdu=0;ihdu<com.hdunum;ihdu++){
    printf("%d %d %x\n", com.hdu[ihdu].nrow,
	   com.hdu[ihdu].filenum,
	   com.hdu[ihdu].eventtype);
  }*/
  
    
}


void hxdhkFits_read( int *filenum, int *eventtype, int *irow, int *istat ){
  
  static int eventnum = 1;
  
  if(eventnum > com.rownum){
    *istat = ANL_NG;
    return;
  }
  if ( com.sort_flag == HXD_HK_FITS_SORT ){
    *filenum = (int)com.row[com.index[eventnum]].filenum;  
    *eventtype = (int)com.row[com.index[eventnum]].eventtype;
    *irow = com.row[com.index[eventnum]].irow;
  } else {
    
    static int i = 1;
    static int ihdu = 0;

    if(i > com.hdu[ihdu].nrow){
      ihdu++;
      i = 1;
    }
    
    while( com.hdu[ihdu].nrow == 0 && ihdu<com.hdunum ){
      ihdu++;
    }
    
    *filenum = com.hdu[ihdu].filenum;
    *eventtype = com.hdu[ihdu].eventtype;
    *irow = i;
    
    i++;
    
  }
  
  eventnum++;
  
  *istat = ANL_OK;
  
}
