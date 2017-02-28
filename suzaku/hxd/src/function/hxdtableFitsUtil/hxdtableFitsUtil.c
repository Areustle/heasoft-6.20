/*
   hxdtableFitsUtil
     v0.0.5 first version
   
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "bnk.h"
#include "fitsio.h"
#include "hxdtableFitsUtil.h"

#define FILELIST_MAXLEN 1024

static struct {
  char filenames[HXD_TABLE_FITS_FILELIST_MAXNUM][FILELIST_MAXLEN];
  int nhdu[HXD_TABLE_FITS_FILELIST_MAXNUM];
  int filenum;  
}com;

static struct {
  int extnum;
  int ifile;
  int colnum;
  int typecode;
  long repeat;
} *col;

static char *pname = "hxdtableFitsUtil";

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
	if( *filenum > HXD_TABLE_FITS_FILELIST_MAXNUM ){
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
    
  return 0;
  
}

void hxdtableFits_Init( char *filelist, HxdTableFits *fits, int *istat){
  
  int i;
  
  if( filelist_to_filenames ( filelist, &com.filenum ) ){
    *istat = ANL_NG;
    return;
  }
  
  for( i=0;i<com.filenum;i++ ){
    
    fits_open_file( &fits->fptr[i], com.filenames[i], READONLY, istat);
    if( *istat ){
      fprintf(stderr, "%s: fits_open_file('%s') failed (%d)\n",
	      pname, com.filenames[i], *istat);
      return;
    } else {
      fits_get_num_hdus( fits->fptr[i], &com.nhdu[i], istat);
    } if ( *istat ) {
      fprintf(stderr, "%s: fits_get_num_hdus('%s') failed (%d)\n",
	      pname, com.filenames[i], *istat);
      return ;
    }
    
  }
  
}


void hxdtableFits_col_register( char *colname, HxdTableFits *fits,
			       int *index, int *istat){

  static id=0;
  
  char get_colname[64];
  
  int hdutype;
  int casesen = TRUE;
  long width;
  int colnum;  
  
  int ifits;
  int hdu;
  
  for( ifits=0;ifits<com.filenum;ifits++ ){
    
    for( hdu=1;hdu<com.nhdu[ifits];hdu++ ){
      
      fits_movabs_hdu( fits->fptr[ifits], hdu+1, &hdutype, istat );
      if ( *istat ) {
	fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
		pname, *istat);
	return;
      }
      
      fits_get_colname( fits->fptr[ifits], casesen, colname, get_colname,
		       &colnum, istat );
      
      if ( *istat ) {
	if ( *istat == COL_NOT_FOUND ) {
	  *istat = 0;
	  continue;
	} else if ( *istat ){
	  fprintf(stderr, "%s: fits_get_colname('%s') failed (%d)\n",
		  pname, colname, *istat);
	  return;
	}
      } else {
	
	col = realloc(col, (id+1)*sizeof(*col));
	if(col == NULL){
	  fprintf(stderr, "%s: col realloc failed\n", pname);
	  *istat = ANL_NG;
	  return;
	}
	
	col[id].colnum = colnum;
	col[id].extnum = hdu+1;
	col[id].ifile = ifits;
	*index = id;
	
	fits_get_coltype( fits->fptr[ifits], colnum, &(col[id].typecode),
			 &(col[id].repeat), &width, istat );
	
	if( *istat ){
	  fprintf(stderr, "%s: fits_get_coltype failed (%d)\n",
		  pname, *istat );
	  return;
	}
	
	/*if( !strcmp(colname, get_colname) )*/
	/*printf("%s %s %d\n",colname, get_colname, hdu+1);*/ 
	
	id++;
	
	return;
      }
    }
  }
  
  fprintf(stderr, "%s: Column '%s' not found\n", pname, colname );
  
  *istat = COL_NOT_FOUND;
  
}

void hxdtableFits_get_col_int( HxdTableFits *fits, char *colname, int index,
                              int irow, int *value, int *istat ){
  
  int hdutype;
  int casesen = TRUE;
  int anynul;
  
  long firstelem = 1;
  
  fits_movabs_hdu( fits->fptr[col[index].ifile], col[index].extnum, &hdutype,
		  istat );
  if ( *istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
	    pname, *istat);
    return;
  } 
  
  if( col[index].typecode == TINT ){
    int nulval = 0;
    fits_read_col_int( fits->fptr[col[index].ifile], col[index].colnum,
		      (long)irow, firstelem, col[index].repeat, nulval,
		      value, &anynul, istat );
    
    if( *istat ){
      fprintf(stderr, "%s: fits_read_col_int failed (%d)\n",
	      pname, *istat );
    }
    return;
  } else if( col[index].typecode == TSHORT ){
    short nulval = 0;
    short *data;
    int i;
    
    data = malloc( sizeof(short)*col[index].repeat );
    if( data == NULL){
      fprintf(stderr, "%s: malloc failed \n", pname );
      *istat = ANL_NG;
      return;
    } else {
      fits_read_col_sht( fits->fptr[col[index].ifile], col[index].colnum,
			(long)irow, firstelem, col[index].repeat, nulval,
			data, &anynul, istat );
    }
    if( *istat ){
      fprintf(stderr, "%s: fits_read_col_sht failed (%d)\n",
	      pname, *istat);
      return;
    }
    
    for (i=0;i<col[index].repeat;i++){
      value[i]=data[i];
    }
    return;
  } else if( col[index].typecode == TBYTE ){
    unsigned char nulval = 0;
    unsigned char  *data;
    int i;
    
    data = malloc( sizeof(char)*col[index].repeat );
    if( data == NULL){
      fprintf(stderr, "%s: malloc failed \n", pname );
      *istat = ANL_NG;
      return;
    } else {
      fits_read_col_byt( fits->fptr[col[index].ifile], col[index].colnum,
			(long)irow, firstelem, col[index].repeat, nulval,
			data, &anynul, istat );
    }
    if( *istat ){
      fprintf(stderr, "%s: fits_read_col_sht failed (%d)\n",
	      pname, *istat);
      return;
    }
    
    for (i=0;i<col[index].repeat;i++){
      value[i]=data[i];
    }
    return;
  } else {
    fprintf(stderr, "%s: double type mismatch \n", pname );
    *istat = ANL_NG;
    return;
  }
  
}

void hxdtableFits_get_col_dbl( HxdTableFits *fits, char *colname, int index,
			      int irow, double *value, int *istat ){
  
  int hdutype;
  int casesen = TRUE;
  int anynul;
  
  long firstelem = 1;
  
  fits_movabs_hdu( fits->fptr[col[index].ifile], col[index].extnum, &hdutype,
		  istat );
  if ( *istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
	    pname, *istat);
    return;
  }
  
  if( col[index].typecode == TDOUBLE ){
    double nulval = 0.0;
    fits_read_col_dbl( fits->fptr[col[index].ifile], col[index].colnum,
		      (long)irow, firstelem, col[index].repeat, nulval,
		      value, &anynul, istat );
    
    if( *istat ){
      fprintf(stderr, "%s: fits_read_col_dbl failed (%d)\n",
		    pname, *istat );
    }
    return;
  } else if( col[index].typecode == TFLOAT ){
    float nulval = 0.0;
    float *data;
    int i;
    
    data = malloc( sizeof(float)*col[index].repeat );
    if( data == NULL){
      fprintf(stderr, "%s: malloc failed \n", pname );
      *istat = ANL_NG;
      return;
    } else {
      fits_read_col_flt( fits->fptr[col[index].ifile], col[index].colnum,
			(long)irow, firstelem, col[index].repeat, nulval,
			data, &anynul, istat );
    }
    if( *istat ){
      fprintf(stderr, "%s: fits_read_col_flt failed (%d)\n",
	      pname, *istat);
      return;
    }
    
    for (i=0;i<col[index].repeat;i++){
      value[i]=data[i];
    }
    return;	
  } else {
    fprintf(stderr, "%s: double type mismatch \n", pname );
    *istat = ANL_NG;
    return;
  }
  
}

void hxdtableFits_get_key_str( HxdTableFits *fits, char *keyname, char *value,
			      int *istat  ){
  
  char comment[81];
  int hdutype;
  int casesen = TRUE;
  
  int ifits;
  int hdu;
  
  for( ifits=0;ifits<com.filenum;ifits++ ){
    
    for( hdu=1;hdu<com.nhdu[ifits];hdu++ ){
      
      fits_movabs_hdu( fits->fptr[ifits], hdu+1, &hdutype, istat );
      if ( *istat ) {
	fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
		pname, *istat);
	return;
      }
      
      fits_read_key_str( fits->fptr[ifits], keyname, value, comment, istat);
      
      if ( *istat ) {
	if ( *istat == KEY_NO_EXIST ) {
	  *istat = 0;
	  continue;
	} else if ( *istat ){
	  fprintf(stderr, "%s: fits_read_key_str('%s') failed (%d)\n",
		  pname, keyname, *istat);
	  return;
	}
      } else {
	return;
      }
    }
  }
  
  *istat = KEY_NO_EXIST;
  
}

void hxdtableFits_get_key_lng( HxdTableFits *fits, char *keyname, long *value,
			      int *istat ){
  
  char comment[81];
  int hdutype;
  int casesen = TRUE;
  
  int ifits;
  int hdu;
  
  for( ifits=0;ifits<com.filenum;ifits++ ){
    
    for( hdu=1;hdu<com.nhdu[ifits];hdu++ ){
      
      fits_movabs_hdu( fits->fptr[ifits], hdu+1, &hdutype, istat );
      if ( *istat ) {
	fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
		pname, *istat);
	return;
      }
      
      fits_read_key_lng( fits->fptr[ifits], keyname, value, comment, istat);
      
      if ( *istat ) {
	if ( *istat == KEY_NO_EXIST ) {
	  *istat = 0;
	  continue;
	} else if ( *istat ){
	  fprintf(stderr, "%s: fits_read_key_lng('%s') failed (%d)\n",
		  pname, keyname, *istat);
	  return;
	}
      } else {
	return;
      }
    }
  }
  
  *istat = KEY_NO_EXIST;
  
}

void hxdtableFits_get_key_dbl( HxdTableFits *fits, char *keyname,
			      double *value, int *istat ){
  
  char comment[81];
  int hdutype;
  int casesen = TRUE;
  
  int ifits;
  int hdu;
  
  for( ifits=0;ifits<com.filenum;ifits++ ){
    
    for( hdu=1;hdu<com.nhdu[ifits];hdu++ ){
      
      fits_movabs_hdu( fits->fptr[ifits], hdu+1, &hdutype, istat );
      if ( *istat ) {
	fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
		pname, *istat);
	return;
      }
      
      fits_read_key_dbl( fits->fptr[ifits], keyname, value, comment, istat);
      
      if ( *istat ) {
	if ( *istat == KEY_NO_EXIST ) {
	  *istat = 0;
	  continue;
	} else if ( *istat ){
	  fprintf(stderr, "%s: fits_read_key_dbl('%s') failed (%d)\n",
		  pname, keyname, *istat);
	  return;
	}
      } else {
	return;
      }
    }
  }
  
  *istat = KEY_NO_EXIST;
  
}
