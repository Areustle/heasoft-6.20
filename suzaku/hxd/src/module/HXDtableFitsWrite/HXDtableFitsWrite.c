/*
   HXDtableFitsWrite ver 0.0.1 created by M. Sugiho
                     ver 0.0.2 fisrt version
		     
		     ver 0.0.7 add dbl_key_decimals
		               input file format changed
			         when using double in <KEYWOD> 
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "atFunctions.h"
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "cli.h"
#include "fitsio.h"

#define MAX_EXTENTION_NUM 8
#define MAX_COLUMN_NUN    128
#define MAX_KEY_NUM       1024

static struct {
  FILE *fp;
  fitsfile *fitsfp;
  int ikey;
  int icol;
  int next;
} com;

static struct {
  int tfields;
  char ttype[MAX_COLUMN_NUN][32];
  char tform[MAX_COLUMN_NUN][8];
  char tunit[MAX_COLUMN_NUN][16];
  char comment[MAX_COLUMN_NUN][128];
  int format[MAX_COLUMN_NUN];
  int iarray[MAX_COLUMN_NUN];
  char extname[32];
  int nkey;
  long nrow;
  int ncol;
}ext[MAX_EXTENTION_NUM];

static struct {
  int format;
  int index;
  char keyname[8];
  char comment[128];
}*key;

static struct {    
  int index;
}*col;

static double        *dbl_data;
static float         *flt_data;
static int           *int_data;
static short         *sht_data;
static unsigned char *byt_data;

static double dbl_key[MAX_KEY_NUM];
static int dbl_key_decimals[MAX_KEY_NUM];
static char str_key[MAX_KEY_NUM][128];
static long lng_key[MAX_KEY_NUM];


static void pre_table_file_read( int *istat ){
    
  int key_flag = 0;
  int col_flag = 0;
  int val_flag = 0;
  
  int ch;
  
  ch = ' ';
  while ( EOF != ch ) {
    int ic, iw;
    char line[65536];
    char *word[2000];
    
    /* read one line */
    ic = iw = 0;
    word[iw++] = line;
    for (;;) {
      ch = fgetc(com.fp);
      if ( EOF == ch || '\n' == ch ) {
	line[ic] = '\0';
	break;
      }
      if ( ic < sizeof(line)-1 ) {
	line[ic] = ch;
	if ( '\t' == ch ) {
	  line[ic] = '\0';
	  if ( iw < sizeof(word)/sizeof(*word) ) {
	    word[iw] = line + ic + 1;
	    iw++;
	  }
	}
	ic++;
      }
    }
    
    while ( iw < sizeof(word)/sizeof(*word) ) {
      word[iw] = "";
      iw++;
    }
    
    if ( word[0][0] ) {      
      if( !strcmp(word[0],"<KEYWORD>") ){
	key_flag ++;
	continue;
      } else if ( !strcmp(word[0],"<COLUMN>") ){		
	col_flag ++;
	continue;
      } else if ( !strcmp(word[0],"<VALUE>") ){		
	val_flag ++;
	continue;
      }      
    }
    
  }
  
  fclose(com.fp);
  
  if( !key_flag ){
    fprintf( stderr, "<KEYWORD> not found\n" );
    *istat = -1;
  }        
  if( col_flag != key_flag-1 ){	
    fprintf( stderr, "input file invalid form: not in order\n" );
    *istat = -1;
  }
  if( val_flag != key_flag-1 ){
    fprintf( stderr, "input file invalid form: not in order\n" );
    *istat = -1;
  }
  
}


static void table_file_read( int *istat ){
  
  int key_flag = 0;
  int col_flag = 0;
  int val_flag = 0;
  
  int dbl_key_index = 0;
  int str_key_index = 0;
  int lng_key_index = 0;
  
  int dbl_data_index = 0;
  int flt_data_index = 0;
  int int_data_index = 0;
  int sht_data_index = 0;
  int byt_data_index = 0;
  
  int ch;
  
  ch = ' ';
  while ( EOF != ch ) {
    int ic, iw;
    char line[65536];
    char *word[2000];
    
    /* read one line */
    ic = iw = 0;
    word[iw++] = line;
    for (;;) {
      ch = fgetc(com.fp);
      if ( EOF == ch || '\n' == ch ) {
	line[ic] = '\0';
	break;
      }
      if ( ic < sizeof(line)-1 ) {
	line[ic] = ch;
	if ( '\t' == ch ) {
	  line[ic] = '\0';
	  if ( iw < sizeof(word)/sizeof(*word) ) {
	    word[iw] = line + ic + 1;
	    iw++;
	  }
	}
	ic++;
      }
    }
    
    while ( iw < sizeof(word)/sizeof(*word) ) {
      word[iw] = "";
      iw++;
    }
    
    if ( word[0][0] ) {
      if( !strcmp(word[0],"<KEYWORD>") ){
	com.next ++;
	key_flag = 1;
	col_flag = 0;
	val_flag = 0;
	ext[com.next].nkey = 0;
	continue;
      } else if ( !strcmp(word[0],"<COLUMN>") ){		
	key_flag = 0;
	col_flag = 1;
	val_flag = 0;
	ext[com.next].tfields = 0;
	ext[com.next].ncol = 0;
	if( word[1][0] ){		    
	  strcpy( ext[com.next].extname, word[1] );
	} else {
	  fprintf( stderr,
		  "input file invalid form: not found EXTNAME\n" );
	  *istat = -1;
	  return;
	}
	continue;
      } else if ( !strcmp(word[0],"<VALUE>") ){		
	key_flag = 0;
	col_flag = 0;
	val_flag = 1;
	ext[com.next].nrow = 0;
	continue;
      }
    } else {
      continue;
    }
    
    if ( key_flag ){
      if( !word[1][0]){
	fprintf( stderr, "input file invalid form in <KEYWORD>\n" );
	*istat = -1;
	return;
      }
      if( !word[2][0] ){
	fprintf( stderr, "input file invalid form in <KEYWORD>\n" );
	*istat = -1;
	return;
      }
      
      key = realloc(key, sizeof(*key)*(com.ikey+1));
      
      strcpy(key[com.ikey].keyname, word[0]);
      
      if( !strcmp(word[1],"double") ){
	key[com.ikey].format = TDOUBLE;
	dbl_key_decimals[dbl_key_index] = atoi(word[2]);
	if( !word[3][0] ){
	  fprintf( stderr, "input file invalid form in <KEYWORD>\n" );
	  *istat = -1;
	  return;
	}
	dbl_key[dbl_key_index] = atof(word[3]);
	if( word[4][0] ){
	  strcpy(key[com.ikey].comment, word[4]);
	} else {
	  strcpy(key[com.ikey].comment, "");
	}
	key[com.ikey].index = dbl_key_index++;
      } else if ( !strcmp(word[1],"string") ){
	key[com.ikey].format = TSTRING;
	strcpy(str_key[str_key_index], word[2]);
	if( word[3][0] ){
	  strcpy(key[com.ikey].comment, word[3]);
	} else {
	  strcpy(key[com.ikey].comment, "");
	}
	key[com.ikey].index = str_key_index++;
      } else if ( !strcmp(word[1],"long") ){
	key[com.ikey].format = TLONG;
	lng_key[lng_key_index] = atoi(word[2]);
	if( word[3][0] ){
	  strcpy(key[com.ikey].comment, word[3]);
	} else {
	  strcpy(key[com.ikey].comment, "");
	}
	key[com.ikey].index = lng_key_index++;
      } else {
	fprintf( stderr, "input type %s undefined \n", word[1] );
	*istat = -1;
	return;
      }
      
      ext[com.next].nkey ++;
      com.ikey ++;
      
    } else if ( col_flag ){
      
      if( !word[1][0]){
	fprintf( stderr, "input file invalid form in <COLUMN>\n" );
	*istat = -1;
	return;
      }
      if( !word[2][0] ){
	fprintf( stderr, "input file invalid form in <COLUMN>\n" );
	*istat = -1;
	return;
      }
      
      if( word[3][0] ){
	strcpy(ext[com.next].tunit[ext[com.next].tfields], word[3]);
      } else {
	strcpy(ext[com.next].tunit[ext[com.next].tfields], "");
      }
      
      if( word[4][0] ){
	strcpy(ext[com.next].comment[ext[com.next].tfields], word[4]);
      } else {
	strcpy(ext[com.next].comment[ext[com.next].tfields], "");
      }
      
      strcpy(ext[com.next].ttype[ext[com.next].tfields], word[0]);
      ext[com.next].iarray[ext[com.next].tfields] = atoi(word[2]);
      strcpy(ext[com.next].tform[ext[com.next].tfields], word[2]);
      ext[com.next].ncol += ext[com.next].iarray[ext[com.next].tfields];
      
      if( !strcmp(word[1],"double") ){
	ext[com.next].format[ext[com.next].tfields] = TDOUBLE;
	strcat(ext[com.next].tform[ext[com.next].tfields], "D");
      } else if( !strcmp(word[1],"float") ){
	ext[com.next].format[ext[com.next].tfields] = TFLOAT;	
	strcat(ext[com.next].tform[ext[com.next].tfields], "E");
      } else if( !strcmp(word[1],"int") ){
	ext[com.next].format[ext[com.next].tfields] = TINT;	
	strcat(ext[com.next].tform[ext[com.next].tfields], "J");
      } else if( !strcmp(word[1],"short") ){
	ext[com.next].format[ext[com.next].tfields] = TSHORT;	
	strcat(ext[com.next].tform[ext[com.next].tfields], "I");
      } else if( !strcmp(word[1],"char") ){
	ext[com.next].format[ext[com.next].tfields] = TBYTE;	
	strcat(ext[com.next].tform[ext[com.next].tfields], "B");
      } else {
	fprintf( stderr, "input type %s undefined \n", word[1] );
	*istat = -1;
	return;
      }
      
      ext[com.next].tfields ++;
      
    } else if ( val_flag ){
      
      int icol = 0;
      int  i,j;
      
      for(i=1;i<ext[com.next].ncol;i++){
	/*(printf("%s %d\n",word[i],i);*/
	if( !word[i][0] ){		  
	  fprintf( stderr, "input file invalid form :[col num] \n" );
	  *istat = -1;
	  return; 
	}
      }
      
      for(i=0;i<ext[com.next].tfields;i++){
	for(j=0;j<ext[com.next].iarray[i];j++){
	  col = realloc( col, sizeof(*col)* (com.icol+1) );
	  
	  if(ext[com.next].format[i] == TDOUBLE){
	    dbl_data = realloc( dbl_data, sizeof(double)
			       * (dbl_data_index + 1) );
	    dbl_data[dbl_data_index] = atof(word[icol]);
	    col[com.icol].index = dbl_data_index ++;
	  } else if(ext[com.next].format[i] == TFLOAT){
	    flt_data = realloc( flt_data, sizeof(float)
			       * (flt_data_index + 1) );
	    flt_data[flt_data_index] = atof(word[icol]);
	    col[com.icol].index = flt_data_index++;
	  } else if(ext[com.next].format[i] == TINT){
	    int_data = realloc( int_data, sizeof(int)
			       * (int_data_index + 1) );
	    int_data[int_data_index] = atoi(word[icol]);
	    col[com.icol].index = int_data_index++;	    
	  } else if(ext[com.next].format[i] == TSHORT){
	    sht_data = realloc( sht_data, sizeof(short)
			       * (sht_data_index + 1) );
	    sht_data[sht_data_index] = atoi(word[icol]);
	    col[com.icol].index = sht_data_index++;
	  } else if(ext[com.next].format[i] == TBYTE){
	    byt_data = realloc( byt_data, sizeof(char)
			       * (byt_data_index + 1) );
	    byt_data[byt_data_index] = atoi(word[icol]);
	    col[com.icol].index = byt_data_index++;
	  }
	  
	  icol++;
	  com.icol ++;
	}
      }
      
      ext[com.next].nrow ++;
      
    }
    
  } /* while ( EOF != ch ) */
  
  fclose(com.fp);
  
}


static key_write ( int extnum, int *istat ){
  
  int i;
  /*int decimals = 16;*/
  
  for( i=com.ikey; i<com.ikey+ext[extnum].nkey ;i++ ){    
    if( key[i].format == TDOUBLE ){
      fits_write_key_dbl ( com.fitsfp, key[i].keyname,
			  dbl_key[key[i].index],
			  dbl_key_decimals[key[i].index],
			  key[i].comment ,istat );
    } else if ( key[i].format == TSTRING ) {
      fits_write_key_str ( com.fitsfp, key[i].keyname,
			  str_key[key[i].index], key[i].comment ,istat );
    } else if ( key[i].format == TLONG ) {
      fits_write_key_lng ( com.fitsfp, key[i].keyname,
			  lng_key[key[i].index], key[i].comment ,istat );  
    }
    
    if( *istat ){
      fprintf(stderr, "fits_write_key failed (%d)\n", istat);
      return;
    }    
  }
  
  com.ikey += ext[extnum].nkey;
  
}


static void primary_header_write( int *istat ){
    
  int bitpix = 8;
  int naxis = 0;
  long naxes = 0 ;
  int extnum = 0;
  
  fits_create_img(com.fitsfp, bitpix, naxis, &naxes, istat);
  if ( *istat ) {
    fprintf(stderr, "fits_create_img failed (%d)\n", *istat);
    return ;
  }
  
  key_write ( extnum, istat );
  
}

static void extention_write( int extnum, int *istat ){
  
  int tbltype = BINARY_TBL;
  long naxis2 = 0;
  int i,j;
  char **ttype, **tform, **tunit;
  
  ttype = malloc(sizeof(char *)*ext[extnum].tfields);
  tform = malloc(sizeof(char *)*ext[extnum].tfields);
  tunit = malloc(sizeof(char *)*ext[extnum].tfields);
  if ( ttype == NULL || tform == NULL || tunit == NULL ){
    fprintf(stderr, "malloc failed\n");
    *istat = -1;
    return;
  }
  
  for(i=0; i<ext[extnum].tfields;i++){
    char **ttp = ttype + i;
    char **tfp = tform + i;
    char **tup = tunit + i;
    
    *ttp = ext[extnum].ttype[i];
    *tfp = ext[extnum].tform[i];
    *tup = ext[extnum].tunit[i];    
  }    
  
  fits_create_tbl(com.fitsfp, tbltype, naxis2, ext[extnum].tfields,
		  ttype, tform, tunit, ext[extnum].extname, istat);
  if ( *istat ) {
    fprintf(stderr, "fits_create_tbl failed (%d)\n", *istat);
    return ;
  }
  
  key_write ( extnum, istat );
  if ( *istat ){
    return;
  }
  
  for(j=0; j<ext[extnum].nrow; j++){
    for(i=0;i<ext[extnum].tfields;i++){
      
      if(ext[extnum].format[i] == TDOUBLE){
	fits_write_col_dbl(com.fitsfp, i+1, j+1, 1,
			   (long) ext[extnum].iarray[i],
			   &dbl_data[col[com.icol].index], istat );
      } else if(ext[extnum].format[i] == TFLOAT){
	fits_write_col_flt(com.fitsfp, i+1, j+1, 1,
			   (long) ext[extnum].iarray[i],
			   &flt_data[col[com.icol].index], istat );
      } else if(ext[extnum].format[i] == TINT){
	fits_write_col_int(com.fitsfp, i+1, j+1, 1,
			   (long) ext[extnum].iarray[i],
			   &int_data[col[com.icol].index], istat );
	int_data[col[com.icol].index];
      } else if(ext[extnum].format[i] == TSHORT){
	fits_write_col_sht(com.fitsfp, i+1, j+1, 1,
			   (long) ext[extnum].iarray[i],
			   &sht_data[col[com.icol].index], istat );
	sht_data[col[com.icol].index];
      } else if(ext[extnum].format[i] == TBYTE){
	fits_write_col_byt(com.fitsfp, i+1, j+1, 1,
			   (long) ext[extnum].iarray[i],
			   &byt_data[col[com.icol].index], istat );
	byt_data[col[com.icol].index];
      }
      
      if( *istat ){
	fprintf(stderr, "fits_write_col faild (%d)\n", *istat);
      }
      
      com.icol += ext[extnum].iarray[i];      
    }
  }    
  
  fits_modify_key_lng(com.fitsfp, "NAXIS2", ext[extnum].nrow, "&", istat);
  if ( *istat ) {
    fprintf(stderr, "fits_update_key NAXIS2 failed (%d)\n",
	    *istat);
    return ;
  }
    
}


int
main(int argc, char **argv){
    
  int istat = 0;
  int i;
  
  if(3 != argc){
    fprintf(stderr, "usage : %s input-file output-file \n" , argv[0] );
    return -1;
  }
  
  com.fp = fopen(argv[1], "r");
  if ( NULL == com.fp ) {
    fprintf(stderr, "%s : file open faild\n",argv[1]);
    return -1;
  }
  
  com.ikey = com.icol = 0;
  com.next = -1;
  
  pre_table_file_read ( &istat );
  if( istat ){
    return -1;
  }
  
  com.fp = fopen(argv[1], "r");
  if ( NULL == com.fp ) {
    fprintf(stderr, "%s : file open faild\n",argv[1]);
    return -1;
  }
  
  table_file_read( &istat );
  if( istat ){
    return -1;
  }
  
  fits_create_file(&com.fitsfp, argv[2], &istat);
  if ( istat ) {
    fprintf(stderr, "%s : fits_create_file failed (%d)\n", argv[2], istat);
    return -1;
  }
  
  com.ikey = com.icol = 0;
  
  primary_header_write ( &istat );
  if( istat ){
    return -1;
  }
  
  for( i=1; i<=com.next; i++ ){
    extention_write ( i, &istat );
    if( istat ){
      return -1;
    }
  }
  
  fits_close_file(com.fitsfp, &istat);
  if ( istat ) {
    fprintf(stderr, "fits_close_file failed (%d)\n", istat);
    return -1;
  }
  
  fprintf(stderr, "%s made from %s \n", argv[2], argv[1]);
    
  return 0;
  
}
