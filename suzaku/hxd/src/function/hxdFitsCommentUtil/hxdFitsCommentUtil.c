#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fitsio.h>
#include <anl.h>
#include "hxdFitsCommentUtil.h"

#define FITS_KEYWORD_LENGTH 8
#define FITS_COMMENT_LENGTH 80
#define FITS_COMMENT_LENGTH_EXCEPT_KEYWROD 70

static char *pname = "hxdFitsCommentUtil";

void hxdFitsComment_write ( fitsfile *fp, int comment_num, char **keyword,
			   char **comment, int *istat){
    
  int i,j=0;
  
  int ikey;
  int icomment;
  
  char tmp_card[FITS_COMMENT_LENGTH+1];
  
  int comment_change = 0;
  
  for (ikey = 1; ; ikey++) {
    /*if ( fits_flush_file(fp, istat) ){
      fprintf(stderr, "Error doing fits_flush_file() (status=%d)\n",*istat);
      return;
    }*/
    
    if( comment_change == comment_num ){
      break;
    }
    
    if ( fits_read_record(fp, ikey, tmp_card, istat) ) {
      fprintf(stderr, "%s: comment not found \n", pname );
      return;
    }
    
    for (icomment = 0; icomment < comment_num; icomment++){
      
      if ( 0 == strncmp( keyword[icomment], tmp_card,
			FITS_KEYWORD_LENGTH ) ){
	
	fits_modify_comment(fp, keyword[icomment],
			    comment[icomment], istat);
	if ( *istat ) {	  		    
	  fprintf(stderr, "%s: fits_modify_comment failed (%d)\n",
		  pname, *istat);
	  return;
	}
	
	if ( fits_read_record(fp, ikey, tmp_card, istat) ) {
	  fprintf(stderr, "%s: fits_read_record failed (%d)\n", pname, *istat);
	  return;
	}
	
	for (i = 12; i < FITS_COMMENT_LENGTH; i++) {
	  if ( tmp_card[i] == comment[icomment][0] &&
	      0 == strncmp(tmp_card+i,comment[icomment],strlen(tmp_card+i))) {
	    
	    char *commentleft;
	    int naddkeys;
	    int commentlen = strlen(comment[icomment]);
	    int writtenlen = FITS_COMMENT_LENGTH - i;
	    
	    if ( commentlen <= writtenlen ) {
	      break;
	    }
	    
	    naddkeys = (commentlen - writtenlen +
			FITS_COMMENT_LENGTH_EXCEPT_KEYWROD -1)/
			  FITS_COMMENT_LENGTH_EXCEPT_KEYWROD;
	    commentleft = comment[icomment] + writtenlen;
	    
	    for (j = 0; j < naddkeys; j++) {
	      char input_card[FITS_COMMENT_LENGTH+1];	      
	      sprintf(input_card,"COMMENT   %-70.70s",
		      commentleft + FITS_COMMENT_LENGTH_EXCEPT_KEYWROD*j);

	      if (fits_insert_record(fp, ikey + j + 1, input_card, istat)) {
		fprintf(stderr,"%s: fits_insert_record failed (%d)\n",
			pname, *istat);
		return;
	      }
	    }
	    
	    break;
	    
	  }
	}
	
	comment_change++;
	break;
	
      }
      
    }
    
  }
  
  *istat = ANL_OK;
    
}

void hxdFitsComment_write_single ( fitsfile *fp, char *keyword,
				  char *comment, int *istat){
  
  int i,j=0;
  
  int ikey;
    
  char tmp_card[FITS_COMMENT_LENGTH+1];
  
  for (ikey = 1; ; ikey++) {
    /*if ( fits_flush_file(fp, istat) ){
      fprintf(stderr, "Error doing fits_flush_file() (status=%d)\n", *istat);
      return;
    }*/
    
    if ( fits_read_record(fp, ikey, tmp_card, istat) ) {
      fprintf(stderr, "%s: comment not found \n", pname );
      return;
    }
    
    if ( 0 == strncmp( keyword, tmp_card, FITS_KEYWORD_LENGTH ) ){
      
      fits_modify_comment(fp, keyword, comment, istat);
      if ( *istat ) {	  		    
	fprintf(stderr, "%s: fits_modify_comment failed (%d)\n",
		pname, *istat);
	return;
      }
      
      if ( fits_read_record(fp, ikey, tmp_card, istat) ) {
	fprintf(stderr, "%s: fits_read_record failed (%d)\n", pname, *istat );
	return;
      }
      
      for (i = 12; i < FITS_COMMENT_LENGTH; i++) {
	if ( tmp_card[i] == comment[0] &&
	    0 == strncmp(tmp_card+i, comment, FITS_COMMENT_LENGTH - i) ) {
	  
	  char *commentleft;
	  int naddkeys;
	  int commentlen = strlen(comment);
	  int writtenlen = FITS_COMMENT_LENGTH - i;
	  
	  if ( commentlen <= writtenlen ) {
	    break;
	  }
	  
	  naddkeys = (commentlen - writtenlen +
		      FITS_COMMENT_LENGTH_EXCEPT_KEYWROD -1)/
			FITS_COMMENT_LENGTH_EXCEPT_KEYWROD;
	  commentleft = comment + writtenlen;
	  
	  for (j = 0; j < naddkeys; j++) {
	    char input_card[FITS_COMMENT_LENGTH+1];	      
	    sprintf(input_card,"COMMENT   %-78.78s",
		    commentleft + FITS_COMMENT_LENGTH_EXCEPT_KEYWROD*j);
	    if (fits_insert_record(fp, ikey + j + 1, input_card, istat)) {
	      fprintf(stderr, "%s: fits_insert_record failed (%d)\n",
		      pname, *istat);
	      return;
	    }
	  }
	  
	  break;
	  
	}
      }
      
      break;
      
    }
    
  }
  
  *istat = ANL_OK;
  
}

