/*
Filename:	decodeevt.c

Purpose:	Decodes event keyword in the input science event file,generates
		TIME,PCUID,ANODE,PROPANE,and CHANNEL columns in output file.

Author:		Zhiyu Guo

Date:		May 1998

History:

*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include <ctype.h>
#include "fitsio.h"
#include "cfortran.h"
#include "ftools.h"
#include "ftoolstruct.h"
#include "xte.h"
#include "xpi.h"
#include "headas_utils.h"

#define STR_KEY_LEN 1024	/*Define the size of string value in a keyword */

struct bitfield_struct { char code; int pos; int len; long int mask; char *arg;} evtb_fields[100];
struct bitfield_struct *parse_evtb(char *, int *n_evtb_fields);
/* Bit-shifting macro */
#define evdata(str,eventword) ((int)( ((eventword)&(str->mask))>>(str->pos) ))

int evt_rd_param();
int copy_main_keywords(fitsfile *infile, fitsfile *outfile);
int get_z_info(int, int *, int *, char **); 
int get_e_info(int, int *, char **);

char *anode_names[6] = {
  "X1L", "X1R", "X2L", "X2R", "X3L", "X3R"
};
int anode_codes[6] = {
  10,    11,    20,    21,    30,    31
};

void decodeevt()
{
    	fitsfile *fptr = 0;    /* pointer to the FITS file, defined in fitsio.h */
	fitsfile *outptr = 0;
	int clobber = 0;
    	int status, hdutype, anynull, hdu_num,morekeys,tfields;
	int pha,pcuid,propane;
	long int eventword = 0;
	char *anode_name = 0;
	int anode_id = 0;
	long i;
	long felem = 1, naxis2 = 0,longnull = 0;
	long inrow, outrow;
	double time1, dblnull = 0;
	/* outfile[] has one additional character in case of clobber=yes */
	char nfound[STR_KEY_LEN],infile[STR_KEY_LEN],outfile[STR_KEY_LEN+1];
	char datamode[30], oformat;

	/* For parsing of bit fields from the event word */
	int n_evtb_fields = 0;
	struct bitfield_struct *evtbits = 0;
	struct bitfield_struct *cstr = 0, *dstr = 0, *estr = 0, *zstr = 0, *pstr = 0;
	int n_evt_bytes = 2;  /* Default 16-bit event word (=2 bytes) */
	long int mmask = 0;

#define BUFFERSIZE 32768
	int buffersize = BUFFERSIZE;
	/* Input event data */
	double times[BUFFERSIZE], outtimes[BUFFERSIZE];  
	int events2[BUFFERSIZE][2], outevents2[BUFFERSIZE][2];
	int events3[BUFFERSIZE][3], outevents3[BUFFERSIZE][3];
	int *events, *outevents;
	/* Output quantities */
	int phas[BUFFERSIZE], pcuids[BUFFERSIZE], anode_ids[BUFFERSIZE],
	  propanes[BUFFERSIZE];
	char anode_names[BUFFERSIZE][16];

/* Define keyword values for the output file */

	char extname[]="XTE_SE";
	char **ttype, **tform, **tunit;  

	/* Original format for regular event data */
	char *ttype0[]={"TIME", "CHANNEL", "PCUID", "ANODE"};
	char *tform0[]={"D", "I", "I", "15A"};
	char *tunit0[]={"s", "chan", "", ""};

	/* Original format for GoodXenon event data */
        char *ttypep[]={"TIME", "CHANNEL", "PCUID", "ANODE", "PROPANE"};
        char *tformp[]={"D", "I", "I", "15A","I"};
        char *tunitp[]={"s", "chan", "", "",""};

	/* New "OGIP" format for all data */
	char *ttypeo[]={"TIME", "Event", "PCUID", "ANODEID", "PHA" };
	char *tformo[]={"D",    "16X",   "B",     "B"      , "B"   };
	char *tunito[]={"s",    "",      "",      ""       , "chan"};

	char comm[]=" ";
	char *tevtb = 0;
	char tddes[67];

	status = 0;

    printf("\n\nRunning DECODEEVT version 3.0\n");
    printf("================================================\n");
    
    status=evt_rd_param(infile,outfile,&oformat,&clobber);
    if(status !=0){
      printf("\n\nCould not complete evt_rd_param call\n");
      printf("Terminate run\n");
      exit(1);
    } 
    
    /* Default format is regular event data */	
    n_evt_bytes = 2;
    events = &(events2[0][0]); outevents = &(outevents2[0][0]);
    tfields=4;
    ttype = ttype0; tform = tform0; tunit = tunit0;
    /* But select "OGIP" format if requested */
    if (oformat == 'O') {
      tfields = 5;
      ttype = ttypeo; tform = tformo; tunit = tunito;
    }
    
    
    /* Open input file */
    
    fits_open_file(&fptr, infile, READONLY, &status);
    if (status) {
      printf("\n ERROR: could not open %s for reading\n\n", infile);
      exit(1);
    }
    /* First skip to the binary table extension to read some important keywords */
    hdu_num=2;
    fits_movabs_hdu(fptr,hdu_num,&hdutype,&status);
    if (status || hdutype != BINARY_TBL) {
      printf("\n ERROR: could not find binary table extension of input file\n");
      exit(1);
    }
    fits_get_num_rows(fptr, &naxis2, &status);
    fits_read_key(fptr,TSTRING,"TDDES2",tddes,nfound,&status);
    fits_read_key(fptr,TSTRING,"DATAMODE",datamode,nfound,&status);
    fits_read_key_longstr(fptr,"TEVTB2",&tevtb,comm,&status);
    if (status) {
      printf("\n ERROR: could not read required keywords DATAMODE, TDDES2, TEVTB2 from input file \n");
      exit(1);
    }

    printf("\nDATAMODE = %s\n",datamode);
    printf(  "NAXIS2 = %ld\n\n",naxis2);

    /* Do not process certain modes which do not fit the model of the
       simplified output files. */
    if (strstr(datamode, "_Alpha_") ||
	strstr(datamode, "_VLE_") ||
	strstr(datamode, "_2LLD_") ||
	strstr(datamode, "Transparent")) {
      printf("\nERROR: decodeevt does not process DATAMODE='%s'\n", datamode);
      exit(1);
    }

    /* Parse the TEVTB2 string into its parts */
    evtbits = parse_evtb(tevtb, &n_evtb_fields);
    for (i=0; i<n_evtb_fields; i++) {
      /* Special case for Propane bit */
      if (evtbits[i].code == 'E' && strcmp(evtbits[i].arg,"VPR") == 0) {
	evtbits[i].code = 'P';
      }
      switch(evtbits[i].code) {
      case 'M': mmask = evtbits[i].mask; break;
      case 'C': cstr = &(evtbits[i]); break;
      case 'D': dstr = &(evtbits[i]); break;
      case 'E': estr = &(evtbits[i]); break;
      case 'Z': zstr = &(evtbits[i]); break;
      case 'P': pstr = &(evtbits[i]); break;
      }
    }
    n_evt_bytes = (evtbits[0].pos+1)/8;
    printf("   (event word size = %d bits = %d bytes)\n\n",
	   n_evt_bytes*8, n_evt_bytes);
    if (n_evt_bytes != 2 && n_evt_bytes != 3) {
      printf("\nERROR: unrecognized byte format for DATAMODE='%s'\n",
	     datamode);
      exit(1);
    }
    if (n_evt_bytes == 3) tformo[1] = "24X";

    /* Any mode-specific handing here */
    if((strncmp(datamode,"GoodXenon",9) == 0)){  
      events = &(events3[0][0]); outevents = &(outevents3[0][0]);
      /* Reset the output file format to include a PROPANE column */
      if (oformat != 'O') {
	tfields=5;
	ttype = ttypep; tform = tformp; tunit = tunitp;
      }
    }

    /* Move back to primary header to prepare for copying */
    hdu_num=1;
    fits_movabs_hdu(fptr,hdu_num,&hdutype,&status);
    
    /* Create an empty file for writing to */

    fits_create_file(&outptr,outfile,&status);
    if(status != 0){
      printf("\n Can't create the output file, it may already exist ! \n\n");
      exit(1);
    }


    /* Copy the primary header of the input file to the file just created */
    morekeys=0;
    fits_copy_hdu(fptr, outptr, morekeys, &status);
    fits_write_chksum(outptr,&status);

    /* Now move to the first table extension of the input file */
    hdu_num=2;
    fits_movabs_hdu(fptr,hdu_num,&hdutype,&status);

    /* Now committing to writing an output table.... */

    /* Create a binary extension in the output file created above */
    fits_create_tbl(outptr,BINARY_TBL,0,tfields,ttype,tform,tunit,
		    extname,&status);
    if (status || copy_main_keywords(fptr, outptr)) {
      printf("\n ERROR: can't copy input keywords to output! \n\n");
      goto FAIL;
    }

    /* Modify column name labels */
    fits_modify_comment(outptr,"TTYPE1","MET event time", &status);
    if (ttype == ttype0 || ttype == ttypep) {
      fits_modify_comment(outptr,"TTYPE2","PCA rebinned pulse height", &status);
      fits_modify_comment(outptr,"TTYPE3","PCU detector number", &status);
      fits_modify_comment(outptr,"TTYPE4","PCU anode name", &status);
      if (ttype == ttypep) {
	fits_modify_comment(outptr,"TTYPE5","Propane layer hit? (1=yes)", &status);
      }
    } else if (ttype == ttypeo) {
      fits_modify_comment(outptr,"TTYPE2","PCU event word", &status);
      fits_modify_comment(outptr,"TTYPE3","PCU detector number", &status);
      fits_modify_comment(outptr,"TTYPE4","PCU anode identifier (layer*10+right)", &status);
      fits_modify_comment(outptr,"TTYPE5","PCA rebinned pulse height", &status);
    }
    
    if (oformat == 'O') {
      int nulval = 255;
      int zero = 0, four = 4, ten = 10, forty = 40;
      fits_write_key(outptr,TINT,"TNULL3",&nulval,"Null value for PCUID",&status);
      fits_write_key(outptr,TINT,"TLMIN3",&zero,  "Minimum legal value for PCUID",&status);
      fits_write_key(outptr,TINT,"TLMAX3",&four,  "Maximum legal value for PCUID",&status);

      fits_write_key(outptr,TINT,"TNULL4",&nulval,"Null value for ANODEID",&status);
      fits_write_key(outptr,TINT,"TLMIN4",&ten,   "Minimum legal value for ANODEID",&status);
      fits_write_key(outptr,TINT,"TLMAX4",&forty, "Maximum legal value for ANODEID",&status);

      fits_write_key(outptr,TINT,"TNULL5",&nulval,"Null value for PHA",&status);
      fits_write_key(outptr,TINT,"TLMIN5",&zero,  "Minimum legal value for PHA",&status);
      if (cstr) {
	int tlmax = (1<<(cstr->len))-1;
	fits_write_key(outptr,TINT,"TLMAX5",&tlmax, "Maximum legal value for PHA",&status);
	fits_write_key_longstr(outptr, "CPIX5", (cstr->arg), "PHA binning expression",&status);
      }
    }
    fits_update_key(outptr,TSTRING,"TDDES2",tddes,comm,&status);
    fits_update_key_longstr(outptr,"TEVTB2",tevtb,comm,&status);
    /* (MJT) 30May2001: need LONGSTRN keyword */
    fits_write_key_longwarn(outptr,&status);
    if (status) {
      printf("\nERROR: could not write required keywords to output\n");
      goto FAIL;
    }

    /* Read EVENT column from the binary extension of the input file, select rows
       that have M token value of 1, i.e., the first element in each row of EVENT
       colum should be greater than 127.  Record the total number of such rows,
       this number will be used as the row number in the output file.
    */
    
    /* Read column values from the first extension of the input file and write
       them to a column in the binary extension of the output file created above */
    
    inrow  = 1;
    outrow = 1;

    /* Loop through the data in chunks */
    while (inrow <= naxis2) {
      int ninbuf = (naxis2 - inrow + 1);
      int noutbuf;
      int kout = 0;
      
      if (ninbuf > buffersize) ninbuf = buffersize;
      if (ninbuf == 0) break;
      
      fits_read_col(fptr, TDOUBLE, 1, inrow, felem, ninbuf,
		    &dblnull, times, &anynull, &status);
      fits_read_col(fptr, TINT,    2, inrow, felem, n_evt_bytes*ninbuf,
		    &longnull, events, &anynull, &status);
      if (status) { 
	printf("\nERROR: could not read rows %ld-%ld from input\n",
	       inrow,inrow+ninbuf-1);
	goto FAIL;
      }
      
      for(i=0; i<ninbuf; i++){
	
	time1 = times[i];
	if (n_evt_bytes == 2) {  /* Standard 16-bit words */
	  eventword = (events2[i][0] << 8) | events2[i][1];

	} else {  /* Deal with GoodXenon 24-bit */
	  eventword = (events3[i][0] << 16) | (events3[i][1] << 8) | events3[i][2];
	}
	
	/* Only process words that have the right event mask */
	if (eventword & mmask) {
	  
	  /* Default values */
	  anode_name = "INDEF";
	  anode_id = 255;
	  propane = 0;
	  if (oformat == 'A') {
	    pha   = -999;
	    pcuid = -999;
	  } else {
	    pha   = 255;
	    pcuid = 255;
	  }
	  
	  if (dstr) pcuid = evdata(dstr,eventword);
	  if (estr) anode_id = get_e_info(evdata(estr,eventword), &anode_id, &anode_name);
	  if (cstr) pha = evdata(cstr,eventword);
	  if (zstr) pcuid = get_z_info(evdata(zstr,eventword),&pcuid, &anode_id, &anode_name);
	  if (pstr) propane = evdata(pstr,eventword);
	  if (propane) anode_id = 40;
	  
	  /* Transcribe the intermediate products to the output arrays */
	  outtimes[kout]     = time1;
	  pcuids[kout]       = pcuid;
	  anode_ids[kout]    = anode_id;
	  phas[kout]         = pha;
	  propanes[kout]     = propane;
	  if (oformat == 'A') strcpy(anode_names[kout], anode_name);
	  if (n_evt_bytes == 2) {
	    /* Standard 16-bit words */
	    outevents2[kout][0] = events2[i][0];
	    outevents2[kout][1] = events2[i][1];
	  } else {
	    /* GoodXenon 24-bit words */
	    outevents3[kout][0] = events3[i][0];
	    outevents3[kout][1] = events3[i][1];
	    outevents3[kout][2] = events3[i][2];
	  }
	  
	  kout++;
	}
	
      }
      
      noutbuf = kout;
      if (noutbuf > 0) {
	
	/* Original output format */
	if (oformat == 'A') {
	  fits_write_col(outptr,TDOUBLE,1,outrow,felem,noutbuf,outtimes,&status);
	  fits_write_col(outptr,TINT,   2,outrow,felem,noutbuf,phas,&status);
	  fits_write_col(outptr,TINT,   3,outrow,felem,noutbuf,pcuids,&status);
	  /* XXX still writing this as individual string elements */
	  for (i=0; i<noutbuf; i++) {
	    anode_name = anode_names[i];
	    fits_write_col(outptr,TSTRING,4,outrow+i,felem,1,&anode_name,&status);
	  }
	  if (tfields == 5) {
	    fits_write_col(outptr,TINT,   5,outrow,felem,noutbuf,propanes,&status);
	  }
	} else {
	  fits_write_col(outptr,TDOUBLE,1,outrow,felem,noutbuf,outtimes,&status);
	  fits_write_col(outptr,TINT,   2,outrow,felem,n_evt_bytes*noutbuf,outevents,&status);
	  fits_write_col(outptr,TINT,   3,outrow,felem,noutbuf,pcuids,&status);
	  fits_write_col(outptr,TINT,   4,outrow,felem,noutbuf,anode_ids,&status);
	  fits_write_col(outptr,TINT,   5,outrow,felem,noutbuf,phas,&status);
	}
	if (status) { 
	  printf("\nERROR: could not write rows %ld-%ld to output.\n",
		 outrow,outrow+noutbuf-1);
	  goto FAIL;
	}
	
      }
      outrow += noutbuf;
      inrow  += ninbuf;
      
    }
    printf("  Copied %ld good event rows to output\n", outrow);
    
    /* Copy any trailing GTI files */
    fits_copy_file(fptr, outptr, 0, 0, 1, &status);
    
    /* Close the files */
    
    if (fptr) fits_close_file(fptr,&status);
    fits_close_file(outptr,&status);
    return;

 FAIL:
    /* Intermediate failure means we make sure to delete the output file */
    {
      int safestatus = 0;  /* Needed because status may be corrupt */
      if (fptr) fits_close_file(fptr, &safestatus);
      safestatus = 0;
      if (outptr) fits_delete_file(outptr, &safestatus);
    }

}

struct bitfield_struct evtb_fields[100];
struct bitfield_struct *parse_evtb(char *tevtb, int *pn_evtb_fields)
{
  int n_evtb_fields = 0;
  int curpos = 0;
  int n_alt = 0;
  int status = 0;
  /* Parse the TEVTB2 field into its alternate forms (M...)^(M...)^(M...) */
  char **alt_list = expand_item_list(tevtb,&n_alt,'^',0,1,1,&status);
  int i, j;

  /* Step through each form looking for the one associated with good xenon events,
     which begin with M[1]{1} marker. */
  for (i=0; i<n_alt; i++) if (strncmp(alt_list[i],"(M[1]{1},",9)==0) {

    /* Now expand the comma-separated bit fields of this alternate */
    char **bitfields = expand_item_list(alt_list[i]+1,&n_evtb_fields,',',1,1,1,&status);
    for (j=n_evtb_fields-1; j>= 0; j--) {
      /* Each bit field is of the form:
              c1[arg]{bitlen}, c2[arg]{bitlen}, c3[arg]{bitlen} ...
	 where cn is a character code which describes which kind of data is in this
	 bit field, arg is a type-dependent string, and {bitlen} is a length of the
	 field in bits.
	 The bit labeling starts from the left (MSB) side and proceds to the 
	 right (LSB) side.
      */
      int bpos = strlen(bitfields[j])-1;
      int bitlen = 0;
      memset(&evtb_fields[j],0,sizeof(evtb_fields[j]));

      /* Parse the character code */
      evtb_fields[j].code = bitfields[j][0];

      /* Parse the bit length */
      while (bpos >= 0 && bitfields[j][bpos] != '{') { bpos--; }
      if (bpos < 0 || bitfields[j][bpos] != '{') continue;
      sscanf(&bitfields[j][bpos],"{%d}",&bitlen);
      if (bitlen == 0) continue;
      evtb_fields[j].len = bitlen;
      evtb_fields[j].pos = curpos;

      /* Now based on the bit length and the current position within
	 the event word, calculate an AND-mask which will be used
	 to mask out the desired bits. */
      evtb_fields[j].mask = ((1<<bitlen)-1)<<curpos;

      /* Retrieve the type-dependent argument string */
      if (bpos > 1 && bitfields[j][bpos-1] == ']' && bitfields[j][1] == '[') {
	bitfields[j][bpos-1] = 0;
	evtb_fields[j].arg = bitfields[j]+2;
      }

      /* Advance the current position */
      curpos += bitlen;
    }
    break;
  }
  /* Free the main list, but keep the event word list in memory since
     we may need the arguments later. */
  if (alt_list) free(alt_list);

  /* Debugging */
  printf("TEVTB2 fields........\n");
  printf("      T Pos Len Bytemask Value\n"
	 "      ----------------------------------------------------\n");
  for (i = 0; i<n_evtb_fields; i++) {
    printf("      %c %3d %3d %08lx %-50.50s%s\n", 
	   evtb_fields[i].code,
	   evtb_fields[i].pos,
	   evtb_fields[i].len,
	   evtb_fields[i].mask,
	   evtb_fields[i].arg,
	   (strlen(evtb_fields[i].arg) > 50) ? "..." : "");
  }

  *pn_evtb_fields = n_evtb_fields;
  return &(evtb_fields[0]);
}

int get_z_info(int zvalue,int *pcuid,int *anode_code, char ** anode_name) 
{
  int anode_id;

  anode_id = (zvalue % 6);
  *pcuid = (zvalue / 6);
  *anode_code = anode_codes[anode_id];
  *anode_name = anode_names[anode_id];
  return *pcuid;
}

int get_e_info(int evalue, int *anode_code, char **anode_name)
{
  switch(evalue){   /* determine E token (ANODE) value */
  case 1: 
    *anode_name = "X1L";
    *anode_code = 10;
    break;
  case 2: 
    *anode_name = "X1R";
    *anode_code = 11;
    break;
  case 4: 
    *anode_name = "X2L";
    *anode_code = 20;
    break;
  case 8: 
    *anode_name = "X2R";
    *anode_code = 21;
    break;
  case 16: 
    *anode_name = "X3L";
    *anode_code = 30;
    break;
  case 32: 
    *anode_name = "X3R";
    *anode_code = 31;
    break;
  default: 
    *anode_name = "INDEF";
    *anode_code = 255;
    break;
  }
  return *anode_code;
}


int copy_main_keywords(fitsfile *infile, fitsfile *outfile)
{
  char card[FLEN_CARD];
  int nkeys = 0;
  int status = 0;
  int i;

  /* True if a keyword is a WCS keyword */
#define is_wcs_keyword(card) \
         ((isdigit(card[0]) && isdigit(card[5]) && \
	    (!strncmp(&card[1],"CUNI",4) || !strncmp(&card[1],"CTYP",4) || \
	     !strncmp(&card[1],"CRVL",4) || !strncmp(&card[1],"CDLT",4) || \
	     !strncmp(&card[1],"CRPX",4))) || \
          (isdigit(card[0]) && isdigit(card[1]) && \
	   (!strncmp(&card[2],"PC",2) || !strncmp(&card[2],"CD",2)))) 

  /* Copy all the user keywords (not the structural keywords) */
  fits_get_hdrspace(infile, &nkeys, NULL, &status);
  
  for (i = 1; i <= nkeys; i++) {
    fits_read_record(infile, i, card, &status);
    
    /* Copy HISTORY keywords and any user keywords, but not COMMENTs */
    if (! (fits_get_keyclass(card) >= TYP_REFSYS_KEY) ) continue;
    if (strncmp(card, "COMMENT ", 7) == 0) continue;
    if (is_wcs_keyword(card)) continue;

    fits_write_record(outfile, card, &status);
  }

  return status;
}

int evt_rd_param(ifile,ofile, oformat, clobber)
     char *ifile, *ofile, *oformat;
     int *clobber;
{
  int parstat=0;
  int BufLen_2 = STR_KEY_LEN-1; /* NOTE: used by Uglst() macro */
  char oformat1[STR_KEY_LEN];
  
  *clobber = 0;
  Uclgsb("clobber",clobber,&parstat);
  if (parstat != 0) {
    printf("\nCould not read 'clobber' parameter.\n");
    exit(1);
  }

  Uclgst("infile",ifile,&parstat);
  if(parstat != 0){
    printf("\nCould not read 'infile' parameter.\n");
    exit(1);
  }

  /* If clobber is set, then prepend a '!' for '!outfile' notation
     for CFITSIO */
  ofile[0] = 0; ofile[1] = 0;
  if (*clobber) {
    printf("  (note: clobber=YES set)\n");
    ofile[0] = '!';
    ofile++;
  }
  Uclgst("outfile",ofile,&parstat);
  if(parstat != 0){
    printf("\nCould not read 'outfile' parameter.\n");
    exit(1);
  }
  
  Uclgst("outformat",oformat1,&parstat);
  if(parstat != 0){
    printf("\nCould not read 'outformat' parameter.\n");
    exit(1);
  }
  if (oformat1[0] == 'o' || oformat1[0] == 'O') {
    *oformat = 'O';
  } else if (oformat1[0] == 'a' || oformat1[0] == 'A') {
    *oformat = 'A';
  } else {
    printf("\nERROR: outformat must be either 'O' or 'A'\n");
    exit(1);
  }

  
  return parstat;
}

/* This code is needed by IRAF */

#ifdef vms
#define F77CALL decodeev
#endif
#ifdef unix
#define F77CALL decodeev_
#endif

void F77CALL()
{
void decodeevt();
decodeevt();
}
