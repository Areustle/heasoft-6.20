/*
Filename:	sbmerge.c

Purpose:	Combine XTE data from several concurrent streams into one
		stream.

Author:		Zhiyu Guo

Date:		Dec. 1997
*/
#define TASK_VERSION "2.0"
/*

History:

   29Oct98 (MJT) Major modifications:
                 gainapp should be int, not char
                 need fits_write_key_longwarn
                 tform[1] / dim2_str_new / TDDES2 handling revamped
                 TIERRELA/TIERABSO comments were switched!
		 clobber parameter fixed

   Version 2.0
   21Mar2008 (CBM) More major modifications:
                   Bug: double-counting of events caused by the way
                      that row times were compared.
                   "Performance bug" was taking hours because of constant
                      open/close operations, especially for zipped files.
                      Now the files are opened once at the beginning,
                      causing a factor of of ~2000x speedup.
                   Fixed error in TDDES2 keyword output.

*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "fitsio.h"
#include "cfortran.h"
#include "ftools.h"
#include "ftoolstruct.h"
#include "xte.h"
#include "xpi.h"

#define BUFF_LEN 255	/*Define the size of file name string */
#define BufLen_2 255
#define tfields 6

int sb_rd_input1();
int sb_rd_input2();
int sb_rd_input3();
int sb_rd_input4();
int index_loc();

#define MAX_FILES 10
void Sbmerge()
{
  fitsfile *fptr;    /* pointer to the FITS file, defined in fitsio.h */
  fitsfile *infiles[MAX_FILES];
  fitsfile *outptr;
  char letter;
  char **file, **names, **datamode;
  char *infile, *tmp;
  /* char dim2_str_new[]=" "; */
  char outfile[BUFF_LEN],num_file[10],radecsys[8];
  char nfound[BUFF_LEN],card[BUFF_LEN],tform2[BUFF_LEN];
  char dim2_str[BUFF_LEN],tmp_str[BUFF_LEN],dummy_file[BUFF_LEN];
  int status,hdutype,ilen,ihold,morekeys,anynull,dim2_len,fdim;
  int *slen,*final_dim,*spillage,*modespec;
  int** chan;
  long *XeCnt, *Tmp_XeCnt,*naxis2, *time;
  long chduaddr,nextaddr,frow,felem,nelem,nrows,longnull;
  long ii,dim2,naxis_min,naxis_max,dim2_new;
  long i,j,k,min_i,max_i,time2,n,dump,tmp_naxis2;
  int num_file1;
  long **loc, **event, **out_evt, **time_2d;
  double *tstart, *tstop;
  double dtstart,dtstop,tres,tstart_max,tstop_min;
  double tierrela,tierabso,rapnt,decpnt,cdlt2;
  float equinox;
  FILE *fileptr;
  
  char date[BUFF_LEN],dateobs[BUFF_LEN],dateend[BUFF_LEN];
  char timeobs[BUFF_LEN],timeend[BUFF_LEN],timesys[BUFF_LEN];
  char timeunit[BUFF_LEN],object[BUFF_LEN],tddes[BUFF_LEN];
  char observer[BUFF_LEN],obs_id[BUFF_LEN],telescope[BUFF_LEN];
  char instrument[BUFF_LEN],obs_mode[BUFF_LEN];
  char tddes2[BUFF_LEN],sub_str1[BUFF_LEN],tddes1[5];
  char** sub_str2;
  char *sub_str3,*sub_str4,*sub_str5,*sub_str6, *tres_str,*dmode;
  
  int mjdrefi, gainapp, clobber;
  double mjdreff,timezero;
  float timedel,deltat;
  int cmin = -1, cmax = -1;
  
  /* Define keyword values in the binary extension for output file */ 
  
  char extname[]="XTE_SA";
  char *ttype[]={"Time","XeCnt","Counts","Channel","Spillage","ModeSpecific"};
  char *tunit[]={"s","Count","Count"," ","Count","Count"};	
  char **tform;
  
  status=0;
  c_ptaskn("sbmerge");
  c_fcecho("sbmerge v" TASK_VERSION);

  status=sb_rd_input1(num_file); /* read the number of files to be combined */
  
  num_file1=atoi(num_file);  /* convert from a string to an integer */
  if (num_file1 > MAX_FILES) {
      c_fcerr("Maximum number of files is 10 !!!");
      exit(1);
  }
  
  /* Assign values to 'tform' array, tform[1] will be assigned later */
 
  tform=(char**) calloc(tfields,sizeof(char*));
  for(i=0;i<tfields;i++){
    tform[i]=(char*) calloc(BUFF_LEN,sizeof(char));
  }
  tform[0]="D";
  strcpy(tform[2],strcat(num_file,"I"));
  strcpy(tform[3],tform[2]);
  tform[4]="B";
  tform[5]="B";
  
  /* Allocate memory to 'file' and 'names' */

  file=(char**) calloc(num_file1,sizeof(char*));
  for(i=0;i<num_file1;i++){
    if(!(file[i]=(char*) calloc(BUFF_LEN,sizeof(char)))){
      c_fcerr("Can't allocate memory to 'file', exit !!!");
      exit(1);
    }
  }
  
  names=(char**) calloc(num_file1,sizeof(char*));
  for(i=0;i<num_file1;i++){
    if(!(names[i]=(char*) calloc(BUFF_LEN,sizeof(char)))){
      c_fcerr("Can't allocate memory to 'names', exit !!!");
      exit(1);
    }
  }
  
  datamode=(char**) calloc(num_file1,sizeof(char*));
  for(i=0;i<num_file1;i++){
    if(!(datamode[i]=(char*) calloc(BUFF_LEN,sizeof(char)))){
      c_fcerr("Can't allocate memory to 'datamode', exit !!!");
      exit(1);
    }
  }
  
  if(!(infile=(char*) calloc(BUFF_LEN,sizeof(char)))){
    c_fcerr("Can't allocate memory to 'infile', exit !!!");
    exit(1);
  }
  
  /* read input files or a list file (start with @) */
  
  status=sb_rd_input2(infile); 
  
  if(status != 0){
    c_fcerr("Could not get the input file name.");
    c_fcerr("Check the file name !");
    exit(1);
  }
  
  ilen=strlen(infile);
  ihold=0;
  for(i=0;i<ilen;i++){
    if(infile[i] == '@'){
      ihold=i;
    }
  }
  
  if(!(tmp=(char*) calloc(BUFF_LEN,sizeof(char)))){
    c_fcerr("Can't allocate memory to 'tmp', exit !!!");
    exit(1);
  }
  
  if(!(slen=(int*) calloc(num_file1,sizeof(int)))){
    c_fcerr("Can't allocate memory to 'slen', exit !!!");
	exit(1);
  }
  
  if(infile[ihold] == '@'){  /* if this is a list file */
    tmp = &infile[1];
    fileptr=fopen(tmp,"r");
    for(i=0;i<num_file1;i++){
      if(!fgets(names[i],BUFF_LEN,fileptr)){
	c_fcerr("Can't get the contents from the file, exit !");
	exit(1);
      }
      slen[i]=strlen(names[i]);
    }
    
    /* Notice, STRLEN returns the actual length of the string plus an ending null
       element, so we need to use slen[i]-1 for actual string length. */
    
    for(i=0;i<num_file1;i++){
      strncpy(file[i],names[i],slen[i]-1); 
    }
    
    fclose(fileptr);
    
  }else{  /* if input data file line by line */
    slen[0]=strlen(infile);
    strncpy(file[0],infile,slen[0]);
    for(i=0;i<num_file1-1;i++){
      status=sb_rd_input2(infile);
      if(status != 0){
	c_fcecho("Could not get the input file name.");
	c_fcecho("Check the file name !");
	exit(1);
      }
      slen[i+1]=strlen(infile);
      strncpy(file[i+1],infile,slen[i+1]);
    }
  }
  
  /* Allocate memory to 'tstart','tstop',and 'naxis2'. */
  
  if(!(tstart=(double*) calloc(num_file1,sizeof(double)))){
    c_fcerr("Can't allocate memory to 'tstart', exit !!!");
    exit(1);
  }
  if(!(tstop=(double*) calloc(num_file1,sizeof(double)))){
    c_fcerr("Can't allocate memory to 'tstart', exit !!!");
    exit(1);
  }
  if(!(naxis2=(long*) calloc(num_file1,sizeof(long)))){
    c_fcerr("Can't allocate memory to 'naxis2', exit !!!");
    exit(1);
  }

  /* Open all input data files once */
  for(i=0;i<num_file1;i++){
    fits_open_file(&fptr,file[i],READONLY,&status);

    if(status != 0){
      c_fcerr("Can't open data file, please check file names or blank lines in list files !");
      fits_report_error(stderr, status);
      exit(1);
    }
    fits_movabs_hdu(fptr,2,&hdutype,&status); /* move to 2nd HDU */

    infiles[i] = fptr;

    c_fcecho("Opened file:");
    c_fcecho(file[i]);
  }

  /* Read 'tstart', 'tstop', 'naxis2', and 'tform2'. */
  
  for(i=0;i<num_file1;i++){
    fptr = infiles[i];
    
    fits_read_key(fptr,TLONG,"NAXIS2",&naxis2[i],nfound,&status);
    
    fits_read_key(fptr,TSTRING,"DATE",date,nfound,&status);
    fits_read_key(fptr,TSTRING,"DATE-OBS",dateobs,nfound,&status);
    fits_read_key(fptr,TSTRING,"TIME-OBS",timeobs,nfound,&status);
    fits_read_key(fptr,TSTRING,"DATE-END",dateend,nfound,&status);
    fits_read_key(fptr,TSTRING,"TIME-END",timeend,nfound,&status);
    fits_read_key(fptr,TSTRING,"TIMESYS",timesys,nfound,&status);
    fits_read_key(fptr,TINT,"MJDREFI",&mjdrefi,nfound,&status);
    fits_read_key(fptr,TDOUBLE,"MJDREFF",&mjdreff,nfound,&status);
    fits_read_key(fptr,TDOUBLE,"TIMEZERO",&timezero,nfound,&status);
    fits_read_key(fptr,TSTRING,"TIMEUNIT",timeunit,nfound,&status);
    
    fits_read_key(fptr,TDOUBLE,"TIERRELA",&tierrela,nfound,&status);
    fits_read_key(fptr,TDOUBLE,"TIERABSO",&tierabso,nfound,&status);
    
    fits_read_key(fptr,TDOUBLE,"TSTART",&tstart[i],nfound,&status);
    fits_read_key(fptr,TDOUBLE,"TSTOP",&tstop[i],nfound,&status);
    
    fits_read_key(fptr,TSTRING,"OBJECT",object,nfound,&status);
    fits_read_key(fptr,TDOUBLE,"RA_PNT",&rapnt,nfound,&status);
    fits_read_key(fptr,TDOUBLE,"DEC_PNT",&decpnt,nfound,&status);
    fits_read_key(fptr,TFLOAT,"EQUINOX",&equinox,nfound,&status);
    fits_read_key(fptr,TSTRING,"RADECSYS",radecsys,nfound,&status);
    
    fits_read_key(fptr,TSTRING,"OBSERVER",observer,nfound,&status);
    fits_read_key(fptr,TSTRING,"OBS_ID",obs_id,nfound,&status);
    fits_read_key(fptr,TSTRING,"TELESCOP",telescope,nfound,&status);
    fits_read_key(fptr,TSTRING,"INSTRUME",instrument,nfound,&status);
    fits_read_key(fptr,TSTRING,"OBS_MODE",obs_mode,nfound,&status);
    
    fits_read_key(fptr,TSTRING,"DATAMODE",datamode[i],nfound,&status);
    
    fits_read_key(fptr,TFLOAT,"TIMEDEL",&timedel,nfound,&status);
    fits_read_key(fptr,TFLOAT,"DELTAT",&deltat,nfound,&status);
    fits_read_key(fptr,TLOGICAL,"GAINAPP",&gainapp,nfound,&status);
    fits_read_key(fptr,TSTRING,"TDDES",tddes,nfound,&status);
    fits_read_key(fptr,TSTRING,"TDDES1",tddes1,nfound,&status);
    fits_read_key(fptr,TSTRING,"TFORM2",tform2,nfound,&status);
    fits_read_key(fptr,TDOUBLE,"1CDLT2",&cdlt2,nfound,&status);
    
  }
  
  /*
    Check to see if the input data files cover the same time period. 
    
    for(i=0;i<num_file1;i++){
    for(j=0;j<num_file1;j++){
    if(i != j){
    dtstart=tstart[i]-tstart[j];
    dtstop=tstop[i]-tstop[j];
    if(dtstart != 0 || dtstop != 0){
    printf("Input data don't cover the same time period, exit !!! \n");
    exit(1);
    }
    }
    }
    }
  */
  
  tstart_max=tstart[0];
  i=0;
  while(i<num_file1){
    if(tstart[i]>tstart_max){
      tstart_max=tstart[i];
    }
    i++;
  }
  
  tstop_min=tstop[0];
  i=0;
  while(i<num_file1){
    if(tstop[i]<tstop_min){
      tstop_min=tstop[i];
    }
    i++;
  }
  
  /* Check 'datamode' to find energy channels and sort files based on increasing
     energy channels */
  
  for(j=1;j<num_file1;j++){
    strcpy(tmp_str,datamode[j]);
    strcpy(dummy_file,file[j]);
    tmp_naxis2=naxis2[j];
    for(k=j-1;k>=0;k--){
      if(strcmp(tmp_str,datamode[k]) < 0){
	strcpy(datamode[k+1],datamode[k]);
	strcpy(file[k+1],file[k]);
	naxis2[k+1]=naxis2[k];
      }else{
	break;
      }
      strcpy(datamode[k],tmp_str);
      strcpy(file[k],dummy_file);
      naxis2[k]=tmp_naxis2;
    }
  }
  
  
  /* Get value for the 2nd dimension of 'XeCnt' field in the binary ext, and
     assign value to tform[1]. */
  
  /*
    if(!(dim2_str_new=(char*) calloc(BUFF_LEN,sizeof(char)))){
    printf("Can't allocate memory to 'dim2_str_new',exit!!! \n");
    exit(1);
    }
  */
  
  dim2_len=strlen(tform2);
  strncpy(dim2_str,tform2,dim2_len-1);
  dim2=atoi(dim2_str);
  dim2_new=dim2*num_file1;
  /* this whole game with dim2_str_new is wrong
     so taking out any use of that variable
     29 Oct 1998, MJT */
  /* sprintf(dim2_str_new,"%5d",dim2);
  tform[1]=strcat(dim2_str_new,"I"); */
  sprintf(tform[1],"%-dI",dim2);
  
  /* Check to find the minimum and maximum NAXIS2s which will be used as NAXIS2 
     in the output file */
  
  naxis_min=naxis2[0];
  i=0;
  min_i=0;
  while(i<num_file1){
    if(naxis2[i] < naxis_min){
      naxis_min=naxis2[i];
      min_i=i;
    }
    i++;
  }
  
  naxis_max=naxis2[0];
  i=0;
  max_i=0;
  while(i<num_file1){
    if(naxis2[i] > naxis_max){
      naxis_max=naxis2[i];
      max_i=i;
    }
    i++;
  }

  status=sb_rd_input3(outfile);  /* read output file name */
  if(status != 0){
    c_fcerr("Could not open the output file.");
    c_fcerr("Check the file name !");
    exit(1);
  }

  status=sb_rd_input4(&clobber);
  if(status != 0){
    c_fcerr("Error reading clobber parameter");
    exit(1);
  }
  
  /* Create an empty file for writing to it */
  
  if (clobber) remove(outfile);
  
  fits_create_file(&outptr,outfile,&status);
  
  if(status != 0){
    c_fcerr("Check the destination file, it may already exist !");
    fits_report_error(stderr, status);
    exit(1);
  }
  
  
  /* Copy primary header of the first file to the output file (all primary
     headers of the input files should be identical). */
  
  fptr = infiles[min_i];
  
  morekeys=0;
  fits_movabs_hdu(fptr,1,&hdutype,&status); /* move to 1st HDU */
  fits_copy_hdu(fptr,outptr,morekeys,&status);
  
  fits_write_chksum(outptr,&status);
  fits_movabs_hdu(fptr,2,&hdutype,&status); /* move to 2nd HDU */
  
  
  /* Combine the DATAMODE keyword from input files to form the DATAMODE
     keyword in outputfile */
  
  if(!(dmode=(char*) calloc(BUFF_LEN,sizeof(char)))){
    printf("Can't allocate memory to 'dmode', exit !!! \n");
    exit(1);
  }
  
  for(i=0;i<num_file1-1;i++){
    strncat(dmode,datamode[i],strlen(datamode[i]));
    strcat(dmode,",");
  }
  strncat(dmode,datamode[num_file1-1],strlen(datamode[num_file1-1]));
  
  /* Create TDDES2 keyword for the output file */
  
  sub_str2=(char**) calloc(num_file1,sizeof(char*));
  for(i=0;i<num_file1;i++){
    if(!(sub_str2[i]=(char*) calloc(80,sizeof(char)))){
      c_fcerr("Can't allocate memory to 'sub_str2', exit !!!");
      exit(1);
    }
  }
  
  if(!(sub_str3=(char*) calloc(80,sizeof(char)))){
    c_fcerr("Can't allocate memory to 'sub_str3', exit !!!");
    exit(1);
  }

  for(i=0;i<num_file1;i++){
    int cmini = -1, cmaxi = -1;
    if (i != 0 && i != (num_file1-1)) continue;
    fptr = infiles[i];

    fits_read_key(fptr,TSTRING,"TDDES2",tddes2,nfound,&status);
    k = index_loc(tddes2,"C[");
    if (k >= 0 && sscanf(tddes2+k,"C[%d~%d]", &cmini, &cmaxi) == 2) {
      if (i == 0) {
	cmin = cmini;
      } else if (i == num_file1-1) {
	cmax = cmaxi;
      }
    }
  }
  if (cmin >= 0 && cmax >= 0) {
    sprintf(sub_str3, "%d~%d", cmin, cmax);
  } else {
    c_fcerr("Could not find all C[] markers in TDDES2 keywords!");
    exit(1);
  }
  
  if(!(sub_str4=(char*) calloc(BUFF_LEN,sizeof(char)))){
    c_fcerr("Can't allocate memory to 'sub_str4', exit !!!");
    exit(1);
  }
  
  if(!(sub_str6=(char*) calloc(80,sizeof(char)))){
    c_fcerr("Can't allocate memory to 'sub_str6', exit !!!");
    exit(1);
  }
 
  strncpy(sub_str4,tddes2,index_loc(tddes2,"C")-1);

  strcat(sub_str4," C[");
  strcat(sub_str4,sub_str3);
  strcat(sub_str4,"]");
  
  tres=timedel/dim2;
  tres_str=(char*) calloc(BUFF_LEN,sizeof(char));
  
  sprintf(tres_str,"%16.14f",tres);
 
  if(!(sub_str5=(char*) calloc(BUFF_LEN,sizeof(char)))){
    c_fcerr("Can't allocate memory to 'sub_str5', exit !!!");
    exit(1);
  }
 
  strcat(sub_str5," & T[0.0;");
  strcat(sub_str5, tres_str);
  strcat(sub_str5,";");
  /* strncpy(sub_str6,dim2_str_new,strlen(dim2_str_new)-1);
  strcat(sub_str5,sub_str6); */
  strncat(sub_str5,tform[1],strlen(tform[1])-1);
  strcat(sub_str5,"]");
  
  strcat(sub_str4,sub_str5);
  
  frow=1;
  felem=1;
  nrows=naxis_max;
  
  /* Create a binary extension in the output file */

  status=0;
  fits_create_tbl(outptr,BINARY_TBL,naxis_min,tfields,ttype,tform,tunit,
		  extname,&status);
  if (status != 0){
    c_fcerr("Error creating binary table extension");
    fits_report_error(stderr, status);
    exit(1);
  }

  /* Write key words in the binary extension */
  
  fits_write_key(outptr,TSTRING,"TDDES1",tddes1,nfound,&status);
  
  fits_write_key_longstr(outptr,"TDDES2",sub_str4,nfound,&status);
  fits_write_key_longwarn(outptr, &status);
  
  fits_write_key(outptr,TDOUBLE,"1CDLT2",&cdlt2,nfound,&status);
  
  fits_write_key(outptr,TSTRING,"1CTYP2","TIME",nfound,&status);
  
  fits_write_key(outptr,TSTRING,"ORIGIN","XTE-SOC",nfound,&status);
  fits_modify_comment(outptr,"ORIGIN", " ",&status);
  
  fits_write_key(outptr,TSTRING,"HDUCLASS","OGIP",nfound,&status);
  fits_modify_comment(outptr,"HDUCLASS",
		      "format conforms to OGIP/GSFC standards",&status);
  
  fits_write_key(outptr,TSTRING,"HDUCLAS1","ARRAY",nfound,&status);
  fits_modify_comment(outptr,"HDUCLAS1",
		      "An array merged from concurrent data",&status);
  
  fits_write_key(outptr,TSTRING,"HDUCLAS2","TOTAL",nfound,&status);
  fits_modify_comment(outptr,"HDUCLAS2", " ",&status);
  
  fits_write_key(outptr,TSTRING,"HDUCLAS3","COUNT",nfound,&status);
  fits_modify_comment(outptr,"HDUCLAS3", " ",&status);
  
  fits_write_key(outptr,TSTRING,"DATE",date,nfound,&status);
  fits_modify_comment(outptr,"DATE",
		      "FITS file creation date (dd/mm/yy)",&status);
  
  fits_write_key(outptr,TSTRING,"DATE-OBS",dateobs,nfound,&status);
  fits_modify_comment(outptr,"DATE-OBS",
		      "Start date for data",&status);
  
  fits_write_key(outptr,TSTRING,"TIME-OBS",timeobs,nfound,&status);
  fits_modify_comment(outptr,"TIME-OBS",
		      "Start time for data",&status);
  
  fits_write_key(outptr,TSTRING,"DATE-END",dateend,nfound,&status);
  fits_modify_comment(outptr,"DATE-END",
		      "End date for data",&status);
  
  fits_write_key(outptr,TSTRING,"TIME-END",timeend,nfound,&status);
  fits_modify_comment(outptr,"TIME-END",
		      "End time for data",&status);
  
  fits_write_key(outptr,TSTRING,"TIMESYS",timesys,nfound,&status);
  fits_modify_comment(outptr,"TIMESYS",
		      "XTE time will be TT (Terrestrial Time)",&status);
  
  fits_write_key(outptr,TINT,"MJDREFI",&mjdrefi,nfound,&status);
  fits_modify_comment(outptr,"MJDREFI",
		      "1994.0(UTC) expressed in TT (integer part)",&status);
  
  fits_write_key(outptr,TDOUBLE,"MJDREFF",&mjdreff,nfound,&status);
  fits_modify_comment(outptr,"MJDREFF",
		      "1994.0(UTC) expressed in TT (fractional part)",&status);
  
  fits_write_key(outptr,TDOUBLE,"TIMEZERO",&timezero,nfound,&status);
  fits_modify_comment(outptr,"TIMEZERO",
		      "Clock correction",&status);
  
  fits_write_key(outptr,TSTRING,"TIMEUNIT",timeunit,nfound,&status);
  fits_modify_comment(outptr,"TIMEUNIT",
		      " ",&status);
  
  fits_write_key(outptr,TDOUBLE,"TSTART",&tstart[0],nfound,&status);
  fits_modify_comment(outptr,"TSTART",
		      "As in the TIME column: raw spacecraft clock",&status);
  
  fits_write_key(outptr,TDOUBLE,"TSTOP",&tstop[0],nfound,&status);
  fits_modify_comment(outptr,"TSTOP",
		      "add TIMEZERO and MJDREF for absolute TT",&status);
  
  fits_write_key(outptr,TSTRING,"OBJECT",object,nfound,&status);
  fits_modify_comment(outptr,"OBJECT",
		      "Source information block",&status);
  
  fits_write_key(outptr,TSTRING,"OBSERVER",observer,nfound,&status);
  fits_modify_comment(outptr,"OBSERVER",
		      "Observation information block",&status);
  
  fits_write_key(outptr,TSTRING,"OBS_ID",obs_id,nfound,&status);
  fits_modify_comment(outptr,"OBS_ID",
		      "<proposal>-<target>-<viewing>-<seq no><type>",&status);
  
  fits_write_key(outptr,TSTRING,"TELESCOP",telescope,nfound,&status);
  fits_modify_comment(outptr,"TELESCOP",
		      " ",&status);
 
  fits_write_key(outptr,TSTRING,"INSTRUME",instrument,nfound,&status);
  fits_modify_comment(outptr,"INSTRUME",
		      " ",&status);
  
  fits_write_key(outptr,TSTRING,"OBS_MODE",obs_mode,nfound,&status);
  fits_modify_comment(outptr,"OBS_MODE",
		      "Pointing, slew, or scan",&status);
  
  fits_write_key(outptr,TFLOAT,"TIMEDEL",&timedel,nfound,&status);
  fits_modify_comment(outptr,"TIMEDEL",
		      "Time resolution",&status);
  
  fits_write_key(outptr,TFLOAT,"DELTAT",&deltat,nfound,&status);
  fits_modify_comment(outptr,"DELTAT",
		      "Telemetry interval",&status);
  
  fits_write_key(outptr,TLOGICAL,"GAINAPP",&gainapp,nfound,&status);
  fits_modify_comment(outptr,"GAINAPP",
		      "Ph analyzer gain correction applied?",&status);
  
  fits_write_key(outptr,TSTRING,"TDDES",tddes,nfound,&status);
  fits_modify_comment(outptr,"TDDES",
		      "Root: Root data descriptor",&status);
  
  fits_write_key(outptr,TSTRING,"DATAMODE",dmode,nfound,&status);
  fits_modify_comment(outptr,"DATAMODE",
		      "Combined SB mode",&status);
  
  fits_write_key(outptr,TDOUBLE,"TIERRELA",&tierrela,nfound,&status);
  fits_modify_comment(outptr,"TIERRELA","Short-term clock stability",&status);
  
  fits_write_key(outptr,TDOUBLE,"TIERABSO",&tierabso,nfound,&status);
  fits_modify_comment(outptr,"TIERABSO","Absolute precision of clock correction",&status);
  
  fits_write_key(outptr,TDOUBLE,"RA_PNT",&rapnt,nfound,&status);
  fits_modify_comment(outptr,"RA_PNT","Nominal pointing",&status);
  
  fits_write_key(outptr,TDOUBLE,"DEC_PNT",&decpnt,nfound,&status);
  fits_modify_comment(outptr,"DEC_PNT","Nominal pointing",&status);
  
  fits_write_key(outptr,TFLOAT,"EQUINOX",&equinox,nfound,&status);
  fits_modify_comment(outptr,"EQUINOX","J2000.0",&status);
  
  fits_write_key(outptr,TSTRING,"RADECSYS",radecsys,nfound,&status);
  fits_modify_comment(outptr,"RADECSYS","Julian coordinate reference frame",&status);
  
  free(sub_str2);
  free(sub_str3);
  free(sub_str4);
  free(sub_str5);
  free(sub_str6);
  
  /* read fields 'time','spillage', and 'modespec' from input binary table */
  
  if(!(time=(long*) calloc(nrows,sizeof(long)))){
    c_fcerr("Can't allocate memory to 'time', exit !!!");
    exit(1);
  }
  
  time_2d=(long**) calloc(nrows,sizeof(long*));
  for(i=0;i<nrows;i++){
    time_2d[i]=(long*) calloc(num_file1,sizeof(long));
  }

  if(!(spillage=(int*) calloc(nrows,sizeof(int)))){
    c_fcerr("Can't allocate memory to 'spillage', exit!!!");
    exit(1);
  }

  if(!(modespec=(int*) calloc(nrows,sizeof(int)))){
    c_fcerr("Can't allocate memory to 'modespec', exit!!!");
    exit(1);
  }

  if(!(final_dim=(int*) calloc(num_file1,sizeof(int)))){
    c_fcerr("Can't allocate memory to 'final_dim', exit!!!");
    exit(1);
  }

  for(i=0;i<num_file1;i++){
    fptr = infiles[i];
    
    fits_read_col(fptr,TLONG,1,frow,felem,naxis2[i],&longnull,
		  time,&anynull,&status);
    
    final_dim[i]=0;
    for(j=0;j<naxis2[i];j++){
      if(time[j] >= tstart_max && time[j] <= tstop_min){
	time_2d[j][i]=time[j];
	final_dim[i]++;
      }
    }
    
    fits_read_col(fptr,TINT,3,frow,felem,naxis2[i],&longnull,
		  spillage,&anynull,&status);
    
    fits_read_col(fptr,TINT,4,frow,felem,naxis2[i],&longnull,
		  modespec,&anynull,&status);
    
  }
  
  i=0;
  fdim=final_dim[0];
  while(i<num_file1){
    if(final_dim[i]<fdim){
      fdim=final_dim[i];
      min_i=i;
    }
    i++;
  }
  
  naxis_min=fdim;
  
  /* find locations where input binary tables have common rows */
  /* The input file with the minimum number of rows is considered
     the master time file */
  loc=(long**) calloc(naxis_min,sizeof(long*));
  for(i=0;i<naxis_min;i++){
    loc[i]=(long*) calloc(num_file1,sizeof(long));
  }
  /* CM NOTE: loc[j][i] is initialized to 0 which is a valid value! */
  
  for(i=0;i<naxis_min;i++){
    time[i]=time_2d[i][min_i];
  }
  
  for(i=0;i<num_file1;i++){
    for(j=0;j<naxis_min;j++){
      for(k=0;k<naxis_max;k++){
	/* CM NOTE: Time file i   == time in master file */
	if(time_2d[k][i] == time_2d[j][min_i]){
	  /* CM NOTE: loc[j][i] = k means that
	     file i row k corresponds to the master time row j */
	  loc[j][i] = k ;
	}
      }
    }
  }
  
  
  if(!(XeCnt=(long*) calloc(dim2,sizeof(long)))){
    c_fcerr("Can't allocate memory to 'XeCnt', exit !!!");
    exit(1);
  }
  
  event=(long**) calloc(nrows,sizeof(long*));
  for(i=0;i<nrows;i++){
    if(!(event[i]=(long*) calloc(num_file1,sizeof(long)))){
      c_fcerr("Can't allocate memory to 'event', exit !!!");
      exit(1);
    }
  }

  /* CM NOTE: Sum the events in each row and each file */
  for(i=0;i<num_file1;i++){
    fptr = infiles[i];
    
    for(j=0;j<naxis2[i];j++){
      fits_read_col(fptr,TLONG,2,j+1,felem,dim2,&longnull,
		    XeCnt,&anynull,&status);
      
      for(k=0;k<dim2;k++){
	event[j][i] += XeCnt[k];  /*calculate broad band cnt*/ 
      }
    }
    
  }
  
  free(XeCnt);

  out_evt=(long**) calloc(naxis_min,sizeof(long*));
  for(i=0;i<naxis_min;i++){
    if(!(out_evt[i]=(long*) calloc(num_file1,sizeof(long)))){
      c_fcerr("Can't allocate memory to 'out_evt', exit !!!");
      exit(1);
    }
  }

  for(i=0;i<num_file1;i++){
    for(j=0;j<naxis_min;j++){
      out_evt[j][i]=event[loc[j][i]][i]; /*counts from common rows*/ 
    }
  }
  
  free(event);

  chan=(int**) calloc(naxis_min,sizeof(int*));
  for(i=0;i<naxis_min;i++){
    if(!(chan[i]=(int*) calloc(num_file1,sizeof(int)))){
      c_fcerr("Can't allocate memory to 'chan', exit !!!");
      exit(1);
    }
  }
  
  for(i=0;i<naxis_min;i++){
    for(j=0;j<num_file1;j++){
      chan[i][j]=j+1;
    }
  }
  
  /* combine XeCnt arrays from input files directly */
  
  if(!(XeCnt=(long*) calloc(dim2,sizeof(long)))){
    c_fcerr("Can't allocate memory to 'XeCnt', exit!!!");
    exit(1);
  }
  
  if(!(Tmp_XeCnt=(long*) calloc(dim2,sizeof(long)))){
    c_fcerr("Can't allocate memory to 'Tmp_XeCnt', exit!!!");
    exit(1);
  }

  for(j=0;j<naxis_min;j++){    /* Loop over output rows */
    for(i=0;i<num_file1;i++){  /* Check each file for matching rows */
      fptr = infiles[i];

      for (k=0;k<naxis_max;k++) {  /* Check each row of file for match */
	if (time_2d[k][i] == time_2d[j][min_i]) {
	  fits_read_col(fptr,TLONG,2,k+1,felem,dim2,&longnull,
			Tmp_XeCnt,&anynull,&status);
	  for(n=0;n<dim2;n++){
	    XeCnt[n]=XeCnt[n]+Tmp_XeCnt[n];
	  }
	  break;
	}
      }
    }

    fits_write_col(outptr,TLONG,1,j+1,felem,1,&time[j],&status);
    fits_write_col(outptr,TLONG,2,j+1,felem,dim2,XeCnt,&status);
    fits_write_col(outptr,TLONG,3,j+1,felem,num_file1,out_evt[j],&status);
    fits_write_col(outptr,TINT,4,j+1,felem,num_file1,chan[j],&status);
    fits_write_col(outptr,TINT,5,j+1,felem,1,&spillage[j],&status);
    fits_write_col(outptr,TINT,6,j+1,felem,1,&modespec[j],&status);

    for(n=0;n<dim2;n++){
      XeCnt[n]=0;
    }
  }
  free(time);
  free(XeCnt);
  free(out_evt);
  free(chan);
  free(Tmp_XeCnt);

  fits_write_chksum(outptr,&status);
  
  /* create an empty HDU in the output file right after the CHDU */
  
  fits_create_hdu(outptr,&status);
  
  /* open infile to get GTI extension */

  fptr = infiles[0];
  
  /* move the HDU in the input file to 3rd (GTI extension) HDU */
  
  fits_movabs_hdu(fptr,3,&hdutype,&status);
  
  /* copy the GTI extension from input file to output file */
  
  fits_copy_hdu(fptr,outptr,morekeys,&status);
  
  fits_write_chksum(outptr,&status);

  for (i=0; i<num_file1; i++) {
    fits_close_file(infiles[i],&status);
  }
  fits_close_file(outptr,&status);
  
  free(spillage);
  free(modespec);
  free(loc);
  free(names);
  free(datamode);
  free(file);
  free(slen);
  free(naxis2);
  free(tstart);
  free(tstop);
  free(tres_str);
  free(time_2d);
  
}  /* end of SBMERGE.c */

int sb_rd_input1(n_fil)
     char* n_fil;
{
  int parstat=0;
  
  Uclgst("num_file",n_fil,&parstat);
  if(parstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Check the number !");
    exit(1);
  }
  return parstat;
}

int sb_rd_input2(fil)
     char* fil;
{
  int parstat=0;

  Uclgst("infile",fil,&parstat);
  if(parstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get input file name.");
    XTE_Fcecho("Check input file name !");
    exit(1);
  }
  return parstat;
} 

int sb_rd_input3(ofil)
     char* ofil;
{
  int parstat=0;
  
  Uclgst("outfile",ofil,&parstat);
  if(parstat != 0){
    XTE_Fcecho(" ");
    XTE_Fcecho("Could not get output file name.");
    XTE_Fcecho("Check output file name !");
    exit(1);
  }
  return parstat;
}

int sb_rd_input4(int *clobb)
{
  int parstat=0;
  
  Uclgsb("clobber",clobb,&parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading clobber parameter");
    exit(1);
  }
  return parstat;
}

int index_loc(str,t)
     char str[],t[];
{
  int i,j,k;
  
  for(i=0;str[i] != '\0'; i++){
    for(j=i,k=0;t[k] != '\0' && str[j] == t[k]; j++,k++)
      ;
    if(t[k] == '\0')
      return(i);
  }
  return(-1);
}

/* This code is needed by IRAF */

#ifdef vms
#define F77CALL sbmerg
#endif
#ifdef unix
#define F77CALL sbmerg_
#endif

void F77CALL()
{
  void Sbmerge();
  Sbmerge();
}
