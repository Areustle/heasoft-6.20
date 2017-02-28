/*
Filename:	sa2phaII.c

Purpose:	Converts a Science Array file into a Type II PHA file
		and allows binning on time resolution.

Author:		Zhiyu Guo

Date:		Nov. 1997

History:

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

#define STR_KEY_LEN 256	/*Define the size of string value in a keyword */
#define SPEC_CHAN 129	/*Define the number of spectral channel in SA files */
#define NUM_FIELDS 30	/*Define the number of anode wire layers in PCUs */

extern void XTE_Fcecho();
int sa_rd_param();

void sa2phaII()
{
    	fitsfile *fptr;    /* pointer to the FITS file, defined in fitsio.h */
	fitsfile *outptr;
    	int status, hdutype,  anynull, hdu_num;
	int morekeys,tfields,num_chan,chan_lo,chan_hi,dok,hok;
	int syserr,grouping,quality,staterr;
	long frow, felem,nelem,nrows,newbin,resbin,naxes2,longnull;
	long** counts;
	long i,j,k,kk,bin_factor1;
	long* specnum;
	long** chan; 
	long** counts1; 
	long** counts_bin;
	double tstart,tstop,exposure;
	double* time; 
	double* time1; 
	float areascal,backscal,corrscal;
	char nfound[STR_KEY_LEN], bin_factor[STR_KEY_LEN];
	char telescope[STR_KEY_LEN],instrume[STR_KEY_LEN];
	char cpix2[STR_KEY_LEN];
 
	char infile[STR_KEY_LEN],outfile[STR_KEY_LEN]; 

/* Define keyword values for the output file */

	char extname[]="SPECTRUM";
	char *ttype[]={"SPEC_NUM", "CHANNEL", "COUNTS", "ROWID"};
	char *tform[]={"I", "129I", "129D", "D"};
	char *tunit[]={" ", "channel", "count", " "};

	status = 0;
	
XTE_Fcecho(" ");
XTE_Fcecho("Running SA2PHAII version 1.0");
XTE_Fcecho("=============================================");

status=sa_rd_param(infile,outfile,bin_factor);
if(status !=0){
	XTE_Fcecho(" ");
	XTE_Fcecho("Could not complete sa_rd_param call");
	XTE_Fcecho("Terminate run");
	exit(1);
} 
bin_factor1=atoi(bin_factor);
resbin=0;
	
/* Open input file */

    fits_open_file(&fptr, infile, READONLY, &status);

/* Create an empty file for writing to */

	fits_create_file(&outptr,outfile,&status);
	if(status != 0){
	printf("\n Can't create the output file, it may already exist ! \n\n");
	exit(1);
	}

	morekeys=0;

/* Copy the primary header of the input file to the file just created */

	fits_copy_hdu(fptr, outptr, morekeys, &status);

fits_write_chksum(outptr,&status);

/* Now move to the first extension of the input file */

	hdu_num=2;
	fits_movabs_hdu(fptr,hdu_num,&hdutype,&status);

/* Read 'NAXIS2' value to determine the number of rows */

        fits_read_key(fptr, TLONG, "NAXIS2",  &naxes2,nfound,&status);
	fits_read_key(fptr,TSTRING,"1CPIX2",cpix2,nfound,&status);

/* Allocat memory to arrays */

if(!(specnum=(long*) malloc(naxes2*sizeof(long)))){
	printf("Can't allocate memory to array 'specnum', exit !!!\n");
	exit(1);
} 

if(!(time=(double*) malloc(naxes2*sizeof(double)))){
	printf("Can't allocate memory to array 'time', exit !!!\n");
        exit(1);
}

frow=1;
felem=1;
nelem=naxes2 * SPEC_CHAN;
longnull=0;

/* Read column values from the first extension of the input file and write
   them to a column in the binary extension of the output file created above */

/* allocate memory to the array 'counts' */

counts=(long**) calloc(naxes2,sizeof(long*));
for(i=0;i<naxes2;i++){
        counts[i]=(long*) calloc(SPEC_CHAN,sizeof(long));
}

k=0;
while(k < NUM_FIELDS){
counts1=(long**) calloc(naxes2,sizeof(long*));
for(i=0;i<naxes2;i++){
        counts1[i]=(long*) calloc(SPEC_CHAN,sizeof(long));
}
	kk = k+2; 
for(i=0;i<naxes2;i++){
        fits_read_col(fptr, TLONG, kk, i+1, felem, SPEC_CHAN,
                &longnull, counts1[i], &anynull, &status);
}

for(i=0;i<SPEC_CHAN;i++){ 
for(j=0;j<naxes2;j++){  
	counts[j][i] += counts1[j][i];
} 
}
k++;
}

/*Calculate the number of rows after binning, and bin data */
 
if((naxes2 % bin_factor1) > 0){
        newbin=naxes2/bin_factor1+1;

	counts_bin=(long**) calloc(newbin,sizeof(long*));
	for(i=0;i<newbin;i++){
		counts_bin[i]=(long*) calloc(SPEC_CHAN,sizeof(long));
	}

	for(i=0;i<SPEC_CHAN;++i){
		for(j=0;j<newbin-1;++j){
			for(k=0;k<bin_factor1;++k){
				counts_bin[j][i] += counts[bin_factor1*j+k][i];
			}
		}
	}
	resbin=naxes2 % bin_factor1;
	for(i=0;i<SPEC_CHAN;++i){
		for(j=0;j<resbin;++j){
		       counts_bin[newbin-1][i] += counts[naxes2-resbin-1+j][i];
		}
	}
}else{
        newbin=naxes2/bin_factor1;

        counts_bin=(long**) calloc(newbin,sizeof(long*));
        for(i=0;i<newbin;i++){
                counts_bin[i]=(long*) calloc(SPEC_CHAN,sizeof(long));
        }

        for(i=0;i<SPEC_CHAN;++i){
                for(j=0;j<newbin;++j){
                        for(k=0;k<bin_factor1;++k){
                                counts_bin[j][i] += counts[bin_factor1*j+k][i];
                        }
                }
        }
}

/* Create a binary extension in the output file created above */
 
        frow= 1;
        felem= 1;
        nelem= newbin*SPEC_CHAN;	/*reset for binning*/
        longnull= 0;
tfields=4;
nrows=newbin;	/*reset for binning*/

fits_create_tbl(outptr,BINARY_TBL,nrows,tfields,ttype,tform,tunit,
extname,&status);


/*read in time bins*/

fits_read_col(fptr, TDOUBLE,1,frow,felem,naxes2,
                &longnull,time,&anynull,&status);

/*create an array for CHANNEL field */

chan=(long**) calloc(newbin,sizeof(long*));
for(i=0;i<newbin;i++){
	chan[i]=(long*) calloc(SPEC_CHAN,sizeof(long));
}

for(i=0;i<SPEC_CHAN;++i){
	for(k=0;k<newbin;++k){

		chan[k][i]=i+1;
}
}

if(!(time1=(double*) malloc(newbin*sizeof(double)))){
        printf("Can't allocate memory to array 'time1', exit !!!\n");
        exit(1); 
}

/*generate new time bin, simply use the time at the beginning of each bin */

if((naxes2 % bin_factor1) > 0){
	for(k=0;k<newbin-1;++k){
		specnum[k]=k+1;
		time1[k]=time[bin_factor1*k];
	}
	specnum[newbin-1]=newbin;
	time1[newbin-1]=time[naxes2-resbin];
}else{
	for(k=0;k<newbin;++k){
                specnum[k]=k+1;
                time1[k]=time[bin_factor1*k];
        }
}

staterr=0;
fits_write_key(outptr,TINT,"STAT_ERR",&staterr,nfound,&status);
fits_modify_comment(outptr,"STAT_ERR",
        "No statistical error specified",&status);

fits_write_key(outptr,TLOGICAL,"POISSERR","T",nfound,&status);
fits_modify_comment(outptr,"POISSERR",
	"Are Poisson Distribution errors assumed",&status);

fits_write_key(outptr,TSTRING,"HDUCLASS","OGIP",nfound,&status);
fits_modify_comment(outptr,"HDUCLASS",
	"format conforms to OGIP/GSFC Spectral standards",&status);

fits_write_key(outptr,TSTRING,"HDUCLAS1","SPECTRUM",nfound,&status); 
fits_modify_comment(outptr,"HDUCLAS1",
        "Extension contains a spectrum",&status);

fits_write_key(outptr,TSTRING,"HDUCLAS2","TOTAL",nfound,&status);
fits_modify_comment(outptr,"HDUCLAS2",
        "Extension contains a spectrum",&status);

fits_write_key(outptr,TSTRING,"HDUCLAS3","TYPE:II",nfound,&status);
fits_modify_comment(outptr,"HDUCLAS3",
        "Multiple PHA file contained",&status);

fits_write_key(outptr,TSTRING,"HDUCLAS4","COUNT",nfound,&status);
fits_modify_comment(outptr,"HDUCLAS4",
        "Extension contains counts",&status);

fits_write_key(outptr,TSTRING,"CHANTYPE","PHA",nfound,&status);
fits_modify_comment(outptr,"CHANTYPE",
        "Channels assigned by detector electronics",&status);

num_chan=SPEC_CHAN;
chan_lo=0;
chan_hi=SPEC_CHAN - 1;
syserr=0;
grouping=0;
quality=0;
areascal=1.0;
backscal=1.0;
corrscal=0.0;

/* Write key words values and comments in the extension */

fits_write_key(outptr,TINT,"SYS_ERR",&syserr,nfound,&status);
fits_modify_comment(outptr,"SYS_ERR",
        "No systematic error was specified",&status);

fits_write_key(outptr,TINT,"GROUPING",&grouping,nfound,&status);
fits_modify_comment(outptr,"GROUPING",
        "No grouping data has been specified",&status);

fits_write_key(outptr,TINT,"QUALITY",&quality,nfound,&status);
fits_modify_comment(outptr,"QUALITY",
        "No data quality information specified",&status);

fits_read_key(fptr,TSTRING,"TELESCOPE",telescope,nfound,&status);
fits_write_key(outptr,TSTRING,"TELESCOPE",telescope,nfound,&status);
fits_modify_comment(outptr,"TELESCOPE",
        "Telescope (mission) name",&status);

fits_read_key(fptr,TSTRING,"INSTRUME",instrume,nfound,&status);
fits_write_key(outptr,TSTRING,"INSTRUME",instrume,nfound,&status);
fits_modify_comment(outptr,"INSTRUME",
        "Instrument name",&status);

fits_write_key(outptr,TSTRING,"FILTER","none",nfound,&status);
fits_modify_comment(outptr,"FILTER",
        "Instrument filter in use",&status);

fits_write_key(outptr,TFLOAT,"AREASCAL",&areascal,nfound,&status);
fits_modify_comment(outptr,"AREASCAL",
        "Nominal effective area",&status);

fits_write_key(outptr,TFLOAT,"BACKSCAL",&backscal,nfound,&status);
fits_modify_comment(outptr,"BACKSCAL",
        "Background scale factor",&status);

fits_write_key(outptr,TFLOAT,"CORRSCAL",&corrscal,nfound,&status);
fits_modify_comment(outptr,"CORRSCAL",
        "Correlation scale factor",&status);

fits_write_key(outptr,TSTRING,"BACKFILE","none",nfound,&status);
fits_modify_comment(outptr,"BACKFILE",
        "Background FITS file for this object",&status);

fits_write_key(outptr,TSTRING,"CORRFILE","none",nfound,&status);
fits_modify_comment(outptr,"CORRFILE",
        "Correlation FITS file for this object",&status);

fits_write_key(outptr,TSTRING,"RESPFILE","none",nfound,&status);
fits_modify_comment(outptr,"RESPFILE",
        "Redistribution matrix file (RMF)",&status);

fits_write_key(outptr,TSTRING,"ANCRFILE","none",nfound,&status);
fits_modify_comment(outptr,"ANCRFILE",
        "Ancillary response file (ARF)",&status);

fits_write_key(outptr,TSTRING,"XFLT0001","none",nfound,&status);
fits_modify_comment(outptr,"XFLT0001",
	"XSPEC selection filter description",&status);

fits_write_key(outptr,TINT,"DETCHANS",&num_chan,nfound,&status);
fits_modify_comment(outptr,"DETCHANS",
        "Total number of detector channels available",&status);

fits_write_key(outptr,TINT,"TLMIN2",&chan_lo,nfound,&status);
fits_modify_comment(outptr,"TLMIN2",
        "Lowest legal channel number",&status);

fits_write_key(outptr,TINT,"TLMAX2",&chan_hi,nfound,&status);
fits_modify_comment(outptr,"TLMAX2",
        "Highest legal channel number",&status);

/*
fits_write_key(outptr,TSTRING,"CPIX2",
	"0~4,5:53,(54~135;2),(136~237;3),(238~249;4),250~255",
	nfound,&status);
fits_modify_comment(outptr,"CPIX2","Channel binning",&status);
*/

fits_write_key(outptr,TSTRING,"1CPIX3",cpix2,nfound,&status);
fits_modify_comment(outptr,"1CPIX3","Channel binning",&status);

fits_write_key(outptr,TSTRING,"PHAVERSN","1992a",nfound,&status);
fits_modify_comment(outptr,"PHAVERSN",
        "OGIP memo number for file format",&status);

/*Initialization*/
tstart=1.0e12*0.0;
tstop=1.0e12*0.0;
exposure=1.0e12*0.0;

fits_read_key(fptr,TDOUBLE,"TSTART",&tstart,nfound,&status);

fits_read_key(fptr,TDOUBLE,"TSTOP",&tstop,nfound,&status);

/* exposure=tstop-tstart; */
exposure=16.0*bin_factor1;

fits_write_key(outptr,TDOUBLE,"EXPOSURE",&exposure,nfound,&status);
fits_modify_comment(outptr,"EXPOSURE",
        "Exposure time",&status);

/* Write the accumulated array and other columns to a column in the output 
   file */

for(i=0;i<newbin;i++){
fits_write_col(outptr,TLONG,3,i+1,felem,SPEC_CHAN,counts_bin[i],&status); 
}

free(counts);
free(counts1);
free(counts_bin);

for(i=0;i<newbin;i++){
fits_write_col(outptr,TLONG,2,i+1,felem,SPEC_CHAN,chan[i],&status);
}

fits_write_col(outptr,TLONG,1,frow,felem,newbin,specnum,&status);

fits_write_col(outptr,TDOUBLE,4,frow,felem,newbin,time1,&status); 

fits_write_chksum(outptr,&status);  
fits_verify_chksum(outptr,&dok,&hok,&status);

free(chan);

/* Close the files */

fits_close_file(fptr,&status);
fits_close_file(outptr,&status);
}

int sa_rd_param(ifile,ofile,bin)
	char *ifile, *ofile;
	char *bin;
{
int parstat=0;
int BufLen_2=255;

Uclgst("infile",ifile,&parstat);
if(parstat != 0){
	XTE_Fcecho(" ");
	XTE_Fcecho("Could not get input file name.");
	XTE_Fcecho("Check input file name !");
	exit(1);
}

Uclgst("outfile",ofile,&parstat);
if(parstat != 0){
        XTE_Fcecho(" ");
        XTE_Fcecho("Could not get output file name.");
        XTE_Fcecho("Check output file name !");
        exit(1);
}

Uclgst("bin_factor",bin,&parstat);
if(parstat != 0){
        XTE_Fcecho(" ");
        XTE_Fcecho("Check the number !");
        exit(1);
}
return parstat;
}

/* This code is needed by IRAF */

#ifdef vms
#define F77CALL sa2phaII
#endif
#ifdef unix
#define F77CALL sa2phaII_
#endif

void F77CALL()
{
void sa2phaII();
sa2phaII();
}
