/*
  FILENAME:    correctlc.c
  author:      M. J. Tripicco
  date:        December 1999

  version: 0.1
*/

#include <stdio.h>
#include <string.h>
#include <errno.h> 
#include "xpi.h"
#include "fitsio.h"

#ifndef TRUE
#define TRUE  1             /* Define a logical TRUE value */
#endif

#ifndef FALSE
#define FALSE 0             /* Define a logical FALSE value */
#endif

#define ERRMSG 255          /* Must be bigger than FLEN_ERRMSG */
#define MXELEM 1            /* Allocate for number of columns for files */

#define MAXC_FNAME 256      /* Define the size of the arrays for C */
#define FITS_CLEN_HDEFKWDS  25 /* used to be defined in cfitsio.h... */

int clcpar(char *, char *, char *, char *, char *, int *);

typedef struct {
    double timezero;
    double timedel;
    long nrows;
    double *time;
    int *pcuon[5];
    int nullval;
    int anynull[5];
} xtefiltfile ;

void Correctlc()
{
    int status=0, pstat=0, xtend, cnum=0, maxpcuon, numpcuon, statechange;
    int cnumrate, cnumerr, timeref;
    int i, j, k, l, hdunum, clobber, pcorrval;
    static char taskname[20] = "CORRECTLC_v0.1";
    char msg[ERRMSG], datestr[30];
    char infile[MAXC_FNAME], outfile[MAXC_FNAME], filtfile[MAXC_FNAME], outfil[MAXC_FNAME];
    char method[16], pculist[18];
    char nullkw[FLEN_VALUE], histext[MAXC_FNAME];
    char pcustr[2], pcuonid[6], maxpcuid[6], pcuchar[1];
    long lcnrows;
    double *lctime, *lcrate, *lcerr, lctimezero, lctimedel, tmpdbl;

    double pcusens[] = { 266.0, 266.0, 268.0, 252.0, 249.0 };
    double mult_num = 0.0, dnull = 9.9999E+99, mult_denom;

    xtefiltfile filtinfo;

    fitsfile *ffp, *ifp, *ofp;
    
    /* Define the TASK common block */
    c_ptaskn(taskname);
    
    c_fcecho(" ");
    sprintf(msg,"Running %s\n========================",taskname);
    c_fcecho(msg);
    
    /* Read the Parameter file */
    status=clcpar(infile,filtfile,outfil,method,pculist,&clobber); 
    if(status != 0){
	c_fcerr("Error reading parameter file");
	c_fcerr("Buh bye!");
	exit(1);
    }
    if (clobber == 1) {
	strcpy(outfile,"!");
	strcat(outfile,outfil);
    } else strcpy(outfile,outfil);
    
    /* Read in relevant info from filter file */
    fits_open_file(&ffp, filtfile, READONLY, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Could not open filter file");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    fits_movabs_hdu(ffp, 2, &xtend, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Could not move to proper extension in filter file");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    fits_read_key_dbl(ffp, "TIMEDEL", &filtinfo.timedel, NULL, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error reading TIMEDEL keyword");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    fits_read_key_dbl(ffp, "TIMEZERO", &filtinfo.timezero, NULL, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error reading TIMEZERO keyword");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    fits_get_num_rows(ffp, &filtinfo.nrows, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error getting number of rows");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    filtinfo.time = (double *) malloc(filtinfo.nrows * sizeof(double));
    fits_read_col(ffp, TDOUBLE, 1, 1L, 1L, filtinfo.nrows, 0, filtinfo.time, NULL,  &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error reading filter file times");
	fits_report_error(stderr, pstat);
	exit(1);
    }

    fits_get_colnum(ffp, CASEINSEN, "PCU0_ON", &cnum, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Can't find PCU0_ON column");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    fits_make_keyn("TNULL", cnum, nullkw, &pstat);
    fits_read_key(ffp, TINT, nullkw, &filtinfo.nullval, NULL, &pstat); 
    /* hereafter we assume all PCU#_ON nullvals are equal */
    filtinfo.pcuon[0] = (int *) malloc(filtinfo.nrows * sizeof(int));
    fits_read_col(ffp, TINT, cnum, 1L, 1L, filtinfo.nrows, &filtinfo.nullval, 
		  filtinfo.pcuon[0], &filtinfo.anynull[0], &pstat);
    fits_get_colnum(ffp, CASEINSEN, "PCU1_ON", &cnum, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Can't find PCU1_ON column");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    filtinfo.pcuon[1] = (int *) malloc(filtinfo.nrows * sizeof(int));
    fits_read_col(ffp, TINT, cnum, 1L, 1L, filtinfo.nrows, &filtinfo.nullval, 
		  filtinfo.pcuon[1], &filtinfo.anynull[1], &pstat);
    fits_get_colnum(ffp, CASEINSEN, "PCU2_ON", &cnum, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Can't find PCU2_ON column");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    filtinfo.pcuon[2] = (int *) malloc(filtinfo.nrows * sizeof(int));
    fits_read_col(ffp, TINT, cnum, 1L, 1L, filtinfo.nrows, &filtinfo.nullval, 
		  filtinfo.pcuon[2], &filtinfo.anynull[2], &pstat);
    fits_get_colnum(ffp, CASEINSEN, "PCU3_ON", &cnum, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Can't find PCU3_ON column");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    filtinfo.pcuon[3] = (int *) malloc(filtinfo.nrows * sizeof(int));
    fits_read_col(ffp, TINT, cnum, 1L, 1L, filtinfo.nrows, &filtinfo.nullval, 
		  filtinfo.pcuon[3], &filtinfo.anynull[3], &pstat);
    fits_get_colnum(ffp, CASEINSEN, "PCU4_ON", &cnum, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Can't find PCU4_ON column");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    filtinfo.pcuon[4] = (int *) malloc(filtinfo.nrows * sizeof(int));
    fits_read_col(ffp, TINT, cnum, 1L, 1L, filtinfo.nrows, &filtinfo.nullval, 
		  filtinfo.pcuon[4], &filtinfo.anynull[4], &pstat);
    fits_close_file(ffp, &pstat);

    /* figure out the appropriate pculist for "max" case */
    if (!strcmp(method, "max")) {
	maxpcuon = 0;
	strcpy(maxpcuid,"");
	for (i=0;i<filtinfo.nrows;i++) {
	    numpcuon = 0;
	    strcpy(pcuonid,"");
	    for (j=0;j<=4; j++) {
		if (*(filtinfo.pcuon[j]+i) == 1) {
		    numpcuon++;
		    sprintf(pcustr,"%d",j);
		    strcat(pcuonid,pcustr);
		}
	    }
	    if (numpcuon == maxpcuon && strcmp(pcuonid,maxpcuid)) {
		sprintf(msg,"Whoops! Row %d has %d on (%s), previous max is %d (%s)\n",
		       i, numpcuon, pcuonid, maxpcuon, maxpcuid);
		c_fcerr(msg);
		c_fcerr("Please specify by setting \"method\" parameter to \"user\"");
		exit(1);
	    }
	    if (numpcuon > maxpcuon) {
		maxpcuon = numpcuon;
	        strcpy(maxpcuid,pcuonid);
	    }
	}
	sprintf(msg,"Maximum number of PCUs on is %d (%s)",maxpcuon,maxpcuid);
	c_fcecho(msg);
	strcpy(pculist,maxpcuid);
    }

    /* determine numerator for rate multiplier */
    if (!strcmp(method,"one")) {
	mult_num = 260.2;
	strcpy(pculist,"<one average PCU>"); /* to be used in HISTORY kwd */
    } else {
	for (i=0;i<strlen(pculist);i++) {
	    sprintf(pcuchar,"%c",pculist[i]);
	    mult_num += pcusens[(int) atoi(pcuchar)];
	}
    }

    /* get RATE/ERROR columns from lightcurve */
    fits_open_file(&ifp, infile, READONLY, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Could not open input lightcurve file");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    fits_movabs_hdu(ifp, 2, &xtend, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Could not move to proper extension in lightcurve file");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    fits_read_key(ifp, TLOGICAL, "PCU_CORR", &pcorrval, NULL, &pstat);
    if(pstat != 0){
	pstat = 0; /* kwd doesn't exist --> no correction applied */
    } else if (pcorrval == 1) {
	c_fcerr(" ");
	c_fcerr("Error: PCU Correction already applied!");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    fits_get_num_rows(ifp, &lcnrows, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error getting number of rows");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    lctime = (double *) malloc(lcnrows * sizeof(double));
    fits_read_col(ifp, TDOUBLE, 1, 1L, 1L, lcnrows, 0, lctime, NULL,  &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error reading TIME column");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    fits_get_colnum(ifp, CASEINSEN, "RATE", &cnumrate, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Can't find RATE column");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    lcrate = (double *) malloc(lcnrows * sizeof(double));
    fits_read_col(ifp, TDOUBLE, cnumrate, 1L, 1L, lcnrows, 0, lcrate, NULL,  &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error reading RATE column");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    fits_get_colnum(ifp, CASEINSEN, "ERROR", &cnumerr, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Can't find ERROR column");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    lcerr = (double *) malloc(lcnrows * sizeof(double));
    fits_read_col(ifp, TDOUBLE, cnumerr, 1L, 1L, lcnrows, 0, lcerr, NULL,  &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error reading ERROR column");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    /* find TIMEZERO or TIMEZER[IF] */
    fits_read_key_dbl(ifp, "TIMEZERO", &lctimezero, NULL, &pstat);
    if(pstat != 0){
	pstat = 0;
	fits_read_key_dbl(ifp, "TIMEZERI", &tmpdbl, NULL, &pstat);
	fits_read_key_dbl(ifp, "TIMEZERF", &lctimezero, NULL, &pstat);
	if (pstat != 0) {
	    c_fcerr(" ");
	    c_fcerr("Error reading TIMEZERO keyword");
	    fits_report_error(stderr, pstat);
	    exit(1);
	}
	lctimezero += tmpdbl;
    }
    /* find TIMEDEL */
    fits_read_key_dbl(ifp, "TIMEDEL", &lctimedel, NULL, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error reading lc TIMEDEL keyword");
	fits_report_error(stderr, pstat);
	exit(1);
    }

    /* correct lc rate/error data arrays */
    for (i=0;i<lcnrows;i++) { 
	statechange=0;
	mult_denom=0.0;
	/* find first relevant filterfile row (j) for each lc row (i) */
	for (j=0;j<filtinfo.nrows-1;j++) {
	    if ((*(lctime+i)+lctimezero >= *(filtinfo.time+j)+filtinfo.timezero) && 
		(*(lctime+i)+lctimezero < *(filtinfo.time+j+1)+filtinfo.timezero)) break; 
	}
	/*	printf("\nlctime %lf (%lf), fftime %lf (%lf)\n",
	 *(lctime+i),*(lctime+i)+lctimezero,*(filtinfo.time+j),*(filtinfo.time+j)+filtinfo.timezero); */
	/* find out if PCA state changed during lc bin */
	for (k=j;k<filtinfo.nrows-1;k++) {
	    if ((*(lctime+i)+lctimezero+lctimedel >= *(filtinfo.time+k)+filtinfo.timezero) &&
		(*(lctime+i)+lctimezero+lctimedel < *(filtinfo.time+k+1)+filtinfo.timezero)) break; 
	}
	if (k == j) k++;
	/* When lc bins are < ff timedel we look at next ff row for state change.
	   But when lc bins are > ff timedel we need to look at the ff row corresponding 
	   to the endpoint of the lc bin */
	for (l=0;l<=4;l++) {
	    /*printf("ff row %d: PCU%d %d \n",j, l, *(filtinfo.pcuon[l]+j));
	      printf("ff row %d: PCU%d %d \n",k, l, *(filtinfo.pcuon[l]+k));*/
	    if (*(filtinfo.pcuon[l]+k) != *(filtinfo.pcuon[l]+j)) {
		/*printf("PCA state change! Writing INDEF in lc row %ld\n",i+1);*/
		statechange=1;
		break;
	    }
	    mult_denom += *(filtinfo.pcuon[l]+j) * pcusens[l];
	}
	if (statechange == 1) {
	    *(lcrate+i) = dnull;
	    *(lcerr+i) = dnull;
	} else if (mult_denom > 0.0) {
	    /*printf("applying correction factor: %lf\n",mult_num/mult_denom);*/
	    *(lcrate+i) = *(lcrate+i) * mult_num / mult_denom;
	    *(lcerr+i) = *(lcerr+i) * mult_num / mult_denom;
	}
    }

    /* copy original lightcurve FITS file */
    fits_create_file(&ofp, outfile, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error creating output file (set \"clobber\" to overwrite)");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    fits_get_num_hdus(ifp, &hdunum, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error determining number of HDUs");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    for (i=1;i<=hdunum;i++) {
	fits_movabs_hdu(ifp, i, &xtend, &pstat);
	if(pstat != 0){
	    c_fcerr(" ");
	    c_fcerr("Error moving to HDU in lightcurve file");
	    fits_report_error(stderr, pstat);
	    exit(1);
	}
	fits_copy_hdu(ifp, ofp, 0, &pstat);
	if(pstat != 0){
	    c_fcerr(" ");
	    c_fcerr("Error copying HDU");
	    fits_report_error(stderr, pstat);
	    exit(1);
	}
    }
    fits_close_file(ifp, &pstat);

    /* write corrected rate/error columns into copied file */
    fits_movabs_hdu(ofp, 2, &xtend, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error moving to HDU2 in output lightcurve file");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    fits_write_colnull(ofp, TDOUBLE, cnumrate, 1L, 1L, lcnrows, 
		       lcrate, &dnull, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error writing RATE to output lightcurve file");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    fits_write_colnull(ofp, TDOUBLE, cnumerr, 1L, 1L, lcnrows, 
		       lcerr, &dnull, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error writing ERROR to output lightcurve file");
	fits_report_error(stderr, pstat);
	exit(1);
    }
    /* insert keywords to indicate that correction has been done */
    fits_write_key(ofp, TLOGICAL, "PCU_CORR", "T",
		   "lightcurve corrected for PCU on/off status?", &pstat);
    strcpy(histext, "RATE/ERROR columns modified by ");
    strcat(histext, taskname);
    strcat(histext, ": ");
    fits_get_system_time(datestr, &timeref,  &pstat);
    strcat(histext, datestr);
    strcat(histext, " for the following list of PCUs: ");
    strcat(histext, pculist);
    fits_write_history(ofp, histext, &pstat);
    if(pstat != 0){
	c_fcerr(" ");
	c_fcerr("Error writing new keywords to output file");
	fits_report_error(stderr, pstat);
	exit(1);
    }

    /* fix checksums and close up */
    fits_write_chksum(ofp, &pstat);
    fits_close_file(ofp, &pstat);
    sprintf(msg,"Lightcurve successfully corrected to PCUs: ");
    strcat(msg, pculist);
    c_fcecho(msg);
}

/*
  Some processing of input parameters takes place in clcpar() below.

  If user chooses method "max" or "one" (or a sensible variant) the
  method parameter is rationalized to "max" or "one"; pculist isn't
  prompted for and is returned as empty (to be dealt with by the main
  routine). If the input method parameter begins with [uU] then user
  is prompted for pculist string, which is stripped of all non-digits
  and returned.
*/
int clcpar(char *ifn, char *ffn, char *ofn, char *meth, char *plst, int *clobb)
{
    int BufLen_2, i, j;
    char text[FLEN_STATUS], msg[81], userplst[16];
    int parstat=0;
    
    BufLen_2=255; /* always must be 1 less than char array size!!!!!! */
    Uclgst("infile", ifn, &parstat);
    if(parstat != 0){
	c_fcerr(" ");
	c_fcerr("Could not get infile parameter");
	fits_get_errstatus(parstat, text);
    }

    Uclgst("filtfile", ffn, &parstat);
    if(parstat != 0){
	c_fcerr(" ");
	c_fcerr("Could not get filtfile parameter");
	fits_get_errstatus(parstat, text);
    }
    
    Uclgst("outfile", ofn, &parstat);
    if(parstat != 0){
	c_fcerr(" ");
	c_fcerr("Could not get outfile parameter");
	fits_get_errstatus(parstat, text);
    }
    
    BufLen_2=15;
    Uclgst("method", meth, &parstat);
    if(parstat != 0){
	c_fcerr(" ");
	c_fcerr("Could not get method parameter");
	fits_get_errstatus(parstat, text);
    }
    
    if (meth[0] == 'u' || meth[0] == 'U') {
	strcpy(meth,"user");
	Uclgst("pculist", userplst, &parstat);
	if(parstat != 0){
	    c_fcerr(" ");
	    c_fcerr("Could not get pculist parameter");
	    fits_get_errstatus(parstat, text);
	}
	j=0;
	for (i=0;i<strlen(userplst);i++){
	    if (isdigit(userplst[i])) {
		plst[j]=userplst[i];
		j++;
	    }
	}
	plst[j]='\0';
    } else if (meth[0] == 'o' || meth[0] == 'O' || meth[0] == '1') {
	strcpy(meth,"one");
    } else if (meth[0] == 'm' || meth[0] == 'M') {
	strcpy(meth,"max");
    } else {
	sprintf(msg,"Illegal method: %s\n  Legal values are: max, one, or user",meth);
	c_fcerr(msg);
	return 1;
    }
    
    Uclgsb("clobber", clobb, &parstat);
    if(parstat != 0){
	c_fcerr(" ");
	c_fcerr("Could not get clobber parameter");
	fits_get_errstatus(parstat, text);
    }
    return parstat;
}








/* This code is needed by IRAF */
  
#ifdef vms
#define F77CALL corrlc
#endif
#ifdef unix
#define F77CALL corrlc_
#endif

void F77CALL() 
{ 
  void Correctlc();
  
  Correctlc(); 
}
