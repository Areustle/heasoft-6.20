/***************************************************
  correctpileup.c 
  1997/07/25 ver 1.0 by taro kotani kotani@riken.go.jp
  1997/11/29 ver 1.1 fixed minor bugs, output proper error
  1997/12/02 ver 1.2 inefficient loop was removed. 1.5 times faster
  1999/11/02 ver 1.3 ftoolization.   Ning Gan
  2000/01/11 ver 1.4 added mandatory FILTER keyword.
                     Removed blank image in the primary.
		     disr may be specified  from
		     parameter.  Ken Ebisawa
  2000/01/13 ver 1.5 added clobber parameter  Ning Gan.
                     Bug fix by  Taro Kotani.
                     Output raw image and spectrum
		     (img0 and pha0 parameters added).
		     Replaced fprintf with c_fcecho or c_fcerr.
		     Channel number is from 0 to 511 (used to be
		     1-512).
                     Other minor interface and style changes.
                     by Ken Ebisawa
  2001/06/13 ver 1.6 Presence of the EVENT extenstion in the
                     *first* extension is no long assumed.
                     by Ken Ebisawa
***************************************************/
#define VERSION "corpileup ver 1.6 kotani@milkyway.gsfc.nasa.gov"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <limits.h>

#include "fitsio.h"
#include "pfile.h"
#include "cftools.h"

#ifndef RAND_MAX
#define RAND_MAX LONG_MAX
#endif

#define  BufLen_2  FLEN_FILENAME-1      /* string parameter maximum length */

#define MAXRAWX 425
#define MAXRAWY 422
#define MINRAWX 6
#define MINRAWY 1
#define MAXSKYX 1280
#define MAXSKYY 1280
#define MINSKYX 1
#define MINSKYY 1
#define DEFAULTSOURCESKYX 475.	/* tentative source position */
#define DEFAULTSOURCESKYY 791.
#define DEFAULTTIMEBIN 7.2e2
#define SEARCHRADIUS 50. /* area to search true source position */
#define EXPOSTIME 4.

#define BRIGHT 1
#define SKY 0
#define DET 1
#define RAW 2

#define PSFSTEP (0.1/0.027)	/* psf calculation step (mm) / pixel size */
#define FLUXTHRESHOLD	0.023	/* area higher than this flux (c/pix/exp) is discarded */
struct event {double time; int pi; short rawx, rawy, grade;};

/* function prototypes */
int getparameters(int *status);
void  openeventsfile(void);
void  makepsf(void);
void  calcpileup(void);
void  correction(void);
int bitexpand(int); 
int isthisannevent(int**, int, int, int*);
int xy2r(short, short, double, double);
void accumlateskyimage(int **psfimage, double *centerskyx, double *centerskyy);
void writepsf(double psf[]);
void counteventnum(int *exposnum, int *startevent, int *eventnum, int *endofdata);
void loadevents(int startevent, int eventnum, struct event events[]);
void accumlateobservedevent(int eventnum, struct event events[], 
		       int ** obsimagnow, 
		       int obsphanow[4096]);
void findcenternow(int eventnum, struct event events[],
	      double *centerrawxnow, double *centerrawynow);
void maketemporalpsf(int exposnum, int eventnum, struct event events[],
		double centerx, double centery, 
		int *temporalpsf[], int psf[]);
void sortevents(int eventnum, struct event events[], struct event
	   *(sortedevent[]), double centerx, double centery);
void rainofphoton(int exposnum, int *temporalpsf[], int psfnow[], 
	     struct event *sortedevents[], double centerrawxnow, 
             double centerrawynow,
	     int simpha[4096],
	     int **inputimg, int inputpha[4096]);
void firstrain(int* psfnow, struct event *events[], int temporalpsf[], 
	  int **eventorder, int *counter,
	  int **inputimg, 
	  int inputpha[4096], int **simimg);
void randamizeevent(int psf, int eventorder[]);
void detectphoton(int** img, int rout, 
                  double centerx, double centery, int resultpsf[]);

void rain(struct event events[], int r, int eventnuminr,
     int requiredrain, int** inputimg, 
     int inputpha[4096], int ** simimg);
void distributecharge(struct event anevent, int** img);
void img2pha(int** img, int *pha);
void clearrawimg(int** img);
void clearpha(int pha[4096]);
void writefiles();
void writeimg(char *filename, int **img);
void writepha(char *outphafilename, int outpha[4096], int bgd[4096]);
void evaluateerror(float error[512], int pha[512], int bgd[512]);
void evaluatequality(short quality[512]);
void writestanderdheader(fitsfile *outfits);
void writephaheader(fitsfile *outfits);
void execio(int status);
void file_presence(char *file);

/* file names */
static char ineventfilename[FLEN_FILENAME];
static char correctedphafilename[FLEN_FILENAME]; 
static char rawimgfilename[FLEN_FILENAME];
static char outimgfilename[FLEN_FILENAME];
static char rawphafilename[FLEN_FILENAME];
static char simphafilename[FLEN_FILENAME];
static char totalinputphafilename[FLEN_FILENAME]; 
static char bgdphafilename[FLEN_FILENAME];
static char psffilename[FLEN_FILENAME];
static fitsfile *eventsfile;

static int totaleventnum, datamode, psfbin, sensor, ccd, evth, spth, coord;
static double tentx=DEFAULTSOURCESKYX, tenty=DEFAULTSOURCESKYY;
static double disr;
static int disrisspecified=FALSE;
static double centerrawx, centerrawy, centerdetx, centerdety;
static double timebin=DEFAULTTIMEBIN, exposure;
static int discardedradius;
static int** totalrawimg;
static int** totalinputimg;
static int totalobspha[4096], totalinputpha[4096], totalsimlatedpha[4096];
static int bgdpha[4096], correctedpha[4096];
static int clobber = 0;

 int corpileup(void){
   int status=0;
   char task[] = "corpileup V1.5"; 	/* task name */
   int i; 
   
   /* dynamic memory allocation */
   totalrawimg = (int **)malloc(sizeof(int *) * (MAXRAWX+1));
   totalinputimg = (int **)malloc(sizeof(int *) * (MAXRAWX+1));
   for (i = 0; i< MAXRAWX+1; i++) {
     totalrawimg[i] = (int *)malloc(sizeof(int ) * (MAXRAWY+1));
     totalinputimg[i] = (int *)malloc(sizeof(int ) * (MAXRAWY+1));
   }
  
   c_ptaskn(task);   	/* register the task name */
 
   getparameters(&status); 
   if(status) {
     c_fcerr("Error in reading parameters." );
     return 1;
   }
   openeventsfile();
   makepsf();
   calcpileup();
   correction(); 
   execio(fits_close_file(eventsfile, &status));
   writefiles(); 

   if(status) {
     c_fcerr("Error in corpileup." );
     return 1;
   }else{
     for (i = 0; i< MAXRAWX+1; i++) {
       free(totalrawimg[i]); 
       free(totalinputimg[i]); 
     }
     free(totalrawimg);
     free(totalinputimg);
     c_fcecho("corpileup completed." );
     return 0;
   }
 }
 
 /* get parameters */
 int getparameters(int *status){ 
   char coorsys[FLEN_FILENAME];     /* coordinate system */ 
   int i;
   double tentcenx, tentceny;
   char message[100];
   char *p;

    /* get name of the input fits file */
   *status = 0;
   Uclgst("inevent", ineventfilename, status);
   if(*status) {
     c_fcerr("could not get ineventfilename parameter" );
     return *status;
   }
   
   Uclgst("outpha", correctedphafilename, status);
   if(*status) {
     c_fcerr("could not get outpha parameter" );
     return *status;
   }
    
   Uclgsd("timebin",&timebin,status);
   if(*status) {
     c_fcerr("could not get timebin parameter" );
     return *status;
   }
   Uclgst("coorsys",coorsys,status);
   if(*status) {
     c_fcerr("could not get coorsys parameter" );
     return *status;
   }
   for (i = 0; i++; i<3) coorsys[i] = tolower(coorsys[i]);
   if(!strncmp(coorsys,"sky",3)) {        
     coord = SKY; 
   } else if (!strncmp(coorsys,"det",3)) { 
     coord = DET; 
   } else if (!strncmp(coorsys,"raw",3)) {
     coord = RAW; 
   } else { 
     c_fcerr("Unknown coordinate system, only sky, det or raw is allowed" );
     *status = 1;
     return *status;
   } 
 
   Uclgsd("tentcenx",&tentx,status);
   if(*status) {
     c_fcerr("could not get tentcenx parameter" );
     return *status;
   }
       
   Uclgsd("tentceny",&tenty,status);
   if(*status) {
     c_fcerr("could not get tentceny parameter" );
     return *status;
   }
   Uclgsd("disr",&disr,status);
   if(*status) {
     c_fcerr("could not get disr parameter" );
     return *status;
   }
   if(disr>=0.0){
     disrisspecified=TRUE;
     sprintf(message, "Truncation radius is set at %g", disr);
     c_fcecho(message);
     sprintf(message, "Events within this radius are discarded");
     c_fcecho(message);
   }else{
     sprintf(message, "Negative disr parameter %g is input.",disr);
     c_fcecho(message);
     sprintf(message, "Truncation radius will be calculated so that the PSF");
     c_fcecho(message);
     sprintf(message, "center where flux exceeds the 0.023 cnts/pixel/exposure");
     c_fcecho(message);
     sprintf(message, "threshold is discarded.");
     c_fcecho(message);
   }

   Uclgst("pha0", rawphafilename, status);
   if(*status) {
     c_fcerr("could not get pha1 parameter" );
     return *status;
   }

   Uclgst("pha1", totalinputphafilename, status);
   if(*status) {
     c_fcerr("could not get pha1 parameter" );
     return *status;
   }
   
   Uclgst("pha2", simphafilename, status);
   if(*status) {
     c_fcerr("could not get pha2 parameter" );
     return *status;
   }
   
   Uclgst("pha3", bgdphafilename, status);
   if(*status) {
     c_fcerr("could not get pha3 parameter" );
     return *status;
   }

   Uclgst("img0", rawimgfilename, status);
   if(*status) {
     c_fcerr("could not get imag0 parameter" );
     return *status;
   }

   Uclgst("img1", outimgfilename, status);
   if(*status) {
     c_fcerr("could not get imag1 parameter" );
     return *status;
   }
   
   Uclgst("psf", psffilename, status);
   if(*status) {
     c_fcerr("could not get psf parameter" );
     return *status;
   }
   
   Uclgsb("clobber",&clobber,status);
   if(*status) {
     c_fcerr("could not get clobber parameter" );
     return *status;
   } 
   /* Check presence of the files, and terminate if they exist and xclobber = no */
   file_presence(correctedphafilename);
   file_presence(rawphafilename);
   file_presence(totalinputphafilename);
   file_presence(simphafilename);
   file_presence(bgdphafilename);
   file_presence(rawimgfilename);
   file_presence(outimgfilename);
   file_presence(psffilename);
 }

 void openeventsfile(void){
   int status=0;
   char value[FLEN_VALUE], comment[FLEN_COMMENT], keyword[FLEN_KEYWORD];
   char message[100];


   execio(fits_open_file(&eventsfile, ineventfilename, READONLY, &status));
   execio(fits_read_key(eventsfile, TSTRING, "INSTRUME", value, comment, &status));
   if (strcmp(value, "SIS0") == 0){
     sensor=0;
   }else if (strcmp(value, "SIS1") == 0){
     sensor=1;
   }else{
     sprintf(message,"Error! unknown instrument: %s", value);
     c_fcerr(message);
     exit(-1);
   }
   sprintf(message, "sensor: %d", sensor);
   c_fcecho(message);

   execio(fits_read_key(eventsfile, TSTRING, "DATAMODE", value, comment, &status));
   if (strcmp(value, "BRIGHT") == 0){
     datamode = BRIGHT;
   }else{
     sprintf(message, "Error! unsupported datamode: %s", value);
     c_fcerr(message);
     exit(-1);
   }
   sprintf(message, "datamode: %s", value);
   c_fcecho(message);

   sprintf(keyword,"S%dCCDPOW", sensor);
   execio(fits_read_key(eventsfile, TSTRING, keyword, value, comment, &status));
   if (strcmp(value, "1000") == 0){
     ccd=0;
   }else if (strcmp(value, "0100") == 0){
     ccd=1;
   }else if (strcmp(value, "0010") == 0){
     ccd=2;
   }else if (strcmp(value, "0001") == 0){
     ccd=3;
   }else{
     sprintf(message, "Error! used ccd: %s", value);
     c_fcerr(message);
     exit(-1);
   }
   sprintf(message, "ccd: %d", ccd);
   c_fcecho(message);

   sprintf(keyword,"S%d_EVTR%d", sensor, ccd);
   execio(fits_read_key(eventsfile, TINT, keyword, &evth, comment, &status));
   sprintf(message, "event threshold: %d", evth);
   c_fcecho(message);
   sprintf(keyword,"S%d_SPTR%d", sensor, ccd);
   execio(fits_read_key(eventsfile, TINT, keyword, &spth, comment, &status));
   sprintf(message, "split threshold: %d", spth);
   c_fcecho(message);
   execio(fits_read_key(eventsfile, TDOUBLE, "EXPOSURE", &exposure, comment, &status));
   sprintf(message, "exposure: %g", exposure);
   c_fcecho(message);
 }
 
 void accumlateskyimage(int** psfimage, double *centerskyx, double *centerskyy){
   int nullvalint=0, status=0, searchedpixel=0;
   int hdutype, anynul, i, j, sumrawx=0, sumrawy=0, sumskyx=0, sumskyy=0;
   int sumdetx=0, sumdety=0;
   short skyx, skyy, rawx, rawy, detx, dety, nulvalshort=0;
   int pi, nulvalint=0;
   long nulvallong;
   char comment[FLEN_COMMENT];
   double r;
   char message[100];
   char keyvalue[8];
   int extnum;

   /*clear the PSF and raw image */   
   for (i=0; i <= MAXSKYX; i++)
     for (j=0; j <= MAXSKYY; j++)
       psfimage[i][j]=0;
   clearrawimg(totalrawimg);
   
   /* Search for the EVENT extenstion (2001-6-13 by K.E.)*/
   extnum = 2;
   strcpy(keyvalue,"");
   while(strstr(keyvalue,"EVENTS")==NULL&& status == 0){
     execio(fits_movabs_hdu(eventsfile, extnum, &hdutype, &status));
     execio(fits_read_key(eventsfile, TSTRING, "EXTNAME", keyvalue, comment, &status));
     extnum ++;
   }

   execio(fits_read_key(eventsfile, TINT, "NAXIS2", &totaleventnum, 
			comment, &status));
   
   for (i=1; i <= totaleventnum; i++){
     execio(fits_read_col(eventsfile, TINT, 2, (long)i, (long)1, (long)1,
			  &nulvalint, &pi, &anynul, &status));
     execio(fits_read_col(eventsfile, TSHORT, 3, (long)i, (long)1, (long)1,
			  &nulvalshort, &skyx, &anynul, &status));
     execio(fits_read_col(eventsfile, TSHORT, 4, (long)i, (long)1, (long)1,
			  &nulvalshort, &skyy, &anynul, &status));
     execio(fits_read_col(eventsfile, TSHORT, 5, (long)i, (long)1, (long)1,
			  &nulvalshort, &rawx, &anynul, &status));
     execio(fits_read_col(eventsfile, TSHORT, 6, (long)i, (long)1, (long)1,
			  &nulvalshort, &rawy, &anynul, &status));
     execio(fits_read_col(eventsfile, TSHORT, 8, (long)i, (long)1, (long)1,
			  &nulvalshort, &detx, &anynul, &status));
     execio(fits_read_col(eventsfile, TSHORT, 9, (long)i, (long)1, (long)1,
			  &nulvalshort, &dety, &anynul, &status));
     /* create PSF in the sky coordinate, and create RAW image  */
     psfimage[skyx][skyy]++;
     totalrawimg[rawx][rawy]++;
     
     if (coord == SKY){
       r = sqrt(pow((double)skyx-tentx,2.)+pow((double)skyy-tenty,2.));
     }else if (coord == DET){
       r = sqrt(pow((double)detx-tentx,2.)+pow((double)dety-tenty,2.));
     }else if (coord == RAW){
       r = sqrt(pow((double)rawx-tentx,2.)+pow((double)rawy-tenty,2.));
     } else {
       sprintf(message, "Error. coord= %d", coord);
       c_fcerr(message);
       exit(-1);
     }
     
     if (r < SEARCHRADIUS){
       sumskyx += skyx;	
       sumskyy += skyy;
       sumrawx += rawx;
       sumrawy += rawy;
       sumdetx += detx;
       sumdety += dety;
       searchedpixel++;
     }
   }
   *centerskyx = (double)sumskyx/(double)searchedpixel;
   *centerskyy = (double)sumskyy/(double)searchedpixel;
   centerrawx = (double)sumrawx/(double)searchedpixel;
   centerrawy = (double)sumrawy/(double)searchedpixel;
   sprintf(message, "source center: sky: (%6.2f, %6.2f)\traw: (%6.2f %6.2f)", 
	   *centerskyx, *centerskyy, centerrawx, centerrawy);
   c_fcecho(message);
   centerdetx = (double)sumdetx/(double)searchedpixel;
   centerdety = (double)sumdety/(double)searchedpixel;
   sprintf(message, "\tdet: (%6.2f, %6.2f)", centerdetx, centerdety);
   c_fcecho(message);
 }
 
 int bitexpand(int pi){
   if (datamode != BRIGHT)
     return(pi);
   if (pi < 1024)
     return(pi);   
   if (pi < 1536)
     return( 1024 + (pi-1024)*2);
   return(2048 + (pi-1536)*4);
 }
 
  void makepsf(void){
   double *psf;
   double  rmax, r, centerskyx, centerskyy;
   int i, j;
   int** psfimage;
   char message[100];
   
   psfimage = (int **)malloc(sizeof(int *)*(MAXSKYX+1));
   for (i = 0; i < MAXSKYX+1; i++) 
     psfimage[i] = (int *)malloc(sizeof(int)*(MAXSKYY+1));
   
   sprintf(message, "integrating PSF...");
   c_fcecho(message);
   accumlateskyimage(psfimage, &centerskyx, &centerskyy);
   rmax = centerrawx - MINRAWX;
   if (MAXRAWX-centerrawx < rmax)
     rmax =  MAXRAWX-centerrawx;
   if (centerrawy-MINRAWY  < rmax)
     rmax =  centerrawy-MINRAWY;
   if (MAXRAWY-centerrawy < rmax)
     rmax =  MAXRAWY-centerrawy;
   sprintf(message, "rmax = %g", rmax);
   c_fcecho(message);
   
   psfbin = (int)(rmax/PSFSTEP+1);
   psf=calloc((unsigned int)psfbin, sizeof(double));
   
   for (i=0; i<psfbin; i++)
     psf[i]=0.;
   
   for ( i=MINSKYX; i<= MAXSKYX; i++)
     for ( j=MINSKYY; j<=MAXSKYY; j++)
       psf[xy2r((short)i, (short)j, centerskyx, centerskyy)] +=
	 (double)psfimage[i][j];
   for ( i=0; i<psfbin; i++){
     r = ((double)i+.5)*PSFSTEP;
     psf[i] = psf[i]/(exposure/4.)/2./M_PI/r/PSFSTEP;
   }
   
   /* determine the truncation  radius (discardedradius) */
   if (disrisspecified == FALSE){	/* calculate the truncation radius */
     discardedradius=psfbin-2;
     while ((0 < discardedradius) && (psf[discardedradius-1] < FLUXTHRESHOLD)) /* Is this dangerous? */
       discardedradius--;
   }else{	/* discardedradius was specified in command line */
     discardedradius = (int)(disr/PSFSTEP+0.5);
     if (psfbin-2 < discardedradius)
       discardedradius = psfbin-2;
   }
   sprintf(message, "truncation radius (pixel)= %g", discardedradius*PSFSTEP);
   c_fcecho(message);
   if (discardedradius==0){
     sprintf(message, "Warning. May fail to find the source position. Check the source center.");
     c_fcecho(message);
   }
   writepsf(psf);
   free(psf);
   
   /* free the psfimage memory */ 
   for (i = 0; i < MAXSKYX+1; i++) 
     free(psfimage[i]);
   free(psfimage);
   
 }
 
 void writepsf(double psf[]){
   int i;
   double r;
   FILE *psffile;
   
   psffile = fopen(psffilename, "w");
   
   for ( i=0; i<psfbin; i++){
     r = ((double)i+.5)*PSFSTEP;
     fprintf(psffile, "%f\t%g\n", r, psf[i]);
   }
   fclose(psffile);
 }
 
 void calcpileup(void){
   int endofdata=FALSE, **temporalpsf, *psfnow, exposnum, i;
   int startevent=1, eventnumnow;
   double centerrawxnow, centerrawynow;
   struct event *eventnow, **sortedevent;
   int j, obsphanow[4096];
   int simlatedpha[4096];
   int inputpha[4096];
   int ** inputimg;
   int ** obsimagnow;
   
   inputimg = (int **)malloc(sizeof(int *)*(MAXRAWX+1));
   for (i = 0; i < MAXRAWX+1; i++) 
     inputimg[i] = (int *)malloc(sizeof(int)*(MAXRAWY+1));
   
   obsimagnow = (int **)malloc(sizeof(int *)*(MAXRAWX+1));
   for (i = 0; i < MAXRAWX+1; i++) 
     obsimagnow[i] = (int *)malloc(sizeof(int)*(MAXRAWY+1));

   clearrawimg(totalinputimg);
   clearpha(totalobspha);
   clearpha(totalsimlatedpha);
   clearpha(totalinputpha);
   
   while(endofdata==FALSE){
     counteventnum(&exposnum, &startevent, &eventnumnow, &endofdata);
     eventnow = calloc((unsigned int)eventnumnow, sizeof(struct event));
     loadevents(startevent, eventnumnow, eventnow);
     startevent += eventnumnow;
     accumlateobservedevent(eventnumnow, eventnow, obsimagnow, obsphanow);
     
     findcenternow(eventnumnow, eventnow, &centerrawxnow, &centerrawynow);
     
     temporalpsf = calloc((unsigned int)exposnum, sizeof(int*));
     for (i=0; i<exposnum; i++)
       temporalpsf[i] = calloc((unsigned int)psfbin, sizeof(int));
     psfnow = calloc((unsigned int)psfbin, sizeof(int));
     maketemporalpsf(exposnum, eventnumnow, eventnow, centerrawxnow, 
		     centerrawynow, temporalpsf, psfnow);
     
     for (i=0; i<eventnumnow; i++)
       if (discardedradius < sqrt(pow((double)eventnow[i].rawx-centerrawxnow, 2.)+
				  pow((double)eventnow[i].rawy-centerrawynow, 2.))/PSFSTEP )
	 totalobspha[eventnow[i].pi]++;
     
     sortedevent = calloc((unsigned int)psfbin, sizeof(struct event*));
     for (i=0; i<psfbin; i++){
       sortedevent[i] = calloc((unsigned int)(psfnow[i]), sizeof(struct event));
     }
     sortevents(eventnumnow, eventnow, sortedevent, centerrawxnow, centerrawynow);
     free(eventnow);
     
     /*
       for (j=0; j<psfbin; j++){
       fprintf(stderr, "%g %d %d %d %d\t%g\n", sortedevent[j][5].time, 
       sortedevent[j][5].rawx, sortedevent[j][5].rawy, 
       sortedevent[j][5].pi, sortedevent[j][5].grade,	
       sqrt(pow((double)sortedevent[j][5].rawx-centerrawxnow, 2.)+
       pow((double)sortedevent[j][5].rawy-centerrawynow, 2.))/PSFSTEP );
       }
     */
     
     rainofphoton(exposnum, temporalpsf, psfnow, sortedevent, centerrawxnow, 
		  centerrawynow, simlatedpha, inputimg, inputpha);
     for (i=0; i<=MAXRAWX; i++)
       for (j=0; j<=MAXRAWY; j++)
	 totalinputimg[i][j] += inputimg[i][j];
     for (i=0; i<4096; i++){
       totalsimlatedpha[i] += simlatedpha[i];
       totalinputpha[i] += inputpha[i];
     }
     
     for (i=0; i<psfbin; i++)
       free(sortedevent[i]);
     free(sortedevent);
     free(psfnow);
     for (i=0; i<exposnum; i++)
       free(temporalpsf[i]);
     free(temporalpsf);
   }		 
   
   for (i = 0; i < MAXRAWX+1; i++) 
     free(inputimg[i]); 
   free(inputimg); 
   
   for (i = 0; i < MAXRAWX+1; i++) 
     free(obsimagnow[i]);
   free(obsimagnow);
 }
 
 
 void counteventnum(int *exposnum, int *startevent, int *eventnum, int *endofdata){
   int anynul, status=0;
   double starttime, currenttime, previoustime, nulval=0.;
   int endofaccumlate=FALSE;
   char message[100];
   
   execio(fits_read_col(eventsfile, TDOUBLE, 1, (long)*startevent, 
			(long)1, (long)1, &nulval, &starttime, 
			&anynul, &status));
   sprintf(message, "startevent=%d\tstarttime=%9.0f", *startevent, starttime);
   c_fcecho(message);
   previoustime = currenttime = starttime;
   *exposnum = 1;
   *eventnum = 0;
   
   while(endofaccumlate==FALSE){
     (*eventnum)++;
     if (*startevent+*eventnum == totaleventnum){
       endofaccumlate = TRUE;
       *endofdata = TRUE;
     }else{
       execio(fits_read_col(eventsfile, TDOUBLE, 1, 
			    (long)(*startevent+*eventnum), (long)1, (long)1, 
			    &nulval, &currenttime, &anynul, &status));
       if (timebin <= currenttime-starttime){
	 endofaccumlate = TRUE;
       }else if (previoustime < currenttime){
	 (*exposnum)++;
	 previoustime = currenttime;
       }
     }
   }
   sprintf(message, "\tevents=%d\texpos=%d", *eventnum, *exposnum);
   c_fcecho(message);
   if (*exposnum < 10){
     sprintf(message, "Warning. expos= %d is too small. This may be avoided by changing timebin= %g", *exposnum, timebin);
     c_fcecho(message);
   }
 }
  
 void loadevents(int startevent, int eventnum, struct event events[]){
   int i, anynul, pi, nulvalint=0, status=0;
   double nulvald=0.;
   short nulvalshort=0;
   
   for (i=0; i<eventnum; i++){
     execio(fits_read_col(eventsfile, TDOUBLE, 1, (long)(startevent+i), 
			  (long)1, (long)1, &nulvald, &(events[i].time),
			  &anynul, &status));
     execio(fits_read_col(eventsfile, TINT, 2, (long)(startevent+i), (long)1,
			  (long)1, &nulvalint, &pi, &anynul, &status));
     events[i].pi = bitexpand(pi);
     execio(fits_read_col(eventsfile, TSHORT, 5, (long)(startevent+i), (long)1,
			  (long)1, &nulvalshort, &(events[i].rawx), &anynul, 
			  &status));
     execio(fits_read_col(eventsfile, TSHORT, 6, (long)(startevent+i), (long)1,
			  (long)1, &nulvalshort, &(events[i].rawy), &anynul, 
			  &status));
     execio(fits_read_col(eventsfile, TSHORT, 10, (long)(startevent+i), (long)1,
			  (long)1, &nulvalshort, &(events[i].grade), &anynul, 
			  &status));
   }
 }
 
 void accumlateobservedevent(int eventnum, struct event events[], 
			     int** obsimagnow, 
			     int obsphanow[4096]){
   int i, j;
   
   for (i=MINRAWX; i<=MAXRAWX; i++)
     for (j=MINRAWY; j<=MAXRAWY; j++)
       obsimagnow[i][j]=0;
   for (i=0; i<4096; i++)
     obsphanow[i]=0;
   
   for (i=0; i<eventnum; i++){
     obsimagnow[events[i].rawx][events[i].rawy]++;
     obsphanow[events[i].pi]++;
   }
   
 }
 
 void findcenternow(int eventnum, struct event events[],
		    double *centerrawxnow, double *centerrawynow){
   int i, sumx=0, sumy=0, eventnuminradius=0;
   char message[100];
   
   for (i=0; i<eventnum; i++){
     if (sqrt(pow((double)events[i].rawx-centerrawx, 2.)+pow((double)events[i].rawy-centerrawy, 2.)) < SEARCHRADIUS){
       sumx += events[i].rawx;
       sumy += events[i].rawy;
       eventnuminradius++;
     }
   }
   *centerrawxnow = (double)sumx / (double)(eventnuminradius);
   *centerrawynow = (double)sumy / (double)(eventnuminradius);
   sprintf(message,"source center (raw): (%g, %g)", *centerrawxnow, *centerrawynow);
   c_fcecho(message);
 }
 
 void maketemporalpsf(int exposnum, int eventnum, struct event events[],
		      double centerx, double centery, 
		      int *temporalpsf[], int psf[]){
   int i, j, expos=0;
   double previoustime;
   char message[100];
   
   previoustime = events[0].time;
   for (i=0; i<psfbin; i++)
     temporalpsf[0][i]=0;
   
   for (j=0; j<eventnum; j++){
     if (previoustime < events[j].time){
       expos++;
       previoustime = events[j].time;
       for (i=0; i<psfbin; i++)
	 temporalpsf[expos][i]=0;
     }
     i = xy2r(events[j].rawx, events[j].rawy, centerx, centery);
     temporalpsf[expos][i]++;
   }
   
   for (i=0; i<psfbin; i++){
     psf[i] = 0;
     for (j=0; j<exposnum; j++)
       psf[i] += temporalpsf[j][i];
   }
 }
 
 void sortevents(int eventnum, struct event events[], struct event
		 *(sortedevent[]), double centerx, double centery){
   int i, j, k, *sortedeventcount;
   
   /*  fprintf(stderr, "sorting events...\n");*/
   sortedeventcount = calloc((unsigned int)psfbin, sizeof(int));
   for (i=0; i<psfbin; i++)
     sortedeventcount[i] = 0;
   
   for (i=0; i<eventnum; i++){
     k = xy2r(events[i].rawx, events[i].rawy, centerx, centery);
     sortedevent[k][sortedeventcount[k]++] = events[i];
   }
   free(sortedeventcount);
 }
 
 void rainofphoton(int exposnum, int *temporalpsf[], int psfnow[], 
		   struct event *sortedevents[], double centerrawxnow, double centerrawynow,
		   int simpha[4096],
		   int **inputimg, int inputpha[4096]){
   int i, j, k, r, requiredrain, *photoncount, **eventorder, *counter;
   int** simimg;
   int loopcounter;
   char message[100];
   
   simimg = (int **)malloc(sizeof(int *)*(MAXRAWX+1));
   for (i = 0; i < MAXRAWX+1; i++) 
     simimg[i] = (int *)malloc(sizeof(int)*(MAXRAWY+1));
   /*  fprintf(stderr, "photons are dropping on the chip...\n");*/
   
   photoncount = calloc((unsigned int)psfbin, sizeof(int));
   eventorder = calloc((unsigned int)psfbin, sizeof(int*));
   counter = calloc((unsigned int)psfbin, sizeof(int));
   for (r=discardedradius; r<psfbin; r++){
     eventorder[r] = calloc((unsigned int)(psfnow[r]), sizeof(int));
     randamizeevent(psfnow[r], eventorder[r]);
     counter[r] = 0;
   }
   
   clearrawimg(inputimg);
   clearpha(inputpha);
   clearpha(simpha);
   
   for (i=0; i<exposnum; i++){
     fprintf(stderr, "\rexposure %d", i);
     clearrawimg(simimg);
     firstrain(psfnow, sortedevents, temporalpsf[i], eventorder, counter, inputimg, inputpha, simimg);
     detectphoton(simimg, psfbin-1, centerrawxnow, centerrawynow, photoncount);
     loopcounter=0;
     for (j=psfbin-1; discardedradius<=j; --j){
       while(photoncount[j] < temporalpsf[i][j] && loopcounter <20){
	 for (k=discardedradius; k<=j; k++){
	   requiredrain = temporalpsf[i][k]-photoncount[k];
	   rain(sortedevents[k], k, psfnow[k], requiredrain, inputimg, inputpha, simimg);
	 }
	 if ( j==psfbin-1){
	   detectphoton(simimg, j, centerrawxnow, centerrawynow, photoncount);
	 }else{
	   detectphoton(simimg, j+1, centerrawxnow, centerrawynow, photoncount);
	   if (photoncount[j+1] < temporalpsf[i][j+1]){	/* outer ring is involved and lost photon? */
	     j++;
	     /*	    fprintf(stderr, "radius expands to %d\n", j);*/
	   }
	 }
	 loopcounter++;	/* if loopcounter >19, then loopout! */
       }
       if (19 < loopcounter){
	 sprintf(message, "\tWarning. loopcounter=20 at ring %d.", j);
	 c_fcecho(message);
       }
     }
     img2pha(simimg, simpha);
   }
   c_fcecho("");
   for (r=discardedradius; r<psfbin; r++)
     free(eventorder[r]);
   free(counter);
   free(eventorder);
   free(photoncount);
   
   for (i = 0; i < MAXRAWX+1; i++) 
     free(simimg[i]); 
   free(simimg); 
 }
 
 void firstrain(int* psfnow, struct event *events[], int temporalpsf[], 
		int **eventorder, int *counter,
		int** inputimg, 
		int inputpha[4096], int** simimg){
   
   int i, r;
   
   for (r=discardedradius; r<psfbin; r++){
     for (i=0; i<temporalpsf[r]; i++){
       inputimg[events[r][eventorder[r][counter[r]]].rawx][events[r][eventorder[r][counter[r]]].rawy]++;
       inputpha[events[r][eventorder[r][counter[r]]].pi]++;
       distributecharge(events[r][eventorder[r][counter[r]]], simimg);
       counter[r]++;
     }
   }
 }
 
 void randamizeevent(int psf, int eventorder[]){
   int i, j, swapnum, changering;
   
   for (j=0; j<psf; j++)
     eventorder[j]=j;
   
   for (j=0; j<psf; j++){
     swapnum = (int)((double)rand()/((double)RAND_MAX+1.)*(double)psf);
     changering = eventorder[j];
     eventorder[j] = eventorder[swapnum];
     eventorder[swapnum] = changering;
   }
 }
 
 
 void detectphoton(int** img, int rout, double centerx, double centery, 
		   int resultpsf[]){
   /*  resultpsf[>rout] is not guaranteed */
   
   int i, j, startrawx, startrawy, endrawx, endrawy, r, pha;
   
   for (i=0; i<psfbin; i++)
     resultpsf[i] = 0;
   
   if (rout==psfbin-1){
     startrawx = MINRAWX;
     endrawx = MAXRAWX;
     startrawy = MINRAWY;
     endrawy = MAXRAWY;
   }else{
     startrawx = (int)floor(centerx-((double)rout+1.)*PSFSTEP-1.);
     if (startrawx < MINRAWX )
       startrawx = MINRAWX;
     startrawy = (int)floor(centery-((double)rout+1.)*PSFSTEP-1.);
     if (startrawy < MINRAWY )
       startrawy = MINRAWY;
     endrawx = (int)ceil(centerx+((double)rout+1.)*PSFSTEP+1.);
     if (MAXRAWX < endrawx )
       endrawx = MAXRAWX;
     endrawy = (int)ceil(centery+((double)rout+1.)*PSFSTEP+1.);
     if (MAXRAWY < endrawy )
       endrawy = MAXRAWY;
   }
   for (i=startrawx; i<=endrawx; i++)
     for (j=startrawy; j<=endrawy; j++){
       r = xy2r((short)i, (short)j, centerx, centery);
       if (r <= rout)
	 if (isthisannevent(img, i, j, &pha)==TRUE)
	   resultpsf[r]++;
     }
   /*
     for (i=0; i<=rout; i++)
     fprintf(stderr, "\t%2d:%d", i, resultpsf[i]);
     fprintf(stderr, "\n");
   */
 }
 
 void rain(struct event events[], int r, int eventnuminr,
     int requiredrain, int** inputimg, 
	   int inputpha[4096], int** simimg){
   
   int i, j;
   
   /*  fprintf(stderr, "%d drops\n", requiredrain);*/
   for (i=0; i<requiredrain; i++){
     j = (int)((double)rand()/((double)RAND_MAX+1.)*(double)eventnuminr);
     /*
       fprintf(stderr, "randomnum= %d\t(%d, %d): %d\n", 
       j, events[j].rawx, events[j].rawy, events[j].pi);
     */
     inputimg[events[j].rawx][events[j].rawy]++;
     inputpha[events[j].pi]++;
     distributecharge(events[j], simimg);
   }
 }
 
 void distributecharge(struct event anevent, int** img){
   int i, j;
   char message[100];
   
   switch (anevent.grade) {
   case 0:
     img[anevent.rawx][anevent.rawy] += anevent.pi;
     break;
   case 2: 
     img[anevent.rawx][anevent.rawy] += anevent.pi-spth;
     if ((double)rand()/(double)RAND_MAX > .5 && anevent.rawy < MAXRAWY){
       img[anevent.rawx][anevent.rawy+1] += spth;
     }else{
       img[anevent.rawx][anevent.rawy-1] += spth;
     }
     break;
   case 3:
     img[anevent.rawx][anevent.rawy] += anevent.pi-spth;
     img[anevent.rawx-1][anevent.rawy] += spth;
     break;
   case 4:
     img[anevent.rawx][anevent.rawy] += anevent.pi-spth;
     img[anevent.rawx+1][anevent.rawy] += spth;
     break;
   defalut:
     img[anevent.rawx][anevent.rawy] += anevent.pi;
     sprintf(message, "Warning. unsupported grade: %d", anevent.grade);
     c_fcecho(message);
     break;
   }
   
   /*
     fprintf(stderr, "\t(%d, %d): %d\n", anevent.rawx, anevent.rawy, anevent.pi);
     for (j=anevent.rawy+1; j >= anevent.rawy-1; --j){
     for (i=anevent.rawx-1; i <= anevent.rawx+1; i++)
     fprintf(stderr, "\t%d", img[i][j]);
     fprintf(stderr, "\n");
     }
   */
   
 }
 
 
 int isthisannevent(int** img, int x, int y, int *pha){
   
   int split=0;
   
   if ( x <= MINRAWX || MAXRAWX <= x ||
	y <= MINRAWY || MAXRAWY <= y )
     return(FALSE);
   
   if (img[x][y] < evth)
     return(FALSE);
   
   if ((img[x][y] < img[x-1][y-1]) ||
       (img[x][y] < img[x  ][y-1]) ||
       (img[x][y] < img[x+1][y-1]) ||
       (img[x][y] < img[x-1][y  ]) ||
       (img[x][y] < img[x+1][y  ]) ||
       (img[x][y] < img[x-1][y+1]) ||
       (img[x][y] < img[x  ][y+1]) ||
       (img[x][y] < img[x+1][y+1]))
     return(FALSE);
   
   *pha = img[x][y];
   if (spth <= img[x  ][y-1]){	/* grade=2? */
     if ((spth <= img[x-1][y-1]) ||
	 (spth <= img[x+1][y-1]))
       return(FALSE);
     *pha += img[x  ][y-1];
     split++;
   }
   if (spth <= img[x-1][y  ]){	/* g=3? */
     if ((spth <= img[x-1][y-1]) ||
	 (spth <= img[x-1][y+1]))
       return(FALSE);
     *pha += img[x-1][y  ];
     split++;
   }
   if (spth <= img[x+1][y  ]){	/* g=4? */
     if ((spth <= img[x+1][y-1]) ||
	 (spth <= img[x+1][y+1]))
       return(FALSE);
     *pha += img[x+1][y  ];
     split++;
   }
   if (spth <= img[x  ][y+1]){	/* g=2? */
     if ((spth <= img[x-1][y+1]) ||
	 (spth <= img[x+1][y+1]))
       return(FALSE);
     *pha += img[x  ][y+1];
     split++;
   }
   
   if (split==0){	/* grade=0, 1 */
     if ((spth < img[x-1][y-1]) ||
	 (spth < img[x+1][y-1]) ||
	 (spth < img[x-1][y+1]) ||
	 (spth < img[x+1][y+1]))
       return(FALSE);
   }else if (1 < split){	/* grade=6, 7 */
     return(FALSE);
   }
   return(TRUE);
 }
 
 void img2pha(int** img, int *pha){
   int i, j, ph;
   
   for (i=MINRAWX+1; i<MAXRAWX; i++)
     for (j=MINRAWY+1; j<MAXRAWY; j++)
       if (isthisannevent(img, i, j, &ph)==TRUE){
	 pha[ph]++;
       }
 }
 
 void clearrawimg(int** img){
   int i, j;
   
   for (i=0; i<=MAXRAWX; i++)
     for (j=0; j<=MAXRAWY; j++)
       img[i][j]=0;
 }
 
 void clearpha(int pha[4096]){
   int i;
   
   for (i=0; i<4096; i++)
     pha[i]=0;
 }
 
 void correction(void){
   int i;
   
   for (i=0; i<4096; i++){
     bgdpha[i] = totalsimlatedpha[i]-totalinputpha[i];
     correctedpha[i] = totalobspha[i]- bgdpha[i];
   }
}
 
 void writefiles(){
   int status=0, dummybgd[4096], i;
   fitsfile *outfits;
   
   writepha(correctedphafilename, correctedpha, bgdpha);

   for (i=0; i<4096; i++)
     dummybgd[i] = 0;
   
   writepha(rawphafilename, totalobspha, dummybgd);
   writepha(bgdphafilename, bgdpha, dummybgd);
   writepha(simphafilename, totalsimlatedpha, dummybgd);
   writepha(totalinputphafilename, totalinputpha, dummybgd);

   writeimg(rawimgfilename, totalrawimg);
   writeimg(outimgfilename, totalinputimg);
   
 }
 
 void writeimg(char *filename, int **img){
   fitsfile *outfits;
   int status=0, tmpimg[MAXRAWY+1][MAXRAWX+1], i, j;
   long naxes[2] = {MAXRAWX+1, MAXRAWY+1};
   
   execio(fits_create_file(&outfits, filename, &status));
   execio(fits_create_img(outfits, 32, 2, naxes, &status));
   writestanderdheader(outfits);
   for (i=0; i<=MAXRAWX; i++)
     for (j=0; j<=MAXRAWY; j++)
       tmpimg[j][i] = img[i][j];
   execio(fits_write_img(outfits, TINT, 1, (MAXRAWX+1)*(MAXRAWY+1), 
			 tmpimg[0], &status));
   execio(fits_close_file(outfits, &status));
 }
 
 void writepha(char *outphafilename, int outpha[4096], int bgd[4096]){
   long naxes[2] = {0, 0};  /* Null data in the primary */
   int binnedbgd[512], binnedpha[512];
   short channel[512], quality[512];
   float error[512];
   char *phattype[4] = {"CHANNEL", "COUNTS", "STAT_ERR", "QUALITY"};
   char *phatform[4] = {"I", "J", "E", "I"}, *tunit[4] = {"\0", "count\0", "count\0", "\0"};
   int status=0, i;
   fitsfile *outfits;
   
   execio(fits_create_file(&outfits, outphafilename, &status));
   execio(fits_create_img(outfits, 32, 0, naxes, &status));
   writestanderdheader(outfits);
   execio(fits_create_tbl(outfits, BINARY_TBL, (long)512, 4, phattype, 
			  phatform, tunit, "SPECTRUM", &status));
   writestanderdheader(outfits);
   writephaheader(outfits);
   
   for (i=0; i<512; i++)
     channel[i]=(short)(i);
   execio(fits_write_col(outfits, TSHORT, 1, (long)1, (long)1, (long)512, 
			 channel, &status));
   for (i=0; i<512; i++){
     binnedpha[i] = 0;
     binnedbgd[i] = 0;
   }
   for (i=0; i<4096; i++){
     binnedpha[i/8] += outpha[i];
     binnedbgd[i/8] += bgd[i];
   }
   execio(fits_write_col(outfits, TINT, 2, (long)1, (long)1, (long)512, 
			 binnedpha, &status));
   evaluateerror(error, binnedpha, binnedbgd);
   execio(fits_write_col(outfits, TFLOAT, 3, (long)1, (long)1, (long)512, 
			 error, &status));
   evaluatequality(quality);
   execio(fits_write_col(outfits, TSHORT, 4, (long)1, (long)1, (long)512, 
			 quality, &status));
   execio(fits_close_file(outfits, &status));
 }
 
 
 void evaluateerror(float error[512], int pha[512], int bgd[512]){
   int i;
   
   for (i=0; i<512; i++){
     /* error[i] = sqrtf(fabs((float)bgd[i])+fabs((float)pha[i])); */
     error[i] = (float) sqrt(fabs((double)bgd[i])+fabs((double)pha[i])); 
     if ( error[i] == 0. )
       error[i] = (float)1.;
   }
 }
 
 
 void evaluatequality(short quality[512]){
   int i;
   
   for (i=0; i<512; i++)
     if (i < ceil((double)evth/8.)){
       quality[i] = (short)5;
     }else{
       quality[i] = (short)0;
     }
 }
 
 void writestanderdheader(fitsfile *outfits){
   char value[FLEN_VALUE], comment[FLEN_COMMENT], keyword[FLEN_KEYWORD];
   int status=0;
   
   execio(fits_write_key(outfits, TSTRING, "TELESCOP", "ASCA", "Telescope (mission) name",  &status));
   sprintf(value, "SIS%d", sensor);
   execio(fits_write_key(outfits, TSTRING, "INSTRUME", value, "Instrument name", &status));
   execio(fits_write_key(outfits, TSTRING, "FILTER", "NONE", "Filter name used", &status));
   if (datamode == BRIGHT)
     execio(fits_write_key(outfits, TSTRING, "DATAMODE", "BRIGHT", "Datamode", &status));
   sprintf(keyword,"S%dCCDPOW", sensor);
   switch (ccd) {
   case 0: 
     strcpy(value, "1000");
     break;
   case 1:
     strcpy(value, "0100");
     break;
   case 2:
     strcpy(value, "0010");
     break;
   case 3:
     strcpy(value, "0001");
     break;
   }
   sprintf(comment, "Which S%d CCDs are in use(0123): 0=OFF 1=ON", sensor);
   execio(fits_write_key(outfits, TSTRING, keyword, value, comment, &status));
   sprintf(keyword,"S%d_EVTR%d", sensor, ccd);
   sprintf(comment,"S%d event threshold for ccd %d", sensor, ccd);
   execio(fits_write_key(outfits, TINT, keyword, &evth, comment, &status));
   sprintf(keyword,"S%d_SPTR%d", sensor, ccd);
   sprintf(comment,"S%d split threshold for ccd %d", sensor, ccd);
   execio(fits_write_key(outfits, TINT, keyword, &spth, comment, &status));
   execio(fits_write_key(outfits, TDOUBLE, "EXPOSURE", &exposure, "Exposure", &status));
   execio(fits_write_key(outfits, TSTRING, "FILIN001", ineventfilename, "Input file name", &status));
   
   execio(fits_write_history(outfits, VERSION, &status));
 }
 
 void writephaheader(fitsfile *outfits){
   char value[FLEN_VALUE], comment[FLEN_COMMENT], keyword[FLEN_KEYWORD];
   char history[1024];
   double valued;
   int valuei, status=0;
   
   execio(fits_write_key(outfits, TSTRING, "HDUCLASS", "OGIP", "format conforms to OGIP standard", &status));
   execio(fits_write_key(outfits, TSTRING, "HDUCLAS1", "SPECTRUM", "PHA dataset (OGIP memo OGIP-92-007)", &status));
   execio(fits_write_key(outfits, TSTRING, "HDUVERS1", "1.1.0", "Version of format (OGIP memo OGIP-92-007a)", &status));
   /*  execio(fits_write_key(outfits, TSTRING, "HDUCLAS2", "\0", "WARNING This is NOT an OGIP-approved value", &status));*/
   /*  execio(fits_write_key(outfits, TSTRING, "HDUCLAS3", "COUNT", "PHA data stored as Counts (not count/s)", &status));*/
   valued = 1.;
   execio(fits_write_key(outfits, TDOUBLE, "AREASCAL", &valued, "nominal effective area", &status));
   execio(fits_write_key(outfits, TDOUBLE, "CORRSCAL", &valued, "correlation scale factor", &status));
   /*  BACKSCAL is defined in the DET coordinates, whose dimension is 1280*1280*/
   valued = (MAXRAWX-MINRAWX+1)*(MAXRAWY-MINRAWY+1)-M_PI*pow((double)discardedradius*PSFSTEP, 2.);
   valued = valued /1280.0/1280.0;
   execio(fits_write_key(outfits, TDOUBLE, "BACKSCAL", &valued, "background scale factor", &status));
   execio(fits_write_key(outfits, TSTRING, "BACKFILE", "NONE", "background FITS file for", &status));
   execio(fits_write_key(outfits, TSTRING, "CORRFILE", "none", "correlation FITS file for", &status));
   execio(fits_write_key(outfits, TSTRING, "RESPFILE", "NONE", "redistribution matrix", &status));
   execio(fits_write_key(outfits, TSTRING, "ANCRFILE", "none", "ancillary response", &status));
   /*  The error is not poissonian. */
   valuei = FALSE;	
   execio(fits_write_key(outfits, TLOGICAL, "POISSERR", &valuei, "Errors are not Poissonian", &status));
   execio(fits_write_key(outfits, TSTRING, "CHANTYPE", "PI", "Channels assigned by detector electronics", &status));
   valuei = 0;
   execio(fits_write_key(outfits, TINT, "TLMIN1", &valuei, "Lowest legal channel number", &status));
   valuei = 511;
   execio(fits_write_key(outfits, TINT, "TLMAX1", &valuei, "Highest legal channel number", &status));
   execio(fits_write_key(outfits, TINT, "NCHAN", &valuei, "Number of detector channels", &status));
   valuei = 0;
   execio(fits_write_key(outfits, TINT, "GROUPING", &valuei, "no grouping of the data has been defined", &status));
   valuei = 512;
   execio(fits_write_key(outfits, TINT, "DETCHANS", &valuei, "Total No. of Detector Channels available", &status));
   execio(fits_write_key(outfits, TINT, "TOTCTS", &totaleventnum, "Total pixel count", &status));
   sprintf(history, "corpileup: source position (RAWX= %g RAWY= %g)",
	   centerrawx, centerrawy);
   execio(fits_write_history(outfits, history, &status));
   sprintf(history, "               (DETX= %g DETY= %g)",
	   centerdetx, centerdety);
   execio(fits_write_history(outfits, history, &status));
   sprintf(history, "               truncation radius= %g pixel",
	   discardedradius*PSFSTEP);
   execio(fits_write_history(outfits, history, &status));
 }
 
 void execio(int status){
   char status_str[FLEN_STATUS], errmsg[FLEN_ERRMSG];
   char message[100];
   
   if (status){
     fits_get_errstatus(status, status_str);   /* get the error description */
     sprintf(message, "cfitsio error %d: %s", status, status_str);
     c_fcerr(message);
     
     while (fits_read_errmsg(errmsg)) { /* get remaining messages */
       sprintf(message, " %s", errmsg);
       c_fcerr(message);
     }
     exit(status);       /* terminate the program, returning error status */
   }
 }
 
 int xy2r(short x, short y, double centerx, double centery){
   /* must not be called before definition of psfbin */
   int r;
   r = (int)(sqrt(pow((double)x-centerx, 2.)+pow((double)y-centery, 2.))/PSFSTEP);
   if (psfbin-1 < r)
     r = psfbin-1;
   return(r);
 }

/* A function to check presense of the output files */ 
 void file_presence(char *file){
   FILE *openfile;
   char message[100];

   openfile = fopen(file,"r");
   if(openfile != NULL) { 
     fclose(openfile);
     if(clobber){
       sprintf(message,"%s will be overwritten since clobber parameter is set.", file);
       c_fcecho(message);
       remove(file);
     }else{
       sprintf(message,"%s already exists but clobber parameter is not set.", file);
       c_fcerr(message);
       exit(1);
     }
   }
 }
