/*
 *  Manage and track wcs info in the form of AST "objects"
 *  Referenced by string WCS# in tcl and fortran
 *
 *  FORTRAN API
 *  Routines that start WCS use an existing wcsid
 *
c Initialize WCS usage (program startup only/ximinit)
      CALL INITWCS  

      IF ( ISASTBAD(value) ) THEN
      ...

c Manage wcsid reference count
      CALL WCSGETREF(wcsid, refcnt)
      CALL WCSINCREF(wcsid)
      CALL WCSDECREF(wcsid)

c Read FITS image header in and associate with a wcsid
      CALL GENIMGWCS(fitsfile, extnum, MAX_IDSTR, wcsid, status)

c Use old-style C-keys (with event lists TC-keys) to create 
c  frames and associate with wcsid
      CALL GENCKEYWCS(naxes, ctype1, ctype2, cunit1, cunit2,
                      crval, crpix, cdelt, crota2, equinox,
     &                MAX_IDSTR, wcsid, status)

c Generate frameset from internal header
      CALL GENHDRWCS(mapid, wcsid, status)

c Make a (deep) copy of wcs structure and return new id
      CALL COPYWCS(wcsid, idlen, copyid, status)

c Assign detector frame to existing wcsid frameset
      CALL WCSSETDET(wcsid, deqc, imgref, detref, begzm, status)

c Rotate existing frameset
      CALL WCSROTATE(wcsid, rotpix, angle, status)

c Swap axes of existing frameset
      CALL WCSAXSWAP(wcsid, status)

c Modify frameset to rebin/recenter (linear transformation) to new map
      CALL WCSLINTRF(wcsid, cenpix, nszx, nszy, newzm, status)

c Frame management (copy and delete)
      CALL WCSFRMCOPY(wcsid, frameid, newdomain, status)
      CALL WCSFRMDEL(wcsid, frameid, status)

c Assign coordinate keys in internal header associated with mapid
c   to values stored in wcs frameset
      CALL WCSTOHDR(wcsid, mapid, status)

c Get information on the sky frame for a particular wcsid
      CALL WCSSKYINFO(wcsid, system, xlab, ylab, equinox)
c Get information on the current frame for a particular wcsid
      CALL WCSFRMINFO(wcsid, system, projection, xlab, ylab, 
     &                unit, equinox)
c
c Coordinate conversion
c
c   Setup frameset to convert from in frame to out frame
      CALL WCSCNVBEG(wcsid, inframeid, outframeid, wcscnvidx, status)

c   Convert double values
      CALL WCSCNVD(wcscnvidx, dinval1, dinval2, doutval1, doutval2)

c   Convert real values
      CALL WCSCNVR(wcscnvidx, rinval1, rinval2, routval1, routval2)

c   Cleanup after conversions
      CALL WCSCNVEND(wcscnvidx)

c   Wrapper routine converts between image and detector coordinates
      CALL WCSIMGPIX(wcsid, ximg, yimg, xpix, ypix, forward, status)

c   Wrapper routine converts between image and sky coordinates
      CALL WCSIMGSKY(wcsid, ximg, yimg, xsky, ysky, equinox, forward, 
     &               status)

c   Wrapper routine converts between different sky coordinates
      CALL WCSSKYSKY(fromsys, fromeqx, fromx, fromy, tosys, toeqx, 
     &               tox, toy, status)

c   Wrapper routine precesses RA/Dec between equinoxes
      CALL WCSPREC(fromeqx, fromx, fromy, tosys, toeqx, tox, toy, status)

c   Calculates distance between two sky points
      CALL WCSDIST(wcsid, frameid, xval1, yval1, xval2, yval2, dist, 
     &             status)
c
c Contour overlay
c
c   Setup contour-drawing routine with transform
      CALL WCSCONXBEG(fromid, toid, status)

c   Contour-drawing buffer routine
      CALL WCSCONXBUF(npts, x, y, k)

c   Cleanup after contour-drawing
      CALL WCSCONXEND(status)
 */
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <math.h>
#include "ast.h"
#include "fitsio.h"
#include "fitsio2.h"
#include "cfortran.h"
#include "cpgplot.h"
#include "../include/astadd.h"
#include "../include/xcommon.h"
#include "../include/maxvals.h"
#include "../include/map.h"
#include "../include/wcsmanager.h"
#include "../include/null.h"

#define MAXWCS 60
#define MAXCNV 60

#define SYSRADEC 1
#define SYSOTHER 2

static AstMapping *conxcnv;

/*
 *  Imported Prototypes
 */
void tclvard(char *name, double value, int isreadonly, int isglobal,
             int *status);
/*
 *  Prototypes (definition in file)
 */
int getwcsidx(char *wcsid);
int getemptywcsidx();
int getemptycnvidx();
AstFrameSet *fillwcs (char *header);
AstFrameSet *hdrtowcs(char *mapid);
void affgkyd(AstFitsChan *fitschan, char *keyname, double *value);
void affgkys(AstFitsChan *fitschan, char *keyname, char *value);

#define HDRTOMEM(A,B,C) CCALLSFSUB3(HDRTOMEM,hdrtomem,STRING,STRING,PINT, A,B,C)
#define CONXBEG() CCALLSFSUB0(CONXBEG,conxbeg)
#define CONXEND() CCALLSFSUB0(CONXEND,conxend)

struct cnvdata {
   AstMapping *mapping;  /* conversion mapping */
   int inissky;          /* whether input coords are sky coords */
   int outissky;         /* whether output coords are sky coords */
   int insys;            /* code indicating input system type */
   int outsys;           /* code indicating input system type */
};

static AstFrameSet * wcslist[MAXWCS];
static int wcsref[MAXWCS];
static struct cnvdata cnvlist[MAXCNV];
static char * wcsname[MAXWCS];

static const char *UNITDEG = "deg";
static const char *UNITPIX = "pixel";

void wcsversion(char *verstr) {

   int version, maj, min, rel;

   version = astVersion;
   maj = version/1000000;
   min = (version - maj*1000000)/1000;
   rel = version - maj*1000000 - min*1000;
   sprintf(verstr, "%d.%d-%d", maj, min, rel);

}
FCALLSCSUB1(wcsversion,WCSVERSION,wcsversion,PSTRING)

void initwcs() {
/*
 *  Initialize wcs manager
 */
   int i, status;
   char numstr[4]; 

   for ( i = 0; i < MAXWCS; i++ ) {
      wcslist[i] = NULL;
      wcsref[i] = 0;
      sprintf(numstr, "%d", i+1);
      wcsname[i] = strcatalloc("WCS", numstr);
   }
   for ( i = 0; i < MAXCNV; i++ ) {
      cnvlist[i].mapping = NULL;
      cnvlist[i].inissky = 0;
      cnvlist[i].outissky = 0;
      cnvlist[i].insys = 0;
      cnvlist[i].outsys = 0;
   }
   astBegin;  /* Start AST context */

   /* 
    *  Set up Tcl variable that can be used to check for bad values
    */
   tclvard("ASTBAD", AST__BAD, 1, 1, &status);
}
FCALLSCSUB0(initwcs,INITWCS,initwcs)

int isastbad (double value) {
   if ( value < 0. && fabs(value) > 1.e+300 ) return 1;
   return 0;
}
FCALLSCFUN1(LOGICAL,isastbad,ISASTBAD,isastbad,DOUBLE)

void wcsgetref(char *wcsid, int *refcnt) {
/*
 *  Get reference count, corresponding to usage of wcsid
 */
   int i;
   i = getwcsidx(wcsid);
   if ( i < 0 ) return;
   *refcnt = wcsref[i];
}
FCALLSCSUB2(wcsgetref,WCSGETREF,wcsgetref,STRING,PINT)

void wcsincref(char *wcsid) {
/*
 *  Increment reference count, corresponding to usage of wcsid
 */
   int i;
   i = getwcsidx(wcsid);
   if ( i < 0 ) return;
   wcsref[i]++;
}
FCALLSCSUB1(wcsincref,WCSINCREF,wcsincref,STRING)

void wcsdecref(char *wcsid) {
/*
 *  Decrement reference count, corresponding to usage of wcsid
 */
   int i;
   i = getwcsidx(wcsid);
   if ( i < 0 ) return;
   wcsref[i]--;
   if ( wcsref[i] == 0 ) {
      astAnnul(wcslist[i]);
      wcslist[i] = NULL;
   }
}
FCALLSCSUB1(wcsdecref,WCSDECREF,wcsdecref,STRING)

void prallwcs() {
/*
 *  Print status of all wcsid's
 */
   int i;
   char buff[50];
   for ( i = 0; i < MAXWCS; i++ ) {
      if ( wcsref[i] > 0 ) {
         sprintf(buff, " %2d - IN USE: %d refs", i+1, wcsref[i]);
      } else {
         sprintf(buff, " %2d - AVAILABLE", i+1);
      }
      cxwrite(buff,15);
   }
}

void genimgwcs(char *filename, int extnum, int wcslen, char *wcsid, int *status) {
/*
 *  Given FITS image, read coords from file into an AstFrameSet
 *
 *  filename I FITS file to read WCS info from
 *  extnum   I Extension number to read WCS info from
 *  wcslen   I Maximum length of wcsid
 *  wcsid    O WCS id associated with newly read in WCS info
 *  status   O Error flag (0 = OK)
 */
   int i, hdutype;
   fitsfile *fptr;
   char *header;
   char errmsg[FLEN_STATUS];

   *status = 0;
   strcpy(wcsid,"");
   i = getemptywcsidx();
   if ( i == -1 ) {  /* No more wcs slots? */
      *status = -1;
      return;
   }

   /* 
   ** Can't use READONLY anymore for internal mem:// file
   ** This is due to some thread-safety changes in CFITSIO 3.15
   */
   if ( strncmp( ( const char* ) filename, "mem://", 6 ) ) {
       fits_open_file(&fptr, filename, READONLY, status);
   } else {
       fits_open_file(&fptr, filename, READWRITE, status);
   }
   fits_movabs_hdu(fptr, extnum+1, &hdutype, status);
   fits_get_image_wcs_keys(fptr, &header, status);
   fits_close_file(fptr, status);
   if ( *status != 0 ) {
      fits_get_errstatus(*status, errmsg);
      cxwarn(errmsg, 5);
      return; 
   }
   wcslist[i] = fillwcs(header);
   free(header);
   if ( wcslist[i] == NULL ) {
      *status = -1;
      return;
   }

   wcsref[i]++;
   safestrcpy(wcsid, wcsname[i], wcslen);
}
FCALLSCSUB5(genimgwcs,GENIMGWCS,genimgwcs,STRING,INT,INT,PSTRING,PINT)

void genckeywcs(int *naxes, char *ctype1, char *ctype2, char *cunit1, 
                char *cunit2, double *crval, double *crpix, 
		double *cdelt, double crota2, int equinox,
                int wcslen, char *wcsid, int *status) {
/*
 *  Given FITS event file, read coords into an AstFrameSet
 *
 *  naxes[2] I Axis lengths
 *  ctype1/2 I Coordinate type
 *  cunit1/2 I Unix of pixel size
 *  crval[2] I Reference values
 *  crpix[2] I Reference pixels
 *  cdelt[2] I Pixel size
 *  crota2   I Rotation
 *  equinox  I Coordinate equinox
 *  wcslen   I Maximum length of wcsid
 *  wcsid    O WCS id associated with newly read in WCS info
 *  status   O Error flag (0 = OK)
 */
   int i, decim;
   AstFitsChan *fitschan, *copychan;
   AstFrameSet *wcsinfo;
   char card[FLEN_CARD];
   char valstr[FLEN_VALUE];
   char comment[2];

   *status = 0;
   i = getemptywcsidx();
   if ( i == -1 ) {  /* No more wcs slots? */
      *status = -1;
      return;
   }
   /*
    *  Write values as keywords to FitsChan
    */
   astBegin;
   /*fitschan = astFitsChan(NULL, NULL, "Encoding=FITS-AIPS++");*/
   fitschan = astFitsChan(NULL, NULL, "");
   
   decim = 15;
   comment[0] = '\0';

   ffmkky("NAXIS", "2", comment, card, status);
   if ( *status == 0 ) astPutFits(fitschan, card, 0);

   ffi2c(naxes[0], valstr, status);
   ffmkky("NAXIS1", valstr, comment, card, status);
   if ( *status == 0 ) astPutFits(fitschan, card, 0);

   ffi2c(naxes[1], valstr, status);
   ffmkky("NAXIS2", valstr, comment, card, status);
   if ( *status == 0 ) astPutFits(fitschan, card, 0);

   ffs2c(ctype1, valstr, status);
   ffmkky("CTYPE1", valstr, comment, card, status);
   if ( *status == 0 ) astPutFits(fitschan, card, 0);

   ffs2c(ctype2, valstr, status);
   ffmkky("CTYPE2", valstr, comment, card, status);
   if ( *status == 0 ) astPutFits(fitschan, card, 0);

   if ( *cunit1 ) {
      ffs2c(cunit1, valstr, status);
      ffmkky("CUNIT1", valstr, comment, card, status);
      if ( *status == 0 ) astPutFits(fitschan, card, 0);
   }

   if ( *cunit2 ) {
      ffs2c(cunit2, valstr, status);
      ffmkky("CUNIT2", valstr, comment, card, status);
      if ( *status == 0 ) astPutFits(fitschan, card, 0);
   }

   ffd2e(crpix[0], decim, valstr, status);
   ffmkky("CRPIX1", valstr, comment, card, status);
   if ( *status == 0 ) astPutFits(fitschan, card, 0);

   ffd2e(crpix[1], decim, valstr, status);
   ffmkky("CRPIX2", valstr, comment, card, status);
   if ( *status == 0 ) astPutFits(fitschan, card, 0);

   ffd2e(crval[0], decim, valstr, status);
   ffmkky("CRVAL1", valstr, comment, card, status);
   if ( *status == 0 ) astPutFits(fitschan, card, 0);

   ffd2e(crval[1], decim, valstr, status);
   ffmkky("CRVAL2", valstr, comment, card, status);
   if ( *status == 0 ) astPutFits(fitschan, card, 0);

   ffd2e(cdelt[0], decim, valstr, status);
   ffmkky("CDELT1", valstr, comment, card, status);
   if ( *status == 0 ) astPutFits(fitschan, card, 0);

   ffd2e(cdelt[1], decim, valstr, status);
   ffmkky("CDELT2", valstr, comment, card, status);
   if ( *status == 0 ) astPutFits(fitschan, card, 0);

   ffd2e(crota2, decim, valstr, status);
   ffmkky("CROTA2", valstr, comment, card, status);
   if ( *status == 0 ) astPutFits(fitschan, card, 0);

   if ( equinox > 0 ) {
      ffi2c(equinox, valstr, status);
      ffmkky("EQUINOX", valstr, comment, card, status);
      if ( *status == 0 ) astPutFits(fitschan, card, 0);
   }

   /*
    *  Generate FrameSet (wcsinfo) from FitsChan
    */
   astClear(fitschan, "Card");
   copychan = astCopy(fitschan);
   wcsinfo = astRead(copychan);
   astAnnul(copychan);
   if ( wcsinfo == AST__NULL ) {
      /*
       *  If AST read fails, assume RA/Dec system
       */
      cxwrite("Defaulting to equatorial tangent coordinates", 10);
      astFindFits(fitschan, "CTYPE1", NULL, 0);
      ffs2c("RA---TAN", valstr, status);
      ffmkky("CTYPE1", valstr, comment, card, status);
      astPutFits(fitschan, card, 1);
      astFindFits(fitschan, "CTYPE2", NULL, 0);
      ffs2c("DEC--TAN", valstr, status);
      ffmkky("CTYPE2", valstr, comment, card, status);
      astPutFits(fitschan, card, 1);
      astClear(fitschan, "Card");
      wcsinfo = astRead(fitschan);
      if ( wcsinfo == AST__NULL ) {
         cxwarn("genckeywcs: Failed to generate wcs info", 5);
         *status = -1;
         astEnd;
         return;
      } 
   }
   astExport(wcsinfo);
   wcslist[i] = wcsinfo;
   wcsref[i]++;
   safestrcpy(wcsid, wcsname[i], wcslen);
   astEnd;
}
FCALLSCSUB13(genckeywcs,GENCKEYWCS,genckeywcs,INTV,STRING,STRING,STRING,STRING,DOUBLEV,DOUBLEV,DOUBLEV,DOUBLE,INT,INT,PSTRING,PINT)

void genhdrwcs (char *mapid, int wcslen, char *wcsid, int *status) {
/*
 *  Generate AST FrameSet from internal header associated with mapid
 *  Save as managed wcsid
 *
 *  mapid    I FITS file to read WCS info from
 *  wcslen   I Maximum length of wcsid
 *  wcsid    O WCS id associated with newly read in WCS info
 *  status   O Error flag (0 = OK)
 */
   int i;

   *status = 0;
   strcpy(wcsid,"");
   i = getemptywcsidx();
   if ( i == -1 ) {  /* No more wcs slots? */
      *status = -1;
      return;
   }

   wcslist[i] = hdrtowcs(mapid);
   if ( wcslist[i] == NULL ) {
      *status = -1;
      return;
   }
   wcsref[i]++;
   safestrcpy(wcsid, wcsname[i], wcslen);
}
FCALLSCSUB4(genhdrwcs,GENHDRWCS,genhdrwcs,STRING,INT,PSTRING,PINT)

void copywcs (char *wcsid, int idlen, char *copyid, int *status) {
/*
 *  Make a (deep) copy of wcs structure and return new id
 *
 *  wcsid    I FITS file to read WCS info from
 *  idlen    I Maximum length of copyid
 *  copyid   O WCS id associated with new copy
 *  status   O Error flag (0 = OK)
 */
   AstFrameSet *wcsinfo;
   int i;

   *status = 0;
   strcpy(copyid,"");
   i = getemptywcsidx();
   if ( i == -1 ) {  /* No more wcs slots? */
      *status = -1;
      return;
   }
   wcsinfo = getmapwcs(wcsid);  /* If applicable, use mapid's wcsid */
   if ( wcsinfo == NULL ) {
      *status = -1;
      return;
   }

   wcslist[i] = astCopy(wcsinfo);
   wcsref[i]++;
   safestrcpy(copyid, wcsname[i], idlen);
}
FCALLSCSUB4(copywcs,COPYWCS,copywcs,STRING,INT,PSTRING,PINT)

void wcssetdet(char *wcsid, int deqc, double *imgref, double *detref,
               double *begzm, int *status) {
/*
 *  Define detector coordinate frame in existing WCS frameset
 *    Domain = PIXEL
 *
 *  wcsid     I WCS id associated with existing WCS frameset
 *  deqc      I Whether detector and image frame are identical
 *  imgref[2] I Reference pixel in image frame
 *  detref[2] I Reference pixel in detector frame
 *  begzm[2] I/O Zoom (rebin) factor between image and detector frames
 *  status    O Error flag (0 = OK)
 */

   AstFrameSet *wcsinfo;

   int nframe, i, igrid, isky, ipixel, icurr;
   AstFrame *frame;
   AstMapping *mapping;
   char const *domain, *ident;
   double imgplus[2], detplus[2];
   double xin[2], yin[2], xout[2], yout[2];
   char buff[100];

   *status = 0;

   /*
    *  Get frameset associated with wcsid
    */
   wcsinfo = getwcs(wcsid);
   if ( wcsinfo == NULL ) {
      *status = -1;
      return;
   }

   /*
    *  There are generally 3 frames, Domain = GRID,SKY,PIXEL
    */
   igrid  = 0;
   isky   = 0;
   ipixel = 0;
   icurr = 0;
   nframe = astGetI(wcsinfo, "Nframe");
   sprintf(buff, "wcssetdet: Nframe: %d", nframe);
   cxwrite(buff, 25);

   for ( i = 1; i <= nframe; i++ ) {
      frame = astGetFrame(wcsinfo, i);
      domain = astGetC(frame, "Domain");
      ident = astGetC(frame, "Ident");
      if ( strcmp(domain, "GRID") == 0 ) {
         if ( !igrid ) igrid = i;
      } else if ( strcmp(domain, "SKY") == 0 ) {
         if ( !isky ) isky = i;
         if ( !icurr ) icurr = isky; /* Use first sky frame as current frame */
      } else if ( strcmp(domain, "PHYSICAL") == 0 || 
                  strcmp(domain, "PIXEL") == 0    ||
                  strncmp(domain, "RAW", 3) == 0  ||
                  strcmp(ident, "P") == 0 ) {
          if ( !ipixel ) { 
              if ( !igrid ) {
                 ipixel = i; 
              } else {
/*
 *  Validate PIXEL frame - must simplify to a UnitMap or WinMap
 */
                 astBegin;
                 mapping = astGetMapping(wcsinfo, igrid, i);
                 if ( astIsAWinMap(mapping) || astIsAUnitMap(mapping) ) { 
                    ipixel = i;
                 } else {
                    sprintf(buff, "Frame %d has non-linear relationship to image grid - cannot be detector frame", i);
                    cxwarn(buff, 15);
                 }
                 astEnd;
              }
          }
      } else if ( !icurr ) {
         icurr = i;  /* Pick first unclassifiable frame as current, */
                     /* unless there is a sky frame                 */ 
      }
      sprintf(buff, "wcssetdet: Frame %d: %s: %s", i, ident, domain);
      cxwrite(buff, 25);
   }
   sprintf(buff, "wcssetdet: Base: %d", astGetI(wcsinfo, "Base"));
   cxwrite(buff, 25);
   sprintf(buff, "wcssetdet: Current: %d", astGetI(wcsinfo, "Current"));
   cxwrite(buff, 25);

   if ( !igrid ) {
      cxwrite(" ERROR: WCS information has no GRID frame", 5);
      *status = -1;
      return;
   }
/*
 *  Validate PIXEL frame - must simplify to a UnitMap or WinMap
 */
   if ( ipixel ) {   
      astBegin;
      mapping = astGetMapping(wcsinfo, igrid, ipixel);
      if ( !(astIsAWinMap(mapping) || astIsAUnitMap(mapping)) ) { 
         sprintf(buff, "Frame %d has non-linear relationship to image grid - cannot be detector frame", ipixel);
         cxwarn(buff, 15);
         ipixel = 0;
      }
      astEnd;
   }

   if ( ipixel ) {        /* PIXEL frame already exists */
/*
 *   Update zoom and reference pixel
 */
      frame = astGetFrame(wcsinfo, ipixel);
      astSetC(frame, "Domain", "PIXEL");
      astSetC(frame, "Ident", "P");
      setupframeset(wcsinfo, "GRID", "PIXEL");
      xin[0] = imgref[0];
      yin[0] = imgref[1];
      xin[1] = imgref[0] + 1;
      yin[1] = imgref[1] + 1;
      astTran2(wcsinfo, 2, xin, yin, 1, xout, yout);
      detref[0] = xout[0];
      detref[1] = yout[0];
      begzm[0] = fabs(xout[1] - xout[0]);
      begzm[1] = fabs(yout[1] - yout[0]);
      if ( icurr ) astSetI(wcsinfo, "Current", icurr);
      return;
   }

   /*
    *  If image/detector frames, add detector frame as UnitMap
    */
   if ( deqc ) {
      astBegin;
      astAddFrame(wcsinfo, igrid, astUnitMap(2, ""), 
                                  astFrame(2, "Domain=PIXEL,Ident=P"));
      if ( icurr ) astSetI(wcsinfo, "Current", icurr);
      astEnd;
      return;
   }

   /*
    *  Base PIXEL frame on input img/det pair and zoom
    *  Use reference pixel and pixel + 1 in img frame as window
    */
   imgplus[0] = imgref[0] + 1;
   imgplus[1] = imgref[1] + 1;
   detplus[0] = detref[0] + begzm[0];
   detplus[1] = detref[1] + begzm[1];
   astBegin;
   astAddFrame(wcsinfo, igrid, 
               astWinMap(2, imgref, imgplus, detref, detplus, ""),
               astFrame(2, "Domain=PIXEL,Ident=P"));
   if ( icurr ) astSetI(wcsinfo, "Current", icurr);
   astEnd;

}
FCALLSCSUB6(wcssetdet,WCSSETDET,wcssetdet,STRING,LOGICAL,DOUBLEV,DOUBLEV,DOUBLEV,PINT)

void wcsrotate (char *wcsid, double *rotpix, double angle, int *status) {
/*
 *  Rotate grid coordinates with respect to the wcs
 *
 *  wcsid     I FITS file to read WCS info from
 *  rotpix[2] I Location of rotation axis
 *  angle     I Rotation angle in degrees (+ve = clockwise)
 *  status    O Error flag (0 = OK)
 */
   AstFrameSet *wcsinfo;
   int igrid, ipixel, icurr;
   double ina[2], inb[2], outa[2], outb[2], matrix[4], radang;
   double xin[2], yin[2], xout[2], yout[2];

   *status = 0;

   /*
    *  Get frameset associated with wcsid
    */
   wcsinfo = getwcs(wcsid);
   if ( wcsinfo == NULL ) {
      *status = -1;
      return;
   }
   igrid = getframeidx(wcsinfo, "GRID");
   if ( igrid == 0 ) {
      cxwrite(" wcsrotate: No GRID frame to remap", 5);
      *status = -1;
      return;
   }
   ipixel = getframeidx(wcsinfo, "PIXEL");
   if ( ipixel == 0 ) {
      cxwrite(" wcsrotate: No PIXEL frame to remap", 5);
      *status = -1;
      return;
   }
   icurr = astGetI(wcsinfo, "Current");
   setupframeset(wcsinfo, "GRID", "GRID");

   radang = angle*D2R;
   matrix[0] = cos(radang);
   matrix[1] = -sin(radang);
   matrix[2] = sin(radang);
   matrix[3] = cos(radang);

   astBegin;
/*
 *  Translate coords such that rotation axis is zero
 */
   ina[0] = rotpix[0];
   ina[1] = rotpix[1];
   inb[0] = rotpix[0] + 1.;
   inb[1] = rotpix[1] + 1.;
   outa[0] = 0.;
   outa[1] = 0.;
   outb[0] = 1.;
   outb[1] = 1.;
   astRemapFrame(wcsinfo, igrid, 
                 astWinMap(2, ina, inb, outa, outb, ""));

   xin[0] = rotpix[0];
   yin[0] = rotpix[1];
   astTran2(wcsinfo, 1, xin, yin, 1, xout, yout);
   ina[0] = xout[0];
   ina[1] = yout[0];
   inb[0] = ina[0] + 1.;
   inb[1] = ina[1] + 1.;
   astRemapFrame(wcsinfo, ipixel, 
                 astWinMap(2, ina, inb, outa, outb, ""));

/*
 *  Perform rotation
 */
   astRemapFrame(wcsinfo, igrid, astMatrixMap(2, 2, 0, matrix, ""));
   astRemapFrame(wcsinfo, ipixel, astMatrixMap(2, 2, 0, matrix, ""));
/*
 *  Move zero of coords back
 */
   ina[0] = 0.;
   ina[1] = 0.;
   outa[0] = rotpix[0];
   outa[1] = rotpix[1];
   inb[0] = 1.;
   inb[1] = 1.;
   outb[0] = rotpix[0] + 1.;
   outb[1] = rotpix[1] + 1.;
   astRemapFrame(wcsinfo, igrid, 
                 astWinMap(2, ina, inb, outa, outb, ""));

   outa[0] = xout[0];
   outa[1] = yout[0];
   outb[0] = xout[0] + 1.;
   outb[1] = yout[0] + 1.;
   astRemapFrame(wcsinfo, ipixel, 
                 astWinMap(2, ina, inb, outa, outb, ""));

   astSetI(wcsinfo, "Current", icurr);
   astEnd;
}
FCALLSCSUB4(wcsrotate,WCSROTATE,wcsrotate,STRING,DOUBLEV,DOUBLE,PINT)

void wcsaxswap (char *wcsid, int *status) {
/*
 *  Swap axes in wcsid
 *
 *  wcsid     I FITS file to read WCS info from
 *  status    O Error flag (0 = OK)
 */
   AstFrameSet *wcsinfo;
   int igrid, ipixel, inperm[2], outperm[2];

   *status = 0;

   /*
    *  Get frameset associated with wcsid
    */
   wcsinfo = getwcs(wcsid);
   if ( wcsinfo == NULL ) {
      *status = -1;
      return;
   }
   igrid = getframeidx(wcsinfo, "GRID");
   if ( igrid == 0 ) {
      cxwrite(" wcsaxswap: No GRID frame to remap", 5);
      *status = -1;
      return;
   }
   ipixel = getframeidx(wcsinfo, "PIXEL");
   if ( ipixel == 0 ) {
      cxwrite(" wcsaxswap: No PIXEL frame to remap", 5);
      *status = -1;
      return;
   }

   astBegin;
/*
 *  Perform swap (aka permute axes)
 */
   inperm[0] = 1;
   inperm[1] = 2;
   outperm[0] = 2;
   outperm[1] = 1;
   astRemapFrame(wcsinfo, igrid, 
                 astPermMap(2, inperm, 2, outperm, NULL, ""));
   astRemapFrame(wcsinfo, ipixel, 
                 astPermMap(2, inperm, 2, outperm, NULL, ""));
   astEnd;

}
FCALLSCSUB2(wcsaxswap,WCSAXSWAP,wcsaxswap,STRING,PINT)

void wcslintrf(char *wcsid, double *cenpix, int nszx, int nszy,
              double *newzm, int *status) {
/*
 * Modify existing frameset to rebin/recenter to new map
 *
 *  wcsid     I WCS id associated with existing WCS frameset
 *  cenpix[2] I New center in coords of existing (GRID) image
 *  nszx/y    I Size of new image
 *  newzm[2]  I Zoom (rebin) factor between existing and new image
 *  status    O Error flag (0 = OK)
 */

   AstFrameSet *wcsinfo;
   int igrid;
   double ina[2], inb[2], outa[2], outb[2];

   *status = 0;

   /*
    *  Get frameset associated with wcsid
    */
   wcsinfo = getwcs(wcsid);
   if ( wcsinfo == NULL ) {
      *status = -1;
      return;
   }

   igrid = getframeidx(wcsinfo, "GRID");
   if ( igrid == 0 ) {
      cxwrite(" wcslintrf: No GRID frame to remap", 5);
      *status = -1;
      return;
   }

   astBegin;
   /*
    *  Base remap on input cenpix, new size and zoom
    *  Use center pixel and pixel + 1 in new frame as window
    */
   ina[0] = cenpix[0];
   ina[1] = cenpix[1];
   inb[0] = ina[0] + newzm[0];
   inb[1] = ina[1] + newzm[1];
   outa[0] = nszx/2 + 0.5;
   outa[1] = nszy/2 + 0.5;
   outb[0] = outa[0] + 1;
   outb[1] = outa[1] + 1;

   astRemapFrame(wcsinfo, igrid, 
                 astWinMap(2, ina, inb, outa, outb, ""));
   astEnd;

}
FCALLSCSUB6(wcslintrf,WCSLINTRF,wcslintrf,STRING,DOUBLEV,INT,INT,DOUBLEV,PINT)

void wcstohdr(char *wcsid, char *mapid, int *status) {
/*
 * Assign coordinate keys in internal header associated with mapid
 *   to values stored in wcs frameset
 *
 *  wcsid    I WCS id associated with existing WCS info
 *  mapid    I Id associated with header to modify
 *  status   O Error flag (0 = OK)
 */
   AstFrameSet *wcsinfo, *wcscopy;
   AstFitsChan *fitschan;
   int ipixel;
   int lataxis = 1;
   int nocrota = 0;
   char card[FLEN_CARD];
   char valstr[FLEN_VALUE];

   char ctype1[FLEN_VALUE], ctype2[FLEN_VALUE];
   char cunit1[FLEN_VALUE], cunit2[FLEN_VALUE];
   double crpix1, crpix2, crval1, crval2;
   double cdelt1, cdelt2, crota2, equinox;
   double xin[2], yin[2], xout[2], yout[2];
   double dd, zmx, zmy, ddelt1, ddelt2, drpix1, drpix2, ximnorth;
   int hdrstat;
   
   *status = 0;

   /*
    *  Get frameset associated with wcsid
    */
   wcsinfo = getmapwcs(wcsid);
   if ( wcsinfo == NULL ) {
      *status = -1;
      return;
   }

   /*
    * Create copy of frameset and remove all but SKY GRID and PIXEL
    */
   wcscopy = astCopy(wcsinfo);

   /*
   ** try to force FITS-AIPS++ encoding
   ** (since the rest of what we do depends on this - bleh )
   */
   fitschan = astFitsChan(NULL, NULL, "Encoding=FITS-AIPS++");
   astWrite(fitschan, wcscopy);
   if ( !astOK || astGetI(fitschan, "Ncard") == 0 ) {
      cxwarn("Failed to write FITS-AIPS++ encoding to internal header, trying FITS-WCS encoding...", 15);
      astClearStatus;
      fitschan = astFitsChan(NULL, NULL, "Encoding=FITS-WCS,CDMatrix=1");
      astWrite(fitschan, wcscopy);
      if ( !astOK ) {
         cxwarn("Failed to write FITS-WCS encoding to internal header", 5);
         astClearStatus;
      }
   }

   astClear(fitschan, "Card");
   while ( astFindFits(fitschan, "%f", card, 1) ) {
      cxwrite(card, 30);
   }

   strcpy(ctype1, "");
   affgkys(fitschan, "CTYPE1", ctype1);
   strcpy(ctype2, "");
   affgkys(fitschan, "CTYPE2", ctype2);

   crpix1 = 0.;
   affgkyd(fitschan, "CRPIX1", &crpix1);
   crpix2 = 0.;
   affgkyd(fitschan, "CRPIX2", &crpix2);
   crval1 = 0.;
   affgkyd(fitschan, "CRVAL1", &crval1);
   crval2 = 0.;
   affgkyd(fitschan, "CRVAL2", &crval2);

   strcpy(cunit1, "");
   affgkys(fitschan, "CUNIT1", cunit1);
   strcpy(cunit2, "");
   affgkys(fitschan, "CUNIT2", cunit2);

   cdelt1 = DNULL(); 
   affgkyd(fitschan, "CDELT1", &cdelt1);
   cdelt2 = DNULL(); 
   affgkyd(fitschan, "CDELT2", &cdelt2);
   if ( ISDNULL(cdelt1) || ISDNULL(cdelt2) ) {
      cdelt1 = 1;
      affgkyd(fitschan, "CD1_1", &cdelt1);
      cdelt2 = 1;
      affgkyd(fitschan, "CD2_2", &cdelt2);
   }
                  
   equinox = DNULL();
   affgkyd(fitschan, "EQUINOX", &equinox);
   if ( ISDNULL(equinox) ) {
      affgkyd(fitschan, "EPOCH", &equinox);
   }
/*
 *  Force RA to axis 1, Dec to axis 2
 */
   if ( strncmp( ctype1, "DEC", 3 ) == 0 || 
        strncmp( ctype1, "LAT", 3 ) == 0 ) {
       strcpy(valstr, ctype1);
       strcpy(ctype1, ctype2);
       strcpy(ctype2, valstr);
       strcpy(valstr, cunit1);
       strcpy(cunit1, cunit2);
       strcpy(cunit2, valstr);
       dd = crval1;
       crval1 = crval2;
       crval2 = dd;
       /*dd = crpix1;
       crpix1 = crpix2;
       crpix2 = dd;*/
       dd = cdelt1;
       cdelt1 = cdelt2;
       cdelt2 = dd;
       cdelt1 = -cdelt1;
       lataxis = 1;
   } else {
       lataxis = 2;
   }
   /* HACK!! */
   crota2 = DNULL();
   sprintf( &valstr[ 0 ], "CROTA%d", lataxis );
   affgkyd( fitschan, valstr, &crota2 );
   if ( ISDNULL( crota2 ) ) {
       nocrota = 1;
   }
   getwcscrota( fitschan, lataxis, &crota2, &cdelt1, &cdelt2 );

   /* if we didn't have one before, and don't need it, don't save it */
   if ( crota2 == 0.0 && nocrota ) {
       crota2 = DNULL( );
   }

   astAnnul(fitschan);
/*
 *  Derived keywords
 */
   ipixel = getframeidx(wcscopy, "PIXEL");
   if ( ipixel == 0 ) {
      cxwrite("wcstohdr: No PIXEL frame", 5);
      *status = -1;
      return;
   }
   astSetI(wcscopy, "Current", ipixel);

   xin[0] = crpix1;
   yin[0] = crpix2;
   xin[1] = crpix1 + 1.;
   yin[1] = crpix2 + 1.;
   astTran2(wcscopy, 2, xin, yin, 1, xout, yout);
   zmx = fabs(xout[1] - xout[0]);
   zmy = fabs(yout[1] - yout[0]);
   ddelt1 = cdelt1/zmx;
   ddelt2 = cdelt2/zmy;
   drpix1 = xout[0];
   drpix2 = yout[0];

   astAnnul(wcscopy);

   if ( crota2 != 0. ) {
      ximnorth = crota2 + 90.;
   } else {
      ximnorth = -270.;
   }
/*
 *  Set internal keywords from wcs info
 */
   hdrstat = 0;
   GHEADD(mapid, "CRPIX1", crpix1, 1, hdrstat);
   GHEADD(mapid, "CRPIX2", crpix2, 1, hdrstat);
   GHEADD(mapid, "DRPIX1", drpix1, 1, hdrstat);
   GHEADD(mapid, "DRPIX2", drpix2, 1, hdrstat);
   GHEADD(mapid, "CRVAL1", crval1, 1, hdrstat);
   GHEADD(mapid, "CRVAL2", crval2, 1, hdrstat);
   GHEADS(mapid, "CTYPE1", ctype1, 1, hdrstat);
   GHEADS(mapid, "CTYPE2", ctype2, 1, hdrstat);
   GHEADS(mapid, "CUNIT1", cunit1, 1, hdrstat);
   GHEADS(mapid, "CUNIT2", cunit2, 1, hdrstat);
   GHEADD(mapid, "CDELT1", cdelt1, 1, hdrstat);
   GHEADD(mapid, "CDELT2", cdelt2, 1, hdrstat);
   GHEADD(mapid, "DDELT1", ddelt1, 1, hdrstat);
   GHEADD(mapid, "DDELT2", ddelt2, 1, hdrstat);
   GHEADD(mapid, "CROTA2", crota2, 1, hdrstat);
   GHEADD(mapid, "XIMNORTH", ximnorth, 1, hdrstat);
   GHEADD(mapid, "EQUINOX", equinox, 1, hdrstat);
   GHEADD(mapid, "ZMX", zmx, 1, hdrstat);
   GHEADD(mapid, "ZMY", zmy, 1, hdrstat);

}
FCALLSCSUB3(wcstohdr,WCSTOHDR,wcstohdr,STRING,STRING,PINT)

/*
** Get the CROTA2 keyword, or fudge it
** and some others 
** This is basically the same as ffgiwcs in CFITSIO, 
** but since we don't have fits files here, we have to improvise
*/
void getwcscrota( AstFitsChan *fitschan, int lataxis, 
                  double* crota2, double* cd1_1, double* cd2_2 ) {

    char key[ 7 ];

    int lonaxis = 1;
    
    double rho_s, rho_b, rho_tmp;

    double cd2_1;    /* CD2_1 if RA is 1st axis */
    double cd1_2;    /* CD1_2 if RA is 2nd axis */

    double tol = .0002;
    double pi  = 3.1415926535897932;
    
    cd1_2 = cd2_1 = 0.;
    
    /* assume it's the 2nd axis if not 1 or 2 (should not happen) */
    if ( lataxis < 1 || lataxis > 1 ) {
        lataxis = 2;
        lonaxis = 1;
    } else {
        lonaxis = 2;
    }
    
    /* this assumes an orthogonal coordinate frame - bad, very bad */
    *cd1_1 = DNULL();
    sprintf( &key[ 0 ], "CDELT%d", lonaxis );
    affgkyd( fitschan, key, cd1_1 );
    *cd2_2 = DNULL();
    sprintf( &key[ 0 ], "CDELT%d", lataxis );
    affgkyd( fitschan, key, cd2_2 );
    if ( ISDNULL( *cd1_1 ) || ISDNULL( *cd2_2 ) ) {

        *cd1_1 = 1.;
        sprintf( &key[ 0 ], "CD%d_%d", lonaxis, lonaxis );
        affgkyd( fitschan, key, cd1_1 );
        
        *cd2_2 = 1.;
        sprintf( &key[ 0 ], "CD%d_%d", lataxis, lataxis );
        affgkyd( fitschan, key, cd2_2 );
        
        sprintf( &key[ 0 ], "CD%d_%d", lataxis, lonaxis );
        affgkyd( fitschan, key, &cd2_1 );

        sprintf( &key[ 0 ], "CD%d_%d", lonaxis, lataxis );
        affgkyd( fitschan, key, &cd1_2 );
    }

    /* try to read the keyword */
    *crota2 = DNULL( );
    sprintf( &key[ 0 ], "CROTA%d", lataxis );
    affgkyd( fitschan, key, crota2 );
    if ( ISDNULL( *crota2 ) ) {

        /* if that fails, calculate it
        ** if there is no CD matrix, then we have already set it to ((1,0),(0,1))
        ** so it should produce a 0 deg rotation
        */
        cxwarn( "Could not determine rotation angle, calculating it", 15 );
        
        /* see Calabretta & Greison, A&A 396, 1077-1122, 2002. */
        /* or cfitsio/wcssub.c */
        rho_s = atan2( cd2_1, *cd1_1 );
        rho_b = atan2( -cd1_2, *cd2_2 );

        if ( rho_b < rho_s ) {
            rho_tmp = rho_s;
            rho_s   = rho_b;
            rho_b   = rho_tmp;
        }
        if ( ( rho_b - rho_s ) > ( pi / 2.0 ) ) {
            rho_s += pi;
        }

        /* warn about skew (probably for the 2nd time) */
        if ( fabs( rho_s - rho_b ) > tol ) {
            cxwarn( "Non-orthogonal coordinate frame detected", 5 );
            cxwarn( "XImage does not handle these cases well\n", 5 );
        }

        /* use the average */
        *crota2 = ( rho_s + rho_b ) / 2.0;

        /* scale the cdelt1/2 */
        *cd1_1 /= cos( *crota2 );
        *cd2_2 /= cos( *crota2 );
        
        /* degrees */
        *crota2 *= 180.0 / pi;

        /* use the "normally chosen" one */
        if ( *cd2_2 < 0 ) {
            *cd1_1   = -( *cd1_1 );
            *cd2_2   = -( *cd2_2 );
            *crota2 -= 180.;
        }
    }
    
    /* if DEC is first axis, rotate crota2 by 90, and invert Y-axis */
    if ( lataxis == 1 ) {
        *crota2 -= 90.0;
        *cd1_1  = -( *cd1_1 );
    }
}
       
void wcsskyinfo(char *wcsid, char *system, char *xlab, char *ylab,
                double *equinox) {
/*
 * Get information on the sky frame for a particular wcsid
 *
 *  wcsid    I WCS id associated with existing WCS info
 *  system   O System property recorded in SKY frame
 *  xlab     O Label for sky x value (e.g. RA, lii)
 *  ylab     O Label for sky y value (e.g. Dec, bii)
 *  equinox  O Equinox of coordinate system
 */
   AstFrameSet *wcsinfo;
   AstFrame *skyframe;
   int isky;

   strcpy(system, "UNKNOWN");
   strcpy(xlab, "Xsky");
   strcpy(ylab, "Ysky");
   *equinox = 0.;

   /*
    *  Get frameset associated with wcsid
    */
   wcsinfo = getwcs(wcsid);
   if ( wcsinfo == NULL ) return;
   isky = getframeidx(wcsinfo, "SKY");
   if ( !isky ) return;

   astBegin;
   skyframe = astGetFrame(wcsinfo, isky);

   strcpy(system, astGetC(skyframe, "System"));
   if ( strncmp(system,"FK",2) == 0 || 
        strcmp(system,"ICRS") == 0 ||
        strcmp(system,"EQUATORIAL") == 0 ) {
      strcpy(xlab, "RA");
      strcpy(ylab, "Dec");
   } else if ( strcmp(system,"GALACTIC") == 0 ) {
      strcpy(xlab, "lii");
      strcpy(ylab, "bii");
   }
   *equinox = astGetD(skyframe, "Equinox");
   astEnd;
}
FCALLSCSUB5(wcsskyinfo,WCSSKYINFO,wcsskyinfo,STRING,PSTRING,PSTRING,PSTRING,PDOUBLE)

void wcsfrminfo(char *id, char *system, char *projection, char *xlab, 
                char *ylab, char *unit, double *equinox) {
/*
 * Get information on the current frame for a particular mapid/wcsid
 *
 *  id         I Wcsid or mapid on which to get coordinate info
 *  system     O System property
 *  projection O Projection property
 *  xlab       O Label for sky x value (e.g. RA, lii)
 *  ylab       O Label for sky y value (e.g. Dec, bii)
 *  unit       O Unit of axes
 *  equinox    O Equinox of coordinate system
 */
   AstFrameSet *wcsinfo;
   AstFrame *frame;

   strcpy(system, "UNKNOWN");
   strcpy(projection, "");
   strcpy(xlab, "X");
   strcpy(ylab, "Y");
   strcpy(unit, "");
   *equinox = 0.;

   /*
    *  Get frameset associated with wcsid
    */
   wcsinfo = getmapwcs(id);  /* If applicable, use mapid's wcsid */
   if ( wcsinfo == NULL ) return;

   strcpy(system, astGetC(wcsinfo, "System"));
   frame = (AstFrame *) wcsinfo;
   if ( astIsASkyFrame(astGetFrame(wcsinfo, AST__CURRENT)) ) {
      strcpy(projection, astGetC(wcsinfo, "Projection"));
      *equinox = astGetD(wcsinfo, "Equinox");
   }
   if ( strncmp(system,"FK",2) == 0 || 
        strcmp(system,"ICRS") == 0 || 
        strcmp(system,"EQUATORIAL") == 0 ) {
      strcpy(xlab, "RA");
      strcpy(ylab, "Dec");
   } else if ( strcmp(system,"GALACTIC") == 0 ) {
      strcpy(xlab, "lii");
      strcpy(ylab, "bii");
   }
   strcpy(unit, getframeunit((AstFrame *) wcsinfo));
}
FCALLSCSUB7(wcsfrminfo,WCSFRMINFO,wcsfrminfo,STRING,PSTRING,PSTRING,PSTRING,PSTRING,PSTRING,PDOUBLE)

void wcsfrmcopy(char *wcsid, char *frameid, char *newdomain, int *status) {
/*
 *  Copy indicated frame
 *
 *  wcsid     I WCS id associated with existing WCS frameset
 *  frameid   I Frame id associated with frame to copy
 *  newdomain I If not blank, use as domain for new frame
 *  status    O Error flag (0 = OK)
 */

   AstFrameSet *wcsinfo;
   AstFrame *frame;
   int iframe, icurr;
   char *msg;

   *status = 0;

   /*
    *  Get frameset associated with wcsid
    */
   wcsinfo = getwcs(wcsid);
   if ( wcsinfo == NULL ) {
      *status = -1;
      return;
   }

   iframe = getframeidx(wcsinfo, frameid);
   if ( iframe == 0 ) {
      msg = strcatalloc("wcsfrmcopy: frame not found: ", frameid);
      cxwrite(msg, 5);
      free(msg);
      *status = -1;
      return;
   }

   astBegin;
   icurr = astGetI(wcsinfo,"Current");
   frame = astFrame(2, "");
   if ( *newdomain ) astSetC(frame, "Domain", newdomain);
   astAddFrame(wcsinfo, iframe, astUnitMap(2, ""), frame);
   astSetI(wcsinfo,"Current",icurr);
   astEnd;
}
FCALLSCSUB4(wcsfrmcopy,WCSFRMCOPY,wcsfrmcopy,STRING,STRING,STRING,PINT)

void wcsfrmdel(char *wcsid, char *frameid, int *status) {
/*
 *  Delete indicated frame
 *
 *  wcsid     I WCS id associated with existing WCS frameset
 *  frameid   I Frame id associated with frame to copy
 *  status    O Error flag (0 = OK)
 */

   AstFrameSet *wcsinfo;
   int iframe;

   *status = 0;

   /*
    *  Get frameset associated with wcsid
    */
   wcsinfo = getwcs(wcsid);
   if ( wcsinfo == NULL ) {
      *status = -1;
      return;
   }

   iframe = getframeidx(wcsinfo, frameid);
   if ( iframe == 0 ) {
      *status = -1;
      return;
   }
   astRemoveFrame(wcsinfo, iframe);
}
FCALLSCSUB3(wcsfrmdel,WCSFRMDEL,wcsfrmdel,STRING,STRING,PINT)

void wcsformat(char *system, double xsky, double ysky, 
               char *xstr, char *ystr) {
/*
 * Format coordinates based on system
 *
 *  system   I Coordinate system (e.g. FK5, GALACTIC) or WCSID
 *  x/ysky   I Input coordinates (degrees)
 *  x/ystr   O Formatted coordinates
 */
   AstFrame *frame;
   char opt[100];
   double pt[2];
   const char *label;
   int match, exact;
   int issky = 1;

   strcpy(xstr, "");
   strcpy(ystr, "");

   if ( !astOK ) astClearStatus;

   /*
    *  Assume sky coordinates unless unknown or cartesian system
    */
   fits_compare_str("Unknown", system, 0, &match, &exact);
   if ( match ) issky = 0;
   fits_compare_str("Cartesian", system, 0, &match, &exact);
   if ( match ) issky = 0;

   astBegin;
   fits_compare_str("WCS*", system, 0, &match, &exact);
   if ( match ) {
      frame = (AstFrame *) astCopy(
                             astGetFrame(getwcs(system), AST__CURRENT));
      issky = astIsASkyFrame(frame);
   } else if ( issky ) {
      sprintf(opt, "System = %s\n", system);
      frame = (AstFrame *) astSkyFrame(opt);
   } else {
      frame = astFrame(2, "");
   }

   /* Use letters to delimit R.A. */
   label = astGetC(frame, "Label(1)");
   if ( strcmp(label, "Right ascension") == 0 ) {
      astSetC(frame, "Format(1)", "lhms.2");
   } else if ( strcmp(label, "Declination") == 0 ) {
      astSetC(frame, "Format(1)", "+dms.2");
   }
   label = astGetC(frame, "Label(2)");
   if ( strcmp(label, "Declination") == 0 ) {
      astSetC(frame, "Format(2)", "+dms.2");
   } else if ( strcmp(label, "Right ascension") == 0 ) {
      astSetC(frame, "Format(2)", "lhms.2");
   }
/*
   astSetI(frame, "Digits", 9);
*/
   pt[0] = xsky;
   pt[1] = ysky;
   if ( issky ) {
      if ( !isastbad(pt[0]) ) pt[0] = pt[0]*D2R;
      if ( !isastbad(pt[1]) ) pt[1] = pt[1]*D2R;
   }
   astNorm(frame, pt);
   if ( !isastbad(pt[0]) ) strcpy(xstr, astFormat(frame, 1, pt[0]));
   if ( !isastbad(pt[1]) ) strcpy(ystr, astFormat(frame, 2, pt[1]));
   astEnd;
}
FCALLSCSUB5(wcsformat,WCSFORMAT,wcsformat,STRING,DOUBLE,DOUBLE,PSTRING,PSTRING)

void wcscnvbeg(char *wcsid, char *inframeid, char *outframeid, 
               int *wcscnvidx, int *status) {
/*
 *  Setup frameset to convert from in frame to out frame
 *
 *  wcsid      I WCS id associated with existing WCS frameset
 *  inframeid  I Frame identifier for in coords (see getframeidx)
 *  outframeid I Frame identifier for out coords (see getframeidx)
 *  wcscnvidx  O Index used internally by wcscnv[rd] (indicates Mapping)
 *  status     O Error flag (0 = OK)
 */
   AstFrameSet *wcsinfo, *wcscopy;
   AstFrame *inframe, *outframe;
   int cnvidx, inframeidx, outframeidx;
   char *msg;
   const char *system;

   *wcscnvidx = -1;
   *status = -1;

   /*
    *  Get slot to save conversion pointer
    */
   cnvidx =  getemptycnvidx();
   if ( cnvidx < 0 ) return;

   /*
    *  Get frameset associated with wcsid
    */
   wcsinfo = getwcs(wcsid);
   if ( wcsinfo == NULL ) return;

   /*
    *  Set Base and Current attributes to prepare for conversion
    */
   inframeidx = getframeidx(wcsinfo, inframeid);
   if ( inframeidx == 0 ) {
      msg = strcatalloc("wcscnvbeg: Input frame not found: ", inframeid);
      cxwrite(msg, 25);
      free(msg);
      return;
   }
   outframeidx = getframeidx(wcsinfo, outframeid);
   if ( outframeidx == 0 ) {
      msg = strcatalloc("wcscnvbeg: Output frame not found: ", outframeid);
      cxwrite(msg, 25);
      free(msg);
      return;
   }

   wcscopy = astCopy(wcsinfo);
   cnvlist[cnvidx].mapping = (AstMapping *) wcscopy;
   astSetI(wcscopy, "Base", inframeidx);
   astSetI(wcscopy, "Current", outframeidx);

   astBegin;
   inframe = astGetFrame(wcscopy, AST__BASE);
   cnvlist[cnvidx].inissky = astIsASkyFrame(inframe);
   if ( cnvlist[cnvidx].inissky ) {
      system = astGetC(inframe, "System");
      if ( strncmp(system,"FK",2) == 0 || 
           strcmp(system,"ICRS") == 0 ||
           strcmp(system,"EQUATORIAL") == 0 ) {
         cnvlist[cnvidx].insys = SYSRADEC;
      } else {
         cnvlist[cnvidx].insys = SYSOTHER;
      }
   }
   outframe = astGetFrame(wcscopy, AST__CURRENT);
   cnvlist[cnvidx].outissky = astIsASkyFrame(outframe);
   if ( cnvlist[cnvidx].outissky ) {
      system = astGetC(outframe, "System");
      if ( strncmp(system,"FK",2) == 0 || 
           strcmp(system,"ICRS") == 0 ||
           strcmp(system,"EQUATORIAL") == 0 ) {
         cnvlist[cnvidx].outsys = SYSRADEC;
      } else {
         cnvlist[cnvidx].outsys = SYSOTHER;
      }
   }
   astEnd;

   *wcscnvidx = cnvidx;
   *status = 0;
}
FCALLSCSUB5(wcscnvbeg,WCSCNVBEG,wcscnvbeg,STRING,STRING,STRING,PINT,PINT)

void wcscnveqx(int wcscnvidx, int ineqx, int outeqx, int *status) {
/*
 *  Setup conversion to account for input and/or output equinox
 *  Entry of 0 for an equinox means no consideration needed
 *
 *  wcscnvidx I Index used internally by wcscnv[rd] (indicates Mapping)
 *  ineqx     I Input equinox  
 *  outeqx    I Output equinox
 *  status    O Error flag (0 = OK)
 */
   AstFrameSet *wcsinfo;
   AstSkyFrame *insky, *outsky;
   AstFrame *bframe, *cframe;
   AstFrameSet *infrset, *outfrset;
   AstMapping *mapping;
   float fineqx, fouteqx, epoch, equinox;
   const char *system;

   *status = -1;

   if ( wcscnvidx < 0 ) {
       cxwrite("wcscnveqx: invalid wcscnvidx", 5);
       return;
   }
   wcsinfo = (AstFrameSet *) cnvlist[wcscnvidx].mapping;

   insky = NULL;
   outsky = NULL;
   fineqx = ineqx;
   fouteqx = outeqx;

   astBegin;
   bframe = astGetFrame(wcsinfo, AST__BASE);
   cframe = astGetFrame(wcsinfo, AST__CURRENT);
   if ( astIsASkyFrame(bframe) && ineqx != 0 ) {
      epoch = astGetF(bframe, "Epoch");
      equinox = astGetF(bframe, "Equinox");
      system = astGetC(bframe, "System");
      if ( epoch != fineqx && equinox != fineqx ) {
         insky = astCopy(bframe);
         if ( (strcmp(system,"FK5") == 0 || 
               strcmp(system,"ICRS") == 0 ||
               strcmp(system,"EQUATORIAL") == 0) && ineqx == 1950 ) {
            astSetC(insky, "System", "FK4");
         } else if ( strcmp(system,"FK4") == 0 && ineqx == 2000 ) {
            astSetC(insky, "System", "FK5");
         }
         astSetF(insky, "Epoch", fineqx);
         astSetF(insky, "Equinox", fineqx);
      }
   }
   if ( astIsASkyFrame(cframe) && outeqx != 0 ) {
      epoch = astGetF(cframe, "Epoch");
      equinox = astGetF(cframe, "Equinox");
      system = astGetC(cframe, "System");
      if ( epoch != fouteqx && equinox != fouteqx ) {
         outsky = astCopy(cframe);
         if ( strcmp(system,"FK5") == 0 && outeqx == 1950 ) {
            astSetC(outsky, "System", "FK4");
         } else if ( strcmp(system,"FK4") == 0 && outeqx == 2000 ) {
            astSetC(outsky, "System", "FK5");
         }
         astSetF(outsky, "Epoch", fouteqx);
         astSetF(outsky, "Equinox", fouteqx);
      }
   }

   infrset = NULL;
   outfrset = NULL;

   if ( insky != NULL ) {
      infrset = astConvert(insky, astGetFrame(wcsinfo, AST__BASE), "");
   }
   if ( outsky != NULL ) {
      outfrset = astConvert(astGetFrame(wcsinfo, AST__CURRENT), outsky, "");
   }

   mapping = NULL;
   if ( infrset != NULL ) {
      mapping = (AstMapping *) astCmpMap(infrset, wcsinfo, 1, "");
   }
   if ( outfrset != NULL ) {
      if ( mapping != NULL ) {
         mapping = (AstMapping *) astCmpMap(mapping, outfrset, 1, "");
      } else {
         mapping = (AstMapping *) astCmpMap(wcsinfo, outfrset, 1, "");
      }
   }

   if ( mapping != NULL ) {
      astExport(mapping);
      astAnnul(cnvlist[wcscnvidx].mapping);
      cnvlist[wcscnvidx].mapping = mapping;
      if ( insky ) cnvlist[wcscnvidx].inissky = 1;
      if ( outsky ) cnvlist[wcscnvidx].outissky = 1;
   }
   astEnd;
   *status = 0;
}
FCALLSCSUB4(wcscnveqx,WCSCNVEQX,wcscnveqx,INT,INT,INT,PINT)

void wcscnvd(int wcscnvidx, double in1, double in2, 
                            double *out1, double *out2) {
/*
 *   Convert coordinates using frameset as mapping (type double)
 *     Expects sky coord i/o in degrees (AST uses radians)
 *
 *  wcscnvidx I Index from wcscnvbeg (indicates Mapping)
 *  in1/2     I Input coordinates
 *  out1/2    O Output coordinates
 */
   AstMapping *mapping;
   double xin[1];
   double yin[1];
   double xout[1];
   double yout[1];

   if ( wcscnvidx < 0 ) {
      *out1 = 0.;
      *out2 = 0.;
      return;
   }
   
   mapping = cnvlist[wcscnvidx].mapping;

   xin[0] = in1;
   yin[0] = in2;
   if ( cnvlist[wcscnvidx].inissky ) { /* Convert to radians */
      xin[0] = xin[0]*D2R; 
      yin[0] = yin[0]*D2R;
   }
/*
   astSetI(mapping, "Report", 1);
*/

   astTran2(mapping, 1, xin, yin, 1, xout, yout);
   if ( cnvlist[wcscnvidx].outissky ) { /* Convert to degrees */
      if ( !isastbad(xout[0]) ) xout[0] = xout[0]*R2D;
      if ( !isastbad(yout[0]) ) yout[0] = yout[0]*R2D;
      if ( cnvlist[wcscnvidx].outsys == SYSRADEC && 
           !isastbad(xout[0]) && xout[0] < 0. ) {
         /* Force RA to be positive */
         xout[0] = 360. + xout[0];
      }
   }
   *out1 = xout[0];
   *out2 = yout[0];
}
FCALLSCSUB5(wcscnvd,WCSCNVD,wcscnvd,INT,DOUBLE,DOUBLE,PDOUBLE,PDOUBLE)

void wcscnvr(int wcscnvidx, float in1, float in2, 
                            float *out1, float *out2) {
/*
 *   Convert coordinates using frameset as mapping (type real)
 *
 *  wcscnvidx I Index from wcscnvbeg (indicates Mapping)
 *  in1/2     I Input coordinates
 *  out1/2    O Output coordinates
 */
   double din1, din2, dout1, dout2;

   din1 = in1;
   din2 = in2;
   wcscnvd(wcscnvidx, din1, din2, &dout1, &dout2);
   *out1 = dout1;
   *out2 = dout2;
}
FCALLSCSUB5(wcscnvr,WCSCNVR,wcscnvr,INT,FLOAT,FLOAT,PFLOAT,PFLOAT)

void wcscnvend(int wcscnvidx) {
/*
 *  Cleanup after conversion
 *
 *  wcscnvidx I Index used internally from wcscnvbeg (indicates Mapping)
 */
   AstMapping *wcsinfo;

   if ( wcscnvidx < 0 ) {
       cxwrite("wcscnvend: invalid wcscnvidx", 5);
       return;
   }
   wcsinfo = (AstMapping *) cnvlist[wcscnvidx].mapping;
   astAnnul(wcsinfo);
   cnvlist[wcscnvidx].mapping = NULL;
   cnvlist[wcscnvidx].inissky = 0;
   cnvlist[wcscnvidx].outissky = 0;
}
FCALLSCSUB1(wcscnvend,WCSCNVEND,wcscnvend,INT)

void wcsimgpix(char *wcsid, double *ximg, double *yimg, double *xpix,
               double *ypix, int forward, int *status) {
/*
 *   Wrapper routine converts between image and detector coordinates
 *     More compact call.  If many conversions are to be made, it
 *     is faster to call wcscnvbeg, wcscnvr/d many times, then wcscnvend
 *
 *  wcsid     I  WCS id associated with existing WCS frameset
 *  x/yimg   I/O Image (GRID) coordinates
 *  x/ypix   I/O Detector (PIXEL) coordinates
 *  forward   I  Direction of conversion (1=img->pix, 0=pix->img)
 *  status    O  Error flag (0 = OK)
 */

   int cnvidx;

   *status = 0;
   if ( forward ) {
      wcscnvbeg(wcsid, "GRID", "PIXEL", &cnvidx, status);
   } else {
      wcscnvbeg(wcsid, "PIXEL", "GRID", &cnvidx, status);
   }
   if ( *status != 0 ) return;
   if ( forward ) {
      wcscnvd(cnvidx, *ximg, *yimg, xpix, ypix);
   } else {
      wcscnvd(cnvidx, *xpix, *ypix, ximg, yimg);
   }
   wcscnvend(cnvidx);
}
FCALLSCSUB7(wcsimgpix,WCSIMGPIX,wcsimgpix,STRING,PDOUBLE,PDOUBLE,PDOUBLE,PDOUBLE,INT,PINT)

void wcsimgsky(char *wcsid, double *ximg, double *yimg, double *xsky,
               double *ysky, int equinox, int forward, int *status) {
/*
 *   Wrapper routine converts between image and sky coordinates
 *     More compact call.  If many conversions are to be made, it
 *     is faster to call wcscnvbeg, wcscnvr/d many times, then wcscnvend
 *
 *  wcsid     I  WCS id associated with existing WCS frameset
 *  x/yimg   I/O Image (GRID) coordinates
 *  x/ysky   I/O Celestial (SKY) coordinates
 *  equinox   I  Equinox associated with i/o sky coords
 *  forward   I  Direction of conversion (1=img->sky, 0=sky->img)
 *  status    O  Error flag (0 = OK)
 */

   int cnvidx;

   *status = 0;
   if ( forward ) {
      wcscnvbeg(wcsid, "GRID", "CUR", &cnvidx, status);
   } else {
      wcscnvbeg(wcsid, "CUR", "GRID", &cnvidx, status);
   }
   if ( *status != 0 ) return;
   if ( forward ) {
      wcscnveqx(cnvidx, 0, equinox, status);
   } else {
      wcscnveqx(cnvidx, equinox, 0, status);
   }
   if ( *status != 0 ) return;
   if ( forward ) {
      wcscnvd(cnvidx, *ximg, *yimg, xsky, ysky);
   } else {
      wcscnvd(cnvidx, *xsky, *ysky, ximg, yimg);
   }
   wcscnvend(cnvidx);
}
FCALLSCSUB8(wcsimgsky,WCSIMGSKY,wcsimgsky,STRING,PDOUBLE,PDOUBLE,PDOUBLE,PDOUBLE,INT,INT,PINT)

void wcsskysky(char *fromsys, double fromeqx, double fromx, double fromy, 
               char *tosys, double toeqx, double *tox, double *toy,
               int *status) {
/*
 *   Wrapper routine converts between different sky coordinates
 *     Doesn't depend on a wcsid
 *
 *  fromsys   I  Input coordinate system (e.g. FK5, GALACTIC)
 *  fromeqx   I  Input equinox
 *  fromx/y   I  Input coordinates (degrees)
 *  tosys     I  Output coordinate system (e.g. FK5, GALACTIC)
 *  toeqx     I  Output equinox
 *  tox/y     O  Output coordinates (degrees)
 *  status    O  Error flag (0 = OK)
 */
   char opt[100];
   double xin[1], yin[1], xout[1], yout[1];
   double pt[2];
   int match, exact;

   AstSkyFrame *inframe, *outframe;
   AstFrameSet *cnvframes;

   *status = 0;


   astBegin;
   fits_compare_str("Cartesian", fromsys, 0, &match, &exact);
   if ( match ) {
      strcpy(opt, "System = Unknown");
   } else {
      sprintf(opt, "System = %s, Equinox = %f", fromsys, fromeqx);
   }
   inframe = astSkyFrame(opt);
   fits_compare_str("Cartesian", tosys, 0, &match, &exact);
   if ( match ) {
      strcpy(opt, "System = Unknown");
   } else {
      sprintf(opt, "System = %s, Equinox = %f", tosys, toeqx);
   }
   outframe = astSkyFrame(opt);
   cnvframes = astConvert(inframe, outframe, "");
   if ( !cnvframes ) {  /* Conversion cannot be completed */
      *status = -1;
      astEnd;
      *tox = AST__BAD;
      *toy = AST__BAD;
      return;
   }
   xin[0] = fromx;
   yin[0] = fromy;
   if ( !isastbad(xin[0]) ) xin[0] = xin[0]*D2R;
   if ( !isastbad(yin[0]) ) yin[0] = yin[0]*D2R;
   astTran2(cnvframes, 1, xin, yin, 1, xout, yout);
   pt[0] = xout[0];
   pt[1] = yout[0];
   astNorm(outframe, pt);
   if ( !isastbad(pt[0]) ) pt[0] = pt[0]*R2D;
   if ( !isastbad(pt[1]) ) pt[1] = pt[1]*R2D;
   *tox = pt[0];
   *toy = pt[1];
   astEnd;
}
FCALLSCSUB9(wcsskysky,WCSSKYSKY,wcsskysky,STRING,DOUBLE,DOUBLE,DOUBLE,STRING,DOUBLE,PDOUBLE,PDOUBLE,PINT)

void wcsprec(double fromeqx, double fromx, double fromy, 
             double toeqx, double *tox, double *toy, int *status) {
/*
 *   Wrapper routine precesses RA/Dec coordinates between equinoxes
 *   Doesn't depend on a wcsid
 *
 *  fromeqx   I  Input equinox
 *  fromx/y   I  Input coordinates (degrees)
 *  toeqx     I  Output equinox
 *  tox/y     O  Output coordinates (degrees)
 *  status    O  Error flag (0 = OK)
 */
   int ifromeqx, itoeqx;
   char *fromsys, *tosys;

   static char* sys1950 = "FK4";
   static char* sys2000 = "FK5";

   *status = 0;

   ifromeqx = (int) fromeqx;
   itoeqx   = (int) toeqx;
   
   /* Copy values if no precession necessary */
   if ( ifromeqx == itoeqx ) {
      *tox = fromx;
      *toy = fromy;
      return;
   }

   /* Determine RA/Dec system based on equinox */
   if ( ifromeqx == 1950 ) {
      fromsys = sys1950;
   } else {
      fromsys = sys2000;
   }
   if ( itoeqx == 1950 ) {
      tosys = sys1950;
   } else {
      tosys = sys2000;
   }

   wcsskysky(fromsys, fromeqx, fromx, fromy, tosys, toeqx, tox, toy,
             status);

   return;
}
FCALLSCSUB7(wcsprec,WCSPREC,wcsprec,DOUBLE,DOUBLE,DOUBLE,DOUBLE,PDOUBLE,PDOUBLE,PINT)

void wcsdist(char *wcsid, char *frameid ,double xval1, double yval1, 
             double xval2, double yval2, double *dist, int *status) {
/*
 *   Calculate distance between two points
 *    If a sky frame is selected, input values assumed as degrees
 *
 *  wcsid     I  WCS id associated with existing WCS frameset
 *  frameid   I  Frame id string (see getframeidx)
 *  x/ysky1   I  Input coordinates for point 1 (degrees)
 *  x/ysky2   I  Input coordinates for point 2 (degrees)
 *  dist      O  Output distance (degrees)
 *  status    O  Error flag (0 = OK)
 */
   double pt1[2], pt2[2];
   int frameidx;
   char *msg;
   AstFrameSet *wcsinfo;
   AstFrame *frame;

   *status = 0;


   /*
    *  Get frameset associated with wcsid
    */
   wcsinfo = getwcs(wcsid);
   if ( wcsinfo == NULL ) {
      *status = -1;
      return;
   }

   /*
    *  Set Base attributes to select input frame
    */
   frameidx = getframeidx(wcsinfo, frameid);
   if ( frameidx == 0 ) {
      msg = strcatalloc("ERROR: Frame not found: ", frameid);
      cxwrite(msg, 5);
      free(msg);
      *status = -1;
      return;
   }

   frame = astGetFrame(wcsinfo, frameidx);

   pt1[0] = AST__BAD;
   pt1[1] = AST__BAD;
   pt2[0] = AST__BAD;
   pt2[1] = AST__BAD;

   astBegin;
   if ( astIsASkyFrame(frame) ) {
      if ( !isastbad(xval1) ) pt1[0] = xval1*D2R;
      if ( !isastbad(yval1) ) pt1[1] = yval1*D2R;
      if ( !isastbad(xval2) ) pt2[0] = xval2*D2R;
      if ( !isastbad(yval2) ) pt2[1] = yval2*D2R;
   } else {
      pt1[0] = xval1;
      pt1[1] = yval1;
      pt2[0] = xval2;
      pt2[1] = yval2;
   }

   astEnd;
   *dist = astDistance(frame, pt1, pt2);
   if ( astIsASkyFrame(frame) && !isastbad(*dist) ) *dist = *dist*R2D;

}
FCALLSCSUB8(wcsdist,WCSDIST,wcsdist,STRING,STRING,DOUBLE,DOUBLE,DOUBLE,DOUBLE,PDOUBLE,PINT)

void wcsconxbeg(char *fromid, char *toid, int *status) {
/*
 *   Setup contour-drawing routine with transform
 *
 *  fromid    I  Transform from wcsid
 *  toid      I  Transform to wcsid
 *  status    O  Error flag (0 = OK)
 */
   AstFrameSet *fromwcs, *towcs, *fromcpy, *tocpy;

   *status = 0;

   /*
    *  Get frameset associated with wcsids
    */
   fromwcs = getwcs(fromid);
   towcs = getwcs(toid);
   if ( fromwcs == NULL || towcs == NULL ) {
      *status = -1;
      return;
   }
   astBegin;
   fromcpy = astCopy(fromwcs);
   tocpy = astCopy(towcs);
   setupframeset(fromcpy, "GRID", "GRID");
   setupframeset(tocpy, "GRID", "PIXEL");
   conxcnv = astConvert(fromcpy, tocpy, "SKY,PIXEL,GRID");
   if ( conxcnv ) {
      conxcnv = astSimplify(conxcnv);
      astExport(conxcnv);
   }
   
   astEnd;
   if ( conxcnv == AST__NULL ) {
      cxwrite(" Failed to match coordinates of underlying image", 10);
      *status = -1;
      return;
   }

   CONXBEG();  /* Initialize contour speedup buffer */
}
FCALLSCSUB3(wcsconxbeg,WCSCONXBEG,wcsconxbeg,STRING,STRING,PINT)

void wcsconxbuf(int npts, float *x, float *y, int *k) {
/*
 *   Contour-drawing routine (executes array of draw commands)
 *
 *  x,y     I  Position in original image coords
 *  xx,yy   O  Position in new image coords
 */
   double *xin, *yin, *xout, *yout;
   float xpix, ypix;
   int i;

   if ( npts <= 0 ) {
      cxwarn(" No contours to draw", 5);
      return;
   }

   xin = (double *) malloc(npts*sizeof(double));
   yin = (double *) malloc(npts*sizeof(double));
   xout = (double *) malloc(npts*sizeof(double));
   yout = (double *) malloc(npts*sizeof(double));
   if ( !xin || !yin || !xout || !yout ) {
      cxwrite(" Allocation failure in wcsconxbuf", 5);
      return;
   }

   for ( i = 0; i < npts; i++ ) {
      xin[i] = x[i];
      yin[i] = y[i];
   }
   astTran2(conxcnv, npts, xin, yin, 1, xout, yout);

   for ( i = 0; i < npts; i++ ) {
      xpix = xout[i];
      ypix = yout[i];
      if ( k[i] == 1 ) {
         cpgdraw(xpix, ypix);
      } else {
         cpgmove(xpix, ypix);
      }
   }
   free(xin);
   free(yin);
   free(xout);
   free(yout);
}
FCALLSCSUB4(wcsconxbuf,WCSCONXBUF,wcsconxbuf,INT,FLOATV,FLOATV,INTV)

void wcsconxend(int *status) {
/*
 *   Cleanup after contour-drawing
 *
 *  status     O  Error flag (0=OK)
 */
   *status = 0;
   CONXEND();
   astAnnul(conxcnv);
   conxcnv = AST__NULL;
}
FCALLSCSUB1(wcsconxend,WCSCONXEND,wcsconxend,PINT)

int getframeidx(AstFrameSet *wcsinfo, char *frameid) {
/*
 *  Get frame index based on frameid string
 *  Returns 0 if not found
 *
 *   frameid is integer=> Returns that int, unless out of bounds
 *   frameid is 1 char => Returns the frame matching that Ident
 *   frameid  = "CUR"  => Returns index of Current frame
 *   frameid  = "BASE" => Returns index of Base frame
 *   frameid  = "IMG"  => Returns first frame with 'GRID' domain
 *   frameid  = "DET"  => Returns first frame with 'PIXEL' domain
 *   otherwise, searches for a matching Domain 
 */
   char const *value;
   int nframe, i;
   AstFrame *frame;
   char *cptr, *domain;
   char idbuf[21];

   static char* gridstr = "GRID";
   static char* pixelstr = "PIXEL";

   /*
    *  Number translated directly, unless out of bounds
    */
   nframe = astGetI(wcsinfo, "Nframe");
   if ( isdigit(*frameid) ) {
      i = atoi(frameid);
      if ( i < 0 || i > nframe ) return 0;
      return i;
   }

   /*
    *  Translate to uppercase for comparisons
    */
   idbuf[20] = '\0';
   strncpy(idbuf, frameid, 20);
   cptr = idbuf;
   while ( *cptr ) {
      *cptr = toupper(*cptr);
      cptr++;
   }
   /*
    *  One character implies Ident
    */
   if ( strlen(idbuf) == 1 ) {
      for ( i = 1; i <= nframe; i++ ) {
         frame = astGetFrame(wcsinfo, i);
         value = astGetC(frame, "Ident");
         if ( strcmp(value, idbuf) == 0 ) return i;
      }
   }

   /*
    *  Analogous to AST__BASE and AST__CURRENT, but user-enterable
    */
   if ( strcmp(idbuf, "CUR") == 0 ) return astGetI(wcsinfo, "Current");
   if ( strcmp(idbuf, "BASE") == 0 ) return astGetI(wcsinfo, "Base");

   /*
    *  Look for domain
    */
   domain = idbuf;
   if ( strcmp(idbuf, "IMG") == 0 ) domain = gridstr;
   if ( strcmp(idbuf, "DET") == 0 ) domain = pixelstr;
   for ( i = 1; i <= nframe; i++ ) {
      frame = astGetFrame(wcsinfo, i);
      value = astGetC(frame, "Domain");
      if ( strcmp(value, domain) == 0 ) return i;
   }
   return 0;
}

const char *getframeunit(AstFrame *frame) {
/*
 *  Return unit of frame given by index
 *  Returns NULL if not found
 */
   const char *unit;

   unit = astGetC(frame, "Unit(1)");
   if ( strncmp(unit, "dd:", 3) == 0 ||
        strncmp(unit, "hh:", 3) == 0 || 
        strncmp(unit, "ddd:", 4) == 0 || 
        strncmp(unit, "deg", 3) == 0 ) {
      return UNITDEG;
   }
   if ( strcmp(unit, "") == 0 ) return UNITPIX;
   return unit;
}

int setupframeset(AstFrameSet *wcsinfo, char *baseframeid, char *curframeid) {
/*
 *  Set frameset Current/Base frames based on input frameid strings
 *  (see getframeidx for frameid conventions)
 *  Returns 0 on failure, Current index on success
 */
   int ibase, icurr;

   ibase = getframeidx(wcsinfo, baseframeid);
   if ( !ibase ) return 0;
   icurr = getframeidx(wcsinfo, curframeid);
   if ( !icurr ) return 0;
   astSetI(wcsinfo, "Base", ibase);
   astSetI(wcsinfo, "Current", icurr);
   return icurr;
}

/*
 *  WCSID slot management routines
 */

int getemptywcsidx() {
/*
 *  Find next empty wcs slot
 */
   int i;
   i = 0;
   while ( i < MAXWCS ) {
      if ( wcslist[i] == NULL ) return i;
      i++;
   }
   cxwrite(" ERROR: Failed to find unoccupied wcs slot", 5);
   return -1;
}

int getwcsidx(char *wcsid) {
/*
 *  Get index associated with wcsid
 */
   int i;
   char wcsbuf[MAX_IDSTR];
   char *cptr;

   i = 0;
   while ( i < MAXWCS ) {
      strcpy(wcsbuf, wcsid);
      cptr = wcsbuf;
      while ( *cptr ) {
	 *cptr = toupper(*cptr);
	 cptr++;
      }
      if ( strcmp(wcsbuf,wcsname[i]) == 0 ) return i;
      i++;
   }
   cxwrite(" ERROR: Failed to find wcsid", 5);
   return -1;
}

AstFrameSet *getwcs(char *wcsid) {
/*
 *  Get AstFrameSet for a particular wcsid
 */
   int i;
   i = getwcsidx(wcsid);
   if ( i < 0 ) return NULL;
   return wcslist[i];
}

AstFrameSet *getmapwcs(char *id) {
/*
 *  Get AstFrameSet for a particular id
 *  If 'wcs#' given assume wcsid, otherwise assume mapid,
 *  retrieve wcsid from mapid and use that to get wcs data structure
 */
   char idbuf[MAX_IDSTR];
   int i, status;
   
   strcpy(idbuf, id);
   for ( i = 0; i < strlen(idbuf); i++ ) {
      idbuf[i] = toupper(idbuf[i]);
   }
   if ( strncmp(idbuf, "WCS", 3) == 0 ) return getwcs(id);

   status = 0;
   GHEADS(id, "WCSID", idbuf, 0, status);
   if ( status != 0 || strcmp(idbuf, "") == 0 ) return NULL;
   return getwcs(idbuf);
}

/*
 *  WCSCNV slot management routines
 */

int getemptycnvidx() {
/*
 *  Find next empty cnv slot
 */
   int i;
   i = 0;
   while ( i < MAXCNV ) {
      if ( cnvlist[i].mapping == NULL ) return i;
      i++;
   }
   cxwrite(" ERROR: Failed to find unoccupied conversion slot", 5);
   return -1;
}

/*
 *  Helper routines used in the creation of wcsid framesets 
 */

#define NUMCKEYS 6

AstFrameSet *fillwcs (char *header) {
/*
 * Takes string containing FITS header and uses it to create AstFrameSet
 * header contains a FITS header card every 80 characters
 */
   AstFrameSet *wcsret;
   AstSkyFrame* skyframe;
   AstFitsChan *fitschan, *copychan;
   char *cptr;
   char keystr[FLEN_KEYWORD];
   char card[FLEN_CARD];
   char valstr[FLEN_VALUE];
   char comment[FLEN_CARD];
   int i, ichat, ndim, ncard, keylen, isky, lataxis, status;

   int permute[ 2 ] = { 2, 1 };
   
   static char * keypatt[NUMCKEYS] = 
      { "CTYPE%d", "CRVAL%d", "CDELT%d", "CRPIX%d", "CROTA%d", "CD%d_%d" };

   status = 0;
   wcsret = (AstFrameSet *) NULL;

   if ( header == NULL ) return NULL;
   fitschan = astFitsChan(NULL, NULL, "");
   cptr = header;

   ichat = getastchat();
   setastchat(15);  /* Problems with individual cards printed at high chat */

   while ( *cptr ) {
      strncpy(card, cptr, 80);
      card[80] = '\0';
      astPutFits(fitschan, card, 0);
      if ( !astOK ) astClearStatus;
      cptr += 80;
   }
   astClear(fitschan, "Card");
   setastchat(ichat); /* Restore original chat level for ast errors */
/*
 *  Clear keywords associated with dimension > 2
 */
   for ( i = 0; i < NUMCKEYS; i++ ) {
      while ( astFindFits(fitschan, keypatt[i], card, 1) ) {
         fits_get_keyname(card, keystr, &keylen, &status);
         if ( status == 0 ) {
            cptr = keystr + keylen - 1;
            while ( cptr >= keystr && isdigit(*cptr) ) { 
               while ( isdigit(*cptr) ) { cptr--; }
               ndim = atoi(cptr+1);
               if ( ndim > 2 ) { 
                  ncard = astGetI(fitschan, "Card");
                  astSetI(fitschan, "Card", ncard - 1);
                  astDelFits(fitschan);
               }
               if ( *cptr == '_' ) {  
                  *cptr = '\0';
                  cptr--; 
               }
            }
         }
         status = 0;
      }
      astClear(fitschan, "Card");
   }
   status = 0;
   while ( astFindFits(fitschan, "CDELT1%1c", card, 0) ) {
      fits_parse_value(card, valstr, comment, &status);
      sprintf(keystr, "CD1_1%c", card[6]);
      ffmkky(keystr, valstr, comment, card, &status);
      if ( status == 0 ) astPutFits(fitschan, card, 1);
      ncard = astGetI(fitschan, "Card");
      astSetI(fitschan, "Card", ncard + 1);
   }
   astClear(fitschan, "Card");
   while ( astFindFits(fitschan, "CDELT2%1c", card, 0) ) {
      fits_parse_value(card, valstr, comment, &status);
      sprintf(keystr, "CD2_2%c", card[6]);
      ffmkky(keystr, valstr, comment, card, &status);
      if ( status == 0 ) astPutFits(fitschan, card, 1);
      ncard = astGetI(fitschan, "Card");
      astSetI(fitschan, "Card", ncard + 1);
   }
   
/*
 *  Having CDELT and a CD matrix in the same header can confuse AST,
 *  remove CDELT in this case
 */
   astClear(fitschan, "Card");
   if ( astFindFits(fitschan, "CD1_1", card, 0) ) {
      astClear(fitschan, "Card");
      if ( astFindFits(fitschan, "CDELT1", card, 0) )
         astDelFits(fitschan);
      astClear(fitschan, "Card");
      if ( astFindFits(fitschan, "CDELT2", card, 0) )
         astDelFits(fitschan);
   }
/*
 *  An EQUINOX of zero can cause problems
 */
   astClear(fitschan, "Card");
   if ( astFindFits(fitschan, "EQUINOX", card, 0) ) {
      status = 0;
      fits_parse_value(card, valstr, comment, &status);
      if ( status != 0 || atoi(valstr) <= 0 ) astDelFits(fitschan);
   }
   astClear(fitschan, "Card");
   copychan = astCopy(fitschan);
   wcsret = astRead(copychan);
   astAnnul(copychan);
   if ( wcsret == AST__NULL || !astOK ) {
      cxwarn("Failed default wcs read, trying more forgiving encoding", 5);
      astClearStatus;
      astSetC(fitschan, "Encoding", "FITS-WCS");
      astSetI(fitschan, "CDMatrix", 1);
      if ( !astOK ) {
          cxwarn("Attempt to use FITS-WCS, CD-matrix encoding failed", 10);
          astClearStatus;
      }
      wcsret = astRead(fitschan);
      if ( wcsret == AST__NULL || !astOK ) {
         cxwarn("Failed to read wcs into AstFrameSet", 5);
         wcsret = (AstFrameSet *) NULL;
         astClearStatus;
         astAnnul(fitschan);
         return wcsret;
      }
                  
   }
      
   /*
   ** check that the order of the axes is 1 => RA, 2 => Dec
   ** if it's not, then reverse it
   ** This does not help us when encoding in FITS-WCS or other
   ** systems, since AST makes it's own choices about order
   ** (and we don't always like those choices - too bad)
   */
   isky = getframeidx( wcsret, "SKY" );
   if ( !isky ) {
      astAnnul( fitschan );
      return wcsret; 
   }
   skyframe = astGetFrame( wcsret, isky );
   lataxis = astGetI( skyframe, "LatAxis" );
   if ( lataxis == 1 ) {

      /* permute the axes */
      cxwarn( "Found permuted axes, fixing", 15 );
      astPermAxes( wcsret, permute );
   }
   astAnnul(fitschan);

   return wcsret;
}

AstFrameSet *hdrtowcs(char *mapid) {
/*
 *  Generate AST FrameSet from internal header associated with mapid
 *  Calling routine responsible for astAnnul of pointer
 */
   static char memfile[] = "mem://internalhdr";
   char wcsid[MAX_IDSTR];
   int wcsidx, status;
   AstFrameSet *wcsptr;

   /* Read into memory file */
   status = 0;
   HDRTOMEM(mapid, memfile, status);
   if ( status != 0 ) return NULL;

   /* Use image reader to read memory file */
   genimgwcs(memfile, 0, MAX_IDSTR, wcsid, &status);
   if ( status != 0 ) return NULL;

   /* Remove from wcs management, assumed for temporary usage */
   wcsidx = getwcsidx(wcsid);
   wcsptr = NULL;
   if ( wcsidx >= 0 ) {
      wcsptr = wcslist[wcsidx];
      wcslist[wcsidx] = NULL;  /* Clear spot in pointer array */
      wcsref[wcsidx] = 0;
   }
   return wcsptr;
}

/*
 *  Helper routines for reading keyword values from FitsChan
 *  If key not found, value left untouched
 *  Used by wcstohdr
 */
void affgkyd(AstFitsChan *fitschan, char *keyname, double *value) {

   char card[FLEN_CARD];
   char valstr[FLEN_VALUE];
   char comment[FLEN_CARD];
   int status;

   status = 0;
   astClear(fitschan, "Card");
   if ( !astFindFits(fitschan, keyname, card, 1) ) return;
   if ( strcmp(card,"") == 0 ) return;
   fits_parse_value(card, valstr, comment, &status);
   *value = atof(valstr);
}

void affgkys(AstFitsChan *fitschan, char *keyname, char *value) {

   char card[FLEN_CARD];
   char valstr[FLEN_VALUE];
   char comment[FLEN_CARD];
   int status;

   status = 0;
   astClear(fitschan, "Card");
   if ( !astFindFits(fitschan, keyname, card, 1) ) return;
   if ( strcmp(card,"") == 0 ) return;
   fits_parse_value(card, valstr, comment, &status);
   if ( valstr[0] == '\'' && valstr[strlen(valstr)-1] == '\'' ) {
      /* Strip quotes */
      valstr[strlen(valstr)-1] = '\0';
      strcpy(value, valstr+1);
   } else {
      strcpy(value, valstr);
   }
}


