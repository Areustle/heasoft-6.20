
/*  This file is a makeshift "library" containing functions copied out of heagen/barycorr/bary.c and 
    heagen/barycorr/dpleph.c.  However, because barycorr was written without regard to Astro-H coding standards, 
    the functions copied herein have been heavily modified to eliminate global variables, forward declaration 
    of enums, and copious instances of bad code resulting in memory leaks. The structures BARYCORR_DATA and 
    DPLEPH_DATA, as declared in barycen.h, now contain what used to be these global variables.
*/


#include "barycen.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <fitsio.h>
#include <longnam.h>

#define NOEROWS 100             /* Number of Orbit Ephemeris rows in memory */
#define SECDAY  86400                   /* Seconds per day */
#define RADEG   57.2957795130823
#define TWOPI   6.283185307179586
#define LB      1.550519748E-8          /* TT and TDB to TCB rate */
#define LC      1.4808268457E-8         /* TCG to TCB rate */
#define LG      6.96929019E-10          /* TT to TCG rate */
#define TAItoTT 32.184                  /* TT - TAI (s) */
#define DOT(A, B)       (A[0]*B[0] + A[1]*B[1] + A[2]*B[2])
#define TIMENULLVAL (-1e300)

#define TIMINGDIR "TIMING_DIR"
#define LHEADIR   "LHEA_DATA"
#define EPHEM     "JPLEPH"
#define PSRTIME   "psrtime.dat"
#define PSRBIN    "psrbin.dat"
#define XTECC     "tdc.dat"
#define TAIUTC    "tai-utc.dat"


/* From heagen/barycorr/bary.c
 *   c200to405 converts a direction cosine vector from the DE200 frame  
 *  (nominally FK5, but not quite) to the DE405 frame (ICRS).
 *
 *  double dir[3]    direction cosine vector (in and out)
 *  int debug        Whether or not to print the conversion  */
void c200to405 (double *dir, int debug) {
    double dir200[3] ;
    double x=0.002/206265.0, y=0.012/206265.0, z=0.006/206265.0 ;
    int i=0;

    for (i=0; i<3; i++)
        dir200[i] = dir[i] ;
    dir[0] += y * dir200[2] - z * dir200[1] ;
    dir[1] += z * dir200[0] - x * dir200[2] ;
    dir[2] += x * dir200[1] - y * dir200[0] ;

    if ( debug ) {
        printf ("Conversion      DE200     to    ICRS:\n") ;
        printf ("   X         %12.9f    %12.9f\n", dir200[0], dir[0]) ;
        printf ("   Y         %12.9f    %12.9f\n", dir200[1], dir[1]) ;
        printf ("   Z         %12.9f    %12.9f\n", dir200[2], dir[2]) ;
    }
    return ;
}


/* From heagen/barycorr/bary.c
 *  baryinit sets up the path to the ephemeris file and gets the leap seconds information.
 *
 *  obs:      Observatory (Unknown, Geocenter, XTE, AXAF)
 *  fromts:   Time system of input
 *  tots:     Time system to convert to
 *  mjdi:     MJDREFI; use mission default if =-1.0
 *  mjdf:     MJDREFF
 *  timeunit: Unit of time (s or d)
 *  ephnum:   Ephemeris number requested (200, 405; 0: default), if < 0: do not initialize ephemeris
 *  bcorr:    Data structure containing former bary.c global variables
 *  dpl:      Data structure containing former dpleph.c global variables
 *  return:   denum; 0 upon error  */
int baryinit (enum Observatory obs, char *fromts, char *tots, double mjdi,
              double mjdf, char *timeunit, int ephnum, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl) {

    char from[5]="00000", to[5]="00000" ;
    int i=0 ;
    FILE *FF ;
    char mon[16], dum1[16], dum2[16] ;

    mission = obs ;

    /*Sort out time systems ----------------------------------------------- */
    strncpy (from, fromts, 3) ;
    strncpy (to, tots, 3) ;
    for (i=0; i<3; i++) {
        if ( ( from[i] > 96 ) && ( from[i] < 123 ) )
            from[i] -= 32 ;
        if ( ( to[i] > 96 ) && ( to[i] < 123 ) )
            to[i] -= 32 ;
    }

    if ( !( strcmp(from, "TT") ) )
        fromTS = TT ;
    else if ( !( strcmp(from, "TDT") ) )
        fromTS = TT ;
    else if ( !( strcmp(from, "ET") ) )
        fromTS = TT ;
    else if ( !( strcmp(from, "TDB") ) )
        fromTS = TDB ;
    else if ( !( strcmp(from, "TB") ) )
        fromTS = TDB ;
    else if ( !( strcmp(from, "TCB") ) )
        fromTS = TCB ;
    else if ( !( strcmp(from, "TAI") ) )
        fromTS = TAI ;
    else if ( !( strcmp(from, "IAT") ) )
        fromTS = TAI ;
    else
        fromTS = UTC ;

    if ( !( strcmp(to, "TT") ) )
        toTS = TT ;
    else if ( !( strcmp(to, "TDT") ) )
        toTS = TT ;
    else if ( !( strcmp(to, "ET") ) )
        toTS = TT ;
    else if ( !( strcmp(to, "TDB") ) )
        toTS = TDB ;
    else if ( !( strcmp(to, "TB") ) )
        toTS = TDB ;
    else if ( !( strcmp(to, "TCB") ) )
        toTS = TCB ;
    else if ( !( strcmp(to, "TAI") ) )
        toTS = TAI ;
    else if ( !( strcmp(to, "IAT") ) )
        toTS = TAI ;
    else
        toTS = UTC ;


    /* Set scale factors ---------------------------------------------------  */
    bcorr->day2met = ( *timeunit == 'd' ) ? 1.0 : SECDAY ;
    bcorr->met2day = 1.0 / bcorr->day2met ;
    bcorr->met2sec = SECDAY / bcorr->day2met ;

    /* Get leap seconds ----------------------------------------------------          */
    /*  19May2005 This block rewritten by Ingo.Kreykenbohm@obs.unige.ch (ISDC)        */
    /*            to eliminate repeated reading of leapsecs file which could lead     */
    /*            to buffer overruns                                                  */

    if (bcorr->numleaps == 0) {
        char thisfile[NEW_FLEN_VALUE];
        sprintf(thisfile,"%s/refdata/%s",getenv("HEADAS"),TAIUTC);
        int nn = get_nlines(thisfile);
        bcorr->leapsMJD = calloc(nn, sizeof(long));
        bcorr->leapsecs = calloc(nn, sizeof(double));
        bcorr->MJDoffset = calloc(nn, sizeof(double));
        bcorr->leapcoeff = calloc(nn, sizeof(double));

        FF = openAFile (TAIUTC) ;
        while ( fscanf (FF, "%d %s  1 =JD 24%ld.5 %s %lg S + (MJD - %lg) X %lg %s",
                        &i, mon, bcorr->leapsMJD+bcorr->numleaps, dum1, bcorr->leapsecs+bcorr->numleaps,
                        bcorr->MJDoffset+bcorr->numleaps, bcorr->leapcoeff+bcorr->numleaps, dum2) == 8 )
            {
                bcorr->numleaps++ ;
                if (bcorr->numleaps == MAX_LEAPS) {
                    fprintf(stderr, "Baryinit: cannot handle more than %i leapseconds. Please adapt MAX_LEAP in bary.h and recompile.",MAX_LEAPS);
                    return 0 ;
                }
            }
        fclose (FF) ;
    }

    /* Set the MJD Reference ----------------------------------------------- */
    if ( ( obs == AXAF ) && ( mjdi == -1.0) ) {
        bcorr->mjdRefi = 50814 ;
        bcorr->mjdReff = 0.0 ;
    }
    /* MJT: for completeness and just in case... */
    else if ( ( obs == SWIFT) && ( mjdi == -1.0) ) {
        bcorr->mjdRefi = 51910 ;
        bcorr->mjdReff = 0.0007428703704;
    }
    else if ( ( obs != XTE ) || ( mjdi != -1.0) ) {
        bcorr->mjdRefi = mjdi + mjdf ;
        bcorr->mjdReff = mjdi - bcorr->mjdRefi ;
        bcorr->mjdReff += mjdf ;
    }

    bcorr->mjdRef.MJDint = bcorr->mjdRefi ;
    bcorr->mjdRef.MJDfr = bcorr->mjdReff ;
    bcorr->mjdRef.ts = fromTS ;

    /* If fromTS is ET or TAI, or (UTC and TIMEUNIT='s'), change it to TT and do the conversion in MJDRef. */
    switch ( fromTS ) {
    case UTC:
        if  ( bcorr->day2met < 10.0 )
            break ;
    case ET:
    case TAI:
        fromTS = TT ;
        toTT (&bcorr->mjdRef,bcorr) ;
        break ;
    default:
        break ;
    }


    /* Initialize clock and orbit ------------------------------------------  */
    mission = obs;

    /* Initialize ephemeris ------------------------------------------------ */
    if ( ephnum >= 0 ) {
        if ( (i = initephem (ephnum, bcorr, dpl)) ) {
            fprintf (stderr, "Error while initializing ephemeris; status: %d\n",i) ;
            bcorr->denum = 0 ;
        }
    }
    else
        bcorr->denum = 1 ;

    return bcorr->denum ;
}





/* From heagen/barycorr/bary.c
 *  openAFile finds one of the ASCII system files and opens it for reading.
 *  name:     Name of the file
 *  openAFile first tries to find the file in TIMINGDIR, then in LHEADIR.
 *  return:   file pointer, NULL if not found   */
FILE *openAFile (const char *name){
    char fileName[1024] ;
    char *filepath = NULL ;
    FILE *FF = NULL ;

    if ( (filepath = getenv (TIMINGDIR)) ) {
        sprintf (fileName, "%s/%s", filepath, name) ;
        FF = fopen (fileName, "r") ;
    }

    if ( FF == NULL )
        if ( (filepath = getenv (LHEADIR)) ) {
            sprintf (fileName, "%s/%s", filepath, name) ;
            FF = fopen (fileName, "r") ;
        }

    if ( filepath == NULL )
        fprintf (stderr, "Neither $%s, nor $%s is defined\n", TIMINGDIR, LHEADIR) ;
    else if ( FF == NULL )
        fprintf (stderr, "File %s could not be found in %s\n", fileName, filepath) ;

    return FF ;
}





/* From heagen/barycorr/bary.c 
 *  toTT converts mjd to TT
 *  double toTT (MJDTime *mjd)
 *  mjd:     MJD (?) day; converted
 *  bcorr:   Data structure containing former bary.c global variables
 *  return:  Change in seconds */
double toTT (MJDTime *mjd, BARYCORR_DATA * bcorr) {

    double total = 0.0 ;
    int ii=0;
    double dt=0;


    switch ( mjd->ts ) {
    case TT:
        break ;
        /*  from TDB and from TCB are not (yet) implemented  */
    case TDB:
    case TCB:
        break ;
    case UTC:

        while ( mjd->MJDfr >= 1.0 ) {
            mjd->MJDint++ ;
            mjd->MJDfr-- ;
        }
        while ( mjd->MJDfr < 0.0 ) {
            mjd->MJDint-- ;
            mjd->MJDfr++ ;
        }

        ii = bcorr->numleaps - 1 ;
        while ( ( mjd->MJDint < bcorr->leapsMJD[ii] ) && ii )
            ii-- ;
        if ( ii > 12 )
            dt = bcorr->leapsecs[ii] ;
        else
            dt = bcorr->leapsecs[ii] + (mjd->MJDint - bcorr->MJDoffset[ii] + mjd->MJDfr) * bcorr->leapcoeff[ii] ;

        total += dt;

    case TAI:
        total += TAItoTT ;
        mjd->MJDfr += total / SECDAY ;
        mjd->ts = TT ;
        break ;
    default:
        break ;
    }
    return total ;
}







/* From heagen/barycorr/bary.c 
 *  openFFile finds one of the FITS system files and opens it for reading.
 *  name:     Name of the file openFFile first tries to find the file in TIMINGDIR, then in LHEADIR.
 *  return:   FITS file pointer, NULL if not found */                                                                           
fitsfile *openFFile (const char *name){
    char fileName[1024] ;
    char *filepath = NULL  ;
    fitsfile *FF = NULL ;
    int error = 0 ;

    if ( (filepath = getenv (TIMINGDIR)) ) {
        sprintf (fileName, "%s/%s", filepath, name) ;
        fits_open_file (&FF, fileName, READONLY, &error) ;
    }

    if ( FF == NULL )
        if ( (filepath = getenv (LHEADIR)) ) {
            error = 0 ;
            sprintf (fileName, "%s/%s", filepath, name) ;
            fits_open_file (&FF, fileName, READONLY, &error) ;
        }

    if ( filepath == NULL )
        fprintf (stderr, "Neither $%s, nor $%s is defined\n", TIMINGDIR, LHEADIR) ;
    else if ( FF == NULL )
        fprintf (stderr, "File %s could not be found in %s\n", fileName, filepath) ;

    return FF ;
}




/* From heagen/barycorr/bary.c
 *   mjdXT:     Time of photon/pulse arrival in MJD(??)
 *   sourcedir: Direction cosines of source position
 *   scposn:    (Spacecraft) position vector in meters wrt geocenter Note: it usually does not 
 *                 matter if provided in FK5 or DE200 frame when using DE405
 *   bcorr:     Data structure containing former bary.c global variables
 *   dpl:       Data structure containing former dpleph.c global variables   
 *
 *  Method:
 *   Compute index of ephemeris memory corresponding to input date
 *   If (input date is not in memory) then
 *    Load next 32 days of ephemeris data into memory
 *   Calculate position of Sun, Earth, and velocity of Earth
 *   Calculate SSBC-to-S/C vector and Sun-to-S/C vector
 *   Calculate distance and angular size of sun
 *  [ If (object was blocked by sun)             ]  commented out
 *  [   Return (no delay calculated)             ]
 *   Add all propagation effects
 *   Return calculated delay (in s)
 *  All distances are in light seconds, velocities in ls/s. */ 
double barycorr (MJDTime *mjdXT, double *sourcedir, double *scposn, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl,
      PRINT_DATA * pdata){
    const double *rce, *rcs, *vce;
    double rca[3], rsa[3];
    const double *eposn ;
    double total=0.0, sundis=0.0, cth=0.0, t2b=0.0 ;
    MJDTime mjdXX ;
    MJDTime *mjdTT ;
    int i=0;
    int iearth = 3 ;
    int isun = 11 ;
    double jdt[2] ;
    MJDTime mjdTDB ;
    static double tdbtdt = 0.0;
    static double tdbtdtdot = 0.0;
    static long oldmjd = 0 ;
    long l =0;

    /* Sort out time systems -----------------------------------------------  */
    total = 0.0 ;
    switch ( mjdXT->ts ) {
    case TT:
        mjdTT = mjdXT ;
        break ;
    case TDB:
    case TCB:
        return 0.0 ;
    case UTC:
    case TAI:
    default:
        mjdTT = &mjdXX ;
        mjdTT->MJDint = mjdXT->MJDint ;
        mjdTT->MJDfr = mjdXT->MJDfr ;
        mjdTT->ts = mjdXT->ts ;
        total += toTT (mjdTT, bcorr) ;
        break ;
    }

    /* from function TTtoTDB */
    while ( mjdTT->MJDfr >= 1.0 ) {
        mjdTT->MJDint++ ;
        mjdTT->MJDfr-- ;
    }
    while ( mjdTT->MJDfr < 0.0 ) {
        mjdTT->MJDint-- ;
        mjdTT->MJDfr++ ;
    }
    
    if ( mjdTT->MJDint != oldmjd ) {
        oldmjd = mjdTT->MJDint ;
        l = oldmjd + 2400001 ;
        
        tdbtdt = ctatv (l, 0.0) ;
        tdbtdtdot = ctatv (l, 0.5) - ctatv (l, -0.5) ;
    }
    
    t2b = ( tdbtdt + (mjdTT->MJDfr - 0.5) * tdbtdtdot ) ;
    /* end from function TTtoTDB */


    /* Check observatory position ------------------------------------------ */
    if ( scposn == NULL ) {
        fprintf (stderr, "barycorr: Invalid Observatory/Spacecraft position vector\n") ;
        return 0.0 ;
    }

    /* Get all positions ---------------------------------------------------  */
    jdt[0] = mjdTT->MJDint + 2400001 ;
    jdt[1] = mjdTT->MJDfr - 0.5 + t2b/SECDAY ;
    if ( (eposn = dpleph (jdt, iearth, isun, bcorr, dpl)) == NULL ) {
        fprintf (stderr, "barycorr: Could not find solar system ephemeris for MJD %ld\n",mjdTT->MJDint) ;
        return 0.0 ;
    }
    rce = eposn ;
    vce = eposn + 3 ;
    rcs = eposn + 6 ;
    for (i = 0; i < 3; i++) {
        rca[i] = rce[i] + scposn[i]/bcorr->c;         /* SSBC-to-S/C vector */
        rsa[i] = rca[i] - rcs[i];              /* Sun-to-S/C vector */
    }

    pdata->vearthx = vce[0]*bcorr->c/1000.0;
    pdata->vearthy = vce[1]*bcorr->c/1000.0;
    pdata->vearthz = vce[2]*bcorr->c/1000.0;
    pdata->vearth = sqrt(pdata->vearthx*pdata->vearthx 
      + pdata->vearthy*pdata->vearthy + pdata->vearthz*pdata->vearthz);
    pdata->eradvel = pdata->vearthx*sourcedir[0] + pdata->vearthy*sourcedir[1]
      + pdata->vearthz*sourcedir[2];

    /* Calculate the time delay due to the gravitational field of the Sun -- */
    /*  (I.I. Shapiro, Phys. Rev. Lett. 13, 789 (1964)). -------------------- */
    sundis = sqrt(DOT(rsa, rsa));
    cth = DOT(sourcedir, rsa)/sundis;

    /* Eclipsed by the sun */
    /*  sunsiz = radsol/sundis; */
    /*  if ((cth + 1) < 0.5*sunsiz*sunsiz) return (0); */

    /* Sum all propagation effects ----------------------------------------- */
    total += t2b
        + DOT(sourcedir, rca)
        + DOT(scposn, vce)/bcorr->c
        + 2*bcorr->msol*log(1+cth);

    pdata->total = total;
    pdata->t2b = t2b;
    pdata->scssbc = DOT(sourcedir, rca);
    pdata->scearth = DOT(scposn, vce)/bcorr->c;
    pdata->grav = 2*bcorr->msol*log(1+cth);
    
  /* Sort out time systems ----------------------------------------------- */
    switch ( toTS ) {
    case TCB:
        mjdTDB.MJDint = mjdTT->MJDint ;
        mjdTDB.MJDfr = mjdTT->MJDfr + total/SECDAY ;
        mjdTDB.ts = TDB ;
        total += LB * ( (mjdTDB.MJDint - 43144) + mjdTDB.MJDfr ) * SECDAY ;
        break ;
    default:
        break ;
    }
    
    return total ;
}



/* From heagen/barycorr/ctatv.c
 *  Computes the cumulative relativistic time correction to earth-based clocks, TDB-TDT, for a 
 *    given time. Routine furnished by the Bureau des Longitudes, modified by removal of terms much 
 *    smaller than 0.1 microsecond.
 *
 *   jdno:      long     INPUT    Julian day number of lookup
 *   fjdno:     double   INPUT    Fractional part of Julian day number
 *   tdbtdt:    double   OUTPUT   Time difference TDB-TDT (seconds)           */
double ctatv (long jdno, double fjdno){

    double t=0.0, tt=0.0, t1=0.0, t2=0.0, t3=0.0, t4=0.0, t5=0.0, t24=0.0, t25=0.0, t29=0.0, t30=0.0;

    t = ((jdno-2451545) + fjdno)/(365250.0) ;
    tt = t*t ;

    t1  =       1656.674564 * sin(  6283.075943033*t + 6.240054195)
        +        22.417471 * sin(  5753.384970095*t + 4.296977442)
        +        13.839792 * sin( 12566.151886066*t + 6.196904410)
        +         4.770086 * sin(   529.690965095*t + 0.444401603)
        +         4.676740 * sin(  6069.776754553*t + 4.021195093)
        +         2.256707 * sin(   213.299095438*t + 5.543113262)
        +         1.694205 * sin(    -3.523118349*t + 5.025132748)
        +         1.554905 * sin( 77713.772618729*t + 5.198467090)
        +         1.276839 * sin(  7860.419392439*t + 5.988822341)
        +         1.193379 * sin(  5223.693919802*t + 3.649823730)
        +         1.115322 * sin(  3930.209696220*t + 1.422745069)
        +         0.794185 * sin( 11506.769769794*t + 2.322313077)
        +         0.600309 * sin(  1577.343542448*t + 2.678271909)
        +         0.496817 * sin(  6208.294251424*t + 5.696701824)
        +         0.486306 * sin(  5884.926846583*t + 0.520007179)
        +         0.468597 * sin(  6244.942814354*t + 5.866398759)
        +         0.447061 * sin(    26.298319800*t + 3.615796498)
        +         0.435206 * sin(  -398.149003408*t + 4.349338347)
        +         0.432392 * sin(    74.781598567*t + 2.435898309)
        +         0.375510 * sin(  5507.553238667*t + 4.103476804) ;

    t2  =          0.243085 * sin(  -775.522611324*t + 3.651837925)
        +         0.230685 * sin(  5856.477659115*t + 4.773852582)
        +         0.203747 * sin( 12036.460734888*t + 4.333987818)
        +         0.173435 * sin( 18849.227549974*t + 6.153743485)
        +         0.159080 * sin( 10977.078804699*t + 1.890075226)
        +         0.143935 * sin(  -796.298006816*t + 5.957517795)
        +         0.137927 * sin( 11790.629088659*t + 1.135934669)
        +         0.119979 * sin(    38.133035638*t + 4.551585768)
        +         0.118971 * sin(  5486.777843175*t + 1.914547226)
        +         0.116120 * sin(  1059.381930189*t + 0.873504123)
        +         0.101868 * sin( -5573.142801634*t + 5.984503847)
        +         0.098358 * sin(  2544.314419883*t + 0.092793886)
        +         0.080164 * sin(   206.185548437*t + 2.095377709)
        +         0.079645 * sin(  4694.002954708*t + 2.949233637)
        +         0.075019 * sin(  2942.463423292*t + 4.980931759)
        +         0.064397 * sin(  5746.271337896*t + 1.280308748)
        +         0.063814 * sin(  5760.498431898*t + 4.167901731)
        +         0.062617 * sin(    20.775395492*t + 2.654394814)
        +         0.058844 * sin(   426.598190876*t + 4.839650148)
        +         0.054139 * sin( 17260.154654690*t + 3.411091093) ;

    t3  =          0.048373 * sin(   155.420399434*t + 2.251573730)
        +         0.048042 * sin(  2146.165416475*t + 1.495846011)
        +         0.046551 * sin(    -0.980321068*t + 0.921573539)
        +         0.042732 * sin(   632.783739313*t + 5.720622217)
        +         0.042560 * sin(161000.685737473*t + 1.270837679)
        +         0.042411 * sin(  6275.962302991*t + 2.869567043)
        +         0.040759 * sin( 12352.852604545*t + 3.981496998)
        +         0.040480 * sin( 15720.838784878*t + 2.546610123)
        +         0.040184 * sin(    -7.113547001*t + 3.565975565)
        +         0.036955 * sin(  3154.687084896*t + 5.071801441)
        +         0.036564 * sin(  5088.628839767*t + 3.324679049)
        +         0.036507 * sin(   801.820931124*t + 6.248866009)
        +         0.034867 * sin(   522.577418094*t + 5.210064075)
        +         0.033529 * sin(  9437.762934887*t + 2.404714239)
        +         0.033477 * sin(  6062.663207553*t + 4.144987272)
        +         0.032438 * sin(  6076.890301554*t + 0.749317412);

        t4  =          0.028244 * sin( -6286.598968340*t + 5.069663519)
        +         0.027567 * sin(  6279.552731642*t + 5.040846034)
        +         0.025196 * sin(  1748.016413067*t + 2.901883301)
        +         0.024816 * sin( -1194.447010225*t + 1.087136918)
        +         0.022567 * sin(  6133.512652857*t + 3.307984806)
        +         0.022509 * sin( 10447.387839604*t + 1.460726241)
        +         0.021691 * sin( 14143.495242431*t + 5.952658009)
        +         0.020937 * sin(  8429.241266467*t + 0.652303414)
        +         0.020322 * sin(   419.484643875*t + 3.735430632)
        +         0.017673 * sin(  6812.766815086*t + 3.186129845)
        +         0.017806 * sin(    73.297125859*t + 3.475975097)
        +         0.016155 * sin( 10213.285546211*t + 1.331103168)
        +         0.015974 * sin( -2352.866153772*t + 6.145309371)
        +         0.015949 * sin(  -220.412642439*t + 4.005298270)
        +         0.015078 * sin( 19651.048481098*t + 3.969480770)
        +         0.014751 * sin(  1349.867409659*t + 4.308933301)
        +         0.014318 * sin( 16730.463689596*t + 3.016058075)
        +         0.014223 * sin( 17789.845619785*t + 2.104551349)
        +         0.013671 * sin(  -536.804512095*t + 5.971672571)
        +         0.012462 * sin(   103.092774219*t + 1.737438797) ;

    t5  =          0.012420 * sin(  4690.479836359*t + 4.734090399)
        +         0.011942 * sin(  8031.092263058*t + 2.053414715)
        +         0.011847 * sin(  5643.178563677*t + 5.489005403)
        +         0.011707 * sin( -4705.732307544*t + 2.654125618)
        +         0.011622 * sin(  5120.601145584*t + 4.863931876)
        +         0.010962 * sin(     3.590428652*t + 2.196567739)
        +         0.010825 * sin(   553.569402842*t + 0.842715011)
        +         0.010396 * sin(   951.718406251*t + 5.717799605)
        +         0.010453 * sin(  5863.591206116*t + 1.913704550)
        +         0.010099 * sin(   283.859318865*t + 1.942176992)
        +         0.009858 * sin(  6309.374169791*t + 1.061816410)
        +         0.009963 * sin(   149.563197135*t + 4.870690598)
        +         0.009370 * sin(149854.400135205*t + 0.673880395) ;

    t24 = t * (  102.156724 * sin(  6283.075849991*t + 4.249032005)
                 +         1.706807 * sin( 12566.151699983*t + 4.205904248)
                 +         0.269668 * sin(   213.299095438*t + 3.400290479)
                 +         0.265919 * sin(   529.690965095*t + 5.836047367)
                 +         0.210568 * sin(    -3.523118349*t + 6.262738348)
                 +         0.077996 * sin(  5223.693919802*t + 4.670344204) ) ;

    t25 = t * (    0.059146 * sin(    26.298319800*t + 1.083044735)
                   +         0.054764 * sin(  1577.343542448*t + 4.534800170)
                   +         0.034420 * sin(  -398.149003408*t + 5.980077351)
                   +         0.033595 * sin(  5507.553238667*t + 5.980162321)
                   +         0.032088 * sin( 18849.227549974*t + 4.162913471)
                   +         0.029198 * sin(  5856.477659115*t + 0.623811863)
                   +         0.027764 * sin(   155.420399434*t + 3.745318113)
                   +         0.025190 * sin(  5746.271337896*t + 2.980330535)
                   +         0.024976 * sin(  5760.498431898*t + 2.467913690)
                   +         0.022997 * sin(  -796.298006816*t + 1.174411803)
                   +         0.021774 * sin(   206.185548437*t + 3.854787540)
                   +         0.017925 * sin(  -775.522611324*t + 1.092065955)
                   +         0.013794 * sin(   426.598190876*t + 2.699831988)
                   +         0.013276 * sin(  6062.663207553*t + 5.845801920)
                   +         0.012869 * sin(  6076.890301554*t + 5.333425680)
                   +         0.012152 * sin(  1059.381930189*t + 6.222874454)
                   +         0.011774 * sin( 12036.460734888*t + 2.292832062)
                   +         0.011081 * sin(    -7.113547001*t + 5.154724984)
                   +         0.010143 * sin(  4694.002954708*t + 4.044013795)
                   +         0.010084 * sin(   522.577418094*t + 0.749320262)
                   +         0.009357 * sin(  5486.777843175*t + 3.416081409) ) ;

    t29 = tt * (   0.370115 * sin(                     4.712388980)
                   +         4.322990 * sin(  6283.075849991*t + 2.642893748)
                   +         0.122605 * sin( 12566.151699983*t + 2.438140634)
                   +         0.019476 * sin(   213.299095438*t + 1.642186981)
                   +         0.016916 * sin(   529.690965095*t + 4.510959344)
                   +         0.013374 * sin(    -3.523118349*t + 1.502210314) ) ;

    t30 = t * tt * 0.143388 * sin( 6283.075849991*t + 1.131453581) ;

    return (t1+t2+t3+t4+t5+t24+t25+t29+t30) * 1.0e-6 ;
}



/* From heagen/barycorr/dpleph.c 
 *  dpleph returns a pointer to an array containing the concatenated state 
 *  vectors (X, Y, Z, VX, VY, VZ) of the target and the center with respect
 *  to the barycenter of the solar system at time jd[0]+jd[1] (in JD(TT)). 
 *
 *   jd:      JD time for which ephemeris is requested
 *   ntarg:   Target for which position is requested
 *   ncentK   Center for which position is requested
 *   bcorr:   Data structure containing former bary.c global variables
 *   dpl:     Data structure containing former dpleph.c global variables    
 *
 *  The numbering convention for ntarg and ncent is:
 *    1 = Mercury            8 = Neptune
 *    2 = Venus              9 = Pluto
 *    3 = Earth             10 = Moon
 *    4 = Mars              11 = Sun
 *    5 = Jupiter           12 = Solar system barycenter
 *    6 = Saturn            13 = Earth-Moon barycenter
 *    7 = Uranus            14 = Nutations (longitude and obliquity; untested)
 *                          15 = Librations
 *  This numbering scheme is 1-relative, to be consistent with the Fortran version. */
double *dpleph (double *jd, int ntarg, int ncent, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl){
  static double posn[12] ;
  long jdint=0;
  double jdtmp=0.0 ;

/* Make sure jd is in proper range */
  jdint = (long) jd[0] ;
  jdtmp = (double) jd[0] - jdint ;
  jd[0] = (double) jdint ;
  jd[1] += jdtmp ;
  while ( jd[1] >= 0.5 ) {
    jd[1]-- ;
    jd[0]++ ;
  }
  while ( jd[1] < -0.5 ) {
    jd[1]++ ;
    jd[0]-- ;
  }

/* Get the positions and return */
  if ( state (jd, ntarg, ncent, posn, bcorr, dpl) ){
      return (double *) NULL ;
  } else {
      return (double *) posn ;
  }
}



/* From heagen/barycorr/dpleph.c
 *     This function reads and interpolates the JPL ephemeris, returning position and velocity 
 *     of the bodies ntarg and ncent with respect to the solar system barycenter at JD (TT) time
 *     jdint + jdfr.          
 *
 *     Arguments:
 *       Input:
 *           jd[0]   JD (TT) - integer part
 *           jd[1]   JD (TT) - fractional part (-0.5 <= jdfr < +0.5)
 *           ntarg   Target number
 *           ncent   Center number
 *           bcorr:  Data structure containing former bary.c global variables
 *           dpl:    Data structure containing former dpleph.c global variables
 *       Output:
 *        posn[12]   Interpolated quantities requested:
 *                     posn[0:2] ntarg position  posn[3:5]  ntarg velocity
 *                     posn[6:8] ncent position  posn[9:11] ncent velocity
 *                     units are lightseconds and lightseconds/s
 *
 *                     posn[0:1] ntarg long/lat  posn[2:3]  ntarg rates
 *                     posn[0:2] ntarg Euler angs posn[3:5] ntarg rates
 *                     units are radians and radians/s (librations)       */
int state (double *jd, int ntarg, int ncent, double *posn, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl){
  double t=0.0, t1=0.0, t2=0.0;
  long recnum=0;

/* Reference to most recent midnight */
  t1 = jd[0] - 0.5 ;
  t2 = jd[1] + 0.5 ;
  t = t1 + t2 ;

/* Error return for epoch out of range */
  if ( ( t < dpl->ss1 ) || ( t > dpl->ss2 ) ) {
    fprintf (stderr, "dpleph[state]: Time %f outside range of ephemeris\n",
             t) ;
    return 1 ;
  }

/* Calculate record # and relative time in interval */
  recnum = (int) ((double) (t1 - dpl->ss1) * dpl->ss3inv) + 1 ;
  if ( t1 == dpl->ss2 )
    recnum-- ;
  t1 = ((t1 - ((double) (recnum - 1) * dpl->ss3 + dpl->ss1)) + t2) * dpl->ss3inv ;

/* Read correct record if not in memory */
  if ( recnum != dpl->currec ) {
    dpl->currec = recnum ;
    if ( readephem (recnum, bcorr, dpl) ) {
      fprintf (stderr, "dpleph[state]: Read failure in ephemeris file, record %ld\n",recnum) ;
      return 2 ;
    }
  }

/* Get state vector for target */
  if ( getstate (ntarg, t1, posn, dpl) )
    return -1 ;

/* Get state vector for center */
  if ( ncent > 0 )
      if ( getstate (ncent, t1, posn+6, dpl) )
     return -2 ;

/* Return */

  return 0 ;
}



/* From heagen/barycorr/dpleph.c
 *     This function reads and interpolates the JPL ephemeris, returning position and velocity 
 *     of body ntarg with respect to the solar system barycenter at Chebyshev time t1.
 *     It does some shuffling of indices: Adding earth vector to moon's state vector, 
 *     converting EM state vector to earth, juggling the indices for nutation and libration, and adding
 *     barycenter, as well as scaling.
 *
 *     Arguments:
 *       Input:
 *           ntarg   Target number
 *              t1   Chebyshev time
 *             dpl   Data structure containing former dpleph.c global variables
 *       Output:
 *        posn[6]   Interpolated quantities requested:
 *                     posn[0:2] ntarg position  posn[3:5] ntarg velocity
 *                     units are lightseconds and lightseconds/s      */
int getstate (int ntarg, double t1, double *posn, DPLEPH_DATA * dpl) {
  int mtarg=0, mcent=0, i=0;
  double st[6], scale=0.0;

  if ( ( ntarg <= 0 ) || ( ntarg > 15 ) ) {
    fprintf (stderr, "dpleph[getstate]: Illegal solar system body: %d\n", ntarg) ;
    return 1 ;
  }

/* Figure out correct translation for objects */
  mtarg = ntarg - 1 ;
  mcent = -1 ;
  scale = 1.0 ;
  switch ( ntarg ) {
  case 3:               /* Earth: from EM barycenter and Moon */
    mcent = 9 ;
    scale = -dpl->emratinv ;
    break ;
  case 10:              /* Moon: need to add EM barycenter */
    mtarg = 2 ;
    mcent = 9 ;
    scale = 1.0 - dpl->emratinv ;
    break ;
  case 13:              /* EM barycenter */
    mtarg = 2 ;
    break ;
  case 14:              /* Nutations */
  case 15:              /* Librations */
    mtarg -= 2 ;
    break ;
  case 12:              /* Solar system barycenter */
    mtarg = -1 ;
    for (i=0; i<6 ; i++)
      posn[i] = 0.0 ;
    return 0 ;
  default:
    break ;
  }

/* Interpolate mtarg */
  if ( dpl->ncf[mtarg] <= 0 ) {
    fprintf (stderr, "dpleph[getstate]: No data for solar system body %d\n", ntarg) ;
    return -1 ;
  }

  if ( mtarg >= 0 )
      interp (dpl->buffer+dpl->iptr[mtarg]-1, t1, posn, dpl, mtarg) ;

  if ( mcent >= 0 ) {
      interp (dpl->buffer+dpl->iptr[mcent]-1, t1, st, dpl, mcent) ;
    for (i=0; i<6; i++)
      posn[i] += st[i] * scale ;
  }

/* Scale */
  if ( mtarg < 12 )
    for (i=0; i<6; i++)
      posn[i] *= dpl->aufac ;
  else
    for (i=0; i<3; i++)
      posn[i] /= SECDAY ;

/* Return */
  return 0 ;
}


/* From heagen/barycorr/dpleph.c 
 *     This function differentiates and interpolates a set of Chebyshev coefficients to give position and velocity
 *
 *     Arguments:
 *       Input:
 *         buf   1st location of array of Chebyshev coefficients of position
 *          t1   t1 is fractional time in interval covered by coefficients at which interpolation is wanted
 *               (0 <= t1 <= 1).  T(2) IS LENGTH OF WHOLE INTERVAL IN INPUT TIME UNITS.
 *         dpl   Data structure containing former dpleph.c global variables
 *       index   index of dpl->na and dpl->ncf to use
 *       Output:
 *       pv[6]   interpolated quantities requested.             */
int interp (double *buf, double t1, double *pv, DPLEPH_DATA * dpl, int index){

  static double pc[18], vc[18] ;
  int i=0, j=0, l=0;
  static int np = 2 ;
  static int nv = 3 ;
  static double twot = 0.0 ;
  double dna=0.0, temp=0.0, dt1=0.0, tc=0.0;
  double *bufptr;
  double *pvptr;
  double vfac=0.0;

  pc[0] = 1.0 ;
  pc[1] = 0.0 ;
  vc[1] = 1.0 ;

/* Get correct sub-interval number for this set of coefficients and then get normalized Chebyshev 
   time within that subinterval */
  dna = (double) dpl->na[index] ;
  dt1 = (int) t1 ;
  temp = dna * t1 ;
  l = (int) ((double) temp - dt1) ;

/* tc is the normalized chebyshev time (-1 <= tc <= 1) */
  tc = 2.0 * (temp - floor(temp) + dt1) -1.0 ;

/* Check to see whether Chebyshev time has changed, and compute new polynomial values if it has.
   (the element pc[1] is the value of t1(tc) and hence contains the value of tc on the previous call.)
   This option was removed since it caused more grief than savings.
 */
  np = 2 ;
  nv = 3 ;
  pc[1] = tc ;
  twot = tc + tc ;

/* Be sure that at least 'ncf' polynomials have been evaluated and are stored in the array 'pc'. */
  if ( np < dpl->ncf[index] ) {
    for (i=np; i<dpl->ncf[index]; i++)
      pc[i] = twot * pc[i-1] - pc[i-2] ;
    np = dpl->ncf[index] ;
  }

/* Interpolate to get position for each component */
  bufptr = buf + l * dpl->ncf[index] * 3 ;
  pvptr = pv ;
  for (i=0; i<3; i++, pvptr++) {
    *pvptr = 0.0 ;
    for (j=0; j<dpl->ncf[index]; j++)
      *pvptr += pc[j] * *(bufptr++) ;
  }

/* If velocity interpolation is wanted, be sure enough derivative polynomials have been generated and stored. */
  vfac = dna * dpl->velfac ;
  vc[2] = twot + twot ;
  if ( nv < dpl->ncf[index] ) {
    for (i=nv; i<dpl->ncf[index]; i++)
      vc[i] = twot * vc[i-1] + 2.0 * pc[i-1] - vc[i-2] ;
    nv = dpl->ncf[index] ;
  }

/* Interpolate to get velocity for each component */
  bufptr = buf + l * dpl->ncf[index] * 3 ;
  pvptr = pv + 3 ;
  for (i=0; i<3; i++, pvptr++) {
    *pvptr = 0.0 ;
    bufptr++ ;
    for (j=1; j<dpl->ncf[index]; j++)
      *pvptr += vc[j] * *(bufptr++) ;
    *pvptr *= vfac ;
  }

/* Return */
  return 0 ;
}


/* From heagen/barycorr/dpleph.c 
 *  opens the ephemeris file and reads record <recnum> from the third extension.
 *
 *     Arguments:
 *       Input:
 *         recnum   record number to be read
 *         bcorr    Data structure containing former bary.c global variables
 *           dpl    Data structure containing former dpleph.c global variables */
int readephem (long recnum, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl){
  fitsfile *ephem_file;
  int status=0 ;
  int htype=0, any=0;
  void *dum = NULL ;

  if ( (ephem_file = openFFile (bcorr->ephfile) ) == NULL ) {
    status = 104 ;
    fprintf(stderr, "dpleph[readephem]: Cannot open file %s\n", EPHEM) ;
  }

  fits_movabs_hdu (ephem_file, 4, &htype, &status) ;
  fits_read_col (ephem_file, TDOUBLE, 1, recnum, 1, dpl->buflen, dum,
                   dpl->buffer, &any, &status) ;

  if ( !status )
    dpl->currec = recnum ;

  fits_close_file (ephem_file, &status) ;

  return status ;
}


/* From heagen/barycorr/dpleph.c 
 *     This function initializes parameters for using the JPL planetary ephemeris and returns some constants.
 *
 *     Arguments:
 *      INPUT:  ephnum   requested DE number of ephemeris (0: default)
 *      IN/OUT: bcorr    Data structure containing former bary.c global variables
 *                dpl    Data structure containing former dpleph.c global variables */
int initephem (int ephnum, BARYCORR_DATA * bcorr, DPLEPH_DATA * dpl){
  fitsfile *ephem_file;
  int status=0 ;
  char comment[80] ;
  int htype=0, any=0 ;
  char *cnam[200] ;
  char cnamchar[1400] ;
  double cval[200] ;
  char extname[80] ;
  double x=0.0 ;
  void *dum = NULL ;
  int i=0 ;

/* Set up constant name array  */
  bcorr->denum = 0 ;
  for (i=0; i<200; i++)
    cnam[i] = cnamchar + 7 * i ;
  dpl->currec = 0 ;
  if ( dpl->buffer ) free (dpl->buffer) ;

/* Open Ephemeris File */
  if ( ephnum )
    sprintf (bcorr->ephfile, "%s.%d", EPHEM, ephnum ) ;
  else
    strcpy (bcorr->ephfile, EPHEM) ;
  if ( (ephem_file = openFFile (bcorr->ephfile) ) == NULL ) {
    status = 104 ;
    fprintf(stderr, "dpleph[initephem]: Cannot open file %s\n", bcorr->ephfile) ;
    return status ;
  }

/* Check first extension */
  fits_movabs_hdu (ephem_file, 2, &htype, &status) ;
  fits_read_key (ephem_file, TSTRING, "EXTNAME", extname, comment, &status) ;
  if ( strcmp(extname, "DE1") ) {
    fprintf(stderr, "First extension of ephemeris file is wrong type: >%s<\n",
            extname);
    status = -11 ;
  }
  fits_read_key (ephem_file, TLONG, "NAXIS2", &dpl->nrecs, comment, &status) ;
  if ( dpl->nrecs > 200 )
    dpl->nrecs = 200 ;
  fits_read_col_str (ephem_file, 1, 1, 1, dpl->nrecs, "",
                   cnam, &any, &status) ;
  fits_read_col (ephem_file, TDOUBLE, 2, 1, 1, dpl->nrecs, dum,
                   cval, &any, &status) ;
  bcorr->denum = (int) ( findcval (cnam, cval, dpl->nrecs, "DENUM") + 0.5 ) ;
  dpl->clight = findcval (cnam, cval, dpl->nrecs, "CLIGHT") ;
  dpl->emratinv = findcval (cnam, cval, dpl->nrecs, "EMRAT") ;
  if ( dpl->emratinv > 0.0 )
    dpl->emratinv = 1.0 / (1.0 + dpl->emratinv) ;
  dpl->au = findcval (cnam, cval, dpl->nrecs, "AU") ;
  x = dpl->au / dpl->clight ;
  bcorr->msol = findcval (cnam, cval, dpl->nrecs, "GMS") ;
  bcorr->msol = bcorr->msol * x * x * x / (86400.0 * 86400.0) ;
  bcorr->radsol = findcval (cnam, cval, dpl->nrecs, "RADS") ;
  if ( bcorr->radsol < 0.0 )
      bcorr->radsol = findcval (cnam, cval, dpl->nrecs, "ASUN") ;
  bcorr->radsol /= dpl->clight ;
  bcorr->c = dpl->clight * 1000.0 ;

/* Check second extension */
  fits_movabs_hdu (ephem_file, 3, &htype, &status) ;
  fits_read_key (ephem_file, TSTRING, "EXTNAME", extname, comment, &status) ;
  if ( strcmp(extname, "DE2") ) {
    fprintf(stderr, "Second extension of ephemeris file is wrong type\n");
    status = -12 ;
  }
  fits_read_key (ephem_file, TLONG, "NAXIS2", &dpl->nrecs, comment, &status) ;
  if ( dpl->nrecs > 13 )
    dpl->nrecs = 13 ;
  fits_read_col (ephem_file, TLONG, 2, 1, 1, dpl->nrecs, dum,
                   dpl->iptr, &any, &status) ;
  fits_read_col (ephem_file, TLONG, 3, 1, 1, dpl->nrecs, dum,
                   dpl->ncf, &any, &status) ;
  fits_read_col (ephem_file, TLONG, 4, 1, 1, dpl->nrecs, dum,
                   dpl->na, &any, &status) ;

/* Check third extension */
  fits_movabs_hdu (ephem_file, 4, &htype, &status) ;
  fits_read_key (ephem_file, TSTRING, "EXTNAME", extname, comment, &status) ;
  if ( strcmp(extname, "DE3") ) {
    fprintf(stderr, "Third extension of ephemeris file is wrong type\n");
    status = -13 ;
  }
  fits_read_key (ephem_file, TDOUBLE, "TSTART", &dpl->ss1, comment, &status) ;
  fits_read_key (ephem_file, TDOUBLE, "TSTOP", &dpl->ss2, comment, &status) ;
  fits_read_key (ephem_file, TDOUBLE, "TIMEDEL", &dpl->ss3, comment, &status) ;
  dpl->ss3inv = 1.0 / dpl->ss3 ;
  fits_read_key (ephem_file, TLONG, "NAXIS1", &dpl->irecsz, comment, &status) ;
  dpl->buflen = dpl->irecsz / 8 ;
  dpl->buffer = (double *) calloc (dpl->buflen, sizeof (double)) ;
  fits_read_key (ephem_file, TLONG, "NAXIS2", &dpl->nrecs, comment, &status) ;
  if ( dpl->nrecs != ((long) ((dpl->ss2 - dpl->ss1 + 0.5 ) * dpl->ss3inv)) ) {
    fprintf(stderr, "Ephemeris file is wrong length\n");
    status = -10 ;
  }
  dpl->aufac = 1.0 / dpl->clight ;
  dpl->velfac = 2.0 / (dpl->ss3 * 86400.0) ;

  if ( status )
    fprintf(stderr, "dpleph[initephem]: Failed to open %s properly; status: %d\n",
            bcorr->ephfile, status) ;

  fits_close_file (ephem_file, &status) ;

  if ( ephnum && ( ephnum != bcorr->denum ) ) {
    fprintf(stderr, "dpleph[initephem]: DENUM of %s (%d) disagrees with request  (%d)\n",
            bcorr->ephfile, bcorr->denum, ephnum) ;
    status = -1 ;
  }

  return status ;
}



/* From heagen/barycorr/dpleph.c
 *     This function returns the value from <cval> that corresponds to the name <name> in <cnam>.
 *     <recnum> from the third extension.
 *
 *     Arguments:
 *       Input:
 *        cnam[n]   list of name strings
 *        cval[n]   list of values
 *              n   number of entries in cbam and cval
 *           name   name string to be matched
 *       Return value:
 *         value from cval that matches name in cnam      */
double findcval (char **cnam, double *cval, long n, char *name) {
  int i=0;
  double t=0.0;

  t = -1.0 ;
  for (i=0; i<n; i++)
    if ( !(strcmp(cnam[i], name)) ) {
      t = cval[i] ;
      break ;
    }

  return t ;
}




/* Revision Log
   $Log: from_barycorr.c,v $
   Revision 1.11  2016/06/02 21:54:52  rshill
   Added radial velocity diagnostic printout; expanded tab characters in code.

   Revision 1.10  2016/06/02 01:36:50  rshill
   Implemented printout of Earth velocity and barycentric correction components for debug=yes.

   Revision 1.9  2015/09/11 16:00:54  driethmi
   Replaced FLEN_VALUE in from_barycorr.c with NEW_FLEN_VALUE.

   Revision 1.8  2015/09/03 19:19:01  driethmi
   Removed leapsecond and ephemeris files from parameters; these are hard-coded
   into the barycorr routines.

   Revision 1.7  2015/08/27 20:42:33  driethmi
   Updated comment blocks preceding each function, and initialized all possible
   variables to zero.


*/
