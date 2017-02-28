/*
 *   function double mkfReadTime( char *Time )
 *       char  yyyy-mm-ddThh:mm:ss.msc or yymmddhhmmssmsc -->
 *            double mjd (return value)
 *       called from mkfChkRecd mkfAddInf
 *
 *   function void mkfWrtTime( double mjd, char *Time )
 *       double mjd ---> char yyyy-mm-ddThh:mm:ss.msc
 *
 *   Ver.1.0  Mar.19, 1993   by T.Takeshima
 *   Ver.1.1  Sep.09, 1993   by T.Takeshima
 *                when 999.5 < msec, add 0.5 msec to mjd
 *                and re-calculate time in mkfWrtTime.
 *   Ver.1.1  Dec.03, 1993   by T.Takeshima
 *                add mkfWrtTime2( int *tin, char *Time )
 *   Ver.1.2  1998-07-15     by Jeff Guerber
 *                Y2K modifications: Write date string in new Fits format;
 *                reading, try both, return 4-digit year.
 *
 */

#include        <stdio.h>
#include        <limits.h>

#ifndef vms
#include        <fcntl.h>
#endif not vms

#include        "mkf2_recd.h"
#include        "fitsio.h"
#include        "ascatime.h"
/*
#define DEBUG
 */

/* Date string to MJD */
double mkfReadTime( char *Time )
{
    AtTime     t;
    double     mjd, dsec;
    int        ms, i, status;
    char       str[3], str2[4];

    /* First see if Time is yyyy-mm-ddThh:mm:ss.ddd format: */
    status = 0;
    if ( fits_str2time( Time, &(t.yr), &(t.mo), &(t.dy), &(t.hr),
			&(t.mn), &dsec, &status ) == 0 ) {
        t.sc = (int)dsec;
        t.ms = (float)((dsec - t.sc) * 1000.);

    } else {   /* if that gave an error, assume it's yymmddhhmmssddd: */

        sprintf( str, "00" );
        sprintf( str2, "000" );
        for( i=0; i < 2; i++){ str[i] = Time[i]; }
        sscanf( str,  "%2d", &(t.yr) );
	if (t.yr < 100) t.yr += 1900;
        for( i=0; i < 2; i++){ str[i] = Time[i+2]; }
        sscanf( str,  "%2d", &(t.mo) );
        for( i=0; i < 2; i++){ str[i] = Time[i+4]; }
        sscanf( str,  "%2d", &(t.dy) );
        for( i=0; i < 2; i++){ str[i] = Time[i+6]; }
        sscanf( str,  "%2d", &(t.hr) );
        for( i=0; i < 2; i++){ str[i] = Time[i+8]; }
        sscanf( str,  "%2d", &(t.mn) );
        for( i=0; i < 2; i++){ str[i] = Time[i+10]; }
        sscanf( str, "%2d", &(t.sc) );
        for( i=0; i < 3; i++){ str2[i] = Time[i+12]; }
        sscanf( str2, "%3d", &ms );
        t.ms = (float)ms;
    }

/* DEBUG
    printf( "DEBUG: Time: %4d-%2d-%2dT%2d:%2d:%2d.%3d\n", t.yr, t.mo, t.dy,
        t.hr, t.mn, t.sc, ms );
 */

    atMJulian( &t, &mjd );

/* DEBUG
    printf( "MJD=%lf\n", mjd );
 */

    return mjd;
}


/* MJD to date string */

void mkfWrtTime( double mjd, char *Time ){

    AtTime     t;
    int        msec, i, status;

    status = 0;

#ifdef DEBUG
    fprintf ( stderr, "mkfWrtTime: mjd = %lf\n", mjd );
#endif

    atMJDate( mjd, &t );

#ifdef DEBUG
  fprintf ( stderr, "mkfWrtTime: mjd = %lf, t = %d %d %d %d %d %d %d\n",
                   mjd, t.yr, t.mo, t.dy, t.hr, t.mn, t.sc, (int)t.ms );
#endif

    msec = (int)t.ms;
    if ( msec == 1000 ) {
        mjd += 0.0005 / 86400.0;
        atMJDate( mjd, &t );
        msec = (int)t.ms;
      }

#ifdef DEBUG
    fprintf( stderr, "%d %d %d %d %d %d %d", t.yr, t.mo, t.dy, t.hr,
	     t.mn, t.sc, msec );
#endif

/*    sprintf( Time, "%2d%2d%2d%2d%2d%2d%3d", t.yr%100, t.mo, t.dy,
 *             t.hr, t.mn, t.sc, msec );
 */

    fits_time2str( t.yr, t.mo, t.dy, t.hr, t.mn, (double)(t.sc+(msec/1000.)),
		   3, Time, &status );

    for ( i = 0; i < 23; i++ ){             /* (prob. not nec. anymore) */
        if( Time[i] == ' ' ) Time[i] = '0';
      }
    return;
  }



/* int[7] array to date string. tin[6] appears to be in units of 10msec. */

void mkfWrtTime2( int *tin, char *Time ){

    double     mjd;
    AtTime     t;
    int        msec, i, status;

    (*tin < 100) ? (t.yr = *tin + 1900) : (t.yr = *tin);
    t.mo = *( tin + 1 );
    t.dy = *( tin + 2 );
    t.hr = *( tin + 3 );
    t.mn = *( tin + 4 );
    t.sc = *( tin + 5 );
    t.ms = (float)( *( tin + 6 ) ) / 10.0;

    atMJulian( &t, &mjd );
    atMJDate( mjd, &t );
    msec = (int)t.ms;
    if ( msec == 1000 ) {
        mjd += 0.0005 / 86400.0;
        atMJDate( mjd, &t );
        msec = (int)t.ms;
      }

/*    sprintf( Time, "%2d%2d%2d%2d%2d%2d%3d", t.yr, t.mo, t.dy,
 *	     t.hr, t.mn, t.sc, msec );
 */

    fits_time2str( t.yr, t.mo, t.dy, t.hr, t.mn, (double)(t.sc+(msec/1000.)),
		   3, Time, &status );

/* DEBUG
    fprintf( stderr, "%4d-%2d-%2dT%2d:%2d:%2d.%3d\n",
             t.yr, t.mo, t.dy, t.hr, t.mn, t.sc, msec );
 */

    for ( i = 0; i < 23; i++ ){
        if( Time[i] == ' ' ) Time[i] = '0';
      }
    return;
  }


/* Ascatime to date string */

void mkfWrtTime3( double tin, char *Time ){

    AtTime     t;
    int        msec, i, status;
    double     mjd;

#ifdef DEBUG
    fprintf ( stderr, "tin = %lf: %s\n", tin, Time );
#endif

    asca2attime( tin, &t );

#ifdef DEBUG
    fprintf( stderr, "tin = %lf: %d %d %d %d %d %d %d\n", tin, t.yr, t.mo,
	     t.dy, t.hr, t.mn, t.sc, t.ms );
#endif

    atMJulian( &t, &mjd );
    atMJDate( mjd, &t );
    msec = (int)t.ms;
    if ( msec == 1000 ) {
        mjd += 0.0005 / 86400.0;
        atMJDate( mjd, &t );
        msec = (int)t.ms;
      }

/*    sprintf( Time, "%2d%2d%2d%2d%2d%2d%3d", t.yr, t.mo, t.dy,
 *	     t.hr, t.mn, t.sc, msec );
 */

    fits_time2str( t.yr, t.mo, t.dy, t.hr, t.mn, (double)(t.sc+(msec/1000.)),
		   3, Time, &status );

#ifdef DEBUG
    fprintf( stderr, "%4d-%2d-%2dT%2d:%2d:%2d.%3d\n", t.yr, t.mo, t.dy,
	     t.hr, t.mn, t.sc, msec );
#endif

    for ( i = 0; i < 23; i++ ){
        if( Time[i] == ' ' ) Time[i] = '0';
      }
    return;
  }
