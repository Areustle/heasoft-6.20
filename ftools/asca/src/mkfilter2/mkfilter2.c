/* FTOOLS info $Header: /headas/headas/ftools/asca/src/mkfilter2/mkfilter2.c,v 1.10 2001/10/15 19:07:47 zpan Exp $ */
/********************************************************************
 * main mkfilter2()
 *
 * Software mkfilter2 is consist of four parts.
 *   1) Open information file, HK files, orbital elements file,
 *        rigidity file, and attitude file.
 *   2) Create a filter file and write header part.
 *   3) Read HK files, process information, write to filter file, and
 *        repeat these until end of HK files.
 *   4) Terminate processes and  close all files.
 *
 * Ver.1.0 May. 11, 94 by T.Takeshima
 * Version 2.02b: (New version number scheme --- mkfilter2 will have
 *                 version numbers 2 and up)
 * Version 2.03: Safeguard against undefined TSTART in HK files added
 *               FTRDEF added so the output has correct size.
 *               (1994 Aug 25)
 *                Modified by Koji Mukai
 * Version 2.22: Another minor bug in TSTART, believed fixed (1995 Jul 19)
 * Version 2.23: Another minor bug in TSTART, believed fixed (1996 Sep 20)
 * Version 2.30: SIS saturation flag bug (transposed table) fixed (1996 Oct)
 * Version 2.31: Modified for use in FTOOLS. Jeff Guerber, RSTX/GSFC, Feb 1998
 * Version 2.40: Read the ALLGTI extensions and use them to improve the rate
 *               calculations.  Jeff Guerber, RSTX/GSFC, Feb. 1998
 * Version 2.31a: Commented out the ALLGTI code for FTOOLS 4.1 release, but
 *          left other changes like handling faQparam errors.  JRG April 1998
 * Version 2.32: Y2000 fixes: Write dates as yyyy-mm-dd.  START column in
 *          output file now contains a FITS new-format date/time string (and
 *          is wider).  Jeff Guerber, RSTX/GSFC, 1998-07-15.
 * Version 2.33: Open HK files earlier, so we can get date for atSetElement2
 *          from file header instead of parsing filename. Output MJD-OBS.
 *          Write checksums.  Jeff Guerber, RSTX/GSFC, 1998-08-05.
 * Version 2.34: Improved handling of SIS saturation flags (mostly mkf2RdctHK)
 *          with option to recalc as before.  Jeff Guerber 1998-08-19
 * Version 2.35: Extrapolate att file: att_margin, faQparamE.  Made Euler angs
 *          undef if problem reading from attitude file.  Warn if orbit file
 *          extrapolated too far.  Jeff Guerber 1998-08-26.
 * Version 2.36: call the new ReadLeapTable instead. Ning Gan 1999-11-05
 ********************************************************************/

#include   <stdio.h>

#include   <string.h>

#include   "fitsio.h"
#include   "cfitsio.h"
#include   "cftools.h"
#include   "mkfilter.h"
#include   "mkf2_recd.h"
#include   "faio.h"
#include   "ascatime.h"

#define    READ  0
#define    ORBIT_TOL  15   /* tolerance on orbit files, days */

/* #define DEBUG */

int mkfilter2()
{
    char           errcom[100], dummy[81];
    char           s0_hk[100], s1_hk[100], g2_hk[100], g3_hk[100];
    char           rig_file[100], orb_file[100], leap_file[100];
    char           att_file[100], out_file[100];
    int            redo_satf;
    char           key_s0[15], key_s1[15], key_g2[15], key_g3[15];
    int            val_s0, val_s1, val_g2, val_g3;
    int            nrs0, nrs1, nrg2, nrg3;
    int            ns0 = 1, ns1 = 1, ng2 = 1, ng3 = 1;
    int            s0_raw, s1_raw, g2_raw, g3_raw;
    int            tcolno, ncolno, vcolno;
    char           tcolname[5], ncolname[5], vcolname[6];

    static MkfRECD recd;

    char           inf_file[100];

    static int     iret = 0, iatt = 0, nbin = 0, iproc = -1;
    int            i, j, times[7], value=0, bsize, saa;
    char           keyword[100];
    double         mjd, tstart, tend, tasca, tsaa=0, tsunshine=0, dsec;
    double         binwid, t_s0, t_s1, t_g2, t_g3, t1, t2=0, qparam[4];
    double         att_margin;
    double         elv, cor, dye;
    AtEulerAng     euler1, euler2;
    AtTime         t;
    float          euler[3];
    int            u_s0=50, u_s1=51, u_g2=52, u_g3=53, u_out=60;
    int            blocksize = 10, group = 0, varidat = 0;
    int            bitpix = 8, naxis = 0, naxes = 0, pcount = 0, gcount = 1;
    int            tfields = 76, nrows, tf = 1, setflg = 0, hdutype = 0;
    long int       simple=1, extend = 1, exact = 1;
    char           date[70], time[70];
    char           sdate[70], edate[70], stime[70], etime[70];
    float          floval;
    int            intval, fdynt = 1;
    double         dblval, angdist;
    AtPolarVect    vp1, vp2;
    unsigned char  sunshine=0;
    AtVect         v1, v2;
    /*    GtiList        gti[4]; */  /* 0=S0, 1=S1, 2=G2, 3=G3 */
    char           msg[C_FCECHO_MSG];

/********************************************************************
 *******************************************************************
 * FIRST PART
 *******************************************************************
 ********************************************************************/

    c_ptaskn("mkfilter2 2.36");

/********************************************************************
 *  open information file and get informations (parameter file)
 ********************************************************************/
#ifdef DEBUG
    fprintf(stderr,"A\n");
#endif

    mkf2ReadInfFil( s0_hk, s1_hk, g2_hk, g3_hk,
                    rig_file, orb_file, leap_file, att_file, euler,
                    binwid, out_file, redo_satf, att_margin, iret );
    c_fcecho(" ");

#ifdef DEBUG
 fprintf( stderr, "s0_hk    = %s\n", s0_hk);
 fprintf( stderr, "s1_hk    = %s\n", s1_hk);
 fprintf( stderr, "g2_hk    = %s\n", g2_hk);
 fprintf( stderr, "g3_hk    = %s\n", g3_hk);
 fprintf( stderr, "rigfile  = %s\n", rig_file);
 fprintf( stderr, "orbfile  = %s\n", orb_file);
 fprintf( stderr, "leapfile = %s\n", leap_file);
 fprintf( stderr, "att_file = %s\n", att_file);
 fprintf( stderr, "euler[0] = %f\n", euler[0]);
 fprintf( stderr, "euler[1] = %f\n", euler[1]);
 fprintf( stderr, "euler[2] = %f\n", euler[2]);
 fprintf( stderr, "binwidth = %f\n", binwid);
 fprintf( stderr, "outfile  = %s\n", out_file);
 fprintf( stderr, "redo_satf= %d\n", redo_satf);
 fprintf( stderr, "att_margin= %f\n", att_margin);
 fprintf( stderr, "iret   = %d\n", iret );
#endif

    /* ascatime_initialize( leap_file ); */
    readLeapTable(leap_file);
    if ( iret != 1 && iret != 2 ) return(1);
    iatt = iret;
    vp1.r = 1.0; vp2.r = 1.0;
    vp1.lon =          (double)euler[0]   / RAD2DEG;
    vp1.lat = ( 90.0 - (double)euler[1] ) / RAD2DEG;
    atPolToVect( &vp1, v1 );

/********************************************************************
 * Open HK files
 ********************************************************************/

    iret = 0;
    FTOPEN( u_s0, s0_hk, READ, bsize, iret );
    if ( iret != 0 ){
        sprintf( msg, "File open error: %s %d\n", s0_hk, iret );
        c_fcerr(msg);
        return(1);
    }
    else {
        sprintf( msg, "S0-HK file: %s\n", s0_hk );
	c_fcecho(msg);
      }
    FTOPEN( u_s1, s1_hk, READ, bsize, iret );
    if ( iret != 0 ) {
        sprintf( msg, "File open error: %s %d\n", s1_hk, iret );
	c_fcerr(msg);
	return(1);
      }
    else {
        sprintf( msg, "S1-HK file: %s\n", s1_hk );
	c_fcecho(msg);
      }
    FTOPEN( u_g2, g2_hk, READ, bsize, iret );
    if ( iret != 0 ) {
        sprintf( msg, "File open error: %s %d\n", g2_hk, iret );
	c_fcerr(msg);
	return(1);
      }
    else {
        sprintf( msg, "G2-HK file: %s\n", g2_hk );
	c_fcecho(msg);
      }
    FTOPEN( u_g3, g3_hk, READ, bsize, iret );
    if ( iret != 0 ) {
        sprintf( msg, "File open error: %s %d\n", g3_hk, iret );
	c_fcerr(msg);
	return(1);
      }
    else {
        sprintf( msg, "G3-HK file: %s\n", g3_hk );
	c_fcecho(msg);
      }

/********************************************************************
 *  set orbital element
 ********************************************************************/
#ifdef DEBUG
    fprintf(stderr,"C\n");
#endif

    iret = 0;

    FTGKYS( u_s0, "DATE-OBS", date, dummy, iret );
    FTGKYS( u_s0, "TIME-OBS", time, dummy, iret );
    FTGKYD( u_s0, "MJD-OBS",   mjd, dummy, iret );
    if ( iret != 0 ) {
      sprintf( msg, "Could not read date/time keywords from %s, iret = %d",
              s0_hk, iret );
      c_fcerr( msg );  return(1);
    }

    fits_str2date( date, &t.yr, &t.mo, &t.dy, &iret );
    fits_str2time( time, 0, 0, 0, &t.hr, &t.mn, &dsec, &iret );

    t.sc = (int)dsec;  t.ms = (float)((dsec - t.sc) * 1000.);
    sprintf(msg,"Date and time are: %04d-%02d-%02d %02d:%02d:%02d  mjd=%lf\n",
            t.yr, t.mo, t.dy, t.hr, t.mn, t.sc, mjd );   c_fcecho(msg);

    sprintf(msg, "Orbit file name is %s\n", orb_file );   c_fcecho(msg);
    iret = atSetElement2( orb_file, mjd, 1 );
    sprintf( msg, "Epoch of Orbital Elements: %04d-%02d-%02d %02d:%02d:%02d\n",
                  atElement.itz.yr, atElement.itz.mo, atElement.itz.dy,
                  atElement.itz.hr, atElement.itz.mn, atElement.itz.sc );
    c_fcecho(msg);
    if ( iret != 0 ){
        sprintf( msg, "Orbital element set error. iret = %d", iret );
	c_fcerr(msg);
        return(1);
      }
    if ( mjd > atElement.mjdz + ORBIT_TOL ) {
        sprintf( msg, "Warning: Orbital elements extrapolated over %g days\n",
		 mjd-atElement.mjdz );
	c_fcerr(msg);
    }

#ifdef DEBUG
    fprintf( stderr, "Orbital Element Set Finished.\n" );
#endif

/********************************************************************
 *  Set Rigfile
 ********************************************************************/
    iret = atRigSet( rig_file );
    if ( iret != 0 ){
        sprintf( msg, "Rigidity set error. iret = %d", iret );
	c_fcerr(msg);
        return(1);
      }
    else {
        sprintf( msg, "Rigidity Data File: %s\n", rig_file );
	c_fcecho(msg);
      }

#ifdef DEBUG
    fprintf( stderr, "Rigidity Set Finished.\n" );
#endif

/********************************************************************
 *  Set Attitude File
 ********************************************************************/
    if ( iatt == 1 ){
        iret = faOpen( att_file );
        if ( iret != 0 ){
            if      ( iret == 91 ){
                sprintf( msg, "Attitude file open error. iret = %d", iret );
		c_fcerr(msg);
	      }
            else if ( iret == 93 ){
                sprintf( msg, "Attitude file read error. iret = %d", iret );
		c_fcerr(msg);
	      }
            else{
                sprintf( msg, "Error in faOpen. iret = %d", iret );
		c_fcerr(msg);
       	      }
            return(1);
          }
        else {
            sprintf( msg, "Attitude FRF: %s\n", att_file );
	    c_fcecho(msg);
          }
      }

#ifdef DEBUG
     fprintf( stderr, "Attitude File, %s,  was opened.\n", att_file );
#endif



    /**********************************************************
     * look for ALLGTI extensions (not an error if there isn't one)
     ***********************************************************/
/*    iret = 0;
 *    mkf2ReadGTI( u_s0, &gti[0], &iret );
 *    if ( iret != 0 ) {
 *      sprintf( msg, "error %d reading ALLGTI for S0-HK", iret );
 *      c_fcerr( msg ); return(iret);
 *    }
 *
 *    mkf2ReadGTI( u_s1, &gti[1], &iret );
 *  if ( iret != 0 ) {
 *    sprintf( msg, "error %d reading ALLGTI for S1-HK", iret );
 *    c_fcerr( msg ); return(iret);
 *  }
 *
 *  mkf2ReadGTI( u_g2, &gti[2], &iret );
 *  if ( iret != 0 ) {
 *    sprintf( msg, "error %d reading ALLGTI for G2-HK", iret );
 *    c_fcerr( msg ); return(iret);
 *  }
 *
 *  mkf2ReadGTI( u_g3, &gti[3], &iret );
 *  if ( iret != 0 ) {
 *    sprintf( msg, "error %d reading ALLGTI for G3-HK", iret );
 *    c_fcerr( msg ); return(iret);
 *  }
 *
 */
/*****************************************************
 * reposition to the HK data table & get header info
 *****************************************************/

    intval = 2;
    iret = 0;

    FTMAHD ( u_s0, intval, hdutype, iret );
    if ( iret != 0 ) {
        sprintf( msg, "error in FTMAHD for S0-HK %d\n", iret );
	c_fcerr(msg);
      }
    iret = 0;
    FTMAHD ( u_s1, intval, hdutype, iret );
    if ( iret != 0 ) {
        sprintf( msg, "error in FTMAHD for S1-HK %d\n", iret );
	c_fcerr(msg);
      }
    iret = 0;
    FTMAHD ( u_g2, intval, hdutype, iret );
    if ( iret != 0 ) {
        sprintf( msg, "error in FTMAHD for G2-HK %d\n", iret );
	c_fcerr(msg);
      }
    iret = 0;
    FTMAHD ( u_g3, intval, hdutype, iret );
    if ( iret != 0 ) {
        sprintf( msg, "error in FTMAHD for G3-HK %d\n", iret );
	c_fcerr(msg);
      }

    iret = 0;
    FTGKYJ ( u_s0, "NAXIS2", s0_raw, dummy, iret);
    if ( iret != 0 ) {
        sprintf( msg, "error in FTGKYJ for S0-HK %d\n", iret );
	c_fcerr(msg);
      }
    iret = 0;
    FTGKYJ ( u_s1, "NAXIS2", s1_raw, dummy, iret);
    if ( iret != 0 ) {
        sprintf( msg, "error in FTGKYJ for S1-HK %d\n", iret );
	c_fcerr(msg);
      }
    iret = 0;
    FTGKYJ ( u_g2, "NAXIS2", g2_raw, dummy, iret);
    if ( iret != 0 ) {
        sprintf( msg, "error in FTGKYJ for G2-HK %d\n", iret );
	c_fcerr(msg);
      }
    iret = 0;
    FTGKYJ ( u_g3, "NAXIS2", g3_raw, dummy, iret);
    if ( iret != 0 ) {
        sprintf( msg, "error in FTGKYJ for G3-HK %d\n", iret );
	c_fcerr(msg);
      }

#ifdef DEBUG
    fprintf( stderr, "iret = %d\n", iret );
    fprintf( stderr, "Raw # S0 %d, S1 %d, G2 %d, G3 %d\n", s0_raw, s1_raw,
	     g2_raw, g3_raw );
#endif

    /*
     * get column numbers, for S0 only -- assume same for the other files
     */

    sprintf( tcolname, "TIME" );
    sprintf( ncolname, "NAME" );
    sprintf( vcolname, "VALUE" );
    iret = 0;
    exact = 1;
    FTGCNO( u_s0, exact, tcolname,  tcolno, iret );
    if ( iret != 0 ){
        sprintf( msg, "ftgcno: iret = %d\n", iret );
	c_fcerr(msg);
        sprintf( msg, "tcolno = %d, ncolno = %d, vcolno = %d, iret = %d\n",
		 tcolno, ncolno, vcolno, iret );
	c_fcerr(msg);
      }
    iret = 0;
    FTGCNO( u_s0, exact, ncolname,  ncolno, iret );
    if ( iret != 0 ){
        sprintf( msg, "ftgcno: iret = %d\n", iret );
	c_fcerr(msg);
	sprintf( msg, "tcolno = %d, ncolno = %d, vcolno = %d, iret = %d\n",
		 tcolno, ncolno, vcolno, iret );
	c_fcerr(msg);
      }
    iret = 0;
    FTGCNO( u_s0, exact, vcolname, vcolno, iret );
    if ( iret != 0 ){
        sprintf( msg, "ftgcno: iret = %d\n", iret );
	c_fcerr(msg);
	sprintf( msg, "tcolno = %d, ncolno = %d, vcolno = %d, iret = %d\n",
		 tcolno, ncolno, vcolno, iret );
	c_fcerr(msg);
      }

/********************************************************************
 * SECOND PART -- set up output file
 ********************************************************************/
    u_out = 60; iret = 0;
    FFINIT( u_out, out_file, iret );  /* check clobber, kill if nec, ftinit */
/*    FTINIT( u_out, out_file, 2880, iret ); */
    if ( iret != 0 ){
        if ( iret = 105 ){
            sprintf( msg, "Error in subroutine <<ftinit>>.\n"
		      "Output file %s already exists.\n", out_file );
	    c_fcerr(msg);
	  }
        else {
            sprintf( msg, "Error in subroutine <<ftinit>> (FITSIO). iret=%d\n",
		     iret );  c_fcerr(msg);
          }
        return(1);
      }
    sprintf( msg, "output FITS File: %s\n", out_file );
    c_fcecho(msg);

/********************************************************************
 * Put primary header or IMAGE extension keywords into the CHU.
 ********************************************************************/
    iret = 0;
    FTPHPR( u_out, simple, bitpix, naxis, naxes, pcount, gcount, extend, iret );
    if ( iret != 0 ){
        sprintf( msg, "Error in subroutine <<ftphpr>> in FITSIO. iret = %d\n",
		 iret );    c_fcerr(msg);
        return(1);
      }

/* Write any additional keywords with FTPKYx. */


    FTPKYS( u_out, "TELESCOP", "ASCA", "mission or telescope name",  iret );

    floval = 48988.0;
    intval = 1;
    FTPKYF( u_out, "MJDREF", floval, intval, "MJD corresponding to SC clock start (1993.0)", iret );

    dblval = 0.0;
    intval = 14;
    FTPKYD( u_out, "TSTART", dblval, intval, "data start time in mission time", iret );
    FTPKYD( u_out, "TSTOP", dblval, intval, "data end time in mission time", iret );

    floval = 0.0;
    intval = 1;
    FTPKYF( u_out, "TIMEZERO", floval, intval, "offset to be applied to times given in the data", iret );
    FTPKYS( u_out, "TIMEUNIT", "s", "unit for time related keywords", iret );

    floval = 1993.0;
    intval = 1;
    FTPKYF( u_out,  "TIMESYS", floval, intval, "unit for time related keywords", iret );

    FTPKYS( u_out, "DATE-OBS", "1993-01-01",
	    "Date (yyyy-mm-dd) of the data start time (UT)", iret );
    FTPKYS( u_out, "TIME-OBS", "00:00:00",
	    "Time (hh:mm:ss) of the data start time (UT)", iret );
    FTPKYS( u_out, "DATE-END", "9999-12-31",
	    "Date (yyyy-mm-dd) of the data end time (UT)", iret );
    FTPKYS( u_out, "TIME-END", "23:59:59",
	    "Time (hh:mm:ss) of the data end time (UT)", iret );

    dblval = 48988.0; intval = 12;
    FTPKYD( u_out, "MJD-OBS", dblval, intval,
            "Modified Julian Date of the data start time", iret );

    FTPKYS( u_out, "ORIGIN", "NASA/GFSC", "who produced the fits file", iret );
    FTPDAT( u_out, iret );
    FTPKYS( u_out, "CREATOR", "MKFILTER v2.36",
            "MKFILTER Version 2.36 by T.Takeshima", iret );

/********************************************************************
 *  Define the structure of the primary array or IMAGE extension
 ********************************************************************/
    FTPDEF( u_out, bitpix, naxis, naxes, pcount, gcount, iret );
    if ( iret != 0 ){
        sprintf( msg, "Error in subroutine <<ftpdef>> in FITSIO. iret = %d\n", iret );
	c_fcerr(msg);
        return(1);
      }

/********************************************************************
 * Create (append) a new empty HDU at the end of the FITS file
 ********************************************************************/
    FTCRHD( u_out, iret );
    if ( iret != 0 ){
        sprintf( msg, "Error in <<ftcrhd>> (FITSIO).  iret = %d\n", iret );
	c_fcerr(msg);
        return(1);
      }

    mkf2Ftphbn( u_out, iret );
    if ( iret != 0 ){
            sprintf( msg, "ABEND: error in <<mkf2Ftphbn>>  iret = %d \n", iret );
	    c_fcerr(msg);
            FTCLOS( u_out, iret );
            return( iret );
          }

    FTPKYS( u_out, "TELESCOP", "ASCA", "mission or telescope name",  iret );

    floval = 48988.0;
    intval = 1;
    FTPKYF( u_out, "MJDREF", floval, intval, "MJD corresponding to SC clock start (1993.0)", iret );

    dblval = 0.0;
    intval = 14;
    FTPKYD( u_out, "TSTART", dblval, intval, "data start time in mission time", iret );
    FTPKYD( u_out, "TSTOP", dblval, intval, "data end time in mission time", iret );

    floval = 0.0;
    intval = 1;
    FTPKYF( u_out, "TIMEZERO", floval, intval, "offset to be applied to times given in the data", iret );
    FTPKYS( u_out, "TIMEUNIT", "s", "unit for time related keywords", iret );

    floval = 1993.0;
    intval = 1;
    FTPKYF( u_out, "TIMESYS", floval, intval, "unit for time related keywords", iret );

    FTPKYS( u_out, "DATE-OBS", "1993-01-01",
	    "Date (yyyy-mm-dd) of the data start time (UT)", iret );
    FTPKYS( u_out, "TIME-OBS", "00:00:00",
	    "Time (hh:mm:ss) of the data start time (UT)", iret );
    FTPKYS( u_out, "DATE-END", "9999-12-31",
	    "Date (yyyy-mm-dd) of the data end time (UT)", iret );
    FTPKYS( u_out, "TIME-END", "23:59:59",
	    "Time (hh:mm:ss) of the data end time (UT)", iret );

    dblval = 48988.0; intval = 12;
    FTPKYD( u_out, "MJD-OBS", dblval, intval,
            "Modified Julian Date of the data start time", iret );

    FTPKYS( u_out, "ORIGIN", "NASA/GSFC", "who produced the fits file", iret );
#ifdef DEBUG
    fprintf ( stderr, "before FTPDAT: iret = %d\n", iret );
#endif
    FTPDAT( u_out, iret );
#ifdef DEBUG
    fprintf ( stderr, "after FTPDAT: iret = %d\n", iret );
#endif
    FTPKYS( u_out, "CREATOR", "MKFILTER v2.36",
            "MKFILTER Version 2.36 by T.Takeshima", iret );

/********************************************************************
 * Get first Records
 ********************************************************************/
#ifdef DEBUG
    fprintf ( stderr, "before mkfGetHK: iret = %d\n", iret );
#endif
    ns0 = 1;
    iret = mkf2GetHK( u_s0, ns0, tcolno, ncolno, vcolno, &t_s0, key_s0, &val_s0 );
    iret = mkf2GetHK( u_s1, ns1, tcolno, ncolno, vcolno, &t_s1, key_s1, &val_s1 );
    iret = mkf2GetHK( u_g2, ng2, tcolno, ncolno, vcolno, &t_g2, key_g2, &val_g2 );
    iret = mkf2GetHK( u_g3, ng3, tcolno, ncolno, vcolno, &t_g3, key_g3, &val_g3 );

#ifdef DEBUG
    printf (" returned time %lf, name %s,  and value %d\n", t_s0, key_s0, val_s0);
    fprintf ( stderr, "after  mkfGetHK: iret = %d\n", iret );
    fprintf ( stderr, "tstart: %lf, %lf, %lf, %lf\n", t_s0, t_s1, t_g2, t_g3 );
#endif

    tstart = 0.0;
    if ( t_s0 > 0.0 ) tstart = t_s0;
    if ( ( tstart == 0.0 || t_s1 < tstart ) && t_s1 > 0.0 ) tstart = t_s1;
    if ( ( tstart == 0.0 || t_g2 < tstart ) && t_g2 > 0.0 ) tstart = t_g2;
    if ( ( tstart == 0.0 || t_g3 < tstart ) && t_g3 > 0.0 ) tstart = t_g3;
    if ( tstart == 0.0 ){
      sprintf( msg, "Failed to obtain sensible TSTART; output will probably be unnecessarily large.\n" );
      c_fcerr(msg);
    }

#ifdef DEBUG
    fprintf ( stderr, "before  mkf2InitRecd: binwid = %6.2f\n", recd.width );
#endif

    mkf2InitRecd( &recd );
    recd.width = (float)binwid;

#ifdef DEBUG
    fprintf ( stderr, "after  mkf2InitRecd: binwid = %6.2f\n", recd.width );
    fprintf ( stderr, "after  mkf2InitRecd: recd.T_SAA = %f\n", recd.T_SAA );
    fprintf ( stderr, "after  mkf2InitRecd: recd.T_DY_NT = %f\n", recd.T_DY_NT );
#endif

/********************************************************************
 *******************************************************************
 * THIRD PART
 *******************************************************************
 ********************************************************************/
    nbin = 0;
    while ( ns0 <= s0_raw || ns1 <= s1_raw || ng2 <= g2_raw || ng3 <= g3_raw ){
        t1    = tstart + (double)nbin            * binwid;
        t2    = tstart + (double)(nbin+1)        * binwid;
        tasca = tstart + ( (double)nbin + 0.50 ) * binwid;

#ifdef DEBUG
        fprintf ( stderr, "Before asca2attime: time = %lf -- %lf, %lf, %d-%d-%d %d:%d:%d.%d\n",
	       t1, t2, tasca, t.yr, t.mo, t.dy, t.hr, t.mn, t.sc, (int)t.ms );
#endif

	asca2attime( t1, &t );

#ifdef DEBUG
        fprintf ( stderr, "After  asca2attime: %lf: %d-%d-%d %d:%d:%d %d\n",
		  t1, t.yr, t.mo, t.dy, t.hr, t.mn, t.sc, (int)t.ms );
#endif

	atMJulian( &t, &mjd );
        mkfWrtTime( mjd, recd.start );

#ifdef DEBUG
        fprintf ( stderr, "start time = %s\n", recd.start );
#endif

/* S0_HK */
        iproc = 0;
        while ( t_s0 < t2 && ns0 <= s0_raw ){
	  /*mkf2RdctHK( &recd, t_s0, key_s0, val_s0, gti, iproc );*/
            mkf2RdctHK( &recd, t_s0, key_s0, val_s0, iproc, redo_satf );

#ifdef DEBUG
	    fprintf( stderr, "S0 %lf %s %d \n", t_s0, key_s0,  val_s0 );
	    if ( ns0 > s0_raw - 5 || ns0%10000 == 0 )
	    fprintf( stderr, "S0 nbin = %d, ns0 = %d/%d \n", nbin, ns0, s0_raw );
#endif

	    ns0++;
	    if ( ns0 <= s0_raw )
            iret = mkf2GetHK( u_s0, ns0, tcolno, ncolno, vcolno, &t_s0,
			      key_s0, &val_s0 );
          }
/* S1_HK */
        iproc = 1;
        while ( t_s1 < t2 && ns1 <= s1_raw ){
          /*  mkf2RdctHK( &recd, t_s1, key_s1, val_s1, gti, iproc ); */
	    mkf2RdctHK( &recd, t_s1, key_s1, val_s1, iproc, redo_satf );

#ifdef DEBUG
	    fprintf ( stderr, "S1 %lf %s %d \n", t_s1, key_s1,  val_s1 );
	    if ( ns1 > s1_raw - 5 || ns1 % 10000 == 0 )
	    fprintf( stderr, "S1 nbin = %d, ns1 = %d/%d \n", nbin, ns1, s1_raw );
#endif

	    ns1++;
	    if ( ns1 <= s1_raw )
            iret = mkf2GetHK( u_s1, ns1, tcolno, ncolno, vcolno, &t_s1,
			      key_s1, &val_s1 );
          }
/* G2_HK */
        iproc = 2;
        while ( t_g2 < t2 && ng2 <= g2_raw ){
	  /* mkf2RdctHK( &recd, t_g2, key_g2, val_g2, gti, iproc ); */
	    mkf2RdctHK( &recd, t_g2, key_g2, val_g2, iproc, redo_satf );

#ifdef DEBUG
	    fprintf ( stderr, "G2 %lf %s %d \n", t_g2, key_g2,  val_g2 );
	    if ( ng2 > g2_raw - 5 || ng2%10000 == 0 )
	    fprintf( stderr, "G2 nbin = %d, ng2 = %d/%d \n", nbin, ng2, g2_raw );
#endif

	    ng2++;
	    if ( ng2 <= g2_raw )
            iret = mkf2GetHK( u_g2, ng2, tcolno, ncolno, vcolno, &t_g2,
			      key_g2, &val_g2 );
          }
/* G3_HK */
        iproc = 3;
        while ( t_g3 < t2 && ng3 <= g3_raw ){
	  /* mkf2RdctHK( &recd, t_g3, key_g3, val_g3, gti, iproc ); */
	    mkf2RdctHK( &recd, t_g3, key_g3, val_g3, iproc, redo_satf );

#ifdef DEBUG
	    fprintf ( stderr, "G3 %lf %s %d \n", t_g3, key_g3,  val_g3 );
	    if ( ng3 > g3_raw - 5 || ng3%10000 == 0 )
	    fprintf( stderr, "G3 nbin = %d, ng3 = %d/%d \n", nbin, ng3, g3_raw );
	    if ( nbin > 2000 && ng3 > g3_raw - 5 )
	    fprintf( stderr, "nbin = %d, %d/%d %d/%d %d/%d %d/%d \n",
		    nbin, ns0, s0_raw, ns1, s1_raw, ng2, g2_raw, ng3, g3_raw );
	    if ( ng3 >= g3_raw ) fprintf ( stderr, "***ng3 = %d\n", ng3 );
#endif

	    ng3++;
	    if ( ng3 <= g3_raw )
            iret = mkf2GetHK( u_g3, ng3, tcolno, ncolno, vcolno, &t_g3,
			      key_g3, &val_g3 );

#ifdef DEBUG
	    if ( ng3 >= g3_raw ) fprintf ( stderr, "###ng3 = %d\n", ng3 );
#endif

          }
/* ALL */
        iproc = 9;

#ifdef DEBUG
	    if ( ng3 >= g3_raw )
            fprintf ( stderr, "ng3 = %d: Before mkf2RdctHK\n", ng3 );
#endif

        /* mkf2RdctHK( &recd, t_s0, keyword, value, gti, iproc ); */
        mkf2RdctHK( &recd, t_s0, keyword, value, iproc, redo_satf );

	iret = 0;
        if ( iatt == 1 &&
	     ( iret = faQparamE( tasca, qparam, att_margin )) <= 1 ){

            atQuatToEuler( qparam, &euler2 );

#ifdef DEBUG
	    if ( ng3 >= g3_raw )
            fprintf ( stderr, "Euler ( %7.3lf, %7.3lf, %7.3lf )   ",
            euler2.phi * RAD2DEG, euler2.theta * RAD2DEG, euler2.psi * RAD2DEG );
#endif

            recd.Euler[0] =        (float)( euler2.phi   * RAD2DEG );
            recd.Euler[1] = 90.0 - (float)( euler2.theta * RAD2DEG );
            recd.Euler[2] =        (float)( euler2.psi   * RAD2DEG );
          }
        else {   /* no attitude file, or error reading it */

#ifdef DEBUG
	    if ( ng3 >= g3_raw )
            fprintf ( stderr, "Euler ( %7.3lf, %7.3lf, %7.3lf )   ",
            euler[0], euler[1], euler[2] );
#endif

            recd.Euler[0] =        euler[0];
            recd.Euler[1] = 90.0 - euler[1];
            recd.Euler[2] =        euler[2];

	    if (iret > 1) {    /* error reading attitude file */
	      if (iret == ILLEGAL_INPUT ) {
		sprintf (msg, "mkfilter2: Warning, faQparam error: time= %.12e outside range of attitude file\n", tasca);
	      }
	      else if (iret == SEEK_ERR) {
		sprintf (msg, "mkfilter2: Warning, faQparam seek error for time= %.12e\n", tasca);
	      }
	      else if (iret == READ_ERR) {
		sprintf (msg, "mkfilter2: Warning, faQparam read error for time= %.12e\n", tasca);
	      }
	      else {
		sprintf ( msg, "mkfilter2: Warning, faQparam error %d, time= %.12e\n",
			iret, tasca );
	      }
	      c_fcecho(msg);
	      c_fcecho("           Euler angles undefined for this bin\n");

	      recd.Euler[0] = -99.999;
	      recd.Euler[1] = -99.999;
	      recd.Euler[2] = -99.999;
	    }
          }

#ifdef DEBUG
	if ( ng3 >= g3_raw )
        fprintf ( stderr, "Euler ( %7.3f, %7.3f, %7.3f ), iatt = %d\n",
                  recd.Euler[0], recd.Euler[1], recd.Euler[2], iatt );
#endif

	recd.ascatime = tasca;
        euler1.phi   = recd.Euler[0];
        euler1.theta = recd.Euler[1];
        euler1.psi   = recd.Euler[2];

        mkf2CalInf( tasca, iatt, &euler1, &elv, &cor, &saa, &recd.FOV,
                    &dye, &recd.Sat_Pos, &recd.SUNSHINE  );

#ifdef DEBUG
	if ( ng3 >= g3_raw )
	fprintf( stderr, "%d: saa = %d, FOV = %u, dye = %lf, sunshine = %u \n",
		nbin, saa, recd.FOV, dye, recd.SUNSHINE );
#endif

        recd.elv = (float)elv;
	recd.COR = (float)cor;
	recd.SAA = (unsigned char)saa;
	recd.Bright_Earth = (float)dye;

	if ( (iatt == 1) && (recd.Euler[0] > -99.) ){
            vp2.lon = (double)recd.Euler[0] / RAD2DEG;
            vp2.lat = (double)recd.Euler[1] / RAD2DEG;
            atPolToVect( &vp2, v2 );
            atAngDistance( v1, v2, &angdist );
            recd.ANG_DIST = (float)( angdist * RAD2DEG );

#ifdef DEBUG
	    if ( ng3 >= g3_raw )
	    fprintf( stderr, "Vect1: ( %7.4lf,  %7.4lf,  %7.4lf ) ->  ( %7.4lf,  %7.4lf,  %7.4lf )\n",
                vp1.r, vp1.lon, vp1.lat, v1[0], v1[1], v1[2] );
	    if ( ng3 >= g3_raw )
	    fprintf( stderr, "Vect2: ( %7.4lf,  %7.4lf,  %7.4lf ) ->  ( %7.4lf,  %7.4lf,  %7.4lf )  dist = %7.4f\n",
                vp2.r, vp2.lon, vp2.lat, v2[0], v2[1], v2[2], recd.ANG_DIST );
#endif

          }
        else {
	    recd.ANG_DIST = -99.999;
	  }

#ifdef DEBUG
	if ( ng3 >= g3_raw )
	fprintf ( stderr, "%d: recd.SAA = %u, recd.T_SAA = %f sec\n",
		  nbin, recd.SAA, recd.T_SAA );
#endif

        if ( recd.SAA == 1 ) {
	    recd.T_SAA = 0.0;
            tsaa = recd.ascatime;
	  }
	else if ( recd.T_SAA >= 0.0 ) {
	    recd.T_SAA = (float)( recd.ascatime - tsaa );
	  }
	else {
	    recd.T_SAA = -99.999;
	  }

#ifdef DEBUG
	if ( ng3 >= g3_raw )
	fprintf ( stderr, "%d: recd.SAA = %u, recd.T_SAA = %f sec\n",
		  nbin, recd.SAA, recd.T_SAA );
#endif

	if ( recd.SUNSHINE != sunshine && fdynt == 0 ){
	    recd.T_DY_NT = 0.0;
	    tsunshine = recd.ascatime;
	    sunshine  = recd.SUNSHINE;
	  }
	else if ( recd.SUNSHINE == sunshine && recd.T_DY_NT >= 0.0 ) {
	    recd.T_DY_NT = (float)( recd.ascatime - tsunshine );
	  }
	else {
	    sunshine  = recd.SUNSHINE;
	    recd.T_DY_NT = -99.999;
	    fdynt = 0;
	  }

#ifdef DEBUG
	if ( ng3 >= g3_raw ) fprintf ( stderr, "put last data\n" );
#endif

        mkf2Put2Fits( u_out, recd );

#ifdef DEBUG
	    if ( ng3 >= g3_raw )
	    fprintf( stderr, "\nout: nbin = %d, %d/%d %d/%d %d/%d %d/%d \n",
		    nbin, ns0, s0_raw, ns1, s1_raw, ng2, g2_raw, ng3, g3_raw );
#endif

        nbin++;
      }

/********************************************************************
 *******************************************************************
 * LAST PART
 *******************************************************************
 ********************************************************************/
/* Set DATE-START/END and TIME-START/END */

    asca2attime( tstart, &t );
    if ( t.yr < 100 ) t.yr += 1900;
    atMJulian( &t, &mjd );
    sprintf( sdate, "%04d-%02d-%02d", t.yr, t.mo, t.dy );
    sprintf( stime, "%02d:%02d:%02d", t.hr, t.mn, t.sc );

    asca2attime( t2, &t );
    if ( t.yr < 100 ) t.yr += 1900;
    sprintf( edate, "%04d-%02d-%02d", t.yr, t.mo, t.dy );
    sprintf( etime, "%02d:%02d:%02d", t.hr, t.mn, t.sc );

    iret = 0;
    FTMKYJ( u_out,   "NAXIS2",   nbin,         "&", iret );
    FTRDEF( u_out, iret);
    intval = 14;
    FTMKYD( u_out,   "TSTART", tstart, intval, "&", iret );
    FTMKYD( u_out,    "TSTOP",     t2, intval, "&", iret );
    FTMKYS( u_out, "DATE-OBS",  sdate,         "&", iret );
    FTMKYS( u_out, "TIME-OBS",  stime,         "&", iret );
    FTMKYS( u_out, "DATE-END",  edate,         "&", iret );
    FTMKYS( u_out, "TIME-END",  etime,         "&", iret );
    intval = 10;
    FTMKYD( u_out,  "MJD-OBS",    mjd, intval, "&", iret );
    FCPCKS( u_out, &iret );
    if ( iret != 0 ) {
       sprintf( msg,
	  "Error updating data-table keywords or checksum, iret = %d", iret);
       c_fcerr(msg);
       FCGMSG(msg); c_fcerr(msg);
       return(1);
    }

/* Move to primary header */
    intval = -1;
    FTMRHD( u_out, intval, hdutype, iret );

/* Modify values of primary header unit keywords */
    intval = 14;
    FTMKYD( u_out,   "TSTART", tstart, intval, "&", iret );
    FTMKYD( u_out,    "TSTOP",     t2, intval, "&", iret );
    FTMKYS( u_out, "DATE-OBS",  sdate,         "&", iret );
    FTMKYS( u_out, "TIME-OBS",  stime,         "&", iret );
    FTMKYS( u_out, "DATE-END",  edate,         "&", iret );
    FTMKYS( u_out, "TIME-END",  etime,         "&", iret );
    intval = 10;
    FTMKYD( u_out,  "MJD-OBS",    mjd, intval, "&", iret );
    FCPCKS( u_out, &iret );
    if ( iret != 0 ) {
       sprintf( msg,
	  "Error updating primary keywords or checksum, iret = %d", iret);
       c_fcerr(msg);
       FCGMSG(msg); c_fcerr(msg);
       return(1);
    }

    FTCLOS( u_s0,  iret );
    FTCLOS( u_s1,  iret );
    FTCLOS( u_g2,  iret );
    FTCLOS( u_g3,  iret );
    FTCLOS( u_out, iret );

    if ( iatt == 1 ) faClose();
    if ( iret == 999 ) {
      c_fcerr( "FRF Close Error.\n" );
    }
    sprintf ( msg, "Total %d Data bins were processed.\n", nbin );
    c_fcecho(msg);

    return(0);
}


/* The following code is needed to allow IRAF to call mkfilter2. */
/* Status is ignored. */

#ifdef vms
#define F77CALL mkfil2
#endif
#ifdef unix
#define F77CALL mkfil2_
#endif

void F77CALL()
{
        int mkfilter2();
	int status;

        status = mkfilter2();
}
