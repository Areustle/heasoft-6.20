/*
 * void function mkf2RdctHK( int     ikind,    (IN)     0:S0-HK, 1:S1-HK,
 *                                                      2:G2-HK, 3:G3-HK,
 *                                                      9:BIN END
 *                           MkfRecd *recd,    (IN/OUT) mkfilter record struct
 *                           double  ascatime, (IN)     ascatime of HK parameter
 *		             char    *keyword, (IN)     keyword  of HK parameter
 *		             int     value,    (IN)     value    of HK parameter
 *                           int     redo_satf (IN)     should SIS saturation
 *                                                      flags be recalculated?
 *			    )
 * Ver.1.0    Apr. 0*, 1994   by T.Takeshima
 *
 * Ver.2.0    Sep. 20, 1994   by Ken Ebisawa
 * GIS_HV_H monitor, GIS_HV_L monitor, GIS_HV current monitor are added.
 * A bug for G3_H2 was fixed.
 * Ver.2.1    Feb. 1998 by Jeff Guerber, RSTX/GSFC.  Bug fix from Koji
 * Mukai: "if( SIStelm[i] <= cSIStelm[i]..." to <.
 * Ver.2.2    Aug. 1998 by Jeff Guerber, RSTX/GSFC. Handle SIS saturation
 *    flags better.  Option to redo as before.
 */

#include	<stdio.h>
#include	<string.h>

#include        "mkf2_recd.h"

#define  DEBUG1


int mkf2StrnCmp( char *s1, char *s2, int n ){
    int  i;

#ifdef DEBUG
    fprintf ( stderr, "mkf2StrnCmp: %s %s %d\n", s1, s2, n );
#endif

    for ( i=0; i<n; i++){
        if ( *(s1+i) != *(s2+i) ) return(-1);
      }
    return(0);
  }

void mkf2RdctHK( MkfRECD *recd, double ascatime, char *keyword, int value,
		 int ikind, int redo_satf ){

    static MkfRECD            buf;
    static double             tnsasx, tnsasy, nsas[2], t0, theta_s;
    static int                nsasset, initf = 0, nsis = 0, ngis = 0, nbin = 0;
    static int                 cSISevnt[8], cSISpixl[8], cSIStelm[8];
    static double             tSISevntp[8], tSISpixlp[8], tSIStelmp[8];
    static double             tSISevnt[8],  tSISpixl[8], tSIStelm[8];
    static int                 SISevnt[8],   SISpixl[8],  SIStelm[8];
    static int                 g2ldhit,   g2h0,   g2h1,   g2h2,   g2l0,   g2l1,   g2l2,   g2cpuin,   g2cpuout,   g2telm;
    static double             tg2ldhit,  tg2h0,  tg2h1,  tg2h2,  tg2l0,  tg2l1,  tg2l2,  tg2cpuin,  tg2cpuout,  tg2telm;
    static double             tg2ldhitp, tg2h0p, tg2h1p, tg2h2p, tg2l0p, tg2l1p, tg2l2p, tg2cpuinp, tg2cpuoutp, tg2telmp;
    static int                 g2ldhitp,  g2h0p,  g2h1p,  g2h2p,  g2l0p,  g2l1p,  g2l2p,  g2cpuinp,  g2cpuoutp;
    static int                ng2ldhit,  ng2h0,  ng2h1,  ng2h2,  ng2l0,  ng2l1,  ng2l2,  ng2cpuin,  ng2cpuout;
    static int                 g3ldhit,   g3h0,   g3h1,   g3h2,   g3l0,   g3l1,   g3l2,   g3cpuin,   g3cpuout,   g3telm,   rbmcount;
    static double             tg3ldhit,  tg3h0,  tg3h1,  tg3h2,  tg3l0,  tg3l1,  tg3l2,  tg3cpuin,  tg3cpuout,  tg3telm,  trbmcount;
    static double             tg3ldhitp, tg3h0p, tg3h1p, tg3h2p, tg3l0p, tg3l1p, tg3l2p, tg3cpuinp, tg3cpuoutp, tg3telmp, trbmcountp;
    static int                 g3ldhitp,  g3h0p,  g3h1p,  g3h2p,  g3l0p,  g3l1p,  g3l2p,  g3cpuinp,  g3cpuoutp,  rbmcountp;
    static int                ng3ldhit,  ng3h0,  ng3h1,  ng3h2,  ng3l0,  ng3l1,  ng3l2,  ng3cpuin,  ng3cpuout,  nrbmcount;
    static AtVect             Vec1, Vec0;
    static unsigned char      pGISmode, pGHVL2, pGHVH2, pGHVL3, pGHVH3;
    int                       i;

    char                      key[9];
    static int                SIStlimit[3][3] = { {256, 1024, 2048},
						  {32,   128,  256},
						  { 8,    32,   64} };
    static int                thismode;
/*                            SIS Telemetry limits for (faint, bright, fast) mode at */
/*                            (high, medium, low) bit rate, correct only if both SIS are on */

/* Initialization */
#ifdef DEBUG
    fprintf ( stderr, "now in mkf2RdctHK\n" );
#endif
    buf = *recd;
    if ( initf == 0 ){
        tnsasx = -99.9; tnsasy = 0.0;
        Vec1[0]  = 0.0;  Vec1[1] = 0.0;  Vec1[2] = 1.0;
	t0 = recd->ascatime - recd->width / 2.0;
	pGISmode = 99;
	pGHVL2 = 99; pGHVH2 = 99; pGHVL3 = 99; pGHVH3 = 99;
	g2h0 = 0; g2h1 = 0; g2h2 = 0; g2l0 = 0; g2l1 = 0; g2l2 = 0; g2cpuin = 0; g2cpuout = 0; g2telm = 0;
	g3h0 = 0; g3h1 = 0; g3h2 = 0; g3l0 = 0; g3l1 = 0; g3l2 = 0; g3cpuin = 0; g3cpuout = 0; g3telm = 0; rbmcount = 0;
	g2h0p = 0; g2h1p = 0; g2h2p = 0; g2l0p = 0; g2l1p = 0; g2l2p = 0; g2cpuinp = 0; g2cpuoutp = 0;
	g3h0p = 0; g3h1p = 0; g3h2p = 0; g3l0p = 0; g3l1p = 0; g3l2p = 0; g3cpuinp = 0; g3cpuoutp = 0; rbmcountp = 0;
	ng2ldhit = 0; ng2h0 = 0; ng2h1 = 0; ng2h2 = 0; ng2l0 = 0; ng2l1 = 0; ng2l2 = 0; ng2cpuin = 0; ng2cpuout = 0;
	ng3ldhit = 0; ng3h0 = 0; ng3h1 = 0; ng3h2 = 0; ng3l0 = 0; ng3l1 = 0; ng3l2 = 0; ng3cpuin = 0; ng3cpuout = 0; nrbmcount = 0;
	tg2h0p = t0; tg2h1p = t0; tg2h2p = t0; tg2l0p = t0; tg2l1p = t0; tg2l2p = t0;
	tg2h0  = t0; tg2h1  = t0; tg2h2  = t0; tg2l0  = t0; tg2l1  = t0; tg2l2  = t0;
	tg3h0p = t0; tg3h1p = t0; tg3h2p = t0; tg3l0p = t0; tg3l1p = t0; tg3l2p = t0;
	tg3h0  = t0; tg3h1  = t0; tg3h2  = t0; tg3l0  = t0; tg3l1  = t0; tg3l2  = t0;
	tg2cpuinp = t0; tg2cpuoutp = t0; tg2telmp = t0; tg3cpuinp = t0; tg3cpuoutp = t0; tg3telmp = t0; trbmcountp = t0;
	tg2cpuin  = t0; tg2cpuout  = t0; tg2telm  = t0;	tg3cpuin  = t0; tg3cpuout  = t0; tg3telm  = t0; trbmcount  = t0;
        tnsasx = t0 - recd->width;
        tnsasy = t0;
#ifdef DEBUG
        fprintf ( stderr, "mkf2RdctHK 1\n" );
#endif
        for ( i = 0; i < 8; i++){
	    tSISevnt[i] = 0.0; tSISevntp[i] = t0; SISevnt[i] = 0; cSISevnt[i] = 0;
      	    tSISpixl[i] = 0.0; tSISpixlp[i] = t0; SISpixl[i] = 0; cSISpixl[i] = 0;
            tSIStelm[i] = 0.0; tSIStelmp[i] = t0; SIStelm[i] = 0; cSIStelm[i] = 0;
	  }
        initf = 1;
      }

/*
 *  If it's a new bin, set the SIS saturation flags to undefined
 */
    if ( nsis == 0 ) {
        for ( i = 0; i < 8; i++ ) {
	    buf.SIS_satf[i] = 99;
        }
    }

/*
 * S0-HK processing
 */
#ifdef DEBUG
    fprintf ( stderr, "mkf2RdctHK 2 keyword: %s, ikind = %d\n", keyword, ikind );
#endif
    if ( ikind == 0 ){
/* BIT_RATE */
        if ( strncmp( keyword, "S0_BRATE", 8 ) == 0 ){
            if ( value == 4 ){
                buf.Bit_Rate = 0;
    	      }
            else if ( value == 2 ){
                buf.Bit_Rate = 1;
    	      }
            else if ( value == 1 ){
                buf.Bit_Rate = 2;
       	      }
            else {
                buf.Bit_Rate = 99;
              }
	  }

/* ACS */
        else if ( strncmp( keyword, "PCM", 3 ) == 0 ){
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-1\n" );
#endif
            if ( value == 0 && buf.ACS == 1 ) buf.ACS = 0;
            if ( value == 1 && buf.ACS == 0 ) buf.ACS = 1;
            if ( buf.ACS == 99 )              buf.ACS = (unsigned char)value;
          }
        else if ( strncmp( keyword, "SAFEHOLD", 8 ) == 0 ){
            if ( value == 0 && buf.ACS <  2 ) buf.ACS = 2;
            if ( value == 1 && buf.ACS == 2 ) buf.ACS = 0;
	    if ( buf.ACS == 99 )              buf.ACS = 2;
          }

/* NSAS */
        else if ( strncmp( keyword, "NSASXDAT", 8 ) == 0 ){
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-2\n" );
#endif
            tnsasx   = ascatime;
	    if ( value > 4095 ){
	        nsas[0]  =  (double)( value / 16 )/2.0;
	      }
	    else {
	        nsas[0]  = (double)value / 2.0;
	      }
            nsasset = nsas_to_sunvec( nsas, Vec0 );
            if ( nsasset == 0 && nsas[0] != 0.0 && nsas[1] != 0.0 ){
                atAngDistance( Vec0, Vec1, &theta_s );
                buf.NSAS = (float)( theta_s * RAD2DEG );
              }
            else {
                buf.NSAS = -99.999;
              }
#ifdef DEBUG
	    fprintf ( stderr, "TIME=%lf, NSASDATX=%d, NSAS=%5.2f, nsasset=%d, nsas=%6.1lf, %6.1lf\n", ascatime, value, buf.NSAS, nsasset, nsas[0], nsas[1] );
#endif
          }
        else if ( strncmp( keyword, "NSASYDAT", 8 ) == 0 ){
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-3\n" );
#endif
            tnsasy   = ascatime;
	    if ( value > 4095 ){
	        nsas[1]  = (double)( value / 16 )/2.0;
	      }
	    else {
	        nsas[1]  = (double)value / 2.0;
	      }
            nsasset = nsas_to_sunvec( nsas, Vec0 );
            if ( nsasset == 0 && nsas[0] != 0.0 && nsas[1] != 0.0 ){
                atAngDistance( Vec0, Vec1, &theta_s );
                buf.NSAS = (float)( theta_s * RAD2DEG );
              }
            else {
	        buf.NSAS = -99.999;
	      }
#ifdef DEBUG
	    fprintf ( stderr, "TIME=%lf, NSASDATY=%d, NSAS=%5.2f, nsasset=%d, nsas=%6.1lf, %6.1lf\n", ascatime, value, buf.NSAS, nsasset, nsas[0], nsas[1] );

#endif
	  }
        else if ( strncmp( keyword, "SUNPRSNX", 8 ) == 0 && value == 0 ) {
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-4\n" );
#endif
	    buf.NSAS = -99.999;
	  }
        else if ( strncmp( keyword, "SUNPRSNY", 8 ) == 0 && value == 0 ) {
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-5\n" );
#endif
	    buf.NSAS = -99.999;
	  }
	else if ( strncmp( keyword, "ETI", 3 ) == 0 ){
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-6\n" );
#endif
	    buf.ETI = value;
	  }

/* S0_MODE */
        else if ( strncmp( keyword, "S0_MODE", 8 ) == 0 ){
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-7\n" );
#endif
	    buf.SIS_obs_mode[0] = (unsigned char)value - 1;
	  }

/* S0_ID */
        else if ( strncmp( keyword, "S0_CCD0", 7 ) == 0 ) {
            if ( buf.SIS_ID[0] == -99 ) buf.SIS_ID[0] = value * 1000;
            else                        buf.SIS_ID[0] = buf.SIS_ID[0] % 1000 + value * 1000;
          }
        else if ( strncmp( keyword, "S0_CCD1", 7 ) == 0 ) {
            if ( buf.SIS_ID[0] == -99 ) buf.SIS_ID[0] = value * 100;
            else                        buf.SIS_ID[0] = ( buf.SIS_ID[0] / 1000 ) * 1000 + value * 100 + buf.SIS_ID[0] % 100;
          }
        else if ( strncmp( keyword, "S0_CCD2", 7 ) == 0 ) {
            if ( buf.SIS_ID[0] == -99 ) buf.SIS_ID[0] = value * 10;
            else                        buf.SIS_ID[0] = ( buf.SIS_ID[0] / 100  ) * 100  + value * 10  + buf.SIS_ID[0] % 10;
          }
        else if ( strncmp( keyword, "S0_CCD3", 7 ) == 0 ) {
            if ( buf.SIS_ID[0] == -99 ) buf.SIS_ID[0] = value * 1;
            else                        buf.SIS_ID[0] = ( buf.SIS_ID[0] / 10 )   * 10   + value;
          }

/* S0_dscr */
        else if ( strncmp( keyword, "S0_ARENA", 8 ) == 0 ) {
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-12\n" );
#endif
            if ( buf.SIS_dscr[0] == 99               )     buf.SIS_dscr[0]  = (unsigned char)value * 2;
            if ( buf.SIS_dscr[0] <   2 && value == 1 )     buf.SIS_dscr[0] += 2;
            if ( buf.SIS_dscr[0] >=  2 && value == 0 )     buf.SIS_dscr[0] -= 2;
          }
        else if ( strncmp( keyword, "S0_LVENA", 8 ) == 0 ) {
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-13\n" );
#endif
            if ( buf.SIS_dscr[0]     == 99               ) buf.SIS_dscr[0]  = (unsigned char)value;
            if ( buf.SIS_dscr[0] % 2 == 0  && value == 1 ) buf.SIS_dscr[0] += 1;
            if ( buf.SIS_dscr[0] % 2 == 1  && value == 0 ) buf.SIS_dscr[0] -= 1;
          }

/* S0_Grade */
        else if ( strncmp( keyword, "S0_GRADE", 8 ) == 0 ){
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-14\n" );
#endif
	    buf.SIS_Grade[0] = value;
	  }

/* S0_AE */
        else if ( strncmp( keyword, "S0_AEPOW", 8 ) == 0 ) {
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-15\n" );
#endif
            if ( buf.SIS_AE[0]     == 99 )               buf.SIS_AE[0]  = (unsigned char)value;
            if ( buf.SIS_AE[0] % 2 == 0  && value == 1 ) buf.SIS_AE[0] += 1;
            if ( buf.SIS_AE[0] % 2 == 1  && value == 0 ) buf.SIS_AE[0] -= 1;
          }
        else if ( strncmp( keyword, "S0_AEANL", 8 ) == 0 ) {
            if ( buf.SIS_AE[0]     == 99 )               buf.SIS_AE[0]  = (unsigned char)value * 2;
            if ( buf.SIS_AE[0] % 2 == 0  && value == 1 ) buf.SIS_AE[0] += 1;
            if ( buf.SIS_AE[0] % 2 == 1  && value == 0 ) buf.SIS_AE[0] -= 1;
          }

/* S0_TEMP */
        else if ( strncmp( keyword, "S0_TEMP", 7 ) == 0 ){
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-16\n" );
#endif
	   buf.SIS_temp[0] = 0.0469 * (float)value - 116.68;
	  }

/* S0_Event_thr */
        else if ( strncmp( keyword, "S0_EVTR0", 8 ) == 0 ){
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-16\n" );
#endif
	    buf.SIS_Event_thr[0] = value;
	  }
        else if ( strncmp( keyword, "S0_EVTR1", 8 ) == 0 ){
	    buf.SIS_Event_thr[1] = value;
	  }
        else if ( strncmp( keyword, "S0_EVTR2", 8 ) == 0 ){
	    buf.SIS_Event_thr[2] = value;
	  }
        else if ( strncmp( keyword, "S0_EVTR3", 8 ) == 0 ){
	    buf.SIS_Event_thr[3] = value;
	  }

/* S0_Split_thr */
        else if ( strncmp( keyword, "S0_SPTR0", 8 ) == 0 ){
	    buf.SIS_Split_thr[0] = value;
	  }
        else if ( strncmp( keyword, "S0_SPTR1", 8 ) == 0 ){
	    buf.SIS_Split_thr[1] = value;
	  }
        else if ( strncmp( keyword, "S0_SPTR2", 8 ) == 0 ){
	    buf.SIS_Split_thr[2] = value;
	  }
        else if ( strncmp( keyword, "S0_SPTR3", 8 ) == 0 ){
	    buf.SIS_Split_thr[3] = value;
	  }

/* S0_EVENT_NUMBER */
        else if ( strncmp( keyword, "S0_EVNT0", 8 ) == 0 ) {
            tSISevnt[0]  = ascatime;
    	    SISevnt[0]  += value;
            cSISevnt[0] += 1;
          }
        else if ( strncmp( keyword, "S0_EVNT1", 8 ) == 0 ) {
            tSISevnt[1]  = ascatime;
    	    SISevnt[1]  += value;
            cSISevnt[1] += 1;
          }
        else if ( strncmp( keyword, "S0_EVNT2", 8 ) == 0 ) {
            tSISevnt[2]  = ascatime;
    	    SISevnt[2]  += value;
            cSISevnt[2] += 1;
          }
        else if ( strncmp( keyword, "S0_EVNT3", 8 ) == 0 ) {
            tSISevnt[3]  = ascatime;
     	    SISevnt[3]  += value;
            cSISevnt[3] += 1;
          }

/* S0_PIXEL_NUMBER */
        else if ( strncmp( keyword, "S0_PIXL0", 8 ) == 0 ) {
            tSISpixl[0]  = ascatime;
    	    SISpixl[0]  += value;
            cSISpixl[0] += 1;
          }
        else if ( strncmp( keyword, "S0_PIXL1", 8 ) == 0 ) {
            tSISpixl[1]  = ascatime;
    	    SISpixl[1]  += value;
            cSISpixl[1] += 1;
          }
        else if ( strncmp( keyword, "S0_PIXL2", 8 ) == 0 ) {
            tSISpixl[2]  = ascatime;
    	    SISpixl[2]  += value;
            cSISpixl[2] += 1;
          }
        else if ( strncmp( keyword, "S0_PIXL3", 8 ) == 0 ) {
            tSISpixl[3]  = ascatime;
            SISpixl[3]  += value;
            cSISpixl[3] += 1;
          }

/* S0_TELM_EVENT_NUMBER */
        else if ( strncmp( keyword, "S0_TELM0", 8 ) == 0 ) {
            tSIStelm[0]  = ascatime;
	    SIStelm[0]  += value;
            cSIStelm[0] += 1;
          }
        else if ( strncmp( keyword, "S0_TELM1", 8 ) == 0 ) {
            tSIStelm[1]  = ascatime;
    	    SIStelm[1]  += value;
            cSIStelm[1] += 1;
          }
        else if ( strncmp( keyword, "S0_TELM2", 8 ) == 0 ) {
            tSIStelm[2]  = ascatime;
    	    SIStelm[2]  += value;
            cSIStelm[2] += 1;
          }
        else if ( strncmp( keyword, "S0_TELM3", 8 ) == 0 ) {
            tSIStelm[3]  = ascatime;
    	    SIStelm[3]  += value;
            cSIStelm[3] += 1;
          }

/* SIS-0 Saturation Flag */
        else if ( strncmp( keyword, "S0_SATF0", 8 ) == 0 ) {
            if ( buf.SIS_satf[0] == 99 )               buf.SIS_satf[0]  = (unsigned char)value;
            if ( buf.SIS_satf[0] == 0  && value == 1 ) buf.SIS_satf[0]  = 1;
          }
        else if ( strncmp( keyword, "S0_SATF1", 8 ) == 0 ) {
            if ( buf.SIS_satf[1] == 99 )               buf.SIS_satf[1]  = (unsigned char)value;
            if ( buf.SIS_satf[1] == 0  && value == 1 ) buf.SIS_satf[1]  = 1;
          }
        else if ( strncmp( keyword, "S0_SATF2", 8 ) == 0 ) {
            if ( buf.SIS_satf[2] == 99 )               buf.SIS_satf[2]  = (unsigned char)value;
            if ( buf.SIS_satf[2] == 0  && value == 1 ) buf.SIS_satf[2]  = 1;
          }
        else if ( strncmp( keyword, "S0_SATF3", 8 ) == 0 ) {
            if ( buf.SIS_satf[3] == 99 )               buf.SIS_satf[3]  = (unsigned char)value;
            if ( buf.SIS_satf[3] == 0  && value == 1 ) buf.SIS_satf[3]  = 1;
          }

/* SIS RBM Flag */
	else if ( strncmp( keyword, "SIS_RBMF", 8 ) == 0 ){
	    buf.RBM_flg[1] = (unsigned char)value;
	  }
	nsis++;
      }

/*
 * S1-HK processing
 */
    else if ( ikind == 1 ){
#ifdef DEBUG
        fprintf ( stderr, "mkf2RdctHK 2\n" );
#endif
/* BIT_RATE */
        if ( strncmp( keyword, "S1_BRATE", 8 ) == 0 ){
            if ( value == 4 ){
                buf.Bit_Rate = 0;
    	      }
            else if ( value == 2 ){
                buf.Bit_Rate = 1;
    	      }
            else if ( value == 1 ){
                buf.Bit_Rate = 2;
       	      }
            else {
                buf.Bit_Rate = 99;
              }
	  }

/* ACS */
        else if ( strncmp( keyword, "PCM", 3 ) == 0 ){
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-1\n" );
#endif
            if ( value == 0 && buf.ACS == 1 ) buf.ACS = 0;
            if ( value == 1 && buf.ACS == 0 ) buf.ACS = 1;
            if ( buf.ACS == 99 )              buf.ACS = (unsigned char)value;
          }
        else if ( strncmp( keyword, "SAFEHOLD", 8 ) == 0 ){
            if ( value == 0 && buf.ACS <  2 ) buf.ACS = 2;
            if ( value == 1 && buf.ACS == 2 ) buf.ACS = 0;
	    if ( buf.ACS == 99 )              buf.ACS = 2;
          }

/* NSAS */
        else if ( strncmp( keyword, "NSASXDAT", 8 ) == 0 ){
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-2\n" );
#endif
            tnsasx   = ascatime;
	    if ( value > 4095 ){
	        nsas[0]  =  (double)( value / 16 )/2.0;
	      }
	    else {
	        nsas[0]  = (double)value / 2.0;
	      }
            nsasset = nsas_to_sunvec( nsas, Vec0 );
            if ( nsasset == 0 && nsas[0] != 0.0 && nsas[1] != 0.0 ){
                atAngDistance( Vec0, Vec1, &theta_s );
                buf.NSAS = (float)( theta_s * RAD2DEG );
              }
            else {
                buf.NSAS = -99.999;
              }
#ifdef DEBUG
	    fprintf ( stderr, "TIME=%lf, NSASDATX=%d, NSAS=%5.2f, nsasset=%d, nsas=%6.1lf, %6.1lf\n", ascatime, value, buf.NSAS, nsasset, nsas[0], nsas[1] );
#endif
          }
        else if ( strncmp( keyword, "NSASYDAT", 8 ) == 0 ){
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-3\n" );
#endif
            tnsasy   = ascatime;
	    if ( value > 4095 ){
	        nsas[1]  = (double)( value / 16 )/2.0;
	      }
	    else {
	        nsas[1]  = (double)value / 2.0;
	      }
            nsasset = nsas_to_sunvec( nsas, Vec0 );
            if ( nsasset == 0 && nsas[0] != 0.0 && nsas[1] != 0.0 ){
                atAngDistance( Vec0, Vec1, &theta_s );
                buf.NSAS = (float)( theta_s * RAD2DEG );
              }
            else {
	        buf.NSAS = -99.999;
	      }
#ifdef DEBUG
	    fprintf ( stderr, "TIME=%lf, NSASDATY=%d, NSAS=%5.2f, nsasset=%d, nsas=%6.1lf, %6.1lf\n", ascatime, value, buf.NSAS, nsasset, nsas[0], nsas[1] );

#endif
	  }
        else if ( strncmp( keyword, "SUNPRSNX", 8 ) == 0 && value == 0 ) {
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-4\n" );
#endif
	    buf.NSAS = -99.999;
	  }
        else if ( strncmp( keyword, "SUNPRSNY", 8 ) == 0 && value == 0 ) {
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-5\n" );
#endif
	    buf.NSAS = -99.999;
	  }
	else if ( strncmp( keyword, "ETI", 3 ) == 0 ){
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK 2-6\n" );
#endif
	    buf.ETI = value;
	  }

/* S1_MODE */
        if ( strncmp( keyword, "S1_MODE", 8 ) == 0 ){
	    buf.SIS_obs_mode[1] = (unsigned char)value - 1;
          }

/* S1_ID */
        else if ( strncmp( keyword, "S1_CCD0", 7 ) == 0 ) {
            if ( buf.SIS_ID[1] == -99 ) buf.SIS_ID[1] =                                   value * 1000;
            else                        buf.SIS_ID[1] =                                   value * 1000 + buf.SIS_ID[1] % 1000;
          }
        else if ( strncmp( keyword, "S1_CCD1", 7 ) == 0 ) {
            if ( buf.SIS_ID[1] == -99 ) buf.SIS_ID[1] =                                   value * 100;
            else                        buf.SIS_ID[1] = ( buf.SIS_ID[1] / 1000 ) * 1000 + value * 100  + buf.SIS_ID[1] % 100;
          }
        else if ( strncmp( keyword, "S1_CCD2", 7 ) == 0 ) {
            if ( buf.SIS_ID[1] == -99 ) buf.SIS_ID[1] =                                   value * 10;
            else                        buf.SIS_ID[1] = ( buf.SIS_ID[1] / 100  ) * 100  + value * 10   + buf.SIS_ID[1] % 10;
          }
        else if ( strncmp( keyword, "S1_CCD3", 7 ) == 0 ) {
            if ( buf.SIS_ID[1] == -99 ) buf.SIS_ID[1] =                                   value;
            else                        buf.SIS_ID[1] = ( buf.SIS_ID[1] / 10   ) * 10   + value;
          }

/* S1_dscr */
        else if ( strncmp( keyword, "S1_ARENA", 8 ) == 0 ) {
            if ( buf.SIS_dscr[1] == 99               )     buf.SIS_dscr[1] = (unsigned char)value * 2;
            if ( buf.SIS_dscr[1] <   2 && value == 1 )     buf.SIS_dscr[1] += 2;
            if ( buf.SIS_dscr[1] >=  2 && value == 0 )     buf.SIS_dscr[1] -= 2;
          }
        else if ( strncmp( keyword, "S1_LVENA", 8 ) == 0 ) {
            if ( buf.SIS_dscr[1]     == 99               ) buf.SIS_dscr[1]  = (unsigned char)value;
            if ( buf.SIS_dscr[1] % 2 == 0  && value == 1 ) buf.SIS_dscr[1] += 1;
            if ( buf.SIS_dscr[1] % 2 == 1  && value == 0 ) buf.SIS_dscr[1] -= 1;
          }

/* S1_Grade */
        else if ( strncmp( keyword, "S1_GRADE", 8 ) == 0 ){
	    buf.SIS_Grade[1] = value;
	  }

/* S1_AE */
        else if ( strncmp( keyword, "S1_AEPOW", 8 ) == 0 ) {
            if ( buf.SIS_AE[1]     == 99 )               buf.SIS_AE[1]  = (unsigned char)value;
            if ( buf.SIS_AE[1] % 2 == 0  && value == 1 ) buf.SIS_AE[1] += 1;
            if ( buf.SIS_AE[1] % 2 == 1  && value == 0 ) buf.SIS_AE[1] -= 1;
          }
        else if ( strncmp( keyword, "S1_AEANL", 8 ) == 0 ) {
            if ( buf.SIS_AE[1]     == 99 )               buf.SIS_AE[1]  = (unsigned char)value * 2;
            if ( buf.SIS_AE[1] % 2 == 0  && value == 1 ) buf.SIS_AE[1] += 1;
            if ( buf.SIS_AE[1] % 2 == 1  && value == 0 ) buf.SIS_AE[1] -= 1;
          }

/* S1_TEMP */
        else if ( strncmp( keyword, "S1_TEMP", 7 ) == 0 ){
	    buf.SIS_temp[1] = 0.0469 * (float)value - 116.68;
          }

/* S1_Event_thr */
        else if ( strncmp( keyword, "S1_EVTR0", 8 ) == 0 ){
	    buf.SIS_Event_thr[4] = value;
          }
        else if ( strncmp( keyword, "S1_EVTR1", 8 ) == 0 ){
	    buf.SIS_Event_thr[5] = value;
          }
        else if ( strncmp( keyword, "S1_EVTR2", 8 ) == 0 ){
	    buf.SIS_Event_thr[6] = value;
          }
        else if ( strncmp( keyword, "S1_EVTR3", 8 ) == 0 ){
	      buf.SIS_Event_thr[7] = value;
          }

/* S1_Split_thr */
        else if ( strncmp( keyword, "S1_SPTR0", 8 ) == 0 ){
	    buf.SIS_Split_thr[4] = value;
          }
        else if ( strncmp( keyword, "S1_SPTR1", 8 ) == 0 ){
	    buf.SIS_Split_thr[5] = value;
          }
        else if ( strncmp( keyword, "S1_SPTR2", 8 ) == 0 ){
	    buf.SIS_Split_thr[6] = value;
          }
        else if ( strncmp( keyword, "S1_SPTR3", 8 ) == 0 ){
	    buf.SIS_Split_thr[7] = value;
          }

/* S1_EVENT_NUMBER */
        else if ( strncmp( keyword, "S1_EVNT0", 8 ) == 0 ) {
            tSISevnt[4]  = ascatime;
    	    SISevnt[4]  += value;
            cSISevnt[4] += 1;
          }
        else if ( strncmp( keyword, "S1_EVNT1", 8 ) == 0 ) {
            tSISevnt[5]  = ascatime;
            SISevnt[5]  += value;
            cSISevnt[5] += 1;
          }
        else if ( strncmp( keyword, "S1_EVNT2", 8 ) == 0 ) {
            tSISevnt[6]  = ascatime;
    	    SISevnt[6]  += value;
            cSISevnt[6] += 1;
          }
        else if ( strncmp( keyword, "S1_EVNT3", 8 ) == 0 ) {
            tSISevnt[7]  = ascatime;
    	    SISevnt[7]  += value;
            cSISevnt[7] += 1;
          }

/* S1_PIXL_NUMBER */
        else if ( strncmp( keyword, "S1_PIXL0", 8 ) == 0 ) {
            tSISpixl[4]  = ascatime;
    	    SISpixl[4]  += value;
            cSISpixl[4] += 1;
          }
        else if ( strncmp( keyword, "S1_PIXL1", 8 ) == 0 ) {
            tSISpixl[5]  = ascatime;
    	    SISpixl[5]  += value;
            cSISpixl[5] += 1;
          }
        else if ( strncmp( keyword, "S1_PIXL2", 8 ) == 0 ) {
            tSISpixl[6]  = ascatime;
    	    SISpixl[6]  += value;
            cSISpixl[6] += 1;
          }
        else if ( strncmp( keyword, "S1_PIXL3", 8 ) == 0 ) {
            tSISpixl[7]  = ascatime;
	    SISpixl[7]  += value;
            cSISpixl[7] += 1;
          }

/* S1_TELM_EVENT_NUMBER */
        else if ( strncmp( keyword, "S1_TELM0", 8 ) == 0 ) {
            tSIStelm[4]  = ascatime;
	    SIStelm[4]  += value;
            cSIStelm[4] += 1;
          }
        else if ( strncmp( keyword, "S1_TELM1", 8 ) == 0 ) {
            tSIStelm[5]  = ascatime;
	    SIStelm[5]  += value;
            cSIStelm[5] += 1;
          }
        else if ( strncmp( keyword, "S1_TELM2", 8 ) == 0 ) {
            tSIStelm[6]  = ascatime;
	    SIStelm[6]  += value;
            cSIStelm[6] += 1;
          }
        else if ( strncmp( keyword, "S1_TELM3", 8 ) == 0 ) {
            tSIStelm[7]  = ascatime;
	    SIStelm[7]  += value;
            cSIStelm[7] += 1;
          }

/* SIS-1 Saturation Flag */
        else if ( strncmp( keyword, "S1_SATF0", 8 ) == 0 ) {
            if ( buf.SIS_satf[4] == 99 )               buf.SIS_satf[4]  = (unsigned char)value;
            if ( buf.SIS_satf[4] == 0  && value == 1 ) buf.SIS_satf[4]  = 1;
          }
        else if ( strncmp( keyword, "S1_SATF1", 8 ) == 0 ) {
            if ( buf.SIS_satf[5] == 99 )               buf.SIS_satf[5]  = (unsigned char)value;
            if ( buf.SIS_satf[5] == 0  && value == 1 ) buf.SIS_satf[5]  = 1;
          }
        else if ( strncmp( keyword, "S1_SATF2", 8 ) == 0 ) {
            if ( buf.SIS_satf[6] == 99 )               buf.SIS_satf[6]  = (unsigned char)value;
            if ( buf.SIS_satf[6] == 0  && value == 1 ) buf.SIS_satf[6]  = 1;
          }
        else if ( strncmp( keyword, "S1_SATF3", 8 ) == 0 ) {
            if ( buf.SIS_satf[7] == 99 )               buf.SIS_satf[7]  = (unsigned char)value;
            if ( buf.SIS_satf[7] == 0  && value == 1 ) buf.SIS_satf[7]  = 1;
          }

	nsis++;
      }

/*
 * G2-HK processing
 */
    else if ( ikind == 2 ){
#ifdef DEBUG
    fprintf ( stderr, "mkf2RdctHK 3\n" );
#endif
/* GIS Observation Mode */
        if ( strncmp( keyword, "MEM_CHK", 7 ) == 0 ) {
	    if ( value == 1 ){
	        pGISmode = buf.GIS_obs_mode;
		buf.GIS_obs_mode = 3;
	      }
	    else {
	        buf.GIS_obs_mode = pGISmode;
	      }
	  }
	else if ( strncmp( keyword, "MPC",  3 ) == 0 &&
	          strncmp( keyword, "MPC_", 4 ) != 0 ) {
	    if ( value == 0 ) {
	        buf.GIS_obs_mode = 0;
                pGISmode = 0;
	      }
	    else if ( value == 1 ) {
	        buf.GIS_obs_mode = 1;
                pGISmode = 1;
	      }
	  }
        else if ( strncmp( keyword, "PCAL", 4 ) == 0 ) {
	    if ( value == 1 ){
	        pGISmode = buf.GIS_obs_mode;
		buf.GIS_obs_mode = 2;
	      }
	    else {
	        buf.GIS_obs_mode = pGISmode;
	      }
	  }

/* G2 HVL LEVEL */
        else if ( strncmp( keyword, "HVL2_POW", 8 ) == 0 ) {
	    if ( value == 0 ){
	        pGHVL2 = buf.GIS_HVL[0];
	        buf.GIS_HVL[0] = 16;
	      }
	    else {
	        buf.GIS_HVL[0] = pGHVL2;
	      }
#ifdef DEBUG
	    fprintf( stderr, "HVL2_POW, val=%d GIS_HVL[0] = %u\n", value, buf.GIS_HVL[0] );
#endif
	  }
        else if ( strncmp( keyword, "GHV2_L", 6 ) == 0 ) {
	    if ( buf.GIS_HVL[0] == 8 || buf.GIS_HVL[0] == 16 ){
	        pGHVL2         = (unsigned char)value;
	      }
	    else {
	        buf.GIS_HVL[0] = (unsigned char)value;
	        pGHVL2         = (unsigned char)value;
	      }
#ifdef DEBUG
	    fprintf( stderr, "GHV2_L, val=%d GIS_HVL[0] = %u\n", value, buf.GIS_HVL[0] );
#endif
	  }

/* G2 HVH LEVEL */
        else if ( strncmp( keyword, "HVH2_POW", 8 ) == 0 ) {
	    if ( value == 0 ){
	        pGHVH2    = buf.GIS_HVH[0];
	        buf.GIS_HVH[0] = 16;
	      }
	    else {
	        buf.GIS_HVH[0] = pGHVH2;
	      }
#ifdef DEBUG
	    fprintf( stderr, "HVH2_POW, val=%d GIS_HVH[0] = %u\n", value, buf.GIS_HVH[0] );
#endif
	  }
        else if ( strncmp( keyword, "GHV2_H", 6 ) == 0 ) {
	    if ( buf.GIS_HVH[0] == 8 || buf.GIS_HVH[0] == 16 ) {
	        pGHVH2         = (unsigned char)value;
	      }
	    else {
	        buf.GIS_HVH[0] = (unsigned char)value;
	        pGHVH2         = (unsigned char)value;
	      }
#ifdef DEBUG
	    fprintf( stderr, "GHV2_H, val=%d GIS_HVH[0] = %u\n", value, buf.GIS_HVH[0] );
#endif
	  }

/* G2-3 HV-RED */
        else if ( strncmp( keyword, "GHV_RED", 7 ) == 0 ) {
            if ( value == 0 ){
                if ( buf.GIS_HVL[0] == 8 )  buf.GIS_HVL[0] = pGHVL2;
                if ( buf.GIS_HVH[0] == 8 )  buf.GIS_HVH[0] = pGHVH2;
                if ( buf.GIS_HVL[1] == 8 )  buf.GIS_HVL[1] = pGHVL3;
                if ( buf.GIS_HVH[1] == 8 )  buf.GIS_HVH[1] = pGHVH3;
	      }
	    else {
	        if ( buf.GIS_HVL[0] != 8 ) {
		    pGHVL2         = buf.GIS_HVL[0];
		    buf.GIS_HVL[0] = 8;
		  }
	        if ( buf.GIS_HVH[0] != 8 ) {
		    pGHVH2         = buf.GIS_HVH[0];
		    buf.GIS_HVH[0] = 8;
		  }
	        if ( buf.GIS_HVL[1] != 8 ) {
		    pGHVL3         = buf.GIS_HVL[1];
		    buf.GIS_HVL[1] = 8;
		  }
	        if ( buf.GIS_HVH[1] != 8 ) {
		    pGHVH3         = buf.GIS_HVH[1];
		    buf.GIS_HVH[1] = 8;
		  }
	      }
	  }

/* G2 LDHIT */
	else if ( strncmp( keyword, "G2_LDHIT", 8 ) == 0 ) {
	    if ( g2ldhit > value ) ng2ldhit++;
	    tg2ldhit = ascatime;
	     g2ldhit = value;
	  }

/* G2 H0 */
	else if ( strncmp( keyword, "G2_H0", 5 ) == 0 ) {
	    if ( g2h0 > value ) ng2h0++;
	    tg2h0 = ascatime;
	     g2h0 = value;
	  }

/* G2 H1 */
	else if ( strncmp( keyword, "G2_H1", 5 ) == 0 ) {
	    if ( g2h1 > value ) ng2h1++;
	    tg2h1 = ascatime;
	     g2h1 = value;
	  }

/* G2 H2 */
	else if ( strncmp( keyword, "G2_H2", 5 ) == 0 ) {
	    if ( g2h2 > value ) ng2h2++;
	    tg2h2 = ascatime;
	     g2h2 = value;
	  }

/* G2 L0 */
	else if ( strncmp( keyword, "G2_L0", 5 ) == 0 ) {
	    if ( g2l0 > value ) ng2l0++;
	    tg2l0 = ascatime;
	     g2l0 = value;
	  }

/* G2 L1 */
	else if ( strncmp( keyword, "G2_L1", 5 ) == 0 ) {
	    if ( g2l1 > value ) ng2l1++;
	    tg2l1 = ascatime;
	     g2l1 = value;
	  }

/* G2 L2 */
	else if ( strncmp( keyword, "G2_L2", 5 ) == 0 ) {
	    if ( g2l2 > value ) ng2l2++;
	    tg2l2 = ascatime;
	     g2l2 = value;
	  }

/* G2 CPU IN */
	else if ( strncmp( keyword, "G2_CPU_I", 8 ) == 0 ) {
	    if ( g2cpuin > value ) ng2cpuin++;
	    tg2cpuin = ascatime;
	     g2cpuin = value;
	  }

/* G2 CPU OUT */
	else if ( strncmp( keyword, "G2_CPU_O", 8 ) == 0 ) {
	    if ( g2cpuout > value ) ng2cpuout++;
	    tg2cpuout = ascatime;
	     g2cpuout = value;
	  }

/* G2 Telemetry Event */
	else if ( strncmp( keyword, "G2_TELM", 8 ) == 0 ) {
	    tg2telm  = ascatime;
             g2telm += value;
	  }

/* G2 CPU Status */
        else if ( strncmp( keyword, "C2_OPR", 6 ) == 0 ) {
	    if ( buf.GIS_CPU_status[0] == 99 ) {
	        buf.GIS_CPU_status[0] = 1 - (unsigned char)value;
	      }
            else {
	        if ( buf.GIS_CPU_status[0] == 0 && value == 0 ) buf.GIS_CPU_status[0] = 1;
	        if ( buf.GIS_CPU_status[0] == 1 && value == 1 ) buf.GIS_CPU_status[0] = 0;
	      }
	  }
        else if ( strncmp( keyword, "C2_RN_ST", 8 ) == 0 ) {
	    if ( buf.GIS_CPU_status[0] == 99 ) {
	        buf.GIS_CPU_status[0] = 2 - (unsigned char)value * 2;
	      }
            else {
	        if ( buf.GIS_CPU_status[0] == 0 && value == 0 ) buf.GIS_CPU_status[0] = 2;
	        if ( buf.GIS_CPU_status[0] == 2 && value == 1 ) buf.GIS_CPU_status[0] = 0;
	      }
	  }
/* GIS Hamming Error */
	else if ( strncmp( keyword, "GIS_HAMM", 8 ) == 0 ){
	    buf.GIS_Ham_Err = (unsigned char)value;
	  }
/* G2 Temperature */
	else if ( strncmp( keyword, "G2_TEMP", 7 ) == 0 ){
	    buf.GIS_temp[0] = (float)value * 0.5459 - 47.50;
	  }

/* RBM Temperature */
	else if ( strncmp( keyword, "RBM_TEMP", 8 ) == 0 ){
	    buf.GIS_temp[2] = (float)value * 0.5459 - 45.90;
	  }

/* GIS RBM Flag */
	else if ( strncmp( keyword, "GIS_RBMF", 8 ) == 0 ){
	    buf.RBM_flg[0] = (unsigned char)value;
	  }

/* RBM count */
	else if ( strncmp( keyword, "RBM_CONT", 8 ) == 0 ) {
	    if ( rbmcount > value ) nrbmcount++;
	    trbmcount = ascatime;
	     rbmcount = value;
	  }
/* The following three items are added by K.E. */
/* HV-H2 monitor */
	else if ( strncmp( keyword, "HVH2_MON", 8 ) == 0 ) {
	     buf.GIS_HVH_monitor[0] = value * 8000.0/255.0;
	  }
/* HV-L2 monitor */
	else if ( strncmp( keyword, "HVL2_MON", 8 ) == 0 ) {
	     buf.GIS_HVL_monitor[0] = value * 1400.0/255.0;
	  }
/* HV-H2 current monitor */
	else if ( strncmp( keyword, "HVH2_CM", 8 ) == 0 ) {
	     buf.GIS_HVHCM[0] = value * 100.0/255.0;
	  }
	ngis++;
      }

/*
 * G3-HK processing
 */
    else if ( ikind == 3 ){
#ifdef DEBUG
    fprintf ( stderr, "mkf2RdctHK 4\n" );
#endif

/* G3 HVL LEVEL */
        if ( strncmp( keyword, "HVL3_POW", 8 ) == 0 ) {
	    if ( value == 0 ){
	        pGHVL3 = buf.GIS_HVL[1];
	        buf.GIS_HVL[1] = 16;
	      }
	    else {
	        buf.GIS_HVL[1] = pGHVL3;
	      }
#ifdef DEBUG
	    fprintf( stderr, "HVL3_POW, val=%d GIS_HVL[1] = %u\n", value, buf.GIS_HVL[1] );
#endif
	  }
        else if ( strncmp( keyword, "GHV3_L", 6 ) == 0 ) {
	    if ( buf.GIS_HVL[1] == 8 || buf.GIS_HVL[1] == 16 ) {
	        pGHVL3         = (unsigned char)value;
	      }
	    else {
	        buf.GIS_HVL[1] = (unsigned char)value;
	        pGHVL3         = (unsigned char)value;
	      }
#ifdef DEBUG
	    fprintf( stderr, "GHV3_L, val=%d GIS_HVL[1] = %u\n", value, buf.GIS_HVL[1] );
#endif
	  }

/* G3 HVH LEVEL */
        else if ( strncmp( keyword, "HVH3_POW", 8 ) == 0 ) {
	    if ( value == 0 ){
	        pGHVH3    = buf.GIS_HVH[1];
	        buf.GIS_HVH[1] = 16;
	      }
	    else {
	        buf.GIS_HVH[1] = pGHVH3;
	      }
#ifdef DEBUG
	    fprintf( stderr, "HVH3_POW, val=%d GIS_HVH[1] = %u\n", value, buf.GIS_HVH[1] );
#endif
	  }
        else if ( strncmp( keyword, "GHV3_H", 6 ) == 0 ) {
	    if ( buf.GIS_HVH[1] == 8  || buf.GIS_HVH[1] == 16 ) {
	        pGHVH3         = (unsigned char)value;
	      }
	    else {
	        buf.GIS_HVH[1] = (unsigned char)value;
	        pGHVH3         = (unsigned char)value;
	      }
#ifdef DEBUG
	    fprintf( stderr, "GVH3_H, val=%d GIS_HVH[1] = %u\n", value, buf.GIS_HVH[1] );
#endif
	  }

/* G3 LDHIT */
	else if ( strncmp( keyword, "G3_LDHIT", 8 ) == 0 ) {
	    if ( g3ldhit > value ) ng3ldhit++;
	    tg3ldhit = ascatime;
	     g3ldhit = value;
	  }

/* G3 H0 */
	else if ( strncmp( keyword, "G3_H0", 5 ) == 0 ) {
	    if ( g3h0 > value ) ng3h0++;
	    tg3h0 = ascatime;
	     g3h0 = value;
	  }

/* G3 H1 */
	else if ( strncmp( keyword, "G3_H1", 5 ) == 0 ) {
	    if ( g3h1 > value ) ng3h1++;
	    tg3h1 = ascatime;
	     g3h1 = value;
	  }

/* G3 H2 */
	else if ( strncmp( keyword, "G3_H2", 5 ) == 0 ) {
	    if ( g3h2 > value ) ng3h2++;
	    tg3h2 = ascatime;
	     g3h2 = value;
	  }

/* G3 L0 */
	else if ( strncmp( keyword, "G3_L0", 5 ) == 0 ) {
	    if ( g3l0 > value ) ng3l0++;
	    tg3l0 = ascatime;
	     g3l0 = value;
	  }

/* G3 L1 */
	else if ( strncmp( keyword, "G3_L1", 5 ) == 0 ) {
	    if ( g3l1 > value ) ng3l1++;
	    tg3l1 = ascatime;
	     g3l1 = value;
	  }

/* G3 L2 */
	else if ( strncmp( keyword, "G3_L2", 5 ) == 0 ) {
	    if ( g3l2 > value ) ng3l2++;
	    tg3l2 = ascatime;
	     g3l2 = value;
	  }

/* G3 CPU IN */
	else if ( strncmp( keyword, "G3_CPU_I", 8 ) == 0 ) {
	    if ( g3cpuin > value ) ng3cpuin++;
	    tg3cpuin = ascatime;
	     g3cpuin = value;
	  }

/* G3 CPU OUT */
	else if ( strncmp( keyword, "G3_CPU_O", 8 ) == 0 ) {
	    if ( g3cpuout > value ) ng3cpuout++;
	    tg3cpuout = ascatime;
	     g3cpuout = value;
	  }

/* G3 Telemetry Event */
	else if ( strncmp( keyword, "G3_TELM", 8 ) == 0 ) {
	    tg3telm  = ascatime;
             g3telm += value;
	  }

/* G3 CPU Status */
        else if ( strncmp( keyword, "C3_OPR", 6 ) == 0 ) {
	    if ( buf.GIS_CPU_status[1] == 99 ) {
	        buf.GIS_CPU_status[1] = 1 - (unsigned char)value;
	      }
            else {
	        if ( buf.GIS_CPU_status[1] == 0 && value == 0 ) buf.GIS_CPU_status[1] = 1;
	        if ( buf.GIS_CPU_status[1] == 1 && value == 1 ) buf.GIS_CPU_status[1] = 0;
	      }
	  }
        else if ( strncmp( keyword, "C3_RN_ST", 8 ) == 0 ) {
	    if ( buf.GIS_CPU_status[1] == 99 ) {
	        buf.GIS_CPU_status[1] = 2 - (unsigned char)value * 2;
	      }
            else {
	        if ( buf.GIS_CPU_status[1] == 0 && value == 0 ) buf.GIS_CPU_status[1] = 2;
	        if ( buf.GIS_CPU_status[1] == 2 && value == 1 ) buf.GIS_CPU_status[1] = 0;
	      }
	  }

/* G3 Temperature */
	else if ( strncmp( keyword, "G3_TEMP", 7 ) == 0 ){
	    buf.GIS_temp[1] = (float)value * 0.5459 - 47.50;
	  }

/* The following three items are added by K.E. */
/* HV-H3 monitor */
	else if ( strncmp( keyword, "HVH3_MON", 8 ) == 0 ) {
	     buf.GIS_HVH_monitor[1] = value * 8000.0/255.0;
	  }
/* HV-L3 monitor */
	else if ( strncmp( keyword, "HVL3_MON", 8 ) == 0 ) {
	     buf.GIS_HVL_monitor[1] = value * 1400.0/255.0;
	  }
/* HV-H3 current monitor */
	else if ( strncmp( keyword, "HVH3_CM", 8 ) == 0 ) {
	     buf.GIS_HVHCM[1] = value * 100.0/255.0;
	  }
	ngis++;
      }


/* bin end process */
    else if ( ikind == 9 ){
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK:ikind = %d\n", ikind );
#endif
      if ( nsis == 0 ){
/* No SIS HK records found during this bin; reinitialize and increment time counters */
#ifdef DEBUG
          fprintf ( stderr, "mkf2RdctHK:call mkf2InitSISRecd\n" );
#endif
	  mkf2InitSISRecd( &buf );
	  for ( i = 0; i < 8; i++) {
              tSISevnt[i] += recd->width; tSISevntp[i] = tSISevnt[i]; cSISevnt[i] = 0;
              tSISpixl[i] += recd->width; tSISpixlp[i] = tSISpixl[i]; cSISpixl[i] = 0;
              tSIStelm[i] += recd->width; tSIStelmp[i] = tSIStelm[i]; cSIStelm[i] = 0;
	    }
	}
      else {
/* SIS HK records were found */
/* SIS Event Number */
#ifdef DEBUG
        fprintf ( stderr, "mkf2RdctHK: SIS Event Number\n" );
#endif
        for ( i = 0; i < 8; i++ ){
/*	    if ( tSISevntp[i] == tSISevnt[i] ){ */
/*                tSISevnt[i]  += recd->width; */
/*                tSISevntp[i]  = tSISevnt[i]; */
	    if ( cSISevnt[i] == 0 ){
                buf.SIS_event_No[i] = -99.999;
              }
            else {
                buf.SIS_event_No[i] = (float)SISevnt[i] / (float)( tSISevnt[i] - tSISevntp[i] );
                tSISevntp[i] = tSISevnt[i];
		SISevnt[i] = 0; cSISevnt[i] = 0;
              }
	  }

/* SIS Pixl Number */
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK: SIS Pixl Number\n" );
#endif
        for ( i = 0; i < 8; i++ ){
/*	    if ( tSISpixlp[i] == tSISpixl[i] ){ */
#ifdef DEBUG
                if ( i == 0 ){
                    fprintf( stderr, "S0C0 PIXL: %12.3lf %12.3lf %10.3lf  %5.2f\n", tSISpixl[i], tSISpixlp[i], tSISpixl[i]-tSISpixlp[i], buf.SIS_pixel_No[i] );
	          }
#endif
/*                tSISpixl[i]  += recd->width; */
/*                tSISpixlp[i]  = tSISpixl[i]; */
	    if ( cSISpixl[i] == 0 ){
                buf.SIS_pixel_No[i] = -99.999;
              }
            else {
                buf.SIS_pixel_No[i] = (float)SISpixl[i] / (float)( tSISpixl[i] - tSISpixlp[i] );
#ifdef DEBUG
                if ( i == 0 ){
                    fprintf( stderr, "S0C0 PIXL: %12.3lf %12.3lf %10.3lf  %5.2f\n", tSISpixl[i], tSISpixlp[i], tSISpixl[i]-tSISpixlp[i], buf.SIS_pixel_No[i] );
	          }
#endif
                tSISpixlp[i] = tSISpixl[i];
		SISpixl[i] = 0; cSISpixl[i] = 0;
              }
	  }

/* SIS Telm Event Number */
#ifdef DEBUG
        fprintf ( stderr, "mkf2RdctHK: SIS Telm Event Number\n" );
#endif
        for ( i = 0; i < 8; i++ ){
/*	    if ( tSIStelmp[i] == tSIStelm[i] ){ */
/*                tSIStelm[i]  += recd->width; */
/*                tSIStelmp[i]  = tSIStelm[i]; */
	    if ( cSIStelm[i] == 0 ){
                buf.SIS_tlm_event[i] = -99.999;
              } /* end of case where Sn_TELMm was not found */
            else {
                buf.SIS_tlm_event[i] = (float)SIStelm[i] / (float)( tSIStelm[i] - tSIStelmp[i] );
                if( i < 4 ) {
                  thismode = buf.SIS_obs_mode[0];
		  }
		else {
		  thismode = buf.SIS_obs_mode[1];
		  } /* End of thismode if block */
		if ( redo_satf ) {
		  if( buf.Bit_Rate < 3 && thismode < 3 ){
		    if( SIStelm[i] < cSIStelm[i] * SIStlimit[buf.Bit_Rate][thismode] ) {
		      buf.SIS_satf[i] = 0;
                    }
		    else {
		      buf.SIS_satf[i] = 1;
                    }
		  } /* End of meaningfull satf determination */
		  else {
		    buf.SIS_satf[i] = 99;
                  }
		}
                tSIStelmp[i] = tSIStelm[i];
		SIStelm[i] = 0; cSIStelm[i] = 0;
              } /* End of case where Sn_TELMm was found */
	  } /* End of for block on i (0--8) on TELM */
      } /* End if if block on nsis; now done all SIS parameters */
      nsis = 0;

/* GIS moniter count rate */
      if ( ngis == 0 ){
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK: call mkf2InitGISRecd\n" );
#endif
	  mkf2InitGISRecd( &buf );
	    tg2ldhit += recd->width;  tg2ldhitp = tg2ldhit;
	    tg2h0     += recd->width; tg2h0p     = tg2h0;
	    tg2h1     += recd->width; tg2h1p     = tg2h1;
	    tg2h2     += recd->width; tg2h2p     = tg2h2;
	    tg2l0     += recd->width; tg2l0p     = tg2l0;
	    tg2l1     += recd->width; tg2l1p     = tg2l1;
	    tg2l2     += recd->width; tg2l2p     = tg2l2;
	    tg2cpuin  += recd->width; tg2cpuinp  = tg2cpuin;
	    tg2cpuout += recd->width; tg2cpuoutp = tg2cpuout;
	    tg2telm   += recd->width; tg2telmp   = tg2telm;
	    tg3ldhit  += recd->width; tg3ldhitp  = tg2ldhit;
	    tg3h0     += recd->width; tg3h0p     = tg3h0;
	    tg3h1     += recd->width; tg3h1p     = tg3h1;
	    tg3h2     += recd->width; tg3h2p     = tg3h2;
	    tg3l0     += recd->width; tg3l0p     = tg3l0;
	    tg3l1     += recd->width; tg3l1p     = tg3l1;
	    tg3l2     += recd->width; tg3l2p     = tg3l2;
	    tg3cpuin  += recd->width; tg3cpuinp  = tg3cpuin;
	    tg3cpuout += recd->width; tg3cpuoutp = tg3cpuout;
	    tg3telm   += recd->width; tg3telmp   = tg3telm;
        }
      else {
/* LDHIT */
#ifdef DEBUG
        fprintf ( stderr, "mkf2RdctHK: LDHIT\n" );
#endif
	if ( tg2ldhit == tg2ldhitp ){
	    tg2ldhit += recd->width;
	    tg2ldhitp = tg2ldhit;
	    buf.GIS_LDHIT[0] = -99.999;
#ifdef DEBUG
	    fprintf( stderr, "G2 LDHIT TIME: %12.3lf %12.3lf %10.3lf  %5.2f\n", tg2ldhit, tg2ldhitp, tg2ldhit - tg2ldhitp, buf.GIS_LDHIT[0] );
#endif
	  }
	else {
	    buf.GIS_LDHIT[0] = (float)( g2ldhit - g2ldhitp + ng2ldhit * 256 ) / (float)( tg2ldhit - tg2ldhitp );
#ifdef DEBUG
	    fprintf( stderr, "G2 LDHIT TIME: %12.3lf %12.3lf %10.3lf  %5.2f\n", tg2ldhit, tg2ldhitp, tg2ldhit - tg2ldhitp, buf.GIS_LDHIT[0] );
#endif
	    tg2ldhitp = tg2ldhit;
	     g2ldhitp =  g2ldhit;
	  }
	if ( tg3ldhit == tg3ldhitp ){
	    tg3ldhit += recd->width;
	    tg3ldhitp = tg3ldhit;
	    buf.GIS_LDHIT[1] = -99.999;
	  }
	else {
	    buf.GIS_LDHIT[1] = (float)( g3ldhit - g3ldhitp + ng3ldhit * 256 ) / (float)( tg3ldhit - tg3ldhitp );
	    tg3ldhitp = tg3ldhit;
	     g3ldhitp =  g3ldhit;
	  }
	ng2ldhit = 0;
	ng3ldhit = 0;

/* H0 */
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK: H0\n" );
#endif
	if ( tg2h0 == tg2h0p ){
	    tg2h0  += recd->width;
	    tg2h0p  = tg2h0;
	    buf.GIS_H0[0] = -99.999;
	  }
	else {
	    buf.GIS_H0[0] = (float)( g2h0 - g2h0p + ng2h0 * 256 ) / (float)( tg2h0 - tg2h0p );
	    tg2h0p = tg2h0;
	     g2h0p =  g2h0;
	  }
	if ( tg3h0 == tg3h0p ){
	    tg3h0 += recd->width;
	    tg3h0p = tg3h0;
	    buf.GIS_H0[1] = -99.999;
	  }
	else {
	    buf.GIS_H0[1] = (float)( g3h0 - g3h0p + ng3h0 * 256 ) / (float)( tg3h0 - tg3h0p );
	     g3h0p =  g3h0;
	    tg3h0p = tg3h0;
	  }
	ng2h0 = 0;
	ng3h0 = 0;

/* H1 */
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK: H1\n" );
#endif
	if ( tg2h1 == tg2h1p ){
	    tg2h1  += recd->width;
	    tg2h1p  = tg2h1;
	    buf.GIS_H1[0] = -99.999;
	  }
	else {
	    buf.GIS_H1[0] = (float)( g2h1 - g2h1p + ng2h1 * 256 ) / (float)( tg2h1 - tg2h1p );
	     g2h1p =  g2h1;
	    tg2h1p = tg2h1;
	  }
	if ( tg3h1 == tg3h1p ){
	    tg3h1  += recd->width;
	    tg3h1p  = tg3h1;
	    buf.GIS_H1[1] = -99.999;
	  }
	else {
	    buf.GIS_H1[1] = (float)( g3h1 - g3h1p + ng3h1 * 256 ) / (float)( tg3h1 - tg3h1p );
	    tg3h1p = tg3h1;
	     g3h1p =  g3h1;
	  }
	ng2h1 = 0;
	ng3h1 = 0;

/* H2 */
#ifdef DEBUG
        fprintf ( stderr, "mkf2RdctHK: H2\n" );
#endif
	if ( tg2h2 == tg2h2p ){
	    tg2h2  += recd->width;
	    tg2h2p  = tg2h2;
	    buf.GIS_H2[0] = -99.999;
	  }
	else {
	    buf.GIS_H2[0] = (float)( g2h2 - g2h2p + ng2h2 * 256 ) / (float)( tg2h2 - tg2h2p );
	    tg2h2p = tg2h2;
	     g2h2p =  g2h2;
	  }
	if ( tg3h2 == tg3h2p ){
	    tg3h2  += recd->width;
	    tg3h2p  = tg3h2;
	    buf.GIS_H2[1] = -99.999;
	  }
	else {
	    buf.GIS_H2[1] = (float)( g3h2 - g3h2p + ng3h2 * 256 ) / (float)( tg3h2 - tg3h2p );
	     g3h2p =  g3h2;
	    tg3h2p = tg3h2;
	  }
	ng2h2 = 0;
	ng3h2 = 0;

/* L0 */
#ifdef DEBUG
        fprintf ( stderr, "mkf2RdctHK: L0\n" );
#endif
	if ( tg2l0 == tg2l0p ){
	    tg2l0  += recd->width;
	    tg2l0p  = tg2l0;
	    buf.GIS_L0[0] = -99.999;
	  }
	else {
	    buf.GIS_L0[0] = (float)( g2l0 - g2l0p + ng2l0 * 256 ) / (float)( tg2l0 - tg2l0p );
	     g2l0p =  g2l0;
	    tg2l0p = tg2l0;
	  }
	if ( tg3l0 == tg3l0p ){
	    tg3l0  += recd->width;
	    tg3l0p  = tg3l0;
	    buf.GIS_L0[1] = -99.999;
	  }
	else {
	    buf.GIS_L0[1] = (float)( g3l0 - g3l0p + ng3l0 * 256 ) / (float)( tg3l0 - tg3l0p );
	     g3l0p =  g3l0;
	    tg3l0p = tg3l0;
	  }
	ng2l0 = 0;
	ng3l0 = 0;

/* L1 */
#ifdef DEBUG
        fprintf ( stderr, "mkf2RdctHK: L1\n" );
#endif
	if ( tg2l1 == tg2l1p ){
	    tg2l1  += recd->width;
	    tg2l1p  = tg2l1;
	    buf.GIS_L1[0] = -99.999;
	  }
	else {
	    buf.GIS_L1[0] = (float)( g2l1 -g2l1p + ng2l1 * 256 ) / (float)( tg2l1 - tg2l1p );
	     g2l1p =  g2l1;
	    tg2l1p = tg2l1;
	  }
	if ( tg3l1 == tg3l1p ){
	    tg3l1  += recd->width;
	    tg3l1p  = tg3l1;
	    buf.GIS_L1[1] = -99.999;
	  }
	else {
	    buf.GIS_L1[1] = (float)( g3l1 - g3l1p + ng3l1 * 256 ) / (float)( tg3l1 - tg3l1p );
	     g3l1p =  g3l1;
	    tg3l1p = tg3l1;
	  }
	ng2l1 = 0;
	ng3l1 = 0;

/* L2 */
#ifdef DEBUG
        fprintf ( stderr, "mkf2RdctHK: L2\n" );
#endif
	if ( tg2l2 == tg2l2p ){
	    tg2l2  += recd->width;
	    tg2l2p  = tg2l2;
	    buf.GIS_L2[0] = -99.999;
	  }
	else {
	    buf.GIS_L2[0] = (float)( g2l2 - g2l2p + ng2l2 * 256 ) / (float)( tg2l2 - tg2l2p );
	     g2l2p =  g2l2;
	    tg2l2p = tg2l2;
	  }
	if ( tg3l2 == tg3l2p ){
	    tg3l2  += recd->width;
	    tg3l2p  = tg3l2;
	    buf.GIS_L2[1] = -99.999;
	  }
	else {
	    buf.GIS_L2[1] = (float)( g3l2 - g3l2p + ng3l2 * 256 ) / (float)( tg3l2 - tg3l2p );
	     g3l2p =  g3l2;
	    tg3l2p = tg3l2;
	  }
	ng2l2 = 0;
	ng3l2 = 0;

/* CPU IN */
#ifdef DEBUG
        fprintf ( stderr, "mkf2RdctHK: CPUIN\n" );
#endif
	if ( tg2cpuin == tg2cpuinp ){
	    tg2cpuin  += recd->width;
	    tg2cpuinp  = tg2cpuin;
	    buf.GIS_CPU_in[0] = -99.999;
	  }
	else {
	    buf.GIS_CPU_in[0] = (float)( g2cpuin - g2cpuinp + ng2cpuin * 256 ) / (float)( tg2cpuin - tg2cpuinp );
	     g2cpuinp =  g2cpuin;
	    tg2cpuinp = tg2cpuin;
	  }
	if ( tg3cpuin == tg3cpuinp ){
	    tg3cpuin  += recd->width;
	    tg3cpuinp  = tg3cpuin;
	    buf.GIS_CPU_in[1] = -99.999;
	  }
	else {
	    buf.GIS_CPU_in[1] = (float)( g3cpuin - g3cpuinp + ng3cpuin * 256 ) / (float)( tg3cpuin - tg3cpuinp );
	     g3cpuinp =  g3cpuin;
	    tg3cpuinp = tg3cpuin;
	  }
	ng2cpuin = 0;
	ng3cpuin = 0;

/* CPU OUT */
#ifdef DEBUG
        fprintf ( stderr, "mkf2RdctHK: CPUOUT\n" );
#endif
	if ( tg2cpuout == tg2cpuoutp ){
	    tg2cpuout  += recd->width;
	    tg2cpuoutp  = tg2cpuout;
	    buf.GIS_CPU_out[0] = -99.999;
	  }
	else {
	    buf.GIS_CPU_out[0] = (float)( g2cpuout - g2cpuoutp + ng2cpuout * 256 ) / (float)( tg2cpuout - tg2cpuoutp );
	     g2cpuoutp =  g2cpuout;
	    tg2cpuoutp = tg2cpuout;
	  }
	if ( tg3cpuout == tg3cpuoutp ){
	    tg3cpuout  += recd->width;
	    tg3cpuoutp  = tg3cpuout;
	    buf.GIS_CPU_out[1] = -99.999;
	  }
	else {
	    buf.GIS_CPU_out[1] = (float)( g3cpuout - g3cpuoutp + ng3cpuout * 256 ) / (float)( tg3cpuout - tg3cpuoutp );
	     g3cpuoutp =  g3cpuout;
	    tg3cpuoutp = tg3cpuout;
	  }
	ng2cpuout = 0;
	ng3cpuout = 0;

/* Telemetry Event */
#ifdef DEBUG
            fprintf ( stderr, "mkf2RdctHK: GIS TELM Event\n" );
#endif
	if ( tg2telm == tg2telmp ){
	    tg2telm  += recd->width;
	    tg2telmp  = tg2telm;
	    buf.GIS_tlm_event[0] = -99.999;
	  }
	else {
	    buf.GIS_tlm_event[0] = (float)g2telm / (float)( tg2telm - tg2telmp );
	    tg2telmp = tg2telm;
	  }
	if ( tg3telm == tg3telmp ){
	    tg3telm  += recd->width;
	    tg3telmp  = tg3telm;
	    buf.GIS_tlm_event[1] = -99.999;
	  }
	else {
	    buf.GIS_tlm_event[1] = (float)g3telm / (float)( tg3telm - tg3telmp );
	    tg3telmp = tg3telm;
	  }
	g2telm = 0;
	g3telm = 0;

/* RBM Count */
#ifdef DEBUG
        fprintf ( stderr, "mkf2RdctHK: RBM_COUNT\n" );
#endif
	if ( trbmcount == trbmcountp ){
	    trbmcount  += recd->width;
	    trbmcountp  = trbmcount;
	    buf.RBM_count = -99.999;
	  }
	else {
	    buf.RBM_count = (float)( rbmcount - rbmcountp + nrbmcount * 65536 ) / (float)( trbmcount - trbmcountp );
	     rbmcountp =  rbmcount;
	    trbmcountp = trbmcount;
	  }
	nrbmcount = 0;
      }
#ifdef DEBUG
      fprintf ( stderr, "mkf2RdctHK: reset ngis\n" );
#endif
      ngis = 0;
      nbin++;
    }

/* Terminate */
#ifdef DEBUG
    fprintf ( stderr, "mkf2RdctHK: copy record\n" );
#endif
    *recd = buf;
#ifdef DEBUG
    fprintf( stderr, "mkf2RdctHK: termination\n" );
#endif
    return;
}
