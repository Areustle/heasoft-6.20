#include   <stdio.h>
#include   "mkfilter.h"
#include   "mkf2_recd.h"
#include   "faio.h"

/* DEBUG
#define DEBUG
 */

int mkf2Put2Fits( int funit, MkfRECD recd ){

    static int      frow = 0, felem = 1, nelements = 1;
    int             iret, colnum, idata;
    unsigned char   ucdata;
    float           fdata;
    double          mjd, ascatime, ddata;
    AtTime          t;

    iret = 0;
    colnum = 1;
    frow++;

/* Put elements into an ASCII or binary table column (in the CDU).
 *
 * GIS_HVH_monitor, GIS_HVL_monitor, GIS_HVHCM added.  Ken Ebisawa 09/20/94 
 */
    FTPCLD( funit, colnum, frow, felem, nelements, recd.ascatime,          iret ); colnum++; /*  1 */
    FTPCLS( funit, colnum, frow, felem, nelements, recd.start,             iret ); colnum++; /*  2 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.width,             iret ); colnum++; /*  3 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.Bit_Rate,          iret ); colnum++; /*  4 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.ACS,               iret ); colnum++; /*  5 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.NSAS,              iret ); colnum++; /*  6 */
    fdata = recd.Euler[0];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /*  7 */
    fdata = recd.Euler[1];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /*  8 */
    fdata = recd.Euler[2];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /*  9 */
    fdata = recd.Sat_Pos.r;
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 10 */
    fdata = recd.Sat_Pos.lon;
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 11 */
    fdata = recd.Sat_Pos.lat;
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 12 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.elv,               iret ); colnum++; /* 13 */        
    FTPCLE( funit, colnum, frow, felem, nelements, recd.COR,               iret ); colnum++; /* 14 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.FOV,               iret ); colnum++; /* 15 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.Bright_Earth,      iret ); colnum++; /* 16 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.SAA,               iret ); colnum++; /* 17 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.T_SAA,             iret ); colnum++; /* 18 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.SUNSHINE,          iret ); colnum++; /* 19 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.T_DY_NT,           iret ); colnum++; /* 20 */
    ucdata = recd.SIS_obs_mode[0];
    FTPCLB( funit, colnum, frow, felem, nelements, ucdata,                 iret ); colnum++; /* 21 */
    ucdata = recd.SIS_obs_mode[1];
    FTPCLB( funit, colnum, frow, felem, nelements, ucdata,                 iret ); colnum++; /* 22 */
    idata = recd.SIS_ID[0];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 23 */
    idata = recd.SIS_ID[1];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 24 */
    ucdata = recd.SIS_dscr[0];
    FTPCLB( funit, colnum, frow, felem, nelements, ucdata,                 iret ); colnum++; /* 25 */
    ucdata = recd.SIS_dscr[1];
    FTPCLB( funit, colnum, frow, felem, nelements, ucdata,                 iret ); colnum++; /* 26 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.SIS_add_dscr,      iret ); colnum++; /* 27 */
    ucdata = recd.SIS_Grade[0];
    FTPCLB( funit, colnum, frow, felem, nelements, ucdata,                 iret ); colnum++; /* 28 */
    ucdata = recd.SIS_Grade[1];
    FTPCLB( funit, colnum, frow, felem, nelements, ucdata,                 iret ); colnum++; /* 29 */
    idata = recd.SIS_Event_thr[0];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 30 */
    idata = recd.SIS_Event_thr[1];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 31 */
    idata = recd.SIS_Event_thr[2];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 32 */
    idata = recd.SIS_Event_thr[3];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 33 */
    idata = recd.SIS_Event_thr[4];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 34 */
    idata = recd.SIS_Event_thr[5];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 35 */
    idata = recd.SIS_Event_thr[6];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 36 */
    idata = recd.SIS_Event_thr[7];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 37 */
    idata = recd.SIS_Split_thr[0];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 38 */
    idata = recd.SIS_Split_thr[1];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 39 */
    idata = recd.SIS_Split_thr[2];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 40 */
    idata = recd.SIS_Split_thr[3];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 41 */
    idata = recd.SIS_Split_thr[4];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 42 */
    idata = recd.SIS_Split_thr[5];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 43 */
    idata = recd.SIS_Split_thr[6];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 44 */
    idata = recd.SIS_Split_thr[7];
    FTPCLJ( funit, colnum, frow, felem, nelements, idata,                  iret ); colnum++; /* 45 */
    ucdata = recd.SIS_AE[0];
    FTPCLB( funit, colnum, frow, felem, nelements, ucdata,                 iret ); colnum++; /* 46 */
    ucdata = recd.SIS_AE[1];
    FTPCLB( funit, colnum, frow, felem, nelements, ucdata,                 iret ); colnum++; /* 47 */
    fdata = recd.SIS_temp[0];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 48 */
    fdata = recd.SIS_temp[1];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 49 */
    fdata = recd.SIS_event_No[0];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 50 */
    fdata = recd.SIS_event_No[1];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 51 */
    fdata = recd.SIS_event_No[2];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 52 */
    fdata = recd.SIS_event_No[3];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 53 */
    fdata = recd.SIS_event_No[4];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 54 */
    fdata = recd.SIS_event_No[5];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 55 */
    fdata = recd.SIS_event_No[6];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 56 */
    fdata = recd.SIS_event_No[7];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 57 */
    fdata = recd.SIS_pixel_No[0];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 58 */
    fdata = recd.SIS_pixel_No[1];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 59 */
    fdata = recd.SIS_pixel_No[2];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 60 */
    fdata = recd.SIS_pixel_No[3];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 61 */
    fdata = recd.SIS_pixel_No[4];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 62 */
    fdata = recd.SIS_pixel_No[5];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 63 */
    fdata = recd.SIS_pixel_No[6];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 64 */
    fdata = recd.SIS_pixel_No[7];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 65 */
    fdata = recd.SIS_tlm_event[0];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 66 */
    fdata = recd.SIS_tlm_event[1];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 67 */
    fdata = recd.SIS_tlm_event[2];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 68 */
    fdata = recd.SIS_tlm_event[3];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 69 */
    fdata = recd.SIS_tlm_event[4];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 70 */
    fdata = recd.SIS_tlm_event[5];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 71 */
    fdata = recd.SIS_tlm_event[6];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 72 */
    fdata = recd.SIS_tlm_event[7];
    FTPCLE( funit, colnum, frow, felem, nelements, fdata,                  iret ); colnum++; /* 73 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.SIS_satf[0],       iret ); colnum++; /* 74 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.SIS_satf[1],       iret ); colnum++; /* 75 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.SIS_satf[2],       iret ); colnum++; /* 76 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.SIS_satf[3],       iret ); colnum++; /* 77 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.SIS_satf[4],       iret ); colnum++; /* 78 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.SIS_satf[5],       iret ); colnum++; /* 79 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.SIS_satf[6],       iret ); colnum++; /* 80 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.SIS_satf[7],       iret ); colnum=90; /* 81 */
                    /* column 82-89 to be left blank (SIS DFE values, to be filled in later if possible */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.GIS_obs_mode,      iret ); colnum++; /* 90 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.GIS_HVL[0],        iret ); colnum++; /* 91 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.GIS_HVL[1],        iret ); colnum++; /* 92 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.GIS_HVH[0],        iret ); colnum++; /* 93 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.GIS_HVH[1],        iret ); colnum++; /* 94 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_LDHIT[0],      iret ); colnum++; /* 95 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_LDHIT[1],      iret ); colnum++; /* 96 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_H0[0],         iret ); colnum++; /* 97 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_H0[1],         iret ); colnum++; /* 98 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_H1[0],         iret ); colnum++; /* 99 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_H1[1],         iret ); colnum++; /* 100 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_H2[0],         iret ); colnum++; /* 101 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_H2[1],         iret ); colnum++; /* 102 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_L0[0],         iret ); colnum++; /* 103 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_L0[1],         iret ); colnum++; /* 104 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_L1[0],         iret ); colnum++; /* 105 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_L1[1],         iret ); colnum++; /* 106 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_L2[0],         iret ); colnum++; /* 107 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_L2[1],         iret ); colnum++; /* 108 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_CPU_in[0],     iret ); colnum++; /* 109 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_CPU_in[1],     iret ); colnum++; /* 110 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_CPU_out[0],    iret ); colnum++; /* 111 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_CPU_out[1],    iret ); colnum++; /* 112 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_tlm_event[0],  iret ); colnum++; /* 113 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_tlm_event[1],  iret ); colnum++; /* 114 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.GIS_CPU_status[0], iret ); colnum++; /* 115 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.GIS_CPU_status[1], iret ); colnum++; /* 116 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.GIS_Ham_Err,       iret ); colnum++; /* 117 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_temp[0],       iret ); colnum++; /* 118 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_temp[1],       iret ); colnum++; /* 119 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_HVH_monitor[0],iret ); colnum++; /* 120 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_HVL_monitor[0],iret ); colnum++; /* 121 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_HVHCM[0],      iret ); colnum++; /* 122 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_HVH_monitor[1],iret ); colnum++; /* 123 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_HVL_monitor[1],iret ); colnum++; /* 124 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_HVHCM[1],      iret ); colnum=128; /* 125 */
    /* colunum = 126/127 are reserved for the GIS2/3 Dead Time */ 
    FTPCLE( funit, colnum, frow, felem, nelements, recd.GIS_temp[2],       iret ); colnum++; /* 128 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.RBM_flg[0],        iret ); colnum++; /* 129 */
    FTPCLB( funit, colnum, frow, felem, nelements, recd.RBM_flg[1],        iret ); colnum++; /* 130 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.RBM_count,         iret ); colnum++; /* 131 */
    FTPCLJ( funit, colnum, frow, felem, nelements, recd.ETI,               iret ); colnum++; /* 132 */
    FTPCLE( funit, colnum, frow, felem, nelements, recd.ANG_DIST,          iret );           /* 133 */

    if ( iret == 0 )  return( frow );
    else              return( -1 * iret );
  }


