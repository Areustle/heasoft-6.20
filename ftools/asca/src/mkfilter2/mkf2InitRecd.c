/*
 *  int mkf2InitRecd( MkfRECD *buf )
 *
 *  Ver 1.0  Mar.22,1994  by T.Takeshima
 *  Ver 1.1  Apr.27, 1994 by T.Takeshima
 */

#include   <stdio.h>
#include   <string.h>

#include   "mkfilter.h"
#include   "mkf2_recd.h"
#include   "faio.h"
/*
#define DEBUG
 */

int mkf2InitRecd( MkfRECD *work ){

/*
 *  Initialize static struct <buf>
 */
#ifdef DEBUG
    fprintf ( stderr, "In mkf2InitRecd 0\n" );
#endif
    work->ascatime         =  0.0;
    strcpy( work->start, "???????????????????????" );
    work->width            = -99.999;
    work->Bit_Rate         =  99;
    work->ACS              =  99;
    work->NSAS             = -99.999;
    work->Euler[0]         = -99.999;
    work->Euler[1]         = -99.999;
    work->Euler[2]         = -99.999;
    work->elv              = -99.999;
    work->COR              = -99.999;
    work->FOV              =  99;
    work->Bright_Earth     = -99.999;
    work->T_SAA            = -99.999;
    work->T_DY_NT          = -99.999;
    work->SUNSHINE         =  99;
#ifdef DEBUG
    fprintf ( stderr, "In mkf2InitRecd 1\n" );
#endif
    work->SAA              =  99;
    work->SIS_obs_mode[0]  =  99;
    work->SIS_obs_mode[1]  =  99;
    work->SIS_ID[0]        = -99;
    work->SIS_ID[1]        = -99;
    work->SIS_dscr[0]      =  99;
    work->SIS_dscr[1]      =  99;
    work->SIS_add_dscr     =  99;
    work->SIS_Grade[0]     =  99;
    work->SIS_Grade[1]     =  99;
#ifdef DEBUG
    fprintf ( stderr, "In mkf2InitRecd 2\n" );
#endif
    work->SIS_Event_thr[0] = -99;
    work->SIS_Event_thr[1] = -99;
    work->SIS_Event_thr[2] = -99;
    work->SIS_Event_thr[3] = -99;
    work->SIS_Event_thr[4] = -99;
    work->SIS_Event_thr[5] = -99;
    work->SIS_Event_thr[6] = -99;
    work->SIS_Event_thr[7] = -99;
    work->SIS_Split_thr[0] = -99;
    work->SIS_Split_thr[1] = -99;
    work->SIS_Split_thr[2] = -99;
    work->SIS_Split_thr[3] = -99;
    work->SIS_Split_thr[4] = -99;
    work->SIS_Split_thr[5] = -99;
    work->SIS_Split_thr[6] = -99;
    work->SIS_Split_thr[7] = -99;
#ifdef DEBUG
    fprintf ( stderr, "In mkf2InitRecd 3\n" );
#endif
    work->SIS_AE[0]        =  99;
    work->SIS_temp[0]      = -99.999;
    work->SIS_temp[1]      = -99.999;
    work->SIS_event_No[0]  = -99.999;
    work->SIS_event_No[1]  = -99.999;
    work->SIS_event_No[2]  = -99.999;
    work->SIS_event_No[3]  = -99.999;
    work->SIS_event_No[4]  = -99.999;
    work->SIS_event_No[5]  = -99.999;
    work->SIS_event_No[6]  = -99.999;
    work->SIS_event_No[7]  = -99.999;
    work->SIS_pixel_No[0]  = -99.999;
    work->SIS_pixel_No[1]  = -99.999;
    work->SIS_pixel_No[2]  = -99.999;
    work->SIS_pixel_No[3]  = -99.999;
    work->SIS_pixel_No[4]  = -99.999;
    work->SIS_pixel_No[5]  = -99.999;
    work->SIS_pixel_No[6]  = -99.999;
    work->SIS_pixel_No[7]  = -99.999;
#ifdef DEBUG
    fprintf ( stderr, "In mkf2InitRecd 4\n" );
#endif
    work->SIS_tlm_event[0] = -99.999;
    work->SIS_tlm_event[1] = -99.999;
    work->SIS_tlm_event[2] = -99.999;
    work->SIS_tlm_event[3] = -99.999;
    work->SIS_tlm_event[4] = -99.999;
    work->SIS_tlm_event[5] = -99.999;
    work->SIS_tlm_event[6] = -99.999;
    work->SIS_tlm_event[7] = -99.999;
    work->SIS_satf[0] = 99;
    work->SIS_satf[1] = 99;
    work->SIS_satf[2] = 99;
    work->SIS_satf[3] = 99;
    work->SIS_satf[4] = 99;
    work->SIS_satf[5] = 99;
    work->SIS_satf[6] = 99;
    work->SIS_satf[7] = 99;
    work->GIS_obs_mode     =  99;
    work->GIS_HVL[0]       =  99;
    work->GIS_HVL[1]       =  99;
    work->GIS_HVH[0]       =  99;
    work->GIS_HVH[1]       =  99;
    work->GIS_LDHIT[0]     = -99.999;
    work->GIS_LDHIT[1]     = -99.999;
    work->GIS_H0[0]        = -99.999;
    work->GIS_H0[1]        = -99.999;
    work->GIS_H1[0]        = -99.999;
    work->GIS_H1[1]        = -99.999;
    work->GIS_H2[0]        = -99.999;
    work->GIS_H2[1]        = -99.999;
    work->GIS_L0[0]        = -99.999;
    work->GIS_L0[1]        = -99.999;
    work->GIS_L1[0]        = -99.999;
    work->GIS_L1[1]        = -99.999;
    work->GIS_L2[0]        = -99.999;
    work->GIS_L2[1]        = -99.999;
#ifdef DEBUG
    fprintf ( stderr, "In mkf2InitRecd 5\n" );
#endif
    work->GIS_CPU_in[0]    = -99.999;
    work->GIS_CPU_in[1]    = -99.999;
    work->GIS_CPU_out[0]   = -99.999;
    work->GIS_CPU_out[1]   = -99.999;
    work->GIS_tlm_event[0] = -99.999;
    work->GIS_tlm_event[1] = -99.999;
    work->GIS_CPU_status[0] = 99;
    work->GIS_CPU_status[1] = 99;
    work->GIS_Ham_Err      =  99;
    work->GIS_temp[0]     = -99.999;
    work->GIS_temp[1]     = -99.999;
    work->GIS_temp[2]     = -99.999;
    work->RBM_flg[0]      =  99;
    work->RBM_flg[1]      =  99;
    work->RBM_count       = -99.999;
#ifdef DEBUG
    fprintf ( stderr, "In mkf2InitRecd 6\n" );
#endif
    return 0;
  }
