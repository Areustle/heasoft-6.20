/* 
 *  int mkf2InitSISRdct( MkfRECD *buf )
 *
 *        Fuction to Initialize MkfRECD
 *        called from mkfMakeSFDump -- mkfRdcdSF 
 *
 *  Ver 1.0  Mar. 21, 1994  by T.Takeshima
 */

#include	<stdio.h>
#include	<string.h>

#include        "mkf2_recd.h"

void mkf2InitSISRecd( MkfRECD *buf ){

    MkfRECD   work;

    work = *buf;
/*
 *  Initialize sis HK monitors in static struct <buf>
 */

    work.SIS_event_No[0]  = -99.999;
    work.SIS_event_No[1]  = -99.999;
    work.SIS_event_No[2]  = -99.999;
    work.SIS_event_No[3]  = -99.999;
    work.SIS_event_No[4]  = -99.999;
    work.SIS_event_No[5]  = -99.999;
    work.SIS_event_No[6]  = -99.999;
    work.SIS_event_No[7]  = -99.999;
    work.SIS_pixel_No[0]  = -99.999;
    work.SIS_pixel_No[1]  = -99.999;
    work.SIS_pixel_No[2]  = -99.999;
    work.SIS_pixel_No[3]  = -99.999;
    work.SIS_pixel_No[4]  = -99.999;
    work.SIS_pixel_No[5]  = -99.999;
    work.SIS_pixel_No[6]  = -99.999;
    work.SIS_pixel_No[7]  = -99.999;
    work.SIS_tlm_event[0] = -99.999;
    work.SIS_tlm_event[1] = -99.999;
    work.SIS_tlm_event[2] = -99.999;
    work.SIS_tlm_event[3] = -99.999;
    work.SIS_tlm_event[4] = -99.999;
    work.SIS_tlm_event[5] = -99.999;
    work.SIS_tlm_event[6] = -99.999;
    work.SIS_tlm_event[7] = -99.999;
    work.SIS_satf[0]      = 99;
    work.SIS_satf[1]      = 99;
    work.SIS_satf[2]      = 99;
    work.SIS_satf[3]      = 99;
    work.SIS_satf[4]      = 99;
    work.SIS_satf[5]      = 99;
    work.SIS_satf[6]      = 99;
    work.SIS_satf[7]      = 99;

    *buf = work;
    return;
  }



