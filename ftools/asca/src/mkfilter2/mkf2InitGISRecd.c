/* 
 *  void mkf2InitGISRecd( MkfRECD *buf )
 *
 *  Ver 1.0  Mar. 21, 1994  by T.Takeshima
 */

#include	<stdio.h>
#include	<string.h>

#include        "mkf2_recd.h"

void mkf2InitGISRecd( MkfRECD *buf ){

    MkfRECD   work;

    work = *buf;
/*
 *  Initialize GIS monitor counts in static struct <buf>
 */

    work.GIS_LDHIT[0]     = -99.999;
    work.GIS_LDHIT[1]     = -99.999;
    work.GIS_CPU_in[0]    = -99.999;
    work.GIS_CPU_in[1]    = -99.999;
    work.GIS_CPU_out[0]   = -99.999;
    work.GIS_CPU_out[1]   = -99.999;
    work.GIS_L0[0]        = -99.999;
    work.GIS_L0[1]        = -99.999;
    work.GIS_L1[0]        = -99.999;
    work.GIS_L1[1]        = -99.999;
    work.GIS_L2[0]        = -99.999;
    work.GIS_L2[1]        = -99.999;
    work.GIS_H0[0]        = -99.999;
    work.GIS_H0[1]        = -99.999;
    work.GIS_H1[0]        = -99.999;
    work.GIS_H1[1]        = -99.999;
    work.GIS_H2[0]        = -99.999;
    work.GIS_H2[1]        = -99.999;
    work.GIS_tlm_event[0] = -99.999;
    work.GIS_tlm_event[1] = -99.999;
    work.RBM_count        = -99.999;

    *buf = work;

    return;
  }



