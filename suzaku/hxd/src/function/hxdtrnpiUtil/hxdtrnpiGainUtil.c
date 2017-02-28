/*
 * TYPE: function
 * NAME: hxdtrnpiUtil
 *
 * file: hxdtrnpiGainUtil.c
 *
 * PURPOSE:
 *  the first step of TRANSIENT PH DATA ( PHA -> PI )
 *  Read Gain History File for TRANSIENT DATA.
 *
 * HISTORY:
 *       version 0.0.2  1999.10.14 created  by Y. Terada
 *       version 0.0.3  1999.10.31 modefied by Y. Terada
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "bnk.h"
#include "fitsio.h"
#include "aste_rand.h"
#include "aste_gethk.h"

#include "hxdtrnpiUtil.h"
static char *pname = "hxdtrnpiUtil";

#define TRN_PH_CH 54
#define TPU_N_BOARD 4
#define GET_KEYWORD_NUM (4*TPU_N_BOARD)
static int gainhst_id[GET_KEYWORD_NUM];

enum{
    COEFFICIENT0,
    OFFSET0,
    COEFFICIENT_ERR0,
    OFFSET_ERR0,
    COEFFICIENT1,
    OFFSET1,
    COEFFICIENT_ERR1,
    OFFSET_ERR1,
    COEFFICIENT2,
    OFFSET2,
    COEFFICIENT_ERR2,
    OFFSET_ERR2,
    COEFFICIENT3,
    OFFSET3,
    COEFFICIENT_ERR3,
    OFFSET_ERR3,
};

enum{
    COEFFICIENT,
    OFFSET,
    COEFFICIENT_ERR,
    OFFSET_ERR,
};

void
hxdtrnpi_Gain_correct_Init(char *gain_history_list,
			   ASTE_HK **gain_history_file, int *istat){
    *gain_history_file = aste_gethk_init (gain_history_list);
    
    if( *gain_history_file == NULL ){
	*istat = ANL_QUIT;
	return;
    }

    *istat = aste_gethk_register(*gain_history_file, "COEFFICIENT0",
				 &gainhst_id[COEFFICIENT0]);
    if( *istat == ANL_FALSE )  return; 

    *istat = aste_gethk_register(*gain_history_file, "OFFSET0",
				 &gainhst_id[OFFSET0]);
    if( *istat == ANL_FALSE )  return; 

    *istat = aste_gethk_register(*gain_history_file, "COEFFICIENT_ERR0",
				 &gainhst_id[COEFFICIENT_ERR0]);
    if( *istat == ANL_FALSE )  return;

    *istat = aste_gethk_register(*gain_history_file, "OFFSET_ERR0",
				 &gainhst_id[OFFSET_ERR0]);
    if( *istat == ANL_FALSE )  return; 

    *istat = ANL_OK;
    
    *istat = aste_gethk_register(*gain_history_file, "COEFFICIENT1",
				 &gainhst_id[COEFFICIENT1]);
    if( *istat == ANL_FALSE )  return; 

    *istat = aste_gethk_register(*gain_history_file, "OFFSET1",
				 &gainhst_id[OFFSET1]);
    if( *istat == ANL_FALSE )  return; 

    *istat = aste_gethk_register(*gain_history_file, "COEFFICIENT_ERR1",
				 &gainhst_id[COEFFICIENT_ERR1]);
    if( *istat == ANL_FALSE )  return;

    *istat = aste_gethk_register(*gain_history_file, "OFFSET_ERR1",
				 &gainhst_id[OFFSET_ERR1]);
    if( *istat == ANL_FALSE )  return; 

    *istat = ANL_OK;
    
    *istat = aste_gethk_register(*gain_history_file, "COEFFICIENT2",
				 &gainhst_id[COEFFICIENT2]);
    if( *istat == ANL_FALSE )  return; 

    *istat = aste_gethk_register(*gain_history_file, "OFFSET2",
				 &gainhst_id[OFFSET2]);
    if( *istat == ANL_FALSE )  return; 

    *istat = aste_gethk_register(*gain_history_file, "COEFFICIENT_ERR2",
				 &gainhst_id[COEFFICIENT_ERR2]);
    if( *istat == ANL_FALSE )  return;

    *istat = aste_gethk_register(*gain_history_file, "OFFSET_ERR2",
				 &gainhst_id[OFFSET_ERR2]);
    if( *istat == ANL_FALSE )  return; 

    *istat = ANL_OK;
    
    *istat = aste_gethk_register(*gain_history_file, "COEFFICIENT3",
				 &gainhst_id[COEFFICIENT3]);
    if( *istat == ANL_FALSE )  return; 

    *istat = aste_gethk_register(*gain_history_file, "OFFSET3",
				 &gainhst_id[OFFSET3]);
    if( *istat == ANL_FALSE )  return; 

    *istat = aste_gethk_register(*gain_history_file, "COEFFICIENT_ERR3",
				 &gainhst_id[COEFFICIENT_ERR3]);
    if( *istat == ANL_FALSE )  return;

    *istat = aste_gethk_register(*gain_history_file, "OFFSET_ERR3",
				 &gainhst_id[OFFSET_ERR3]);
    if( *istat == ANL_FALSE )  return; 

    *istat = ANL_OK;
    
}

void
hxdtrnpi_Gain_correction(double *pi_ae, double *pi, int *gain_table,
			 int *istat){
    int ch;
    
    for(ch=0; ch<TRN_PH_CH; ch) {
	pi[ch] = gain_table[COEFFICIENT] * pi_ae[ch] + gain_table[OFFSET];
    }
    
    *istat = ANL_OK;
}

void
hxdtrnpi_gaintable_read(ASTE_HK *gain_history_file,
			double trntime, int board,
			int *gain_table, int *istat){
    double stime[3]; /* 0:latch time 1:pre time (not suport) 2:next time */
    
    if( board == 0 ) {
	*istat = aste_gethk_int (gain_history_file, "COEFFICIENT0",
				 &gainhst_id[COEFFICIENT0], trntime,
				 &gain_table[COEFFICIENT], stime);
	if ( *istat == ANL_FALSE )  return;
	
	*istat = aste_gethk_int (gain_history_file, "OFFSET0",
				 &gainhst_id[OFFSET0], trntime,
				 &gain_table[OFFSET], stime);
	if ( *istat == ANL_FALSE )  return;
	
	*istat = aste_gethk_int (gain_history_file, "COEFFICIENT_ERR0",
				 &gainhst_id[COEFFICIENT_ERR0], trntime,
				 &gain_table[COEFFICIENT_ERR], stime);
	if ( *istat == ANL_FALSE )  return;
	
	*istat = aste_gethk_int (gain_history_file, "OFFSET_ERR0",
				 &gainhst_id[OFFSET_ERR0], trntime,
				 &gain_table[OFFSET_ERR], stime);
	if ( *istat == ANL_FALSE )  return;
    } else if( board == 1 ) {
	*istat = aste_gethk_int (gain_history_file, "COEFFICIENT1",
				 &gainhst_id[COEFFICIENT1], trntime,
				 &gain_table[COEFFICIENT], stime);
	if ( *istat == ANL_FALSE )  return;
	
	*istat = aste_gethk_int (gain_history_file, "OFFSET1",
				 &gainhst_id[OFFSET1], trntime,
				 &gain_table[OFFSET], stime);
	if ( *istat == ANL_FALSE )  return;
	
	*istat = aste_gethk_int (gain_history_file, "COEFFICIENT_ERR1",
				 &gainhst_id[COEFFICIENT_ERR1], trntime,
				 &gain_table[COEFFICIENT_ERR], stime);
	if ( *istat == ANL_FALSE )  return;
	
	*istat = aste_gethk_int (gain_history_file, "OFFSET_ERR1",
				 &gainhst_id[OFFSET_ERR1], trntime,
				 &gain_table[OFFSET_ERR], stime);
	if ( *istat == ANL_FALSE )  return;
    } else if( board == 2 ) {
	*istat = aste_gethk_int (gain_history_file, "COEFFICIENT2",
				 &gainhst_id[COEFFICIENT2], trntime,
				 &gain_table[COEFFICIENT], stime);
	if ( *istat == ANL_FALSE )  return;
	
	*istat = aste_gethk_int (gain_history_file, "OFFSET2",
				 &gainhst_id[OFFSET2], trntime,
				 &gain_table[OFFSET], stime);
	if ( *istat == ANL_FALSE )  return;
	
	*istat = aste_gethk_int (gain_history_file, "COEFFICIENT_ERR2",
				 &gainhst_id[COEFFICIENT_ERR2], trntime,
				 &gain_table[COEFFICIENT_ERR], stime);
	if ( *istat == ANL_FALSE )  return;
	
	*istat = aste_gethk_int (gain_history_file, "OFFSET_ERR2",
				 &gainhst_id[OFFSET_ERR2], trntime,
				 &gain_table[OFFSET_ERR], stime);
	if ( *istat == ANL_FALSE )  return;
    } else if( board == 3 ) {
	*istat = aste_gethk_int (gain_history_file, "COEFFICIENT3",
				 &gainhst_id[COEFFICIENT3], trntime,
				 &gain_table[COEFFICIENT], stime);
	if ( *istat == ANL_FALSE )  return;
	
	*istat = aste_gethk_int (gain_history_file, "OFFSET3",
				 &gainhst_id[OFFSET3], trntime,
				 &gain_table[OFFSET], stime);
	if ( *istat == ANL_FALSE )  return;
	
	*istat = aste_gethk_int (gain_history_file, "COEFFICIENT_ERR3",
				 &gainhst_id[COEFFICIENT_ERR3], trntime,
				 &gain_table[COEFFICIENT_ERR], stime);
	if ( *istat == ANL_FALSE )  return;
	
	*istat = aste_gethk_int (gain_history_file, "OFFSET_ERR3",
				 &gainhst_id[OFFSET_ERR3], trntime,
				 &gain_table[OFFSET_ERR], stime);
	if ( *istat == ANL_FALSE )  return;
    }
    
    *istat = ANL_OK;
}



