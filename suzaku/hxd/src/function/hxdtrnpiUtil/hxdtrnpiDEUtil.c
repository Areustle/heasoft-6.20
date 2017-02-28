/*
 * TYPE: function
 * NAME: hxdtrnpiUtil
 *
 * file: hxdtrnpiDEUtil.c
 *
 * PURPOSE:
 *  the first step of TRANSIENT PH DATA ( PHA -> PI )
 *  unpack the rebinned PH spectrum by DE on board software.
 *
 * HISTORY:
 *       version 0.0.1  1999.10.8  created  by Y. Terada 
 *       version 0.0.2  1999.10.14 modefied by Y. Terada
 *                 add hxdtrnpi_rebintable_read();
 *                 add hxdtrnpi_calc_rdbin();  used in HXDtrnRbnTblFitsWrite.
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

#define GET_KEYWORD_NUM TRN_PH_CH
static int rebin_id[GET_KEYWORD_NUM];

void
hxdtrnpi_DE_correct_Init(char *rebin_file_list, ASTE_HK **rebin_file,
			 int *istat){
    char keyword[256] = "";
    int ch;
    
    *rebin_file = aste_gethk_init( rebin_file_list );

    if( *rebin_file == NULL ){
	*istat = ANL_QUIT;
	return;
    } 

    for(ch=0; ch<GET_KEYWORD_NUM; ch++){
	sprintf(keyword, "REBIN%d", ch);
	*istat = aste_gethk_register(*rebin_file, keyword, &rebin_id[ch]);
	if( *istat == ANL_FALSE )  return; 
    }
    
    *istat = ANL_OK;
}

void
hxdtrnpi_DE_correct(int board, int *pha_tlm, double *pha_ae,
		    int rebin_setting[TRN_PH_CH] ){
/* ************* (1) HXD-DE PI program zip_trn.c **********************
 *  [fig.1]     << rebin_setting >>
 * PHA_TLM  ^                          011
 *      15  |                          *--*
 *       .  |
 *       .  |                011..
 *       .  |          01111 *--..     ======= PI program code =======
 *       2  |     0111 *----*          1: Add the prev_PH 
 *       1  |0111 *---*                0: Telemetry Edit.(skip Next CH)
 *       0  |*---*                     ===============================
 *          +-------------------------------->
 *           0 1 2 3..                    53   PHA_AE(=PHA_AE)
 * *********** (2) unpack function[hxdtrnpi_DE_correct] ***************
 *  [fig.2]     
 * PHA_TLM  ^                           (3)
 *   .  15  |:packed_bin_num          *--*
 *   |   .  |           bin_width[4]:??
 * (bin) .  |      bin_width[3]:(2)
 *       .  |bin_width[2]:(5)*--..     ===================================
 *       2  |      (4) *----*          first : PRE SCAN (get "bin_width"s)
 *       1  | (4) *---*                second: UNPACK
 *       0  |*---*                     ===================================
 *          +-------------------------------->
 *           0 1 2 3..                    53   PHA_AE(=PHA_AE)
 *                       (ch)--->
 ************************************************************************/
    int packed_bin_num =  -1; /* rebin_setting[0] must be 0 */
    int ch,  integrated_ch=0;
    int bin, bin_width[TRN_PH_CH];

    /* ======= (0) INIT ========= */
    for( ch=0; ch<TRN_PH_CH; ch++ ) bin_width[ch] = 0;
    
    if( rebin_setting[ch]&(0x08>>board) == 1 ) { /**** table Check ****/
	fprintf(stderr,"hxdtrnpi: DE: rebin table wrong!\n");
	return; /* need ERROR handling process */
    }
    
    /* ====== (I) PRE SCAN  ===== */
    for( ch=0; ch<TRN_PH_CH; ch++ ){
	if( (rebin_setting[ch]&(0x08>>board)) == 0x00 )  packed_bin_num++;
	bin_width[packed_bin_num] ++;
    }

    /* ===== (II) UNPACK REBINNING ==== */
    for( bin=0; bin<=packed_bin_num; bin++ ){
	for( ch=integrated_ch; ch<(integrated_ch+bin_width[bin]); ch++ )
	    pha_ae[ch] =  (double) pha_tlm[bin] / bin_width[bin];
	integrated_ch += bin_width[bin];
    }

    return;
}

void
hxdtrnpi_DE_no_correct(int *pha_tlm, double *pha_ae){
    int ch;
    for( ch=0; ch<TRN_PH_CH; ch++ ) pha_ae[ch] = (double) pha_tlm[ch];
}

void
hxdtrnpi_AEDE_correct(int *pha_ae, double *pha_adc){
    int ch;
    
    for (ch = 0; ch <= 47; ch ++){
	pha_adc[ch] =pha_ae[ch];
    }
    
    pha_adc[48] =pha_ae[48]/2.0;
    pha_adc[49] =pha_ae[48]/2.0;
    pha_adc[50] =pha_ae[49]/2.0;
    pha_adc[51] =pha_ae[49]/2.0;
    pha_adc[52] =pha_ae[50]/2.0;
    pha_adc[53] =pha_ae[50]/2.0;
    pha_adc[54] =pha_ae[51]/2.0;
    pha_adc[55] =pha_ae[51]/2.0;
    
    pha_adc[56] =pha_ae[52]/4.0;
    pha_adc[57] =pha_ae[52]/4.0;
    pha_adc[58] =pha_ae[52]/4.0;
    pha_adc[59] =pha_ae[52]/4.0;

    pha_adc[60] =pha_ae[53]/4.0;
    pha_adc[61] =pha_ae[53]/4.0;
    pha_adc[62] =pha_ae[53]/4.0;
    pha_adc[63] =pha_ae[53]/4.0;

}
		    
void
hxdtrnpi_rebintable_read(ASTE_HK *rebin_file, double trntime,
			 int *rebin_table, int *istat){
    char keyword[256] = "";
    double stime[3]; /* 0:latch time 1:pre time (not suport) 2:next time */
    int ch;
    
    for(ch=0; ch<GET_KEYWORD_NUM; ch++){
	sprintf(keyword, "TRNBIN%d", ch);
	*istat = aste_gethk_int (rebin_file, keyword, &rebin_id[ch],
				 trntime, &rebin_table[ch], stime);
	if ( *istat == ANL_FALSE )  return;
    }

    *istat = ANL_OK;
}

void
hxdtrnpi_calc_rdbin(int *binning_table, int *calc_rdbin, int board){
    int ch, new_bin_num;
    
    for( ch=0; ch<TRN_PH_CH; ch++ ){
	if( (binning_table[ch]&(0x08>>board)) == 0x00 )  new_bin_num++;
    }

    *calc_rdbin = TRN_PH_CH - new_bin_num;
}



