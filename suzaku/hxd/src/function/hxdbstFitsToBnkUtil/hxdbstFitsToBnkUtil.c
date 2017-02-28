/*
   v0.0.1 test version created by M. sugiho
   v0.1.0 HXD-II Y.Terada, 2003-12-20
   v0.1.1 by Y.Terada, 2003-12-22
   v0.1.2 by H.Takahashi, Y.Terada, 2004-03-11
   v0.1.3 by Y.Terada, 2004-03-12
   v0.2.0 by Y.Terada, 2005-05-18 detele ETI
   v0.2.1 by Y.Terada, 2005-05-24, aetime and s_time
   v0.2.2 by Y.Terada, M.Ozaki 2008-10-25, bst_w_ant
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "bnk.h"
#include "fitsio.h"

#include "hxdbstFitsToBnkUtil.h"

static char *pname = "hxdbstFitsToBnkUtil";

void hxdbstFitsToBnk_init(){
  
  BnkDef( "HXD:BST:FRZD_TM", sizeof(double)*HXDBSTFITS_TPU_N_BOARD );
  BnkDef( "HXD:BST:FRTM_CT", sizeof(int)*HXDBSTFITS_TPU_N_BOARD );
  BnkDef( "HXD:BST:TM_MODE",  sizeof(int)*HXDBSTFITS_TPU_N_BOARD );
  
  BnkDef("HXD:BST:PACKET_AETIME", sizeof(double) );
  BnkDef("HXD:BST:PACKET_S_TIME", sizeof(double) );
  BnkDef("HXD:BST:PACKET_SEC_HEADER", sizeof(int) );
  BnkDef("HXD:BST:IBLOCK", sizeof(int) );

  BnkDef("HXD:BST:ERR_COD", sizeof(int) );
  BnkDef("HXD:BST:SOFT_FLG", sizeof(int) );
  BnkDef("HXD:BST:CURRENT_TPU", sizeof(int) );
  BnkDef("HXD:BST:REST_REQ", sizeof(int) );
  BnkDef("HXD:BST:DATA_SIZE", sizeof(int) );
  BnkDef("HXD:BST:READ_CNT", sizeof(int) );
  BnkDef("HXD:BST:DE_BOARD", sizeof(int) );

  BnkDef("HXD:BST:BOARD", sizeof(int) );
  BnkDef("HXD:BST:DATA_SEQ", sizeof(int) );
  BnkDef("HXD:BST:TIME", sizeof(int) );  
  BnkDef("HXD:BST:TH0", sizeof(int)*HXD_BST_TH_TIME_BIN );
  BnkDef("HXD:BST:TH1", sizeof(int)*HXD_BST_TH_TIME_BIN );
  BnkDef("HXD:BST:TH2", sizeof(int)*HXD_BST_TH_TIME_BIN );
  BnkDef("HXD:BST:TH3", sizeof(int)*HXD_BST_TH_TIME_BIN );  
  BnkDef("HXD:BST:PH", sizeof(int)*HXD_BST_PH_ENE_BIN );
  BnkDef("HXD:BST:PI", sizeof(int)*HXD_BST_PH_ENE_BIN );
  BnkDef("HXD:BST:OVER_FLOW", sizeof(int) );
  BnkDef("HXD:BST:PSEUDO", sizeof(int) );
  BnkDef("HXD:BST:T_ANT", sizeof(int)*HXDBSTFITS_N_TANT );
  BnkDef("HXD:BST:UD", sizeof(int) );
  BnkDef("HXD:BST:DEAD_TIME", sizeof(int) );
  BnkDef("HXD:BST:SUM_LD", sizeof(int) );
  BnkDef("HXD:BST:W_ANT", sizeof(int)*HXDBSTFITS_WPU_N_BOARD );
  BnkDef("HXD:BST:QUARITY", sizeof(int));
  
}

void hxdbstFitsToBnk_put( HxdBstFits *fits ){

  int i;

  int bst_iblock;
  int bst_sys_err_cod;
  int bst_soft_flg;
  int bst_current_tpu;
  int bst_rest_req;
  int bst_data_size;
  int bst_read_cnt;
  int bst_de_board;
  int bst_ae_board;
  int bst_data_seq;
  int bst_frzd_time;
  int bst_th0[HXD_BST_TH_TIME_BIN];
  int bst_th1[HXD_BST_TH_TIME_BIN];
  int bst_th2[HXD_BST_TH_TIME_BIN];
  int bst_th3[HXD_BST_TH_TIME_BIN];
  int bst_ph[HXD_BST_PH_ENE_BIN];
  int bst_pi[HXD_BST_PH_ENE_BIN];
  int bst_over_flow;
  int bst_pseudo;
  int bst_t_ant[HXDBSTFITS_N_TANT];
  int bst_ud;
  int bst_dead_time;
  int bst_sum_ld;
  int bst_w_ant[HXDBSTFITS_WPU_N_BOARD];
  int bst_quality;

  bst_iblock    = (int) fits->bst_iblock; 
  bst_sys_err_cod  = (int) fits->bst_sys_err_cod;
  bst_soft_flg  = (int) fits->bst_soft_flg;
  bst_current_tpu  = (int) fits->bst_current_tpu;
  bst_rest_req  = (int) fits->bst_rest_req;
  bst_data_size = (int) fits->bst_data_size;
  bst_read_cnt  = (int) fits->bst_read_cnt;
  bst_de_board     = (int) fits->bst_de_module;
  bst_ae_board     = (int) fits->bst_ae_module;
  bst_data_seq  = (int) fits->bst_data_seq;
  bst_frzd_time = (int) fits->bst_frzd_time;
  bst_over_flow = (int) fits->bst_over_flow;
  bst_pseudo    = (int) fits->bst_pseudo;
  bst_t_ant[0]  = (int) fits->bst_t_ant0;
  bst_t_ant[1]  = (int) fits->bst_t_ant1;
  bst_t_ant[2]  = (int) fits->bst_t_ant2;
  bst_t_ant[3]  = (int) fits->bst_t_ant3;
  bst_t_ant[4]  = (int) fits->bst_t_ant4;  
  bst_ud        = (int) fits->bst_ud;
  bst_dead_time = (int) fits->bst_dead_time;
  bst_sum_ld    = (int) fits->bst_sum_ld;
  bst_w_ant[0]  = (int) fits->bst_w_ant0;
  bst_w_ant[1]  = (int) fits->bst_w_ant1;
  bst_w_ant[2]  = (int) fits->bst_w_ant2;
  bst_w_ant[3]  = (int) fits->bst_w_ant3;
  
  for(i=0;i<HXD_BST_PH_ENE_BIN;i++){
      bst_ph[i] = (int) fits->bst_ph[i];
  }
  
  for(i=0;i<HXD_BST_PH_ENE_BIN;i++){
      bst_pi[i] = (int) fits->bst_pi[i];
  }
  
  for(i=0;i<HXD_BST_TH_TIME_BIN;i++){
      bst_th0[i] = (int) fits->bst_th0[i];
      bst_th1[i] = (int) fits->bst_th1[i];
      bst_th2[i] = (int) fits->bst_th2[i];
      bst_th3[i] = (int) fits->bst_th3[i];
  }

  bst_quality = (int) fits->bst_quality;
  
  BnkfPutM("HXD:BST:PACKET_AETIME", sizeof(double), &fits->aetime);
  BnkfPutM("HXD:BST:PACKET_S_TIME", sizeof(double), &fits->s_time);
  BnkfPutM("HXD:BST:PACKET_SEC_HEADER", sizeof(int), &fits->ti);
  BnkfPutM("HXD:BST:IBLOCK", sizeof(int), &bst_iblock );
  BnkfPutM("HXD:BST:ERR_COD", sizeof(int), &bst_sys_err_cod );
  BnkfPutM("HXD:BST:SOFT_FLG", sizeof(int), &bst_soft_flg );
  BnkfPutM("HXD:BST:CURRENT_TPU", sizeof(int), &bst_current_tpu );
  BnkfPutM("HXD:BST:REST_REQ", sizeof(int), &bst_rest_req );
  BnkfPutM("HXD:BST:DATA_SIZE", sizeof(int), &bst_data_size );
  BnkfPutM("HXD:BST:READ_CNT", sizeof(int), &bst_read_cnt );
  BnkfPutM("HXD:BST:DE_BOARD", sizeof(int), &bst_de_board );
  BnkfPutM("HXD:BST:BOARD", sizeof(int), &bst_ae_board );
  BnkfPutM("HXD:BST:DATA_SEQ", sizeof(int), &bst_data_seq );
  BnkfPutM("HXD:BST:TIME", sizeof(int), &bst_frzd_time );  
  BnkfPutM("HXD:BST:TH0", sizeof(int)*HXD_BST_TH_TIME_BIN, bst_th0 );
  BnkfPutM("HXD:BST:TH1", sizeof(int)*HXD_BST_TH_TIME_BIN, bst_th1 );
  BnkfPutM("HXD:BST:TH2", sizeof(int)*HXD_BST_TH_TIME_BIN, bst_th2 );
  BnkfPutM("HXD:BST:TH3", sizeof(int)*HXD_BST_TH_TIME_BIN, bst_th3 );  
  BnkfPutM("HXD:BST:PH", sizeof(int)*HXD_BST_PH_ENE_BIN, bst_ph );
  BnkfPutM("HXD:BST:PI", sizeof(int)*HXD_BST_PH_ENE_BIN, bst_pi );
  BnkfPutM("HXD:BST:OVER_FLOW", sizeof(int), &bst_over_flow );
  BnkfPutM("HXD:BST:PSEUDO", sizeof(int), &bst_pseudo );
  BnkfPutM("HXD:BST:T_ANT", sizeof(int)*HXDBSTFITS_N_TANT, bst_t_ant );
  BnkfPutM("HXD:BST:UD", sizeof(int), &bst_ud );
  BnkfPutM("HXD:BST:DEAD_TIME", sizeof(int), &bst_dead_time );
  BnkfPutM("HXD:BST:SUM_LD", sizeof(int), &bst_sum_ld );
  BnkfPutM("HXD:BST:W_ANT", sizeof(int)*HXDBSTFITS_WPU_N_BOARD, bst_w_ant );
  BnkfPutM("HXD:BST:QUARITY", sizeof(int), &bst_quality);
  
}

void hxdbstFitsToBnk_get( HxdBstFits *fits ){

  int size;

  int i;
  
  int bst_iblock;
  int bst_sys_err_cod;
  int bst_soft_flg;
  int bst_current_tpu;
  int bst_rest_req;
  int bst_data_size;
  int bst_read_cnt;
  int bst_de_board;
  int bst_ae_board;
  int bst_data_seq;
  int bst_frzd_time;
  int bst_th0[HXD_BST_TH_TIME_BIN];
  int bst_th1[HXD_BST_TH_TIME_BIN];
  int bst_th2[HXD_BST_TH_TIME_BIN];
  int bst_th3[HXD_BST_TH_TIME_BIN];
  int bst_ph[HXD_BST_PH_ENE_BIN];
  int bst_over_flow;
  int bst_pseudo;
  int bst_t_ant[HXDBSTFITS_N_TANT];
  int bst_ud;
  int bst_dead_time;
  int bst_sum_ld;
  int bst_w_ant[HXDBSTFITS_WPU_N_BOARD];

  BnkfGetM("HXD:BST:PACKET_S_TIME", sizeof(double), &size, &fits->s_time);
  fits->aetime = fits->s_time;
  BnkfGetM("HXD:BST:IBLOCK", sizeof(int), &size, &bst_iblock );
  BnkfGetM("HXD:BST:ERR_COD", sizeof(int), &size, &bst_sys_err_cod );
  BnkfGetM("HXD:BST:SOFT_FLG", sizeof(int), &size, &bst_soft_flg );
  BnkfGetM("HXD:BST:CURRENT_TPU", sizeof(int), &size, &bst_current_tpu );
  BnkfGetM("HXD:BST:REST_REQ", sizeof(int), &size, &bst_rest_req );
  BnkfGetM("HXD:BST:DATA_SIZE", sizeof(int), &size, &bst_data_size );
  BnkfGetM("HXD:BST:READ_CNT", sizeof(int), &size, &bst_read_cnt );
  BnkfGetM("HXD:BST:DE_BOARD", sizeof(int), &size, &bst_de_board );
  BnkfGetM("HXD:BST:BOARD", sizeof(int), &size, &bst_ae_board );
  BnkfGetM("HXD:BST:DATA_SEQ", sizeof(int), &size, &bst_data_seq );
  BnkfGetM("HXD:BST:TIME", sizeof(int), &size, &bst_frzd_time );  
  BnkfGetM("HXD:BST:TH0", sizeof(int)*HXD_BST_TH_TIME_BIN, &size, bst_th0 );
  BnkfGetM("HXD:BST:TH1", sizeof(int)*HXD_BST_TH_TIME_BIN, &size, bst_th1 );
  BnkfGetM("HXD:BST:TH2", sizeof(int)*HXD_BST_TH_TIME_BIN, &size, bst_th2 );
  BnkfGetM("HXD:BST:TH3", sizeof(int)*HXD_BST_TH_TIME_BIN, &size, bst_th3 );  
  BnkfGetM("HXD:BST:PH", sizeof(int)*HXD_BST_PH_ENE_BIN, &size, bst_ph );
  BnkfGetM("HXD:BST:OVER_FLOW", sizeof(int), &size, &bst_over_flow );
  BnkfGetM("HXD:BST:PSEUDO", sizeof(int), &size, &bst_pseudo );
  BnkfGetM("HXD:BST:T_ANT", sizeof(int)*HXDBSTFITS_N_TANT, &size, bst_t_ant );
  BnkfGetM("HXD:BST:UD", sizeof(int), &size, &bst_ud );
  BnkfGetM("HXD:BST:DEAD_TIME", sizeof(int), &size, &bst_dead_time );
  BnkfGetM("HXD:BST:SUM_LD", sizeof(int), &size, &bst_sum_ld );
  BnkfGetM("HXD:BST:W_ANT", sizeof(int)*HXDBSTFITS_WPU_N_BOARD, 
	   &size, bst_w_ant );


  fits->bst_iblock = (short) bst_iblock; 
  fits->bst_sys_err_cod = (unsigned char) bst_sys_err_cod;
  fits->bst_soft_flg = (unsigned char) bst_soft_flg;
  fits->bst_current_tpu = (unsigned char) bst_current_tpu;
  fits->bst_rest_req = (unsigned char) bst_rest_req;
  fits->bst_data_size = (unsigned char) bst_data_size;
  fits->bst_read_cnt = (unsigned char) bst_read_cnt;
  fits->bst_de_module = (unsigned char) bst_de_board;
  fits->bst_ae_module = (unsigned char) bst_ae_board;
  fits->bst_data_seq = (unsigned char) bst_data_seq;
  fits->bst_frzd_time = (unsigned int) bst_frzd_time;
  fits->bst_over_flow = (unsigned short) bst_over_flow;
  fits->bst_pseudo = (unsigned short) bst_pseudo;
  fits->bst_t_ant0 = (unsigned short) bst_t_ant[0];
  fits->bst_t_ant1 = (unsigned short) bst_t_ant[1];
  fits->bst_t_ant2 = (unsigned short) bst_t_ant[2];
  fits->bst_t_ant3 = (unsigned short) bst_t_ant[3];
  fits->bst_t_ant4 = (unsigned short) bst_t_ant[4];
  fits->bst_ud = (unsigned short) bst_ud;
  fits->bst_dead_time = (unsigned short) bst_dead_time;
  fits->bst_sum_ld = (unsigned short) bst_sum_ld;
  fits->bst_w_ant0 = (unsigned short) bst_w_ant[0];
  fits->bst_w_ant1 = (unsigned short) bst_w_ant[1];
  fits->bst_w_ant2 = (unsigned short) bst_w_ant[2];
  fits->bst_w_ant3 = (unsigned short) bst_w_ant[3];
  
  for(i=0;i<HXD_BST_PH_ENE_BIN;i++){
    fits->bst_ph[i] = (unsigned short) bst_ph[i];
  }
  
  for(i=0;i<HXD_BST_PH_ENE_BIN;i++){
    fits->bst_pi[i] = 0x00;
  }
  
  for(i=0;i<HXD_BST_TH_TIME_BIN;i++){
    fits->bst_th0[i] = (unsigned short) bst_th0[i];
    fits->bst_th1[i] = (unsigned short) bst_th1[i];
    fits->bst_th2[i] = (unsigned short) bst_th2[i];
    fits->bst_th3[i] = (unsigned short) bst_th3[i];
  }

  fits->bst_quality = 0x00;
}
