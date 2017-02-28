/*
   v0.0.1 test version created by M. sugiho
   v0.0.4 for HXD-II, by Y. Terada 2003-04-07
   v0.1.0 for HXD-II, by Y. Terada 2003-05-03
   v0.1.2 change key name, by Y. Terada 2003-12-20
   v0.1.3 Add DATA_SIZE for illigal data, M.Suzuki, Y.Terada 2003-12-23
   v0.1.4 change ETI format Y.Terada 2004-03-12
   v0.2.0 delete ETI        Y.Terada 2005-05-17
   v0.2.1 aetime and s_time Y.Terada 2005-05-24
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "bnk.h"
#include "fitsio.h"

#include "hxdtrnFitsToBnkUtil.h"

static char *pname = "hxdtrnFitsToBnkUtil";

void hxdtrnFitsToBnk_init(){

  BnkDef( "HXD:TRN:EV_TIME",  sizeof(double) );
  
  BnkDef( "HXD:TRN:PACKET_AETIME",  sizeof(double) );
  BnkDef( "HXD:TRN:PACKET_S_TIME",  sizeof(double) );
  BnkDef( "HXD:TRN:PACKET_SEC_HEADER",  sizeof(int) );
  
  BnkDef( "HXD:TRB:IBLOCK",  sizeof(int) );
  
  BnkDef( "HXD:TRN:AE_DE_Len_Err",  sizeof(int) );
  BnkDef( "HXD:TRN:BOARD",  sizeof(int) );
  BnkDef( "HXD:TRN:BLOCK",  sizeof(int) );
  BnkDef( "HXD:TRN:RDBIN",  sizeof(int) );
  BnkDef( "HXD:TRN:TBLID",  sizeof(int) );

  BnkDef( "HXD:TRN:DATA_SIZE",  sizeof(int) ); /** add **/
  BnkDef( "HXD:TRN:SYS_ERR_CODE", sizeof(int) ); /** add **/
  
  BnkDef( "HXD:TRH:BLOCK",         sizeof(int) );
  BnkDef( "HXD:TRH:TIME",          sizeof(int) );
  BnkDef( "HXD:TRH:GB_TIME",       sizeof(int) );
  BnkDef( "HXD:TRH:GB_FLG",        sizeof(int) );
  BnkDef( "HXD:TRH:TIME_MODE",     sizeof(int) );
  BnkDef( "HXD:TRH:RBM",           sizeof(int) );
  BnkDef( "HXD:TRH:GB_FRZ",        sizeof(int) );
  BnkDef( "HXD:TRH:DT_MODE",       sizeof(int) );
  BnkDef( "HXD:TRH:SUMLD_MODE",    sizeof(int) );
  BnkDef( "HXD:TRH:BOARD",         sizeof(int) );
  BnkDef( "HXD:TRH:GB_TRG",        sizeof(int) );

  BnkDef( "HXD:TRB:PI",            sizeof(int)*HXD_TRN_MAX_ENE_BIN );
  
  BnkDef( "HXD:TRB:PH",            sizeof(int)*HXD_TRN_MAX_ENE_BIN );
  BnkDef( "HXD:TRB:OVER_FLOW",     sizeof(int) );
  BnkDef( "HXD:TRB:PSEUDO",        sizeof(int) );
  BnkDef( "HXD:TRB:TRN_ANT",       sizeof(int)*5 );
  BnkDef( "HXD:TRB:UD",            sizeof(int) );
  BnkDef( "HXD:TRB:DEAD_TIME",     sizeof(int) );
  BnkDef( "HXD:TRB:SUM_LD",        sizeof(int) );
  BnkDef( "HXD:TRB:WELL_ANT",      sizeof(int)*4 ); 

  BnkDef( "HXD:TRN:TRN_QUALITY",  sizeof(int) );
  
}

void hxdtrnFitsToBnk_put( HxdTrnFits *fits ){
  
  int i;
  
  int trn_iblock;  
  int trn_ae_de_length_chk;
  int trn_de_module;
  int trn_de_block;
  int trn_rdbin;
  int trn_tblid;        /** add **/
  int trn_sys_err_code; /** add **/
  int trn_data_size;
  int trn_ae_block;
  int trn_gb_flg;
  int trn_time_mode;
  int trn_rbm;
  int trn_gb_frz;
  int trn_dt_mode;
  int trn_sumld_mode;
  int trn_ae_module;
  int trn_gb_trg;
  int trn_ph[HXD_TRN_MAX_ENE_BIN];
  int trn_pi[HXD_TRN_MAX_ENE_BIN];
  int trn_over_flow;
  int trn_pseudo;
  int trn_t_ant[5];
  int trn_ud;
  int trn_dead_time;
  int trn_sum_ld;
  int trn_w_ant[4];
  int trn_quality;

  trn_iblock = (int)fits->trn_iblock;
  trn_ae_de_length_chk = (int) fits->trn_ae_de_length_chk;
  trn_de_module = (int)fits->trn_de_module;
  trn_de_block = (int)fits->trn_de_block;
  trn_rdbin = (int)fits->trn_rdbin;
  trn_tblid = (int)fits->trn_tblid;
  trn_data_size = (int) fits->trn_data_size; /** add (illegal data) **/
  trn_sys_err_code = (int) fits->trn_sys_err_code; /** add (illegal data) **/
  trn_ae_block = (int)fits->trn_ae_block;
  trn_gb_flg = (int)fits->trn_gb_flg;
  trn_time_mode = (int)fits->trn_time_mode;
  trn_rbm = (int)fits->trn_rbm;
  trn_gb_frz = (int)fits->trn_gb_frz;
  trn_dt_mode = (int)fits->trn_dt_mode;
  trn_sumld_mode = (int)fits->trn_sumld_mode;
  trn_ae_module = (int)fits->trn_ae_module;
  trn_gb_trg = (int)fits->trn_gb_trg;

  for(i=0;i<HXD_TRN_MAX_ENE_BIN;i++){
    trn_pi[i] = (int)fits->trn_pi[i];
  }
  
  for(i=0;i<HXD_TRN_MAX_ENE_BIN;i++){
    trn_ph[i] = (int)fits->trn_ph[i];
  }
  
  trn_over_flow = (int)fits->trn_over_flow;
  trn_pseudo = (int)fits->trn_pseudo;
  trn_t_ant[0] = (int)fits->trn_t_ant0;
  trn_t_ant[1] = (int)fits->trn_t_ant1;
  trn_t_ant[2] = (int)fits->trn_t_ant2;
  trn_t_ant[3] = (int)fits->trn_t_ant3;
  trn_t_ant[4] = (int)fits->trn_t_ant4;  
  trn_ud = (int)fits->trn_ud;
  trn_dead_time = (int)fits->trn_dead_time;
  trn_sum_ld = (int)fits->trn_sum_ld;
  trn_w_ant[0] = (int)fits->trn_w_ant0;
  trn_w_ant[1] = (int)fits->trn_w_ant1;
  trn_w_ant[2] = (int)fits->trn_w_ant2;
  trn_w_ant[3] = (int)fits->trn_w_ant3;
  
  trn_quality  = (int)fits->trn_quality;

  BnkfPutM( "HXD:TRN:EV_TIME",  sizeof(double), &fits->time );
  
  BnkfPutM( "HXD:TRN:PACKET_AETIME",  sizeof(double), &fits->aetime );
  BnkfPutM( "HXD:TRN:PACKET_S_TIME",  sizeof(double), &fits->s_time );
  BnkfPutM( "HXD:TRN:PACKET_SEC_HEADER",  sizeof(int), &fits->ti );
  
  BnkfPutM( "HXD:TRB:IBLOCK",  sizeof(int), &trn_iblock );
  
  BnkfPutM( "HXD:TRN:AE_DE_Len_Err",  sizeof(int), &trn_ae_de_length_chk );
  BnkfPutM( "HXD:TRN:BOARD",  sizeof(int), &trn_de_module );
  BnkfPutM( "HXD:TRN:BLOCK",  sizeof(int), &trn_de_block );
  BnkfPutM( "HXD:TRN:RDBIN",  sizeof(int), &trn_rdbin );
  BnkfPutM( "HXD:TRN:TBLID",  sizeof(int), &trn_tblid );

  BnkfPutM( "HXD:TRN:DATA_SIZE",  sizeof(int), &trn_data_size ); /** add **/
  BnkfPutM( "HXD:TRN:SYS_ERR_CODE",sizeof(int), &trn_sys_err_code );/** add **/
  
  BnkfPutM( "HXD:TRH:BLOCK",         sizeof(int), &trn_ae_block );
  BnkfPutM( "HXD:TRH:TIME",          sizeof(int), &fits->trntime);
  BnkfPutM( "HXD:TRH:GB_TIME",       sizeof(int), &fits->trn_gb_time);
  BnkfPutM( "HXD:TRH:GB_FLG",        sizeof(int), &trn_gb_flg );
  BnkfPutM( "HXD:TRH:TIME_MODE",     sizeof(int), &trn_time_mode );
  BnkfPutM( "HXD:TRH:RBM",           sizeof(int), &trn_rbm );
  BnkfPutM( "HXD:TRH:GB_FRZ",        sizeof(int), &trn_gb_frz );
  BnkfPutM( "HXD:TRH:DT_MODE",       sizeof(int), &trn_dt_mode );
  BnkfPutM( "HXD:TRH:SUMLD_MODE",    sizeof(int), &trn_sumld_mode );
  BnkfPutM( "HXD:TRH:BOARD",         sizeof(int), &trn_ae_module );
  BnkfPutM( "HXD:TRH:GB_TRG",        sizeof(int), &trn_gb_trg );
  
  BnkfPutM( "HXD:TRB:PI",            sizeof(int)*HXD_TRN_MAX_ENE_BIN, trn_pi);
  
  BnkfPutM( "HXD:TRB:PH",            sizeof(int)*HXD_TRN_MAX_ENE_BIN, trn_ph);
  BnkfPutM( "HXD:TRB:OVER_FLOW",     sizeof(int), &trn_over_flow );
  BnkfPutM( "HXD:TRB:PSEUDO",        sizeof(int), &trn_pseudo );
  BnkfPutM( "HXD:TRB:TRN_ANT",       sizeof(int)*5, trn_t_ant );
  BnkfPutM( "HXD:TRB:UD",            sizeof(int), &trn_ud );
  BnkfPutM( "HXD:TRB:DEAD_TIME",     sizeof(int), &trn_dead_time );
  BnkfPutM( "HXD:TRB:SUM_LD",        sizeof(int), &trn_sum_ld );
  BnkfPutM( "HXD:TRB:WELL_ANT",      sizeof(int)*4, trn_w_ant );  

  BnkfPutM( "HXD:TRN:TRN_QUALITY",      sizeof(int), &trn_quality);  
  
}

void hxdtrnFitsToBnk_get( HxdTrnFits *fits ){

  int size;
  
  int i;
  
  int trn_iblock;  
  int trn_ae_de_length_chk;
  int trn_de_module;
  int trn_de_block;
  int trn_rdbin;
  int trn_tblid;
  int trn_data_size;    /** add **/
  int trn_sys_err_code; /** add **/
  int trn_ae_block;
  int trn_gb_flg;
  int trn_time_mode;
  int trn_rbm;
  int trn_gb_frz;
  int trn_dt_mode;
  int trn_sumld_mode;
  int trn_ae_module;
  int trn_gb_trg;
  int trn_ph[HXD_TRN_MAX_ENE_BIN];
  int trn_over_flow;
  int trn_pseudo;
  int trn_t_ant[5];
  int trn_ud;
  int trn_dead_time;
  int trn_sum_ld;
  int trn_w_ant[4];
  int trn_quality;

  /** Get time, ti, eti in HXDeventFitsWrite **/
  BnkfGetM("HXD:TRN:PACKET_S_TIME", sizeof(double), &size, &fits->s_time);
  fits->aetime = fits->s_time;
  
  BnkfGetM("HXD:TRH:TIME", sizeof(int), &size, &fits->trntime);
  BnkfGetM("HXD:TRH:GB_TIME", sizeof(int), &size, &fits->trn_gb_time);
  
  BnkfGetM("HXD:TRB:IBLOCK", sizeof(int), &size, &trn_iblock );
  BnkfGetM("HXD:TRN:AE_DE_Len_Err", sizeof(int), &size,
           &trn_ae_de_length_chk );
  BnkfGetM("HXD:TRN:BOARD", sizeof(int), &size, &trn_de_module );
  BnkfGetM("HXD:TRN:BLOCK", sizeof(int), &size, &trn_de_block );
  BnkfGetM("HXD:TRN:RDBIN", sizeof(int), &size, &trn_rdbin );
  BnkfGetM("HXD:TRN:TBLID", sizeof(int), &size, &trn_tblid );

  BnkfGetM("HXD:TRN:DATA_SIZE",sizeof(int), &size, &trn_data_size );
  BnkfGetM("HXD:TRN:SYS_ERR_CODE",sizeof(int), &size, &trn_sys_err_code );
  /** add (Put in HXDeventExtruct) **/

  BnkfGetM("HXD:TRH:BLOCK", sizeof(int), &size, &trn_ae_block );
  BnkfGetM("HXD:TRH:GB_FLG", sizeof(int), &size, &trn_gb_flg );
  BnkfGetM("HXD:TRH:TIME_MODE", sizeof(int), &size, &trn_time_mode );
  BnkfGetM("HXD:TRH:RBM", sizeof(int), &size, &trn_rbm);
  BnkfGetM("HXD:TRH:GB_FRZ", sizeof(int), &size, &trn_gb_frz );
  BnkfGetM("HXD:TRH:DT_MODE", sizeof(int), &size, &trn_dt_mode );
  BnkfGetM("HXD:TRH:SUMLD_MODE", sizeof(int), &size, &trn_sumld_mode );
  BnkfGetM("HXD:TRH:BOARD", sizeof(int), &size, &trn_ae_module );
  BnkfGetM("HXD:TRH:GB_TRG", sizeof(int), &size, &trn_gb_trg );

  BnkfGetM("HXD:TRB:PH", sizeof(int)*HXD_TRN_MAX_ENE_BIN, &size, trn_ph );
  BnkfGetM("HXD:TRB:OVER_FLOW", sizeof(int), &size, &trn_over_flow );
  BnkfGetM("HXD:TRB:PSEUDO", sizeof(int), &size, &trn_pseudo );
  BnkfGetM("HXD:TRB:TRN_ANT", sizeof(int)*5, &size, trn_t_ant );
  BnkfGetM("HXD:TRB:UD", sizeof(int), &size, &trn_ud );
  BnkfGetM("HXD:TRB:DEAD_TIME", sizeof(int), &size, &trn_dead_time );
  BnkfGetM("HXD:TRB:SUM_LD", sizeof(int), &size, &trn_sum_ld );
  BnkfGetM("HXD:TRB:WELL_ANT", sizeof(int)*4, &size, trn_w_ant );
/*
  BnkfGetM( "HXD:TRN:TRN_QUALITY", sizeof(int), &size, &trn_quality);    
*/  
  fits->trn_iblock = (short) trn_iblock;  
  fits->trn_ae_de_length_chk = (unsigned char) trn_ae_de_length_chk;
  fits->trn_de_module = (unsigned char) trn_de_module;
  fits->trn_de_block = (unsigned char) trn_de_block;
  fits->trn_rdbin = (unsigned char) trn_rdbin;
  fits->trn_tblid = (unsigned char) trn_tblid;
  fits->trn_data_size = (unsigned short) trn_data_size;
  fits->trn_sys_err_code = (unsigned char) trn_sys_err_code;
  fits->trn_ae_block = (unsigned char) trn_ae_block;
  fits->trn_gb_flg = (unsigned char) trn_gb_flg;
  fits->trn_time_mode = (unsigned char) trn_time_mode;
  fits->trn_rbm = (unsigned char) trn_rbm;
  fits->trn_gb_frz = (unsigned char) trn_gb_frz;
  fits->trn_dt_mode = (unsigned char) trn_dt_mode;
  fits->trn_sumld_mode = (unsigned char) trn_sumld_mode;
  fits->trn_ae_module = (unsigned char) trn_ae_module;
  fits->trn_gb_trg = (unsigned char) trn_gb_trg;
  fits->trn_over_flow = (unsigned short) trn_over_flow;
  fits->trn_pseudo = (unsigned short) trn_pseudo;
  fits->trn_t_ant0 = (unsigned short) trn_t_ant[0];
  fits->trn_t_ant1 = (unsigned short) trn_t_ant[1];
  fits->trn_t_ant2 = (unsigned short) trn_t_ant[2];
  fits->trn_t_ant3 = (unsigned short) trn_t_ant[3];
  fits->trn_t_ant4 = (unsigned short) trn_t_ant[4];
  fits->trn_ud = (unsigned short) trn_ud;
  fits->trn_dead_time = (unsigned short) trn_dead_time;
  fits->trn_sum_ld = (unsigned short) trn_sum_ld;
  fits->trn_w_ant0 = (unsigned short) trn_w_ant[0];
  fits->trn_w_ant1 = (unsigned short) trn_w_ant[1];
  fits->trn_w_ant2 = (unsigned short) trn_w_ant[2];
  fits->trn_w_ant3 = (unsigned short) trn_w_ant[3];    
  
  for(i=0;i<HXD_TRN_MAX_ENE_BIN;i++){
    fits->trn_ph[i] = (unsigned short) trn_ph[i];
  }
/*  
  fits->trn_quality = (unsigned short) trn_quality;
*/
}





