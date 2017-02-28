#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "fitsio.h"
#include "cfortran.h"

#include "HXD.h"

enum {
  HK=2,SYS,ACU,SCL,PWH,RHK
};
enum {
  STM=2,PPR,PST,AET_HC,AET_SC
};
enum {
  MEM_DMP=2,ECC_DMP,IO_DMP,RECC_DMP,RIO_DMP
};
enum {
  SFC=2,SFF1,SFF2,DLT,SP_PMT,SP_PIN
};

enum {
  HXD_DE_MODE,
  HXD_NMIST,
  HXD_SYSST,
  HXD_CMD_ANS_BK0,
  HXD_CMD_ANS_BK1,
  HXD_CMD_ANS_BK2,
  HXD_CMD_ANS_BK3,
  HXD_CMD_ANS_BK4,
  HXD_CMD_ANS_BK5,
  HXD_CMD_ANS_BK6,
  HXD_RSV_CMD_LENG,
  HXD_RSV_CMD_CNT,
  HXD_CMD_REJ_COD,
  HXD_CMD_REJ_CNT,
  HXD_HXD_DIS,
  HXD_DTRATE,
  HXD_SW_VERSION,
  HXD_DP_CNTL_ERR,
  HXD_TLM_ERR,
  HXD_CMD_ERR,
  HXD_LM_1B_EC,
  HXD_LM_2B_EC,
  HXD_DB_1B_EC,
  HXD_DB_2B_EC,
  HXD_ERR_COD,
  HXD_ERR_LOG_CNT,
  HXD_ERR_TSK_INF,
  HXD_ILL_INT_CNT,
  HXD_AE_HK_FAIL,
  HXD_ACU_SW_ERR,
  HXD_TPU_SW_ERR,
  HXD_WPU_SW_ERR,
  HXD_DATA_CPY_ERR,
  HXD_ACU_HW_ERR,
  HXD_TPU_HW_ERR,
  HXD_WPU_HW_ERR,
  HXD_MP_ADRS,
  HXD_MP_ERR_ADRS,
  HXD_LM_2B_E_ADRS,
  HXD_DMA_2B_E_ADRS,
  HXD_RAM_DUMP,
  HXD_PROMWT,
  HXD_PROM_PTL,
  HXD_MCHK_ADRS,
  HXD_ROM_1B_EC,
  HXD_ROM_2B_EC,
  HXD_ROMPA_ADRS,
  HXD_ROM_1B_E_ADRS,
  HXD_ROM_2B_E_ADRS,
  HXD_GTST,
  HXD_AECMD_CODE,
  HXD_AECMD_CNT,
  HXD_AECMD_REJ_CODE,
  HXD_AECMD_REJ_CNT,
  HXD_PI_ERR_CODE,
  HXD_PI_ERR_CNT,
  HXD_PI_ACUCMD_ENA,
  HXD_PI_STATUS,
  HXD_WPU1_ST,
  HXD_WPU0_ST,
  HXD_WPU3_ST,
  HXD_WPU2_ST,
  HXD_TPU1_ST,
  HXD_TPU0_ST,
  HXD_TPU3_ST,
  HXD_TPU2_ST,
  HXD_HKA_HV_ENA_DIS,
  HXD_HV_W1_ST,
  HXD_HV_W0_ST,
  HXD_HV_W3_ST,
  HXD_HV_W2_ST,
  HXD_HV_T1_ST,
  HXD_HV_T0_ST,
  HXD_HV_T3_ST,
  HXD_HV_T2_ST,
  HXD_HV_P1_ST,
  HXD_HV_P0_ST,
  HXD_HV_P3_ST,
  HXD_HV_P2_ST,
  HXD_ACU_HK_DATAID,
  HXD_PSU_VOLT,
  HXD_PSU_P5VD_CAL,
  HXD_PSU_P12V_CAL,
  HXD_PSU_P5VA_CAL,
  HXD_PSU_M12V_CAL,
  HXD_PSU_M5VA_CAL,
  HXD_ACU_HK_TIME,
  HXD_AE_TM_LATCH_TM,
  HXD_SENSOR_TEMP0,
  HXD_SENSOR_TEMP1,
  HXD_SENSOR_TEMP2,
  HXD_SENSOR_TEMP3,
  HXD_SENSOR_TEMP4,
  HXD_SENSOR_TEMP5,
  HXD_SENSOR_TEMP6,
  HXD_SENSOR_TEMP7,
  HXD_SENSOR_TEMP8,
  HXD_SENSOR_TEMP9,
  HXD_SENSOR_TEMPA,
  HXD_SENSOR_TEMPB,
  HXD_SENSOR_TEMPC,
  HXD_SENSOR_TEMPD,
  HXD_SENSOR_TEMPE,
  HXD_SENSOR_TEMPF,
  HXD_TEMP_W10_CAL,
  HXD_TEMP_W11_CAL,
  HXD_TEMP_W12_CAL,
  HXD_TEMP_W13_CAL,
  HXD_TEMP_W20_CAL,
  HXD_TEMP_W21_CAL,
  HXD_TEMP_W22_CAL,
  HXD_TEMP_W23_CAL,
  HXD_TEMP_W00_CAL,
  HXD_TEMP_W01_CAL,
  HXD_TEMP_W02_CAL,
  HXD_TEMP_W03_CAL,
  HXD_TEMP_W30_CAL,
  HXD_TEMP_W31_CAL,
  HXD_TEMP_W32_CAL,
  HXD_TEMP_W33_CAL,
  HXD_TEMP_T10_CAL,
  HXD_TEMP_T12_CAL,
  HXD_TEMP_T14_CAL,
  HXD_TEMP_T21_CAL,
  HXD_TEMP_T23_CAL,
  HXD_TEMP_HV_W2_CAL,
  HXD_TEMP_HV_P1_CAL,
  HXD_TEMP_HV_T1_CAL,
  HXD_TEMP_T00_CAL,
  HXD_TEMP_T02_CAL,
  HXD_TEMP_T04_CAL,
  HXD_TEMP_T31_CAL,
  HXD_TEMP_T33_CAL,
  HXD_TEMP_HV_W0_CAL,
  HXD_TEMP_HV_P0_CAL,
  HXD_TEMP_HV_T3_CAL,
  HXD_TEMP_CAP4_CAL,
  HXD_TEMP_CAP3_CAL,
  HXD_TEMP_BODY4_CAL,
  HXD_TEMP_BODY3_CAL,
  HXD_TEMP_BTM3_CAL,
  HXD_TEMP_BTM4_CAL,
  HXD_TEMP_BAR3_CAL,
  HXD_TEMP_CENTER_CAL,
  HXD_TEMP_CAP2_CAL,
  HXD_TEMP_CAP1_CAL,
  HXD_TEMP_BODY2_CAL,
  HXD_TEMP_BODY1_CAL,
  HXD_TEMP_BTM1_CAL,
  HXD_TEMP_BTM2_CAL,
  HXD_TEMP_BAR1_CAL,
  HXD_TEMP_BAR2_CAL,
  HXD_AE_STATUS,
  HXD_WPU_CLK_RATE,
  HXD_HV_REF_W0,
  HXD_HV_REF_W1,
  HXD_HV_REF_W2,
  HXD_HV_REF_W3,
  HXD_HV_REF_T0,
  HXD_HV_REF_T1,
  HXD_HV_REF_T2,
  HXD_HV_REF_T3,
  HXD_HV_REF_P0,
  HXD_HV_REF_P1,
  HXD_HV_REF_P2,
  HXD_HV_REF_P3,
  HXD_HV_REF,
  HXD_HV_W0_VOLT,
  HXD_HV_W1_VOLT,
  HXD_HV_W2_VOLT,
  HXD_HV_W3_VOLT,
  HXD_HV_T0_VOLT,
  HXD_HV_T1_VOLT,
  HXD_HV_T2_VOLT,
  HXD_HV_T3_VOLT,
  HXD_HV_P0_VOLT,
  HXD_HV_P1_VOLT,
  HXD_HV_P2_VOLT,
  HXD_HV_P3_VOLT,
  HXD_HV_W0_CAL,
  HXD_HV_W1_CAL,
  HXD_HV_W2_CAL,
  HXD_HV_W3_CAL,
  HXD_HV_T0_CAL,
  HXD_HV_T1_CAL,
  HXD_HV_T2_CAL,
  HXD_HV_T3_CAL,
  HXD_HV_P0_CAL,
  HXD_HV_P1_CAL,
  HXD_HV_P2_CAL,
  HXD_HV_P3_CAL,
  HXD_WPU_ID,
  HXD_TMSTAMP_SEQNO,
  HXD_PIN_UD_MOD,
  HXD_TIME_STAMP,
  HXD_WPU_HK_PROC_TM,
  HXD_GMBURST_FLAG_0,
  HXD_GMBURST_FLAG_1,
  HXD_GMBURST_FLAG_2,
  HXD_GMBURST_FLAG_3,
  HXD_GMBURST_FRZ_0,
  HXD_GMBURST_FRZ_1,
  HXD_GMBURST_FRZ_2,
  HXD_GMBURST_FRZ_3,
  HXD_RBM_FLAG_0,
  HXD_RBM_FLAG_1,
  HXD_RBM_FLAG_2,
  HXD_RBM_FLAG_3,
  HXD_TPU0_BLK_CNT,
  HXD_TPU1_BLK_CNT,
  HXD_TPU2_BLK_CNT,
  HXD_TPU3_BLK_CNT,
  HXD_HKA_TRN_LD00,
  HXD_HKA_TRN_LD01,
  HXD_HKA_TRN_LD02,
  HXD_HKA_TRN_LD03,
  HXD_HKA_TRN_LD04,
  HXD_HKA_TRN_LD10,
  HXD_HKA_TRN_LD11,
  HXD_HKA_TRN_LD12,
  HXD_HKA_TRN_LD13,
  HXD_HKA_TRN_LD14,
  HXD_HKA_TRN_LD20,
  HXD_HKA_TRN_LD21,
  HXD_HKA_TRN_LD22,
  HXD_HKA_TRN_LD23,
  HXD_HKA_TRN_LD24,
  HXD_HKA_TRN_LD30,
  HXD_HKA_TRN_LD31,
  HXD_HKA_TRN_LD32,
  HXD_HKA_TRN_LD33,
  HXD_HKA_TRN_LD34,
  HXD_HKA_WEL_A_LD00,
  HXD_HKA_WEL_A_LD01,
  HXD_HKA_WEL_A_LD02,
  HXD_HKA_WEL_A_LD03,
  HXD_HKA_WEL_A_LD10,
  HXD_HKA_WEL_A_LD11,
  HXD_HKA_WEL_A_LD12,
  HXD_HKA_WEL_A_LD13,
  HXD_HKA_WEL_A_LD20,
  HXD_HKA_WEL_A_LD21,
  HXD_HKA_WEL_A_LD22,
  HXD_HKA_WEL_A_LD23,
  HXD_HKA_WEL_A_LD30,
  HXD_HKA_WEL_A_LD31,
  HXD_HKA_WEL_A_LD32,
  HXD_HKA_WEL_A_LD33,
  HXD_HKA_WEL_LD00,
  HXD_HKA_WEL_LD01,
  HXD_HKA_WEL_LD02,
  HXD_HKA_WEL_LD03,
  HXD_HKA_WEL_LD10,
  HXD_HKA_WEL_LD11,
  HXD_HKA_WEL_LD12,
  HXD_HKA_WEL_LD13,
  HXD_HKA_WEL_LD20,
  HXD_HKA_WEL_LD21,
  HXD_HKA_WEL_LD22,
  HXD_HKA_WEL_LD23,
  HXD_HKA_WEL_LD30,
  HXD_HKA_WEL_LD31,
  HXD_HKA_WEL_LD32,
  HXD_HKA_WEL_LD33,
  HXD_HKA_PINLD00,
  HXD_HKA_PINLD01,
  HXD_HKA_PINLD02,
  HXD_HKA_PINLD03,
  HXD_HKA_PINLD10,
  HXD_HKA_PINLD11,
  HXD_HKA_PINLD12,
  HXD_HKA_PINLD13,
  HXD_HKA_PINLD20,
  HXD_HKA_PINLD21,
  HXD_HKA_PINLD22,
  HXD_HKA_PINLD23,
  HXD_HKA_PINLD30,
  HXD_HKA_PINLD31,
  HXD_HKA_PINLD32,
  HXD_HKA_PINLD33,
  HXD_HKA_WELUD00,
  HXD_HKA_WELUD01,
  HXD_HKA_WELUD02,
  HXD_HKA_WELUD03,
  HXD_HKA_WELUD10,
  HXD_HKA_WELUD11,
  HXD_HKA_WELUD12,
  HXD_HKA_WELUD13,
  HXD_HKA_WELUD20,
  HXD_HKA_WELUD21,
  HXD_HKA_WELUD22,
  HXD_HKA_WELUD23,
  HXD_HKA_WELUD30,
  HXD_HKA_WELUD31,
  HXD_HKA_WELUD32,
  HXD_HKA_WELUD33,
  HXD_HKA_PINUD00,
  HXD_HKA_PINUD01,
  HXD_HKA_PINUD02,
  HXD_HKA_PINUD03,
  HXD_HKA_PINUD10,
  HXD_HKA_PINUD11,
  HXD_HKA_PINUD12,
  HXD_HKA_PINUD13,
  HXD_HKA_PINUD20,
  HXD_HKA_PINUD21,
  HXD_HKA_PINUD22,
  HXD_HKA_PINUD23,
  HXD_HKA_PINUD30,
  HXD_HKA_PINUD31,
  HXD_HKA_PINUD32,
  HXD_HKA_PINUD33,
  HXD_HKA_W00_DT,
  HXD_HKA_W01_DT,
  HXD_HKA_W02_DT,
  HXD_HKA_W03_DT,
  HXD_HKA_W10_DT,
  HXD_HKA_W11_DT,
  HXD_HKA_W12_DT,
  HXD_HKA_W13_DT,
  HXD_HKA_W20_DT,
  HXD_HKA_W21_DT,
  HXD_HKA_W22_DT,
  HXD_HKA_W23_DT,
  HXD_HKA_W30_DT,
  HXD_HKA_W31_DT,
  HXD_HKA_W32_DT,
  HXD_HKA_W33_DT,
};

static char pname[] = "HXDHKFitsReadHK";

static int colnum[328];
static int time_colnum;

void
HXDHKFitsReadHK_init()
{
  BnkDef( "HXD:HK:PACKET_AETIME", sizeof(double) );
  BnkDef( "HXD:HKD:DEST", sizeof(int) );
  BnkDef( "HXD:HKD:NMIST", sizeof(int) );
  BnkDef( "HXD:HKD:SYST", sizeof(int) );
  BnkDef( "HXD:HKD:CABK", sizeof(int)*7 );
  BnkDef( "HXD:HKD:RCLENG", sizeof(int) );
  BnkDef( "HXD:HKD:RCCNT", sizeof(int) );
  BnkDef( "HXD:HKD:CREJCOD", sizeof(int) );
  BnkDef( "HXD:HKD:CRECNT", sizeof(int) );
  BnkDef( "HXD:HKD:HXDDIS", sizeof(int) );
  BnkDef( "HXD:HKD:DTRATE", sizeof(int) );
  BnkDef( "HXD:HKD:SW_VER", sizeof(int) );
  BnkDef( "HXD:HKD:CNTL_DMAERR", sizeof(int) );
  BnkDef( "HXD:HKD:TLM_DMAERR", sizeof(int) );
  BnkDef( "HXD:HKD:CMD_DMAERR", sizeof(int) );
  BnkDef( "HXD:HKD:LM1BEC", sizeof(int) );
  BnkDef( "HXD:HKD:LM2BEC", sizeof(int) );
  BnkDef( "HXD:HKD:DB1BEC", sizeof(int) );
  BnkDef( "HXD:HKD:DB2BEC", sizeof(int) );
  BnkDef( "HXD:HKD:ERRCOD", sizeof(int) );
  BnkDef( "HXD:HKD:ERRLOGC", sizeof(int) );
  BnkDef( "HXD:HKD:ERRTINF", sizeof(int) );
  BnkDef( "HXD:HKD:ILINTC", sizeof(int) );
  BnkDef( "HXD:HKD:AE_HK_FAIL", sizeof(int) );
  BnkDef( "HXD:HKD:ACU_SW_ERR", sizeof(int) );
  BnkDef( "HXD:HKD:TPU_SW_ERR", sizeof(int) );
  BnkDef( "HXD:HKD:WPU_SW_ERR", sizeof(int) );
  BnkDef( "HXD:HKD:DATA_CPY_ERR", sizeof(int) );
  BnkDef( "HXD:HKD:ACU_HW_ERR", sizeof(int) );
  BnkDef( "HXD:HKD:TPU_HW_ERR", sizeof(int) );
  BnkDef( "HXD:HKD:WPU_HW_ERR", sizeof(int) );
  BnkDef( "HXD:HKD:MPADR", sizeof(int) );
  BnkDef( "HXD:HKD:MPEA", sizeof(int) );
  BnkDef( "HXD:HKD:LM2BA", sizeof(int) );
  BnkDef( "HXD:HKD:DM2BA", sizeof(int) );
  BnkDef( "HXD:HKD:RAM_DUMP", sizeof(int) );
  BnkDef( "HXD:HKD:PROMWT", sizeof(int) );
  BnkDef( "HXD:HKD:PROM_PTL", sizeof(int) );
  BnkDef( "HXD:HKD:MCHAD", sizeof(int) );
  BnkDef( "HXD:HKD:ROM1BEC", sizeof(int) );
  BnkDef( "HXD:HKD:ROM2BEC", sizeof(int) );
  BnkDef( "HXD:HKD:ROMPA", sizeof(int) );
  BnkDef( "HXD:HKD:ROM1BEA", sizeof(int) );
  BnkDef( "HXD:HKD:ROM2BEA", sizeof(int) );
  BnkDef( "HXD:HKD:WEVTGT", sizeof(int) );
  BnkDef( "HXD:HKD:AECCDE", sizeof(int) );
  BnkDef( "HXD:HKD:AECCNT", sizeof(int) );
  BnkDef( "HXD:HKD:AECRCDE", sizeof(int) );
  BnkDef( "HXD:HKD:AECRCNT", sizeof(int) );
  BnkDef( "HXD:HKD:PIERRCODE", sizeof(int) );
  BnkDef( "HXD:HKD:PIERRCNT", sizeof(int) );
  BnkDef( "HXD:HKD:PIACUCMD", sizeof(int) );
  BnkDef( "HXD:HKD:PISTATUS", sizeof(int) );
  BnkDef( "HXD:HKA:WPU_ON/OF", sizeof(int)*4 );
  BnkDef( "HXD:HKA:TPU_ON/OF", sizeof(int)*4 );
  BnkDef( "HXD:HKA:HV_ENA/DIS", sizeof(int) );
  BnkDef( "HXD:HKA:HVW_ON/OF", sizeof(int)*4 );
  BnkDef( "HXD:HKA:HVT_ON/OF", sizeof(int)*4 );
  BnkDef( "HXD:HKA:HVP_ON/OF", sizeof(int)*4 );
  BnkDef( "HXD:HKA:DATA_ID", sizeof(int) );
  BnkDef( "HXD:HKA:PSU_VOL", sizeof(int) );
  BnkDef( "HXD:HKA:0:PSU_VOLT", sizeof(double) );
  BnkDef( "HXD:HKA:1:PSU_VOLT", sizeof(double) );
  BnkDef( "HXD:HKA:2:PSU_VOLT", sizeof(double) );
  BnkDef( "HXD:HKA:3:PSU_VOLT", sizeof(double) );
  BnkDef( "HXD:HKA:4:PSU_VOLT", sizeof(double) );
  BnkDef( "HXD:HKA:HKTIME", sizeof(int) );
  BnkDef( "HXD:HKA:LATCH_TIM", sizeof(int) );
  BnkDef( "HXD:HKA:TEMP", sizeof(int)*16 );
  BnkDef( "HXD:HKA:0:4:TEMP", sizeof(double)*16 );
  BnkDef( "HXD:HKA:1:5:TEMP", sizeof(double)*16 );
  BnkDef( "HXD:HKA:2:6:TEMP", sizeof(double)*16 );
  BnkDef( "HXD:HKA:AE_STATUS", sizeof(int) );
  BnkDef( "HXD:HKA:WPU_CLK_RATE", sizeof(int) );
  BnkDef( "HXD:HKA:HVREF_W", sizeof(int)*4 );
  BnkDef( "HXD:HKA:HVREF_T", sizeof(int)*4 );
  BnkDef( "HXD:HKA:HVREF_P", sizeof(int)*4 );
  BnkDef( "HXD:HKA:HVREF_DATA", sizeof(int) );
  BnkDef( "HXD:HKA:HV_W", sizeof(int)*4 );
  BnkDef( "HXD:HKA:HV_T", sizeof(int)*4 );
  BnkDef( "HXD:HKA:HV_P", sizeof(int)*4 );
  BnkDef( "HXD:HKA:HV_W:real", sizeof(double)*4 );
  BnkDef( "HXD:HKA:HV_T:real", sizeof(double)*4 );
  BnkDef( "HXD:HKA:HV_P:real", sizeof(double)*4 );
  BnkDef( "HXD:HKA:BOARD", sizeof(int) );
  BnkDef( "HXD:HKA:STMP_SEQ", sizeof(int) );
  BnkDef( "HXD:HKA:PUD_MODE", sizeof(int) );
  BnkDef( "HXD:HKA:STMP_TIM", sizeof(int) );
  BnkDef( "HXD:HKA:WHKPROC_TM", sizeof(int) );
  BnkDef( "HXD:HKA:GB_FLG", sizeof(int)*4 );
  BnkDef( "HXD:HKA:GB_FRZ", sizeof(int)*4 );
  BnkDef( "HXD:HKA:RBM_FLG", sizeof(int)*4 );
  BnkDef( "HXD:HKA:TRN_BLCK", sizeof(int)*4 );
  BnkDef( "HXD:HKA:TRN/A_BD_LD0", sizeof(int)*5 );
  BnkDef( "HXD:HKA:TRN/A_BD_LD1", sizeof(int)*5 );
  BnkDef( "HXD:HKA:TRN/A_BD_LD2", sizeof(int)*5 );
  BnkDef( "HXD:HKA:TRN/A_BD_LD3", sizeof(int)*5 );
  BnkDef( "HXD:HKA:WEL/A_BD_LD0", sizeof(int)*4 );
  BnkDef( "HXD:HKA:WEL/A_BD_LD1", sizeof(int)*4 );
  BnkDef( "HXD:HKA:WEL/A_BD_LD2", sizeof(int)*4 );
  BnkDef( "HXD:HKA:WEL/A_BD_LD3", sizeof(int)*4 );
  BnkDef( "HXD:HKA:WELBD_LD0", sizeof(int)*4 );
  BnkDef( "HXD:HKA:WELBD_LD1", sizeof(int)*4 );
  BnkDef( "HXD:HKA:WELBD_LD2", sizeof(int)*4 );
  BnkDef( "HXD:HKA:WELBD_LD3", sizeof(int)*4 );
  BnkDef( "HXD:HKA:PINBD_LD0", sizeof(int)*4 );
  BnkDef( "HXD:HKA:PINBD_LD1", sizeof(int)*4 );
  BnkDef( "HXD:HKA:PINBD_LD2", sizeof(int)*4 );
  BnkDef( "HXD:HKA:PINBD_LD3", sizeof(int)*4 );
  BnkDef( "HXD:HKA:WELBD_UD0", sizeof(int)*4 );
  BnkDef( "HXD:HKA:WELBD_UD1", sizeof(int)*4 );
  BnkDef( "HXD:HKA:WELBD_UD2", sizeof(int)*4 );
  BnkDef( "HXD:HKA:WELBD_UD3", sizeof(int)*4 );
  BnkDef( "HXD:HKA:PINBD_UD0", sizeof(int)*4 );
  BnkDef( "HXD:HKA:PINBD_UD1", sizeof(int)*4 );
  BnkDef( "HXD:HKA:PINBD_UD2", sizeof(int)*4 );
  BnkDef( "HXD:HKA:PINBD_UD3", sizeof(int)*4 );
  BnkDef( "HXD:HKA:WDTCNT0", sizeof(int)*4 );
  BnkDef( "HXD:HKA:WDTCNT1", sizeof(int)*4 );
  BnkDef( "HXD:HKA:WDTCNT2", sizeof(int)*4 );
  BnkDef( "HXD:HKA:WDTCNT3", sizeof(int)*4 );
  
}


int
HXDHKFitsReadHK_bgnrun(fitsfile *fp)
{
  int istat = 0;

  int casesen = TRUE;
  int hdutype;
  
  fits_movabs_hdu( fp, HK, &hdutype, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (%d)\n",
	    pname, istat);
    return istat;
  } else {
    if( fits_get_colnum(fp, casesen, "TIME", &time_colnum, &istat) ){
      fprintf(stderr, "%s: fits_get_colnum('TIME') failed (%d)\n",
	      pname, istat);
      return istat;
    }
  }
  
  if( fits_get_colnum(fp, casesen, "HXD_DE_MODE",
                      &colnum[HXD_DE_MODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_DE_MODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_NMIST",
                      &colnum[HXD_NMIST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_NMIST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SYSST",
                      &colnum[HXD_SYSST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SYSST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_CMD_ANS_BK0",
                      &colnum[HXD_CMD_ANS_BK0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_CMD_ANS_BK0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_CMD_ANS_BK1",
                      &colnum[HXD_CMD_ANS_BK1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_CMD_ANS_BK1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_CMD_ANS_BK2",
                      &colnum[HXD_CMD_ANS_BK2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_CMD_ANS_BK2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_CMD_ANS_BK3",
                      &colnum[HXD_CMD_ANS_BK3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_CMD_ANS_BK3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_CMD_ANS_BK4",
                      &colnum[HXD_CMD_ANS_BK4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_CMD_ANS_BK4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_CMD_ANS_BK5",
                      &colnum[HXD_CMD_ANS_BK5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_CMD_ANS_BK5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_CMD_ANS_BK6",
                      &colnum[HXD_CMD_ANS_BK6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_CMD_ANS_BK6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_RSV_CMD_LENG",
                      &colnum[HXD_RSV_CMD_LENG], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_RSV_CMD_LENG') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_RSV_CMD_CNT",
                      &colnum[HXD_RSV_CMD_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_RSV_CMD_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_CMD_REJ_COD",
                      &colnum[HXD_CMD_REJ_COD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_CMD_REJ_COD') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_CMD_REJ_CNT",
                      &colnum[HXD_CMD_REJ_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_CMD_REJ_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HXD_DIS",
                      &colnum[HXD_HXD_DIS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HXD_DIS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_DTRATE",
                      &colnum[HXD_DTRATE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_DTRATE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SW_VERSION",
                      &colnum[HXD_SW_VERSION], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SW_VERSION') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_DP_CNTL_ERR",
                      &colnum[HXD_DP_CNTL_ERR], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_DP_CNTL_ERR') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TLM_ERR",
                      &colnum[HXD_TLM_ERR], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TLM_ERR') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_CMD_ERR",
                      &colnum[HXD_CMD_ERR], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_CMD_ERR') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_LM_1B_EC",
                      &colnum[HXD_LM_1B_EC], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_LM_1B_EC') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_LM_2B_EC",
                      &colnum[HXD_LM_2B_EC], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_LM_2B_EC') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_DB_1B_EC",
                      &colnum[HXD_DB_1B_EC], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_DB_1B_EC') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_DB_2B_EC",
                      &colnum[HXD_DB_2B_EC], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_DB_2B_EC') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ERR_COD",
                      &colnum[HXD_ERR_COD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ERR_COD') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ERR_LOG_CNT",
                      &colnum[HXD_ERR_LOG_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ERR_LOG_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ERR_TSK_INF",
                      &colnum[HXD_ERR_TSK_INF], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ERR_TSK_INF') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ILL_INT_CNT",
                      &colnum[HXD_ILL_INT_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ILL_INT_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AE_HK_FAIL",
                      &colnum[HXD_AE_HK_FAIL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AE_HK_FAIL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_SW_ERR",
                      &colnum[HXD_ACU_SW_ERR], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_SW_ERR') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TPU_SW_ERR",
                      &colnum[HXD_TPU_SW_ERR], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TPU_SW_ERR') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WPU_SW_ERR",
                      &colnum[HXD_WPU_SW_ERR], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WPU_SW_ERR') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_DATA_CPY_ERR",
                      &colnum[HXD_DATA_CPY_ERR], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_DATA_CPY_ERR') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HW_ERR",
                      &colnum[HXD_ACU_HW_ERR], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HW_ERR') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TPU_HW_ERR",
                      &colnum[HXD_TPU_HW_ERR], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TPU_HW_ERR') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WPU_HW_ERR",
                      &colnum[HXD_WPU_HW_ERR], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WPU_HW_ERR') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MP_ADRS",
                      &colnum[HXD_MP_ADRS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MP_ADRS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MP_ERR_ADRS",
                      &colnum[HXD_MP_ERR_ADRS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MP_ERR_ADRS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_LM_2B_E_ADRS",
                      &colnum[HXD_LM_2B_E_ADRS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_LM_2B_E_ADRS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_DMA_2B_E_ADRS",
                      &colnum[HXD_DMA_2B_E_ADRS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_DMA_2B_E_ADRS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_RAM_DUMP",
                      &colnum[HXD_RAM_DUMP], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_RAM_DUMP') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PROMWT",
                      &colnum[HXD_PROMWT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PROMWT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PROM_PTL",
                      &colnum[HXD_PROM_PTL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PROM_PTL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_MCHK_ADRS",
                      &colnum[HXD_MCHK_ADRS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_MCHK_ADRS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_1B_EC",
                      &colnum[HXD_ROM_1B_EC], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_1B_EC') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_2B_EC",
                      &colnum[HXD_ROM_2B_EC], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_2B_EC') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROMPA_ADRS",
                      &colnum[HXD_ROMPA_ADRS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROMPA_ADRS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_1B_E_ADRS",
                      &colnum[HXD_ROM_1B_E_ADRS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_1B_E_ADRS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ROM_2B_E_ADRS",
                      &colnum[HXD_ROM_2B_E_ADRS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ROM_2B_E_ADRS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GTST",
                      &colnum[HXD_GTST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GTST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AECMD_CODE",
                      &colnum[HXD_AECMD_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AECMD_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AECMD_CNT",
                      &colnum[HXD_AECMD_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AECMD_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AECMD_REJ_CODE",
                      &colnum[HXD_AECMD_REJ_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AECMD_REJ_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AECMD_REJ_CNT",
                      &colnum[HXD_AECMD_REJ_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AECMD_REJ_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PI_ERR_CODE",
                      &colnum[HXD_PI_ERR_CODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PI_ERR_CODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PI_ERR_CNT",
                      &colnum[HXD_PI_ERR_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PI_ERR_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PI_ACUCMD_ENA",
                      &colnum[HXD_PI_ACUCMD_ENA], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PI_ACUCMD_ENA') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PI_STATUS",
                      &colnum[HXD_PI_STATUS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PI_STATUS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WPU1_ST",
                      &colnum[HXD_WPU1_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WPU1_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WPU0_ST",
                      &colnum[HXD_WPU0_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WPU0_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WPU3_ST",
                      &colnum[HXD_WPU3_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WPU3_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WPU2_ST",
                      &colnum[HXD_WPU2_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WPU2_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TPU1_ST",
                      &colnum[HXD_TPU1_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TPU1_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TPU0_ST",
                      &colnum[HXD_TPU0_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TPU0_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TPU3_ST",
                      &colnum[HXD_TPU3_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TPU3_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TPU2_ST",
                      &colnum[HXD_TPU2_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TPU2_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_HV_ENA_DIS",
                      &colnum[HXD_HKA_HV_ENA_DIS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_HV_ENA_DIS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_W1_ST",
                      &colnum[HXD_HV_W1_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_W1_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_W0_ST",
                      &colnum[HXD_HV_W0_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_W0_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_W3_ST",
                      &colnum[HXD_HV_W3_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_W3_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_W2_ST",
                      &colnum[HXD_HV_W2_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_W2_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_T1_ST",
                      &colnum[HXD_HV_T1_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_T1_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_T0_ST",
                      &colnum[HXD_HV_T0_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_T0_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_T3_ST",
                      &colnum[HXD_HV_T3_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_T3_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_T2_ST",
                      &colnum[HXD_HV_T2_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_T2_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_P1_ST",
                      &colnum[HXD_HV_P1_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_P1_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_P0_ST",
                      &colnum[HXD_HV_P0_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_P0_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_P3_ST",
                      &colnum[HXD_HV_P3_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_P3_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_P2_ST",
                      &colnum[HXD_HV_P2_ST], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_P2_ST') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HK_DATAID",
                      &colnum[HXD_ACU_HK_DATAID], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HK_DATAID') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PSU_VOLT",
                      &colnum[HXD_PSU_VOLT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PSU_VOLT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PSU_P5VD_CAL",
                      &colnum[HXD_PSU_P5VD_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PSU_P5VD_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PSU_P12V_CAL",
                      &colnum[HXD_PSU_P12V_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PSU_P12V_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PSU_P5VA_CAL",
                      &colnum[HXD_PSU_P5VA_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PSU_P5VA_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PSU_M12V_CAL",
                      &colnum[HXD_PSU_M12V_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PSU_M12V_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PSU_M5VA_CAL",
                      &colnum[HXD_PSU_M5VA_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PSU_M5VA_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_ACU_HK_TIME",
                      &colnum[HXD_ACU_HK_TIME], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_ACU_HK_TIME') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AE_TM_LATCH_TM",
                      &colnum[HXD_AE_TM_LATCH_TM], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AE_TM_LATCH_TM') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SENSOR_TEMP0",
                      &colnum[HXD_SENSOR_TEMP0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SENSOR_TEMP0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SENSOR_TEMP1",
                      &colnum[HXD_SENSOR_TEMP1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SENSOR_TEMP1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SENSOR_TEMP2",
                      &colnum[HXD_SENSOR_TEMP2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SENSOR_TEMP2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SENSOR_TEMP3",
                      &colnum[HXD_SENSOR_TEMP3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SENSOR_TEMP3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SENSOR_TEMP4",
                      &colnum[HXD_SENSOR_TEMP4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SENSOR_TEMP4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SENSOR_TEMP5",
                      &colnum[HXD_SENSOR_TEMP5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SENSOR_TEMP5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SENSOR_TEMP6",
                      &colnum[HXD_SENSOR_TEMP6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SENSOR_TEMP6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SENSOR_TEMP7",
                      &colnum[HXD_SENSOR_TEMP7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SENSOR_TEMP7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SENSOR_TEMP8",
                      &colnum[HXD_SENSOR_TEMP8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SENSOR_TEMP8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SENSOR_TEMP9",
                      &colnum[HXD_SENSOR_TEMP9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SENSOR_TEMP9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SENSOR_TEMPA",
                      &colnum[HXD_SENSOR_TEMPA], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SENSOR_TEMPA') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SENSOR_TEMPB",
                      &colnum[HXD_SENSOR_TEMPB], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SENSOR_TEMPB') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SENSOR_TEMPC",
                      &colnum[HXD_SENSOR_TEMPC], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SENSOR_TEMPC') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SENSOR_TEMPD",
                      &colnum[HXD_SENSOR_TEMPD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SENSOR_TEMPD') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SENSOR_TEMPE",
                      &colnum[HXD_SENSOR_TEMPE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SENSOR_TEMPE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_SENSOR_TEMPF",
                      &colnum[HXD_SENSOR_TEMPF], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_SENSOR_TEMPF') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_W10_CAL",
                      &colnum[HXD_TEMP_W10_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_W10_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_W11_CAL",
                      &colnum[HXD_TEMP_W11_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_W11_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_W12_CAL",
                      &colnum[HXD_TEMP_W12_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_W12_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_W13_CAL",
                      &colnum[HXD_TEMP_W13_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_W13_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_W20_CAL",
                      &colnum[HXD_TEMP_W20_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_W20_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_W21_CAL",
                      &colnum[HXD_TEMP_W21_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_W21_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_W22_CAL",
                      &colnum[HXD_TEMP_W22_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_W22_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_W23_CAL",
                      &colnum[HXD_TEMP_W23_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_W23_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_W00_CAL",
                      &colnum[HXD_TEMP_W00_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_W00_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_W01_CAL",
                      &colnum[HXD_TEMP_W01_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_W01_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_W02_CAL",
                      &colnum[HXD_TEMP_W02_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_W02_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_W03_CAL",
                      &colnum[HXD_TEMP_W03_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_W03_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_W30_CAL",
                      &colnum[HXD_TEMP_W30_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_W30_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_W31_CAL",
                      &colnum[HXD_TEMP_W31_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_W31_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_W32_CAL",
                      &colnum[HXD_TEMP_W32_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_W32_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_W33_CAL",
                      &colnum[HXD_TEMP_W33_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_W33_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_T10_CAL",
                      &colnum[HXD_TEMP_T10_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_T10_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_T12_CAL",
                      &colnum[HXD_TEMP_T12_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_T12_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_T14_CAL",
                      &colnum[HXD_TEMP_T14_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_T14_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_T21_CAL",
                      &colnum[HXD_TEMP_T21_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_T21_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_T23_CAL",
                      &colnum[HXD_TEMP_T23_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_T23_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_HV_W2_CAL",
                      &colnum[HXD_TEMP_HV_W2_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_HV_W2_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_HV_P1_CAL",
                      &colnum[HXD_TEMP_HV_P1_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_HV_P1_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_HV_T1_CAL",
                      &colnum[HXD_TEMP_HV_T1_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_HV_T1_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_T00_CAL",
                      &colnum[HXD_TEMP_T00_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_T00_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_T02_CAL",
                      &colnum[HXD_TEMP_T02_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_T02_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_T04_CAL",
                      &colnum[HXD_TEMP_T04_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_T04_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_T31_CAL",
                      &colnum[HXD_TEMP_T31_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_T31_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_T33_CAL",
                      &colnum[HXD_TEMP_T33_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_T33_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_HV_W0_CAL",
                      &colnum[HXD_TEMP_HV_W0_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_HV_W0_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_HV_P0_CAL",
                      &colnum[HXD_TEMP_HV_P0_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_HV_P0_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_HV_T3_CAL",
                      &colnum[HXD_TEMP_HV_T3_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_HV_T3_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_CAP4_CAL",
                      &colnum[HXD_TEMP_CAP4_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_CAP4_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_CAP3_CAL",
                      &colnum[HXD_TEMP_CAP3_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_CAP3_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_BODY4_CAL",
                      &colnum[HXD_TEMP_BODY4_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_BODY4_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_BODY3_CAL",
                      &colnum[HXD_TEMP_BODY3_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_BODY3_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_BTM3_CAL",
                      &colnum[HXD_TEMP_BTM3_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_BTM3_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_BTM4_CAL",
                      &colnum[HXD_TEMP_BTM4_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_BTM4_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_BAR3_CAL",
                      &colnum[HXD_TEMP_BAR3_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_BAR3_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_CENTER_CAL",
                      &colnum[HXD_TEMP_CENTER_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_CENTER_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_CAP2_CAL",
                      &colnum[HXD_TEMP_CAP2_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_CAP2_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_CAP1_CAL",
                      &colnum[HXD_TEMP_CAP1_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_CAP1_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_BODY2_CAL",
                      &colnum[HXD_TEMP_BODY2_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_BODY2_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_BODY1_CAL",
                      &colnum[HXD_TEMP_BODY1_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_BODY1_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_BTM1_CAL",
                      &colnum[HXD_TEMP_BTM1_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_BTM1_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_BTM2_CAL",
                      &colnum[HXD_TEMP_BTM2_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_BTM2_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_BAR1_CAL",
                      &colnum[HXD_TEMP_BAR1_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_BAR1_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TEMP_BAR2_CAL",
                      &colnum[HXD_TEMP_BAR2_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TEMP_BAR2_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_AE_STATUS",
                      &colnum[HXD_AE_STATUS], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_AE_STATUS') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WPU_CLK_RATE",
                      &colnum[HXD_WPU_CLK_RATE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WPU_CLK_RATE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_REF_W0",
                      &colnum[HXD_HV_REF_W0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_REF_W0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_REF_W1",
                      &colnum[HXD_HV_REF_W1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_REF_W1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_REF_W2",
                      &colnum[HXD_HV_REF_W2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_REF_W2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_REF_W3",
                      &colnum[HXD_HV_REF_W3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_REF_W3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_REF_T0",
                      &colnum[HXD_HV_REF_T0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_REF_T0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_REF_T1",
                      &colnum[HXD_HV_REF_T1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_REF_T1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_REF_T2",
                      &colnum[HXD_HV_REF_T2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_REF_T2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_REF_T3",
                      &colnum[HXD_HV_REF_T3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_REF_T3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_REF_P0",
                      &colnum[HXD_HV_REF_P0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_REF_P0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_REF_P1",
                      &colnum[HXD_HV_REF_P1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_REF_P1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_REF_P2",
                      &colnum[HXD_HV_REF_P2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_REF_P2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_REF_P3",
                      &colnum[HXD_HV_REF_P3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_REF_P3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_REF",
                      &colnum[HXD_HV_REF], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_REF') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_W0_VOLT",
                      &colnum[HXD_HV_W0_VOLT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_W0_VOLT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_W1_VOLT",
                      &colnum[HXD_HV_W1_VOLT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_W1_VOLT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_W2_VOLT",
                      &colnum[HXD_HV_W2_VOLT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_W2_VOLT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_W3_VOLT",
                      &colnum[HXD_HV_W3_VOLT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_W3_VOLT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_T0_VOLT",
                      &colnum[HXD_HV_T0_VOLT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_T0_VOLT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_T1_VOLT",
                      &colnum[HXD_HV_T1_VOLT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_T1_VOLT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_T2_VOLT",
                      &colnum[HXD_HV_T2_VOLT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_T2_VOLT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_T3_VOLT",
                      &colnum[HXD_HV_T3_VOLT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_T3_VOLT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_P0_VOLT",
                      &colnum[HXD_HV_P0_VOLT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_P0_VOLT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_P1_VOLT",
                      &colnum[HXD_HV_P1_VOLT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_P1_VOLT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_P2_VOLT",
                      &colnum[HXD_HV_P2_VOLT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_P2_VOLT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_P3_VOLT",
                      &colnum[HXD_HV_P3_VOLT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_P3_VOLT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_W0_CAL",
                      &colnum[HXD_HV_W0_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_W0_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_W1_CAL",
                      &colnum[HXD_HV_W1_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_W1_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_W2_CAL",
                      &colnum[HXD_HV_W2_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_W2_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_W3_CAL",
                      &colnum[HXD_HV_W3_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_W3_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_T0_CAL",
                      &colnum[HXD_HV_T0_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_T0_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_T1_CAL",
                      &colnum[HXD_HV_T1_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_T1_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_T2_CAL",
                      &colnum[HXD_HV_T2_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_T2_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_T3_CAL",
                      &colnum[HXD_HV_T3_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_T3_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_P0_CAL",
                      &colnum[HXD_HV_P0_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_P0_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_P1_CAL",
                      &colnum[HXD_HV_P1_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_P1_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_P2_CAL",
                      &colnum[HXD_HV_P2_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_P2_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HV_P3_CAL",
                      &colnum[HXD_HV_P3_CAL], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HV_P3_CAL') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WPU_ID",
                      &colnum[HXD_WPU_ID], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WPU_ID') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TMSTAMP_SEQNO",
                      &colnum[HXD_TMSTAMP_SEQNO], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TMSTAMP_SEQNO') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PIN_UD_MOD",
                      &colnum[HXD_PIN_UD_MOD], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PIN_UD_MOD') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TIME_STAMP",
                      &colnum[HXD_TIME_STAMP], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TIME_STAMP') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_WPU_HK_PROC_TM",
                      &colnum[HXD_WPU_HK_PROC_TM], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_WPU_HK_PROC_TM') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GMBURST_FLAG_0",
                      &colnum[HXD_GMBURST_FLAG_0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GMBURST_FLAG_0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GMBURST_FLAG_1",
                      &colnum[HXD_GMBURST_FLAG_1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GMBURST_FLAG_1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GMBURST_FLAG_2",
                      &colnum[HXD_GMBURST_FLAG_2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GMBURST_FLAG_2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GMBURST_FLAG_3",
                      &colnum[HXD_GMBURST_FLAG_3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GMBURST_FLAG_3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GMBURST_FRZ_0",
                      &colnum[HXD_GMBURST_FRZ_0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GMBURST_FRZ_0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GMBURST_FRZ_1",
                      &colnum[HXD_GMBURST_FRZ_1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GMBURST_FRZ_1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GMBURST_FRZ_2",
                      &colnum[HXD_GMBURST_FRZ_2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GMBURST_FRZ_2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_GMBURST_FRZ_3",
                      &colnum[HXD_GMBURST_FRZ_3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_GMBURST_FRZ_3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_RBM_FLAG_0",
                      &colnum[HXD_RBM_FLAG_0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_RBM_FLAG_0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_RBM_FLAG_1",
                      &colnum[HXD_RBM_FLAG_1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_RBM_FLAG_1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_RBM_FLAG_2",
                      &colnum[HXD_RBM_FLAG_2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_RBM_FLAG_2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_RBM_FLAG_3",
                      &colnum[HXD_RBM_FLAG_3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_RBM_FLAG_3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TPU0_BLK_CNT",
                      &colnum[HXD_TPU0_BLK_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TPU0_BLK_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TPU1_BLK_CNT",
                      &colnum[HXD_TPU1_BLK_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TPU1_BLK_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TPU2_BLK_CNT",
                      &colnum[HXD_TPU2_BLK_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TPU2_BLK_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_TPU3_BLK_CNT",
                      &colnum[HXD_TPU3_BLK_CNT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_TPU3_BLK_CNT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD00",
                      &colnum[HXD_HKA_TRN_LD00], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD00') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD01",
                      &colnum[HXD_HKA_TRN_LD01], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD01') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD02",
                      &colnum[HXD_HKA_TRN_LD02], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD02') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD03",
                      &colnum[HXD_HKA_TRN_LD03], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD03') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD04",
                      &colnum[HXD_HKA_TRN_LD04], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD04') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD10",
                      &colnum[HXD_HKA_TRN_LD10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD11",
                      &colnum[HXD_HKA_TRN_LD11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD12",
                      &colnum[HXD_HKA_TRN_LD12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD13",
                      &colnum[HXD_HKA_TRN_LD13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD14",
                      &colnum[HXD_HKA_TRN_LD14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD20",
                      &colnum[HXD_HKA_TRN_LD20], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD20') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD21",
                      &colnum[HXD_HKA_TRN_LD21], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD21') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD22",
                      &colnum[HXD_HKA_TRN_LD22], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD22') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD23",
                      &colnum[HXD_HKA_TRN_LD23], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD23') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD24",
                      &colnum[HXD_HKA_TRN_LD24], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD24') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD30",
                      &colnum[HXD_HKA_TRN_LD30], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD30') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD31",
                      &colnum[HXD_HKA_TRN_LD31], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD31') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD32",
                      &colnum[HXD_HKA_TRN_LD32], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD32') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD33",
                      &colnum[HXD_HKA_TRN_LD33], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD33') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_TRN_LD34",
                      &colnum[HXD_HKA_TRN_LD34], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_TRN_LD34') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_A_LD00",
                      &colnum[HXD_HKA_WEL_A_LD00], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_A_LD00') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_A_LD01",
                      &colnum[HXD_HKA_WEL_A_LD01], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_A_LD01') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_A_LD02",
                      &colnum[HXD_HKA_WEL_A_LD02], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_A_LD02') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_A_LD03",
                      &colnum[HXD_HKA_WEL_A_LD03], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_A_LD03') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_A_LD10",
                      &colnum[HXD_HKA_WEL_A_LD10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_A_LD10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_A_LD11",
                      &colnum[HXD_HKA_WEL_A_LD11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_A_LD11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_A_LD12",
                      &colnum[HXD_HKA_WEL_A_LD12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_A_LD12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_A_LD13",
                      &colnum[HXD_HKA_WEL_A_LD13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_A_LD13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_A_LD20",
                      &colnum[HXD_HKA_WEL_A_LD20], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_A_LD20') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_A_LD21",
                      &colnum[HXD_HKA_WEL_A_LD21], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_A_LD21') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_A_LD22",
                      &colnum[HXD_HKA_WEL_A_LD22], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_A_LD22') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_A_LD23",
                      &colnum[HXD_HKA_WEL_A_LD23], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_A_LD23') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_A_LD30",
                      &colnum[HXD_HKA_WEL_A_LD30], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_A_LD30') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_A_LD31",
                      &colnum[HXD_HKA_WEL_A_LD31], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_A_LD31') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_A_LD32",
                      &colnum[HXD_HKA_WEL_A_LD32], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_A_LD32') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_A_LD33",
                      &colnum[HXD_HKA_WEL_A_LD33], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_A_LD33') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_LD00",
                      &colnum[HXD_HKA_WEL_LD00], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_LD00') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_LD01",
                      &colnum[HXD_HKA_WEL_LD01], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_LD01') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_LD02",
                      &colnum[HXD_HKA_WEL_LD02], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_LD02') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_LD03",
                      &colnum[HXD_HKA_WEL_LD03], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_LD03') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_LD10",
                      &colnum[HXD_HKA_WEL_LD10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_LD10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_LD11",
                      &colnum[HXD_HKA_WEL_LD11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_LD11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_LD12",
                      &colnum[HXD_HKA_WEL_LD12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_LD12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_LD13",
                      &colnum[HXD_HKA_WEL_LD13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_LD13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_LD20",
                      &colnum[HXD_HKA_WEL_LD20], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_LD20') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_LD21",
                      &colnum[HXD_HKA_WEL_LD21], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_LD21') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_LD22",
                      &colnum[HXD_HKA_WEL_LD22], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_LD22') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_LD23",
                      &colnum[HXD_HKA_WEL_LD23], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_LD23') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_LD30",
                      &colnum[HXD_HKA_WEL_LD30], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_LD30') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_LD31",
                      &colnum[HXD_HKA_WEL_LD31], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_LD31') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_LD32",
                      &colnum[HXD_HKA_WEL_LD32], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_LD32') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WEL_LD33",
                      &colnum[HXD_HKA_WEL_LD33], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WEL_LD33') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINLD00",
                      &colnum[HXD_HKA_PINLD00], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINLD00') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINLD01",
                      &colnum[HXD_HKA_PINLD01], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINLD01') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINLD02",
                      &colnum[HXD_HKA_PINLD02], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINLD02') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINLD03",
                      &colnum[HXD_HKA_PINLD03], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINLD03') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINLD10",
                      &colnum[HXD_HKA_PINLD10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINLD10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINLD11",
                      &colnum[HXD_HKA_PINLD11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINLD11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINLD12",
                      &colnum[HXD_HKA_PINLD12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINLD12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINLD13",
                      &colnum[HXD_HKA_PINLD13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINLD13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINLD20",
                      &colnum[HXD_HKA_PINLD20], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINLD20') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINLD21",
                      &colnum[HXD_HKA_PINLD21], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINLD21') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINLD22",
                      &colnum[HXD_HKA_PINLD22], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINLD22') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINLD23",
                      &colnum[HXD_HKA_PINLD23], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINLD23') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINLD30",
                      &colnum[HXD_HKA_PINLD30], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINLD30') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINLD31",
                      &colnum[HXD_HKA_PINLD31], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINLD31') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINLD32",
                      &colnum[HXD_HKA_PINLD32], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINLD32') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINLD33",
                      &colnum[HXD_HKA_PINLD33], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINLD33') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WELUD00",
                      &colnum[HXD_HKA_WELUD00], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WELUD00') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WELUD01",
                      &colnum[HXD_HKA_WELUD01], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WELUD01') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WELUD02",
                      &colnum[HXD_HKA_WELUD02], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WELUD02') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WELUD03",
                      &colnum[HXD_HKA_WELUD03], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WELUD03') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WELUD10",
                      &colnum[HXD_HKA_WELUD10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WELUD10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WELUD11",
                      &colnum[HXD_HKA_WELUD11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WELUD11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WELUD12",
                      &colnum[HXD_HKA_WELUD12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WELUD12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WELUD13",
                      &colnum[HXD_HKA_WELUD13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WELUD13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WELUD20",
                      &colnum[HXD_HKA_WELUD20], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WELUD20') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WELUD21",
                      &colnum[HXD_HKA_WELUD21], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WELUD21') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WELUD22",
                      &colnum[HXD_HKA_WELUD22], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WELUD22') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WELUD23",
                      &colnum[HXD_HKA_WELUD23], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WELUD23') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WELUD30",
                      &colnum[HXD_HKA_WELUD30], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WELUD30') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WELUD31",
                      &colnum[HXD_HKA_WELUD31], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WELUD31') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WELUD32",
                      &colnum[HXD_HKA_WELUD32], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WELUD32') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_WELUD33",
                      &colnum[HXD_HKA_WELUD33], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_WELUD33') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINUD00",
                      &colnum[HXD_HKA_PINUD00], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINUD00') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINUD01",
                      &colnum[HXD_HKA_PINUD01], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINUD01') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINUD02",
                      &colnum[HXD_HKA_PINUD02], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINUD02') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINUD03",
                      &colnum[HXD_HKA_PINUD03], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINUD03') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINUD10",
                      &colnum[HXD_HKA_PINUD10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINUD10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINUD11",
                      &colnum[HXD_HKA_PINUD11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINUD11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINUD12",
                      &colnum[HXD_HKA_PINUD12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINUD12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINUD13",
                      &colnum[HXD_HKA_PINUD13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINUD13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINUD20",
                      &colnum[HXD_HKA_PINUD20], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINUD20') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINUD21",
                      &colnum[HXD_HKA_PINUD21], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINUD21') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINUD22",
                      &colnum[HXD_HKA_PINUD22], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINUD22') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINUD23",
                      &colnum[HXD_HKA_PINUD23], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINUD23') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINUD30",
                      &colnum[HXD_HKA_PINUD30], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINUD30') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINUD31",
                      &colnum[HXD_HKA_PINUD31], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINUD31') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINUD32",
                      &colnum[HXD_HKA_PINUD32], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINUD32') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_PINUD33",
                      &colnum[HXD_HKA_PINUD33], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_PINUD33') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_W00_DT",
                      &colnum[HXD_HKA_W00_DT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_W00_DT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_W01_DT",
                      &colnum[HXD_HKA_W01_DT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_W01_DT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_W02_DT",
                      &colnum[HXD_HKA_W02_DT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_W02_DT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_W03_DT",
                      &colnum[HXD_HKA_W03_DT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_W03_DT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_W10_DT",
                      &colnum[HXD_HKA_W10_DT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_W10_DT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_W11_DT",
                      &colnum[HXD_HKA_W11_DT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_W11_DT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_W12_DT",
                      &colnum[HXD_HKA_W12_DT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_W12_DT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_W13_DT",
                      &colnum[HXD_HKA_W13_DT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_W13_DT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_W20_DT",
                      &colnum[HXD_HKA_W20_DT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_W20_DT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_W21_DT",
                      &colnum[HXD_HKA_W21_DT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_W21_DT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_W22_DT",
                      &colnum[HXD_HKA_W22_DT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_W22_DT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_W23_DT",
                      &colnum[HXD_HKA_W23_DT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_W23_DT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_W30_DT",
                      &colnum[HXD_HKA_W30_DT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_W30_DT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_W31_DT",
                      &colnum[HXD_HKA_W31_DT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_W31_DT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_W32_DT",
                      &colnum[HXD_HKA_W32_DT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_W32_DT') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_HKA_W33_DT",
                      &colnum[HXD_HKA_W33_DT], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_HKA_W33_DT') failed (%d)\n",
            pname, istat); return istat;}
  
  return ANL_OK;
}


int
HXDHKFitsReadHK_ana(fitsfile *fp, int irow)
{
  
  int istat = 0;
  
  int anynul;
  int casesen = TRUE;
  int hdutype;

  long firstelem = 1;
  long nelements = 1;

  double time;
  
  fits_movabs_hdu( fp, HK, &hdutype, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu (%d) failed (%d)\n",
	    pname, HK, istat);
    return istat;
  } else {
    double nulval=1.0;
    fits_read_col_dbl(fp, time_colnum, irow, firstelem, nelements,
		      nulval, &time, &anynul, &istat);
    BnkfPutM ("HXD:HK:PACKET_AETIME", sizeof(double), &time);
    BnkfPutM ("HXD:ALL:PACKET_AETIME", sizeof(double), &time);
  }
  
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_de_mode;
    fits_read_col_byt(fp, colnum[HXD_DE_MODE], irow, firstelem,
                      nelements, nulval, &hxd_de_mode, &anynul, &istat);
    data[0] = hxd_de_mode;
    BnkfPutM ("HXD:HKD:DEST", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_nmist;
    fits_read_col_byt(fp, colnum[HXD_NMIST], irow, firstelem,
                      nelements, nulval, &hxd_nmist, &anynul, &istat);
    data[0] = hxd_nmist;
    BnkfPutM ("HXD:HKD:NMIST", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_sysst;
    fits_read_col_usht(fp, colnum[HXD_SYSST], irow, firstelem,
                      nelements, nulval, &hxd_sysst, &anynul, &istat);
    data[0] = hxd_sysst;
    BnkfPutM ("HXD:HKD:SYST", sizeof(int)*1, data);
  }
  {
    unsigned int data[7];
    unsigned char nulval=1;
    unsigned char hxd_cmd_ans_bk0;
    unsigned char hxd_cmd_ans_bk1;
    unsigned char hxd_cmd_ans_bk2;
    unsigned char hxd_cmd_ans_bk3;
    unsigned char hxd_cmd_ans_bk4;
    unsigned char hxd_cmd_ans_bk5;
    unsigned char hxd_cmd_ans_bk6;
    fits_read_col_byt(fp, colnum[HXD_CMD_ANS_BK0], irow, firstelem,
                      nelements, nulval, &hxd_cmd_ans_bk0, &anynul, &istat);
    data[0] = hxd_cmd_ans_bk0;
    fits_read_col_byt(fp, colnum[HXD_CMD_ANS_BK1], irow, firstelem,
                      nelements, nulval, &hxd_cmd_ans_bk1, &anynul, &istat);
    data[1] = hxd_cmd_ans_bk1;
    fits_read_col_byt(fp, colnum[HXD_CMD_ANS_BK2], irow, firstelem,
                      nelements, nulval, &hxd_cmd_ans_bk2, &anynul, &istat);
    data[2] = hxd_cmd_ans_bk2;
    fits_read_col_byt(fp, colnum[HXD_CMD_ANS_BK3], irow, firstelem,
                      nelements, nulval, &hxd_cmd_ans_bk3, &anynul, &istat);
    data[3] = hxd_cmd_ans_bk3;
    fits_read_col_byt(fp, colnum[HXD_CMD_ANS_BK4], irow, firstelem,
                      nelements, nulval, &hxd_cmd_ans_bk4, &anynul, &istat);
    data[4] = hxd_cmd_ans_bk4;
    fits_read_col_byt(fp, colnum[HXD_CMD_ANS_BK5], irow, firstelem,
                      nelements, nulval, &hxd_cmd_ans_bk5, &anynul, &istat);
    data[5] = hxd_cmd_ans_bk5;
    fits_read_col_byt(fp, colnum[HXD_CMD_ANS_BK6], irow, firstelem,
                      nelements, nulval, &hxd_cmd_ans_bk6, &anynul, &istat);
    data[6] = hxd_cmd_ans_bk6;
    BnkfPutM ("HXD:HKD:CABK", sizeof(int)*7, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_rsv_cmd_leng;
    fits_read_col_byt(fp, colnum[HXD_RSV_CMD_LENG], irow, firstelem,
                      nelements, nulval, &hxd_rsv_cmd_leng, &anynul, &istat);
    data[0] = hxd_rsv_cmd_leng;
    BnkfPutM ("HXD:HKD:RCLENG", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_rsv_cmd_cnt;
    fits_read_col_byt(fp, colnum[HXD_RSV_CMD_CNT], irow, firstelem,
                      nelements, nulval, &hxd_rsv_cmd_cnt, &anynul, &istat);
    data[0] = hxd_rsv_cmd_cnt;
    BnkfPutM ("HXD:HKD:RCCNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_cmd_rej_cod;
    fits_read_col_byt(fp, colnum[HXD_CMD_REJ_COD], irow, firstelem,
                      nelements, nulval, &hxd_cmd_rej_cod, &anynul, &istat);
    data[0] = hxd_cmd_rej_cod;
    BnkfPutM ("HXD:HKD:CREJCOD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_cmd_rej_cnt;
    fits_read_col_byt(fp, colnum[HXD_CMD_REJ_CNT], irow, firstelem,
                      nelements, nulval, &hxd_cmd_rej_cnt, &anynul, &istat);
    data[0] = hxd_cmd_rej_cnt;
    BnkfPutM ("HXD:HKD:CRECNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_hxd_dis;
    fits_read_col_usht(fp, colnum[HXD_HXD_DIS], irow, firstelem,
                      nelements, nulval, &hxd_hxd_dis, &anynul, &istat);
    data[0] = hxd_hxd_dis;
    BnkfPutM ("HXD:HKD:HXDDIS", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_dtrate;
    fits_read_col_byt(fp, colnum[HXD_DTRATE], irow, firstelem,
                      nelements, nulval, &hxd_dtrate, &anynul, &istat);
    data[0] = hxd_dtrate;
    BnkfPutM ("HXD:HKD:DTRATE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_sw_version;
    fits_read_col_usht(fp, colnum[HXD_SW_VERSION], irow, firstelem,
                      nelements, nulval, &hxd_sw_version, &anynul, &istat);
    data[0] = hxd_sw_version;
    BnkfPutM ("HXD:HKD:SW_VER", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_dp_cntl_err;
    fits_read_col_byt(fp, colnum[HXD_DP_CNTL_ERR], irow, firstelem,
                      nelements, nulval, &hxd_dp_cntl_err, &anynul, &istat);
    data[0] = hxd_dp_cntl_err;
    BnkfPutM ("HXD:HKD:CNTL_DMAERR", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_tlm_err;
    fits_read_col_byt(fp, colnum[HXD_TLM_ERR], irow, firstelem,
                      nelements, nulval, &hxd_tlm_err, &anynul, &istat);
    data[0] = hxd_tlm_err;
    BnkfPutM ("HXD:HKD:TLM_DMAERR", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_cmd_err;
    fits_read_col_byt(fp, colnum[HXD_CMD_ERR], irow, firstelem,
                      nelements, nulval, &hxd_cmd_err, &anynul, &istat);
    data[0] = hxd_cmd_err;
    BnkfPutM ("HXD:HKD:CMD_DMAERR", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_lm_1b_ec;
    fits_read_col_usht(fp, colnum[HXD_LM_1B_EC], irow, firstelem,
                      nelements, nulval, &hxd_lm_1b_ec, &anynul, &istat);
    data[0] = hxd_lm_1b_ec;
    BnkfPutM ("HXD:HKD:LM1BEC", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_lm_2b_ec;
    fits_read_col_byt(fp, colnum[HXD_LM_2B_EC], irow, firstelem,
                      nelements, nulval, &hxd_lm_2b_ec, &anynul, &istat);
    data[0] = hxd_lm_2b_ec;
    BnkfPutM ("HXD:HKD:LM2BEC", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_db_1b_ec;
    fits_read_col_usht(fp, colnum[HXD_DB_1B_EC], irow, firstelem,
                      nelements, nulval, &hxd_db_1b_ec, &anynul, &istat);
    data[0] = hxd_db_1b_ec;
    BnkfPutM ("HXD:HKD:DB1BEC", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_db_2b_ec;
    fits_read_col_byt(fp, colnum[HXD_DB_2B_EC], irow, firstelem,
                      nelements, nulval, &hxd_db_2b_ec, &anynul, &istat);
    data[0] = hxd_db_2b_ec;
    BnkfPutM ("HXD:HKD:DB2BEC", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_err_cod;
    fits_read_col_byt(fp, colnum[HXD_ERR_COD], irow, firstelem,
                      nelements, nulval, &hxd_err_cod, &anynul, &istat);
    data[0] = hxd_err_cod;
    BnkfPutM ("HXD:HKD:ERRCOD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_err_log_cnt;
    fits_read_col_usht(fp, colnum[HXD_ERR_LOG_CNT], irow, firstelem,
                      nelements, nulval, &hxd_err_log_cnt, &anynul, &istat);
    data[0] = hxd_err_log_cnt;
    BnkfPutM ("HXD:HKD:ERRLOGC", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_err_tsk_inf;
    fits_read_col_byt(fp, colnum[HXD_ERR_TSK_INF], irow, firstelem,
                      nelements, nulval, &hxd_err_tsk_inf, &anynul, &istat);
    data[0] = hxd_err_tsk_inf;
    BnkfPutM ("HXD:HKD:ERRTINF", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_ill_int_cnt;
    fits_read_col_byt(fp, colnum[HXD_ILL_INT_CNT], irow, firstelem,
                      nelements, nulval, &hxd_ill_int_cnt, &anynul, &istat);
    data[0] = hxd_ill_int_cnt;
    BnkfPutM ("HXD:HKD:ILINTC", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_ae_hk_fail;
    fits_read_col_byt(fp, colnum[HXD_AE_HK_FAIL], irow, firstelem,
                      nelements, nulval, &hxd_ae_hk_fail, &anynul, &istat);
    data[0] = hxd_ae_hk_fail;
    BnkfPutM ("HXD:HKD:AE_HK_FAIL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_acu_sw_err;
    fits_read_col_byt(fp, colnum[HXD_ACU_SW_ERR], irow, firstelem,
                      nelements, nulval, &hxd_acu_sw_err, &anynul, &istat);
    data[0] = hxd_acu_sw_err;
    BnkfPutM ("HXD:HKD:ACU_SW_ERR", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_tpu_sw_err;
    fits_read_col_byt(fp, colnum[HXD_TPU_SW_ERR], irow, firstelem,
                      nelements, nulval, &hxd_tpu_sw_err, &anynul, &istat);
    data[0] = hxd_tpu_sw_err;
    BnkfPutM ("HXD:HKD:TPU_SW_ERR", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_wpu_sw_err;
    fits_read_col_byt(fp, colnum[HXD_WPU_SW_ERR], irow, firstelem,
                      nelements, nulval, &hxd_wpu_sw_err, &anynul, &istat);
    data[0] = hxd_wpu_sw_err;
    BnkfPutM ("HXD:HKD:WPU_SW_ERR", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_data_cpy_err;
    fits_read_col_byt(fp, colnum[HXD_DATA_CPY_ERR], irow, firstelem,
                      nelements, nulval, &hxd_data_cpy_err, &anynul, &istat);
    data[0] = hxd_data_cpy_err;
    BnkfPutM ("HXD:HKD:DATA_CPY_ERR", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_acu_hw_err;
    fits_read_col_byt(fp, colnum[HXD_ACU_HW_ERR], irow, firstelem,
                      nelements, nulval, &hxd_acu_hw_err, &anynul, &istat);
    data[0] = hxd_acu_hw_err;
    BnkfPutM ("HXD:HKD:ACU_HW_ERR", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_tpu_hw_err;
    fits_read_col_byt(fp, colnum[HXD_TPU_HW_ERR], irow, firstelem,
                      nelements, nulval, &hxd_tpu_hw_err, &anynul, &istat);
    data[0] = hxd_tpu_hw_err;
    BnkfPutM ("HXD:HKD:TPU_HW_ERR", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_wpu_hw_err;
    fits_read_col_byt(fp, colnum[HXD_WPU_HW_ERR], irow, firstelem,
                      nelements, nulval, &hxd_wpu_hw_err, &anynul, &istat);
    data[0] = hxd_wpu_hw_err;
    BnkfPutM ("HXD:HKD:WPU_HW_ERR", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    int nulval=1;
    int hxd_mp_adrs;
    fits_read_col_int(fp, colnum[HXD_MP_ADRS], irow, firstelem,
                      nelements, nulval, &hxd_mp_adrs, &anynul, &istat);
    data[0] = hxd_mp_adrs;
    BnkfPutM ("HXD:HKD:MPADR", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    int nulval=1;
    int hxd_mp_err_adrs;
    fits_read_col_int(fp, colnum[HXD_MP_ERR_ADRS], irow, firstelem,
                      nelements, nulval, &hxd_mp_err_adrs, &anynul, &istat);
    data[0] = hxd_mp_err_adrs;
    BnkfPutM ("HXD:HKD:MPEA", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    int nulval=1;
    int hxd_lm_2b_e_adrs;
    fits_read_col_int(fp, colnum[HXD_LM_2B_E_ADRS], irow, firstelem,
                      nelements, nulval, &hxd_lm_2b_e_adrs, &anynul, &istat);
    data[0] = hxd_lm_2b_e_adrs;
    BnkfPutM ("HXD:HKD:LM2BA", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    int nulval=1;
    int hxd_dma_2b_e_adrs;
    fits_read_col_int(fp, colnum[HXD_DMA_2B_E_ADRS], irow, firstelem,
                      nelements, nulval, &hxd_dma_2b_e_adrs, &anynul, &istat);
    data[0] = hxd_dma_2b_e_adrs;
    BnkfPutM ("HXD:HKD:DM2BA", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_ram_dump;
    fits_read_col_byt(fp, colnum[HXD_RAM_DUMP], irow, firstelem,
                      nelements, nulval, &hxd_ram_dump, &anynul, &istat);
    data[0] = hxd_ram_dump;
    BnkfPutM ("HXD:HKD:RAM_DUMP", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_promwt;
    fits_read_col_byt(fp, colnum[HXD_PROMWT], irow, firstelem,
                      nelements, nulval, &hxd_promwt, &anynul, &istat);
    data[0] = hxd_promwt;
    BnkfPutM ("HXD:HKD:PROMWT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_prom_ptl;
    fits_read_col_byt(fp, colnum[HXD_PROM_PTL], irow, firstelem,
                      nelements, nulval, &hxd_prom_ptl, &anynul, &istat);
    data[0] = hxd_prom_ptl;
    BnkfPutM ("HXD:HKD:PROM_PTL", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    int nulval=1;
    int hxd_mchk_adrs;
    fits_read_col_int(fp, colnum[HXD_MCHK_ADRS], irow, firstelem,
                      nelements, nulval, &hxd_mchk_adrs, &anynul, &istat);
    data[0] = hxd_mchk_adrs;
    BnkfPutM ("HXD:HKD:MCHAD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_rom_1b_ec;
    fits_read_col_usht(fp, colnum[HXD_ROM_1B_EC], irow, firstelem,
                      nelements, nulval, &hxd_rom_1b_ec, &anynul, &istat);
    data[0] = hxd_rom_1b_ec;
    BnkfPutM ("HXD:HKD:ROM1BEC", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_rom_2b_ec;
    fits_read_col_byt(fp, colnum[HXD_ROM_2B_EC], irow, firstelem,
                      nelements, nulval, &hxd_rom_2b_ec, &anynul, &istat);
    data[0] = hxd_rom_2b_ec;
    BnkfPutM ("HXD:HKD:ROM2BEC", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    int nulval=1;
    int hxd_rompa_adrs;
    fits_read_col_int(fp, colnum[HXD_ROMPA_ADRS], irow, firstelem,
                      nelements, nulval, &hxd_rompa_adrs, &anynul, &istat);
    data[0] = hxd_rompa_adrs;
    BnkfPutM ("HXD:HKD:ROMPA", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    int nulval=1;
    int hxd_rom_1b_e_adrs;
    fits_read_col_int(fp, colnum[HXD_ROM_1B_E_ADRS], irow, firstelem,
                      nelements, nulval, &hxd_rom_1b_e_adrs, &anynul, &istat);
    data[0] = hxd_rom_1b_e_adrs;
    BnkfPutM ("HXD:HKD:ROM1BEA", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    int nulval=1;
    int hxd_rom_2b_e_adrs;
    fits_read_col_int(fp, colnum[HXD_ROM_2B_E_ADRS], irow, firstelem,
                      nelements, nulval, &hxd_rom_2b_e_adrs, &anynul, &istat);
    data[0] = hxd_rom_2b_e_adrs;
    BnkfPutM ("HXD:HKD:ROM2BEA", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_gtst;
    fits_read_col_byt(fp, colnum[HXD_GTST], irow, firstelem,
                      nelements, nulval, &hxd_gtst, &anynul, &istat);
    data[0] = hxd_gtst;
    BnkfPutM ("HXD:HKD:WEVTGT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    int nulval=1;
    int hxd_aecmd_code;
    fits_read_col_int(fp, colnum[HXD_AECMD_CODE], irow, firstelem,
                      nelements, nulval, &hxd_aecmd_code, &anynul, &istat);
    data[0] = hxd_aecmd_code;
    BnkfPutM ("HXD:HKD:AECCDE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_aecmd_cnt;
    fits_read_col_byt(fp, colnum[HXD_AECMD_CNT], irow, firstelem,
                      nelements, nulval, &hxd_aecmd_cnt, &anynul, &istat);
    data[0] = hxd_aecmd_cnt;
    BnkfPutM ("HXD:HKD:AECCNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_aecmd_rej_code;
    fits_read_col_byt(fp, colnum[HXD_AECMD_REJ_CODE], irow, firstelem,
                      nelements, nulval, &hxd_aecmd_rej_code, &anynul, &istat);
    data[0] = hxd_aecmd_rej_code;
    BnkfPutM ("HXD:HKD:AECRCDE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_aecmd_rej_cnt;
    fits_read_col_byt(fp, colnum[HXD_AECMD_REJ_CNT], irow, firstelem,
                      nelements, nulval, &hxd_aecmd_rej_cnt, &anynul, &istat);
    data[0] = hxd_aecmd_rej_cnt;
    BnkfPutM ("HXD:HKD:AECRCNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pi_err_code;
    fits_read_col_usht(fp, colnum[HXD_PI_ERR_CODE], irow, firstelem,
                      nelements, nulval, &hxd_pi_err_code, &anynul, &istat);
    data[0] = hxd_pi_err_code;
    BnkfPutM ("HXD:HKD:PIERRCODE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_pi_err_cnt;
    fits_read_col_usht(fp, colnum[HXD_PI_ERR_CNT], irow, firstelem,
                      nelements, nulval, &hxd_pi_err_cnt, &anynul, &istat);
    data[0] = hxd_pi_err_cnt;
    BnkfPutM ("HXD:HKD:PIERRCNT", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_pi_acucmd_ena;
    fits_read_col_byt(fp, colnum[HXD_PI_ACUCMD_ENA], irow, firstelem,
                      nelements, nulval, &hxd_pi_acucmd_ena, &anynul, &istat);
    data[0] = hxd_pi_acucmd_ena;
    BnkfPutM ("HXD:HKD:PIACUCMD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_pi_status;
    fits_read_col_byt(fp, colnum[HXD_PI_STATUS], irow, firstelem,
                      nelements, nulval, &hxd_pi_status, &anynul, &istat);
    data[0] = hxd_pi_status;
    BnkfPutM ("HXD:HKD:PISTATUS", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_wpu1_st;
    unsigned char hxd_wpu0_st;
    unsigned char hxd_wpu3_st;
    unsigned char hxd_wpu2_st;
    fits_read_col_byt(fp, colnum[HXD_WPU1_ST], irow, firstelem,
                      nelements, nulval, &hxd_wpu1_st, &anynul, &istat);
    data[0] = hxd_wpu1_st;
    fits_read_col_byt(fp, colnum[HXD_WPU0_ST], irow, firstelem,
                      nelements, nulval, &hxd_wpu0_st, &anynul, &istat);
    data[1] = hxd_wpu0_st;
    fits_read_col_byt(fp, colnum[HXD_WPU3_ST], irow, firstelem,
                      nelements, nulval, &hxd_wpu3_st, &anynul, &istat);
    data[2] = hxd_wpu3_st;
    fits_read_col_byt(fp, colnum[HXD_WPU2_ST], irow, firstelem,
                      nelements, nulval, &hxd_wpu2_st, &anynul, &istat);
    data[3] = hxd_wpu2_st;
    BnkfPutM ("HXD:HKA:WPU_ON/OF", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_tpu1_st;
    unsigned char hxd_tpu0_st;
    unsigned char hxd_tpu3_st;
    unsigned char hxd_tpu2_st;
    fits_read_col_byt(fp, colnum[HXD_TPU1_ST], irow, firstelem,
                      nelements, nulval, &hxd_tpu1_st, &anynul, &istat);
    data[0] = hxd_tpu1_st;
    fits_read_col_byt(fp, colnum[HXD_TPU0_ST], irow, firstelem,
                      nelements, nulval, &hxd_tpu0_st, &anynul, &istat);
    data[1] = hxd_tpu0_st;
    fits_read_col_byt(fp, colnum[HXD_TPU3_ST], irow, firstelem,
                      nelements, nulval, &hxd_tpu3_st, &anynul, &istat);
    data[2] = hxd_tpu3_st;
    fits_read_col_byt(fp, colnum[HXD_TPU2_ST], irow, firstelem,
                      nelements, nulval, &hxd_tpu2_st, &anynul, &istat);
    data[3] = hxd_tpu2_st;
    BnkfPutM ("HXD:HKA:TPU_ON/OF", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_hka_hv_ena_dis;
    fits_read_col_byt(fp, colnum[HXD_HKA_HV_ENA_DIS], irow, firstelem,
                      nelements, nulval, &hxd_hka_hv_ena_dis, &anynul, &istat);
    data[0] = hxd_hka_hv_ena_dis;
    BnkfPutM ("HXD:HKA:HV_ENA/DIS", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_hv_w1_st;
    unsigned char hxd_hv_w0_st;
    unsigned char hxd_hv_w3_st;
    unsigned char hxd_hv_w2_st;
    fits_read_col_byt(fp, colnum[HXD_HV_W1_ST], irow, firstelem,
                      nelements, nulval, &hxd_hv_w1_st, &anynul, &istat);
    data[0] = hxd_hv_w1_st;
    fits_read_col_byt(fp, colnum[HXD_HV_W0_ST], irow, firstelem,
                      nelements, nulval, &hxd_hv_w0_st, &anynul, &istat);
    data[1] = hxd_hv_w0_st;
    fits_read_col_byt(fp, colnum[HXD_HV_W3_ST], irow, firstelem,
                      nelements, nulval, &hxd_hv_w3_st, &anynul, &istat);
    data[2] = hxd_hv_w3_st;
    fits_read_col_byt(fp, colnum[HXD_HV_W2_ST], irow, firstelem,
                      nelements, nulval, &hxd_hv_w2_st, &anynul, &istat);
    data[3] = hxd_hv_w2_st;
    BnkfPutM ("HXD:HKA:HVW_ON/OF", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_hv_t1_st;
    unsigned char hxd_hv_t0_st;
    unsigned char hxd_hv_t3_st;
    unsigned char hxd_hv_t2_st;
    fits_read_col_byt(fp, colnum[HXD_HV_T1_ST], irow, firstelem,
                      nelements, nulval, &hxd_hv_t1_st, &anynul, &istat);
    data[0] = hxd_hv_t1_st;
    fits_read_col_byt(fp, colnum[HXD_HV_T0_ST], irow, firstelem,
                      nelements, nulval, &hxd_hv_t0_st, &anynul, &istat);
    data[1] = hxd_hv_t0_st;
    fits_read_col_byt(fp, colnum[HXD_HV_T3_ST], irow, firstelem,
                      nelements, nulval, &hxd_hv_t3_st, &anynul, &istat);
    data[2] = hxd_hv_t3_st;
    fits_read_col_byt(fp, colnum[HXD_HV_T2_ST], irow, firstelem,
                      nelements, nulval, &hxd_hv_t2_st, &anynul, &istat);
    data[3] = hxd_hv_t2_st;
    BnkfPutM ("HXD:HKA:HVT_ON/OF", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_hv_p1_st;
    unsigned char hxd_hv_p0_st;
    unsigned char hxd_hv_p3_st;
    unsigned char hxd_hv_p2_st;
    fits_read_col_byt(fp, colnum[HXD_HV_P1_ST], irow, firstelem,
                      nelements, nulval, &hxd_hv_p1_st, &anynul, &istat);
    data[0] = hxd_hv_p1_st;
    fits_read_col_byt(fp, colnum[HXD_HV_P0_ST], irow, firstelem,
                      nelements, nulval, &hxd_hv_p0_st, &anynul, &istat);
    data[1] = hxd_hv_p0_st;
    fits_read_col_byt(fp, colnum[HXD_HV_P3_ST], irow, firstelem,
                      nelements, nulval, &hxd_hv_p3_st, &anynul, &istat);
    data[2] = hxd_hv_p3_st;
    fits_read_col_byt(fp, colnum[HXD_HV_P2_ST], irow, firstelem,
                      nelements, nulval, &hxd_hv_p2_st, &anynul, &istat);
    data[3] = hxd_hv_p2_st;
    BnkfPutM ("HXD:HKA:HVP_ON/OF", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_acu_hk_dataid;
    fits_read_col_byt(fp, colnum[HXD_ACU_HK_DATAID], irow, firstelem,
                      nelements, nulval, &hxd_acu_hk_dataid, &anynul, &istat);
    data[0] = hxd_acu_hk_dataid;
    BnkfPutM ("HXD:HKA:DATA_ID", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_psu_volt;
    fits_read_col_byt(fp, colnum[HXD_PSU_VOLT], irow, firstelem,
                      nelements, nulval, &hxd_psu_volt, &anynul, &istat);
    data[0] = hxd_psu_volt;
    BnkfPutM ("HXD:HKA:PSU_VOL", sizeof(int)*1, data);
  }
  {
    double data[1];
    double nulval=1.0;
    double hxd_psu_p5vd_cal;
    fits_read_col_dbl(fp, colnum[HXD_PSU_P5VD_CAL], irow, firstelem,
                      nelements, nulval, &hxd_psu_p5vd_cal, &anynul, &istat);
    data[0] = hxd_psu_p5vd_cal;
    BnkfPutM ("HXD:HKA:0:PSU_VOLT", sizeof(double)*1, data);
  }
  {
    double data[1];
    double nulval=1.0;
    double hxd_psu_p12v_cal;
    fits_read_col_dbl(fp, colnum[HXD_PSU_P12V_CAL], irow, firstelem,
                      nelements, nulval, &hxd_psu_p12v_cal, &anynul, &istat);
    data[0] = hxd_psu_p12v_cal;
    BnkfPutM ("HXD:HKA:1:PSU_VOLT", sizeof(double)*1, data);
  }
  {
    double data[1];
    double nulval=1.0;
    double hxd_psu_p5va_cal;
    fits_read_col_dbl(fp, colnum[HXD_PSU_P5VA_CAL], irow, firstelem,
                      nelements, nulval, &hxd_psu_p5va_cal, &anynul, &istat);
    data[0] = hxd_psu_p5va_cal;
    BnkfPutM ("HXD:HKA:2:PSU_VOLT", sizeof(double)*1, data);
  }
  {
    double data[1];
    double nulval=1.0;
    double hxd_psu_m12v_cal;
    fits_read_col_dbl(fp, colnum[HXD_PSU_M12V_CAL], irow, firstelem,
                      nelements, nulval, &hxd_psu_m12v_cal, &anynul, &istat);
    data[0] = hxd_psu_m12v_cal;
    BnkfPutM ("HXD:HKA:3:PSU_VOLT", sizeof(double)*1, data);
  }
  {
    double data[1];
    double nulval=1.0;
    double hxd_psu_m5va_cal;
    fits_read_col_dbl(fp, colnum[HXD_PSU_M5VA_CAL], irow, firstelem,
                      nelements, nulval, &hxd_psu_m5va_cal, &anynul, &istat);
    data[0] = hxd_psu_m5va_cal;
    BnkfPutM ("HXD:HKA:4:PSU_VOLT", sizeof(double)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_acu_hk_time;
    fits_read_col_byt(fp, colnum[HXD_ACU_HK_TIME], irow, firstelem,
                      nelements, nulval, &hxd_acu_hk_time, &anynul, &istat);
    data[0] = hxd_acu_hk_time;
    BnkfPutM ("HXD:HKA:HKTIME", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    int nulval=1;
    int hxd_ae_tm_latch_tm;
    fits_read_col_int(fp, colnum[HXD_AE_TM_LATCH_TM], irow, firstelem,
                      nelements, nulval, &hxd_ae_tm_latch_tm, &anynul, &istat);
    data[0] = hxd_ae_tm_latch_tm;
    BnkfPutM ("HXD:HKA:LATCH_TIM", sizeof(int)*1, data);
  }
  {
    unsigned int data[16];
    unsigned char nulval=1;
    unsigned char hxd_sensor_temp0;
    unsigned char hxd_sensor_temp1;
    unsigned char hxd_sensor_temp2;
    unsigned char hxd_sensor_temp3;
    unsigned char hxd_sensor_temp4;
    unsigned char hxd_sensor_temp5;
    unsigned char hxd_sensor_temp6;
    unsigned char hxd_sensor_temp7;
    unsigned char hxd_sensor_temp8;
    unsigned char hxd_sensor_temp9;
    unsigned char hxd_sensor_tempa;
    unsigned char hxd_sensor_tempb;
    unsigned char hxd_sensor_tempc;
    unsigned char hxd_sensor_tempd;
    unsigned char hxd_sensor_tempe;
    unsigned char hxd_sensor_tempf;
    fits_read_col_byt(fp, colnum[HXD_SENSOR_TEMP0], irow, firstelem,
                      nelements, nulval, &hxd_sensor_temp0, &anynul, &istat);
    data[0] = hxd_sensor_temp0;
    fits_read_col_byt(fp, colnum[HXD_SENSOR_TEMP1], irow, firstelem,
                      nelements, nulval, &hxd_sensor_temp1, &anynul, &istat);
    data[1] = hxd_sensor_temp1;
    fits_read_col_byt(fp, colnum[HXD_SENSOR_TEMP2], irow, firstelem,
                      nelements, nulval, &hxd_sensor_temp2, &anynul, &istat);
    data[2] = hxd_sensor_temp2;
    fits_read_col_byt(fp, colnum[HXD_SENSOR_TEMP3], irow, firstelem,
                      nelements, nulval, &hxd_sensor_temp3, &anynul, &istat);
    data[3] = hxd_sensor_temp3;
    fits_read_col_byt(fp, colnum[HXD_SENSOR_TEMP4], irow, firstelem,
                      nelements, nulval, &hxd_sensor_temp4, &anynul, &istat);
    data[4] = hxd_sensor_temp4;
    fits_read_col_byt(fp, colnum[HXD_SENSOR_TEMP5], irow, firstelem,
                      nelements, nulval, &hxd_sensor_temp5, &anynul, &istat);
    data[5] = hxd_sensor_temp5;
    fits_read_col_byt(fp, colnum[HXD_SENSOR_TEMP6], irow, firstelem,
                      nelements, nulval, &hxd_sensor_temp6, &anynul, &istat);
    data[6] = hxd_sensor_temp6;
    fits_read_col_byt(fp, colnum[HXD_SENSOR_TEMP7], irow, firstelem,
                      nelements, nulval, &hxd_sensor_temp7, &anynul, &istat);
    data[7] = hxd_sensor_temp7;
    fits_read_col_byt(fp, colnum[HXD_SENSOR_TEMP8], irow, firstelem,
                      nelements, nulval, &hxd_sensor_temp8, &anynul, &istat);
    data[8] = hxd_sensor_temp8;
    fits_read_col_byt(fp, colnum[HXD_SENSOR_TEMP9], irow, firstelem,
                      nelements, nulval, &hxd_sensor_temp9, &anynul, &istat);
    data[9] = hxd_sensor_temp9;
    fits_read_col_byt(fp, colnum[HXD_SENSOR_TEMPA], irow, firstelem,
                      nelements, nulval, &hxd_sensor_tempa, &anynul, &istat);
    data[10] = hxd_sensor_tempa;
    fits_read_col_byt(fp, colnum[HXD_SENSOR_TEMPB], irow, firstelem,
                      nelements, nulval, &hxd_sensor_tempb, &anynul, &istat);
    data[11] = hxd_sensor_tempb;
    fits_read_col_byt(fp, colnum[HXD_SENSOR_TEMPC], irow, firstelem,
                      nelements, nulval, &hxd_sensor_tempc, &anynul, &istat);
    data[12] = hxd_sensor_tempc;
    fits_read_col_byt(fp, colnum[HXD_SENSOR_TEMPD], irow, firstelem,
                      nelements, nulval, &hxd_sensor_tempd, &anynul, &istat);
    data[13] = hxd_sensor_tempd;
    fits_read_col_byt(fp, colnum[HXD_SENSOR_TEMPE], irow, firstelem,
                      nelements, nulval, &hxd_sensor_tempe, &anynul, &istat);
    data[14] = hxd_sensor_tempe;
    fits_read_col_byt(fp, colnum[HXD_SENSOR_TEMPF], irow, firstelem,
                      nelements, nulval, &hxd_sensor_tempf, &anynul, &istat);
    data[15] = hxd_sensor_tempf;
    BnkfPutM ("HXD:HKA:TEMP", sizeof(int)*16, data);
  }
  {
    double data[16];
    double nulval=1.0;
    double hxd_temp_w10_cal;
    double hxd_temp_w11_cal;
    double hxd_temp_w12_cal;
    double hxd_temp_w13_cal;
    double hxd_temp_w20_cal;
    double hxd_temp_w21_cal;
    double hxd_temp_w22_cal;
    double hxd_temp_w23_cal;
    double hxd_temp_w00_cal;
    double hxd_temp_w01_cal;
    double hxd_temp_w02_cal;
    double hxd_temp_w03_cal;
    double hxd_temp_w30_cal;
    double hxd_temp_w31_cal;
    double hxd_temp_w32_cal;
    double hxd_temp_w33_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_W10_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_w10_cal, &anynul, &istat);
    data[0] = hxd_temp_w10_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_W11_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_w11_cal, &anynul, &istat);
    data[1] = hxd_temp_w11_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_W12_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_w12_cal, &anynul, &istat);
    data[2] = hxd_temp_w12_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_W13_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_w13_cal, &anynul, &istat);
    data[3] = hxd_temp_w13_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_W20_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_w20_cal, &anynul, &istat);
    data[4] = hxd_temp_w20_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_W21_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_w21_cal, &anynul, &istat);
    data[5] = hxd_temp_w21_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_W22_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_w22_cal, &anynul, &istat);
    data[6] = hxd_temp_w22_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_W23_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_w23_cal, &anynul, &istat);
    data[7] = hxd_temp_w23_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_W00_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_w00_cal, &anynul, &istat);
    data[8] = hxd_temp_w00_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_W01_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_w01_cal, &anynul, &istat);
    data[9] = hxd_temp_w01_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_W02_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_w02_cal, &anynul, &istat);
    data[10] = hxd_temp_w02_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_W03_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_w03_cal, &anynul, &istat);
    data[11] = hxd_temp_w03_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_W30_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_w30_cal, &anynul, &istat);
    data[12] = hxd_temp_w30_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_W31_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_w31_cal, &anynul, &istat);
    data[13] = hxd_temp_w31_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_W32_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_w32_cal, &anynul, &istat);
    data[14] = hxd_temp_w32_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_W33_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_w33_cal, &anynul, &istat);
    data[15] = hxd_temp_w33_cal;
    BnkfPutM ("HXD:HKA:0:4:TEMP", sizeof(double)*16, data);
  }
  {
    double data[16];
    double nulval=1.0;
    double hxd_temp_t10_cal;
    double hxd_temp_t12_cal;
    double hxd_temp_t14_cal;
    double hxd_temp_t21_cal;
    double hxd_temp_t23_cal;
    double hxd_temp_hv_w2_cal;
    double hxd_temp_hv_p1_cal;
    double hxd_temp_hv_t1_cal;
    double hxd_temp_t00_cal;
    double hxd_temp_t02_cal;
    double hxd_temp_t04_cal;
    double hxd_temp_t31_cal;
    double hxd_temp_t33_cal;
    double hxd_temp_hv_w0_cal;
    double hxd_temp_hv_p0_cal;
    double hxd_temp_hv_t3_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_T10_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_t10_cal, &anynul, &istat);
    data[0] = hxd_temp_t10_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_T12_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_t12_cal, &anynul, &istat);
    data[1] = hxd_temp_t12_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_T14_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_t14_cal, &anynul, &istat);
    data[2] = hxd_temp_t14_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_T21_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_t21_cal, &anynul, &istat);
    data[3] = hxd_temp_t21_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_T23_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_t23_cal, &anynul, &istat);
    data[4] = hxd_temp_t23_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_HV_W2_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_hv_w2_cal, &anynul, &istat);
    data[5] = hxd_temp_hv_w2_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_HV_P1_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_hv_p1_cal, &anynul, &istat);
    data[6] = hxd_temp_hv_p1_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_HV_T1_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_hv_t1_cal, &anynul, &istat);
    data[7] = hxd_temp_hv_t1_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_T00_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_t00_cal, &anynul, &istat);
    data[8] = hxd_temp_t00_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_T02_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_t02_cal, &anynul, &istat);
    data[9] = hxd_temp_t02_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_T04_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_t04_cal, &anynul, &istat);
    data[10] = hxd_temp_t04_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_T31_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_t31_cal, &anynul, &istat);
    data[11] = hxd_temp_t31_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_T33_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_t33_cal, &anynul, &istat);
    data[12] = hxd_temp_t33_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_HV_W0_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_hv_w0_cal, &anynul, &istat);
    data[13] = hxd_temp_hv_w0_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_HV_P0_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_hv_p0_cal, &anynul, &istat);
    data[14] = hxd_temp_hv_p0_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_HV_T3_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_hv_t3_cal, &anynul, &istat);
    data[15] = hxd_temp_hv_t3_cal;
    BnkfPutM ("HXD:HKA:1:5:TEMP", sizeof(double)*16, data);
  }
  {
    double data[16];
    double nulval=1.0;
    double hxd_temp_cap4_cal;
    double hxd_temp_cap3_cal;
    double hxd_temp_body4_cal;
    double hxd_temp_body3_cal;
    double hxd_temp_btm3_cal;
    double hxd_temp_btm4_cal;
    double hxd_temp_bar3_cal;
    double hxd_temp_center_cal;
    double hxd_temp_cap2_cal;
    double hxd_temp_cap1_cal;
    double hxd_temp_body2_cal;
    double hxd_temp_body1_cal;
    double hxd_temp_btm1_cal;
    double hxd_temp_btm2_cal;
    double hxd_temp_bar1_cal;
    double hxd_temp_bar2_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_CAP4_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_cap4_cal, &anynul, &istat);
    data[0] = hxd_temp_cap4_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_CAP3_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_cap3_cal, &anynul, &istat);
    data[1] = hxd_temp_cap3_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_BODY4_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_body4_cal, &anynul, &istat);
    data[2] = hxd_temp_body4_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_BODY3_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_body3_cal, &anynul, &istat);
    data[3] = hxd_temp_body3_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_BTM3_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_btm3_cal, &anynul, &istat);
    data[4] = hxd_temp_btm3_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_BTM4_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_btm4_cal, &anynul, &istat);
    data[5] = hxd_temp_btm4_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_BAR3_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_bar3_cal, &anynul, &istat);
    data[6] = hxd_temp_bar3_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_CENTER_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_center_cal, &anynul, &istat);
    data[7] = hxd_temp_center_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_CAP2_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_cap2_cal, &anynul, &istat);
    data[8] = hxd_temp_cap2_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_CAP1_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_cap1_cal, &anynul, &istat);
    data[9] = hxd_temp_cap1_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_BODY2_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_body2_cal, &anynul, &istat);
    data[10] = hxd_temp_body2_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_BODY1_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_body1_cal, &anynul, &istat);
    data[11] = hxd_temp_body1_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_BTM1_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_btm1_cal, &anynul, &istat);
    data[12] = hxd_temp_btm1_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_BTM2_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_btm2_cal, &anynul, &istat);
    data[13] = hxd_temp_btm2_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_BAR1_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_bar1_cal, &anynul, &istat);
    data[14] = hxd_temp_bar1_cal;
    fits_read_col_dbl(fp, colnum[HXD_TEMP_BAR2_CAL], irow, firstelem,
                      nelements, nulval, &hxd_temp_bar2_cal, &anynul, &istat);
    data[15] = hxd_temp_bar2_cal;
    BnkfPutM ("HXD:HKA:2:6:TEMP", sizeof(double)*16, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_ae_status;
    fits_read_col_byt(fp, colnum[HXD_AE_STATUS], irow, firstelem,
                      nelements, nulval, &hxd_ae_status, &anynul, &istat);
    data[0] = hxd_ae_status;
    BnkfPutM ("HXD:HKA:AE_STATUS", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_wpu_clk_rate;
    fits_read_col_byt(fp, colnum[HXD_WPU_CLK_RATE], irow, firstelem,
                      nelements, nulval, &hxd_wpu_clk_rate, &anynul, &istat);
    data[0] = hxd_wpu_clk_rate;
    BnkfPutM ("HXD:HKA:WPU_CLK_RATE", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_hv_ref_w0;
    unsigned char hxd_hv_ref_w1;
    unsigned char hxd_hv_ref_w2;
    unsigned char hxd_hv_ref_w3;
    fits_read_col_byt(fp, colnum[HXD_HV_REF_W0], irow, firstelem,
                      nelements, nulval, &hxd_hv_ref_w0, &anynul, &istat);
    data[0] = hxd_hv_ref_w0;
    fits_read_col_byt(fp, colnum[HXD_HV_REF_W1], irow, firstelem,
                      nelements, nulval, &hxd_hv_ref_w1, &anynul, &istat);
    data[1] = hxd_hv_ref_w1;
    fits_read_col_byt(fp, colnum[HXD_HV_REF_W2], irow, firstelem,
                      nelements, nulval, &hxd_hv_ref_w2, &anynul, &istat);
    data[2] = hxd_hv_ref_w2;
    fits_read_col_byt(fp, colnum[HXD_HV_REF_W3], irow, firstelem,
                      nelements, nulval, &hxd_hv_ref_w3, &anynul, &istat);
    data[3] = hxd_hv_ref_w3;
    BnkfPutM ("HXD:HKA:HVREF_W", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_hv_ref_t0;
    unsigned char hxd_hv_ref_t1;
    unsigned char hxd_hv_ref_t2;
    unsigned char hxd_hv_ref_t3;
    fits_read_col_byt(fp, colnum[HXD_HV_REF_T0], irow, firstelem,
                      nelements, nulval, &hxd_hv_ref_t0, &anynul, &istat);
    data[0] = hxd_hv_ref_t0;
    fits_read_col_byt(fp, colnum[HXD_HV_REF_T1], irow, firstelem,
                      nelements, nulval, &hxd_hv_ref_t1, &anynul, &istat);
    data[1] = hxd_hv_ref_t1;
    fits_read_col_byt(fp, colnum[HXD_HV_REF_T2], irow, firstelem,
                      nelements, nulval, &hxd_hv_ref_t2, &anynul, &istat);
    data[2] = hxd_hv_ref_t2;
    fits_read_col_byt(fp, colnum[HXD_HV_REF_T3], irow, firstelem,
                      nelements, nulval, &hxd_hv_ref_t3, &anynul, &istat);
    data[3] = hxd_hv_ref_t3;
    BnkfPutM ("HXD:HKA:HVREF_T", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_hv_ref_p0;
    unsigned char hxd_hv_ref_p1;
    unsigned char hxd_hv_ref_p2;
    unsigned char hxd_hv_ref_p3;
    fits_read_col_byt(fp, colnum[HXD_HV_REF_P0], irow, firstelem,
                      nelements, nulval, &hxd_hv_ref_p0, &anynul, &istat);
    data[0] = hxd_hv_ref_p0;
    fits_read_col_byt(fp, colnum[HXD_HV_REF_P1], irow, firstelem,
                      nelements, nulval, &hxd_hv_ref_p1, &anynul, &istat);
    data[1] = hxd_hv_ref_p1;
    fits_read_col_byt(fp, colnum[HXD_HV_REF_P2], irow, firstelem,
                      nelements, nulval, &hxd_hv_ref_p2, &anynul, &istat);
    data[2] = hxd_hv_ref_p2;
    fits_read_col_byt(fp, colnum[HXD_HV_REF_P3], irow, firstelem,
                      nelements, nulval, &hxd_hv_ref_p3, &anynul, &istat);
    data[3] = hxd_hv_ref_p3;
    BnkfPutM ("HXD:HKA:HVREF_P", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_hv_ref;
    fits_read_col_byt(fp, colnum[HXD_HV_REF], irow, firstelem,
                      nelements, nulval, &hxd_hv_ref, &anynul, &istat);
    data[0] = hxd_hv_ref;
    BnkfPutM ("HXD:HKA:HVREF_DATA", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_hv_w0_volt;
    unsigned short hxd_hv_w1_volt;
    unsigned short hxd_hv_w2_volt;
    unsigned short hxd_hv_w3_volt;
    fits_read_col_usht(fp, colnum[HXD_HV_W0_VOLT], irow, firstelem,
                      nelements, nulval, &hxd_hv_w0_volt, &anynul, &istat);
    data[0] = hxd_hv_w0_volt;
    fits_read_col_usht(fp, colnum[HXD_HV_W1_VOLT], irow, firstelem,
                      nelements, nulval, &hxd_hv_w1_volt, &anynul, &istat);
    data[1] = hxd_hv_w1_volt;
    fits_read_col_usht(fp, colnum[HXD_HV_W2_VOLT], irow, firstelem,
                      nelements, nulval, &hxd_hv_w2_volt, &anynul, &istat);
    data[2] = hxd_hv_w2_volt;
    fits_read_col_usht(fp, colnum[HXD_HV_W3_VOLT], irow, firstelem,
                      nelements, nulval, &hxd_hv_w3_volt, &anynul, &istat);
    data[3] = hxd_hv_w3_volt;
    BnkfPutM ("HXD:HKA:HV_W", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_hv_t0_volt;
    unsigned short hxd_hv_t1_volt;
    unsigned short hxd_hv_t2_volt;
    unsigned short hxd_hv_t3_volt;
    fits_read_col_usht(fp, colnum[HXD_HV_T0_VOLT], irow, firstelem,
                      nelements, nulval, &hxd_hv_t0_volt, &anynul, &istat);
    data[0] = hxd_hv_t0_volt;
    fits_read_col_usht(fp, colnum[HXD_HV_T1_VOLT], irow, firstelem,
                      nelements, nulval, &hxd_hv_t1_volt, &anynul, &istat);
    data[1] = hxd_hv_t1_volt;
    fits_read_col_usht(fp, colnum[HXD_HV_T2_VOLT], irow, firstelem,
                      nelements, nulval, &hxd_hv_t2_volt, &anynul, &istat);
    data[2] = hxd_hv_t2_volt;
    fits_read_col_usht(fp, colnum[HXD_HV_T3_VOLT], irow, firstelem,
                      nelements, nulval, &hxd_hv_t3_volt, &anynul, &istat);
    data[3] = hxd_hv_t3_volt;
    BnkfPutM ("HXD:HKA:HV_T", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_hv_p0_volt;
    unsigned short hxd_hv_p1_volt;
    unsigned short hxd_hv_p2_volt;
    unsigned short hxd_hv_p3_volt;
    fits_read_col_usht(fp, colnum[HXD_HV_P0_VOLT], irow, firstelem,
                      nelements, nulval, &hxd_hv_p0_volt, &anynul, &istat);
    data[0] = hxd_hv_p0_volt;
    fits_read_col_usht(fp, colnum[HXD_HV_P1_VOLT], irow, firstelem,
                      nelements, nulval, &hxd_hv_p1_volt, &anynul, &istat);
    data[1] = hxd_hv_p1_volt;
    fits_read_col_usht(fp, colnum[HXD_HV_P2_VOLT], irow, firstelem,
                      nelements, nulval, &hxd_hv_p2_volt, &anynul, &istat);
    data[2] = hxd_hv_p2_volt;
    fits_read_col_usht(fp, colnum[HXD_HV_P3_VOLT], irow, firstelem,
                      nelements, nulval, &hxd_hv_p3_volt, &anynul, &istat);
    data[3] = hxd_hv_p3_volt;
    BnkfPutM ("HXD:HKA:HV_P", sizeof(int)*4, data);
  }
  {
    double data[4];
    double nulval=1.0;
    double hxd_hv_w0_cal;
    double hxd_hv_w1_cal;
    double hxd_hv_w2_cal;
    double hxd_hv_w3_cal;
    fits_read_col_dbl(fp, colnum[HXD_HV_W0_CAL], irow, firstelem,
                      nelements, nulval, &hxd_hv_w0_cal, &anynul, &istat);
    data[0] = hxd_hv_w0_cal;
    fits_read_col_dbl(fp, colnum[HXD_HV_W1_CAL], irow, firstelem,
                      nelements, nulval, &hxd_hv_w1_cal, &anynul, &istat);
    data[1] = hxd_hv_w1_cal;
    fits_read_col_dbl(fp, colnum[HXD_HV_W2_CAL], irow, firstelem,
                      nelements, nulval, &hxd_hv_w2_cal, &anynul, &istat);
    data[2] = hxd_hv_w2_cal;
    fits_read_col_dbl(fp, colnum[HXD_HV_W3_CAL], irow, firstelem,
                      nelements, nulval, &hxd_hv_w3_cal, &anynul, &istat);
    data[3] = hxd_hv_w3_cal;
    BnkfPutM ("HXD:HKA:HV_W:real", sizeof(double)*4, data);
  }
  {
    double data[4];
    double nulval=1.0;
    double hxd_hv_t0_cal;
    double hxd_hv_t1_cal;
    double hxd_hv_t2_cal;
    double hxd_hv_t3_cal;
    fits_read_col_dbl(fp, colnum[HXD_HV_T0_CAL], irow, firstelem,
                      nelements, nulval, &hxd_hv_t0_cal, &anynul, &istat);
    data[0] = hxd_hv_t0_cal;
    fits_read_col_dbl(fp, colnum[HXD_HV_T1_CAL], irow, firstelem,
                      nelements, nulval, &hxd_hv_t1_cal, &anynul, &istat);
    data[1] = hxd_hv_t1_cal;
    fits_read_col_dbl(fp, colnum[HXD_HV_T2_CAL], irow, firstelem,
                      nelements, nulval, &hxd_hv_t2_cal, &anynul, &istat);
    data[2] = hxd_hv_t2_cal;
    fits_read_col_dbl(fp, colnum[HXD_HV_T3_CAL], irow, firstelem,
                      nelements, nulval, &hxd_hv_t3_cal, &anynul, &istat);
    data[3] = hxd_hv_t3_cal;
    BnkfPutM ("HXD:HKA:HV_T:real", sizeof(double)*4, data);
  }
  {
    double data[4];
    double nulval=1.0;
    double hxd_hv_p0_cal;
    double hxd_hv_p1_cal;
    double hxd_hv_p2_cal;
    double hxd_hv_p3_cal;
    fits_read_col_dbl(fp, colnum[HXD_HV_P0_CAL], irow, firstelem,
                      nelements, nulval, &hxd_hv_p0_cal, &anynul, &istat);
    data[0] = hxd_hv_p0_cal;
    fits_read_col_dbl(fp, colnum[HXD_HV_P1_CAL], irow, firstelem,
                      nelements, nulval, &hxd_hv_p1_cal, &anynul, &istat);
    data[1] = hxd_hv_p1_cal;
    fits_read_col_dbl(fp, colnum[HXD_HV_P2_CAL], irow, firstelem,
                      nelements, nulval, &hxd_hv_p2_cal, &anynul, &istat);
    data[2] = hxd_hv_p2_cal;
    fits_read_col_dbl(fp, colnum[HXD_HV_P3_CAL], irow, firstelem,
                      nelements, nulval, &hxd_hv_p3_cal, &anynul, &istat);
    data[3] = hxd_hv_p3_cal;
    BnkfPutM ("HXD:HKA:HV_P:real", sizeof(double)*4, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_wpu_id;
    fits_read_col_byt(fp, colnum[HXD_WPU_ID], irow, firstelem,
                      nelements, nulval, &hxd_wpu_id, &anynul, &istat);
    data[0] = hxd_wpu_id;
    BnkfPutM ("HXD:HKA:BOARD", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_tmstamp_seqno;
    fits_read_col_byt(fp, colnum[HXD_TMSTAMP_SEQNO], irow, firstelem,
                      nelements, nulval, &hxd_tmstamp_seqno, &anynul, &istat);
    data[0] = hxd_tmstamp_seqno;
    BnkfPutM ("HXD:HKA:STMP_SEQ", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_pin_ud_mod;
    fits_read_col_byt(fp, colnum[HXD_PIN_UD_MOD], irow, firstelem,
                      nelements, nulval, &hxd_pin_ud_mod, &anynul, &istat);
    data[0] = hxd_pin_ud_mod;
    BnkfPutM ("HXD:HKA:PUD_MODE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    int nulval=1;
    int hxd_time_stamp;
    fits_read_col_int(fp, colnum[HXD_TIME_STAMP], irow, firstelem,
                      nelements, nulval, &hxd_time_stamp, &anynul, &istat);
    data[0] = hxd_time_stamp;
    BnkfPutM ("HXD:HKA:STMP_TIM", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    int nulval=1;
    int hxd_wpu_hk_proc_tm;
    fits_read_col_int(fp, colnum[HXD_WPU_HK_PROC_TM], irow, firstelem,
                      nelements, nulval, &hxd_wpu_hk_proc_tm, &anynul, &istat);
    data[0] = hxd_wpu_hk_proc_tm;
    BnkfPutM ("HXD:HKA:WHKPROC_TM", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_gmburst_flag_0;
    unsigned char hxd_gmburst_flag_1;
    unsigned char hxd_gmburst_flag_2;
    unsigned char hxd_gmburst_flag_3;
    fits_read_col_byt(fp, colnum[HXD_GMBURST_FLAG_0], irow, firstelem,
                      nelements, nulval, &hxd_gmburst_flag_0, &anynul, &istat);
    data[0] = hxd_gmburst_flag_0;
    fits_read_col_byt(fp, colnum[HXD_GMBURST_FLAG_1], irow, firstelem,
                      nelements, nulval, &hxd_gmburst_flag_1, &anynul, &istat);
    data[1] = hxd_gmburst_flag_1;
    fits_read_col_byt(fp, colnum[HXD_GMBURST_FLAG_2], irow, firstelem,
                      nelements, nulval, &hxd_gmburst_flag_2, &anynul, &istat);
    data[2] = hxd_gmburst_flag_2;
    fits_read_col_byt(fp, colnum[HXD_GMBURST_FLAG_3], irow, firstelem,
                      nelements, nulval, &hxd_gmburst_flag_3, &anynul, &istat);
    data[3] = hxd_gmburst_flag_3;
    BnkfPutM ("HXD:HKA:GB_FLG", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_gmburst_frz_0;
    unsigned char hxd_gmburst_frz_1;
    unsigned char hxd_gmburst_frz_2;
    unsigned char hxd_gmburst_frz_3;
    fits_read_col_byt(fp, colnum[HXD_GMBURST_FRZ_0], irow, firstelem,
                      nelements, nulval, &hxd_gmburst_frz_0, &anynul, &istat);
    data[0] = hxd_gmburst_frz_0;
    fits_read_col_byt(fp, colnum[HXD_GMBURST_FRZ_1], irow, firstelem,
                      nelements, nulval, &hxd_gmburst_frz_1, &anynul, &istat);
    data[1] = hxd_gmburst_frz_1;
    fits_read_col_byt(fp, colnum[HXD_GMBURST_FRZ_2], irow, firstelem,
                      nelements, nulval, &hxd_gmburst_frz_2, &anynul, &istat);
    data[2] = hxd_gmburst_frz_2;
    fits_read_col_byt(fp, colnum[HXD_GMBURST_FRZ_3], irow, firstelem,
                      nelements, nulval, &hxd_gmburst_frz_3, &anynul, &istat);
    data[3] = hxd_gmburst_frz_3;
    BnkfPutM ("HXD:HKA:GB_FRZ", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_rbm_flag_0;
    unsigned char hxd_rbm_flag_1;
    unsigned char hxd_rbm_flag_2;
    unsigned char hxd_rbm_flag_3;
    fits_read_col_byt(fp, colnum[HXD_RBM_FLAG_0], irow, firstelem,
                      nelements, nulval, &hxd_rbm_flag_0, &anynul, &istat);
    data[0] = hxd_rbm_flag_0;
    fits_read_col_byt(fp, colnum[HXD_RBM_FLAG_1], irow, firstelem,
                      nelements, nulval, &hxd_rbm_flag_1, &anynul, &istat);
    data[1] = hxd_rbm_flag_1;
    fits_read_col_byt(fp, colnum[HXD_RBM_FLAG_2], irow, firstelem,
                      nelements, nulval, &hxd_rbm_flag_2, &anynul, &istat);
    data[2] = hxd_rbm_flag_2;
    fits_read_col_byt(fp, colnum[HXD_RBM_FLAG_3], irow, firstelem,
                      nelements, nulval, &hxd_rbm_flag_3, &anynul, &istat);
    data[3] = hxd_rbm_flag_3;
    BnkfPutM ("HXD:HKA:RBM_FLG", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_tpu0_blk_cnt;
    unsigned char hxd_tpu1_blk_cnt;
    unsigned char hxd_tpu2_blk_cnt;
    unsigned char hxd_tpu3_blk_cnt;
    fits_read_col_byt(fp, colnum[HXD_TPU0_BLK_CNT], irow, firstelem,
                      nelements, nulval, &hxd_tpu0_blk_cnt, &anynul, &istat);
    data[0] = hxd_tpu0_blk_cnt;
    fits_read_col_byt(fp, colnum[HXD_TPU1_BLK_CNT], irow, firstelem,
                      nelements, nulval, &hxd_tpu1_blk_cnt, &anynul, &istat);
    data[1] = hxd_tpu1_blk_cnt;
    fits_read_col_byt(fp, colnum[HXD_TPU2_BLK_CNT], irow, firstelem,
                      nelements, nulval, &hxd_tpu2_blk_cnt, &anynul, &istat);
    data[2] = hxd_tpu2_blk_cnt;
    fits_read_col_byt(fp, colnum[HXD_TPU3_BLK_CNT], irow, firstelem,
                      nelements, nulval, &hxd_tpu3_blk_cnt, &anynul, &istat);
    data[3] = hxd_tpu3_blk_cnt;
    BnkfPutM ("HXD:HKA:TRN_BLCK", sizeof(int)*4, data);
  }
  {
    unsigned int data[5];
    int nulval=1;
    int hxd_hka_trn_ld00;
    int hxd_hka_trn_ld01;
    int hxd_hka_trn_ld02;
    int hxd_hka_trn_ld03;
    int hxd_hka_trn_ld04;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD00], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld00, &anynul, &istat);
    data[0] = hxd_hka_trn_ld00;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD01], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld01, &anynul, &istat);
    data[1] = hxd_hka_trn_ld01;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD02], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld02, &anynul, &istat);
    data[2] = hxd_hka_trn_ld02;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD03], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld03, &anynul, &istat);
    data[3] = hxd_hka_trn_ld03;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD04], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld04, &anynul, &istat);
    data[4] = hxd_hka_trn_ld04;
    BnkfPutM ("HXD:HKA:TRN/A_BD_LD0", sizeof(int)*5, data);
  }
  {
    unsigned int data[5];
    int nulval=1;
    int hxd_hka_trn_ld10;
    int hxd_hka_trn_ld11;
    int hxd_hka_trn_ld12;
    int hxd_hka_trn_ld13;
    int hxd_hka_trn_ld14;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD10], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld10, &anynul, &istat);
    data[0] = hxd_hka_trn_ld10;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD11], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld11, &anynul, &istat);
    data[1] = hxd_hka_trn_ld11;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD12], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld12, &anynul, &istat);
    data[2] = hxd_hka_trn_ld12;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD13], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld13, &anynul, &istat);
    data[3] = hxd_hka_trn_ld13;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD14], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld14, &anynul, &istat);
    data[4] = hxd_hka_trn_ld14;
    BnkfPutM ("HXD:HKA:TRN/A_BD_LD1", sizeof(int)*5, data);
  }
  {
    unsigned int data[5];
    int nulval=1;
    int hxd_hka_trn_ld20;
    int hxd_hka_trn_ld21;
    int hxd_hka_trn_ld22;
    int hxd_hka_trn_ld23;
    int hxd_hka_trn_ld24;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD20], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld20, &anynul, &istat);
    data[0] = hxd_hka_trn_ld20;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD21], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld21, &anynul, &istat);
    data[1] = hxd_hka_trn_ld21;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD22], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld22, &anynul, &istat);
    data[2] = hxd_hka_trn_ld22;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD23], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld23, &anynul, &istat);
    data[3] = hxd_hka_trn_ld23;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD24], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld24, &anynul, &istat);
    data[4] = hxd_hka_trn_ld24;
    BnkfPutM ("HXD:HKA:TRN/A_BD_LD2", sizeof(int)*5, data);
  }
  {
    unsigned int data[5];
    int nulval=1;
    int hxd_hka_trn_ld30;
    int hxd_hka_trn_ld31;
    int hxd_hka_trn_ld32;
    int hxd_hka_trn_ld33;
    int hxd_hka_trn_ld34;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD30], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld30, &anynul, &istat);
    data[0] = hxd_hka_trn_ld30;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD31], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld31, &anynul, &istat);
    data[1] = hxd_hka_trn_ld31;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD32], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld32, &anynul, &istat);
    data[2] = hxd_hka_trn_ld32;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD33], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld33, &anynul, &istat);
    data[3] = hxd_hka_trn_ld33;
    fits_read_col_int(fp, colnum[HXD_HKA_TRN_LD34], irow, firstelem,
                      nelements, nulval, &hxd_hka_trn_ld34, &anynul, &istat);
    data[4] = hxd_hka_trn_ld34;
    BnkfPutM ("HXD:HKA:TRN/A_BD_LD3", sizeof(int)*5, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_wel_a_ld00;
    int hxd_hka_wel_a_ld01;
    int hxd_hka_wel_a_ld02;
    int hxd_hka_wel_a_ld03;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_A_LD00], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_a_ld00, &anynul, &istat);
    data[0] = hxd_hka_wel_a_ld00;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_A_LD01], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_a_ld01, &anynul, &istat);
    data[1] = hxd_hka_wel_a_ld01;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_A_LD02], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_a_ld02, &anynul, &istat);
    data[2] = hxd_hka_wel_a_ld02;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_A_LD03], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_a_ld03, &anynul, &istat);
    data[3] = hxd_hka_wel_a_ld03;
    BnkfPutM ("HXD:HKA:WEL/A_BD_LD0", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_wel_a_ld10;
    int hxd_hka_wel_a_ld11;
    int hxd_hka_wel_a_ld12;
    int hxd_hka_wel_a_ld13;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_A_LD10], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_a_ld10, &anynul, &istat);
    data[0] = hxd_hka_wel_a_ld10;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_A_LD11], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_a_ld11, &anynul, &istat);
    data[1] = hxd_hka_wel_a_ld11;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_A_LD12], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_a_ld12, &anynul, &istat);
    data[2] = hxd_hka_wel_a_ld12;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_A_LD13], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_a_ld13, &anynul, &istat);
    data[3] = hxd_hka_wel_a_ld13;
    BnkfPutM ("HXD:HKA:WEL/A_BD_LD1", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_wel_a_ld20;
    int hxd_hka_wel_a_ld21;
    int hxd_hka_wel_a_ld22;
    int hxd_hka_wel_a_ld23;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_A_LD20], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_a_ld20, &anynul, &istat);
    data[0] = hxd_hka_wel_a_ld20;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_A_LD21], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_a_ld21, &anynul, &istat);
    data[1] = hxd_hka_wel_a_ld21;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_A_LD22], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_a_ld22, &anynul, &istat);
    data[2] = hxd_hka_wel_a_ld22;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_A_LD23], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_a_ld23, &anynul, &istat);
    data[3] = hxd_hka_wel_a_ld23;
    BnkfPutM ("HXD:HKA:WEL/A_BD_LD2", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_wel_a_ld30;
    int hxd_hka_wel_a_ld31;
    int hxd_hka_wel_a_ld32;
    int hxd_hka_wel_a_ld33;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_A_LD30], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_a_ld30, &anynul, &istat);
    data[0] = hxd_hka_wel_a_ld30;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_A_LD31], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_a_ld31, &anynul, &istat);
    data[1] = hxd_hka_wel_a_ld31;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_A_LD32], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_a_ld32, &anynul, &istat);
    data[2] = hxd_hka_wel_a_ld32;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_A_LD33], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_a_ld33, &anynul, &istat);
    data[3] = hxd_hka_wel_a_ld33;
    BnkfPutM ("HXD:HKA:WEL/A_BD_LD3", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_wel_ld00;
    int hxd_hka_wel_ld01;
    int hxd_hka_wel_ld02;
    int hxd_hka_wel_ld03;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_LD00], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_ld00, &anynul, &istat);
    data[0] = hxd_hka_wel_ld00;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_LD01], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_ld01, &anynul, &istat);
    data[1] = hxd_hka_wel_ld01;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_LD02], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_ld02, &anynul, &istat);
    data[2] = hxd_hka_wel_ld02;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_LD03], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_ld03, &anynul, &istat);
    data[3] = hxd_hka_wel_ld03;
    BnkfPutM ("HXD:HKA:WELBD_LD0", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_wel_ld10;
    int hxd_hka_wel_ld11;
    int hxd_hka_wel_ld12;
    int hxd_hka_wel_ld13;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_LD10], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_ld10, &anynul, &istat);
    data[0] = hxd_hka_wel_ld10;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_LD11], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_ld11, &anynul, &istat);
    data[1] = hxd_hka_wel_ld11;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_LD12], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_ld12, &anynul, &istat);
    data[2] = hxd_hka_wel_ld12;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_LD13], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_ld13, &anynul, &istat);
    data[3] = hxd_hka_wel_ld13;
    BnkfPutM ("HXD:HKA:WELBD_LD1", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_wel_ld20;
    int hxd_hka_wel_ld21;
    int hxd_hka_wel_ld22;
    int hxd_hka_wel_ld23;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_LD20], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_ld20, &anynul, &istat);
    data[0] = hxd_hka_wel_ld20;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_LD21], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_ld21, &anynul, &istat);
    data[1] = hxd_hka_wel_ld21;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_LD22], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_ld22, &anynul, &istat);
    data[2] = hxd_hka_wel_ld22;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_LD23], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_ld23, &anynul, &istat);
    data[3] = hxd_hka_wel_ld23;
    BnkfPutM ("HXD:HKA:WELBD_LD2", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_wel_ld30;
    int hxd_hka_wel_ld31;
    int hxd_hka_wel_ld32;
    int hxd_hka_wel_ld33;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_LD30], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_ld30, &anynul, &istat);
    data[0] = hxd_hka_wel_ld30;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_LD31], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_ld31, &anynul, &istat);
    data[1] = hxd_hka_wel_ld31;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_LD32], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_ld32, &anynul, &istat);
    data[2] = hxd_hka_wel_ld32;
    fits_read_col_int(fp, colnum[HXD_HKA_WEL_LD33], irow, firstelem,
                      nelements, nulval, &hxd_hka_wel_ld33, &anynul, &istat);
    data[3] = hxd_hka_wel_ld33;
    BnkfPutM ("HXD:HKA:WELBD_LD3", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_pinld00;
    int hxd_hka_pinld01;
    int hxd_hka_pinld02;
    int hxd_hka_pinld03;
    fits_read_col_int(fp, colnum[HXD_HKA_PINLD00], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinld00, &anynul, &istat);
    data[0] = hxd_hka_pinld00;
    fits_read_col_int(fp, colnum[HXD_HKA_PINLD01], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinld01, &anynul, &istat);
    data[1] = hxd_hka_pinld01;
    fits_read_col_int(fp, colnum[HXD_HKA_PINLD02], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinld02, &anynul, &istat);
    data[2] = hxd_hka_pinld02;
    fits_read_col_int(fp, colnum[HXD_HKA_PINLD03], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinld03, &anynul, &istat);
    data[3] = hxd_hka_pinld03;
    BnkfPutM ("HXD:HKA:PINBD_LD0", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_pinld10;
    int hxd_hka_pinld11;
    int hxd_hka_pinld12;
    int hxd_hka_pinld13;
    fits_read_col_int(fp, colnum[HXD_HKA_PINLD10], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinld10, &anynul, &istat);
    data[0] = hxd_hka_pinld10;
    fits_read_col_int(fp, colnum[HXD_HKA_PINLD11], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinld11, &anynul, &istat);
    data[1] = hxd_hka_pinld11;
    fits_read_col_int(fp, colnum[HXD_HKA_PINLD12], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinld12, &anynul, &istat);
    data[2] = hxd_hka_pinld12;
    fits_read_col_int(fp, colnum[HXD_HKA_PINLD13], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinld13, &anynul, &istat);
    data[3] = hxd_hka_pinld13;
    BnkfPutM ("HXD:HKA:PINBD_LD1", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_pinld20;
    int hxd_hka_pinld21;
    int hxd_hka_pinld22;
    int hxd_hka_pinld23;
    fits_read_col_int(fp, colnum[HXD_HKA_PINLD20], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinld20, &anynul, &istat);
    data[0] = hxd_hka_pinld20;
    fits_read_col_int(fp, colnum[HXD_HKA_PINLD21], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinld21, &anynul, &istat);
    data[1] = hxd_hka_pinld21;
    fits_read_col_int(fp, colnum[HXD_HKA_PINLD22], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinld22, &anynul, &istat);
    data[2] = hxd_hka_pinld22;
    fits_read_col_int(fp, colnum[HXD_HKA_PINLD23], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinld23, &anynul, &istat);
    data[3] = hxd_hka_pinld23;
    BnkfPutM ("HXD:HKA:PINBD_LD2", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_pinld30;
    int hxd_hka_pinld31;
    int hxd_hka_pinld32;
    int hxd_hka_pinld33;
    fits_read_col_int(fp, colnum[HXD_HKA_PINLD30], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinld30, &anynul, &istat);
    data[0] = hxd_hka_pinld30;
    fits_read_col_int(fp, colnum[HXD_HKA_PINLD31], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinld31, &anynul, &istat);
    data[1] = hxd_hka_pinld31;
    fits_read_col_int(fp, colnum[HXD_HKA_PINLD32], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinld32, &anynul, &istat);
    data[2] = hxd_hka_pinld32;
    fits_read_col_int(fp, colnum[HXD_HKA_PINLD33], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinld33, &anynul, &istat);
    data[3] = hxd_hka_pinld33;
    BnkfPutM ("HXD:HKA:PINBD_LD3", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_welud00;
    int hxd_hka_welud01;
    int hxd_hka_welud02;
    int hxd_hka_welud03;
    fits_read_col_int(fp, colnum[HXD_HKA_WELUD00], irow, firstelem,
                      nelements, nulval, &hxd_hka_welud00, &anynul, &istat);
    data[0] = hxd_hka_welud00;
    fits_read_col_int(fp, colnum[HXD_HKA_WELUD01], irow, firstelem,
                      nelements, nulval, &hxd_hka_welud01, &anynul, &istat);
    data[1] = hxd_hka_welud01;
    fits_read_col_int(fp, colnum[HXD_HKA_WELUD02], irow, firstelem,
                      nelements, nulval, &hxd_hka_welud02, &anynul, &istat);
    data[2] = hxd_hka_welud02;
    fits_read_col_int(fp, colnum[HXD_HKA_WELUD03], irow, firstelem,
                      nelements, nulval, &hxd_hka_welud03, &anynul, &istat);
    data[3] = hxd_hka_welud03;
    BnkfPutM ("HXD:HKA:WELBD_UD0", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_welud10;
    int hxd_hka_welud11;
    int hxd_hka_welud12;
    int hxd_hka_welud13;
    fits_read_col_int(fp, colnum[HXD_HKA_WELUD10], irow, firstelem,
                      nelements, nulval, &hxd_hka_welud10, &anynul, &istat);
    data[0] = hxd_hka_welud10;
    fits_read_col_int(fp, colnum[HXD_HKA_WELUD11], irow, firstelem,
                      nelements, nulval, &hxd_hka_welud11, &anynul, &istat);
    data[1] = hxd_hka_welud11;
    fits_read_col_int(fp, colnum[HXD_HKA_WELUD12], irow, firstelem,
                      nelements, nulval, &hxd_hka_welud12, &anynul, &istat);
    data[2] = hxd_hka_welud12;
    fits_read_col_int(fp, colnum[HXD_HKA_WELUD13], irow, firstelem,
                      nelements, nulval, &hxd_hka_welud13, &anynul, &istat);
    data[3] = hxd_hka_welud13;
    BnkfPutM ("HXD:HKA:WELBD_UD1", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_welud20;
    int hxd_hka_welud21;
    int hxd_hka_welud22;
    int hxd_hka_welud23;
    fits_read_col_int(fp, colnum[HXD_HKA_WELUD20], irow, firstelem,
                      nelements, nulval, &hxd_hka_welud20, &anynul, &istat);
    data[0] = hxd_hka_welud20;
    fits_read_col_int(fp, colnum[HXD_HKA_WELUD21], irow, firstelem,
                      nelements, nulval, &hxd_hka_welud21, &anynul, &istat);
    data[1] = hxd_hka_welud21;
    fits_read_col_int(fp, colnum[HXD_HKA_WELUD22], irow, firstelem,
                      nelements, nulval, &hxd_hka_welud22, &anynul, &istat);
    data[2] = hxd_hka_welud22;
    fits_read_col_int(fp, colnum[HXD_HKA_WELUD23], irow, firstelem,
                      nelements, nulval, &hxd_hka_welud23, &anynul, &istat);
    data[3] = hxd_hka_welud23;
    BnkfPutM ("HXD:HKA:WELBD_UD2", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_welud30;
    int hxd_hka_welud31;
    int hxd_hka_welud32;
    int hxd_hka_welud33;
    fits_read_col_int(fp, colnum[HXD_HKA_WELUD30], irow, firstelem,
                      nelements, nulval, &hxd_hka_welud30, &anynul, &istat);
    data[0] = hxd_hka_welud30;
    fits_read_col_int(fp, colnum[HXD_HKA_WELUD31], irow, firstelem,
                      nelements, nulval, &hxd_hka_welud31, &anynul, &istat);
    data[1] = hxd_hka_welud31;
    fits_read_col_int(fp, colnum[HXD_HKA_WELUD32], irow, firstelem,
                      nelements, nulval, &hxd_hka_welud32, &anynul, &istat);
    data[2] = hxd_hka_welud32;
    fits_read_col_int(fp, colnum[HXD_HKA_WELUD33], irow, firstelem,
                      nelements, nulval, &hxd_hka_welud33, &anynul, &istat);
    data[3] = hxd_hka_welud33;
    BnkfPutM ("HXD:HKA:WELBD_UD3", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_pinud00;
    int hxd_hka_pinud01;
    int hxd_hka_pinud02;
    int hxd_hka_pinud03;
    fits_read_col_int(fp, colnum[HXD_HKA_PINUD00], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinud00, &anynul, &istat);
    data[0] = hxd_hka_pinud00;
    fits_read_col_int(fp, colnum[HXD_HKA_PINUD01], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinud01, &anynul, &istat);
    data[1] = hxd_hka_pinud01;
    fits_read_col_int(fp, colnum[HXD_HKA_PINUD02], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinud02, &anynul, &istat);
    data[2] = hxd_hka_pinud02;
    fits_read_col_int(fp, colnum[HXD_HKA_PINUD03], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinud03, &anynul, &istat);
    data[3] = hxd_hka_pinud03;
    BnkfPutM ("HXD:HKA:PINBD_UD0", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_pinud10;
    int hxd_hka_pinud11;
    int hxd_hka_pinud12;
    int hxd_hka_pinud13;
    fits_read_col_int(fp, colnum[HXD_HKA_PINUD10], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinud10, &anynul, &istat);
    data[0] = hxd_hka_pinud10;
    fits_read_col_int(fp, colnum[HXD_HKA_PINUD11], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinud11, &anynul, &istat);
    data[1] = hxd_hka_pinud11;
    fits_read_col_int(fp, colnum[HXD_HKA_PINUD12], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinud12, &anynul, &istat);
    data[2] = hxd_hka_pinud12;
    fits_read_col_int(fp, colnum[HXD_HKA_PINUD13], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinud13, &anynul, &istat);
    data[3] = hxd_hka_pinud13;
    BnkfPutM ("HXD:HKA:PINBD_UD1", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_pinud20;
    int hxd_hka_pinud21;
    int hxd_hka_pinud22;
    int hxd_hka_pinud23;
    fits_read_col_int(fp, colnum[HXD_HKA_PINUD20], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinud20, &anynul, &istat);
    data[0] = hxd_hka_pinud20;
    fits_read_col_int(fp, colnum[HXD_HKA_PINUD21], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinud21, &anynul, &istat);
    data[1] = hxd_hka_pinud21;
    fits_read_col_int(fp, colnum[HXD_HKA_PINUD22], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinud22, &anynul, &istat);
    data[2] = hxd_hka_pinud22;
    fits_read_col_int(fp, colnum[HXD_HKA_PINUD23], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinud23, &anynul, &istat);
    data[3] = hxd_hka_pinud23;
    BnkfPutM ("HXD:HKA:PINBD_UD2", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_pinud30;
    int hxd_hka_pinud31;
    int hxd_hka_pinud32;
    int hxd_hka_pinud33;
    fits_read_col_int(fp, colnum[HXD_HKA_PINUD30], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinud30, &anynul, &istat);
    data[0] = hxd_hka_pinud30;
    fits_read_col_int(fp, colnum[HXD_HKA_PINUD31], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinud31, &anynul, &istat);
    data[1] = hxd_hka_pinud31;
    fits_read_col_int(fp, colnum[HXD_HKA_PINUD32], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinud32, &anynul, &istat);
    data[2] = hxd_hka_pinud32;
    fits_read_col_int(fp, colnum[HXD_HKA_PINUD33], irow, firstelem,
                      nelements, nulval, &hxd_hka_pinud33, &anynul, &istat);
    data[3] = hxd_hka_pinud33;
    BnkfPutM ("HXD:HKA:PINBD_UD3", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_w00_dt;
    int hxd_hka_w01_dt;
    int hxd_hka_w02_dt;
    int hxd_hka_w03_dt;
    fits_read_col_int(fp, colnum[HXD_HKA_W00_DT], irow, firstelem,
                      nelements, nulval, &hxd_hka_w00_dt, &anynul, &istat);
    data[0] = hxd_hka_w00_dt;
    fits_read_col_int(fp, colnum[HXD_HKA_W01_DT], irow, firstelem,
                      nelements, nulval, &hxd_hka_w01_dt, &anynul, &istat);
    data[1] = hxd_hka_w01_dt;
    fits_read_col_int(fp, colnum[HXD_HKA_W02_DT], irow, firstelem,
                      nelements, nulval, &hxd_hka_w02_dt, &anynul, &istat);
    data[2] = hxd_hka_w02_dt;
    fits_read_col_int(fp, colnum[HXD_HKA_W03_DT], irow, firstelem,
                      nelements, nulval, &hxd_hka_w03_dt, &anynul, &istat);
    data[3] = hxd_hka_w03_dt;
    BnkfPutM ("HXD:HKA:WDTCNT0", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_w10_dt;
    int hxd_hka_w11_dt;
    int hxd_hka_w12_dt;
    int hxd_hka_w13_dt;
    fits_read_col_int(fp, colnum[HXD_HKA_W10_DT], irow, firstelem,
                      nelements, nulval, &hxd_hka_w10_dt, &anynul, &istat);
    data[0] = hxd_hka_w10_dt;
    fits_read_col_int(fp, colnum[HXD_HKA_W11_DT], irow, firstelem,
                      nelements, nulval, &hxd_hka_w11_dt, &anynul, &istat);
    data[1] = hxd_hka_w11_dt;
    fits_read_col_int(fp, colnum[HXD_HKA_W12_DT], irow, firstelem,
                      nelements, nulval, &hxd_hka_w12_dt, &anynul, &istat);
    data[2] = hxd_hka_w12_dt;
    fits_read_col_int(fp, colnum[HXD_HKA_W13_DT], irow, firstelem,
                      nelements, nulval, &hxd_hka_w13_dt, &anynul, &istat);
    data[3] = hxd_hka_w13_dt;
    BnkfPutM ("HXD:HKA:WDTCNT1", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_w20_dt;
    int hxd_hka_w21_dt;
    int hxd_hka_w22_dt;
    int hxd_hka_w23_dt;
    fits_read_col_int(fp, colnum[HXD_HKA_W20_DT], irow, firstelem,
                      nelements, nulval, &hxd_hka_w20_dt, &anynul, &istat);
    data[0] = hxd_hka_w20_dt;
    fits_read_col_int(fp, colnum[HXD_HKA_W21_DT], irow, firstelem,
                      nelements, nulval, &hxd_hka_w21_dt, &anynul, &istat);
    data[1] = hxd_hka_w21_dt;
    fits_read_col_int(fp, colnum[HXD_HKA_W22_DT], irow, firstelem,
                      nelements, nulval, &hxd_hka_w22_dt, &anynul, &istat);
    data[2] = hxd_hka_w22_dt;
    fits_read_col_int(fp, colnum[HXD_HKA_W23_DT], irow, firstelem,
                      nelements, nulval, &hxd_hka_w23_dt, &anynul, &istat);
    data[3] = hxd_hka_w23_dt;
    BnkfPutM ("HXD:HKA:WDTCNT2", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    int nulval=1;
    int hxd_hka_w30_dt;
    int hxd_hka_w31_dt;
    int hxd_hka_w32_dt;
    int hxd_hka_w33_dt;
    fits_read_col_int(fp, colnum[HXD_HKA_W30_DT], irow, firstelem,
                      nelements, nulval, &hxd_hka_w30_dt, &anynul, &istat);
    data[0] = hxd_hka_w30_dt;
    fits_read_col_int(fp, colnum[HXD_HKA_W31_DT], irow, firstelem,
                      nelements, nulval, &hxd_hka_w31_dt, &anynul, &istat);
    data[1] = hxd_hka_w31_dt;
    fits_read_col_int(fp, colnum[HXD_HKA_W32_DT], irow, firstelem,
                      nelements, nulval, &hxd_hka_w32_dt, &anynul, &istat);
    data[2] = hxd_hka_w32_dt;
    fits_read_col_int(fp, colnum[HXD_HKA_W33_DT], irow, firstelem,
                      nelements, nulval, &hxd_hka_w33_dt, &anynul, &istat);
    data[3] = hxd_hka_w33_dt;
    BnkfPutM ("HXD:HKA:WDTCNT3", sizeof(int)*4, data);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_col failed (%d)\n",
	    pname, istat);
    return istat;
  }
  
  return ANL_OK;
  
}
