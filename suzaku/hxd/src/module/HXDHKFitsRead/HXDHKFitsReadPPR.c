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
  HXD_PPR_PI_VERSION,
  HXD_PPR_LOOPTIMES,
  HXD_PPR_AE_CMD_MODE,
  HXD_PPR_MONITOR_MODE,
  HXD_PPR_HK_MODE,
  HXD_PPR_WEL_FUNC,
  HXD_PPR_BST_READ_MASK,
  HXD_PPR_BST_JDG_MODE0,
  HXD_PPR_BST_JDG_MODE1,
  HXD_PPR_BST_JDG_MODE2,
  HXD_PPR_BST_JDG_MODE3,
  HXD_PPR_TRN_MODE,
  HXD_PPR_TRN_BIN_MODE,
  HXD_PPR_TRN_SUM_MODE,
  HXD_PPR_TRN_BIN0,
  HXD_PPR_TRN_BIN1,
  HXD_PPR_TRN_BIN2,
  HXD_PPR_TRN_BIN3,
  HXD_PPR_TRN_BIN4,
  HXD_PPR_TRN_BIN5,
  HXD_PPR_TRN_BIN6,
  HXD_PPR_TRN_BIN7,
  HXD_PPR_TRN_BIN8,
  HXD_PPR_TRN_BIN9,
  HXD_PPR_TRN_BIN10,
  HXD_PPR_TRN_BIN11,
  HXD_PPR_TRN_BIN12,
  HXD_PPR_TRN_BIN13,
  HXD_PPR_TRN_BIN14,
  HXD_PPR_TRN_BIN15,
  HXD_PPR_TRN_BIN16,
  HXD_PPR_TRN_BIN17,
  HXD_PPR_TRN_BIN18,
  HXD_PPR_TRN_BIN19,
  HXD_PPR_TRN_BIN20,
  HXD_PPR_TRN_BIN21,
  HXD_PPR_TRN_BIN22,
  HXD_PPR_TRN_BIN23,
  HXD_PPR_TRN_BIN24,
  HXD_PPR_TRN_BIN25,
  HXD_PPR_TRN_BIN26,
  HXD_PPR_TRN_BIN27,
  HXD_PPR_TRN_BIN28,
  HXD_PPR_TRN_BIN29,
  HXD_PPR_TRN_BIN30,
  HXD_PPR_TRN_BIN31,
  HXD_PPR_TRN_BIN32,
  HXD_PPR_TRN_BIN33,
  HXD_PPR_TRN_BIN34,
  HXD_PPR_TRN_BIN35,
  HXD_PPR_TRN_BIN36,
  HXD_PPR_TRN_BIN37,
  HXD_PPR_TRN_BIN38,
  HXD_PPR_TRN_BIN39,
  HXD_PPR_TRN_BIN40,
  HXD_PPR_TRN_BIN41,
  HXD_PPR_TRN_BIN42,
  HXD_PPR_TRN_BIN43,
  HXD_PPR_TRN_BIN44,
  HXD_PPR_TRN_BIN45,
  HXD_PPR_TRN_BIN46,
  HXD_PPR_TRN_BIN47,
  HXD_PPR_TRN_BIN48,
  HXD_PPR_TRN_BIN49,
  HXD_PPR_TRN_BIN50,
  HXD_PPR_TRN_BIN51,
  HXD_PPR_TRN_BIN52,
  HXD_PPR_TRN_BIN53,
  HXD_PPR_TRN_TBL0,
  HXD_PPR_TRN_TBL1,
  HXD_PPR_TRN_TBL2,
  HXD_PPR_TRN_TBL3,
  HXD_PPR_TRN_TBL4,
  HXD_PPR_TRN_TBL5,
  HXD_PPR_TRN_TBL6,
  HXD_PPR_TRN_TBL7,
  HXD_PPR_TRN_MEMTBL0,
  HXD_PPR_TRN_MEMTBL1,
  HXD_PPR_TRN_MEMTBL2,
  HXD_PPR_TRN_MEMTBL3,
  HXD_PPR_TRN_NBIN_MD0,
  HXD_PPR_TRN_NBIN_MD1,
  HXD_PPR_TRN_NBIN_MD2,
  HXD_PPR_TRN_NBIN_MD3,
  HXD_PPR_PHSF_FUNIT_I,
  HXD_PPR_PHSF_FUNIT_II,
  HXD_PPR_HST_MODE0,
  HXD_PPR_HST_MODE1,
  HXD_PPR_PHSF_PRESET0,
  HXD_PPR_PHSF_PRESET1,
  HXD_PPR_HSF_ASSIGN0,
  HXD_PPR_HSF_ASSIGN1,
  HXD_PPR_HST_ACQUIRE,
  HXD_PPR_EVSEL_MODE0,
  HXD_PPR_EVSEL_MODE1,
  HXD_PPR_EVSEL_MODE2,
  HXD_PPR_EVSEL_MODE3,
  HXD_PPR_EVSEL_MODE4,
  HXD_PPR_EVSEL_MODE5,
  HXD_PPR_EVSEL_MODE6,
  HXD_PPR_EVSEL_MODE7,
  HXD_PPR_EVSEL_MODE8,
  HXD_PPR_EVSEL_MODE9,
  HXD_PPR_EVSEL_MODE10,
  HXD_PPR_EVSEL_MODE11,
  HXD_PPR_EVSEL_MODE12,
  HXD_PPR_EVSEL_MODE13,
  HXD_PPR_EVSEL_MODE14,
  HXD_PPR_EVSEL_MODE15,
  HXD_PPR_EVSEL_2D_FLD0,
  HXD_PPR_EVSEL_2D_FLD1,
  HXD_PPR_EVSEL_2D_FLD2,
  HXD_PPR_EVSEL_2D_FLD3,
  HXD_PPR_EVSEL_2D_FLD4,
  HXD_PPR_EVSEL_2D_FLD5,
  HXD_PPR_EVSEL_2D_FLD6,
  HXD_PPR_EVSEL_2D_FLD7,
  HXD_PPR_EVSEL_2D_FLD8,
  HXD_PPR_EVSEL_2D_FLD9,
  HXD_PPR_EVSEL_2D_FLD10,
  HXD_PPR_EVSEL_2D_FLD11,
  HXD_PPR_EVSEL_2D_FLD12,
  HXD_PPR_EVSEL_2D_FLD13,
  HXD_PPR_EVSEL_2D_FLD14,
  HXD_PPR_EVSEL_2D_FLD15,
  HXD_PPR_EVSEL_2D_FMAX0,
  HXD_PPR_EVSEL_2D_FMAX1,
  HXD_PPR_EVSEL_2D_FMAX2,
  HXD_PPR_EVSEL_2D_FMAX3,
  HXD_PPR_EVSEL_2D_FMAX4,
  HXD_PPR_EVSEL_2D_FMAX5,
  HXD_PPR_EVSEL_2D_FMAX6,
  HXD_PPR_EVSEL_2D_FMAX7,
  HXD_PPR_EVSEL_2D_FMAX8,
  HXD_PPR_EVSEL_2D_FMAX9,
  HXD_PPR_EVSEL_2D_FMAX10,
  HXD_PPR_EVSEL_2D_FMAX11,
  HXD_PPR_EVSEL_2D_FMAX12,
  HXD_PPR_EVSEL_2D_FMAX13,
  HXD_PPR_EVSEL_2D_FMAX14,
  HXD_PPR_EVSEL_2D_FMAX15,
  HXD_PPR_EVSEL_2D_FMAX16,
  HXD_PPR_EVSEL_2D_FMAX17,
  HXD_PPR_EVSEL_2D_FMAX18,
  HXD_PPR_EVSEL_2D_FMAX19,
  HXD_PPR_EVSEL_2D_FMAX20,
  HXD_PPR_EVSEL_2D_FMAX21,
  HXD_PPR_EVSEL_2D_FMAX22,
  HXD_PPR_EVSEL_2D_FMAX23,
  HXD_PPR_EVSEL_2D_FMAX24,
  HXD_PPR_EVSEL_2D_FMAX25,
  HXD_PPR_EVSEL_2D_FMAX26,
  HXD_PPR_EVSEL_2D_FMAX27,
  HXD_PPR_EVSEL_2D_FMAX28,
  HXD_PPR_EVSEL_2D_FMAX29,
  HXD_PPR_EVSEL_2D_FMAX30,
  HXD_PPR_EVSEL_2D_FMAX31,
  HXD_PPR_EVSEL_2D_FMIN0,
  HXD_PPR_EVSEL_2D_FMIN1,
  HXD_PPR_EVSEL_2D_FMIN2,
  HXD_PPR_EVSEL_2D_FMIN3,
  HXD_PPR_EVSEL_2D_FMIN4,
  HXD_PPR_EVSEL_2D_FMIN5,
  HXD_PPR_EVSEL_2D_FMIN6,
  HXD_PPR_EVSEL_2D_FMIN7,
  HXD_PPR_EVSEL_2D_FMIN8,
  HXD_PPR_EVSEL_2D_FMIN9,
  HXD_PPR_EVSEL_2D_FMIN10,
  HXD_PPR_EVSEL_2D_FMIN11,
  HXD_PPR_EVSEL_2D_FMIN12,
  HXD_PPR_EVSEL_2D_FMIN13,
  HXD_PPR_EVSEL_2D_FMIN14,
  HXD_PPR_EVSEL_2D_FMIN15,
  HXD_PPR_EVSEL_2D_FMIN16,
  HXD_PPR_EVSEL_2D_FMIN17,
  HXD_PPR_EVSEL_2D_FMIN18,
  HXD_PPR_EVSEL_2D_FMIN19,
  HXD_PPR_EVSEL_2D_FMIN20,
  HXD_PPR_EVSEL_2D_FMIN21,
  HXD_PPR_EVSEL_2D_FMIN22,
  HXD_PPR_EVSEL_2D_FMIN23,
  HXD_PPR_EVSEL_2D_FMIN24,
  HXD_PPR_EVSEL_2D_FMIN25,
  HXD_PPR_EVSEL_2D_FMIN26,
  HXD_PPR_EVSEL_2D_FMIN27,
  HXD_PPR_EVSEL_2D_FMIN28,
  HXD_PPR_EVSEL_2D_FMIN29,
  HXD_PPR_EVSEL_2D_FMIN30,
  HXD_PPR_EVSEL_2D_FMIN31,
  HXD_PPR_EVSEL_2D_MAXH0,
  HXD_PPR_EVSEL_2D_MAXH1,
  HXD_PPR_EVSEL_2D_MAXH2,
  HXD_PPR_EVSEL_2D_MAXH3,
  HXD_PPR_EVSEL_2D_MAXH4,
  HXD_PPR_EVSEL_2D_MAXH5,
  HXD_PPR_EVSEL_2D_MAXH6,
  HXD_PPR_EVSEL_2D_MAXH7,
  HXD_PPR_EVSEL_2D_MAXH8,
  HXD_PPR_EVSEL_2D_MAXH9,
  HXD_PPR_EVSEL_2D_MAXH10,
  HXD_PPR_EVSEL_2D_MAXH11,
  HXD_PPR_EVSEL_2D_MAXH12,
  HXD_PPR_EVSEL_2D_MAXH13,
  HXD_PPR_EVSEL_2D_MAXH14,
  HXD_PPR_EVSEL_2D_MAXH15,
  HXD_PPR_EVSEL_2D_MAXH16,
  HXD_PPR_EVSEL_2D_MAXH17,
  HXD_PPR_EVSEL_2D_MAXH18,
  HXD_PPR_EVSEL_2D_MAXH19,
  HXD_PPR_EVSEL_2D_MAXH20,
  HXD_PPR_EVSEL_2D_MAXH21,
  HXD_PPR_EVSEL_2D_MAXH22,
  HXD_PPR_EVSEL_2D_MAXH23,
  HXD_PPR_EVSEL_2D_MAXH24,
  HXD_PPR_EVSEL_2D_MAXH25,
  HXD_PPR_EVSEL_2D_MAXH26,
  HXD_PPR_EVSEL_2D_MAXH27,
  HXD_PPR_EVSEL_2D_MAXH28,
  HXD_PPR_EVSEL_2D_MAXH29,
  HXD_PPR_EVSEL_2D_MAXH30,
  HXD_PPR_EVSEL_2D_MAXH31,
  HXD_PPR_EVSEL_2D_MAXL0,
  HXD_PPR_EVSEL_2D_MAXL1,
  HXD_PPR_EVSEL_2D_MAXL2,
  HXD_PPR_EVSEL_2D_MAXL3,
  HXD_PPR_EVSEL_2D_MAXL4,
  HXD_PPR_EVSEL_2D_MAXL5,
  HXD_PPR_EVSEL_2D_MAXL6,
  HXD_PPR_EVSEL_2D_MAXL7,
  HXD_PPR_EVSEL_2D_MAXL8,
  HXD_PPR_EVSEL_2D_MAXL9,
  HXD_PPR_EVSEL_2D_MAXL10,
  HXD_PPR_EVSEL_2D_MAXL11,
  HXD_PPR_EVSEL_2D_MAXL12,
  HXD_PPR_EVSEL_2D_MAXL13,
  HXD_PPR_EVSEL_2D_MAXL14,
  HXD_PPR_EVSEL_2D_MAXL15,
  HXD_PPR_EVSEL_2D_MAXL16,
  HXD_PPR_EVSEL_2D_MAXL17,
  HXD_PPR_EVSEL_2D_MAXL18,
  HXD_PPR_EVSEL_2D_MAXL19,
  HXD_PPR_EVSEL_2D_MAXL20,
  HXD_PPR_EVSEL_2D_MAXL21,
  HXD_PPR_EVSEL_2D_MAXL22,
  HXD_PPR_EVSEL_2D_MAXL23,
  HXD_PPR_EVSEL_2D_MAXL24,
  HXD_PPR_EVSEL_2D_MAXL25,
  HXD_PPR_EVSEL_2D_MAXL26,
  HXD_PPR_EVSEL_2D_MAXL27,
  HXD_PPR_EVSEL_2D_MAXL28,
  HXD_PPR_EVSEL_2D_MAXL29,
  HXD_PPR_EVSEL_2D_MAXL30,
  HXD_PPR_EVSEL_2D_MAXL31,
  HXD_PPR_EVSEL_2D_MINH0,
  HXD_PPR_EVSEL_2D_MINH1,
  HXD_PPR_EVSEL_2D_MINH2,
  HXD_PPR_EVSEL_2D_MINH3,
  HXD_PPR_EVSEL_2D_MINH4,
  HXD_PPR_EVSEL_2D_MINH5,
  HXD_PPR_EVSEL_2D_MINH6,
  HXD_PPR_EVSEL_2D_MINH7,
  HXD_PPR_EVSEL_2D_MINH8,
  HXD_PPR_EVSEL_2D_MINH9,
  HXD_PPR_EVSEL_2D_MINH10,
  HXD_PPR_EVSEL_2D_MINH11,
  HXD_PPR_EVSEL_2D_MINH12,
  HXD_PPR_EVSEL_2D_MINH13,
  HXD_PPR_EVSEL_2D_MINH14,
  HXD_PPR_EVSEL_2D_MINH15,
  HXD_PPR_EVSEL_2D_MINH16,
  HXD_PPR_EVSEL_2D_MINH17,
  HXD_PPR_EVSEL_2D_MINH18,
  HXD_PPR_EVSEL_2D_MINH19,
  HXD_PPR_EVSEL_2D_MINH20,
  HXD_PPR_EVSEL_2D_MINH21,
  HXD_PPR_EVSEL_2D_MINH22,
  HXD_PPR_EVSEL_2D_MINH23,
  HXD_PPR_EVSEL_2D_MINH24,
  HXD_PPR_EVSEL_2D_MINH25,
  HXD_PPR_EVSEL_2D_MINH26,
  HXD_PPR_EVSEL_2D_MINH27,
  HXD_PPR_EVSEL_2D_MINH28,
  HXD_PPR_EVSEL_2D_MINH29,
  HXD_PPR_EVSEL_2D_MINH30,
  HXD_PPR_EVSEL_2D_MINH31,
  HXD_PPR_EVSEL_2D_MINL0,
  HXD_PPR_EVSEL_2D_MINL1,
  HXD_PPR_EVSEL_2D_MINL2,
  HXD_PPR_EVSEL_2D_MINL3,
  HXD_PPR_EVSEL_2D_MINL4,
  HXD_PPR_EVSEL_2D_MINL5,
  HXD_PPR_EVSEL_2D_MINL6,
  HXD_PPR_EVSEL_2D_MINL7,
  HXD_PPR_EVSEL_2D_MINL8,
  HXD_PPR_EVSEL_2D_MINL9,
  HXD_PPR_EVSEL_2D_MINL10,
  HXD_PPR_EVSEL_2D_MINL11,
  HXD_PPR_EVSEL_2D_MINL12,
  HXD_PPR_EVSEL_2D_MINL13,
  HXD_PPR_EVSEL_2D_MINL14,
  HXD_PPR_EVSEL_2D_MINL15,
  HXD_PPR_EVSEL_2D_MINL16,
  HXD_PPR_EVSEL_2D_MINL17,
  HXD_PPR_EVSEL_2D_MINL18,
  HXD_PPR_EVSEL_2D_MINL19,
  HXD_PPR_EVSEL_2D_MINL20,
  HXD_PPR_EVSEL_2D_MINL21,
  HXD_PPR_EVSEL_2D_MINL22,
  HXD_PPR_EVSEL_2D_MINL23,
  HXD_PPR_EVSEL_2D_MINL24,
  HXD_PPR_EVSEL_2D_MINL25,
  HXD_PPR_EVSEL_2D_MINL26,
  HXD_PPR_EVSEL_2D_MINL27,
  HXD_PPR_EVSEL_2D_MINL28,
  HXD_PPR_EVSEL_2D_MINL29,
  HXD_PPR_EVSEL_2D_MINL30,
  HXD_PPR_EVSEL_2D_MINL31,
  HXD_PPR_EVSEL_DLT_TIME0,
  HXD_PPR_EVSEL_DLT_TIME1,
  HXD_PPR_EVSEL_DLT_TIME2,
  HXD_PPR_EVSEL_DLT_TIME3,
  HXD_PPR_EVSEL_DLT_TIME4,
  HXD_PPR_EVSEL_DLT_TIME5,
  HXD_PPR_EVSEL_DLT_TIME6,
  HXD_PPR_EVSEL_DLT_TIME7,
  HXD_PPR_EVSEL_DLT_TIME8,
  HXD_PPR_EVSEL_DLT_TIME9,
  HXD_PPR_EVSEL_DLT_TIME10,
  HXD_PPR_EVSEL_DLT_TIME11,
  HXD_PPR_EVSEL_DLT_TIME12,
  HXD_PPR_EVSEL_DLT_TIME13,
  HXD_PPR_EVSEL_DLT_TIME14,
  HXD_PPR_EVSEL_DLT_TIME15,
  HXD_PPR_EVSEL_FLG_MASK0,
  HXD_PPR_EVSEL_FLG_MASK1,
  HXD_PPR_EVSEL_FLG_MASK2,
  HXD_PPR_EVSEL_FLG_MASK3,
  HXD_PPR_EVSEL_FLG_MASK4,
  HXD_PPR_EVSEL_FLG_MASK5,
  HXD_PPR_EVSEL_FLG_MASK6,
  HXD_PPR_EVSEL_FLG_MASK7,
  HXD_PPR_EVSEL_FLG_MASK8,
  HXD_PPR_EVSEL_FLG_MASK9,
  HXD_PPR_EVSEL_FLG_MASK10,
  HXD_PPR_EVSEL_FLG_MASK11,
  HXD_PPR_EVSEL_FLG_MASK12,
  HXD_PPR_EVSEL_FLG_MASK13,
  HXD_PPR_EVSEL_FLG_MASK14,
  HXD_PPR_EVSEL_FLG_MASK15,
  HXD_PPR_EVSEL_TRG_MASK0,
  HXD_PPR_EVSEL_TRG_MASK1,
  HXD_PPR_EVSEL_TRG_MASK2,
  HXD_PPR_EVSEL_TRG_MASK3,
  HXD_PPR_EVSEL_TRG_MASK4,
  HXD_PPR_EVSEL_TRG_MASK5,
  HXD_PPR_EVSEL_TRG_MASK6,
  HXD_PPR_EVSEL_TRG_MASK7,
  HXD_PPR_EVSEL_TRG_MASK8,
  HXD_PPR_EVSEL_TRG_MASK9,
  HXD_PPR_EVSEL_TRG_MASK10,
  HXD_PPR_EVSEL_TRG_MASK11,
  HXD_PPR_EVSEL_TRG_MASK12,
  HXD_PPR_EVSEL_TRG_MASK13,
  HXD_PPR_EVSEL_TRG_MASK14,
  HXD_PPR_EVSEL_TRG_MASK15,
  HXD_PPR_EVSEL_HIT_MSK10,
  HXD_PPR_EVSEL_HIT_MSK11,
  HXD_PPR_EVSEL_HIT_MSK12,
  HXD_PPR_EVSEL_HIT_MSK13,
  HXD_PPR_EVSEL_HIT_MSK14,
  HXD_PPR_EVSEL_HIT_MSK15,
  HXD_PPR_EVSEL_HIT_MSK16,
  HXD_PPR_EVSEL_HIT_MSK17,
  HXD_PPR_EVSEL_HIT_MSK18,
  HXD_PPR_EVSEL_HIT_MSK19,
  HXD_PPR_EVSEL_HIT_MSK110,
  HXD_PPR_EVSEL_HIT_MSK111,
  HXD_PPR_EVSEL_HIT_MSK112,
  HXD_PPR_EVSEL_HIT_MSK113,
  HXD_PPR_EVSEL_HIT_MSK114,
  HXD_PPR_EVSEL_HIT_MSK115,
  HXD_PPR_EVSEL_HIT_MSK20,
  HXD_PPR_EVSEL_HIT_MSK21,
  HXD_PPR_EVSEL_HIT_MSK22,
  HXD_PPR_EVSEL_HIT_MSK23,
  HXD_PPR_EVSEL_HIT_MSK24,
  HXD_PPR_EVSEL_HIT_MSK25,
  HXD_PPR_EVSEL_HIT_MSK26,
  HXD_PPR_EVSEL_HIT_MSK27,
  HXD_PPR_EVSEL_HIT_MSK28,
  HXD_PPR_EVSEL_HIT_MSK29,
  HXD_PPR_EVSEL_HIT_MSK210,
  HXD_PPR_EVSEL_HIT_MSK211,
  HXD_PPR_EVSEL_HIT_MSK212,
  HXD_PPR_EVSEL_HIT_MSK213,
  HXD_PPR_EVSEL_HIT_MSK214,
  HXD_PPR_EVSEL_HIT_MSK215,
  HXD_PPR_EVSEL_PIN_LD0,
  HXD_PPR_EVSEL_PIN_LD1,
  HXD_PPR_EVSEL_PIN_LD2,
  HXD_PPR_EVSEL_PIN_LD3,
  HXD_PPR_EVSEL_PIN_LD4,
  HXD_PPR_EVSEL_PIN_LD5,
  HXD_PPR_EVSEL_PIN_LD6,
  HXD_PPR_EVSEL_PIN_LD7,
  HXD_PPR_EVSEL_PIN_LD8,
  HXD_PPR_EVSEL_PIN_LD9,
  HXD_PPR_EVSEL_PIN_LD10,
  HXD_PPR_EVSEL_PIN_LD11,
  HXD_PPR_EVSEL_PIN_LD12,
  HXD_PPR_EVSEL_PIN_LD13,
  HXD_PPR_EVSEL_PIN_LD14,
  HXD_PPR_EVSEL_PIN_LD15,
  HXD_PPR_EVSEL_PIN_LD16,
  HXD_PPR_EVSEL_PIN_LD17,
  HXD_PPR_EVSEL_PIN_LD18,
  HXD_PPR_EVSEL_PIN_LD19,
  HXD_PPR_EVSEL_PIN_LD20,
  HXD_PPR_EVSEL_PIN_LD21,
  HXD_PPR_EVSEL_PIN_LD22,
  HXD_PPR_EVSEL_PIN_LD23,
  HXD_PPR_EVSEL_PIN_LD24,
  HXD_PPR_EVSEL_PIN_LD25,
  HXD_PPR_EVSEL_PIN_LD26,
  HXD_PPR_EVSEL_PIN_LD27,
  HXD_PPR_EVSEL_PIN_LD28,
  HXD_PPR_EVSEL_PIN_LD29,
  HXD_PPR_EVSEL_PIN_LD30,
  HXD_PPR_EVSEL_PIN_LD31,
  HXD_PPR_EVSEL_PIN_LD32,
  HXD_PPR_EVSEL_PIN_LD33,
  HXD_PPR_EVSEL_PIN_LD34,
  HXD_PPR_EVSEL_PIN_LD35,
  HXD_PPR_EVSEL_PIN_LD36,
  HXD_PPR_EVSEL_PIN_LD37,
  HXD_PPR_EVSEL_PIN_LD38,
  HXD_PPR_EVSEL_PIN_LD39,
  HXD_PPR_EVSEL_PIN_LD40,
  HXD_PPR_EVSEL_PIN_LD41,
  HXD_PPR_EVSEL_PIN_LD42,
  HXD_PPR_EVSEL_PIN_LD43,
  HXD_PPR_EVSEL_PIN_LD44,
  HXD_PPR_EVSEL_PIN_LD45,
  HXD_PPR_EVSEL_PIN_LD46,
  HXD_PPR_EVSEL_PIN_LD47,
  HXD_PPR_EVSEL_PIN_LD48,
  HXD_PPR_EVSEL_PIN_LD49,
  HXD_PPR_EVSEL_PIN_LD50,
  HXD_PPR_EVSEL_PIN_LD51,
  HXD_PPR_EVSEL_PIN_LD52,
  HXD_PPR_EVSEL_PIN_LD53,
  HXD_PPR_EVSEL_PIN_LD54,
  HXD_PPR_EVSEL_PIN_LD55,
  HXD_PPR_EVSEL_PIN_LD56,
  HXD_PPR_EVSEL_PIN_LD57,
  HXD_PPR_EVSEL_PIN_LD58,
  HXD_PPR_EVSEL_PIN_LD59,
  HXD_PPR_EVSEL_PIN_LD60,
  HXD_PPR_EVSEL_PIN_LD61,
  HXD_PPR_EVSEL_PIN_LD62,
  HXD_PPR_EVSEL_PIN_LD63,
  HXD_PPR_BST_INTERVAL0,
  HXD_PPR_BST_INTERVAL1,
  HXD_PPR_BST_INTERVAL2,
  HXD_PPR_BST_INTERVAL3,
  HXD_PPR_GB_TRG_MSK0,
  HXD_PPR_GB_TRG_MSK1,
  HXD_PPR_GB_TRG_MSK2,
  HXD_PPR_GB_TRG_MSK3,
  HXD_PPR_GB_PH_FRM0,
  HXD_PPR_GB_PH_FRM1,
  HXD_PPR_GB_PH_FRM2,
  HXD_PPR_GB_PH_FRM3,
  HXD_PPR_GB_PH_FRM4,
  HXD_PPR_GB_PH_FRM5,
  HXD_PPR_GB_PH_FRM6,
  HXD_PPR_GB_PH_FRM7,
  HXD_PPR_GB_PH_TO0,
  HXD_PPR_GB_PH_TO1,
  HXD_PPR_GB_PH_TO2,
  HXD_PPR_GB_PH_TO3,
  HXD_PPR_GB_PH_TO4,
  HXD_PPR_GB_PH_TO5,
  HXD_PPR_GB_PH_TO6,
  HXD_PPR_GB_PH_TO7,
  HXD_PPR_GB_WANT_SUM0,
  HXD_PPR_GB_WANT_SUM1,
  HXD_PPR_GB_WANT_SUM2,
  HXD_PPR_GB_WANT_SUM3,
  HXD_PPR_GB_PH_FLG0,
  HXD_PPR_GB_PH_FLG1,
  HXD_PPR_GB_PH_FLG2,
  HXD_PPR_GB_PH_FLG3,
  HXD_PPR_GB_WLD_FLG0,
  HXD_PPR_GB_WLD_FLG1,
  HXD_PPR_GB_WLD_FLG2,
  HXD_PPR_GB_WLD_FLG3,
  HXD_PPR_GB_PSE_FLG0,
  HXD_PPR_GB_PSE_FLG1,
  HXD_PPR_GB_PSE_FLG2,
  HXD_PPR_GB_PSE_FLG3,
  HXD_PPR_GB_ITIME_PH0,
  HXD_PPR_GB_ITIME_PH1,
  HXD_PPR_GB_ITIME_PH2,
  HXD_PPR_GB_ITIME_PH3,
  HXD_PPR_GB_ITIME_PH4,
  HXD_PPR_GB_ITIME_PH5,
  HXD_PPR_GB_ITIME_PH6,
  HXD_PPR_GB_ITIME_PH7,
  HXD_PPR_GB_LEVEL_PH0,
  HXD_PPR_GB_LEVEL_PH1,
  HXD_PPR_GB_LEVEL_PH2,
  HXD_PPR_GB_LEVEL_PH3,
  HXD_PPR_GB_LEVEL_PH4,
  HXD_PPR_GB_LEVEL_PH5,
  HXD_PPR_GB_LEVEL_PH6,
  HXD_PPR_GB_LEVEL_PH7,
  HXD_PPR_GB_ITIME_WLD0,
  HXD_PPR_GB_ITIME_WLD1,
  HXD_PPR_GB_ITIME_WLD2,
  HXD_PPR_GB_ITIME_WLD3,
  HXD_PPR_GB_LEVEL_WLD0,
  HXD_PPR_GB_LEVEL_WLD1,
  HXD_PPR_GB_LEVEL_WLD2,
  HXD_PPR_GB_LEVEL_WLD3,
  HXD_PPR_GB_ITIME_PSE0,
  HXD_PPR_GB_ITIME_PSE1,
  HXD_PPR_GB_ITIME_PSE2,
  HXD_PPR_GB_ITIME_PSE3,
  HXD_PPR_GB_LEVEL_PSE0,
  HXD_PPR_GB_LEVEL_PSE1,
  HXD_PPR_GB_LEVEL_PSE2,
  HXD_PPR_GB_LEVEL_PSE3,
  HXD_PPR_GB_TRG_MODE,
  HXD_PPR_GB_PH_MODE,
  HXD_PPR_GB_WLD_MODE,
  HXD_PPR_GB_PSE_MODE,
};

static char pname[] = "HXDHKFitsReadPPR";

static int colnum[537];
static int time_colnum;

void
HXDHKFitsReadPPR_init()
{
  BnkDef( "HXD:PPR:PACKET_AETIME", sizeof(double) );
  BnkDef( "HXD:PPR:PI_VERSION", sizeof(int) );
  BnkDef( "HXD:PPR:LOOPTIMES", sizeof(int) );
  BnkDef( "HXD:PPR:AE_CMD_MODE", sizeof(int) );
  BnkDef( "HXD:PPR:MONITOR_MODE", sizeof(int) );
  BnkDef( "HXD:PPR:HK_MODE", sizeof(int) );
  BnkDef( "HXD:PPR:WEL_FUNC", sizeof(int) );
  BnkDef( "HXD:PPR:BST_READ_MASK", sizeof(int) );
  BnkDef( "HXD:PPR:BST_JDG_MODE", sizeof(int)*4 );
  BnkDef( "HXD:PPR:TRN_MODE", sizeof(int) );
  BnkDef( "HXD:PPR:TRN_BIN_MODE", sizeof(int) );
  BnkDef( "HXD:PPR:TRN_SUM_MODE", sizeof(int) );
  BnkDef( "HXD:PPR:TRN_BIN", sizeof(int)*54 );
  BnkDef( "HXD:PPR:TRN_TBL", sizeof(int)*8 );
  BnkDef( "HXD:PPR:TRN_MEMTBL", sizeof(int)*4 );
  BnkDef( "HXD:PPR:TRN_NBIN_BD", sizeof(int)*4 );
  BnkDef( "HXD:PPR:PHSF_FUNIT_I", sizeof(int) );
  BnkDef( "HXD:PPR:PHSF_FUNIT_II", sizeof(int) );
  BnkDef( "HXD:PPR:HST_MODE", sizeof(int)*2 );
  BnkDef( "HXD:PPR:PHSF_PRESET", sizeof(int)*2 );
  BnkDef( "HXD:PPR:HSF_ASSIGN", sizeof(int)*2 );
  BnkDef( "HXD:PPR:HST_AQUIRE", sizeof(int) );
  BnkDef( "HXD:PPR:EVSEL_MODE", sizeof(int)*16 );
  BnkDef( "HXD:PPR:EVSEL_2D_FLD", sizeof(int)*16 );
  BnkDef( "HXD:PPR:EVSEL_2D_FMAX", sizeof(int)*32 );
  BnkDef( "HXD:PPR:EVSEL_2D_FMIN", sizeof(int)*32 );
  BnkDef( "HXD:PPR:EVSEL_2D_MAXH", sizeof(int)*32 );
  BnkDef( "HXD:PPR:EVSEL_2D_MAXL", sizeof(int)*32 );
  BnkDef( "HXD:PPR:EVSEL_2D_MINH", sizeof(int)*32 );
  BnkDef( "HXD:PPR:EVSEL_2D_MINL", sizeof(int)*32 );
  BnkDef( "HXD:PPR:EVSEL_DLT_TIME", sizeof(int)*16 );
  BnkDef( "HXD:PPR:EVSEL_FLG_MASK", sizeof(int)*16 );
  BnkDef( "HXD:PPR:EVSEL_TRG_MASK", sizeof(int)*16 );
  BnkDef( "HXD:PPR:EVSEL_HIT_MSK1", sizeof(int)*16 );
  BnkDef( "HXD:PPR:EVSEL_HIT_MSK2", sizeof(int)*16 );
  BnkDef( "HXD:PPR:EVSEL_PIN_LD", sizeof(int)*64 );
  BnkDef( "HXD:PPR:BST_INTERVAL", sizeof(int)*4 );
  BnkDef( "HXD:PPR:GB_TRG_MSK", sizeof(int)*4 );
  BnkDef( "HXD:PPR:GB_PH_FRM", sizeof(int)*8 );
  BnkDef( "HXD:PPR:GB_PH_TO", sizeof(int)*8 );
  BnkDef( "HXD:PPR:GB_WANT_SUM", sizeof(int)*4 );
  BnkDef( "HXD:PPR:GB_PH_FLG", sizeof(int)*4 );
  BnkDef( "HXD:PPR:GB_WLD_FLG", sizeof(int)*4 );
  BnkDef( "HXD:PPR:GB_PSE_FLG", sizeof(int)*4 );
  BnkDef( "HXD:PPR:GB_ITIME_PH", sizeof(int)*8 );
  BnkDef( "HXD:PPR:GB_LEBEL_PH", sizeof(int)*8 );
  BnkDef( "HXD:PPR:GB_ITIME_WLD", sizeof(int)*4 );
  BnkDef( "HXD:PPR:GB_LEBEL_WLD", sizeof(int)*4 );
  BnkDef( "HXD:PPR:GB_ITIME_PSE", sizeof(int)*4 );
  BnkDef( "HXD:PPR:GB_LEBEL_PSE", sizeof(int)*4 );
  BnkDef( "HXD:PPR:GB_TRG_MODE", sizeof(int) );
  BnkDef( "HXD:PPR:GB_PH_MODE", sizeof(int) );
  BnkDef( "HXD:PPR:GB_WLD_MODE", sizeof(int) );
  BnkDef( "HXD:PPR:GB_PSE_MODE", sizeof(int) );
  
}


int
HXDHKFitsReadPPR_bgnrun(fitsfile *fp)
{
  int istat = 0;

  int casesen = TRUE;
  int hdutype;
  
  fits_movabs_hdu( fp, PPR, &hdutype, &istat );
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
  
  if( fits_get_colnum(fp, casesen, "HXD_PPR_PI_VERSION",
                      &colnum[HXD_PPR_PI_VERSION], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_PI_VERSION') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_LOOPTIMES",
                      &colnum[HXD_PPR_LOOPTIMES], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_LOOPTIMES') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_AE_CMD_MODE",
                      &colnum[HXD_PPR_AE_CMD_MODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_AE_CMD_MODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_MONITOR_MODE",
                      &colnum[HXD_PPR_MONITOR_MODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_MONITOR_MODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_HK_MODE",
                      &colnum[HXD_PPR_HK_MODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_HK_MODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_WEL_FUNC",
                      &colnum[HXD_PPR_WEL_FUNC], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_WEL_FUNC') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_BST_READ_MASK",
                      &colnum[HXD_PPR_BST_READ_MASK], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_BST_READ_MASK') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_BST_JDG_MODE0",
                      &colnum[HXD_PPR_BST_JDG_MODE0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_BST_JDG_MODE0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_BST_JDG_MODE1",
                      &colnum[HXD_PPR_BST_JDG_MODE1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_BST_JDG_MODE1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_BST_JDG_MODE2",
                      &colnum[HXD_PPR_BST_JDG_MODE2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_BST_JDG_MODE2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_BST_JDG_MODE3",
                      &colnum[HXD_PPR_BST_JDG_MODE3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_BST_JDG_MODE3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_MODE",
                      &colnum[HXD_PPR_TRN_MODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_MODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN_MODE",
                      &colnum[HXD_PPR_TRN_BIN_MODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN_MODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_SUM_MODE",
                      &colnum[HXD_PPR_TRN_SUM_MODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_SUM_MODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN0",
                      &colnum[HXD_PPR_TRN_BIN0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN1",
                      &colnum[HXD_PPR_TRN_BIN1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN2",
                      &colnum[HXD_PPR_TRN_BIN2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN3",
                      &colnum[HXD_PPR_TRN_BIN3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN4",
                      &colnum[HXD_PPR_TRN_BIN4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN5",
                      &colnum[HXD_PPR_TRN_BIN5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN6",
                      &colnum[HXD_PPR_TRN_BIN6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN7",
                      &colnum[HXD_PPR_TRN_BIN7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN8",
                      &colnum[HXD_PPR_TRN_BIN8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN9",
                      &colnum[HXD_PPR_TRN_BIN9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN10",
                      &colnum[HXD_PPR_TRN_BIN10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN11",
                      &colnum[HXD_PPR_TRN_BIN11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN12",
                      &colnum[HXD_PPR_TRN_BIN12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN13",
                      &colnum[HXD_PPR_TRN_BIN13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN14",
                      &colnum[HXD_PPR_TRN_BIN14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN15",
                      &colnum[HXD_PPR_TRN_BIN15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN16",
                      &colnum[HXD_PPR_TRN_BIN16], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN16') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN17",
                      &colnum[HXD_PPR_TRN_BIN17], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN17') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN18",
                      &colnum[HXD_PPR_TRN_BIN18], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN18') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN19",
                      &colnum[HXD_PPR_TRN_BIN19], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN19') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN20",
                      &colnum[HXD_PPR_TRN_BIN20], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN20') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN21",
                      &colnum[HXD_PPR_TRN_BIN21], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN21') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN22",
                      &colnum[HXD_PPR_TRN_BIN22], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN22') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN23",
                      &colnum[HXD_PPR_TRN_BIN23], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN23') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN24",
                      &colnum[HXD_PPR_TRN_BIN24], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN24') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN25",
                      &colnum[HXD_PPR_TRN_BIN25], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN25') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN26",
                      &colnum[HXD_PPR_TRN_BIN26], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN26') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN27",
                      &colnum[HXD_PPR_TRN_BIN27], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN27') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN28",
                      &colnum[HXD_PPR_TRN_BIN28], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN28') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN29",
                      &colnum[HXD_PPR_TRN_BIN29], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN29') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN30",
                      &colnum[HXD_PPR_TRN_BIN30], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN30') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN31",
                      &colnum[HXD_PPR_TRN_BIN31], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN31') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN32",
                      &colnum[HXD_PPR_TRN_BIN32], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN32') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN33",
                      &colnum[HXD_PPR_TRN_BIN33], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN33') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN34",
                      &colnum[HXD_PPR_TRN_BIN34], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN34') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN35",
                      &colnum[HXD_PPR_TRN_BIN35], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN35') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN36",
                      &colnum[HXD_PPR_TRN_BIN36], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN36') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN37",
                      &colnum[HXD_PPR_TRN_BIN37], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN37') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN38",
                      &colnum[HXD_PPR_TRN_BIN38], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN38') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN39",
                      &colnum[HXD_PPR_TRN_BIN39], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN39') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN40",
                      &colnum[HXD_PPR_TRN_BIN40], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN40') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN41",
                      &colnum[HXD_PPR_TRN_BIN41], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN41') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN42",
                      &colnum[HXD_PPR_TRN_BIN42], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN42') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN43",
                      &colnum[HXD_PPR_TRN_BIN43], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN43') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN44",
                      &colnum[HXD_PPR_TRN_BIN44], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN44') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN45",
                      &colnum[HXD_PPR_TRN_BIN45], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN45') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN46",
                      &colnum[HXD_PPR_TRN_BIN46], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN46') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN47",
                      &colnum[HXD_PPR_TRN_BIN47], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN47') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN48",
                      &colnum[HXD_PPR_TRN_BIN48], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN48') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN49",
                      &colnum[HXD_PPR_TRN_BIN49], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN49') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN50",
                      &colnum[HXD_PPR_TRN_BIN50], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN50') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN51",
                      &colnum[HXD_PPR_TRN_BIN51], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN51') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN52",
                      &colnum[HXD_PPR_TRN_BIN52], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN52') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_BIN53",
                      &colnum[HXD_PPR_TRN_BIN53], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_BIN53') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_TBL0",
                      &colnum[HXD_PPR_TRN_TBL0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_TBL0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_TBL1",
                      &colnum[HXD_PPR_TRN_TBL1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_TBL1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_TBL2",
                      &colnum[HXD_PPR_TRN_TBL2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_TBL2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_TBL3",
                      &colnum[HXD_PPR_TRN_TBL3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_TBL3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_TBL4",
                      &colnum[HXD_PPR_TRN_TBL4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_TBL4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_TBL5",
                      &colnum[HXD_PPR_TRN_TBL5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_TBL5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_TBL6",
                      &colnum[HXD_PPR_TRN_TBL6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_TBL6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_TBL7",
                      &colnum[HXD_PPR_TRN_TBL7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_TBL7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_MEMTBL0",
                      &colnum[HXD_PPR_TRN_MEMTBL0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_MEMTBL0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_MEMTBL1",
                      &colnum[HXD_PPR_TRN_MEMTBL1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_MEMTBL1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_MEMTBL2",
                      &colnum[HXD_PPR_TRN_MEMTBL2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_MEMTBL2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_MEMTBL3",
                      &colnum[HXD_PPR_TRN_MEMTBL3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_MEMTBL3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_NBIN_MD0",
                      &colnum[HXD_PPR_TRN_NBIN_MD0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_NBIN_MD0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_NBIN_MD1",
                      &colnum[HXD_PPR_TRN_NBIN_MD1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_NBIN_MD1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_NBIN_MD2",
                      &colnum[HXD_PPR_TRN_NBIN_MD2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_NBIN_MD2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_TRN_NBIN_MD3",
                      &colnum[HXD_PPR_TRN_NBIN_MD3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_TRN_NBIN_MD3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_PHSF_FUNIT_I",
                      &colnum[HXD_PPR_PHSF_FUNIT_I], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_PHSF_FUNIT_I') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_PHSF_FUNIT_II",
                      &colnum[HXD_PPR_PHSF_FUNIT_II], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_PHSF_FUNIT_II') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_HST_MODE0",
                      &colnum[HXD_PPR_HST_MODE0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_HST_MODE0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_HST_MODE1",
                      &colnum[HXD_PPR_HST_MODE1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_HST_MODE1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_PHSF_PRESET0",
                      &colnum[HXD_PPR_PHSF_PRESET0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_PHSF_PRESET0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_PHSF_PRESET1",
                      &colnum[HXD_PPR_PHSF_PRESET1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_PHSF_PRESET1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_HSF_ASSIGN0",
                      &colnum[HXD_PPR_HSF_ASSIGN0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_HSF_ASSIGN0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_HSF_ASSIGN1",
                      &colnum[HXD_PPR_HSF_ASSIGN1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_HSF_ASSIGN1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_HST_ACQUIRE",
                      &colnum[HXD_PPR_HST_ACQUIRE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_HST_ACQUIRE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_MODE0",
                      &colnum[HXD_PPR_EVSEL_MODE0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_MODE0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_MODE1",
                      &colnum[HXD_PPR_EVSEL_MODE1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_MODE1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_MODE2",
                      &colnum[HXD_PPR_EVSEL_MODE2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_MODE2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_MODE3",
                      &colnum[HXD_PPR_EVSEL_MODE3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_MODE3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_MODE4",
                      &colnum[HXD_PPR_EVSEL_MODE4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_MODE4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_MODE5",
                      &colnum[HXD_PPR_EVSEL_MODE5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_MODE5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_MODE6",
                      &colnum[HXD_PPR_EVSEL_MODE6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_MODE6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_MODE7",
                      &colnum[HXD_PPR_EVSEL_MODE7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_MODE7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_MODE8",
                      &colnum[HXD_PPR_EVSEL_MODE8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_MODE8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_MODE9",
                      &colnum[HXD_PPR_EVSEL_MODE9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_MODE9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_MODE10",
                      &colnum[HXD_PPR_EVSEL_MODE10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_MODE10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_MODE11",
                      &colnum[HXD_PPR_EVSEL_MODE11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_MODE11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_MODE12",
                      &colnum[HXD_PPR_EVSEL_MODE12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_MODE12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_MODE13",
                      &colnum[HXD_PPR_EVSEL_MODE13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_MODE13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_MODE14",
                      &colnum[HXD_PPR_EVSEL_MODE14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_MODE14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_MODE15",
                      &colnum[HXD_PPR_EVSEL_MODE15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_MODE15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FLD0",
                      &colnum[HXD_PPR_EVSEL_2D_FLD0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FLD0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FLD1",
                      &colnum[HXD_PPR_EVSEL_2D_FLD1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FLD1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FLD2",
                      &colnum[HXD_PPR_EVSEL_2D_FLD2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FLD2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FLD3",
                      &colnum[HXD_PPR_EVSEL_2D_FLD3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FLD3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FLD4",
                      &colnum[HXD_PPR_EVSEL_2D_FLD4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FLD4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FLD5",
                      &colnum[HXD_PPR_EVSEL_2D_FLD5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FLD5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FLD6",
                      &colnum[HXD_PPR_EVSEL_2D_FLD6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FLD6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FLD7",
                      &colnum[HXD_PPR_EVSEL_2D_FLD7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FLD7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FLD8",
                      &colnum[HXD_PPR_EVSEL_2D_FLD8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FLD8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FLD9",
                      &colnum[HXD_PPR_EVSEL_2D_FLD9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FLD9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FLD10",
                      &colnum[HXD_PPR_EVSEL_2D_FLD10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FLD10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FLD11",
                      &colnum[HXD_PPR_EVSEL_2D_FLD11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FLD11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FLD12",
                      &colnum[HXD_PPR_EVSEL_2D_FLD12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FLD12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FLD13",
                      &colnum[HXD_PPR_EVSEL_2D_FLD13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FLD13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FLD14",
                      &colnum[HXD_PPR_EVSEL_2D_FLD14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FLD14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FLD15",
                      &colnum[HXD_PPR_EVSEL_2D_FLD15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FLD15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX0",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX1",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX2",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX3",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX4",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX5",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX6",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX7",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX8",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX9",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX10",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX11",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX12",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX13",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX14",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX15",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX16",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX16], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX16') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX17",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX17], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX17') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX18",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX18], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX18') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX19",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX19], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX19') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX20",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX20], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX20') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX21",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX21], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX21') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX22",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX22], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX22') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX23",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX23], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX23') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX24",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX24], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX24') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX25",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX25], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX25') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX26",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX26], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX26') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX27",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX27], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX27') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX28",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX28], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX28') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX29",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX29], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX29') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX30",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX30], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX30') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMAX31",
                      &colnum[HXD_PPR_EVSEL_2D_FMAX31], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMAX31') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN0",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN1",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN2",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN3",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN4",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN5",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN6",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN7",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN8",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN9",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN10",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN11",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN12",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN13",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN14",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN15",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN16",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN16], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN16') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN17",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN17], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN17') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN18",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN18], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN18') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN19",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN19], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN19') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN20",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN20], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN20') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN21",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN21], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN21') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN22",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN22], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN22') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN23",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN23], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN23') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN24",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN24], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN24') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN25",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN25], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN25') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN26",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN26], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN26') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN27",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN27], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN27') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN28",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN28], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN28') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN29",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN29], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN29') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN30",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN30], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN30') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_FMIN31",
                      &colnum[HXD_PPR_EVSEL_2D_FMIN31], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_FMIN31') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH0",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH1",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH2",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH3",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH4",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH5",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH6",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH7",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH8",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH9",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH10",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH11",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH12",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH13",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH14",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH15",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH16",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH16], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH16') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH17",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH17], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH17') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH18",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH18], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH18') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH19",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH19], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH19') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH20",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH20], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH20') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH21",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH21], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH21') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH22",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH22], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH22') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH23",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH23], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH23') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH24",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH24], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH24') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH25",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH25], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH25') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH26",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH26], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH26') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH27",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH27], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH27') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH28",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH28], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH28') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH29",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH29], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH29') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH30",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH30], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH30') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXH31",
                      &colnum[HXD_PPR_EVSEL_2D_MAXH31], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXH31') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL0",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL1",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL2",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL3",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL4",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL5",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL6",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL7",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL8",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL9",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL10",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL11",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL12",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL13",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL14",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL15",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL16",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL16], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL16') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL17",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL17], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL17') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL18",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL18], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL18') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL19",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL19], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL19') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL20",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL20], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL20') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL21",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL21], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL21') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL22",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL22], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL22') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL23",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL23], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL23') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL24",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL24], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL24') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL25",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL25], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL25') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL26",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL26], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL26') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL27",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL27], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL27') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL28",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL28], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL28') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL29",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL29], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL29') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL30",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL30], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL30') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MAXL31",
                      &colnum[HXD_PPR_EVSEL_2D_MAXL31], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MAXL31') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH0",
                      &colnum[HXD_PPR_EVSEL_2D_MINH0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH1",
                      &colnum[HXD_PPR_EVSEL_2D_MINH1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH2",
                      &colnum[HXD_PPR_EVSEL_2D_MINH2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH3",
                      &colnum[HXD_PPR_EVSEL_2D_MINH3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH4",
                      &colnum[HXD_PPR_EVSEL_2D_MINH4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH5",
                      &colnum[HXD_PPR_EVSEL_2D_MINH5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH6",
                      &colnum[HXD_PPR_EVSEL_2D_MINH6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH7",
                      &colnum[HXD_PPR_EVSEL_2D_MINH7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH8",
                      &colnum[HXD_PPR_EVSEL_2D_MINH8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH9",
                      &colnum[HXD_PPR_EVSEL_2D_MINH9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH10",
                      &colnum[HXD_PPR_EVSEL_2D_MINH10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH11",
                      &colnum[HXD_PPR_EVSEL_2D_MINH11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH12",
                      &colnum[HXD_PPR_EVSEL_2D_MINH12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH13",
                      &colnum[HXD_PPR_EVSEL_2D_MINH13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH14",
                      &colnum[HXD_PPR_EVSEL_2D_MINH14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH15",
                      &colnum[HXD_PPR_EVSEL_2D_MINH15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH16",
                      &colnum[HXD_PPR_EVSEL_2D_MINH16], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH16') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH17",
                      &colnum[HXD_PPR_EVSEL_2D_MINH17], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH17') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH18",
                      &colnum[HXD_PPR_EVSEL_2D_MINH18], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH18') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH19",
                      &colnum[HXD_PPR_EVSEL_2D_MINH19], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH19') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH20",
                      &colnum[HXD_PPR_EVSEL_2D_MINH20], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH20') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH21",
                      &colnum[HXD_PPR_EVSEL_2D_MINH21], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH21') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH22",
                      &colnum[HXD_PPR_EVSEL_2D_MINH22], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH22') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH23",
                      &colnum[HXD_PPR_EVSEL_2D_MINH23], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH23') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH24",
                      &colnum[HXD_PPR_EVSEL_2D_MINH24], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH24') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH25",
                      &colnum[HXD_PPR_EVSEL_2D_MINH25], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH25') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH26",
                      &colnum[HXD_PPR_EVSEL_2D_MINH26], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH26') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH27",
                      &colnum[HXD_PPR_EVSEL_2D_MINH27], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH27') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH28",
                      &colnum[HXD_PPR_EVSEL_2D_MINH28], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH28') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH29",
                      &colnum[HXD_PPR_EVSEL_2D_MINH29], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH29') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH30",
                      &colnum[HXD_PPR_EVSEL_2D_MINH30], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH30') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINH31",
                      &colnum[HXD_PPR_EVSEL_2D_MINH31], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINH31') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL0",
                      &colnum[HXD_PPR_EVSEL_2D_MINL0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL1",
                      &colnum[HXD_PPR_EVSEL_2D_MINL1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL2",
                      &colnum[HXD_PPR_EVSEL_2D_MINL2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL3",
                      &colnum[HXD_PPR_EVSEL_2D_MINL3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL4",
                      &colnum[HXD_PPR_EVSEL_2D_MINL4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL5",
                      &colnum[HXD_PPR_EVSEL_2D_MINL5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL6",
                      &colnum[HXD_PPR_EVSEL_2D_MINL6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL7",
                      &colnum[HXD_PPR_EVSEL_2D_MINL7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL8",
                      &colnum[HXD_PPR_EVSEL_2D_MINL8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL9",
                      &colnum[HXD_PPR_EVSEL_2D_MINL9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL10",
                      &colnum[HXD_PPR_EVSEL_2D_MINL10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL11",
                      &colnum[HXD_PPR_EVSEL_2D_MINL11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL12",
                      &colnum[HXD_PPR_EVSEL_2D_MINL12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL13",
                      &colnum[HXD_PPR_EVSEL_2D_MINL13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL14",
                      &colnum[HXD_PPR_EVSEL_2D_MINL14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL15",
                      &colnum[HXD_PPR_EVSEL_2D_MINL15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL16",
                      &colnum[HXD_PPR_EVSEL_2D_MINL16], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL16') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL17",
                      &colnum[HXD_PPR_EVSEL_2D_MINL17], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL17') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL18",
                      &colnum[HXD_PPR_EVSEL_2D_MINL18], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL18') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL19",
                      &colnum[HXD_PPR_EVSEL_2D_MINL19], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL19') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL20",
                      &colnum[HXD_PPR_EVSEL_2D_MINL20], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL20') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL21",
                      &colnum[HXD_PPR_EVSEL_2D_MINL21], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL21') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL22",
                      &colnum[HXD_PPR_EVSEL_2D_MINL22], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL22') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL23",
                      &colnum[HXD_PPR_EVSEL_2D_MINL23], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL23') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL24",
                      &colnum[HXD_PPR_EVSEL_2D_MINL24], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL24') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL25",
                      &colnum[HXD_PPR_EVSEL_2D_MINL25], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL25') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL26",
                      &colnum[HXD_PPR_EVSEL_2D_MINL26], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL26') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL27",
                      &colnum[HXD_PPR_EVSEL_2D_MINL27], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL27') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL28",
                      &colnum[HXD_PPR_EVSEL_2D_MINL28], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL28') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL29",
                      &colnum[HXD_PPR_EVSEL_2D_MINL29], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL29') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL30",
                      &colnum[HXD_PPR_EVSEL_2D_MINL30], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL30') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_2D_MINL31",
                      &colnum[HXD_PPR_EVSEL_2D_MINL31], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_2D_MINL31') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_DLT_TIME0",
                      &colnum[HXD_PPR_EVSEL_DLT_TIME0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_DLT_TIME0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_DLT_TIME1",
                      &colnum[HXD_PPR_EVSEL_DLT_TIME1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_DLT_TIME1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_DLT_TIME2",
                      &colnum[HXD_PPR_EVSEL_DLT_TIME2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_DLT_TIME2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_DLT_TIME3",
                      &colnum[HXD_PPR_EVSEL_DLT_TIME3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_DLT_TIME3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_DLT_TIME4",
                      &colnum[HXD_PPR_EVSEL_DLT_TIME4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_DLT_TIME4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_DLT_TIME5",
                      &colnum[HXD_PPR_EVSEL_DLT_TIME5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_DLT_TIME5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_DLT_TIME6",
                      &colnum[HXD_PPR_EVSEL_DLT_TIME6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_DLT_TIME6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_DLT_TIME7",
                      &colnum[HXD_PPR_EVSEL_DLT_TIME7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_DLT_TIME7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_DLT_TIME8",
                      &colnum[HXD_PPR_EVSEL_DLT_TIME8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_DLT_TIME8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_DLT_TIME9",
                      &colnum[HXD_PPR_EVSEL_DLT_TIME9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_DLT_TIME9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_DLT_TIME10",
                      &colnum[HXD_PPR_EVSEL_DLT_TIME10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_DLT_TIME10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_DLT_TIME11",
                      &colnum[HXD_PPR_EVSEL_DLT_TIME11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_DLT_TIME11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_DLT_TIME12",
                      &colnum[HXD_PPR_EVSEL_DLT_TIME12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_DLT_TIME12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_DLT_TIME13",
                      &colnum[HXD_PPR_EVSEL_DLT_TIME13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_DLT_TIME13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_DLT_TIME14",
                      &colnum[HXD_PPR_EVSEL_DLT_TIME14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_DLT_TIME14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_DLT_TIME15",
                      &colnum[HXD_PPR_EVSEL_DLT_TIME15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_DLT_TIME15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_FLG_MASK0",
                      &colnum[HXD_PPR_EVSEL_FLG_MASK0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_FLG_MASK0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_FLG_MASK1",
                      &colnum[HXD_PPR_EVSEL_FLG_MASK1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_FLG_MASK1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_FLG_MASK2",
                      &colnum[HXD_PPR_EVSEL_FLG_MASK2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_FLG_MASK2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_FLG_MASK3",
                      &colnum[HXD_PPR_EVSEL_FLG_MASK3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_FLG_MASK3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_FLG_MASK4",
                      &colnum[HXD_PPR_EVSEL_FLG_MASK4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_FLG_MASK4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_FLG_MASK5",
                      &colnum[HXD_PPR_EVSEL_FLG_MASK5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_FLG_MASK5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_FLG_MASK6",
                      &colnum[HXD_PPR_EVSEL_FLG_MASK6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_FLG_MASK6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_FLG_MASK7",
                      &colnum[HXD_PPR_EVSEL_FLG_MASK7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_FLG_MASK7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_FLG_MASK8",
                      &colnum[HXD_PPR_EVSEL_FLG_MASK8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_FLG_MASK8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_FLG_MASK9",
                      &colnum[HXD_PPR_EVSEL_FLG_MASK9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_FLG_MASK9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_FLG_MASK10",
                      &colnum[HXD_PPR_EVSEL_FLG_MASK10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_FLG_MASK10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_FLG_MASK11",
                      &colnum[HXD_PPR_EVSEL_FLG_MASK11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_FLG_MASK11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_FLG_MASK12",
                      &colnum[HXD_PPR_EVSEL_FLG_MASK12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_FLG_MASK12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_FLG_MASK13",
                      &colnum[HXD_PPR_EVSEL_FLG_MASK13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_FLG_MASK13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_FLG_MASK14",
                      &colnum[HXD_PPR_EVSEL_FLG_MASK14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_FLG_MASK14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_FLG_MASK15",
                      &colnum[HXD_PPR_EVSEL_FLG_MASK15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_FLG_MASK15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_TRG_MASK0",
                      &colnum[HXD_PPR_EVSEL_TRG_MASK0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_TRG_MASK0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_TRG_MASK1",
                      &colnum[HXD_PPR_EVSEL_TRG_MASK1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_TRG_MASK1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_TRG_MASK2",
                      &colnum[HXD_PPR_EVSEL_TRG_MASK2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_TRG_MASK2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_TRG_MASK3",
                      &colnum[HXD_PPR_EVSEL_TRG_MASK3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_TRG_MASK3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_TRG_MASK4",
                      &colnum[HXD_PPR_EVSEL_TRG_MASK4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_TRG_MASK4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_TRG_MASK5",
                      &colnum[HXD_PPR_EVSEL_TRG_MASK5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_TRG_MASK5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_TRG_MASK6",
                      &colnum[HXD_PPR_EVSEL_TRG_MASK6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_TRG_MASK6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_TRG_MASK7",
                      &colnum[HXD_PPR_EVSEL_TRG_MASK7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_TRG_MASK7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_TRG_MASK8",
                      &colnum[HXD_PPR_EVSEL_TRG_MASK8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_TRG_MASK8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_TRG_MASK9",
                      &colnum[HXD_PPR_EVSEL_TRG_MASK9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_TRG_MASK9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_TRG_MASK10",
                      &colnum[HXD_PPR_EVSEL_TRG_MASK10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_TRG_MASK10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_TRG_MASK11",
                      &colnum[HXD_PPR_EVSEL_TRG_MASK11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_TRG_MASK11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_TRG_MASK12",
                      &colnum[HXD_PPR_EVSEL_TRG_MASK12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_TRG_MASK12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_TRG_MASK13",
                      &colnum[HXD_PPR_EVSEL_TRG_MASK13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_TRG_MASK13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_TRG_MASK14",
                      &colnum[HXD_PPR_EVSEL_TRG_MASK14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_TRG_MASK14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_TRG_MASK15",
                      &colnum[HXD_PPR_EVSEL_TRG_MASK15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_TRG_MASK15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK10",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK11",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK12",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK13",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK14",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK15",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK16",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK16], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK16') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK17",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK17], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK17') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK18",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK18], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK18') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK19",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK19], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK19') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK110",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK110], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK110') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK111",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK111], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK111') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK112",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK112], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK112') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK113",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK113], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK113') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK114",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK114], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK114') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK115",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK115], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK115') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK20",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK20], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK20') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK21",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK21], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK21') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK22",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK22], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK22') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK23",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK23], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK23') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK24",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK24], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK24') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK25",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK25], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK25') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK26",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK26], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK26') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK27",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK27], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK27') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK28",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK28], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK28') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK29",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK29], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK29') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK210",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK210], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK210') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK211",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK211], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK211') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK212",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK212], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK212') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK213",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK213], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK213') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK214",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK214], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK214') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_HIT_MSK215",
                      &colnum[HXD_PPR_EVSEL_HIT_MSK215], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_HIT_MSK215') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD0",
                      &colnum[HXD_PPR_EVSEL_PIN_LD0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD1",
                      &colnum[HXD_PPR_EVSEL_PIN_LD1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD2",
                      &colnum[HXD_PPR_EVSEL_PIN_LD2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD3",
                      &colnum[HXD_PPR_EVSEL_PIN_LD3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD4",
                      &colnum[HXD_PPR_EVSEL_PIN_LD4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD5",
                      &colnum[HXD_PPR_EVSEL_PIN_LD5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD6",
                      &colnum[HXD_PPR_EVSEL_PIN_LD6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD7",
                      &colnum[HXD_PPR_EVSEL_PIN_LD7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD8",
                      &colnum[HXD_PPR_EVSEL_PIN_LD8], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD8') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD9",
                      &colnum[HXD_PPR_EVSEL_PIN_LD9], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD9') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD10",
                      &colnum[HXD_PPR_EVSEL_PIN_LD10], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD10') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD11",
                      &colnum[HXD_PPR_EVSEL_PIN_LD11], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD11') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD12",
                      &colnum[HXD_PPR_EVSEL_PIN_LD12], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD12') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD13",
                      &colnum[HXD_PPR_EVSEL_PIN_LD13], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD13') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD14",
                      &colnum[HXD_PPR_EVSEL_PIN_LD14], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD14') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD15",
                      &colnum[HXD_PPR_EVSEL_PIN_LD15], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD15') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD16",
                      &colnum[HXD_PPR_EVSEL_PIN_LD16], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD16') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD17",
                      &colnum[HXD_PPR_EVSEL_PIN_LD17], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD17') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD18",
                      &colnum[HXD_PPR_EVSEL_PIN_LD18], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD18') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD19",
                      &colnum[HXD_PPR_EVSEL_PIN_LD19], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD19') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD20",
                      &colnum[HXD_PPR_EVSEL_PIN_LD20], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD20') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD21",
                      &colnum[HXD_PPR_EVSEL_PIN_LD21], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD21') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD22",
                      &colnum[HXD_PPR_EVSEL_PIN_LD22], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD22') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD23",
                      &colnum[HXD_PPR_EVSEL_PIN_LD23], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD23') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD24",
                      &colnum[HXD_PPR_EVSEL_PIN_LD24], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD24') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD25",
                      &colnum[HXD_PPR_EVSEL_PIN_LD25], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD25') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD26",
                      &colnum[HXD_PPR_EVSEL_PIN_LD26], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD26') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD27",
                      &colnum[HXD_PPR_EVSEL_PIN_LD27], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD27') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD28",
                      &colnum[HXD_PPR_EVSEL_PIN_LD28], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD28') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD29",
                      &colnum[HXD_PPR_EVSEL_PIN_LD29], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD29') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD30",
                      &colnum[HXD_PPR_EVSEL_PIN_LD30], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD30') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD31",
                      &colnum[HXD_PPR_EVSEL_PIN_LD31], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD31') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD32",
                      &colnum[HXD_PPR_EVSEL_PIN_LD32], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD32') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD33",
                      &colnum[HXD_PPR_EVSEL_PIN_LD33], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD33') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD34",
                      &colnum[HXD_PPR_EVSEL_PIN_LD34], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD34') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD35",
                      &colnum[HXD_PPR_EVSEL_PIN_LD35], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD35') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD36",
                      &colnum[HXD_PPR_EVSEL_PIN_LD36], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD36') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD37",
                      &colnum[HXD_PPR_EVSEL_PIN_LD37], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD37') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD38",
                      &colnum[HXD_PPR_EVSEL_PIN_LD38], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD38') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD39",
                      &colnum[HXD_PPR_EVSEL_PIN_LD39], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD39') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD40",
                      &colnum[HXD_PPR_EVSEL_PIN_LD40], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD40') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD41",
                      &colnum[HXD_PPR_EVSEL_PIN_LD41], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD41') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD42",
                      &colnum[HXD_PPR_EVSEL_PIN_LD42], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD42') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD43",
                      &colnum[HXD_PPR_EVSEL_PIN_LD43], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD43') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD44",
                      &colnum[HXD_PPR_EVSEL_PIN_LD44], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD44') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD45",
                      &colnum[HXD_PPR_EVSEL_PIN_LD45], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD45') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD46",
                      &colnum[HXD_PPR_EVSEL_PIN_LD46], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD46') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD47",
                      &colnum[HXD_PPR_EVSEL_PIN_LD47], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD47') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD48",
                      &colnum[HXD_PPR_EVSEL_PIN_LD48], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD48') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD49",
                      &colnum[HXD_PPR_EVSEL_PIN_LD49], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD49') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD50",
                      &colnum[HXD_PPR_EVSEL_PIN_LD50], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD50') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD51",
                      &colnum[HXD_PPR_EVSEL_PIN_LD51], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD51') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD52",
                      &colnum[HXD_PPR_EVSEL_PIN_LD52], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD52') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD53",
                      &colnum[HXD_PPR_EVSEL_PIN_LD53], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD53') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD54",
                      &colnum[HXD_PPR_EVSEL_PIN_LD54], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD54') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD55",
                      &colnum[HXD_PPR_EVSEL_PIN_LD55], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD55') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD56",
                      &colnum[HXD_PPR_EVSEL_PIN_LD56], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD56') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD57",
                      &colnum[HXD_PPR_EVSEL_PIN_LD57], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD57') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD58",
                      &colnum[HXD_PPR_EVSEL_PIN_LD58], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD58') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD59",
                      &colnum[HXD_PPR_EVSEL_PIN_LD59], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD59') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD60",
                      &colnum[HXD_PPR_EVSEL_PIN_LD60], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD60') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD61",
                      &colnum[HXD_PPR_EVSEL_PIN_LD61], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD61') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD62",
                      &colnum[HXD_PPR_EVSEL_PIN_LD62], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD62') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_EVSEL_PIN_LD63",
                      &colnum[HXD_PPR_EVSEL_PIN_LD63], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_EVSEL_PIN_LD63') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_BST_INTERVAL0",
                      &colnum[HXD_PPR_BST_INTERVAL0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_BST_INTERVAL0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_BST_INTERVAL1",
                      &colnum[HXD_PPR_BST_INTERVAL1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_BST_INTERVAL1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_BST_INTERVAL2",
                      &colnum[HXD_PPR_BST_INTERVAL2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_BST_INTERVAL2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_BST_INTERVAL3",
                      &colnum[HXD_PPR_BST_INTERVAL3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_BST_INTERVAL3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_TRG_MSK0",
                      &colnum[HXD_PPR_GB_TRG_MSK0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_TRG_MSK0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_TRG_MSK1",
                      &colnum[HXD_PPR_GB_TRG_MSK1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_TRG_MSK1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_TRG_MSK2",
                      &colnum[HXD_PPR_GB_TRG_MSK2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_TRG_MSK2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_TRG_MSK3",
                      &colnum[HXD_PPR_GB_TRG_MSK3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_TRG_MSK3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_FRM0",
                      &colnum[HXD_PPR_GB_PH_FRM0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_FRM0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_FRM1",
                      &colnum[HXD_PPR_GB_PH_FRM1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_FRM1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_FRM2",
                      &colnum[HXD_PPR_GB_PH_FRM2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_FRM2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_FRM3",
                      &colnum[HXD_PPR_GB_PH_FRM3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_FRM3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_FRM4",
                      &colnum[HXD_PPR_GB_PH_FRM4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_FRM4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_FRM5",
                      &colnum[HXD_PPR_GB_PH_FRM5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_FRM5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_FRM6",
                      &colnum[HXD_PPR_GB_PH_FRM6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_FRM6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_FRM7",
                      &colnum[HXD_PPR_GB_PH_FRM7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_FRM7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_TO0",
                      &colnum[HXD_PPR_GB_PH_TO0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_TO0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_TO1",
                      &colnum[HXD_PPR_GB_PH_TO1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_TO1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_TO2",
                      &colnum[HXD_PPR_GB_PH_TO2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_TO2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_TO3",
                      &colnum[HXD_PPR_GB_PH_TO3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_TO3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_TO4",
                      &colnum[HXD_PPR_GB_PH_TO4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_TO4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_TO5",
                      &colnum[HXD_PPR_GB_PH_TO5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_TO5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_TO6",
                      &colnum[HXD_PPR_GB_PH_TO6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_TO6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_TO7",
                      &colnum[HXD_PPR_GB_PH_TO7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_TO7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_WANT_SUM0",
                      &colnum[HXD_PPR_GB_WANT_SUM0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_WANT_SUM0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_WANT_SUM1",
                      &colnum[HXD_PPR_GB_WANT_SUM1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_WANT_SUM1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_WANT_SUM2",
                      &colnum[HXD_PPR_GB_WANT_SUM2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_WANT_SUM2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_WANT_SUM3",
                      &colnum[HXD_PPR_GB_WANT_SUM3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_WANT_SUM3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_FLG0",
                      &colnum[HXD_PPR_GB_PH_FLG0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_FLG0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_FLG1",
                      &colnum[HXD_PPR_GB_PH_FLG1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_FLG1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_FLG2",
                      &colnum[HXD_PPR_GB_PH_FLG2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_FLG2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_FLG3",
                      &colnum[HXD_PPR_GB_PH_FLG3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_FLG3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_WLD_FLG0",
                      &colnum[HXD_PPR_GB_WLD_FLG0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_WLD_FLG0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_WLD_FLG1",
                      &colnum[HXD_PPR_GB_WLD_FLG1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_WLD_FLG1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_WLD_FLG2",
                      &colnum[HXD_PPR_GB_WLD_FLG2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_WLD_FLG2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_WLD_FLG3",
                      &colnum[HXD_PPR_GB_WLD_FLG3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_WLD_FLG3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PSE_FLG0",
                      &colnum[HXD_PPR_GB_PSE_FLG0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PSE_FLG0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PSE_FLG1",
                      &colnum[HXD_PPR_GB_PSE_FLG1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PSE_FLG1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PSE_FLG2",
                      &colnum[HXD_PPR_GB_PSE_FLG2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PSE_FLG2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PSE_FLG3",
                      &colnum[HXD_PPR_GB_PSE_FLG3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PSE_FLG3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_ITIME_PH0",
                      &colnum[HXD_PPR_GB_ITIME_PH0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_ITIME_PH0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_ITIME_PH1",
                      &colnum[HXD_PPR_GB_ITIME_PH1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_ITIME_PH1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_ITIME_PH2",
                      &colnum[HXD_PPR_GB_ITIME_PH2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_ITIME_PH2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_ITIME_PH3",
                      &colnum[HXD_PPR_GB_ITIME_PH3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_ITIME_PH3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_ITIME_PH4",
                      &colnum[HXD_PPR_GB_ITIME_PH4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_ITIME_PH4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_ITIME_PH5",
                      &colnum[HXD_PPR_GB_ITIME_PH5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_ITIME_PH5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_ITIME_PH6",
                      &colnum[HXD_PPR_GB_ITIME_PH6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_ITIME_PH6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_ITIME_PH7",
                      &colnum[HXD_PPR_GB_ITIME_PH7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_ITIME_PH7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_LEVEL_PH0",
                      &colnum[HXD_PPR_GB_LEVEL_PH0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_LEVEL_PH0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_LEVEL_PH1",
                      &colnum[HXD_PPR_GB_LEVEL_PH1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_LEVEL_PH1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_LEVEL_PH2",
                      &colnum[HXD_PPR_GB_LEVEL_PH2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_LEVEL_PH2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_LEVEL_PH3",
                      &colnum[HXD_PPR_GB_LEVEL_PH3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_LEVEL_PH3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_LEVEL_PH4",
                      &colnum[HXD_PPR_GB_LEVEL_PH4], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_LEVEL_PH4') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_LEVEL_PH5",
                      &colnum[HXD_PPR_GB_LEVEL_PH5], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_LEVEL_PH5') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_LEVEL_PH6",
                      &colnum[HXD_PPR_GB_LEVEL_PH6], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_LEVEL_PH6') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_LEVEL_PH7",
                      &colnum[HXD_PPR_GB_LEVEL_PH7], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_LEVEL_PH7') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_ITIME_WLD0",
                      &colnum[HXD_PPR_GB_ITIME_WLD0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_ITIME_WLD0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_ITIME_WLD1",
                      &colnum[HXD_PPR_GB_ITIME_WLD1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_ITIME_WLD1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_ITIME_WLD2",
                      &colnum[HXD_PPR_GB_ITIME_WLD2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_ITIME_WLD2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_ITIME_WLD3",
                      &colnum[HXD_PPR_GB_ITIME_WLD3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_ITIME_WLD3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_LEVEL_WLD0",
                      &colnum[HXD_PPR_GB_LEVEL_WLD0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_LEVEL_WLD0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_LEVEL_WLD1",
                      &colnum[HXD_PPR_GB_LEVEL_WLD1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_LEVEL_WLD1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_LEVEL_WLD2",
                      &colnum[HXD_PPR_GB_LEVEL_WLD2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_LEVEL_WLD2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_LEVEL_WLD3",
                      &colnum[HXD_PPR_GB_LEVEL_WLD3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_LEVEL_WLD3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_ITIME_PSE0",
                      &colnum[HXD_PPR_GB_ITIME_PSE0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_ITIME_PSE0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_ITIME_PSE1",
                      &colnum[HXD_PPR_GB_ITIME_PSE1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_ITIME_PSE1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_ITIME_PSE2",
                      &colnum[HXD_PPR_GB_ITIME_PSE2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_ITIME_PSE2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_ITIME_PSE3",
                      &colnum[HXD_PPR_GB_ITIME_PSE3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_ITIME_PSE3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_LEVEL_PSE0",
                      &colnum[HXD_PPR_GB_LEVEL_PSE0], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_LEVEL_PSE0') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_LEVEL_PSE1",
                      &colnum[HXD_PPR_GB_LEVEL_PSE1], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_LEVEL_PSE1') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_LEVEL_PSE2",
                      &colnum[HXD_PPR_GB_LEVEL_PSE2], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_LEVEL_PSE2') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_LEVEL_PSE3",
                      &colnum[HXD_PPR_GB_LEVEL_PSE3], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_LEVEL_PSE3') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_TRG_MODE",
                      &colnum[HXD_PPR_GB_TRG_MODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_TRG_MODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PH_MODE",
                      &colnum[HXD_PPR_GB_PH_MODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PH_MODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_WLD_MODE",
                      &colnum[HXD_PPR_GB_WLD_MODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_WLD_MODE') failed (%d)\n",
            pname, istat); return istat;}
  if( fits_get_colnum(fp, casesen, "HXD_PPR_GB_PSE_MODE",
                      &colnum[HXD_PPR_GB_PSE_MODE], &istat) ){
    fprintf(stderr, "%s: fits_get_colnum('HXD_PPR_GB_PSE_MODE') failed (%d)\n",
            pname, istat); return istat;}
  
  return ANL_OK;
}


int
HXDHKFitsReadPPR_ana(fitsfile *fp, int irow)
{
  
  int istat = 0;
  
  int anynul;
  int casesen = TRUE;
  int hdutype;

  long firstelem = 1;
  long nelements = 1;

  double time;
  
  fits_movabs_hdu( fp, PPR, &hdutype, &istat );
  if ( istat ) {
    fprintf(stderr, "%s: fits_movabs_hdu (%d) failed (%d)\n",
	    pname, PPR, istat);
    return istat;
  } else {
    double nulval=1.0;
    fits_read_col_dbl(fp, time_colnum, irow, firstelem, nelements,
		      nulval, &time, &anynul, &istat);
    BnkfPutM ("HXD:PPR:PACKET_AETIME", sizeof(double), &time);
    BnkfPutM ("HXD:ALL:PACKET_AETIME", sizeof(double), &time);
  }
  
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_ppr_pi_version;
    fits_read_col_byt(fp, colnum[HXD_PPR_PI_VERSION], irow, firstelem,
                      nelements, nulval, &hxd_ppr_pi_version, &anynul, &istat);
    data[0] = hxd_ppr_pi_version;
    BnkfPutM ("HXD:PPR:PI_VERSION", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_ppr_looptimes;
    fits_read_col_usht(fp, colnum[HXD_PPR_LOOPTIMES], irow, firstelem,
                      nelements, nulval, &hxd_ppr_looptimes, &anynul, &istat);
    data[0] = hxd_ppr_looptimes;
    BnkfPutM ("HXD:PPR:LOOPTIMES", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_ppr_ae_cmd_mode;
    fits_read_col_usht(fp, colnum[HXD_PPR_AE_CMD_MODE], irow, firstelem,
                      nelements, nulval, &hxd_ppr_ae_cmd_mode, &anynul, &istat);
    data[0] = hxd_ppr_ae_cmd_mode;
    BnkfPutM ("HXD:PPR:AE_CMD_MODE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_ppr_monitor_mode;
    fits_read_col_usht(fp, colnum[HXD_PPR_MONITOR_MODE], irow, firstelem,
                      nelements, nulval, &hxd_ppr_monitor_mode, &anynul, &istat);
    data[0] = hxd_ppr_monitor_mode;
    BnkfPutM ("HXD:PPR:MONITOR_MODE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_ppr_hk_mode;
    fits_read_col_usht(fp, colnum[HXD_PPR_HK_MODE], irow, firstelem,
                      nelements, nulval, &hxd_ppr_hk_mode, &anynul, &istat);
    data[0] = hxd_ppr_hk_mode;
    BnkfPutM ("HXD:PPR:HK_MODE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_ppr_wel_func;
    fits_read_col_usht(fp, colnum[HXD_PPR_WEL_FUNC], irow, firstelem,
                      nelements, nulval, &hxd_ppr_wel_func, &anynul, &istat);
    data[0] = hxd_ppr_wel_func;
    BnkfPutM ("HXD:PPR:WEL_FUNC", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_ppr_bst_read_mask;
    fits_read_col_usht(fp, colnum[HXD_PPR_BST_READ_MASK], irow, firstelem,
                      nelements, nulval, &hxd_ppr_bst_read_mask, &anynul, &istat);
    data[0] = hxd_ppr_bst_read_mask;
    BnkfPutM ("HXD:PPR:BST_READ_MASK", sizeof(int)*1, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_ppr_bst_jdg_mode0;
    unsigned short hxd_ppr_bst_jdg_mode1;
    unsigned short hxd_ppr_bst_jdg_mode2;
    unsigned short hxd_ppr_bst_jdg_mode3;
    fits_read_col_usht(fp, colnum[HXD_PPR_BST_JDG_MODE0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_bst_jdg_mode0, &anynul, &istat);
    data[0] = hxd_ppr_bst_jdg_mode0;
    fits_read_col_usht(fp, colnum[HXD_PPR_BST_JDG_MODE1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_bst_jdg_mode1, &anynul, &istat);
    data[1] = hxd_ppr_bst_jdg_mode1;
    fits_read_col_usht(fp, colnum[HXD_PPR_BST_JDG_MODE2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_bst_jdg_mode2, &anynul, &istat);
    data[2] = hxd_ppr_bst_jdg_mode2;
    fits_read_col_usht(fp, colnum[HXD_PPR_BST_JDG_MODE3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_bst_jdg_mode3, &anynul, &istat);
    data[3] = hxd_ppr_bst_jdg_mode3;
    BnkfPutM ("HXD:PPR:BST_JDG_MODE", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_ppr_trn_mode;
    fits_read_col_usht(fp, colnum[HXD_PPR_TRN_MODE], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_mode, &anynul, &istat);
    data[0] = hxd_ppr_trn_mode;
    BnkfPutM ("HXD:PPR:TRN_MODE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_ppr_trn_bin_mode;
    fits_read_col_usht(fp, colnum[HXD_PPR_TRN_BIN_MODE], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin_mode, &anynul, &istat);
    data[0] = hxd_ppr_trn_bin_mode;
    BnkfPutM ("HXD:PPR:TRN_BIN_MODE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_ppr_trn_sum_mode;
    fits_read_col_usht(fp, colnum[HXD_PPR_TRN_SUM_MODE], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_sum_mode, &anynul, &istat);
    data[0] = hxd_ppr_trn_sum_mode;
    BnkfPutM ("HXD:PPR:TRN_SUM_MODE", sizeof(int)*1, data);
  }
  {
    unsigned int data[54];
    unsigned char nulval=1;
    unsigned char hxd_ppr_trn_bin0;
    unsigned char hxd_ppr_trn_bin1;
    unsigned char hxd_ppr_trn_bin2;
    unsigned char hxd_ppr_trn_bin3;
    unsigned char hxd_ppr_trn_bin4;
    unsigned char hxd_ppr_trn_bin5;
    unsigned char hxd_ppr_trn_bin6;
    unsigned char hxd_ppr_trn_bin7;
    unsigned char hxd_ppr_trn_bin8;
    unsigned char hxd_ppr_trn_bin9;
    unsigned char hxd_ppr_trn_bin10;
    unsigned char hxd_ppr_trn_bin11;
    unsigned char hxd_ppr_trn_bin12;
    unsigned char hxd_ppr_trn_bin13;
    unsigned char hxd_ppr_trn_bin14;
    unsigned char hxd_ppr_trn_bin15;
    unsigned char hxd_ppr_trn_bin16;
    unsigned char hxd_ppr_trn_bin17;
    unsigned char hxd_ppr_trn_bin18;
    unsigned char hxd_ppr_trn_bin19;
    unsigned char hxd_ppr_trn_bin20;
    unsigned char hxd_ppr_trn_bin21;
    unsigned char hxd_ppr_trn_bin22;
    unsigned char hxd_ppr_trn_bin23;
    unsigned char hxd_ppr_trn_bin24;
    unsigned char hxd_ppr_trn_bin25;
    unsigned char hxd_ppr_trn_bin26;
    unsigned char hxd_ppr_trn_bin27;
    unsigned char hxd_ppr_trn_bin28;
    unsigned char hxd_ppr_trn_bin29;
    unsigned char hxd_ppr_trn_bin30;
    unsigned char hxd_ppr_trn_bin31;
    unsigned char hxd_ppr_trn_bin32;
    unsigned char hxd_ppr_trn_bin33;
    unsigned char hxd_ppr_trn_bin34;
    unsigned char hxd_ppr_trn_bin35;
    unsigned char hxd_ppr_trn_bin36;
    unsigned char hxd_ppr_trn_bin37;
    unsigned char hxd_ppr_trn_bin38;
    unsigned char hxd_ppr_trn_bin39;
    unsigned char hxd_ppr_trn_bin40;
    unsigned char hxd_ppr_trn_bin41;
    unsigned char hxd_ppr_trn_bin42;
    unsigned char hxd_ppr_trn_bin43;
    unsigned char hxd_ppr_trn_bin44;
    unsigned char hxd_ppr_trn_bin45;
    unsigned char hxd_ppr_trn_bin46;
    unsigned char hxd_ppr_trn_bin47;
    unsigned char hxd_ppr_trn_bin48;
    unsigned char hxd_ppr_trn_bin49;
    unsigned char hxd_ppr_trn_bin50;
    unsigned char hxd_ppr_trn_bin51;
    unsigned char hxd_ppr_trn_bin52;
    unsigned char hxd_ppr_trn_bin53;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin0, &anynul, &istat);
    data[0] = hxd_ppr_trn_bin0;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin1, &anynul, &istat);
    data[1] = hxd_ppr_trn_bin1;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin2, &anynul, &istat);
    data[2] = hxd_ppr_trn_bin2;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin3, &anynul, &istat);
    data[3] = hxd_ppr_trn_bin3;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin4, &anynul, &istat);
    data[4] = hxd_ppr_trn_bin4;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin5, &anynul, &istat);
    data[5] = hxd_ppr_trn_bin5;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin6, &anynul, &istat);
    data[6] = hxd_ppr_trn_bin6;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin7, &anynul, &istat);
    data[7] = hxd_ppr_trn_bin7;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN8], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin8, &anynul, &istat);
    data[8] = hxd_ppr_trn_bin8;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN9], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin9, &anynul, &istat);
    data[9] = hxd_ppr_trn_bin9;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN10], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin10, &anynul, &istat);
    data[10] = hxd_ppr_trn_bin10;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN11], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin11, &anynul, &istat);
    data[11] = hxd_ppr_trn_bin11;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN12], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin12, &anynul, &istat);
    data[12] = hxd_ppr_trn_bin12;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN13], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin13, &anynul, &istat);
    data[13] = hxd_ppr_trn_bin13;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN14], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin14, &anynul, &istat);
    data[14] = hxd_ppr_trn_bin14;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN15], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin15, &anynul, &istat);
    data[15] = hxd_ppr_trn_bin15;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN16], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin16, &anynul, &istat);
    data[16] = hxd_ppr_trn_bin16;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN17], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin17, &anynul, &istat);
    data[17] = hxd_ppr_trn_bin17;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN18], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin18, &anynul, &istat);
    data[18] = hxd_ppr_trn_bin18;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN19], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin19, &anynul, &istat);
    data[19] = hxd_ppr_trn_bin19;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN20], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin20, &anynul, &istat);
    data[20] = hxd_ppr_trn_bin20;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN21], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin21, &anynul, &istat);
    data[21] = hxd_ppr_trn_bin21;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN22], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin22, &anynul, &istat);
    data[22] = hxd_ppr_trn_bin22;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN23], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin23, &anynul, &istat);
    data[23] = hxd_ppr_trn_bin23;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN24], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin24, &anynul, &istat);
    data[24] = hxd_ppr_trn_bin24;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN25], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin25, &anynul, &istat);
    data[25] = hxd_ppr_trn_bin25;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN26], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin26, &anynul, &istat);
    data[26] = hxd_ppr_trn_bin26;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN27], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin27, &anynul, &istat);
    data[27] = hxd_ppr_trn_bin27;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN28], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin28, &anynul, &istat);
    data[28] = hxd_ppr_trn_bin28;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN29], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin29, &anynul, &istat);
    data[29] = hxd_ppr_trn_bin29;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN30], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin30, &anynul, &istat);
    data[30] = hxd_ppr_trn_bin30;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN31], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin31, &anynul, &istat);
    data[31] = hxd_ppr_trn_bin31;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN32], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin32, &anynul, &istat);
    data[32] = hxd_ppr_trn_bin32;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN33], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin33, &anynul, &istat);
    data[33] = hxd_ppr_trn_bin33;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN34], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin34, &anynul, &istat);
    data[34] = hxd_ppr_trn_bin34;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN35], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin35, &anynul, &istat);
    data[35] = hxd_ppr_trn_bin35;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN36], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin36, &anynul, &istat);
    data[36] = hxd_ppr_trn_bin36;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN37], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin37, &anynul, &istat);
    data[37] = hxd_ppr_trn_bin37;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN38], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin38, &anynul, &istat);
    data[38] = hxd_ppr_trn_bin38;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN39], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin39, &anynul, &istat);
    data[39] = hxd_ppr_trn_bin39;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN40], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin40, &anynul, &istat);
    data[40] = hxd_ppr_trn_bin40;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN41], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin41, &anynul, &istat);
    data[41] = hxd_ppr_trn_bin41;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN42], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin42, &anynul, &istat);
    data[42] = hxd_ppr_trn_bin42;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN43], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin43, &anynul, &istat);
    data[43] = hxd_ppr_trn_bin43;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN44], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin44, &anynul, &istat);
    data[44] = hxd_ppr_trn_bin44;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN45], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin45, &anynul, &istat);
    data[45] = hxd_ppr_trn_bin45;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN46], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin46, &anynul, &istat);
    data[46] = hxd_ppr_trn_bin46;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN47], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin47, &anynul, &istat);
    data[47] = hxd_ppr_trn_bin47;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN48], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin48, &anynul, &istat);
    data[48] = hxd_ppr_trn_bin48;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN49], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin49, &anynul, &istat);
    data[49] = hxd_ppr_trn_bin49;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN50], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin50, &anynul, &istat);
    data[50] = hxd_ppr_trn_bin50;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN51], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin51, &anynul, &istat);
    data[51] = hxd_ppr_trn_bin51;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN52], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin52, &anynul, &istat);
    data[52] = hxd_ppr_trn_bin52;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_BIN53], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_bin53, &anynul, &istat);
    data[53] = hxd_ppr_trn_bin53;
    BnkfPutM ("HXD:PPR:TRN_BIN", sizeof(int)*54, data);
  }
  {
    unsigned int data[8];
    unsigned char nulval=1;
    unsigned char hxd_ppr_trn_tbl0;
    unsigned char hxd_ppr_trn_tbl1;
    unsigned char hxd_ppr_trn_tbl2;
    unsigned char hxd_ppr_trn_tbl3;
    unsigned char hxd_ppr_trn_tbl4;
    unsigned char hxd_ppr_trn_tbl5;
    unsigned char hxd_ppr_trn_tbl6;
    unsigned char hxd_ppr_trn_tbl7;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_TBL0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_tbl0, &anynul, &istat);
    data[0] = hxd_ppr_trn_tbl0;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_TBL1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_tbl1, &anynul, &istat);
    data[1] = hxd_ppr_trn_tbl1;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_TBL2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_tbl2, &anynul, &istat);
    data[2] = hxd_ppr_trn_tbl2;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_TBL3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_tbl3, &anynul, &istat);
    data[3] = hxd_ppr_trn_tbl3;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_TBL4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_tbl4, &anynul, &istat);
    data[4] = hxd_ppr_trn_tbl4;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_TBL5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_tbl5, &anynul, &istat);
    data[5] = hxd_ppr_trn_tbl5;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_TBL6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_tbl6, &anynul, &istat);
    data[6] = hxd_ppr_trn_tbl6;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_TBL7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_tbl7, &anynul, &istat);
    data[7] = hxd_ppr_trn_tbl7;
    BnkfPutM ("HXD:PPR:TRN_TBL", sizeof(int)*8, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_ppr_trn_memtbl0;
    unsigned char hxd_ppr_trn_memtbl1;
    unsigned char hxd_ppr_trn_memtbl2;
    unsigned char hxd_ppr_trn_memtbl3;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_MEMTBL0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_memtbl0, &anynul, &istat);
    data[0] = hxd_ppr_trn_memtbl0;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_MEMTBL1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_memtbl1, &anynul, &istat);
    data[1] = hxd_ppr_trn_memtbl1;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_MEMTBL2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_memtbl2, &anynul, &istat);
    data[2] = hxd_ppr_trn_memtbl2;
    fits_read_col_byt(fp, colnum[HXD_PPR_TRN_MEMTBL3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_memtbl3, &anynul, &istat);
    data[3] = hxd_ppr_trn_memtbl3;
    BnkfPutM ("HXD:PPR:TRN_MEMTBL", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned short nulval=1;
    unsigned short hxd_ppr_trn_nbin_md0;
    unsigned short hxd_ppr_trn_nbin_md1;
    unsigned short hxd_ppr_trn_nbin_md2;
    unsigned short hxd_ppr_trn_nbin_md3;
    fits_read_col_usht(fp, colnum[HXD_PPR_TRN_NBIN_MD0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_nbin_md0, &anynul, &istat);
    data[0] = hxd_ppr_trn_nbin_md0;
    fits_read_col_usht(fp, colnum[HXD_PPR_TRN_NBIN_MD1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_nbin_md1, &anynul, &istat);
    data[1] = hxd_ppr_trn_nbin_md1;
    fits_read_col_usht(fp, colnum[HXD_PPR_TRN_NBIN_MD2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_nbin_md2, &anynul, &istat);
    data[2] = hxd_ppr_trn_nbin_md2;
    fits_read_col_usht(fp, colnum[HXD_PPR_TRN_NBIN_MD3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_trn_nbin_md3, &anynul, &istat);
    data[3] = hxd_ppr_trn_nbin_md3;
    BnkfPutM ("HXD:PPR:TRN_NBIN_BD", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_ppr_phsf_funit_i;
    fits_read_col_usht(fp, colnum[HXD_PPR_PHSF_FUNIT_I], irow, firstelem,
                      nelements, nulval, &hxd_ppr_phsf_funit_i, &anynul, &istat);
    data[0] = hxd_ppr_phsf_funit_i;
    BnkfPutM ("HXD:PPR:PHSF_FUNIT_I", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned short nulval=1;
    unsigned short hxd_ppr_phsf_funit_ii;
    fits_read_col_usht(fp, colnum[HXD_PPR_PHSF_FUNIT_II], irow, firstelem,
                      nelements, nulval, &hxd_ppr_phsf_funit_ii, &anynul, &istat);
    data[0] = hxd_ppr_phsf_funit_ii;
    BnkfPutM ("HXD:PPR:PHSF_FUNIT_II", sizeof(int)*1, data);
  }
  {
    unsigned int data[2];
    unsigned short nulval=1;
    unsigned short hxd_ppr_hst_mode0;
    unsigned short hxd_ppr_hst_mode1;
    fits_read_col_usht(fp, colnum[HXD_PPR_HST_MODE0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_hst_mode0, &anynul, &istat);
    data[0] = hxd_ppr_hst_mode0;
    fits_read_col_usht(fp, colnum[HXD_PPR_HST_MODE1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_hst_mode1, &anynul, &istat);
    data[1] = hxd_ppr_hst_mode1;
    BnkfPutM ("HXD:PPR:HST_MODE", sizeof(int)*2, data);
  }
  {
    unsigned int data[2];
    unsigned short nulval=1;
    unsigned short hxd_ppr_phsf_preset0;
    unsigned short hxd_ppr_phsf_preset1;
    fits_read_col_usht(fp, colnum[HXD_PPR_PHSF_PRESET0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_phsf_preset0, &anynul, &istat);
    data[0] = hxd_ppr_phsf_preset0;
    fits_read_col_usht(fp, colnum[HXD_PPR_PHSF_PRESET1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_phsf_preset1, &anynul, &istat);
    data[1] = hxd_ppr_phsf_preset1;
    BnkfPutM ("HXD:PPR:PHSF_PRESET", sizeof(int)*2, data);
  }
  {
    unsigned int data[2];
    unsigned short nulval=1;
    unsigned short hxd_ppr_hsf_assign0;
    unsigned short hxd_ppr_hsf_assign1;
    fits_read_col_usht(fp, colnum[HXD_PPR_HSF_ASSIGN0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_hsf_assign0, &anynul, &istat);
    data[0] = hxd_ppr_hsf_assign0;
    fits_read_col_usht(fp, colnum[HXD_PPR_HSF_ASSIGN1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_hsf_assign1, &anynul, &istat);
    data[1] = hxd_ppr_hsf_assign1;
    BnkfPutM ("HXD:PPR:HSF_ASSIGN", sizeof(int)*2, data);
  }
  {
    unsigned int data[1];
    unsigned int nulval=1;
    unsigned int hxd_ppr_hst_acquire;
    fits_read_col_uint(fp, colnum[HXD_PPR_HST_ACQUIRE], irow, firstelem,
                      nelements, nulval, &hxd_ppr_hst_acquire, &anynul, &istat);
    data[0] = hxd_ppr_hst_acquire;
    BnkfPutM ("HXD:PPR:HST_AQUIRE", sizeof(int)*1, data);
  }
  {
    unsigned int data[16];
    unsigned short nulval=1;
    unsigned short hxd_ppr_evsel_mode0;
    unsigned short hxd_ppr_evsel_mode1;
    unsigned short hxd_ppr_evsel_mode2;
    unsigned short hxd_ppr_evsel_mode3;
    unsigned short hxd_ppr_evsel_mode4;
    unsigned short hxd_ppr_evsel_mode5;
    unsigned short hxd_ppr_evsel_mode6;
    unsigned short hxd_ppr_evsel_mode7;
    unsigned short hxd_ppr_evsel_mode8;
    unsigned short hxd_ppr_evsel_mode9;
    unsigned short hxd_ppr_evsel_mode10;
    unsigned short hxd_ppr_evsel_mode11;
    unsigned short hxd_ppr_evsel_mode12;
    unsigned short hxd_ppr_evsel_mode13;
    unsigned short hxd_ppr_evsel_mode14;
    unsigned short hxd_ppr_evsel_mode15;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_MODE0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_mode0, &anynul, &istat);
    data[0] = hxd_ppr_evsel_mode0;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_MODE1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_mode1, &anynul, &istat);
    data[1] = hxd_ppr_evsel_mode1;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_MODE2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_mode2, &anynul, &istat);
    data[2] = hxd_ppr_evsel_mode2;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_MODE3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_mode3, &anynul, &istat);
    data[3] = hxd_ppr_evsel_mode3;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_MODE4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_mode4, &anynul, &istat);
    data[4] = hxd_ppr_evsel_mode4;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_MODE5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_mode5, &anynul, &istat);
    data[5] = hxd_ppr_evsel_mode5;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_MODE6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_mode6, &anynul, &istat);
    data[6] = hxd_ppr_evsel_mode6;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_MODE7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_mode7, &anynul, &istat);
    data[7] = hxd_ppr_evsel_mode7;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_MODE8], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_mode8, &anynul, &istat);
    data[8] = hxd_ppr_evsel_mode8;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_MODE9], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_mode9, &anynul, &istat);
    data[9] = hxd_ppr_evsel_mode9;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_MODE10], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_mode10, &anynul, &istat);
    data[10] = hxd_ppr_evsel_mode10;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_MODE11], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_mode11, &anynul, &istat);
    data[11] = hxd_ppr_evsel_mode11;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_MODE12], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_mode12, &anynul, &istat);
    data[12] = hxd_ppr_evsel_mode12;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_MODE13], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_mode13, &anynul, &istat);
    data[13] = hxd_ppr_evsel_mode13;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_MODE14], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_mode14, &anynul, &istat);
    data[14] = hxd_ppr_evsel_mode14;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_MODE15], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_mode15, &anynul, &istat);
    data[15] = hxd_ppr_evsel_mode15;
    BnkfPutM ("HXD:PPR:EVSEL_MODE", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned short nulval=1;
    unsigned short hxd_ppr_evsel_2d_fld0;
    unsigned short hxd_ppr_evsel_2d_fld1;
    unsigned short hxd_ppr_evsel_2d_fld2;
    unsigned short hxd_ppr_evsel_2d_fld3;
    unsigned short hxd_ppr_evsel_2d_fld4;
    unsigned short hxd_ppr_evsel_2d_fld5;
    unsigned short hxd_ppr_evsel_2d_fld6;
    unsigned short hxd_ppr_evsel_2d_fld7;
    unsigned short hxd_ppr_evsel_2d_fld8;
    unsigned short hxd_ppr_evsel_2d_fld9;
    unsigned short hxd_ppr_evsel_2d_fld10;
    unsigned short hxd_ppr_evsel_2d_fld11;
    unsigned short hxd_ppr_evsel_2d_fld12;
    unsigned short hxd_ppr_evsel_2d_fld13;
    unsigned short hxd_ppr_evsel_2d_fld14;
    unsigned short hxd_ppr_evsel_2d_fld15;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FLD0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fld0, &anynul, &istat);
    data[0] = hxd_ppr_evsel_2d_fld0;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FLD1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fld1, &anynul, &istat);
    data[1] = hxd_ppr_evsel_2d_fld1;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FLD2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fld2, &anynul, &istat);
    data[2] = hxd_ppr_evsel_2d_fld2;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FLD3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fld3, &anynul, &istat);
    data[3] = hxd_ppr_evsel_2d_fld3;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FLD4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fld4, &anynul, &istat);
    data[4] = hxd_ppr_evsel_2d_fld4;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FLD5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fld5, &anynul, &istat);
    data[5] = hxd_ppr_evsel_2d_fld5;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FLD6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fld6, &anynul, &istat);
    data[6] = hxd_ppr_evsel_2d_fld6;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FLD7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fld7, &anynul, &istat);
    data[7] = hxd_ppr_evsel_2d_fld7;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FLD8], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fld8, &anynul, &istat);
    data[8] = hxd_ppr_evsel_2d_fld8;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FLD9], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fld9, &anynul, &istat);
    data[9] = hxd_ppr_evsel_2d_fld9;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FLD10], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fld10, &anynul, &istat);
    data[10] = hxd_ppr_evsel_2d_fld10;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FLD11], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fld11, &anynul, &istat);
    data[11] = hxd_ppr_evsel_2d_fld11;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FLD12], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fld12, &anynul, &istat);
    data[12] = hxd_ppr_evsel_2d_fld12;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FLD13], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fld13, &anynul, &istat);
    data[13] = hxd_ppr_evsel_2d_fld13;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FLD14], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fld14, &anynul, &istat);
    data[14] = hxd_ppr_evsel_2d_fld14;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FLD15], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fld15, &anynul, &istat);
    data[15] = hxd_ppr_evsel_2d_fld15;
    BnkfPutM ("HXD:PPR:EVSEL_2D_FLD", sizeof(int)*16, data);
  }
  {
    unsigned int data[32];
    unsigned short nulval=1;
    unsigned short hxd_ppr_evsel_2d_fmax0;
    unsigned short hxd_ppr_evsel_2d_fmax1;
    unsigned short hxd_ppr_evsel_2d_fmax2;
    unsigned short hxd_ppr_evsel_2d_fmax3;
    unsigned short hxd_ppr_evsel_2d_fmax4;
    unsigned short hxd_ppr_evsel_2d_fmax5;
    unsigned short hxd_ppr_evsel_2d_fmax6;
    unsigned short hxd_ppr_evsel_2d_fmax7;
    unsigned short hxd_ppr_evsel_2d_fmax8;
    unsigned short hxd_ppr_evsel_2d_fmax9;
    unsigned short hxd_ppr_evsel_2d_fmax10;
    unsigned short hxd_ppr_evsel_2d_fmax11;
    unsigned short hxd_ppr_evsel_2d_fmax12;
    unsigned short hxd_ppr_evsel_2d_fmax13;
    unsigned short hxd_ppr_evsel_2d_fmax14;
    unsigned short hxd_ppr_evsel_2d_fmax15;
    unsigned short hxd_ppr_evsel_2d_fmax16;
    unsigned short hxd_ppr_evsel_2d_fmax17;
    unsigned short hxd_ppr_evsel_2d_fmax18;
    unsigned short hxd_ppr_evsel_2d_fmax19;
    unsigned short hxd_ppr_evsel_2d_fmax20;
    unsigned short hxd_ppr_evsel_2d_fmax21;
    unsigned short hxd_ppr_evsel_2d_fmax22;
    unsigned short hxd_ppr_evsel_2d_fmax23;
    unsigned short hxd_ppr_evsel_2d_fmax24;
    unsigned short hxd_ppr_evsel_2d_fmax25;
    unsigned short hxd_ppr_evsel_2d_fmax26;
    unsigned short hxd_ppr_evsel_2d_fmax27;
    unsigned short hxd_ppr_evsel_2d_fmax28;
    unsigned short hxd_ppr_evsel_2d_fmax29;
    unsigned short hxd_ppr_evsel_2d_fmax30;
    unsigned short hxd_ppr_evsel_2d_fmax31;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax0, &anynul, &istat);
    data[0] = hxd_ppr_evsel_2d_fmax0;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax1, &anynul, &istat);
    data[1] = hxd_ppr_evsel_2d_fmax1;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax2, &anynul, &istat);
    data[2] = hxd_ppr_evsel_2d_fmax2;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax3, &anynul, &istat);
    data[3] = hxd_ppr_evsel_2d_fmax3;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax4, &anynul, &istat);
    data[4] = hxd_ppr_evsel_2d_fmax4;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax5, &anynul, &istat);
    data[5] = hxd_ppr_evsel_2d_fmax5;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax6, &anynul, &istat);
    data[6] = hxd_ppr_evsel_2d_fmax6;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax7, &anynul, &istat);
    data[7] = hxd_ppr_evsel_2d_fmax7;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX8], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax8, &anynul, &istat);
    data[8] = hxd_ppr_evsel_2d_fmax8;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX9], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax9, &anynul, &istat);
    data[9] = hxd_ppr_evsel_2d_fmax9;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX10], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax10, &anynul, &istat);
    data[10] = hxd_ppr_evsel_2d_fmax10;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX11], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax11, &anynul, &istat);
    data[11] = hxd_ppr_evsel_2d_fmax11;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX12], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax12, &anynul, &istat);
    data[12] = hxd_ppr_evsel_2d_fmax12;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX13], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax13, &anynul, &istat);
    data[13] = hxd_ppr_evsel_2d_fmax13;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX14], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax14, &anynul, &istat);
    data[14] = hxd_ppr_evsel_2d_fmax14;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX15], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax15, &anynul, &istat);
    data[15] = hxd_ppr_evsel_2d_fmax15;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX16], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax16, &anynul, &istat);
    data[16] = hxd_ppr_evsel_2d_fmax16;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX17], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax17, &anynul, &istat);
    data[17] = hxd_ppr_evsel_2d_fmax17;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX18], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax18, &anynul, &istat);
    data[18] = hxd_ppr_evsel_2d_fmax18;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX19], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax19, &anynul, &istat);
    data[19] = hxd_ppr_evsel_2d_fmax19;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX20], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax20, &anynul, &istat);
    data[20] = hxd_ppr_evsel_2d_fmax20;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX21], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax21, &anynul, &istat);
    data[21] = hxd_ppr_evsel_2d_fmax21;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX22], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax22, &anynul, &istat);
    data[22] = hxd_ppr_evsel_2d_fmax22;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX23], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax23, &anynul, &istat);
    data[23] = hxd_ppr_evsel_2d_fmax23;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX24], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax24, &anynul, &istat);
    data[24] = hxd_ppr_evsel_2d_fmax24;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX25], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax25, &anynul, &istat);
    data[25] = hxd_ppr_evsel_2d_fmax25;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX26], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax26, &anynul, &istat);
    data[26] = hxd_ppr_evsel_2d_fmax26;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX27], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax27, &anynul, &istat);
    data[27] = hxd_ppr_evsel_2d_fmax27;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX28], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax28, &anynul, &istat);
    data[28] = hxd_ppr_evsel_2d_fmax28;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX29], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax29, &anynul, &istat);
    data[29] = hxd_ppr_evsel_2d_fmax29;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX30], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax30, &anynul, &istat);
    data[30] = hxd_ppr_evsel_2d_fmax30;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMAX31], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmax31, &anynul, &istat);
    data[31] = hxd_ppr_evsel_2d_fmax31;
    BnkfPutM ("HXD:PPR:EVSEL_2D_FMAX", sizeof(int)*32, data);
  }
  {
    unsigned int data[32];
    unsigned short nulval=1;
    unsigned short hxd_ppr_evsel_2d_fmin0;
    unsigned short hxd_ppr_evsel_2d_fmin1;
    unsigned short hxd_ppr_evsel_2d_fmin2;
    unsigned short hxd_ppr_evsel_2d_fmin3;
    unsigned short hxd_ppr_evsel_2d_fmin4;
    unsigned short hxd_ppr_evsel_2d_fmin5;
    unsigned short hxd_ppr_evsel_2d_fmin6;
    unsigned short hxd_ppr_evsel_2d_fmin7;
    unsigned short hxd_ppr_evsel_2d_fmin8;
    unsigned short hxd_ppr_evsel_2d_fmin9;
    unsigned short hxd_ppr_evsel_2d_fmin10;
    unsigned short hxd_ppr_evsel_2d_fmin11;
    unsigned short hxd_ppr_evsel_2d_fmin12;
    unsigned short hxd_ppr_evsel_2d_fmin13;
    unsigned short hxd_ppr_evsel_2d_fmin14;
    unsigned short hxd_ppr_evsel_2d_fmin15;
    unsigned short hxd_ppr_evsel_2d_fmin16;
    unsigned short hxd_ppr_evsel_2d_fmin17;
    unsigned short hxd_ppr_evsel_2d_fmin18;
    unsigned short hxd_ppr_evsel_2d_fmin19;
    unsigned short hxd_ppr_evsel_2d_fmin20;
    unsigned short hxd_ppr_evsel_2d_fmin21;
    unsigned short hxd_ppr_evsel_2d_fmin22;
    unsigned short hxd_ppr_evsel_2d_fmin23;
    unsigned short hxd_ppr_evsel_2d_fmin24;
    unsigned short hxd_ppr_evsel_2d_fmin25;
    unsigned short hxd_ppr_evsel_2d_fmin26;
    unsigned short hxd_ppr_evsel_2d_fmin27;
    unsigned short hxd_ppr_evsel_2d_fmin28;
    unsigned short hxd_ppr_evsel_2d_fmin29;
    unsigned short hxd_ppr_evsel_2d_fmin30;
    unsigned short hxd_ppr_evsel_2d_fmin31;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin0, &anynul, &istat);
    data[0] = hxd_ppr_evsel_2d_fmin0;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin1, &anynul, &istat);
    data[1] = hxd_ppr_evsel_2d_fmin1;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin2, &anynul, &istat);
    data[2] = hxd_ppr_evsel_2d_fmin2;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin3, &anynul, &istat);
    data[3] = hxd_ppr_evsel_2d_fmin3;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin4, &anynul, &istat);
    data[4] = hxd_ppr_evsel_2d_fmin4;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin5, &anynul, &istat);
    data[5] = hxd_ppr_evsel_2d_fmin5;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin6, &anynul, &istat);
    data[6] = hxd_ppr_evsel_2d_fmin6;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin7, &anynul, &istat);
    data[7] = hxd_ppr_evsel_2d_fmin7;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN8], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin8, &anynul, &istat);
    data[8] = hxd_ppr_evsel_2d_fmin8;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN9], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin9, &anynul, &istat);
    data[9] = hxd_ppr_evsel_2d_fmin9;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN10], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin10, &anynul, &istat);
    data[10] = hxd_ppr_evsel_2d_fmin10;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN11], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin11, &anynul, &istat);
    data[11] = hxd_ppr_evsel_2d_fmin11;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN12], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin12, &anynul, &istat);
    data[12] = hxd_ppr_evsel_2d_fmin12;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN13], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin13, &anynul, &istat);
    data[13] = hxd_ppr_evsel_2d_fmin13;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN14], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin14, &anynul, &istat);
    data[14] = hxd_ppr_evsel_2d_fmin14;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN15], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin15, &anynul, &istat);
    data[15] = hxd_ppr_evsel_2d_fmin15;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN16], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin16, &anynul, &istat);
    data[16] = hxd_ppr_evsel_2d_fmin16;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN17], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin17, &anynul, &istat);
    data[17] = hxd_ppr_evsel_2d_fmin17;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN18], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin18, &anynul, &istat);
    data[18] = hxd_ppr_evsel_2d_fmin18;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN19], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin19, &anynul, &istat);
    data[19] = hxd_ppr_evsel_2d_fmin19;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN20], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin20, &anynul, &istat);
    data[20] = hxd_ppr_evsel_2d_fmin20;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN21], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin21, &anynul, &istat);
    data[21] = hxd_ppr_evsel_2d_fmin21;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN22], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin22, &anynul, &istat);
    data[22] = hxd_ppr_evsel_2d_fmin22;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN23], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin23, &anynul, &istat);
    data[23] = hxd_ppr_evsel_2d_fmin23;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN24], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin24, &anynul, &istat);
    data[24] = hxd_ppr_evsel_2d_fmin24;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN25], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin25, &anynul, &istat);
    data[25] = hxd_ppr_evsel_2d_fmin25;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN26], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin26, &anynul, &istat);
    data[26] = hxd_ppr_evsel_2d_fmin26;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN27], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin27, &anynul, &istat);
    data[27] = hxd_ppr_evsel_2d_fmin27;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN28], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin28, &anynul, &istat);
    data[28] = hxd_ppr_evsel_2d_fmin28;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN29], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin29, &anynul, &istat);
    data[29] = hxd_ppr_evsel_2d_fmin29;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN30], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin30, &anynul, &istat);
    data[30] = hxd_ppr_evsel_2d_fmin30;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_FMIN31], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_fmin31, &anynul, &istat);
    data[31] = hxd_ppr_evsel_2d_fmin31;
    BnkfPutM ("HXD:PPR:EVSEL_2D_FMIN", sizeof(int)*32, data);
  }
  {
    unsigned int data[32];
    unsigned short nulval=1;
    unsigned short hxd_ppr_evsel_2d_maxh0;
    unsigned short hxd_ppr_evsel_2d_maxh1;
    unsigned short hxd_ppr_evsel_2d_maxh2;
    unsigned short hxd_ppr_evsel_2d_maxh3;
    unsigned short hxd_ppr_evsel_2d_maxh4;
    unsigned short hxd_ppr_evsel_2d_maxh5;
    unsigned short hxd_ppr_evsel_2d_maxh6;
    unsigned short hxd_ppr_evsel_2d_maxh7;
    unsigned short hxd_ppr_evsel_2d_maxh8;
    unsigned short hxd_ppr_evsel_2d_maxh9;
    unsigned short hxd_ppr_evsel_2d_maxh10;
    unsigned short hxd_ppr_evsel_2d_maxh11;
    unsigned short hxd_ppr_evsel_2d_maxh12;
    unsigned short hxd_ppr_evsel_2d_maxh13;
    unsigned short hxd_ppr_evsel_2d_maxh14;
    unsigned short hxd_ppr_evsel_2d_maxh15;
    unsigned short hxd_ppr_evsel_2d_maxh16;
    unsigned short hxd_ppr_evsel_2d_maxh17;
    unsigned short hxd_ppr_evsel_2d_maxh18;
    unsigned short hxd_ppr_evsel_2d_maxh19;
    unsigned short hxd_ppr_evsel_2d_maxh20;
    unsigned short hxd_ppr_evsel_2d_maxh21;
    unsigned short hxd_ppr_evsel_2d_maxh22;
    unsigned short hxd_ppr_evsel_2d_maxh23;
    unsigned short hxd_ppr_evsel_2d_maxh24;
    unsigned short hxd_ppr_evsel_2d_maxh25;
    unsigned short hxd_ppr_evsel_2d_maxh26;
    unsigned short hxd_ppr_evsel_2d_maxh27;
    unsigned short hxd_ppr_evsel_2d_maxh28;
    unsigned short hxd_ppr_evsel_2d_maxh29;
    unsigned short hxd_ppr_evsel_2d_maxh30;
    unsigned short hxd_ppr_evsel_2d_maxh31;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh0, &anynul, &istat);
    data[0] = hxd_ppr_evsel_2d_maxh0;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh1, &anynul, &istat);
    data[1] = hxd_ppr_evsel_2d_maxh1;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh2, &anynul, &istat);
    data[2] = hxd_ppr_evsel_2d_maxh2;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh3, &anynul, &istat);
    data[3] = hxd_ppr_evsel_2d_maxh3;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh4, &anynul, &istat);
    data[4] = hxd_ppr_evsel_2d_maxh4;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh5, &anynul, &istat);
    data[5] = hxd_ppr_evsel_2d_maxh5;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh6, &anynul, &istat);
    data[6] = hxd_ppr_evsel_2d_maxh6;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh7, &anynul, &istat);
    data[7] = hxd_ppr_evsel_2d_maxh7;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH8], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh8, &anynul, &istat);
    data[8] = hxd_ppr_evsel_2d_maxh8;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH9], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh9, &anynul, &istat);
    data[9] = hxd_ppr_evsel_2d_maxh9;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH10], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh10, &anynul, &istat);
    data[10] = hxd_ppr_evsel_2d_maxh10;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH11], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh11, &anynul, &istat);
    data[11] = hxd_ppr_evsel_2d_maxh11;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH12], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh12, &anynul, &istat);
    data[12] = hxd_ppr_evsel_2d_maxh12;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH13], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh13, &anynul, &istat);
    data[13] = hxd_ppr_evsel_2d_maxh13;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH14], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh14, &anynul, &istat);
    data[14] = hxd_ppr_evsel_2d_maxh14;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH15], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh15, &anynul, &istat);
    data[15] = hxd_ppr_evsel_2d_maxh15;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH16], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh16, &anynul, &istat);
    data[16] = hxd_ppr_evsel_2d_maxh16;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH17], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh17, &anynul, &istat);
    data[17] = hxd_ppr_evsel_2d_maxh17;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH18], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh18, &anynul, &istat);
    data[18] = hxd_ppr_evsel_2d_maxh18;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH19], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh19, &anynul, &istat);
    data[19] = hxd_ppr_evsel_2d_maxh19;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH20], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh20, &anynul, &istat);
    data[20] = hxd_ppr_evsel_2d_maxh20;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH21], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh21, &anynul, &istat);
    data[21] = hxd_ppr_evsel_2d_maxh21;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH22], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh22, &anynul, &istat);
    data[22] = hxd_ppr_evsel_2d_maxh22;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH23], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh23, &anynul, &istat);
    data[23] = hxd_ppr_evsel_2d_maxh23;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH24], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh24, &anynul, &istat);
    data[24] = hxd_ppr_evsel_2d_maxh24;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH25], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh25, &anynul, &istat);
    data[25] = hxd_ppr_evsel_2d_maxh25;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH26], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh26, &anynul, &istat);
    data[26] = hxd_ppr_evsel_2d_maxh26;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH27], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh27, &anynul, &istat);
    data[27] = hxd_ppr_evsel_2d_maxh27;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH28], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh28, &anynul, &istat);
    data[28] = hxd_ppr_evsel_2d_maxh28;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH29], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh29, &anynul, &istat);
    data[29] = hxd_ppr_evsel_2d_maxh29;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH30], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh30, &anynul, &istat);
    data[30] = hxd_ppr_evsel_2d_maxh30;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXH31], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxh31, &anynul, &istat);
    data[31] = hxd_ppr_evsel_2d_maxh31;
    BnkfPutM ("HXD:PPR:EVSEL_2D_MAXH", sizeof(int)*32, data);
  }
  {
    unsigned int data[32];
    unsigned short nulval=1;
    unsigned short hxd_ppr_evsel_2d_maxl0;
    unsigned short hxd_ppr_evsel_2d_maxl1;
    unsigned short hxd_ppr_evsel_2d_maxl2;
    unsigned short hxd_ppr_evsel_2d_maxl3;
    unsigned short hxd_ppr_evsel_2d_maxl4;
    unsigned short hxd_ppr_evsel_2d_maxl5;
    unsigned short hxd_ppr_evsel_2d_maxl6;
    unsigned short hxd_ppr_evsel_2d_maxl7;
    unsigned short hxd_ppr_evsel_2d_maxl8;
    unsigned short hxd_ppr_evsel_2d_maxl9;
    unsigned short hxd_ppr_evsel_2d_maxl10;
    unsigned short hxd_ppr_evsel_2d_maxl11;
    unsigned short hxd_ppr_evsel_2d_maxl12;
    unsigned short hxd_ppr_evsel_2d_maxl13;
    unsigned short hxd_ppr_evsel_2d_maxl14;
    unsigned short hxd_ppr_evsel_2d_maxl15;
    unsigned short hxd_ppr_evsel_2d_maxl16;
    unsigned short hxd_ppr_evsel_2d_maxl17;
    unsigned short hxd_ppr_evsel_2d_maxl18;
    unsigned short hxd_ppr_evsel_2d_maxl19;
    unsigned short hxd_ppr_evsel_2d_maxl20;
    unsigned short hxd_ppr_evsel_2d_maxl21;
    unsigned short hxd_ppr_evsel_2d_maxl22;
    unsigned short hxd_ppr_evsel_2d_maxl23;
    unsigned short hxd_ppr_evsel_2d_maxl24;
    unsigned short hxd_ppr_evsel_2d_maxl25;
    unsigned short hxd_ppr_evsel_2d_maxl26;
    unsigned short hxd_ppr_evsel_2d_maxl27;
    unsigned short hxd_ppr_evsel_2d_maxl28;
    unsigned short hxd_ppr_evsel_2d_maxl29;
    unsigned short hxd_ppr_evsel_2d_maxl30;
    unsigned short hxd_ppr_evsel_2d_maxl31;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl0, &anynul, &istat);
    data[0] = hxd_ppr_evsel_2d_maxl0;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl1, &anynul, &istat);
    data[1] = hxd_ppr_evsel_2d_maxl1;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl2, &anynul, &istat);
    data[2] = hxd_ppr_evsel_2d_maxl2;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl3, &anynul, &istat);
    data[3] = hxd_ppr_evsel_2d_maxl3;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl4, &anynul, &istat);
    data[4] = hxd_ppr_evsel_2d_maxl4;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl5, &anynul, &istat);
    data[5] = hxd_ppr_evsel_2d_maxl5;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl6, &anynul, &istat);
    data[6] = hxd_ppr_evsel_2d_maxl6;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl7, &anynul, &istat);
    data[7] = hxd_ppr_evsel_2d_maxl7;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL8], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl8, &anynul, &istat);
    data[8] = hxd_ppr_evsel_2d_maxl8;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL9], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl9, &anynul, &istat);
    data[9] = hxd_ppr_evsel_2d_maxl9;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL10], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl10, &anynul, &istat);
    data[10] = hxd_ppr_evsel_2d_maxl10;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL11], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl11, &anynul, &istat);
    data[11] = hxd_ppr_evsel_2d_maxl11;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL12], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl12, &anynul, &istat);
    data[12] = hxd_ppr_evsel_2d_maxl12;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL13], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl13, &anynul, &istat);
    data[13] = hxd_ppr_evsel_2d_maxl13;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL14], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl14, &anynul, &istat);
    data[14] = hxd_ppr_evsel_2d_maxl14;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL15], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl15, &anynul, &istat);
    data[15] = hxd_ppr_evsel_2d_maxl15;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL16], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl16, &anynul, &istat);
    data[16] = hxd_ppr_evsel_2d_maxl16;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL17], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl17, &anynul, &istat);
    data[17] = hxd_ppr_evsel_2d_maxl17;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL18], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl18, &anynul, &istat);
    data[18] = hxd_ppr_evsel_2d_maxl18;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL19], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl19, &anynul, &istat);
    data[19] = hxd_ppr_evsel_2d_maxl19;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL20], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl20, &anynul, &istat);
    data[20] = hxd_ppr_evsel_2d_maxl20;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL21], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl21, &anynul, &istat);
    data[21] = hxd_ppr_evsel_2d_maxl21;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL22], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl22, &anynul, &istat);
    data[22] = hxd_ppr_evsel_2d_maxl22;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL23], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl23, &anynul, &istat);
    data[23] = hxd_ppr_evsel_2d_maxl23;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL24], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl24, &anynul, &istat);
    data[24] = hxd_ppr_evsel_2d_maxl24;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL25], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl25, &anynul, &istat);
    data[25] = hxd_ppr_evsel_2d_maxl25;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL26], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl26, &anynul, &istat);
    data[26] = hxd_ppr_evsel_2d_maxl26;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL27], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl27, &anynul, &istat);
    data[27] = hxd_ppr_evsel_2d_maxl27;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL28], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl28, &anynul, &istat);
    data[28] = hxd_ppr_evsel_2d_maxl28;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL29], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl29, &anynul, &istat);
    data[29] = hxd_ppr_evsel_2d_maxl29;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL30], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl30, &anynul, &istat);
    data[30] = hxd_ppr_evsel_2d_maxl30;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MAXL31], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_maxl31, &anynul, &istat);
    data[31] = hxd_ppr_evsel_2d_maxl31;
    BnkfPutM ("HXD:PPR:EVSEL_2D_MAXL", sizeof(int)*32, data);
  }
  {
    unsigned int data[32];
    unsigned short nulval=1;
    unsigned short hxd_ppr_evsel_2d_minh0;
    unsigned short hxd_ppr_evsel_2d_minh1;
    unsigned short hxd_ppr_evsel_2d_minh2;
    unsigned short hxd_ppr_evsel_2d_minh3;
    unsigned short hxd_ppr_evsel_2d_minh4;
    unsigned short hxd_ppr_evsel_2d_minh5;
    unsigned short hxd_ppr_evsel_2d_minh6;
    unsigned short hxd_ppr_evsel_2d_minh7;
    unsigned short hxd_ppr_evsel_2d_minh8;
    unsigned short hxd_ppr_evsel_2d_minh9;
    unsigned short hxd_ppr_evsel_2d_minh10;
    unsigned short hxd_ppr_evsel_2d_minh11;
    unsigned short hxd_ppr_evsel_2d_minh12;
    unsigned short hxd_ppr_evsel_2d_minh13;
    unsigned short hxd_ppr_evsel_2d_minh14;
    unsigned short hxd_ppr_evsel_2d_minh15;
    unsigned short hxd_ppr_evsel_2d_minh16;
    unsigned short hxd_ppr_evsel_2d_minh17;
    unsigned short hxd_ppr_evsel_2d_minh18;
    unsigned short hxd_ppr_evsel_2d_minh19;
    unsigned short hxd_ppr_evsel_2d_minh20;
    unsigned short hxd_ppr_evsel_2d_minh21;
    unsigned short hxd_ppr_evsel_2d_minh22;
    unsigned short hxd_ppr_evsel_2d_minh23;
    unsigned short hxd_ppr_evsel_2d_minh24;
    unsigned short hxd_ppr_evsel_2d_minh25;
    unsigned short hxd_ppr_evsel_2d_minh26;
    unsigned short hxd_ppr_evsel_2d_minh27;
    unsigned short hxd_ppr_evsel_2d_minh28;
    unsigned short hxd_ppr_evsel_2d_minh29;
    unsigned short hxd_ppr_evsel_2d_minh30;
    unsigned short hxd_ppr_evsel_2d_minh31;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh0, &anynul, &istat);
    data[0] = hxd_ppr_evsel_2d_minh0;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh1, &anynul, &istat);
    data[1] = hxd_ppr_evsel_2d_minh1;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh2, &anynul, &istat);
    data[2] = hxd_ppr_evsel_2d_minh2;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh3, &anynul, &istat);
    data[3] = hxd_ppr_evsel_2d_minh3;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh4, &anynul, &istat);
    data[4] = hxd_ppr_evsel_2d_minh4;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh5, &anynul, &istat);
    data[5] = hxd_ppr_evsel_2d_minh5;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh6, &anynul, &istat);
    data[6] = hxd_ppr_evsel_2d_minh6;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh7, &anynul, &istat);
    data[7] = hxd_ppr_evsel_2d_minh7;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH8], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh8, &anynul, &istat);
    data[8] = hxd_ppr_evsel_2d_minh8;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH9], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh9, &anynul, &istat);
    data[9] = hxd_ppr_evsel_2d_minh9;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH10], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh10, &anynul, &istat);
    data[10] = hxd_ppr_evsel_2d_minh10;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH11], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh11, &anynul, &istat);
    data[11] = hxd_ppr_evsel_2d_minh11;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH12], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh12, &anynul, &istat);
    data[12] = hxd_ppr_evsel_2d_minh12;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH13], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh13, &anynul, &istat);
    data[13] = hxd_ppr_evsel_2d_minh13;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH14], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh14, &anynul, &istat);
    data[14] = hxd_ppr_evsel_2d_minh14;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH15], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh15, &anynul, &istat);
    data[15] = hxd_ppr_evsel_2d_minh15;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH16], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh16, &anynul, &istat);
    data[16] = hxd_ppr_evsel_2d_minh16;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH17], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh17, &anynul, &istat);
    data[17] = hxd_ppr_evsel_2d_minh17;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH18], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh18, &anynul, &istat);
    data[18] = hxd_ppr_evsel_2d_minh18;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH19], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh19, &anynul, &istat);
    data[19] = hxd_ppr_evsel_2d_minh19;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH20], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh20, &anynul, &istat);
    data[20] = hxd_ppr_evsel_2d_minh20;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH21], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh21, &anynul, &istat);
    data[21] = hxd_ppr_evsel_2d_minh21;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH22], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh22, &anynul, &istat);
    data[22] = hxd_ppr_evsel_2d_minh22;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH23], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh23, &anynul, &istat);
    data[23] = hxd_ppr_evsel_2d_minh23;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH24], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh24, &anynul, &istat);
    data[24] = hxd_ppr_evsel_2d_minh24;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH25], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh25, &anynul, &istat);
    data[25] = hxd_ppr_evsel_2d_minh25;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH26], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh26, &anynul, &istat);
    data[26] = hxd_ppr_evsel_2d_minh26;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH27], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh27, &anynul, &istat);
    data[27] = hxd_ppr_evsel_2d_minh27;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH28], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh28, &anynul, &istat);
    data[28] = hxd_ppr_evsel_2d_minh28;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH29], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh29, &anynul, &istat);
    data[29] = hxd_ppr_evsel_2d_minh29;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH30], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh30, &anynul, &istat);
    data[30] = hxd_ppr_evsel_2d_minh30;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINH31], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minh31, &anynul, &istat);
    data[31] = hxd_ppr_evsel_2d_minh31;
    BnkfPutM ("HXD:PPR:EVSEL_2D_MINH", sizeof(int)*32, data);
  }
  {
    unsigned int data[32];
    unsigned short nulval=1;
    unsigned short hxd_ppr_evsel_2d_minl0;
    unsigned short hxd_ppr_evsel_2d_minl1;
    unsigned short hxd_ppr_evsel_2d_minl2;
    unsigned short hxd_ppr_evsel_2d_minl3;
    unsigned short hxd_ppr_evsel_2d_minl4;
    unsigned short hxd_ppr_evsel_2d_minl5;
    unsigned short hxd_ppr_evsel_2d_minl6;
    unsigned short hxd_ppr_evsel_2d_minl7;
    unsigned short hxd_ppr_evsel_2d_minl8;
    unsigned short hxd_ppr_evsel_2d_minl9;
    unsigned short hxd_ppr_evsel_2d_minl10;
    unsigned short hxd_ppr_evsel_2d_minl11;
    unsigned short hxd_ppr_evsel_2d_minl12;
    unsigned short hxd_ppr_evsel_2d_minl13;
    unsigned short hxd_ppr_evsel_2d_minl14;
    unsigned short hxd_ppr_evsel_2d_minl15;
    unsigned short hxd_ppr_evsel_2d_minl16;
    unsigned short hxd_ppr_evsel_2d_minl17;
    unsigned short hxd_ppr_evsel_2d_minl18;
    unsigned short hxd_ppr_evsel_2d_minl19;
    unsigned short hxd_ppr_evsel_2d_minl20;
    unsigned short hxd_ppr_evsel_2d_minl21;
    unsigned short hxd_ppr_evsel_2d_minl22;
    unsigned short hxd_ppr_evsel_2d_minl23;
    unsigned short hxd_ppr_evsel_2d_minl24;
    unsigned short hxd_ppr_evsel_2d_minl25;
    unsigned short hxd_ppr_evsel_2d_minl26;
    unsigned short hxd_ppr_evsel_2d_minl27;
    unsigned short hxd_ppr_evsel_2d_minl28;
    unsigned short hxd_ppr_evsel_2d_minl29;
    unsigned short hxd_ppr_evsel_2d_minl30;
    unsigned short hxd_ppr_evsel_2d_minl31;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl0, &anynul, &istat);
    data[0] = hxd_ppr_evsel_2d_minl0;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl1, &anynul, &istat);
    data[1] = hxd_ppr_evsel_2d_minl1;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl2, &anynul, &istat);
    data[2] = hxd_ppr_evsel_2d_minl2;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl3, &anynul, &istat);
    data[3] = hxd_ppr_evsel_2d_minl3;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl4, &anynul, &istat);
    data[4] = hxd_ppr_evsel_2d_minl4;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl5, &anynul, &istat);
    data[5] = hxd_ppr_evsel_2d_minl5;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl6, &anynul, &istat);
    data[6] = hxd_ppr_evsel_2d_minl6;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl7, &anynul, &istat);
    data[7] = hxd_ppr_evsel_2d_minl7;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL8], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl8, &anynul, &istat);
    data[8] = hxd_ppr_evsel_2d_minl8;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL9], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl9, &anynul, &istat);
    data[9] = hxd_ppr_evsel_2d_minl9;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL10], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl10, &anynul, &istat);
    data[10] = hxd_ppr_evsel_2d_minl10;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL11], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl11, &anynul, &istat);
    data[11] = hxd_ppr_evsel_2d_minl11;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL12], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl12, &anynul, &istat);
    data[12] = hxd_ppr_evsel_2d_minl12;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL13], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl13, &anynul, &istat);
    data[13] = hxd_ppr_evsel_2d_minl13;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL14], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl14, &anynul, &istat);
    data[14] = hxd_ppr_evsel_2d_minl14;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL15], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl15, &anynul, &istat);
    data[15] = hxd_ppr_evsel_2d_minl15;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL16], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl16, &anynul, &istat);
    data[16] = hxd_ppr_evsel_2d_minl16;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL17], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl17, &anynul, &istat);
    data[17] = hxd_ppr_evsel_2d_minl17;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL18], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl18, &anynul, &istat);
    data[18] = hxd_ppr_evsel_2d_minl18;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL19], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl19, &anynul, &istat);
    data[19] = hxd_ppr_evsel_2d_minl19;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL20], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl20, &anynul, &istat);
    data[20] = hxd_ppr_evsel_2d_minl20;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL21], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl21, &anynul, &istat);
    data[21] = hxd_ppr_evsel_2d_minl21;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL22], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl22, &anynul, &istat);
    data[22] = hxd_ppr_evsel_2d_minl22;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL23], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl23, &anynul, &istat);
    data[23] = hxd_ppr_evsel_2d_minl23;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL24], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl24, &anynul, &istat);
    data[24] = hxd_ppr_evsel_2d_minl24;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL25], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl25, &anynul, &istat);
    data[25] = hxd_ppr_evsel_2d_minl25;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL26], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl26, &anynul, &istat);
    data[26] = hxd_ppr_evsel_2d_minl26;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL27], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl27, &anynul, &istat);
    data[27] = hxd_ppr_evsel_2d_minl27;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL28], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl28, &anynul, &istat);
    data[28] = hxd_ppr_evsel_2d_minl28;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL29], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl29, &anynul, &istat);
    data[29] = hxd_ppr_evsel_2d_minl29;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL30], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl30, &anynul, &istat);
    data[30] = hxd_ppr_evsel_2d_minl30;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_2D_MINL31], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_2d_minl31, &anynul, &istat);
    data[31] = hxd_ppr_evsel_2d_minl31;
    BnkfPutM ("HXD:PPR:EVSEL_2D_MINL", sizeof(int)*32, data);
  }
  {
    unsigned int data[16];
    unsigned short nulval=1;
    unsigned short hxd_ppr_evsel_dlt_time0;
    unsigned short hxd_ppr_evsel_dlt_time1;
    unsigned short hxd_ppr_evsel_dlt_time2;
    unsigned short hxd_ppr_evsel_dlt_time3;
    unsigned short hxd_ppr_evsel_dlt_time4;
    unsigned short hxd_ppr_evsel_dlt_time5;
    unsigned short hxd_ppr_evsel_dlt_time6;
    unsigned short hxd_ppr_evsel_dlt_time7;
    unsigned short hxd_ppr_evsel_dlt_time8;
    unsigned short hxd_ppr_evsel_dlt_time9;
    unsigned short hxd_ppr_evsel_dlt_time10;
    unsigned short hxd_ppr_evsel_dlt_time11;
    unsigned short hxd_ppr_evsel_dlt_time12;
    unsigned short hxd_ppr_evsel_dlt_time13;
    unsigned short hxd_ppr_evsel_dlt_time14;
    unsigned short hxd_ppr_evsel_dlt_time15;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_DLT_TIME0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_dlt_time0, &anynul, &istat);
    data[0] = hxd_ppr_evsel_dlt_time0;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_DLT_TIME1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_dlt_time1, &anynul, &istat);
    data[1] = hxd_ppr_evsel_dlt_time1;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_DLT_TIME2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_dlt_time2, &anynul, &istat);
    data[2] = hxd_ppr_evsel_dlt_time2;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_DLT_TIME3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_dlt_time3, &anynul, &istat);
    data[3] = hxd_ppr_evsel_dlt_time3;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_DLT_TIME4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_dlt_time4, &anynul, &istat);
    data[4] = hxd_ppr_evsel_dlt_time4;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_DLT_TIME5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_dlt_time5, &anynul, &istat);
    data[5] = hxd_ppr_evsel_dlt_time5;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_DLT_TIME6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_dlt_time6, &anynul, &istat);
    data[6] = hxd_ppr_evsel_dlt_time6;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_DLT_TIME7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_dlt_time7, &anynul, &istat);
    data[7] = hxd_ppr_evsel_dlt_time7;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_DLT_TIME8], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_dlt_time8, &anynul, &istat);
    data[8] = hxd_ppr_evsel_dlt_time8;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_DLT_TIME9], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_dlt_time9, &anynul, &istat);
    data[9] = hxd_ppr_evsel_dlt_time9;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_DLT_TIME10], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_dlt_time10, &anynul, &istat);
    data[10] = hxd_ppr_evsel_dlt_time10;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_DLT_TIME11], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_dlt_time11, &anynul, &istat);
    data[11] = hxd_ppr_evsel_dlt_time11;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_DLT_TIME12], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_dlt_time12, &anynul, &istat);
    data[12] = hxd_ppr_evsel_dlt_time12;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_DLT_TIME13], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_dlt_time13, &anynul, &istat);
    data[13] = hxd_ppr_evsel_dlt_time13;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_DLT_TIME14], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_dlt_time14, &anynul, &istat);
    data[14] = hxd_ppr_evsel_dlt_time14;
    fits_read_col_usht(fp, colnum[HXD_PPR_EVSEL_DLT_TIME15], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_dlt_time15, &anynul, &istat);
    data[15] = hxd_ppr_evsel_dlt_time15;
    BnkfPutM ("HXD:PPR:EVSEL_DLT_TIME", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned int nulval=1;
    unsigned int hxd_ppr_evsel_flg_mask0;
    unsigned int hxd_ppr_evsel_flg_mask1;
    unsigned int hxd_ppr_evsel_flg_mask2;
    unsigned int hxd_ppr_evsel_flg_mask3;
    unsigned int hxd_ppr_evsel_flg_mask4;
    unsigned int hxd_ppr_evsel_flg_mask5;
    unsigned int hxd_ppr_evsel_flg_mask6;
    unsigned int hxd_ppr_evsel_flg_mask7;
    unsigned int hxd_ppr_evsel_flg_mask8;
    unsigned int hxd_ppr_evsel_flg_mask9;
    unsigned int hxd_ppr_evsel_flg_mask10;
    unsigned int hxd_ppr_evsel_flg_mask11;
    unsigned int hxd_ppr_evsel_flg_mask12;
    unsigned int hxd_ppr_evsel_flg_mask13;
    unsigned int hxd_ppr_evsel_flg_mask14;
    unsigned int hxd_ppr_evsel_flg_mask15;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_FLG_MASK0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_flg_mask0, &anynul, &istat);
    data[0] = hxd_ppr_evsel_flg_mask0;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_FLG_MASK1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_flg_mask1, &anynul, &istat);
    data[1] = hxd_ppr_evsel_flg_mask1;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_FLG_MASK2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_flg_mask2, &anynul, &istat);
    data[2] = hxd_ppr_evsel_flg_mask2;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_FLG_MASK3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_flg_mask3, &anynul, &istat);
    data[3] = hxd_ppr_evsel_flg_mask3;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_FLG_MASK4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_flg_mask4, &anynul, &istat);
    data[4] = hxd_ppr_evsel_flg_mask4;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_FLG_MASK5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_flg_mask5, &anynul, &istat);
    data[5] = hxd_ppr_evsel_flg_mask5;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_FLG_MASK6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_flg_mask6, &anynul, &istat);
    data[6] = hxd_ppr_evsel_flg_mask6;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_FLG_MASK7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_flg_mask7, &anynul, &istat);
    data[7] = hxd_ppr_evsel_flg_mask7;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_FLG_MASK8], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_flg_mask8, &anynul, &istat);
    data[8] = hxd_ppr_evsel_flg_mask8;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_FLG_MASK9], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_flg_mask9, &anynul, &istat);
    data[9] = hxd_ppr_evsel_flg_mask9;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_FLG_MASK10], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_flg_mask10, &anynul, &istat);
    data[10] = hxd_ppr_evsel_flg_mask10;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_FLG_MASK11], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_flg_mask11, &anynul, &istat);
    data[11] = hxd_ppr_evsel_flg_mask11;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_FLG_MASK12], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_flg_mask12, &anynul, &istat);
    data[12] = hxd_ppr_evsel_flg_mask12;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_FLG_MASK13], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_flg_mask13, &anynul, &istat);
    data[13] = hxd_ppr_evsel_flg_mask13;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_FLG_MASK14], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_flg_mask14, &anynul, &istat);
    data[14] = hxd_ppr_evsel_flg_mask14;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_FLG_MASK15], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_flg_mask15, &anynul, &istat);
    data[15] = hxd_ppr_evsel_flg_mask15;
    BnkfPutM ("HXD:PPR:EVSEL_FLG_MASK", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned int nulval=1;
    unsigned int hxd_ppr_evsel_trg_mask0;
    unsigned int hxd_ppr_evsel_trg_mask1;
    unsigned int hxd_ppr_evsel_trg_mask2;
    unsigned int hxd_ppr_evsel_trg_mask3;
    unsigned int hxd_ppr_evsel_trg_mask4;
    unsigned int hxd_ppr_evsel_trg_mask5;
    unsigned int hxd_ppr_evsel_trg_mask6;
    unsigned int hxd_ppr_evsel_trg_mask7;
    unsigned int hxd_ppr_evsel_trg_mask8;
    unsigned int hxd_ppr_evsel_trg_mask9;
    unsigned int hxd_ppr_evsel_trg_mask10;
    unsigned int hxd_ppr_evsel_trg_mask11;
    unsigned int hxd_ppr_evsel_trg_mask12;
    unsigned int hxd_ppr_evsel_trg_mask13;
    unsigned int hxd_ppr_evsel_trg_mask14;
    unsigned int hxd_ppr_evsel_trg_mask15;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_TRG_MASK0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_trg_mask0, &anynul, &istat);
    data[0] = hxd_ppr_evsel_trg_mask0;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_TRG_MASK1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_trg_mask1, &anynul, &istat);
    data[1] = hxd_ppr_evsel_trg_mask1;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_TRG_MASK2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_trg_mask2, &anynul, &istat);
    data[2] = hxd_ppr_evsel_trg_mask2;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_TRG_MASK3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_trg_mask3, &anynul, &istat);
    data[3] = hxd_ppr_evsel_trg_mask3;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_TRG_MASK4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_trg_mask4, &anynul, &istat);
    data[4] = hxd_ppr_evsel_trg_mask4;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_TRG_MASK5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_trg_mask5, &anynul, &istat);
    data[5] = hxd_ppr_evsel_trg_mask5;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_TRG_MASK6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_trg_mask6, &anynul, &istat);
    data[6] = hxd_ppr_evsel_trg_mask6;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_TRG_MASK7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_trg_mask7, &anynul, &istat);
    data[7] = hxd_ppr_evsel_trg_mask7;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_TRG_MASK8], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_trg_mask8, &anynul, &istat);
    data[8] = hxd_ppr_evsel_trg_mask8;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_TRG_MASK9], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_trg_mask9, &anynul, &istat);
    data[9] = hxd_ppr_evsel_trg_mask9;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_TRG_MASK10], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_trg_mask10, &anynul, &istat);
    data[10] = hxd_ppr_evsel_trg_mask10;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_TRG_MASK11], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_trg_mask11, &anynul, &istat);
    data[11] = hxd_ppr_evsel_trg_mask11;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_TRG_MASK12], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_trg_mask12, &anynul, &istat);
    data[12] = hxd_ppr_evsel_trg_mask12;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_TRG_MASK13], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_trg_mask13, &anynul, &istat);
    data[13] = hxd_ppr_evsel_trg_mask13;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_TRG_MASK14], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_trg_mask14, &anynul, &istat);
    data[14] = hxd_ppr_evsel_trg_mask14;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_TRG_MASK15], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_trg_mask15, &anynul, &istat);
    data[15] = hxd_ppr_evsel_trg_mask15;
    BnkfPutM ("HXD:PPR:EVSEL_TRG_MASK", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned int nulval=1;
    unsigned int hxd_ppr_evsel_hit_msk10;
    unsigned int hxd_ppr_evsel_hit_msk11;
    unsigned int hxd_ppr_evsel_hit_msk12;
    unsigned int hxd_ppr_evsel_hit_msk13;
    unsigned int hxd_ppr_evsel_hit_msk14;
    unsigned int hxd_ppr_evsel_hit_msk15;
    unsigned int hxd_ppr_evsel_hit_msk16;
    unsigned int hxd_ppr_evsel_hit_msk17;
    unsigned int hxd_ppr_evsel_hit_msk18;
    unsigned int hxd_ppr_evsel_hit_msk19;
    unsigned int hxd_ppr_evsel_hit_msk110;
    unsigned int hxd_ppr_evsel_hit_msk111;
    unsigned int hxd_ppr_evsel_hit_msk112;
    unsigned int hxd_ppr_evsel_hit_msk113;
    unsigned int hxd_ppr_evsel_hit_msk114;
    unsigned int hxd_ppr_evsel_hit_msk115;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK10], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk10, &anynul, &istat);
    data[0] = hxd_ppr_evsel_hit_msk10;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK11], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk11, &anynul, &istat);
    data[1] = hxd_ppr_evsel_hit_msk11;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK12], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk12, &anynul, &istat);
    data[2] = hxd_ppr_evsel_hit_msk12;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK13], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk13, &anynul, &istat);
    data[3] = hxd_ppr_evsel_hit_msk13;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK14], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk14, &anynul, &istat);
    data[4] = hxd_ppr_evsel_hit_msk14;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK15], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk15, &anynul, &istat);
    data[5] = hxd_ppr_evsel_hit_msk15;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK16], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk16, &anynul, &istat);
    data[6] = hxd_ppr_evsel_hit_msk16;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK17], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk17, &anynul, &istat);
    data[7] = hxd_ppr_evsel_hit_msk17;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK18], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk18, &anynul, &istat);
    data[8] = hxd_ppr_evsel_hit_msk18;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK19], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk19, &anynul, &istat);
    data[9] = hxd_ppr_evsel_hit_msk19;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK110], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk110, &anynul, &istat);
    data[10] = hxd_ppr_evsel_hit_msk110;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK111], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk111, &anynul, &istat);
    data[11] = hxd_ppr_evsel_hit_msk111;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK112], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk112, &anynul, &istat);
    data[12] = hxd_ppr_evsel_hit_msk112;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK113], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk113, &anynul, &istat);
    data[13] = hxd_ppr_evsel_hit_msk113;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK114], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk114, &anynul, &istat);
    data[14] = hxd_ppr_evsel_hit_msk114;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK115], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk115, &anynul, &istat);
    data[15] = hxd_ppr_evsel_hit_msk115;
    BnkfPutM ("HXD:PPR:EVSEL_HIT_MSK1", sizeof(int)*16, data);
  }
  {
    unsigned int data[16];
    unsigned int nulval=1;
    unsigned int hxd_ppr_evsel_hit_msk20;
    unsigned int hxd_ppr_evsel_hit_msk21;
    unsigned int hxd_ppr_evsel_hit_msk22;
    unsigned int hxd_ppr_evsel_hit_msk23;
    unsigned int hxd_ppr_evsel_hit_msk24;
    unsigned int hxd_ppr_evsel_hit_msk25;
    unsigned int hxd_ppr_evsel_hit_msk26;
    unsigned int hxd_ppr_evsel_hit_msk27;
    unsigned int hxd_ppr_evsel_hit_msk28;
    unsigned int hxd_ppr_evsel_hit_msk29;
    unsigned int hxd_ppr_evsel_hit_msk210;
    unsigned int hxd_ppr_evsel_hit_msk211;
    unsigned int hxd_ppr_evsel_hit_msk212;
    unsigned int hxd_ppr_evsel_hit_msk213;
    unsigned int hxd_ppr_evsel_hit_msk214;
    unsigned int hxd_ppr_evsel_hit_msk215;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK20], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk20, &anynul, &istat);
    data[0] = hxd_ppr_evsel_hit_msk20;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK21], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk21, &anynul, &istat);
    data[1] = hxd_ppr_evsel_hit_msk21;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK22], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk22, &anynul, &istat);
    data[2] = hxd_ppr_evsel_hit_msk22;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK23], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk23, &anynul, &istat);
    data[3] = hxd_ppr_evsel_hit_msk23;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK24], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk24, &anynul, &istat);
    data[4] = hxd_ppr_evsel_hit_msk24;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK25], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk25, &anynul, &istat);
    data[5] = hxd_ppr_evsel_hit_msk25;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK26], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk26, &anynul, &istat);
    data[6] = hxd_ppr_evsel_hit_msk26;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK27], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk27, &anynul, &istat);
    data[7] = hxd_ppr_evsel_hit_msk27;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK28], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk28, &anynul, &istat);
    data[8] = hxd_ppr_evsel_hit_msk28;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK29], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk29, &anynul, &istat);
    data[9] = hxd_ppr_evsel_hit_msk29;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK210], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk210, &anynul, &istat);
    data[10] = hxd_ppr_evsel_hit_msk210;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK211], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk211, &anynul, &istat);
    data[11] = hxd_ppr_evsel_hit_msk211;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK212], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk212, &anynul, &istat);
    data[12] = hxd_ppr_evsel_hit_msk212;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK213], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk213, &anynul, &istat);
    data[13] = hxd_ppr_evsel_hit_msk213;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK214], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk214, &anynul, &istat);
    data[14] = hxd_ppr_evsel_hit_msk214;
    fits_read_col_uint(fp, colnum[HXD_PPR_EVSEL_HIT_MSK215], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_hit_msk215, &anynul, &istat);
    data[15] = hxd_ppr_evsel_hit_msk215;
    BnkfPutM ("HXD:PPR:EVSEL_HIT_MSK2", sizeof(int)*16, data);
  }
  {
    unsigned int data[64];
    unsigned char nulval=1;
    unsigned char hxd_ppr_evsel_pin_ld0;
    unsigned char hxd_ppr_evsel_pin_ld1;
    unsigned char hxd_ppr_evsel_pin_ld2;
    unsigned char hxd_ppr_evsel_pin_ld3;
    unsigned char hxd_ppr_evsel_pin_ld4;
    unsigned char hxd_ppr_evsel_pin_ld5;
    unsigned char hxd_ppr_evsel_pin_ld6;
    unsigned char hxd_ppr_evsel_pin_ld7;
    unsigned char hxd_ppr_evsel_pin_ld8;
    unsigned char hxd_ppr_evsel_pin_ld9;
    unsigned char hxd_ppr_evsel_pin_ld10;
    unsigned char hxd_ppr_evsel_pin_ld11;
    unsigned char hxd_ppr_evsel_pin_ld12;
    unsigned char hxd_ppr_evsel_pin_ld13;
    unsigned char hxd_ppr_evsel_pin_ld14;
    unsigned char hxd_ppr_evsel_pin_ld15;
    unsigned char hxd_ppr_evsel_pin_ld16;
    unsigned char hxd_ppr_evsel_pin_ld17;
    unsigned char hxd_ppr_evsel_pin_ld18;
    unsigned char hxd_ppr_evsel_pin_ld19;
    unsigned char hxd_ppr_evsel_pin_ld20;
    unsigned char hxd_ppr_evsel_pin_ld21;
    unsigned char hxd_ppr_evsel_pin_ld22;
    unsigned char hxd_ppr_evsel_pin_ld23;
    unsigned char hxd_ppr_evsel_pin_ld24;
    unsigned char hxd_ppr_evsel_pin_ld25;
    unsigned char hxd_ppr_evsel_pin_ld26;
    unsigned char hxd_ppr_evsel_pin_ld27;
    unsigned char hxd_ppr_evsel_pin_ld28;
    unsigned char hxd_ppr_evsel_pin_ld29;
    unsigned char hxd_ppr_evsel_pin_ld30;
    unsigned char hxd_ppr_evsel_pin_ld31;
    unsigned char hxd_ppr_evsel_pin_ld32;
    unsigned char hxd_ppr_evsel_pin_ld33;
    unsigned char hxd_ppr_evsel_pin_ld34;
    unsigned char hxd_ppr_evsel_pin_ld35;
    unsigned char hxd_ppr_evsel_pin_ld36;
    unsigned char hxd_ppr_evsel_pin_ld37;
    unsigned char hxd_ppr_evsel_pin_ld38;
    unsigned char hxd_ppr_evsel_pin_ld39;
    unsigned char hxd_ppr_evsel_pin_ld40;
    unsigned char hxd_ppr_evsel_pin_ld41;
    unsigned char hxd_ppr_evsel_pin_ld42;
    unsigned char hxd_ppr_evsel_pin_ld43;
    unsigned char hxd_ppr_evsel_pin_ld44;
    unsigned char hxd_ppr_evsel_pin_ld45;
    unsigned char hxd_ppr_evsel_pin_ld46;
    unsigned char hxd_ppr_evsel_pin_ld47;
    unsigned char hxd_ppr_evsel_pin_ld48;
    unsigned char hxd_ppr_evsel_pin_ld49;
    unsigned char hxd_ppr_evsel_pin_ld50;
    unsigned char hxd_ppr_evsel_pin_ld51;
    unsigned char hxd_ppr_evsel_pin_ld52;
    unsigned char hxd_ppr_evsel_pin_ld53;
    unsigned char hxd_ppr_evsel_pin_ld54;
    unsigned char hxd_ppr_evsel_pin_ld55;
    unsigned char hxd_ppr_evsel_pin_ld56;
    unsigned char hxd_ppr_evsel_pin_ld57;
    unsigned char hxd_ppr_evsel_pin_ld58;
    unsigned char hxd_ppr_evsel_pin_ld59;
    unsigned char hxd_ppr_evsel_pin_ld60;
    unsigned char hxd_ppr_evsel_pin_ld61;
    unsigned char hxd_ppr_evsel_pin_ld62;
    unsigned char hxd_ppr_evsel_pin_ld63;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld0, &anynul, &istat);
    data[0] = hxd_ppr_evsel_pin_ld0;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld1, &anynul, &istat);
    data[1] = hxd_ppr_evsel_pin_ld1;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld2, &anynul, &istat);
    data[2] = hxd_ppr_evsel_pin_ld2;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld3, &anynul, &istat);
    data[3] = hxd_ppr_evsel_pin_ld3;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld4, &anynul, &istat);
    data[4] = hxd_ppr_evsel_pin_ld4;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld5, &anynul, &istat);
    data[5] = hxd_ppr_evsel_pin_ld5;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld6, &anynul, &istat);
    data[6] = hxd_ppr_evsel_pin_ld6;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld7, &anynul, &istat);
    data[7] = hxd_ppr_evsel_pin_ld7;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD8], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld8, &anynul, &istat);
    data[8] = hxd_ppr_evsel_pin_ld8;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD9], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld9, &anynul, &istat);
    data[9] = hxd_ppr_evsel_pin_ld9;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD10], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld10, &anynul, &istat);
    data[10] = hxd_ppr_evsel_pin_ld10;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD11], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld11, &anynul, &istat);
    data[11] = hxd_ppr_evsel_pin_ld11;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD12], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld12, &anynul, &istat);
    data[12] = hxd_ppr_evsel_pin_ld12;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD13], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld13, &anynul, &istat);
    data[13] = hxd_ppr_evsel_pin_ld13;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD14], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld14, &anynul, &istat);
    data[14] = hxd_ppr_evsel_pin_ld14;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD15], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld15, &anynul, &istat);
    data[15] = hxd_ppr_evsel_pin_ld15;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD16], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld16, &anynul, &istat);
    data[16] = hxd_ppr_evsel_pin_ld16;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD17], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld17, &anynul, &istat);
    data[17] = hxd_ppr_evsel_pin_ld17;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD18], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld18, &anynul, &istat);
    data[18] = hxd_ppr_evsel_pin_ld18;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD19], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld19, &anynul, &istat);
    data[19] = hxd_ppr_evsel_pin_ld19;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD20], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld20, &anynul, &istat);
    data[20] = hxd_ppr_evsel_pin_ld20;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD21], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld21, &anynul, &istat);
    data[21] = hxd_ppr_evsel_pin_ld21;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD22], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld22, &anynul, &istat);
    data[22] = hxd_ppr_evsel_pin_ld22;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD23], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld23, &anynul, &istat);
    data[23] = hxd_ppr_evsel_pin_ld23;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD24], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld24, &anynul, &istat);
    data[24] = hxd_ppr_evsel_pin_ld24;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD25], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld25, &anynul, &istat);
    data[25] = hxd_ppr_evsel_pin_ld25;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD26], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld26, &anynul, &istat);
    data[26] = hxd_ppr_evsel_pin_ld26;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD27], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld27, &anynul, &istat);
    data[27] = hxd_ppr_evsel_pin_ld27;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD28], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld28, &anynul, &istat);
    data[28] = hxd_ppr_evsel_pin_ld28;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD29], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld29, &anynul, &istat);
    data[29] = hxd_ppr_evsel_pin_ld29;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD30], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld30, &anynul, &istat);
    data[30] = hxd_ppr_evsel_pin_ld30;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD31], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld31, &anynul, &istat);
    data[31] = hxd_ppr_evsel_pin_ld31;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD32], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld32, &anynul, &istat);
    data[32] = hxd_ppr_evsel_pin_ld32;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD33], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld33, &anynul, &istat);
    data[33] = hxd_ppr_evsel_pin_ld33;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD34], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld34, &anynul, &istat);
    data[34] = hxd_ppr_evsel_pin_ld34;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD35], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld35, &anynul, &istat);
    data[35] = hxd_ppr_evsel_pin_ld35;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD36], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld36, &anynul, &istat);
    data[36] = hxd_ppr_evsel_pin_ld36;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD37], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld37, &anynul, &istat);
    data[37] = hxd_ppr_evsel_pin_ld37;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD38], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld38, &anynul, &istat);
    data[38] = hxd_ppr_evsel_pin_ld38;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD39], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld39, &anynul, &istat);
    data[39] = hxd_ppr_evsel_pin_ld39;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD40], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld40, &anynul, &istat);
    data[40] = hxd_ppr_evsel_pin_ld40;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD41], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld41, &anynul, &istat);
    data[41] = hxd_ppr_evsel_pin_ld41;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD42], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld42, &anynul, &istat);
    data[42] = hxd_ppr_evsel_pin_ld42;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD43], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld43, &anynul, &istat);
    data[43] = hxd_ppr_evsel_pin_ld43;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD44], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld44, &anynul, &istat);
    data[44] = hxd_ppr_evsel_pin_ld44;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD45], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld45, &anynul, &istat);
    data[45] = hxd_ppr_evsel_pin_ld45;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD46], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld46, &anynul, &istat);
    data[46] = hxd_ppr_evsel_pin_ld46;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD47], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld47, &anynul, &istat);
    data[47] = hxd_ppr_evsel_pin_ld47;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD48], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld48, &anynul, &istat);
    data[48] = hxd_ppr_evsel_pin_ld48;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD49], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld49, &anynul, &istat);
    data[49] = hxd_ppr_evsel_pin_ld49;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD50], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld50, &anynul, &istat);
    data[50] = hxd_ppr_evsel_pin_ld50;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD51], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld51, &anynul, &istat);
    data[51] = hxd_ppr_evsel_pin_ld51;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD52], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld52, &anynul, &istat);
    data[52] = hxd_ppr_evsel_pin_ld52;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD53], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld53, &anynul, &istat);
    data[53] = hxd_ppr_evsel_pin_ld53;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD54], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld54, &anynul, &istat);
    data[54] = hxd_ppr_evsel_pin_ld54;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD55], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld55, &anynul, &istat);
    data[55] = hxd_ppr_evsel_pin_ld55;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD56], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld56, &anynul, &istat);
    data[56] = hxd_ppr_evsel_pin_ld56;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD57], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld57, &anynul, &istat);
    data[57] = hxd_ppr_evsel_pin_ld57;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD58], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld58, &anynul, &istat);
    data[58] = hxd_ppr_evsel_pin_ld58;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD59], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld59, &anynul, &istat);
    data[59] = hxd_ppr_evsel_pin_ld59;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD60], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld60, &anynul, &istat);
    data[60] = hxd_ppr_evsel_pin_ld60;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD61], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld61, &anynul, &istat);
    data[61] = hxd_ppr_evsel_pin_ld61;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD62], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld62, &anynul, &istat);
    data[62] = hxd_ppr_evsel_pin_ld62;
    fits_read_col_byt(fp, colnum[HXD_PPR_EVSEL_PIN_LD63], irow, firstelem,
                      nelements, nulval, &hxd_ppr_evsel_pin_ld63, &anynul, &istat);
    data[63] = hxd_ppr_evsel_pin_ld63;
    BnkfPutM ("HXD:PPR:EVSEL_PIN_LD", sizeof(int)*64, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_ppr_bst_interval0;
    unsigned char hxd_ppr_bst_interval1;
    unsigned char hxd_ppr_bst_interval2;
    unsigned char hxd_ppr_bst_interval3;
    fits_read_col_byt(fp, colnum[HXD_PPR_BST_INTERVAL0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_bst_interval0, &anynul, &istat);
    data[0] = hxd_ppr_bst_interval0;
    fits_read_col_byt(fp, colnum[HXD_PPR_BST_INTERVAL1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_bst_interval1, &anynul, &istat);
    data[1] = hxd_ppr_bst_interval1;
    fits_read_col_byt(fp, colnum[HXD_PPR_BST_INTERVAL2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_bst_interval2, &anynul, &istat);
    data[2] = hxd_ppr_bst_interval2;
    fits_read_col_byt(fp, colnum[HXD_PPR_BST_INTERVAL3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_bst_interval3, &anynul, &istat);
    data[3] = hxd_ppr_bst_interval3;
    BnkfPutM ("HXD:PPR:BST_INTERVAL", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_ppr_gb_trg_msk0;
    unsigned char hxd_ppr_gb_trg_msk1;
    unsigned char hxd_ppr_gb_trg_msk2;
    unsigned char hxd_ppr_gb_trg_msk3;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_TRG_MSK0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_trg_msk0, &anynul, &istat);
    data[0] = hxd_ppr_gb_trg_msk0;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_TRG_MSK1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_trg_msk1, &anynul, &istat);
    data[1] = hxd_ppr_gb_trg_msk1;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_TRG_MSK2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_trg_msk2, &anynul, &istat);
    data[2] = hxd_ppr_gb_trg_msk2;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_TRG_MSK3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_trg_msk3, &anynul, &istat);
    data[3] = hxd_ppr_gb_trg_msk3;
    BnkfPutM ("HXD:PPR:GB_TRG_MSK", sizeof(int)*4, data);
  }
  {
    unsigned int data[8];
    unsigned char nulval=1;
    unsigned char hxd_ppr_gb_ph_frm0;
    unsigned char hxd_ppr_gb_ph_frm1;
    unsigned char hxd_ppr_gb_ph_frm2;
    unsigned char hxd_ppr_gb_ph_frm3;
    unsigned char hxd_ppr_gb_ph_frm4;
    unsigned char hxd_ppr_gb_ph_frm5;
    unsigned char hxd_ppr_gb_ph_frm6;
    unsigned char hxd_ppr_gb_ph_frm7;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_FRM0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_frm0, &anynul, &istat);
    data[0] = hxd_ppr_gb_ph_frm0;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_FRM1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_frm1, &anynul, &istat);
    data[1] = hxd_ppr_gb_ph_frm1;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_FRM2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_frm2, &anynul, &istat);
    data[2] = hxd_ppr_gb_ph_frm2;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_FRM3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_frm3, &anynul, &istat);
    data[3] = hxd_ppr_gb_ph_frm3;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_FRM4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_frm4, &anynul, &istat);
    data[4] = hxd_ppr_gb_ph_frm4;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_FRM5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_frm5, &anynul, &istat);
    data[5] = hxd_ppr_gb_ph_frm5;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_FRM6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_frm6, &anynul, &istat);
    data[6] = hxd_ppr_gb_ph_frm6;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_FRM7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_frm7, &anynul, &istat);
    data[7] = hxd_ppr_gb_ph_frm7;
    BnkfPutM ("HXD:PPR:GB_PH_FRM", sizeof(int)*8, data);
  }
  {
    unsigned int data[8];
    unsigned char nulval=1;
    unsigned char hxd_ppr_gb_ph_to0;
    unsigned char hxd_ppr_gb_ph_to1;
    unsigned char hxd_ppr_gb_ph_to2;
    unsigned char hxd_ppr_gb_ph_to3;
    unsigned char hxd_ppr_gb_ph_to4;
    unsigned char hxd_ppr_gb_ph_to5;
    unsigned char hxd_ppr_gb_ph_to6;
    unsigned char hxd_ppr_gb_ph_to7;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_TO0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_to0, &anynul, &istat);
    data[0] = hxd_ppr_gb_ph_to0;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_TO1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_to1, &anynul, &istat);
    data[1] = hxd_ppr_gb_ph_to1;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_TO2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_to2, &anynul, &istat);
    data[2] = hxd_ppr_gb_ph_to2;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_TO3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_to3, &anynul, &istat);
    data[3] = hxd_ppr_gb_ph_to3;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_TO4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_to4, &anynul, &istat);
    data[4] = hxd_ppr_gb_ph_to4;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_TO5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_to5, &anynul, &istat);
    data[5] = hxd_ppr_gb_ph_to5;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_TO6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_to6, &anynul, &istat);
    data[6] = hxd_ppr_gb_ph_to6;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_TO7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_to7, &anynul, &istat);
    data[7] = hxd_ppr_gb_ph_to7;
    BnkfPutM ("HXD:PPR:GB_PH_TO", sizeof(int)*8, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_ppr_gb_want_sum0;
    unsigned char hxd_ppr_gb_want_sum1;
    unsigned char hxd_ppr_gb_want_sum2;
    unsigned char hxd_ppr_gb_want_sum3;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_WANT_SUM0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_want_sum0, &anynul, &istat);
    data[0] = hxd_ppr_gb_want_sum0;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_WANT_SUM1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_want_sum1, &anynul, &istat);
    data[1] = hxd_ppr_gb_want_sum1;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_WANT_SUM2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_want_sum2, &anynul, &istat);
    data[2] = hxd_ppr_gb_want_sum2;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_WANT_SUM3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_want_sum3, &anynul, &istat);
    data[3] = hxd_ppr_gb_want_sum3;
    BnkfPutM ("HXD:PPR:GB_WANT_SUM", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_ppr_gb_ph_flg0;
    unsigned char hxd_ppr_gb_ph_flg1;
    unsigned char hxd_ppr_gb_ph_flg2;
    unsigned char hxd_ppr_gb_ph_flg3;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_FLG0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_flg0, &anynul, &istat);
    data[0] = hxd_ppr_gb_ph_flg0;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_FLG1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_flg1, &anynul, &istat);
    data[1] = hxd_ppr_gb_ph_flg1;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_FLG2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_flg2, &anynul, &istat);
    data[2] = hxd_ppr_gb_ph_flg2;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_FLG3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_flg3, &anynul, &istat);
    data[3] = hxd_ppr_gb_ph_flg3;
    BnkfPutM ("HXD:PPR:GB_PH_FLG", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_ppr_gb_wld_flg0;
    unsigned char hxd_ppr_gb_wld_flg1;
    unsigned char hxd_ppr_gb_wld_flg2;
    unsigned char hxd_ppr_gb_wld_flg3;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_WLD_FLG0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_wld_flg0, &anynul, &istat);
    data[0] = hxd_ppr_gb_wld_flg0;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_WLD_FLG1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_wld_flg1, &anynul, &istat);
    data[1] = hxd_ppr_gb_wld_flg1;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_WLD_FLG2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_wld_flg2, &anynul, &istat);
    data[2] = hxd_ppr_gb_wld_flg2;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_WLD_FLG3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_wld_flg3, &anynul, &istat);
    data[3] = hxd_ppr_gb_wld_flg3;
    BnkfPutM ("HXD:PPR:GB_WLD_FLG", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_ppr_gb_pse_flg0;
    unsigned char hxd_ppr_gb_pse_flg1;
    unsigned char hxd_ppr_gb_pse_flg2;
    unsigned char hxd_ppr_gb_pse_flg3;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PSE_FLG0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_pse_flg0, &anynul, &istat);
    data[0] = hxd_ppr_gb_pse_flg0;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PSE_FLG1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_pse_flg1, &anynul, &istat);
    data[1] = hxd_ppr_gb_pse_flg1;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PSE_FLG2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_pse_flg2, &anynul, &istat);
    data[2] = hxd_ppr_gb_pse_flg2;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PSE_FLG3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_pse_flg3, &anynul, &istat);
    data[3] = hxd_ppr_gb_pse_flg3;
    BnkfPutM ("HXD:PPR:GB_PSE_FLG", sizeof(int)*4, data);
  }
  {
    unsigned int data[8];
    unsigned char nulval=1;
    unsigned char hxd_ppr_gb_itime_ph0;
    unsigned char hxd_ppr_gb_itime_ph1;
    unsigned char hxd_ppr_gb_itime_ph2;
    unsigned char hxd_ppr_gb_itime_ph3;
    unsigned char hxd_ppr_gb_itime_ph4;
    unsigned char hxd_ppr_gb_itime_ph5;
    unsigned char hxd_ppr_gb_itime_ph6;
    unsigned char hxd_ppr_gb_itime_ph7;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_ITIME_PH0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_itime_ph0, &anynul, &istat);
    data[0] = hxd_ppr_gb_itime_ph0;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_ITIME_PH1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_itime_ph1, &anynul, &istat);
    data[1] = hxd_ppr_gb_itime_ph1;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_ITIME_PH2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_itime_ph2, &anynul, &istat);
    data[2] = hxd_ppr_gb_itime_ph2;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_ITIME_PH3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_itime_ph3, &anynul, &istat);
    data[3] = hxd_ppr_gb_itime_ph3;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_ITIME_PH4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_itime_ph4, &anynul, &istat);
    data[4] = hxd_ppr_gb_itime_ph4;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_ITIME_PH5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_itime_ph5, &anynul, &istat);
    data[5] = hxd_ppr_gb_itime_ph5;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_ITIME_PH6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_itime_ph6, &anynul, &istat);
    data[6] = hxd_ppr_gb_itime_ph6;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_ITIME_PH7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_itime_ph7, &anynul, &istat);
    data[7] = hxd_ppr_gb_itime_ph7;
    BnkfPutM ("HXD:PPR:GB_ITIME_PH", sizeof(int)*8, data);
  }
  {
    unsigned int data[8];
    unsigned char nulval=1;
    unsigned char hxd_ppr_gb_level_ph0;
    unsigned char hxd_ppr_gb_level_ph1;
    unsigned char hxd_ppr_gb_level_ph2;
    unsigned char hxd_ppr_gb_level_ph3;
    unsigned char hxd_ppr_gb_level_ph4;
    unsigned char hxd_ppr_gb_level_ph5;
    unsigned char hxd_ppr_gb_level_ph6;
    unsigned char hxd_ppr_gb_level_ph7;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_LEVEL_PH0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_level_ph0, &anynul, &istat);
    data[0] = hxd_ppr_gb_level_ph0;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_LEVEL_PH1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_level_ph1, &anynul, &istat);
    data[1] = hxd_ppr_gb_level_ph1;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_LEVEL_PH2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_level_ph2, &anynul, &istat);
    data[2] = hxd_ppr_gb_level_ph2;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_LEVEL_PH3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_level_ph3, &anynul, &istat);
    data[3] = hxd_ppr_gb_level_ph3;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_LEVEL_PH4], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_level_ph4, &anynul, &istat);
    data[4] = hxd_ppr_gb_level_ph4;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_LEVEL_PH5], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_level_ph5, &anynul, &istat);
    data[5] = hxd_ppr_gb_level_ph5;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_LEVEL_PH6], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_level_ph6, &anynul, &istat);
    data[6] = hxd_ppr_gb_level_ph6;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_LEVEL_PH7], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_level_ph7, &anynul, &istat);
    data[7] = hxd_ppr_gb_level_ph7;
    BnkfPutM ("HXD:PPR:GB_LEBEL_PH", sizeof(int)*8, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_ppr_gb_itime_wld0;
    unsigned char hxd_ppr_gb_itime_wld1;
    unsigned char hxd_ppr_gb_itime_wld2;
    unsigned char hxd_ppr_gb_itime_wld3;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_ITIME_WLD0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_itime_wld0, &anynul, &istat);
    data[0] = hxd_ppr_gb_itime_wld0;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_ITIME_WLD1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_itime_wld1, &anynul, &istat);
    data[1] = hxd_ppr_gb_itime_wld1;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_ITIME_WLD2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_itime_wld2, &anynul, &istat);
    data[2] = hxd_ppr_gb_itime_wld2;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_ITIME_WLD3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_itime_wld3, &anynul, &istat);
    data[3] = hxd_ppr_gb_itime_wld3;
    BnkfPutM ("HXD:PPR:GB_ITIME_WLD", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_ppr_gb_level_wld0;
    unsigned char hxd_ppr_gb_level_wld1;
    unsigned char hxd_ppr_gb_level_wld2;
    unsigned char hxd_ppr_gb_level_wld3;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_LEVEL_WLD0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_level_wld0, &anynul, &istat);
    data[0] = hxd_ppr_gb_level_wld0;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_LEVEL_WLD1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_level_wld1, &anynul, &istat);
    data[1] = hxd_ppr_gb_level_wld1;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_LEVEL_WLD2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_level_wld2, &anynul, &istat);
    data[2] = hxd_ppr_gb_level_wld2;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_LEVEL_WLD3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_level_wld3, &anynul, &istat);
    data[3] = hxd_ppr_gb_level_wld3;
    BnkfPutM ("HXD:PPR:GB_LEBEL_WLD", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_ppr_gb_itime_pse0;
    unsigned char hxd_ppr_gb_itime_pse1;
    unsigned char hxd_ppr_gb_itime_pse2;
    unsigned char hxd_ppr_gb_itime_pse3;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_ITIME_PSE0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_itime_pse0, &anynul, &istat);
    data[0] = hxd_ppr_gb_itime_pse0;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_ITIME_PSE1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_itime_pse1, &anynul, &istat);
    data[1] = hxd_ppr_gb_itime_pse1;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_ITIME_PSE2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_itime_pse2, &anynul, &istat);
    data[2] = hxd_ppr_gb_itime_pse2;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_ITIME_PSE3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_itime_pse3, &anynul, &istat);
    data[3] = hxd_ppr_gb_itime_pse3;
    BnkfPutM ("HXD:PPR:GB_ITIME_PSE", sizeof(int)*4, data);
  }
  {
    unsigned int data[4];
    unsigned char nulval=1;
    unsigned char hxd_ppr_gb_level_pse0;
    unsigned char hxd_ppr_gb_level_pse1;
    unsigned char hxd_ppr_gb_level_pse2;
    unsigned char hxd_ppr_gb_level_pse3;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_LEVEL_PSE0], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_level_pse0, &anynul, &istat);
    data[0] = hxd_ppr_gb_level_pse0;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_LEVEL_PSE1], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_level_pse1, &anynul, &istat);
    data[1] = hxd_ppr_gb_level_pse1;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_LEVEL_PSE2], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_level_pse2, &anynul, &istat);
    data[2] = hxd_ppr_gb_level_pse2;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_LEVEL_PSE3], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_level_pse3, &anynul, &istat);
    data[3] = hxd_ppr_gb_level_pse3;
    BnkfPutM ("HXD:PPR:GB_LEBEL_PSE", sizeof(int)*4, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_ppr_gb_trg_mode;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_TRG_MODE], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_trg_mode, &anynul, &istat);
    data[0] = hxd_ppr_gb_trg_mode;
    BnkfPutM ("HXD:PPR:GB_TRG_MODE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_ppr_gb_ph_mode;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PH_MODE], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_ph_mode, &anynul, &istat);
    data[0] = hxd_ppr_gb_ph_mode;
    BnkfPutM ("HXD:PPR:GB_PH_MODE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_ppr_gb_wld_mode;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_WLD_MODE], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_wld_mode, &anynul, &istat);
    data[0] = hxd_ppr_gb_wld_mode;
    BnkfPutM ("HXD:PPR:GB_WLD_MODE", sizeof(int)*1, data);
  }
  {
    unsigned int data[1];
    unsigned char nulval=1;
    unsigned char hxd_ppr_gb_pse_mode;
    fits_read_col_byt(fp, colnum[HXD_PPR_GB_PSE_MODE], irow, firstelem,
                      nelements, nulval, &hxd_ppr_gb_pse_mode, &anynul, &istat);
    data[0] = hxd_ppr_gb_pse_mode;
    BnkfPutM ("HXD:PPR:GB_PSE_MODE", sizeof(int)*1, data);
  }
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_read_col failed (%d)\n",
	    pname, istat);
    return istat;
  }
  
  return ANL_OK;
  
}
