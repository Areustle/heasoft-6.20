/* HXD.h             FM version                               */
/*          modified 1998.4.21. Terada                        */
/*          last modified 1998.5.26. Terada                   */
/*          last modified 1998.5.30. Kazu N                   */
/*          last modified 1998.6.04. Kazu N                   */
/*          last modified 1998.6.11. Kazu N                   */
/*          last modified 1998.6.15. Kazu N                   */
/*          version 1.8.0 1kami version linked to 1.6.4.Bnkf  */
/*          version 1.7.* PI develop version                  */
/*          version 1.8.* QL develop version                  */
/*          version 1.8.0 -> 1.8.1                            */
/*            HISTGRAM packet(0x12B-0x12E),                   */
/*            DUMP packet(0x139-0x13E except 0x13B ,0x338)    */
/*            RHK packet(0x530) new                           */
/*            SFH -> HISH                                     */
/*          version 1.8.1 -> 1.8.2                            */
/*            APR -> STM                                      */
/*            used constant number  commented "used"          */ 
/*          version 1.8.2 -> 1.9.0                            */
/*	      only HXD_PST_LENGTH HXD_PPPR_LENGTH changed     */
/*          version 1.9.0 -> 1.9.1                            */
/*            HXD_SP_PMT_PKT_LENGTH,HXD_SP_PIN_PKT_LENGTH,    */
/*            HXD_DLT_PKT_LENGTH  2024 (bug!!) -> 2048        */
/*          version 1.9.1 -> 1.9.2                            */
/*            HXD_HISH_LENGTH 12 -> 16                        */	    
/*          version 1.9.2 -> 1.9.3                            */
/*            only HXD_PST_LENGTH HXD_PPPR_LENGTH changed     */
/*          version 1.9.3 -> 1.9.4                            */
/*            add #define HXD_APID_AET  0x013B                */
/*            add #define HXD_EVTYP_AET HXD_APID_AET          */
/*            support since HXDviewer version 1.8.3           */
/*          version 2.0.0                                     */
/*            support PI program version 0.9.8                */
/*          version 2.1.2                                     */
/*            support PI program version 0.9.9.8              */
/*          version 2.1.3                                     */
/*            change BnkPut BnkGet                            */
/*          version 2.1.4                                     */
/*            add HXD_UPDATE_***                              */
/*          version 2.1.5                                     */
/*            add uclpario.h for ftools                       */
/*          version 2.1.6                                     */
/*            support HCE packet for Thermal Vaccumm test     */
/*          version 3.0.0                                     */
/*            HXD-I                                           */
/*          version 3.0.1                                     */
/*            HXD-II                                          */
/*           change Sys APID, Sys HK length                   */
/*           add PWH length                                   */
/*          version 3.0.2, 2003-01-24 Terada                  */
/*           new PPR size for PI prog v1.1.1                  */
/*          version 3.0.3, 2003-02-22 Terada                  */
/*           new BST size for PI prog v1.1.2                  */
/*          version 3.0.4, 2003-03-19 Terada                  */
/*           new PPR size for PI prog v1.1.3                  */
/*          version 3.0.5, 2003-04-11 Terada                  */
/*           new PPR size for PI prog v1.1.7                  */
/*          version 3.0.6, 2003-05-02 Terada                  */
/*           add PWH update                                   */

#ifndef HXD_H_USE_OBSOLUTE
#define HXD_H_USE_OBSOLUTE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

#ifdef BnkPut
#undef BnkPut
#endif
#define BnkPut BnkfPutM

#ifdef BnkGet
#undef BnkGet
#endif
#define BnkGet  BnkfGetM


#define PI_HEADER_SIZE    4 
#define CCSDS_HEADER_SIZE 10
#define CCSDS_MAX_LENGTH  0x4000

#define HXD_HK_LENGTH_PM  (270-CCSDS_HEADER_SIZE)
#define HXD_HK_LENGTH_FM  (222-CCSDS_HEADER_SIZE)
#define HXD_HK_LENGTH_PM2  (498-CCSDS_HEADER_SIZE)     /* used */
#define HXD_HK_LENGTH HXD_HK_LENGTH_PM2
#define HXD_RHK_LENGTH    (45 -CCSDS_HEADER_SIZE)
#define HXD_SYS_LENGTH_PM (124-CCSDS_HEADER_SIZE)   
#define HXD_SYS_LENGTH_FM (160-CCSDS_HEADER_SIZE)      
#define HXD_SYS_LENGTH_PM2 (166-CCSDS_HEADER_SIZE)     /* used */
#define HXD_SYS_LENGTH HXD_SYS_LENGTH_PM2

#define HXD_PSM_PST_LENGTH_094 804     /* PI version 0.9.4 */
#define HXD_PSM_PPR_LENGTH_094 852
#define HXD_PSM_PST_LENGTH_0947 800    /* PI version 0.9.4-07*/
#define HXD_PSM_PPR_LENGTH_0947 776 
#define HXD_PSM_PST_LENGTH_095 804     /* PI version 0.9.5 */
#define HXD_PSM_PPR_LENGTH_095 776 
#define HXD_PSM_PST_LENGTH_096 868     /* PI version 0.9.6 */
#define HXD_PSM_PPR_LENGTH_096 776 
#define HXD_PSM_PST_LENGTH_096_6 868   /* PI version 0.9.6 rev.6 */
#define HXD_PSM_PPR_LENGTH_096_6 740 
#define HXD_PSM_PST_LENGTH_096_9 872   /* PI version 0.9.6 rev.9 */
#define HXD_PSM_PPR_LENGTH_096_9 744 
#define HXD_PSM_PST_LENGTH_096_13 868  /* PI version 0.9.6 rev.13 */
#define HXD_PSM_PPR_LENGTH_096_13 732
#define HXD_PSM_PST_LENGTH_1kami 788                   
#define HXD_PSM_PPR_LENGTH_1kami 656   /* 656 (or 848@2box) + 4 ?? */
#define HXD_PSM_PST_LENGTH_093 792     /* PI version 0.9.3 */
#define HXD_PSM_PPR_LENGTH_093 660
#define HXD_PSM_PST_LENGTH_096_16 868  /* PI version 0.9.6 rev.16 */
#define HXD_PSM_PPR_LENGTH_096_16 924 
#define HXD_PSM_PST_LENGTH_098_01 992  /* PI version 0.9.8 rev.01 */
#define HXD_PSM_PPR_LENGTH_098_01 920 
#define HXD_PSM_PST_LENGTH_099_04 1008  /* PI version 0.9.9 rev.04 */
#define HXD_PSM_PPR_LENGTH_099_04 984
#define HXD_PSM_PST_LENGTH_099_08 1000  /* PI version 0.9.9 rev.08 */
#define HXD_PSM_PPR_LENGTH_099_08 984
#define HXD_PSM_PPR_LENGTH_111_00 988   /* PI version 1.1.0 */
#define HXD_PSM_PPR_LENGTH_113_00 996   /* PI version 1.1.3 */
#define HXD_PSM_PST_LENGTH_117_00 1008  /* PI version 0.9.9 rev.08 */
#define HXD_PST_LENGTH HXD_PSM_PST_LENGTH_117_00
#define HXD_PPR_LENGTH HXD_PSM_PPR_LENGTH_113_00

#define HXD_APR_LENGTH 880
#define HXD_STM_LENGTH HXD_APR_LENGTH /* APR -> STM */ /* used */
#define HXD_WEL_LENGTH  16                             /* used */
#define HXD_WE0_LENGTH  HXD_WEL_LENGTH                 /* used */
#define HXD_WE1_LENGTH  HXD_WEL_LENGTH                 /* used */
#define HXD_WE2_LENGTH  HXD_WEL_LENGTH                 /* used */
#define HXD_WE3_LENGTH  HXD_WEL_LENGTH                 /* used */
#define HXD_PWH_LENGTH  32                             /* used */
#define HXD_SCL_LENGTH_PM  41
#define HXD_SCL_LENGTH_FM  49
#define HXD_SCL_LENGTH HXD_SCL_LENGTH_FM               /* used */
#define HXD_BST_LENGTH_HXD 512  
#define HXD_BST_LENGTH_HXDII 400
#define HXD_BST_LENGTH_ERR 512                         /* used */
#define HXD_BST_LENGTH_PACKET 1024                     /* used */
#define HXD_BST_LENGTH HXD_BST_LENGTH_HXDII            /* used */
#define HXD_ACU_LENGTH_PM  47                          
#define HXD_ACU_LENGTH_FM  51
#define HXD_ACU_LENGTH  HXD_ACU_LENGTH_FM              /* used */
#define HXD_TRH_LENGTH  8                              /* used */
#define HXD_TRB_LENGTH  136                            /* used */
#define HXD_TRN_LENGTH (HXD_TRB_LENGTH+HXD_TRH_LENGTH) /* used */
#define HXD_HCE_LENGTH  426                            /* used */

#if HXD_H_USE_OBSOLUTE
#define HXD_TRN_HEADER_LENGTH  HXD_TRH_LENGTH /* obsolute */
#define HXD_TRN_BODY_LENGTH HXD_TRB_LENGTH    /* obsolute */
#endif

#define HXD_SFH_LENGTH     12   /* 16 - PI_HEADER_LENGTH */
/* SFH is obsolute from ver 1.8.1 */
#define HXD_HISH_LENGTH    16                          /* used */
/* PI flight version */
#define HXD_SFC_PKT_LENGTH     2048 /*(2*16*16*16/4)*/ /* used */
#define HXD_SFC_LENGTH  512         /* (2*64*64) */    /* used */
#define HXD_SFF_QRT_LENGTH     2048 /* (2*64*64/4) */
#define HXD_SFF_LENGTH  8192        /* (2*64*64)   */
#define HXD_SFF1_QRT_LENGTH HXD_SFF_QRT_LENGTH         /* used */
#define HXD_SFF2_QRT_LENGTH HXD_SFF_QRT_LENGTH         /* used */
#define HXD_SFF1_LENGTH HXD_SFF_LENGTH                 /* used */
#define HXD_SFF2_LENGTH HXD_SFF_LENGTH                 /* used */
#define HXD_DLT_LENGTH        512  /* 2*256 */         /* used */
#define HXD_DLT_PKT_LENGTH    2048 /* 2*256*16/4 */    /* used */
#define HXD_SP_PMT_LENGTH     512  /* 2*256 */         /* used */
#define HXD_SP_PMT_PKT_LENGTH 2048 /* 2*256*16/4 */    /* used */
#define HXD_SP_PIN_LENGTH     512  /* 2*64*4 */        /* used */
#define HXD_SP_PIN_PKT_LENGTH 2048 /* 2*64*64/4 */     /* used */

#define HXD_DHU_LENGTH 426                             /* used */
/* 436 - 10 (header)   DP test 11/1 */ 

#define HXD_IO_DMP_LENGTH   (15 - CCSDS_HEADER_SIZE)   /* used */
#define HXD_ECC_DMP_LENGTH  (19 - CCSDS_HEADER_SIZE)   /* used */
#define HXD_AET_HC_LENGTH   47                         /* used */
#define HXD_AET_SC_LENGTH   674                        /* used */
#define HXD_RIO_DMP_LENGTH  HXD_IO_DMP_LENGTH          /* used */
#define HXD_RECC_DMP_LENGTH HXD_ECC_DMP_LENGTH         /* used */
#define HXD_MEM_DMP_LENGTH (1551 - CCSDS_HEADER_SIZE)  /* used */
#define HXD_MEM_DMP_HEADER_LENGTH 5                    /* used */

/*
 * Board id is hard corded anywhere in code. 
 */

#define HXD_WPU0_BOARDID 0
#define HXD_WPU1_BOARDID 1
#define HXD_WPU2_BOARDID 2
#define HXD_WPU3_BOARDID 3
#define HXD_TPU0_BOARDID 4
#define HXD_TPU1_BOARDID 5
#define HXD_TPU2_BOARDID 6
#define HXD_TPU3_BOARDID 7
#define HXD_ACU_BOARDID  8

#define HXD_GSE_BOARDID  9
#define HXD_NULL_BOARDID 10

/*
 * APID
 */

#define HXD_APID_DHU_PM 0x428             /*  DP test 11/19 */
#define HXD_APID_DHU_FM 0x220
/* #define HXD_APID_DHU HXD_APID_DHU_FM */
#define HXD_APID_DHU HXD_APID_DHU_PM      /* 1998/09/23 FM 1kami*/
#define HXD_APID_HK  0x520                               /* used */
#define HXD_APID_RHK 0x530                               /* used */

/*#define HXD_APID_SYS 0x121*/
#define HXD_APID_SYS 0x521                               /* used */
#define HXD_APID_TRN 0x122                               /* used */
#define HXD_APID_ACU 0x123                               /* used */
#define HXD_APID_HIS HXD_APID_ACU
#define HXD_APID_SCL 0x124                               /* used */
#define HXD_APID_PPS 0x125                               /* used */
/* new!! PI program status packet */
#define HXD_APID_BST 0x126                               /* used */
#define HXD_APID_SFC 0x129                               /* used */
#define HXD_APID_SFF 0x12A                
#define HXD_APID_SFF1 HXD_APID_SFF                       /* used */
#define HXD_APID_SFF2 0x12B                              /* used */
#define HXD_APID_DLT  0x12C                              /* used */
#define HXD_APID_SP_PMT 0x12D                            /* used */
#define HXD_APID_SP_PIN 0x12E                            /* used */

#define HXD_APID_WEL    0x128        /* obsolete */
#define HXD_APID_WEL0 	0x130                            /* used */
/* APID_WEL 0x130 - 0x133  *//*         FM version      */
#define HXD_APID_WEL1 	0x131                            /* used */
#define HXD_APID_WEL2 	0x132                            /* used */
#define HXD_APID_WEL3 	0x133                            /* used */
#define HXD_APID_IO_DMP   0x139                          /* used */
#define HXD_APID_ECC_DMP  0x13A                          /* used */
#define HXD_APID_AET      0x13B                          /* used */
#define HXD_APID_AE_TABLE HXD_APID_AET
#define HXD_APID_RIO_DMP  0x13D                          /* used */
#define HXD_APID_RECC_DMP 0x13E                          /* used */
#define HXD_APID_MEM_DMP  0x338                          /* used */
#define HXD_APID_HCE      0x430                          /* for TV9909 */

/*
 * EVTYP
 */

#define HXD_EVTYP_HK  HXD_APID_HK                        /* used */
/* HK -> HXD:HK:* in bnk def.*/
#define HXD_EVTYP_RHK  HXD_APID_RHK                      /* used */
#define HXD_EVTYP_SYS HXD_APID_SYS                       /* used */

#define HXD_EVTYP_TRH  0x1221                            /* used */
#define HXD_EVTYP_TRB  0x1222                            /* used */
#define HXD_EVTYP_TRN HXD_APID_TRN                       /* used */
/* Trn AE_DE_Length error */
#define HXD_EVTYP_ACU HXD_APID_ACU                       /* used */
#define HXD_EVTYP_SCL HXD_APID_SCL                       /* used */
#define HXD_EVTYP_BST HXD_APID_BST                       /* used */

/* sons of PPS packet APID 0x125 */
/*#define HXD_EVTYP_STM 0x2500               dummy, no exist */
#define HXD_EVTYP_APR 0x2505               /* dummy, no exist */
#define HXD_EVTYP_STM HXD_EVTYP_APR                      /* used */
#define HXD_EVTYP_PSM 0x2510               /* dummy, no exist */
#define HXD_EVTYP_PST 0x2511                             /* used */
#define HXD_EVTYP_PPR 0x2512                             /* used */

#define HXD_EVTYP_WEL HXD_APID_WEL                       /* used */
#define HXD_EVTYP_WE0 HXD_APID_WEL0                      /* used */
/* WELL evnt :FM */
#define HXD_EVTYP_WE1 HXD_APID_WEL1                      /* used */
#define HXD_EVTYP_WE2 HXD_APID_WEL2                      /* used */
#define HXD_EVTYP_WE3 HXD_APID_WEL3                      /* used */

#define HXD_EVTYP_DHU HXD_APID_DHU                       /* used */
/*  DP test 11/1 */ 
#define HXD_EVTYP_HCE HXD_APID_HCE                       /* used */
/*  TV test 1999.09 */

#define HXD_EVTYP_SFH   0x1290       /* SFH is obsolute from ver 1.8.1 */
#define HXD_EVTYP_HISH  HXD_EVTYP_SFH                    /* used */
#define HXD_EVTYP_SFC   HXD_APID_SFC                     /* used */
#define HXD_EVTYP_SFF   HXD_APID_SFF       /* old version compatibility */
#define HXD_EVTYP_SFF1  HXD_APID_SFF1                    /* used */
#define HXD_EVTYP_SFF2  HXD_APID_SFF2                    /* used */
#define HXD_EVTYP_DLT   HXD_APID_DLT                     /* used */
#define HXD_EVTYP_SP_PMT HXD_APID_SP_PMT                 /* used */
#define HXD_EVTYP_SP_PIN HXD_APID_SP_PIN                 /* used */

#define HXD_EVTYP_IO_DMP    HXD_APID_IO_DMP              /* used */
#define HXD_EVTYP_ECC_DMP   HXD_APID_ECC_DMP             /* used */

#define HXD_EVTYP_AET       HXD_APID_AET                 /* used */

#define HXD_EVTYP_AET_HC 0x13B1                          /* used */
#define HXD_EVTYP_AET_SC 0x13B2                          /* used */
#define HXD_EVTYP_RIO_DMP   HXD_APID_RIO_DMP             /* used */
#define HXD_EVTYP_RECC_DMP  HXD_APID_RECC_DMP            /* used */

#define HXD_EVTYP_MEM_DMP   HXD_APID_MEM_DMP             /* used */

/*
UPDATE FLAGS
*/

#define HXD_UPDATE_HK          0x01
#define HXD_UPDATE_RHK         0x02
#define HXD_UPDATE_SYS         0x04
#define HXD_UPDATE_ACU         0x08
#define HXD_UPDATE_SCL         0x10
#define HXD_UPDATE_STM         0x20
#define HXD_UPDATE_PST         0x40
#define HXD_UPDATE_PPR         0x80
#define HXD_UPDATE_AET_HC      0x0100
#define HXD_UPDATE_AET_SC      0x0200
#define HXD_UPDATE_SFC         0x0400
#define HXD_UPDATE_SFF1        0x0800
#define HXD_UPDATE_SFF2        0x1000
#define HXD_UPDATE_DLT         0x2000
#define HXD_UPDATE_SP_PMT      0x4000
#define HXD_UPDATE_SP_PIN      0x8000
#define HXD_UPDATE_IO_DMP      0x010000
#define HXD_UPDATE_ECC_DMP     0x020000
#define HXD_UPDATE_RIO_DMP     0x040000
#define HXD_UPDATE_RECC_DMP    0x080000
#define HXD_UPDATE_MEM_DMP     0x100000
#define HXD_UPDATE_WEL         0x200000
#define HXD_UPDATE_TRN         0x400000
#define HXD_UPDATE_BST         0x800000
#define HXD_UPDATE_PWH         0x1000000

/*
STM PPS_STATUSID
*/

#define HXD_PI_PPS_STM_ALL_ID 0                           /* used */
#define HXD_PI_PPS_STM_MEM_ID 1                           /* used */
#define HXD_PI_PPS_STM_OBS_ID 2                           /* used */
#define HXD_PI_PPS_STM_TLM_ID 3                           /* used */
#define HXD_PI_PPS_STM_DST_ID 4                           /* used */
#define HXD_PI_PPS_STM_APR_ID 5                           /* used */
#define HXD_PI_PPS_STM_TLCHT_ID 6                         /* used */
#define HXD_PI_PPS_STM_PIAEC_ID 7                         /* used */

/*
 * Data Structure
 *
 */

#define HXD_PWH_DATANUM 16                             /* used */
							  
#define HXD_WEL_BOARDID_POS  0            /* FM version :                */
#define HXD_WEL_BOARDID_BIT  7            /*       WEL_BOARD_ID not used */
#define HXD_WEL_RANDPOS      14            /*   RANDPOS..can't understand */

#define HXD_SCL_BOARDID_POS_PM 40
#define HXD_SCL_BOARDID_POS_FM 48
#define HXD_SCL_BOARDID_POS HXD_SCL_BOARDID_POS_FM
#define HXD_SCL_BOARDID_BIT  1
#define HXD_SCL_RANDPOS      0            /*   RANDPOS..can't understand */

#define HXD_TRN_BOARDID_POS  6
#define HXD_TRN_BOARDID_BIT  1
#define HXD_TRN_RANDPOS      0            /*   RANDPOS..can't understand */

#define HXD_BST_BOARDID_POS  0
#define HXD_BST_BOARDID_BIT  7
#define HXD_BST_RANDPOS      2            /*   RANDPOS..can't understand */

#define HXD_ACU_RANDPOS      0            /*   RANDPOS..can't understand */

#define HXD_TRN_BLOCKCOUNT_POS  7
#define HXD_TRN_BLOCKCOUNT_MASK 0x07      /*   HXD_TRN_BLOCKCOUNT_BIT 2  */

#define HXD_TRN_GBFRZ_POS    6
#define HXD_TRN_GBFRZ_MASK   0x10         /*   HXD_TRN_GBFRZ_BIT 4       */

#define HXD_TRN_GBFLG_POS    3
#define HXD_TRN_GBFLG_MASK   0x01         /*   HXD_TRN_GBFLG_POS 0       */

#define HXD_ACU_ONOFF_POS_PM    0
#define HXD_ACU_ONOFF_POS_FM    5
#define HXD_ACU_ONOFF_POS HXD_ACU_ONOFF_POS_FM
#define HXD_ACU_ONOFF_MASK   0xff         /*   HXD_ACU_ONOFF_POS_BIT 0-7 */

#define HXD_SYS_DMPINF_POS_PM   (120-CCSDS_HEADER_SIZE)/* ???? where ????*/
#define HXD_SYS_DMPINF_POS_FM   (120-CCSDS_HEADER_SIZE)
#define HXD_SYS_DMPINF_POS HXD_SYS_DMPINF_POS_FM
#define HXD_SYS_TBLTYP_POS_PM   (121-CCSDS_HEADER_SIZE)
#define HXD_SYS_TBLTYP_POS_FM   (121-CCSDS_HEADER_SIZE)
#define HXD_SYS_TBLTYP_POS HXD_SYS_TBLTYP_POS_FM
#define HXD_SYS_TBLNO_POS_PM    (122-CCSDS_HEADER_SIZE)
#define HXD_SYS_TBLNO_POS_FM    (122-CCSDS_HEADER_SIZE)
#define HXD_SYS_TBLNO_POS HXD_SYS_TBLNO_POS_FM
#define HXD_SYS_TBLLEN_POS_PM   (123-CCSDS_HEADER_SIZE)
#define HXD_SYS_TBLLEN_POS_FM   (123-CCSDS_HEADER_SIZE)
#define HXD_SYS_TBLLEN_POS HXD_SYS_TBLLEN_POS_FM
#define HXD_SYS_DMPPDT_POS_PM   (124-CCSDS_HEADER_SIZE)
#define HXD_SYS_DMPPDT_POS_FM   (124-CCSDS_HEADER_SIZE)
#define HXD_SYS_DMPPDT_POS HXD_SYS_DMPPDT_POS_FM


#define HXD_SYS_DMPINF_NONE  0x00
#define HXD_SYS_DMPINF_TABLE 0x10
#define HXD_SYS_TBLTYP_HARD  1
#define HXD_SYS_TBLTYP_SOFT  2

/* ??? ??? */
#define HXD_AEP_APH_POS  (-CCSDS_HEADER_SIZE+0x8e)
#define HXD_AEP_APS_POS  (-CCSDS_HEADER_SIZE+0x97)
#define HXD_AEP_PST_POS  (-CCSDS_HEADER_SIZE+0x2d2)
#define HXD_AEP_PIS_POS  HXD_AEP_PST_POS  
/* obsolete */

#define BCMAX 12

/* HK Status packet */
#define HXD_DEST_IDLE 1
#define HXD_DEST_OBS  4
