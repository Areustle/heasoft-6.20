/* -------------------------------------------------------------------------
 *   hxdTimeUtil.h                                         (version 0.2)
 * ------------------------------------------------------------------------*/
#ifndef _HXD_TIME_H_
#define _HXD_TIME_H_

/* ===================== BASIC DEFINITION ===================== */
#define WPU_UNIT_NUM 16
#define WPU_NBOARD    4
#define TPU_NBOARD    4
#define FIRST         1
#define NOT_FIRST     0
/* ========= Shift For ( transformation to 37 bit counter ) ===== */
#define HXD_TIME_COUNTER_015_SHIFT          0
#define HXD_TIME_COUNTER_030_SHIFT          1
#define HXD_TIME_COUNTER_061_SHIFT          2
#define HXD_TIME_COUNTER_122_SHIFT          3
#define HXD_TIME_COUNTER_244_SHIFT          4
#define HXD_TIME_COUNTER_15m_SHIFT          10
#define HXD_TIME_COUNTER_Sec_SHIFT          16
#define HXD_TIME_COUNTER_Up5_SHIFT          31

#define HXD_TIME_INVALID_AETIME 0x00000

/**************************************************************************
 *************************  WEL TIME DETERMINATIN *************************
 **************************************************************************/
/* ===================== WPU CLOCK RATE ======================= */
#define HXD_WPU_CLOCK_RATE_NORMAL          0
#define HXD_WPU_CLOCK_RATE_FINE            1
#define HXD_WPU_CLOCK_RATE_SUPER_FINE      2
#define HXD_WPU_CLOCK_RATE_SUPER_FINE2     3
#define HXD_WPU_CLOCK_RATE_NORMAL_OLD      10
#define HXD_WPU_CLOCK_RATE_FINE_OLD        11
#define HXD_WPU_CLOCK_RATE_SUPER_FINE_OLD  12
#define HXD_WPU_CLOCK_RATE_SUPER_FINE2_OLD 13
/* ============= 37 bit counter WPU counter MASK ================= */
#define HXD_WPU_CLK_NORMAL_MASK   0x003ffff8
#define HXD_WPU_CLK_FINE_MASK     0x001ffffc
#define HXD_WPU_CLK_S_FINE_MASK   0x000ffffe
#define HXD_WPU_CLK_SS_FINE_MASK  0x0007ffff

/* ============= 37 bit counter SCL time counter MASK ================= */
#define HXD_SCL_CLK_NORMAL_MASK   0x03fffffc
#define HXD_SCL_CLK_FINE_MASK     0x01fffffe
#define HXD_SCL_CLK_S_FINE_MASK   0x00ffffff
#define HXD_SCL_CLK_SS_FINE_MASK  0x007fffff

/* =============== FULL Range for WPU COUNTER ================== */
/* how many counts up in 7.65 us from WPU_counter_reset to reset */
#define HXD_WPU_CLK_NORMAL_FULL_TIME_COUNT  0x400000
#define HXD_WPU_CLK_FINE_FULL_TIME_COUNT    0x200000
#define HXD_WPU_CLK_S_FINE_FULL_TIME_COUNT  0x100000
#define HXD_WPU_CLK_SS_FINE_FULL_TIME_COUNT 0x080000
#define HXD_WPU_TIME_FULL_BIT_NUMBER        19

/* =============== FULL Range for SCL TIME COUNTER ================== */
/* how many counts up in 7.65 us from WPU_counter_reset to reset */
#define HXD_SCL_CLK_NORMAL_FULL_TIME_COUNT  0x4000000
#define HXD_SCL_CLK_FINE_FULL_TIME_COUNT    0x2000000
#define HXD_SCL_CLK_S_FINE_FULL_TIME_COUNT  0x1000000
#define HXD_SCL_CLK_SS_FINE_FULL_TIME_COUNT 0x0800000
#define HXD_SCL_TIME_FULL_BIT_NUMBER        24

/**************************************************************************
 *************************  TRN TIME DETERMINATIN *************************
 **************************************************************************/
#define HXD_TPU_TIME_MODE0        0
#define HXD_TPU_TIME_MODE_DEFAULT 1
#define HXD_TPU_TIME_MODE2        2
#define HXD_TPU_TIME_MODE3        3

#define TIME_STEP_05SEC   0x00008000
#define TIME_STEP_1SEC    0x00010000
#define TIME_STEP_2SEC    0x00020000
#define TIME_STEP_4SEC    0x00040000

/**************************************************************************
 *************************  BST TIME DETERMINATIN *************************
 **************************************************************************/
#define BST_EXTENDED_NO      0
#define BST_EXTENDED_DONE    1


#define ASTE_DP_BASE_TICK        (1.0/65536.0)
#define HXD_ZERO_CORRECTION_TIME (1.0/524288.0)

#define TIME_DEBUG 0
#endif

