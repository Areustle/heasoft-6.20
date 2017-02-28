/*
 *   HXDpiRPT for RPT
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "fitsio.h"
#include "hxd/HXD.h"

#include "hxdpiUtil.h"

char HXDpiRPT_version[] = "version 2.0.1";

static char pname[] = "HXDpiRPT";

static char *ae_fast_table_file = "linearity_table_def/AE_fast.tbl";
static char *ae_slow_table_file = "linearity_table_def/AE_slow.tbl";
static char *ae_pin_table_file = "linearity_table_def/AE_pin.tbl";
static char *s_fast_table_file = "linearity_table_def/S_fast.tbl";
static char *s_slow_table_file = "linearity_table_def/S_slow.tbl";
static char *s_pin_table_file = "linearity_table_def/S_pin.tbl";
static char *gain1_fast_table_file = "";
static char *gain1_slow_table_file = "";
static char *gain1_pin_table_file = "";

void
HXDpiRPT_startup(int *status)
{
    *status = ANL_OK;
}

void
HXDpiRPT_com(int *status)
{
    *status = ANL_OK;
}

void
HXDpiRPT_init(int *status)
{	
    *status = ANL_OK;
}

void
HXDpiRPT_his(int *status)
{
    *status = ANL_OK;
}

void
HXDpiRPT_bgnrun(int *status)
{
    
    int istat=0;
    
    hxdpi_AE_correct_Init(ae_fast_table_file, ae_slow_table_file,
			  ae_pin_table_file, &istat );
    if( istat ){
	*status=ANL_QUIT;
	return;
    }
    
    hxdpi_Gain_each_Init(gain1_fast_table_file, gain1_slow_table_file,
			 gain1_pin_table_file, &istat );
    if( istat ){
      *status=ANL_QUIT;
      return;
    }
    
    /*
    hxdpi_Gain_total_Init(gain2_fast_table_file, gain2_slow_table_file,
			  gain2_pin_table_file, &istat );
    if( istat ){
      *status=ANL_QUIT;
      return;
    }
    */
    
    hxdpi_S_correct_Init(s_fast_table_file, s_slow_table_file,
			 s_pin_table_file, &istat );
    if( istat ){
      *status=ANL_QUIT;
      return;
    }
    
    *status = ANL_OK;
    
}

void
HXDpiRPT_ana(int nevent, int eventid, int *status)
{
    *status = ANL_OK;
}

void
HXDpiRPT_endrun(int *status)
{    
    *status = ANL_OK;
}

void
HXDpiRPT_exit(int *status)
{
    *status = ANL_OK;
}
