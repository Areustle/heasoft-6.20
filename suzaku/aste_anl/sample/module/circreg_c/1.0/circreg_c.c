/************************************************************************/
/*
/* circreg_c.c
/*     filter module to select events with circular region on its image
/*     with DETX/Y as a reference
/*
/*     94/06/26 Created by M.Hirayama
/*
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "cfortran.h"
#include "hbook.h"
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "anl_misc.h"

/* static variables for setting of image regions */
static float center_x[4]={640.5, 640.5, 128.5, 128.5};/* center of the region */
static float center_y[4]={640.5, 640.5, 128.5, 128.5};
static float radius[4]={1024.0, 1024.0, 120.0, 120.0};/* radius of the region */

void
circreg_init(int *status)
{
  EVSDEF("CIRCREG:BEGIN");
  EVSDEF("CIRCREG:ENTRY");
  EVSDEF("CIRCREG:OK");
  ANL_PUT_VERSION("CIRCREG", "version 1.0");

  *status = ASCA_ANL_OK;
}

void
circreg_com(int *status)
{
  int sensor=0;

  while(1) {
    /* title */
    printf("\n*** Circular Region ***\n\n");
	
    /* ask user for sensor ID */
    INTRD("Sensor to set (-1=end of set)", sensor);
    if ((sensor<0) || (sensor>3)) break;
	
    /* ask user for center and radius of selection region */
    FLTRD("DETX of Center (pixel)", center_x[sensor]);
    FLTRD("DETY of Center (pixel)", center_y[sensor]);
    FLTRD("Radius (pixel)", radius[sensor]);
  }
  *status = ASCA_ANL_OK;
}

void
circreg_his(int *status)
{
  *status = ASCA_ANL_OK;
}

void
circreg_bgnrun(int *status)

{
  EVSSET("CIRCREG:BEGIN");

  *status = ASCA_ANL_OK;
}

void
circreg_ana(int nevent, int eventid, int *status)
{
  int sensor;
  int detx, dety;
  float r, dx, dy;
  int size;

  EVSSET("CIRCREG:ENTRY");

/* select OBS events */
  if ((!EVS("SIS:SIS0 Event")) && (!EVS("SIS:SIS1 Event"))
      && (!EVS("GIS:GIS2 Event")) && (!EVS("GIS:GIS3 Event"))) {
    EVSSET("CIRCREG:OK");
    *status = ASCA_ANL_OK;
    return;
  }

/* BNKGET */
  BNKGET("ANL:SENSOR", 4, size, &sensor);
  BNKGET("ANL:DETX", 4, size, &detx);
  BNKGET("ANL:DETY", 4, size, &dety);

/* check sensor ID */
  if ((sensor<0) || (sensor>3)) {
    EVSSET("CIRCREG:OK");
    *status = ASCA_ANL_OK;
    return;
  }

/* calculate distance from region center */
  dx = (float)detx - center_x[sensor];
  dy = (float)dety - center_y[sensor];
  r = sqrt(dx*dx + dy*dy);

/* check if the event is in or out of specified circle */
  if (r>radius[sensor]) {
    /* skip the event */
    *status = ASCA_ANL_SKIP;
  } else {
    /* normal return */
    EVSSET("CIRCREG:OK");
    *status = ASCA_ANL_OK;
  }
}

void
circreg_endrun(int *status)
{
  *status = ASCA_ANL_OK;
}

void
circreg_exit(int *status)
{
  *status = ASCA_ANL_OK;
}

/*-------------------
   C wrapper for circreg
-------------------*/

FCALLSCSUB1(circreg_init,CIRCREG_INIT,circreg_init,PINT)
FCALLSCSUB1(circreg_com,CIRCREG_COM,circreg_com,PINT)
FCALLSCSUB1(circreg_his,CIRCREG_HIS,circreg_his,PINT)
FCALLSCSUB1(circreg_bgnrun,CIRCREG_BGNRUN,circreg_bgnrun,PINT)
FCALLSCSUB3(circreg_ana,CIRCREG_ANA,circreg_ana,INT,INT,PINT)
FCALLSCSUB1(circreg_endrun,CIRCREG_ENDRUN,circreg_endrun,PINT)
FCALLSCSUB1(circreg_exit,CIRCREG_EXIT,circreg_exit,PINT)
