/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/ofaintdfe/omain.c,v 3.8 1999/07/19 16:36:21 peachey Exp $   */
/*                   */
/* main program for sisftool   by K.Mitsuda */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "ocfitsis5_41.h" 


void ofaintdfe()
{
  int status=0;

/*get parms, parms are stored in global variables */
  if(status=cfGetSysParms()) {
    ffatal("can't get parms in cfGetSysParms. Status=%d\n",status);
  }
  if(status=anGetUserParms()) {
    ffatal("can't get parms in cfGetUserParms. Status=%d\n",status);
  }

/*open fits file*/
  if(status=cfOpenFits())
    ffatal("in cfOpenFits. Status=%d\n",status);

/*event processing*/
  if(status=cfSisEventProc(event))
    fprintf(stderr, "Error: in cfSisEventProc. Status=%d\n",status);

/*user termination processng*/
  if(status=anUserTerminate(event.sensor))
    fprintf(stderr, "Error: in anUserTerminate. Status=%d\n",status);

/*close fits file*/
  cfCloseFits();
}
