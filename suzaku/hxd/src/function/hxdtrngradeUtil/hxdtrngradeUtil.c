#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <anl.h>
#include <bnk.h>
#include <fitsio.h>
#include <aste_rand.h>

#include "hxdtrngradeUtil.h"

static char *pname = "hxdtrngradeUtil";

#define DEBUG 0

void hxdtrn_gradeUtil( int *trn_quality){
    
  if (DEBUG) fprintf(stdout, "hxdtrngrade called\n");

  *trn_quality = HXD_TRNGRADE_OK;

  if (DEBUG) fprintf(stdout, "hxdtrngrade done\n");
  return;
}
