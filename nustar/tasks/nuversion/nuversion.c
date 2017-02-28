/*
 * 
 *	nuversion.c: --- NuSTAR ---
 *
 *	INVOCATION:
 *
 *		nuversion [parameter=value ...]
 *
 *	DESCRIPTION:
 *          This task prints into standard output the NuSTARDAS Package
 *          version number and date.
 *
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *
 *        0.1.0 - NS 25/10/10 - First working version
 *
 *	AUTHORS:
 *
 *       ASDC - ASI Science Data Center
 */

#define NUVERSION_C
#define NUVERSION_VERSION  0.1.0

#include "nustardasversion.h"

int main() {
  Version_t nustardas_v;
  GetNuSTARDASVersion(nustardas_v);  
  fprintf(stdout,"%s\n",nustardas_v);
  return 0;
}
