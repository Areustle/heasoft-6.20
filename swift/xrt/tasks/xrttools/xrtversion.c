/*
 * 
 *	xrtversion.c: --- Swift/XRT ---
 *
 *	INVOCATION:
 *
 *		xrtversion [parameter=value ...]
 *
 *	DESCRIPTION:
 *          This task prints into standard output the XRTDAS Package
 *          verdion number and date.
 *
 *	DOCUMENTATION:
 *
 *	CHANGE HISTORY:
 *
 *        0.1.0 - FT 08/02/02 - First working version
 *
 *	AUTHORS:
 *
 *       Italian Swift Archive Center (FRASCATI)
 */

#define XRTVERSION_C
#define XRTVERSION_VERSION  0.1.0

#include <xrtdasversion.h>

int main() {
  Version_t swxrtdas_v;
  GetSWXRTDASVersion(swxrtdas_v);  
  fprintf(stdout,"%s\n",swxrtdas_v);
  return 0;
}
