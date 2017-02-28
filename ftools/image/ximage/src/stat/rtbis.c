#include <math.h>
#include "../include/xcommon.h"

#define JMAX 1000 /*Maximum allowed number of bisections.*/

float rtbis(float (*func)(float), float x1, float x2, float xacc, int *status)
/* NR 9.1
 *  Using bisection, find the root of a function func known to lie 
 *  between x1 and x2. The root, returned as rtbis, will be refined 
 *  until its accuracy is +/-xacc.
 */
{
    int j;
    float dx,f,fmid,xmid,rtb;
    f=(*func)(x1);
    fmid=(*func)(x2);
    if (f*fmid >= 0.0) {
       cxwrite("Root must be bracketed for bisection in rtbis", 10);
       *status = -1;
       return 0.0;
    }
    
    rtb = f < 0.0 ? (dx=x2-x1,x1) : (dx=x1-x2,x2); /* Orient the search so 
						    * that f>0 lies at x+dx. */
    
    for (j=1;j<=JMAX;j++) {
	
	fmid=(*func)(xmid=rtb+(dx *= 0.5)); /*Bisection loop.*/
	if (fmid <= 0.0) rtb=xmid;
	if (fabs(dx) < xacc || fmid == 0.0) return rtb;
    }
    cxwrite("Too many bisections in rtbis", 10);
    *status = -1;
    return 0.0;
}
