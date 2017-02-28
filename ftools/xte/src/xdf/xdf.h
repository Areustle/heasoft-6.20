/* $Id: xdf.h,v 1.3 1997/02/26 20:58:03 oneel Exp $ */

/*   xdf.h  -  header file for xdf code
 *             contains mainly function prototypes
 */

#include "fitsdefs.h"
#include "fitsio.h"

int SelectObs (ClientData, Tcl_Interp *, int, char **) ;
int SelectAppIds (ClientData, Tcl_Interp *, int, char **) ;
int SelectFiles (ClientData, Tcl_Interp *, int, char **) ;

double date2sec (char *, int) ;
int getObsIds (Tcl_Interp *, char *, char ***, char ***, double **, 
	       double **, char ***) ;
int matchObs (char *, double, double, int, char **, int, double *, double *) ;
int getObs (Tcl_Interp *,char *, char *, double *, double *, char *, 
	    double *, int *,
            char *, char *, double *, double *, char **) ;
int getAppIds (Tcl_Interp *,char *, int *,char ***, char *, char *) ;
int addItem (char ***, int, char *) ;
int getFiles (int *, char ***, char *, char *, double, int, double *, double *,
	      int, char **) ;
int checkTime (char *, double, int, double *, double *) ;
