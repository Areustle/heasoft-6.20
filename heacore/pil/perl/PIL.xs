/* Perl interface to ISDC's PIL library.*/
/* Author: Ziqin Pan                    */
/* January 28, 2003                     */

#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "Av_CharPtrPtr.h"
#ifdef __cplusplus
}
#endif



#include "pil.h"
#include "pil_error.h"





MODULE = HEACORE::PIL 	PACKAGE = HEACORE::PIL		


int
PILGetBool(name,result)
	const char* name
	int &result
	OUTPUT:
	 result sv_setiv(ST(1), (int) result);	
	
int     
PILGetInt (name, result)
	const char* name
	int & result
	OUTPUT:
	 result sv_setiv(ST(1), (int) result);	

int
PILGetReal( name,result)
	const char *name
	double &result
	OUTPUT:
	 result sv_setnv(ST(1), (double) result);	

int
PILGetReal4(name,result)
	const char *name
	float &result
	OUTPUT:
	 result sv_setnv(ST(1), (float) result);	

int
PILGetString(name,result)
	const char *name
	char * result  = calloc(PIL_LINESIZE,sizeof(char));
	OUTPUT:
	 result sv_setpv(ST(1), result);	
	CLEANUP:
	free(result);

int
PILGetFname(name,result)
	const char *name
	char * result  = calloc(PIL_LINESIZE,sizeof(char));
	OUTPUT:
	 result sv_setpv(ST(1), result);	

	CLEANUP:
	free(result);

int
PILPutBool(name,b)
	const char *name
	int b

int
PILPutInt(name,i)
	const char *name
	int i

int
PILPutReal(name,d)
	const char *name
	double d

int
PILPutString(name,s)
	const char *name
	const char *s

int
PILPutFname(name,s)
	const char *name
	const char *s

int
PILInit(argc,argv)
	int       argc
	char **	  argv

int
PILClose(status)
	int status
