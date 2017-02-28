#include <string.h>
/****************************************************************
 *  								*
 *   set_binspec						*
 *								*
 *   Construct the binspec string from the imagetype, colname   *
 *   min, max, binsize, and weight strings.			*
 *   The empty strings mean the default values.		        *
 *								*
 *   The prototype have to be included in the program invoking	*
 *   this function.						*
 *								*
 ****************************************************************/
int set_binspec( 
   char *imagetype,			/* image type(b,i,j,f,d) */
   int  haxis,				/* number of axes.	 */
   char **colname,			/* column names of axes  */ 
   char **min,				/* lower limits of axes  */ 
   char **max,				/* upper limits of axes  */ 
   char **binsz,			/* binsizes  of axes	 */
   char *weight,			/* weight		 */
   char *binspec,			/* bin spec..		 */
   int  *status )			/* error status		 */

{
    int i,j; 
    char* default_col[] = {"X", "Y", "Z", "T"};
    char temp[10];

    *status = 0;
    strcpy(binspec,"bin");
    if(strlen(imagetype)) {
	if(strlen(imagetype) > 1) { 
	     strcpy(temp,imagetype); /* preserve the original */
	     temp[1] = '\0';
	     strcat(binspec,temp);
        }
	else {
            strcat(binspec,imagetype);
        }
    }
    else {
	if(strlen(weight))strcat(binspec,"r");
    }

    /* Add axes */
    strcat(binspec,"  ");
    for( i = 0, j = haxis; i < haxis; i++, j--) { 
	if(strlen(colname[i])) {
	    strcat(binspec,colname[i]);
        }
	else {
	    strcat(binspec,default_col[i]);
        }
	strcat(binspec,"=");
	if(strlen(min[i]))strcat(binspec,min[i]);
	strcat(binspec,":");
	if(strlen(max[i]))strcat(binspec,max[i]);
	strcat(binspec,":");
	if(strlen(binsz[i]))strcat(binspec,binsz[i]);
	if(j>1) strcat(binspec,", ");
    } 
    
    /* Add weight */
    if(strlen(weight)) {
	strcat(binspec,"; ");
	strcat(binspec,weight);
    }
    return *status;
}
