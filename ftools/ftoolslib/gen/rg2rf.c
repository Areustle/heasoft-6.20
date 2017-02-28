/****************************************************************
*       rg2rf							*
*								*
*								*
*   Convert the old row range to the new row filter expression. *
*								*
*   The old row range  is specified as first-last, which means  *
*   the selected rows*start from row "first" and stop at row 	*
*   "last". It also accepts the multiple ranges seperated by    *
*   ",". see fcgrgs in misc.for for details.			*
*								*
*   The row_filter will return string with 0 length  if no row 	*
*   filter is defined. The brackets [ and ] are not included.   *
*								*
*   This program makes no attempt either to check the conflicts *
*   or to find the largest common range of the multiple ranges. *
*   								*
*   Ning Gan  6/2/96						*
*								*
*   Usage: 							*
*								*
*      int rg2rf(char *, char *);                               *
*								*
*****************************************************************/
#define SUBLEN 81
#define MAXNUMRANGE 100
#include <string.h>
#include <stdlib.h>
void parse_range(char* row_range,int* row_only,int* row_first,int* row_last);
void  constr_sub_row_fltr(int row_only, int row_first,int row_last, 
char subfltr[SUBLEN] ); 

int rg2rf( char* row_range,	/* row range spec. */
	   char* row_filter 	/* row filter */
         )
{
    int row_only;
    int row_first;
    int row_last;

    char temp[SUBLEN];
    char* row_tmp;

    char *p1;
    char *range[MAXNUMRANGE];
    int num_range = 0;

    int i,n;
    int flag = 0;
    


    row_filter[0] = '\0';
    n = strlen(row_range);
    row_tmp = (char *)malloc(n+1);
    strcpy(row_tmp,row_range);

    /* only one row spec. */
    if(strchr(row_tmp,',') == NULL) { 	
	parse_range(row_tmp, &row_only, &row_first,&row_last);
        constr_sub_row_fltr(row_only, row_first,row_last, temp);
	if(strlen(temp)!= 0) { 
	    strcat(row_filter,temp);
        }
        free (row_tmp);
	return 0; 
    } 

    /* multiple row specs, parse the string into row specs. */
    i = 0;
    if( (p1 = strtok(row_tmp,",")) != NULL) {
	n = strlen(p1);
	if(n!=0) {
	    range[i] = (char *) malloc(n+1);
	    strcpy(range[i],p1);
	    i++;
        }
    }
    while ((p1 = strtok(NULL,",")) != NULL) {	/* loop on the tokes */
	n = strlen(p1);
	if(n!=0) {
	    range[i] = (char *) malloc(n+1);
	    strcpy(range[i],p1);
	    i++;
        }
    }
    num_range = i;

    for( i = 0; i < num_range; i++) {
	parse_range(range[i], &row_only, &row_first,&row_last);
        constr_sub_row_fltr(row_only, row_first,row_last, temp);
	if(strlen(temp)!= 0 && flag == 0) {  /* for the first range */
	    strcat(row_filter,temp);
	    flag = 1;
        }
	else if(strlen(temp)!= 0 && flag == 1) { /* for the rest */
	    strcat(row_filter," || ");
	    strcat(row_filter,temp);
        }
    }
    free (row_tmp);
    for( i = 0; i < num_range; i++) {
        free (range[i]);
    }
    return 0;
}

/*********************************
 *				 *
 * parse the range specification *
 *				 *
 *********************************/
void parse_range(char* row_range,   	/* single row range */
		int* row_only,		/* only row, is mutually
		                        exclusive to row_first and row_last */
		int* row_first,   	/* first row */
		int* row_last )		/* last row */

{
    char* p;
    char* p1;

    /* if undefined  row_only = -1, *row_first = 1 and  *row_last = -1 */
    *row_first =  1;
    *row_last  = -1;
    *row_only  = -1;

    p = row_range;
    while(*p == ' ') p++; /* ignore the leading space */
    if(strlen(p) == 0) return ;  /* empty range */

    if(strchr(p,'-') == NULL) {	 /* only one row */
	sscanf(p,"%d",row_only);	
	return;
    }
    if(*p == '-') {
            p1 = p + 1;
	    while(*p1 == ' ') p1++; 
	    if(strlen(p1) != 0) { 
		*row_first = 1;
	        sscanf(p1,"%d",row_last);		
            }
    }
    else {			
        if( (p1 = strtok(p,"-"))!=NULL ) { 
	    sscanf(p1,"%d",row_first);	
	    /* both the first row and last row are entered */
	    if((p1 = strtok(NULL, "-"))!=NULL)sscanf(p1,"%d",row_last);	
        }
    }
    return ;
} 


/*********************************
 *				 *
 * constrct the sub string of    *
 * row filter (not include [ and *
 * ]).                           *
 *				 *
 *********************************/
void  constr_sub_row_fltr( 
		 int  row_only,		/* only row, is mutually
		                   	exclusive to row_first and row_last*/
		 int row_first, 	/* first row */   
		 int row_last, 		/* last row */
		 char subfltr[SUBLEN] 	/* substring of row filter */
                 )   
{ 
    char temp[SUBLEN];
    subfltr[0] = '\0';
    temp[0] = '\0';
    if(row_first <= 1 && row_last == -1 && row_only == -1) return; 
    strcpy(subfltr,"(");
    if(row_only > 0) {
        sprintf(temp,"#row == %i ",row_only);
	strcat(subfltr,temp);
        strcat(subfltr,")");
	return;
    }
    if(row_first > 1)  {
        sprintf(temp,"#row >= %i ",row_first);
	strcat(subfltr,temp);
        if(row_last > 0) strcat(subfltr,"&&");
    }
    if( row_last > 0 ) { 
	sprintf(temp," #row <= %i ",row_last);
        strcat(subfltr,temp);
    }         
    strcat(subfltr,")");
    return;
}

