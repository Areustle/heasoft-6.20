#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char* get_next_item(char** columns);

int gcls(               
    char* columns,      /* I - String list of column names        */
    char** colist,      /* O - array of separate column names     */
    int* ascend)        /* O - exclude name flags                 */
/* 
 Gets the list of columns names.
*/
{
    FILE * nmfile;
    char tmpnm[256];
    char* item;
    int slen,ncols;
    int i;
    
    ncols = 0;

    if ((item=get_next_item(&columns)) ==NULL) /* Get first item from columns string */
        return ncols;  

    if (*item =='@')                      /* Column name is read from file */
    {
        nmfile = fopen((item+1), "r");
        if(!nmfile) 
        {
            fprintf(stderr,
               "GCLS: Could not open the column name file %s", (item+1));
            return ncols;
        }

        while (fgets(tmpnm, 256, nmfile))
        {
            tmpnm[255] = '\0';
            slen = strlen(tmpnm);
            tmpnm[slen-1] = '\0';
            colist[ncols] =(char*) calloc(slen,sizeof(char));
            strcpy(colist[ncols],tmpnm);
            ncols++;
            if(ncols > 999)
            {
                fprintf(stderr,"GCLS: The number of column truncated  to 999");
                break;
            }
        }
        fclose(nmfile);
    }
    else                              /* Get column name from columns */ 
    {
        ncols = 1;
        *colist =item;
    }

     for (i=0; i<ncols; i++) 
     {
         if( *colist[i] == '-' )
         { 
             *(ascend+i) =0;
             (colist[i])++;
         }
         else *(ascend+i)=1;
     }
   

    ncols = ncols                    /* Process next item of columns */ 
        + gcls(columns,colist+ncols,ascend+ncols);
    return ncols;

}

    
char* get_next_item(
      char** columns)     /* IO - Input of a string list of columns namea */
                          /*      Output of the remaining of string       */
{
    char * str;
    char * str1;
    char * str2;
    char * item;
    if(*columns ==NULL ) return NULL;
      
    while (**columns == ' ')     /* Ignore leading white space  */
        (*columns)++;

    if (**columns == '\0')      /* Return if columns is empty */ 
        return NULL;

    if (**columns == '"')       /* Handler doule quote */ 
    {
        item =(*columns)+1;
        str =item;
        while ( *str !='"' && *str !='\0') str++;
        if( *str =='"') {
             *str='\0';
             *columns=str+1;
        }
        else *columns =NULL;
        return item;
    }
    else                        /* Space is always a seperator */
    {                           /* Comma might be a seperator */
        item = *columns;
        str = item;
        str1 = strchr(str,'[');
        str2 = strchr(str,']');
       
        while (*str !='\0')     
        {
            if(*str == ' ' ) 
            {
                *str ='\0';
                *columns = str+1;
                return item;
            }
            else  if(*str ==',' && 
                    ( str1 ==NULL || str1 > str
                    || (str2 !=NULL && str2 < str)))
            {
                *str='\0';
                *columns = str+1;
                return item;
            }
            str++;
        }

        *columns = NULL;
        return item;

    }
 

}
        

    
        
     
