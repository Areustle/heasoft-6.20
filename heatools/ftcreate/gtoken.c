#include <ctype.h>
#include <string.h>

int gtoken( int nfields, char* line, int start, long* begcol, long* twidth)
{

    int maxlen, i;
    
    maxlen = strlen(line);
    i = start;

    while ( i < maxlen && isspace((int) line[i]) ) i++;
    if( nfields <= 0 || i >=maxlen  ) return 0;


    begcol[0] = i ;

    if( line[i] =='\'' ) 
    {    
         do
         {
            i++;
            begcol[0]=i;
         }
         while (line[i] =='\'');
      
         while(1)
         {
             if( i >= maxlen || line[i]=='\'' )
             {
                 twidth[0] = i -begcol[0];
                 i=i+1;
                 break;
             }
             i++;
         } 
    }
    else 
    {
        while(1) 
        {
            i++;
            if( i >= maxlen || isspace((int) line[i]) ) 
            {    
                 twidth[0] = i-begcol[0];
                 break;
            }
        }
    }

    return ( 1 + gtoken(nfields-1,line,i,begcol+1,twidth+1));

}



            

