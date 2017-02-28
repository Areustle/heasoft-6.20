#include<stdio.h>
#include <string.h>

int cmpl(
          char ** list1,
          int * nlist1,
          char ** list2,
          int * nlist2)
{
     int i,j;
     int subset =0;
     int found =0;

     for (i=0 ; i<*nlist1; i++) 
     { 
           found =0;
           for (j =0; j<*nlist2; j++) 
           {
              if (strcasecmp(list1[i],list2[j]) == 0) 
              {
                  found =1;
                  break;
              }
           }
           
           if(!found)
           {
               subset =1;
               fprintf(stderr,"cmpl: not found in table:%s\n", list1[i]);
               break;
           }

      }

      return subset;
}

     

          

