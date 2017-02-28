/* 
 *  stralloc.c
 */

#include <stdlib.h>
#include <string.h>

char *stralloc (const char *temp_str) {
/*
     Allocate space for copy of input string

     temp_str = Input string
     Returns pointer to copy of input string
*/
   int len;
   char *perm_str;

   if ( !temp_str ) return NULL;
   len = strlen(temp_str);
   perm_str= (char *) malloc((len+1)*sizeof(char));
   if (perm_str == NULL) return (NULL);
   strcpy(perm_str,temp_str);
   return (perm_str);
}
