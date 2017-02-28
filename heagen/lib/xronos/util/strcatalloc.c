/* 
 * strcatalloc.c
 */

#include <stdlib.h>
#include <string.h>

char *strcatalloc (const char *str1, const char *str2) {
/*
 *  Takes two string, allocates space and concatenates them
 */
   int i, len1, len2;
   char *perm_str;

   if ( !str1 || !str2 ) return NULL;
   len1 = strlen(str1);
   len2 = strlen(str2);
   perm_str = (char *) malloc((len1+len2+1)*sizeof(char));
   if (perm_str == NULL) return (NULL);
   for ( i = 0; i < len1; ++i ) 
      *(perm_str+i) = *(str1+i);
   for ( i = 0; i <= len2; ++i )
      *(perm_str+len1+i) = *(str2+i);
   return (perm_str);
}
