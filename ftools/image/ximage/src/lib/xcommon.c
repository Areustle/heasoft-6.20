/* 
 *  Common routines needed from C
 */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "cfortran.h"
#include "../include/xcommon.h"

#define XWRITE(A,B) CCALLSFSUB2(XWRITE,xwrite,STRING,INT,A,B)
#define XWARN(A,B) CCALLSFSUB2(XWARN,xwarn,STRING,INT,A,B)

int abbrmatch(char *fullstr, char *abbrstr) {
/*
 *  Abbreviated compare, returns TRUE(i.e. 1) if abbrstr is
 *  an abbreviation of fullstr. Case is ignored
 */
    char *aptr, *fptr;
    int match;

    aptr = abbrstr;
    fptr = fullstr;

    if ( !aptr || !*aptr ) return 0;  /* Fail if abbr is NULL or empty */

    match = 1;
    while ( *aptr && match ) {
       if ( *fptr ) {
          if ( tolower(*aptr) != tolower(*fptr) ) match = 0;
       } else {
          match = 0;
       }
       aptr++;
       fptr++;
    }
    return (match);
}

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

void cxwrite (const char *msg, const int ichat) {
/*
     msg = Message to be logged/printed
     ichat = chat level to log/print it 

     Wrapper for xwrite (On OSF, assigning to read-only)
*/
   char *tmpmsg;

   if ( (tmpmsg = stralloc(msg)) ) {
      XWRITE(tmpmsg, ichat);
      free(tmpmsg);
   } else {
      printf(" Malloc for cxwrite failed\n");
      printf(" Message: %s\n", msg);
   }
}

void cxwarn (const char *msg, const int ichat) {
/*
     msg = Message to be logged/printed
     ichat = chat level to log/print it 

     Wrapper for xwarn (On OSF, assigning to read-only)
*/
   char *tmpmsg;

   if ( (tmpmsg = stralloc(msg)) ) {
      XWARN(tmpmsg, ichat);
      free(tmpmsg);
   } else {
      printf(" Malloc for cxwarn failed\n");
      printf(" Message: %s\n", msg);
   }
}

char *quotok(char *str1, const char *str2) {
/*
    An implementation of strtok which handles quoting

    str1 = String to parse
    str2 = List of delimiters

    Like strtok, the first call takes str1 as string, while subsequent calls 
    must use the null pointer.  str1 is modified in the process.

    Returns pointer to token.

    For example: "info, info",14,yes
    Returns pointer to: info, info
                  then: 14
                  then: yes
 */
   char *begptr, *curptr, *delptr;
   static char *holdptr = NULL;

   if ( str1 ) {
      begptr = str1;
   } else {
      if ( !holdptr ) return(NULL);
      begptr = holdptr;
   }
   curptr = begptr;

   /* Match quotes */

   if ( *curptr == '"' ) {
      curptr++;
      begptr = curptr;

      /* Keep going into you find matching quote */

      while ( *curptr != '"' && *curptr != '\0') curptr++;
      if ( *curptr != '"' ) {
         cxwrite("Bad token, unmatched quote", 5);
         return (NULL);
      }
      *curptr = '\0';
      curptr++;

      /* Check for delims immediately after quote */

      if ( curptr == strpbrk(curptr, str2) ) {
         holdptr = curptr + 1;
      } else {
         holdptr = NULL;
      }
   } else {

      /* Find delims */

      delptr = strpbrk(curptr, str2);
      if ( delptr ) {
         *delptr = '\0';
         holdptr = delptr + 1;
      } else {

         /* Zap newline if there */

         while ( *curptr != '\n' && *curptr != '\0') curptr++;
         if ( *curptr == '\n' ) *curptr = '\0';
         holdptr = NULL;
      }
   }
   return(begptr);
}

char **quosplit(char *str1, const char *str2) {
/*
    Similar to perl split command, where str1 is the string
    to be split and str2 are the characters to split on.  
    Returns null-terminated array of strings.  str1 is modified 
    in the process.

    If a delimiter falls in a quoted string it is ignored

    For example str1 = apples "red grapes" oranges  str2 = " "
                       ^     0^           0^

    Returns pointers corresponding to ^ and inserts nulls in 0 spots.
    Returned value is null-terminated array of strings (char *)'s
 */
   char *begptr, *curptr, *filptr, **strary, **tmpary;
   int chunksz, numalloc, numstr;
   int qucnt, brcnt, bsflag;

   chunksz = 2;

   numalloc = chunksz;
   numstr = 0;
   strary = (char **) malloc (chunksz*sizeof(char *));
   if ( !strary ) return NULL;
   
   /* printf("str1: %p->%s<-\n", str1, str1); */
   begptr = str1;
   curptr = begptr;
   qucnt = 0;
   brcnt = 0;
   bsflag = 0;
   while ( *curptr ) {

      /* Skip next char if preceded by \ */
/* Backslash quoting is conflicting with Tcl parser,
 * comment out until we find actual case where its needed
 *    if ( *curptr == '\\' ) {
 *       bsflag++;
 *       curptr++;
 *    } else 
 */

      if ( *curptr == '"' ) {  /* Keep track of quote state */
         if ( qucnt ) {
            /* printf ("quote end\n"); */
            qucnt--;
         } else {
            /* printf ("quote begin\n"); */
            qucnt++;
         }
      } else if ( *curptr == '{' ) {
         brcnt++;
      } else if ( *curptr == '}' ) {
         brcnt--;
      } else if ( !brcnt && !qucnt && strchr(str2, *curptr) )  { 
         *curptr = '\0';
         if ( numstr+1 > numalloc ) {
            tmpary = realloc( strary, (numalloc+chunksz)*sizeof(char *) );
            if ( !tmpary ) return NULL;
            strary = tmpary;
            numalloc += chunksz;
         }
         /* printf ("token: %d %s\n", numstr, begptr); */
         strary[numstr] = begptr;
         numstr++;
         begptr = curptr + 1;
      }
      curptr++;
   }

   /* Remove backslashes used for quoting */ 
   if ( bsflag ) {
      curptr = begptr;
      filptr = begptr;
      while ( *curptr ) {
         if ( *curptr == '\\' ) curptr++;
         *filptr = *curptr;
         filptr++;
         curptr++;
      }
      *filptr = '\0';
   }

   if ( numstr+2 > numalloc ) {
      tmpary = realloc( strary, (numalloc+2)*sizeof(char *) );
      if ( !tmpary ) return NULL;
      strary = tmpary;
   }
   if ( numstr == 0 && !*begptr ) begptr = NULL;
   strary[numstr] = begptr;
   strary[numstr+1] = NULL;
   return(strary);
}

char *safestrcpy(char *str1, char *str2, int maxlen) {
/*
 *  maxlen corresponds to the size of str1 buffer
 *  Only copy as much of str2 as str1 will hold
 */
   int i;

   i = 0; 
   while ( i < maxlen-1 && str2 && str2[i] ) {
      str1[i] = str2[i];
      i++;
   }
   str1[i] = '\0';
   return(str1);
}
