/* 
 *  Common routines needed from C
 */

void cxwrite (const char *, const int);
void cxwarn (const char *, const int);
int abbrmatch(char *, char *);
char *stralloc (const char *);
char *strcatalloc (const char *, const char *);
char *quotok(char *, const char *);
char **quosplit(char *, const char *);
void printtest(char *);
char *safestrcpy(char *, char *, int);
