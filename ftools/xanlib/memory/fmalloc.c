

/*

  Replace for system malloc to aid in debugging memory leaks.
This will initially be available only for unix.

  jdd 6FEB1997


$Id: fmalloc.c,v 1.3 1997/02/13 23:22:18 dah Exp $

*/

#include<stdlib.h>

#undef malloc
#undef free
#undef realloc

#ifdef ftools_mem_debug

/* in the following we determine if FMEM_DEBUG is set
   in the environment; if so use Tcl stuff, otherwise use
   standard libc stuff
   */

void * F_malloc(size_t size) {

return (getenv("FMEM_DEBUG") != NULL) ?
         Tcl_DbCkalloc(size, "unknown", 0)
       :
         malloc(size);
}

void F_free(void *ptr) {

return (getenv("FMEM_DEBUG") != NULL) ?
         Tcl_DbCkfree(ptr, "unknown", 0)
       :
         free(ptr);
}

void *F_realloc(void *ptr, size_t size) {

return (getenv("FMEM_DEBUG") != NULL) ?
         Tcl_DbCkrealloc(ptr, size, "unknown", 0)
       :
         realloc(ptr,size);
}

#else

/* if no debug routines were requested at compile time
   don't even bother checking -- just define for the 
   stadards and go on.
*/
   #define F_malloc  malloc
   #define F_free    free
   #define F_realloc realloc

#endif

