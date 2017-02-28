#include "powdata.h"
#include <limits.h>

/* on some system , e.g. linux, SUNs DBL_MAX is in float.h */
#ifndef DBL_MAX
#include <float.h>
#endif

#ifndef DBL_MIN
#include <float.h>
#endif


PowData *
PowFindData(char *data_name) {
  Tcl_HashEntry *entry_ptr;
  PowData *data_ptr;

  if(data_name == NULL || strstr(data_name,"NULL") != NULL) {
    return (PowData *) NULL;
  }
  
  entry_ptr = Tcl_FindHashEntry(&PowDataTable,data_name);
  if (entry_ptr == NULL) {
    return (PowData *) NULL;
  }
  data_ptr = (PowData *) Tcl_GetHashValue(entry_ptr);
  return data_ptr;
}

