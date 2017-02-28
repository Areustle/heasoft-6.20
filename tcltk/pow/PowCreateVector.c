#include "pow.h"

void PowCreateVector(char *vector_name, char *data_name, int *offset,
		     int *length, char *units, int *status) {
  PowVector *vector_instance;
  PowData *dataptr;
  Tcl_HashEntry *entry_ptr,*data_entry_ptr;
  int new = 0;
  char *str_ptr;

  entry_ptr = Tcl_CreateHashEntry(&PowVectorTable, vector_name, &new);
#ifdef DEBUG
  if (!new) {
    printf("Reusing vector name: %s\n",vector_name);
  }
#endif
  vector_instance = (PowVector *) ckalloc(sizeof(PowVector));

  if(vector_instance == NULL) {
    *status = TCL_ERROR;
    fprintf(stderr, "Couldn't malloc vector structure space");
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  }

  Tcl_SetHashValue( entry_ptr, vector_instance);


  data_entry_ptr = Tcl_FindHashEntry(&PowDataTable, data_name);

  if(data_entry_ptr == NULL) {
    *status = TCL_ERROR;
    fprintf(stderr, "Couldn't find data: %s\n", data_name);
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  }

  dataptr = (PowData *) Tcl_GetHashValue(data_entry_ptr);


  str_ptr = ckalloc(strlen(vector_name)+1);
  strncpy(str_ptr,vector_name,strlen(vector_name)+1);

  vector_instance->vector_name = str_ptr;
  vector_instance->dataptr = dataptr;
  vector_instance->offset = *offset;
  if (length == NULL) {
    vector_instance->length = dataptr->length;
  } else {
    vector_instance->length = *length;
  }
  str_ptr = ckalloc(strlen(units)+1);
  strncpy(str_ptr,units,strlen(units)+1);
  vector_instance->units = str_ptr;
}




void PowCreateVectorEN(char *vector_name, char *data_name, 
			     int *length, double *start, double *increment, 
			     char *units, int *status) {
  double *array;
  int data_type;
  int offset;
  int i;
  PowData *data_instance;

  array = (double *) ckalloc(*length * sizeof(double));
  for (i = 0; i < *length; i++) {
    array[i] = *start + *increment * (double) i;
  }

  data_type = DOUBLE_DATA;

  i = 0;
  PowCreateData(data_name, (void *) array, &data_type, length, &i, status);
  
  data_instance = PowFindData(data_name);
  /*Since this data was made by us, we'll mark it as a POW copy so that
    PowDestroyData will free it.*/
  data_instance->copy = 1;

  offset = 0;
  
  PowCreateVector(vector_name, data_name, &offset, length, units, status);
  
  return;

}


void PowDestroyVector(char *vector_name, int *status) {
  Tcl_HashEntry *entry_ptr;
  char errormsg[1024];
  PowVector *vector_ptr;
  
  entry_ptr = Tcl_FindHashEntry(&PowVectorTable,vector_name);
  
  if (entry_ptr == NULL) {
    *status = TCL_ERROR;
    sprintf(errormsg,"Can't find POWVector Object %s to destroy",vector_name);
    Tcl_SetResult(interp,errormsg,TCL_VOLATILE);
    return;
  }


  vector_ptr = (PowVector *)Tcl_GetHashValue(entry_ptr);

  /*Delete the entry from the master POWData Hash*/
  Tcl_DeleteHashEntry(entry_ptr);
  
  /*free the PowVector memory itself and the string holding the name,
     although this is small change*/
  ckfree(vector_ptr->vector_name);
  ckfree(vector_ptr->units);
  ckfree((char *)vector_ptr);
  return;
}
