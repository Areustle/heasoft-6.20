#include "powdata.h"


/* Initializes a PowData structure and adds it to powTable */

void PowCreateData(char *data_name, void *data_array, int *data_type,
		   int *length, int *copy, int *status) {
  PowData *array_instance;
  Tcl_HashEntry *entry_ptr;
  char *str_ptr;
  char *orig_ptr,*cpy_ptr;
  int new = 0;
  int i;


  entry_ptr = Tcl_CreateHashEntry(&PowDataTable, data_name, &new);
#ifdef DEBUG
  if (!new) {
    printf("Reusing data name: %s\n",data_name);
  }
#endif

  
  array_instance = (PowData *) ckalloc(sizeof(PowData));
  
  if(array_instance == NULL) {
    *status = TCL_ERROR;
    fprintf(stderr, "Couldn't ckalloc array structure space");
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  }


  Tcl_SetHashValue( entry_ptr, array_instance);

  

  str_ptr = ckalloc(strlen(data_name)+1);
  strncpy(str_ptr,data_name,strlen(data_name)+1);


  array_instance->data_name = str_ptr;
  array_instance->data_array = data_array;
  array_instance->copy = *copy;
  array_instance->data_type = *data_type;
  array_instance->length = *length;

  if (array_instance->data_type >= 8 || array_instance->data_type <= -8) {
    switch (array_instance->data_type) {
    case 8:
      array_instance->data_type = 0;
      break;
    case 16:
      array_instance->data_type = 1;
      break;
    case 32:
      array_instance->data_type = 2;
      break;
    case -32:
      array_instance->data_type = 3;
      break;
    case -64:
      array_instance->data_type = 4;
      break;
    default:
      *status = TCL_ERROR;
      fprintf(stderr, "Unknown data type\n");
      Tcl_DeleteHashEntry(entry_ptr);
      return;
    }
  }

      

  if( *copy > 0 ) {
    /* copy data:  If *copy<0, don't copy but free pointer in DestroyData */
    orig_ptr = (char *) data_array;
    array_instance->data_array = (void *) ckalloc(*length * pixelSizes[*data_type]);
    if(array_instance->data_array == NULL) {
      *status = TCL_ERROR;
      fprintf(stderr, "Couldn't allocate space for copy of data.");
      Tcl_DeleteHashEntry(entry_ptr);
      return;
    }

    
    cpy_ptr = (char *) array_instance->data_array;

    for (i=0; i < *length * pixelSizes[*data_type]; i++) {
      *(cpy_ptr++) = *(orig_ptr++);
    }
  }
}
  

void PowDestroyData(char *data_name, int *status) {
  Tcl_HashEntry *entry_ptr;
  char errormsg[1024];
  PowData *data_ptr;
  
  entry_ptr = Tcl_FindHashEntry(&PowDataTable,data_name);
  
  if (entry_ptr == NULL) {
    *status = TCL_ERROR;
    sprintf(errormsg,"Can't find POWData Object %s to destroy",data_name);
    Tcl_SetResult(interp,errormsg,TCL_VOLATILE);
    return;
  }


  data_ptr = Tcl_GetHashValue(entry_ptr);

  /*free the data if it's ours*/
  if (data_ptr->copy != 0) {
    ckfree(data_ptr->data_array);
  }

  /*Delete the entry from the master POWData Hash*/
  Tcl_DeleteHashEntry(entry_ptr);
  
  /*free the PowData memory itself and the string holding the name,
     although this is small change*/
  ckfree( (char*)data_ptr->data_name);
  ckfree( (char*)data_ptr);

}


void PowRegisterData(PowData *dataptr,int *status) {
  int new = 0;
  Tcl_HashEntry *entry_ptr;


  entry_ptr = Tcl_CreateHashEntry(&PowDataTable, dataptr->data_name, &new);
#ifdef DEBUG
  if (!new) {
    printf("Reusing data name: %s\n",data_name);
  }
#endif

  Tcl_SetHashValue( entry_ptr, dataptr);
  return;
}
