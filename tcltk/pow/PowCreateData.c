#include "pow.h"

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
    case 64:
      array_instance->data_type = 5;
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
  
/* Initializes a PowData structure and adds it to powTable */
void PowCreateDataFlip(char *data_name, char *direction, int *height, int *width, int *status) {
     PowData *array_instance;
     char *start_ptr;
     char *orig_ptr, *cpy_ptr;
     int data_type = 0;
     int i, j, k;
     int cIdx, whichset;
     long value, maxValue, minValue;
     void *tmp_array;
   
     array_instance = PowFindData(data_name);

     if (array_instance == (PowData *) NULL) 
     {
        *status = TCL_ERROR;
        fprintf(stderr, "Couldn't locate data_name, %s.", data_name);
        return;
     }

     data_type = array_instance->data_type;
     orig_ptr = (char *) array_instance->data_array;

     tmp_array = (void *) ckalloc(array_instance->length * pixelSizes[array_instance->data_type]);
     if (tmp_array == NULL) 
     {
        *status = TCL_ERROR;
        fprintf(stderr, "Couldn't allocate space for copy of data.");
        return;
     }

     cpy_ptr = (char *) tmp_array;

     switch (*direction) 
     {
         case 'X': 
            cIdx = 0;
            for (i=0; i < *height; i++)
	    {
	        whichset = i;
	        maxValue = whichset * (*width) + (*width) - 1;
	        minValue = whichset * (*width); 

	        for (j = 0; j < *width; j++)
	        {
                    value = maxValue;
                    maxValue--;
		    start_ptr = orig_ptr + value * pixelSizes[array_instance->data_type];

		    for (k = 0; k < pixelSizes[array_instance->data_type]; k++) 
	            {
		        *(cpy_ptr++) = *(start_ptr++);
		    }
                    cIdx++;
                }
            }
            break;

         case 'Y': 
            cIdx = 0;
	    whichset = *height;
	    for (i=0; i < *height; i++) 
	    {
	        maxValue = whichset * (*width) - 1;
	        minValue = whichset * (*width) - (*width);
	        for (j=0; j < *width; j++) 
	        {
                    value = minValue;
                    minValue++;
	            start_ptr = orig_ptr + value * pixelSizes[array_instance->data_type];

		    for (k = 0; k < pixelSizes[array_instance->data_type]; k++) 
	            {
		        *(cpy_ptr++) = *(start_ptr++);
		    }
                    cIdx++;
                }
	        whichset--;
            }
            break;

         default: 
            break;
     }

    start_ptr = (char *) array_instance->data_array;
    cpy_ptr = (char *) tmp_array;
   
    /* copy the result data back to array */
    for (k = 0; k < array_instance->length * pixelSizes[array_instance->data_type]; k++) 
    {
	*(start_ptr++) = *(cpy_ptr++);
    }
    ckfree(tmp_array);
}

void PowCreateVectorDataFlip(char *data_name, int *length, int *status) {
     PowData *array_instance;
     char *start_ptr;
     char *orig_ptr, *cpy_ptr, *pixel_ptr;
     int data_type = 0;
     int i, k;
     void *tmp_array, *pixel_array;
   
     array_instance = PowFindData(data_name);

     if (array_instance == (PowData *) NULL) 
     {
        *status = TCL_ERROR;
        fprintf(stderr, "Couldn't locate data_name, %s.", data_name);
        return;
     }

     data_type = array_instance->data_type;
     orig_ptr = (char *) array_instance->data_array;

     tmp_array = (void *) ckalloc(array_instance->length * pixelSizes[array_instance->data_type]);

     if (tmp_array == NULL) 
     {
        *status = TCL_ERROR;
        fprintf(stderr, "Couldn't allocate space for copy of data.");
        return;
     }

     cpy_ptr = (char *) tmp_array;

     for (i=(array_instance->length) - 1; i >= 0; i--)
     {
         pixel_array = (char *) ckalloc(pixelSizes[array_instance->data_type] + 1);
         pixel_ptr = (char *) pixel_array;
         start_ptr = orig_ptr + i * pixelSizes[array_instance->data_type];

         for (k = 0; k < pixelSizes[array_instance->data_type]; k++)
         {
             *(cpy_ptr++) = *(start_ptr);
             *(pixel_ptr++) = *(start_ptr++);
         }
         ckfree(pixel_array);
     }

     start_ptr = (char *) array_instance->data_array;
     cpy_ptr = (char *) tmp_array;
   
     /* copy the result data back to array */
     for (k = 0; k < array_instance->length * pixelSizes[array_instance->data_type]; k++) 
     {
 	*(start_ptr++) = *(cpy_ptr++);
     }
     ckfree(tmp_array);
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


  data_ptr = (PowData *)Tcl_GetHashValue(entry_ptr);

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
