#include "pow.h"

void PowCreateImage(char *image_name, char *data_name, int *xoffset, 
		    int *yoffset, int *width, int *height, double *xorigin, 
		    double *xinc, double *yorigin, double *yinc, char *xunits,
		    char *yunits, char *zunits, int *status) {
/* xinc or yinc == 0 will mean count by integers */
  PowImage *image_instance;
  PowData *dataptr;
#if !(defined(__WIN32__) || defined(macintosh))
  Tk_PictHandle pict_image_handle;
  Tk_PictImageBlock pict_block;
  PictMaster *masterPtr;
#endif
  Tk_PhotoHandle photo_image_handle;
  Tk_PhotoImageBlock photo_block;
  Tcl_HashEntry *entry_ptr;
  int new = 0;
  char *str_ptr;
  int pseudoImages;
  double min,max;
  char smin[30];
  char smax[30];
  double datum;
  int i, wcsStatus;
  const char *WCSstring;
  char powWCS[7]="powWCS";

  Tcl_GetInt(interp,Tcl_GetVar(interp,"powPseudoImages",TCL_GLOBAL_ONLY),
	    &pseudoImages);

  

  entry_ptr = Tcl_CreateHashEntry(&PowImageTable, image_name, &new);
  if (!new) {
/*
fprintf(stdout, "Reusing image name: %s\n", image_name); 
*/
#ifdef DEBUG
    printf("Reusing image name: %s",image_name);
#endif
#if !(defined(__WIN32__) || defined(macintosh))
    /* zero out the data ptr field so VISU won't throw away our data */
    /* when you reuse an image name, VISU calls ImgPictDelete on it */
    /* this frees the data pointer.  We don't want that.... */
    if( pseudoImages ) {
       masterPtr = (PictMaster *)Tk_FindPict(image_name);
       if ( (unsigned char*)masterPtr->data == masterPtr->bytedata) {
	  masterPtr->bytedata = NULL;
       }
       masterPtr->data = NULL;
    }
#endif

  }

  image_instance = (PowImage *) ckalloc(sizeof(PowImage));

  if(image_instance == NULL) {
    *status = TCL_ERROR;
    fprintf(stderr, "Couldn't malloc image structure space");
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  }

  Tcl_SetHashValue( entry_ptr, image_instance);


  str_ptr = ckalloc(strlen(image_name)+1);
  strncpy(str_ptr,image_name,strlen(image_name)+1);


  image_instance->image_name = str_ptr;
  image_instance->xoffset = *xoffset;
  image_instance->yoffset = *yoffset;
  image_instance->width = *width;
  image_instance->height = *height;
  image_instance->xorigin = *xorigin;
  image_instance->xinc = *xinc;
  image_instance->yorigin = *yorigin;
  image_instance->yinc = *yinc;
  str_ptr = ckalloc(strlen(xunits)+1);
  strncpy(str_ptr,xunits,strlen(xunits)+1);
  image_instance->xunits = str_ptr;
  str_ptr = ckalloc(strlen(yunits)+1);
  strncpy(str_ptr,yunits,strlen(yunits)+1);
  image_instance->yunits = str_ptr;
  str_ptr = ckalloc(strlen(zunits)+1);
  strncpy(str_ptr,zunits,strlen(zunits)+1);
  image_instance->zunits = str_ptr;

  /* Now set up the image */

  if (pseudoImages != 0) {
#if !(defined(__WIN32__) || defined(macintosh))
    /* use Pict widget (Visu) */


    if (Tcl_VarEval(interp,"image create pict ",image_instance->image_name,(char *) NULL) == TCL_ERROR) {
      *status =  TCL_ERROR;
      fprintf(stderr, "%s\n", Tcl_GetStringResult(interp));
      Tcl_DeleteHashEntry(entry_ptr);
      return;
    }
    
    pict_image_handle = Tk_FindPict(image_instance->image_name);

    if(pict_image_handle == NULL) {
      *status = TCL_ERROR;
      fprintf(stderr, "%s\n", Tcl_GetStringResult(interp));
      Tcl_DeleteHashEntry(entry_ptr);
      return;
    }

    image_instance->image_handle = (void *) pict_image_handle;
#else
    fprintf(stderr,"You should not see this. Pict images disabled in Win32.\n");
    return;
#endif /*__WIN32__ || macintosh*/

  } else {
    /* use Photo widget */
    if (Tcl_VarEval(interp,"image create photo ",image_instance->image_name,(char *) NULL) == TCL_ERROR) {
      *status =  TCL_ERROR;
      fprintf(stderr, "%s\n", Tcl_GetStringResult(interp));
      Tcl_DeleteHashEntry(entry_ptr);
      return;
    }
    
    photo_image_handle = Tk_FindPhoto(interp,image_instance->image_name);
    if(photo_image_handle == NULL) {
      *status = TCL_ERROR;
      fprintf(stderr, "%s\n", Tcl_GetStringResult(interp));
      Tcl_DeleteHashEntry(entry_ptr);
      return;
    }
    
    image_instance->image_handle = (void *) photo_image_handle;
  }

  /* Get the data address out of the hash table */
  entry_ptr = Tcl_FindHashEntry (&PowDataTable, data_name);
  if(entry_ptr == NULL) {
    *status = TCL_ERROR;
    fprintf(stderr, "%s\n", Tcl_GetStringResult(interp));
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  } 
  dataptr = (PowData *) Tcl_GetHashValue(entry_ptr);


  /*Setup displayed min and max stuff */

  min =  DBL_MAX;
  max = -DBL_MAX;

  for (i = 0; i < image_instance->width * image_instance->height; i++) {
    datum = PowExtractDatum(dataptr,i);
    if (datum != DBL_MAX) {
      min = (datum < min) ? datum : min;
      max = (datum > max) ? datum : max;
    }
  }
  if( max==-DBL_MAX ) min=max=0.0;

  sprintf(smin,"%.17lg",min);
  sprintf(smax,"%.17lg",max);
  
  Tcl_SetVar2(interp,"powRBmin",image_name,smin,TCL_GLOBAL_ONLY);
  Tcl_SetVar2(interp,"powRBmax",image_name,smax,TCL_GLOBAL_ONLY);

			
  image_instance->dataptr = dataptr;

  /* notice that the "offsets" are not implemented here yet*/
  /* this will require some serious tweaking to pixelPtr and skip fields */

  if (pseudoImages != 0) {
#if !(defined(__WIN32__) || defined(macintosh))
    /*Pict*/
    pict_block.datatype = dataptr->data_type;
    pict_block.pixelPtr = (unsigned char *) dataptr->data_array;
    pict_block.width = image_instance->width;
    pict_block.height = image_instance->height;
    pict_block.pixelSize = pixelSizes[dataptr->data_type];
    pict_block.pitch = pict_block.pixelSize;
    pict_block.skip = 0;
    pict_block.copy = NO_COPY;
    

    Tk_PictExpand(pict_image_handle,image_instance->width,image_instance->height);
    
    Tk_PictPutBlock(pict_image_handle,&pict_block,0,0,image_instance->width,image_instance->height);
#else
    fprintf(stderr,"You should not see this\n");
    return;
#endif /*__WIN32__ || macintosh*/
  } else {

    PowDitherToPhoto(image_instance,&photo_block,min,max);

    photo_block.pixelSize = 3;
    photo_block.width = image_instance->width;
    photo_block.height = image_instance->height;
    photo_block.pitch = image_instance->width * 3;
    photo_block.offset[0] = 0;
    photo_block.offset[1] = 1;
    photo_block.offset[2] = 2;

    Tk_PhotoExpand(interp, photo_image_handle,
		   image_instance->width, image_instance->height);
    
    Tk_PhotoPutBlock(interp, photo_image_handle, &photo_block, 0, 0,
		     image_instance->width, image_instance->height, TK_PHOTO_COMPOSITE_SET);
    
    ckfree(photo_block.pixelPtr);

  }

  /* Call WCS init procedure if applicable */
  wcsStatus = TCL_ERROR;
  WCSstring = Tcl_GetVar2(interp,powWCS,image_name,TCL_GLOBAL_ONLY);
  if( (WCSstring != NULL) && strcmp(WCSstring,"") ) {
     wcsStatus = Tcl_VarEval(interp, "powWCSInitImage ", image_name, " ",
                             WCSstring, (char *) NULL);
  }

  if( wcsStatus == TCL_ERROR ) {
    image_instance->xorigin -= 0.5*image_instance->xinc;
    image_instance->yorigin -= 0.5*image_instance->yinc;
    image_instance->xotherend = image_instance->xorigin + 
                                image_instance->width*image_instance->xinc;
    image_instance->yotherend = image_instance->yorigin + 
                                image_instance->height*image_instance->yinc;

    memset(image_instance->WCS.type, '\0', 6);
    image_instance->WCS.nAxis     = 2;
    image_instance->WCS.refVal[0] = image_instance->xorigin;
    image_instance->WCS.refVal[1] = image_instance->yorigin;
    image_instance->WCS.refPix[0] = -0.5;
    image_instance->WCS.refPix[1] = -0.5;

    image_instance->WCS.cdFrwd[0][0] = image_instance->xinc;
    image_instance->WCS.cdFrwd[1][1] = image_instance->yinc;
    image_instance->WCS.cdFrwd[0][1] = 0.0;
    image_instance->WCS.cdFrwd[1][0] = 0.0;

    image_instance->WCS.cdRvrs[0][0] = 1.0/image_instance->xinc;
    image_instance->WCS.cdRvrs[1][1] = 1.0/image_instance->yinc;
    image_instance->WCS.cdRvrs[0][1] = 0.0;
    image_instance->WCS.cdRvrs[1][0] = 0.0;
  }

  return;
}


void PowDestroyImage(char *image_name, int *status) {
  Tcl_HashEntry *entry_ptr;
  char errormsg[1024];
  PowImage *image_ptr;
  
  entry_ptr = Tcl_FindHashEntry(&PowImageTable,image_name);
  
  if (entry_ptr == NULL) {
    *status = TCL_ERROR;
    sprintf(errormsg,"Can't find POWImage Object %s to destroy",image_name);
    Tcl_SetResult(interp,errormsg,TCL_VOLATILE);
    return;
  }


  image_ptr = (PowImage *)Tcl_GetHashValue(entry_ptr);

  /*Delete the entry from the master POWData Hash*/
  Tcl_DeleteHashEntry(entry_ptr);
  
  /*free the PowImage memory itself and the string holding the name,
     although this is small change*/
  ckfree(image_ptr->image_name);
  ckfree(image_ptr->xunits);
  ckfree(image_ptr->yunits);
  ckfree(image_ptr->zunits);
  ckfree((char*)image_ptr);
  return;
}
