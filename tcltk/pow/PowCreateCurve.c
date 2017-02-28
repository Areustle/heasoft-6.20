#include "pow.h"

void PowCreateCurve(char *curve_name, char *x_vector, char *x_error,
		    char *y_vector, char *y_error, char *z_vector, 
		    char *z_error, int *status) {
  /*Until we see a problem with this, the length of the curve
    is the length of the x-vector (or y-vector if x-vector is null) or z-
    vector if y-vector is null)).  If any of the other non-null vectors
    are shorter than this, the command returns an error.*/
  PowCurve *curve_instance;
  Tcl_HashEntry *entry_ptr;
  int new = 0, wcsStatus;
  char *str_ptr;
  int length = 0;
  const char *WCSstring;
  char powWCS[7]="powWCS";

  entry_ptr = Tcl_CreateHashEntry(&PowCurveTable, curve_name, &new);
#ifdef DEBUG  
  if (!new) {
    printf("Reusing curve name: %s\n",curve_name);
  }
#endif

  curve_instance = (PowCurve *) ckalloc(sizeof(PowCurve));

  if(curve_instance == NULL) {
    *status = TCL_ERROR;
    fprintf(stderr, "Couldn't malloc curve structure space");
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  }

  Tcl_SetHashValue( entry_ptr, curve_instance);

  str_ptr = ckalloc(strlen(curve_name)+1);
  strncpy(str_ptr,curve_name,strlen(curve_name)+1);

  curve_instance->curve_name = str_ptr;
  length = 0;
  
  if ((curve_instance->x_vector = PowFindVector(x_vector)) != NULL) {
    length = (curve_instance->x_vector)->length;
    if ((curve_instance->x_error = PowFindVector(x_error)) != NULL) {
      if ((curve_instance->x_error)->length < length) {
	*status = TCL_ERROR;
	fprintf(stderr, "x_error vector too short\n");
	Tcl_DeleteHashEntry(entry_ptr);
 	return;
      }
    }
  }else if ((curve_instance->x_error = PowFindVector(x_error)) != NULL) {
    /*we've got an x_error but no x_vector */
    *status = TCL_ERROR;
    fprintf(stderr, "Can't have an X error without an X vector\n");
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  }
  if ((curve_instance->y_vector = PowFindVector(y_vector)) != NULL) {
    if(length == 0) {
      length = (curve_instance->y_vector)->length;
    } else {
      if( length > (curve_instance->y_vector)->length) {
	*status = TCL_ERROR;
	fprintf(stderr, "Y vector shorter than X vector\n");
	Tcl_DeleteHashEntry(entry_ptr);
	return;
      }
    }
    if ((curve_instance->y_error = PowFindVector(y_error)) != NULL) {
      if ((curve_instance->y_error)->length < length) {
	*status = TCL_ERROR;
	fprintf(stderr, "y_error vector too short\n");
	Tcl_DeleteHashEntry(entry_ptr);
	return;
      }
    }
  }else if ((curve_instance->y_error = PowFindVector(y_error)) != NULL) {
    /*we've got an y_error but no y_vector */
    *status = TCL_ERROR;
    fprintf(stderr, "Can't have a Y error without a Y vector\n");
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  }
  if ((curve_instance->z_vector = PowFindVector(z_vector)) != NULL) {
    if(length == 0) {
      length = (curve_instance->z_vector)->length;
    } else {
      if( length > (curve_instance->z_vector)->length) {
	*status = TCL_ERROR;
	fprintf(stderr, "Z vector too short.\n");
	Tcl_DeleteHashEntry(entry_ptr);
	return;
      }
    }
    if ((curve_instance->z_error = PowFindVector(z_error)) != NULL) {
      if ((curve_instance->z_error)->length < length) {
	*status = TCL_ERROR;
	fprintf(stderr, "z_error vector too short.\n");
	Tcl_DeleteHashEntry(entry_ptr);
	return;
      }
    }
  }else if ((curve_instance->z_error = PowFindVector(z_error)) != NULL) {
    /*we've got an z_error but no z_vector */
    *status = TCL_ERROR;
    fprintf(stderr, "Can't have a Z error without a Z vector\n");
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  }
  /* do final sanity checking */
  if(length == 0) {
    *status = TCL_ERROR;
    fprintf(stderr, "Invalid curve, no non-zero vectors.\n");
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  }  else {
    curve_instance->length = length;
  }
  /* if a vector not named "NULL" comes back NULL, it probably doesn't exist */
  /* error out */
  if ( x_vector != NULL && (strstr(x_vector,"NULL") == NULL ) && (curve_instance->x_vector == NULL)) {
    *status = TCL_ERROR;
    fprintf(stderr, "Vector %s doesn't exist.\n",x_vector);
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  }
  if ( x_error != NULL && (strstr(x_error,"NULL") == NULL ) && (curve_instance->x_error == NULL)) {
    *status = TCL_ERROR;
    fprintf(stderr, "Vector %s doesn't exist.\n",x_error);
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  }
  if ( y_vector != NULL && (strstr(y_vector,"NULL") == NULL) && (curve_instance->y_vector == NULL)) {
    *status = TCL_ERROR;
    fprintf(stderr, "Vector %s doesn't exist.\n",y_vector);
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  }
  if ( y_error != NULL && (strstr(y_error,"NULL") == NULL ) && (curve_instance->y_error == NULL)) {
    *status = TCL_ERROR;
    fprintf(stderr, "Vector %s doesn't exist.\n",y_error);
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  }
  if ( z_vector != NULL && (strstr(z_vector,"NULL") == NULL ) && (curve_instance->z_vector == NULL)) {
    *status = TCL_ERROR;
    fprintf(stderr, "Vector %s doesn't exist.\n",z_vector);
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  }
  if ( z_error != NULL && (strstr(z_error,"NULL") == NULL ) && (curve_instance->z_error == NULL)) {
    *status = TCL_ERROR;
    fprintf(stderr, "Vector %s doesn't exist.\n",z_error);
    Tcl_DeleteHashEntry(entry_ptr);
    return;
  }

  /* Call WCS init procedure if applicable */
  wcsStatus = TCL_ERROR;
  WCSstring = Tcl_GetVar2(interp,powWCS,curve_name,TCL_GLOBAL_ONLY);
  if( (WCSstring != NULL) && strcmp(WCSstring,"") ) {
    wcsStatus = Tcl_VarEval(interp, "powWCSInitCurve ", curve_name, " ",
                            WCSstring, (char *) NULL);
  }
  if ( wcsStatus == TCL_ERROR ) {
     /*  Set WCS structure to defaults... The Identity transform  */
     PowInitWCS( &curve_instance->WCS, 2 );
  }
}



void PowCreateHisto(char *histo_name, char *x_vector, char *y_vector,
		    int *status) {
  /*create 2 new vectors (named "histo_name"_histo_x and "histo_name"_histo_y)
    if x_vector is NULL or "NULL" create the x vector with integral numbered
    bins.  Then use these vectors to create a curve "histo_name" */

  PowVector *X,*Y;
  int i;
  double *dummy_X,*dummy_Y,*counter;
  double a,b,halfwidth;
  char *xname,*yname;
  int dtype = DOUBLE_DATA,copy = 0, offset = 0;
  int newflag;
  int histo_length;

  xname = ckalloc(strlen(x_vector) + 9);
  
  strcpy(xname,histo_name);
  strcat(xname,"_histo_x");

  if ((Y = PowFindVector(y_vector)) == NULL) {
    fprintf(stderr,"You must specify an existing Y vector.");
    *status = TCL_ERROR;
    return;
  }

  yname =  ckalloc(strlen(x_vector) + 9);

  strcpy(yname,histo_name);
  strcat(yname,"_histo_y");
  
  /* create Y histo data */
  dummy_Y = (double *)ckalloc((Y->length * 2 + 2) * sizeof(double));
  
  counter = dummy_Y;
  *counter++ = 0;
  for (i=0; i < Y->length; i++) {
    a = PowExtractDatum(Y->dataptr,i);
    *counter++ = a;
    *counter++ = a;
  }
  *counter++ = 0;
  

  if ((X = PowFindVector(x_vector)) == NULL) {
    newflag = 1;
  } else {
    newflag = 0;
    if (X->length < Y->length) {
      fprintf(stderr,"X vector too short.");
      *status = TCL_ERROR;
      return;
    }
  }
   
  /* create X histo data */
    
  dummy_X = (double *)ckalloc((Y->length * 2 + 2) * sizeof(double));
    
  counter = dummy_X;
  
  if (newflag) {
    *counter++ = 0.5;
    for (i=1; i <= Y->length; i++) {
      *counter++ = i - 0.5;
      *counter++ = i + 0.5;
    } 
    *counter++ = i + 0.5;

  } else {
    a = PowExtractDatum(X->dataptr,0);
    b = PowExtractDatum(X->dataptr,1);
    halfwidth = (b - a)/2.0;
    *counter++ = a - halfwidth;
    *counter++ = a - halfwidth;
    *counter++ = a + halfwidth;
    

    for (i=1; i < Y->length; i++) {
      b = PowExtractDatum(X->dataptr,i);
      halfwidth = (b - a)/2.0;
      *counter++ = b - halfwidth;
      *counter++ = b + halfwidth;
      a = b;
    }
    *counter++ = b + halfwidth;
  }



  histo_length = Y->length * 2 + 2;

  PowCreateData( xname, dummy_X, &dtype, &histo_length, &copy, status);
  PowCreateVector( xname, xname, &offset, &histo_length, "NULL", status);
  PowCreateData( yname, dummy_Y, &dtype, &histo_length, &copy, status);
  PowCreateVector( yname, yname, &offset, &histo_length, "NULL", status);
  PowCreateCurve( histo_name, xname, NULL, yname, NULL, NULL, NULL, status);

  return;
}

void PowDestroyCurve(char *curve_name, int *status) {
  Tcl_HashEntry *entry_ptr;
  char errormsg[1024];
  PowCurve *curve_ptr;
  
  entry_ptr = Tcl_FindHashEntry(&PowCurveTable,curve_name);
  
  if (entry_ptr == NULL) {
    *status = TCL_ERROR;
    sprintf(errormsg,"Can't find POWCurve Object %s to destroy",curve_name);
    Tcl_SetResult(interp,errormsg,TCL_VOLATILE);
    return;
  }


  curve_ptr = (struct PowCurve *)Tcl_GetHashValue(entry_ptr);

  /*Delete the entry from the master POWData Hash*/
  Tcl_DeleteHashEntry(entry_ptr);
  
  /*free the PowCurve memory itself and the string holding the name,
     although this is small change*/
  ckfree(curve_ptr->curve_name);
  ckfree((char*)curve_ptr);
  return;
}

void PowCreateCurveFlip (char *graphName, char *direction, int *status) {
    PowCurve *current_curve;
    PowVector *Xvec, *Yvec;
    double xdatum, ydatum;
    int i, j;
    char curveName[1024];

    sprintf(curveName, "c1_%s", graphName);
    status = 0;
    current_curve = PowFindCurve(curveName);

    Xvec = current_curve->x_vector;
    Yvec = current_curve->y_vector;

    for (i = Xvec->offset, j = Yvec->offset ; i < Xvec->offset + current_curve->length ; i++, j++) {
        xdatum = PowExtractDatum(Xvec->dataptr,i);
        ydatum = PowExtractDatum(Yvec->dataptr,j);
        if (strcmp(direction, "X")) {
        /*   PowPutDatum(Xvec->dataptr, (double)i, i); */
        } else if (strcmp(direction, "Y")) {
        /*   PowPutDatum(Yvec->dataptr, (double)j, j); */
        }
    }
}
