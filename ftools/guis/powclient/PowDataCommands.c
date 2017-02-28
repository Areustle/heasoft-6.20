#include "powdata.h"

/* on some system , e.g. linux, SUNs DBL_MAX is in float.h */
#ifndef DBL_MAX
#include <float.h>
#endif

#ifndef DBL_MIN
#include <float.h>
#endif

#ifdef macintosh
#include <MacMemory.h>
#endif


int PowCreateDataFromChannel(ClientData clientData, Tcl_Interp *interp,
                             int argc, Tcl_Obj *const argv[] )
{
  /* usage: powCDFC channel data_name bitpix byteOrder */
  Tcl_Channel channel;
  char buffer[1024];
  char *data, *cName, *dName, *bin, *bout;
  int done=0, copy=-1, status=0, i, j;
  int nPts, datasize, bitpix, byteOrder, bytesRead, len=0;

  if( argc!=5 ) {
    Tcl_SetResult(interp, "usage: powCreateDataFromChannel chanName "
                  "data_name bitpix byteOrder", TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  cName = Tcl_GetStringFromObj( argv[1], NULL );
  dName = Tcl_GetStringFromObj( argv[2], NULL );
  Tcl_GetIntFromObj( interp, argv[3], &bitpix );
  Tcl_GetIntFromObj( interp, argv[4], &byteOrder );
  if( bitpix<0 || bitpix>4 ) {
    Tcl_SetResult(interp, "Unsupported bitpix value", TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  channel = Tcl_GetChannel( interp, cName, NULL );
  if( channel==NULL ) {
    Tcl_AppendResult(interp, "Unable to find channel ", cName, NULL);
    return TCL_ERROR;
  }

  switch (bitpix) {
  case 0: datasize = 1; break;
  case 1: datasize = 2; break;
  case 2: datasize = 4; break;
  case 3: datasize = 4; break;
  case 4: datasize = 8; break;
  }

  while( !done ) {
    bytesRead = Tcl_Read( channel, buffer, 1024 );
  
    if( len==0 ) {
      len = bytesRead;
      data = (char *) ckalloc( bytesRead * sizeof(char) );
    } else if( bytesRead>0 ) {
      len += bytesRead;
      data = (char *) ckrealloc( data, len * sizeof(char) );
      if( bytesRead<1024 ) done=1;
    } else if( bytesRead==0 ) {
      done=1;
    } else {
      /* ERROR */
      if( len>0 ) ckfree( data );
      Tcl_AppendResult(interp, "Error reading channel", NULL);
      return TCL_ERROR;
    }
      
    if( byteOrder>0 || datasize==1 ) {
      memcpy( data+len-bytesRead, buffer, bytesRead );
    } else {
      bin  = buffer;
      bout = data+len-bytesRead;
      nPts = bytesRead/datasize;
      for( i=0; i<nPts; i++ ) {
        for( j=0; j<datasize; j++ )
          bout[datasize-j-1] = *bin++;
        bout += datasize;
      }
    }

  } /* end while() */

  nPts = len/datasize;
  PowCreateData(dName,data,&bitpix,&nPts,&copy,&status);
  if( status ) {
    Tcl_AppendResult(interp, "Error creating data ", dName, NULL);
    return TCL_ERROR;
  }
  return TCL_OK;
}

int PowCreateDataFromPtr(ClientData clientData, Tcl_Interp *interp,
                         int argc, Tcl_Obj *const argv[] )
{
  Tcl_Obj **dList;
  char *data, *dPtr, *dName, *bin, *bout;
  int done=0, copy=-1, status=0, i, j;
  int nPts, datasize, bitpix, byteOrder;

  if( argc!=4 ) {
    Tcl_SetResult(interp, "usage: powCreateDataFromPtr dataPtr "
                  "data_name byteOrder", TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  /*  Parse dataPtr of form [address bitpix [naxes]]  */
  if( Tcl_ListObjGetElements( interp, argv[1], &i, &dList ) != TCL_OK
      || i != 3 ) {
     Tcl_AppendResult(interp, 
                      "Bad dataList parameter: address dataType naxes",
                      TCL_VOLATILE);
     return TCL_ERROR;
  }
  dPtr   = Tcl_GetStringFromObj( dList[0], NULL );
  if (sscanf(dPtr,PTRFORMAT,&data) != 1) {
    Tcl_SetResult(interp,"Couldn't parse data address into an integer",
                  TCL_VOLATILE);
    return TCL_ERROR;
  }
  Tcl_GetIntFromObj( interp, dList[1], &bitpix );

  /*  Read NAXES list and calculate data length  */

  if( Tcl_ListObjGetElements( interp, dList[2], &i, &dList ) != TCL_OK ) {
     Tcl_AppendResult(interp, "Bad naxes parameter", TCL_VOLATILE);
     return TCL_ERROR;
  }
  for( nPts=1; i; ) {
     Tcl_GetIntFromObj( interp, dList[--i], &j );
     nPts *= j;
  }

  dName  = Tcl_GetStringFromObj( argv[2], NULL );
  Tcl_GetIntFromObj( interp, argv[3], &byteOrder );
  if( bitpix<0 || bitpix>4 ) {
    Tcl_SetResult(interp, "Unsupported bitpix value", TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  switch (bitpix) {
  case 0: datasize = 1; break;
  case 1: datasize = 2; break;
  case 2: datasize = 4; break;
  case 3: datasize = 4; break;
  case 4: datasize = 8; break;
  }

  if( byteOrder>0 || datasize==1 ) {
     copy = 1;
  } else {
     bin  = (char *)data;
     bout = (char *)ckalloc( nPts * datasize * sizeof(char) );
     for( i=0; i<nPts; i++ ) {
        for( j=0; j<datasize; j++ )
           bout[datasize-j-1] = *bin++;
        bout += datasize;
     }
     copy = -1;  /*  Have POW take ownership of new data array  */
     data = bout;
  }

  PowCreateData(dName,data,&bitpix,&nPts,&copy,&status);
  if( status ) {
    Tcl_AppendResult(interp, "Error creating data ", dName, NULL);
    return TCL_ERROR;
  }
  return TCL_OK;
}

int PowCreateDataFromList(ClientData clientData, Tcl_Interp *interp, 
			  int argc, char *argv[]) {
  /* usage: powCreateDataFromList data_name list_o_data*/
  int largc;
  char **largv, **counter;
  double *data, *datacounter;
  int i,j;
  int status = 0;
  PowData *data_instance;
  char ptrString[40];
  int string_flag = 0;

  if (argc < 3 || argc > 4) {
    interp->result = "usage: powCreateDataFromList data_name list_o_data ?stringflag?";
    return TCL_ERROR;
  }

  if (Tcl_SplitList(interp,argv[2],&largc,&largv) != TCL_OK) {
    Tcl_SetResult(interp,"Couldn't split input data list",TCL_VOLATILE);
    return TCL_ERROR;
  }


  if (argc == 4) {
    if (Tcl_GetBoolean(interp,argv[3],&string_flag) != TCL_OK) {
      Tcl_SetResult(interp,"Couldn't convert stringflag to boolean",
		    TCL_VOLATILE);
      return TCL_ERROR;
    }
  }

  if (string_flag) {
    i=0;
    j=STRING_DATA;
    
    PowCreateData(argv[1],(void *)largv,&j,&largc,&i,&status);

  } else {
    data = (double *)ckalloc(largc * sizeof(double));
    
    counter = largv;
    datacounter = data;
  
    for (i=0;i<largc;i++) {
      Tcl_GetDouble(interp,*counter,datacounter);
      counter++;
      datacounter++;
    }
  
  /*  ckfree((char *) largv);*/

    i=0;
    j=DOUBLE_DATA;

    PowCreateData(argv[1],data,&j,&largc,&i, &status);
  }

  if ( status != 0) {
    Tcl_SetResult(interp,"Couldn't create data",TCL_VOLATILE); 
    return TCL_ERROR;
  }

  data_instance = PowFindData(argv[1]);

  /*Since this data was made by us, we'll mark it as a POW copy so that
    PowDestroyData will free it.*/
  data_instance->copy = 1;
  

  /*Return the string representation of the PowData pointer */
  sprintf(ptrString,PTRFORMAT,data_instance);
  Tcl_SetResult(interp,ptrString,TCL_VOLATILE);
  

  return TCL_OK;
}
  
int PowCreateStrFromPtr(ClientData clientData, Tcl_Interp *interp,
                        int argc, Tcl_Obj *const argv[] )
{
  Tcl_Obj **dList;
  char *data, *dPtr, *dName, *bin, *bout;
  int done=0, copy=-1, status=0, i, j;
  int nPts, datasize, bitpix, byteOrder;

  if( argc!=4 ) {
    Tcl_SetResult(interp, "usage: powCreateStrFromPtr "
                  "address bitpix naxes", TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  dPtr   = Tcl_GetStringFromObj( argv[1], NULL );
  if (sscanf(dPtr,PTRFORMAT,&data) != 1) {
    Tcl_SetResult(interp,"Couldn't parse data address into an integer",
                  TCL_VOLATILE);
    return TCL_ERROR;
  }
  Tcl_GetIntFromObj( interp, argv[2], &bitpix );

  /*  Read NAXES list and calculate data length  */

  if( Tcl_ListObjGetElements( interp, argv[3], &i, &dList ) != TCL_OK ) {
     Tcl_AppendResult(interp, "Bad naxes parameter", TCL_VOLATILE);
     return TCL_ERROR;
  }
  for( nPts=1; i; ) {
     Tcl_GetIntFromObj( interp, dList[--i], &j );
     nPts *= j;
  }


  if( bitpix<0 || bitpix>4 ) {
    Tcl_SetResult(interp, "Unsupported bitpix value", TCL_VOLATILE);
    return TCL_ERROR;
  }
  
  switch (bitpix) {
  case 0: datasize = 1; break;
  case 1: datasize = 2; break;
  case 2: datasize = 4; break;
  case 3: datasize = 4; break;
  case 4: datasize = 8; break;
  }

  Tcl_SetObjResult( interp, Tcl_NewStringObj( data, nPts * datasize ) );
  return TCL_OK;
}

