#ifdef __WIN32__
#include <windows.h>
#endif
#include "pow.h"
#include "stretcharrow.xbm"

	   
/*
 *----------------------------------------------------------------------
 *
 * DllEntryPoint --
 *
 *	This wrapper function is used by Windows to invoke the
 *	initialization code for the DLL.  If we are compiling
 *	with Visual C++, this routine will be renamed to DllMain.
 *	routine.
 *
 * Results:
 *	Returns TRUE;
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

#ifdef __WIN32__
int WINAPI
dllEntry(HANDLE hDll, DWORD reason, LPVOID reserved)
{
    return TRUE;
}
#endif
 

int pixelSizes[6] = {1,2,4,4,8,8};
char *WCSpih_Message[] = { "Success.",
                           "Null wcsprm pointer passed.",
                           "Memory allocation failed.",
                           "Linear transformation matrix is singular.",
                           "Inconsistent or unrecognized coordinate axis types.",
                           "Invalid parameter value.",
                           "Invalid coordinate transformation parameters.",
                           "Ill-conditioned coordinate transformation parameters.",
                           "One or more of the world coordinates were invalid." };

char *WCStrans_Message[] = {
                          "Success",
                          "Null wcsprm pointer passed",
                          "Memory allocation failed",
                          "Linear transformation matrix is singular",
                          "Inconsistent or unrecognized coordinate axis types",
                          "Invalid parameter value",
                          "Invalid coordinate transformation parameters",
                          "Ill-conditioned coordinate transformation parameters",
                          "One or more of the pixel coordinates were invalid",
                          "One or more of the world coordinates were invalid",
                          "Invalid world coordinate",
                          "No solution found in the specified interval",
                          "Invalid subimage specification",
                          "Non-separable subimage coordinate system"};

/* globals linked with tcl variables */

int tty = 0;
int Pow_Done = 0;  /* current method for event handling, will change */
#
extern int Pow_Allocated;

Tcl_HashTable PowDataTable;
Tcl_HashTable PowImageTable;
Tcl_HashTable PowVectorTable;
Tcl_HashTable PowCurveTable;
Tcl_HashTable PowGraphTable;


Tcl_Interp *interp = NULL;		/* Interpreter for application. */
Tk_Window mainWindow;	/* The main window for the application.  If
				 * NULL then the application no longer
				 * exists. */

/*This stuff currently lives here for plugin purposes */
#ifdef PLUGIN
extern int singleBarFastGen(ClientData clientData, Tcl_Interp *interp,
			    int argc, char *argv[]);
extern int singleRollMe(ClientData clientData, Tcl_Interp *interp,
			int argc, char *argv[]);
extern int c_given_RADecMJD_return_Roll(ClientData clientData,
					Tcl_Interp *interp,
					int argc, char *argv[]);

#endif /*PLUGIN */



int Pow_Init(Tcl_Interp *interp_instance) {
  Tcl_DString pow_env;
  const char *charptr;
  char temp[1000];

  Tcl_DStringInit(&pow_env);


  interp = interp_instance;


  if (Visu_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }

#if defined(MAC_TCL) && defined(PLUGIN)


   strcpy(temp,"source -rsrc html_library\n");
   strcat(temp,"source -rsrc notebook\n");
   strcat(temp,"source -rsrc pow\n");
   strcat(temp,"source -rsrc powEdit\n");
   strcat(temp,"source -rsrc powRgn\n");
   strcat(temp,"source -rsrc powMovie\n");
   strcat(temp,"source -rsrc powScript\n");
   strcat(temp,"powInitGlobals\n");
   if( Tcl_GlobalEval(interp,temp) == TCL_ERROR ) {
   	return TCL_ERROR;
   }

#else

  charptr = Tcl_GetVar2(interp,"env", "POW_LIBRARY", TCL_GLOBAL_ONLY);
  if( charptr == NULL ) {
      puts("Could not find defaults.");
      puts("  Set your POW_LIBRARY environment variable.");
      return TCL_ERROR;
  } else {
    Tcl_DStringAppend(&pow_env,charptr, -1);
  }

  /*  Brackets needed around %s to prevent Windows-style paths  */
  /*  ... eg, c:\fv... from getting converted to control chars  */
  /*  when doing the GlobalEval.                                */
  sprintf(temp,"lappend auto_path {%s}; powInitGlobals",charptr);
  if( Tcl_GlobalEval(interp,temp) == TCL_ERROR )
     return TCL_ERROR;

  /*  Lets try using auto_path instead of all these sources....
	
  Tcl_SetVar(interp, "powsrcdir",Tcl_DStringValue(&pow_env), TCL_GLOBAL_ONLY);

  Tcl_DStringInit(&pow_script);
  Tcl_DStringAppend(&pow_script,Tcl_DStringValue(&pow_env),-1);
  Tcl_DStringAppend(&pow_script,"/html_library.tcl",-1);
  if( Tcl_EvalFile(interp,Tcl_DStringValue(&pow_script))  == TCL_ERROR) {
    fprintf(stderr, "%s\n", interp->result);
    return TCL_ERROR;
  }
  Tcl_DStringFree(&pow_script);

  Tcl_DStringInit(&pow_script);
  Tcl_DStringAppend(&pow_script,Tcl_DStringValue(&pow_env),-1);
  Tcl_DStringAppend(&pow_script,"/notebook.tcl",-1);
  if( Tcl_EvalFile(interp,Tcl_DStringValue(&pow_script))  == TCL_ERROR) {
    fprintf(stderr, "%s\n", interp->result);
    return TCL_ERROR;
  }
  Tcl_DStringFree(&pow_script);

  Tcl_DStringInit(&pow_script);
  Tcl_DStringAppend(&pow_script,Tcl_DStringValue(&pow_env),-1);
  Tcl_DStringAppend(&pow_script,"/pow.tcl",-1); 
  if( Tcl_EvalFile(interp,Tcl_DStringValue(&pow_script))  == TCL_ERROR) {
    fprintf(stderr, "%s\n", interp->result);
    return TCL_ERROR;
  }
  Tcl_DStringFree(&pow_script);

  Tcl_DStringInit(&pow_script);
  Tcl_DStringAppend(&pow_script,Tcl_DStringValue(&pow_env),-1);
  Tcl_DStringAppend(&pow_script,"/powEdit.tcl",-1); 
  if( Tcl_EvalFile(interp,Tcl_DStringValue(&pow_script))  == TCL_ERROR) {
    fprintf(stderr, "%s\n", interp->result);
    return TCL_ERROR;
  }
  Tcl_DStringFree(&pow_script);

  Tcl_DStringInit(&pow_script);
  Tcl_DStringAppend(&pow_script,Tcl_DStringValue(&pow_env),-1);
  Tcl_DStringAppend(&pow_script,"/powRgn.tcl",-1); 
  if( Tcl_EvalFile(interp,Tcl_DStringValue(&pow_script))  == TCL_ERROR) {
    fprintf(stderr, "%s\n", interp->result);
    return TCL_ERROR;
  }
  Tcl_DStringFree(&pow_script);
  */

#endif  /*  MAC_TCL  and PLUGIN*/ 
  
  Tcl_InitHashTable(&PowDataTable, TCL_STRING_KEYS);
  Tcl_InitHashTable(&PowImageTable, TCL_STRING_KEYS);
  Tcl_InitHashTable(&PowVectorTable, TCL_STRING_KEYS);
  Tcl_InitHashTable(&PowCurveTable, TCL_STRING_KEYS);
  Tcl_InitHashTable(&PowGraphTable, TCL_STRING_KEYS);

  Tk_DefineBitmap(interp, Tk_GetUid("stretcharrow"), stretcharrow_bits,
            stretcharrow_width, stretcharrow_height);




#if !(defined(__WIN32__) || defined(macintosh))

  /*  Keeps track of whether Pow has allocated its PseudoColor cells or not  */
  strcpy(temp,"Pow_Allocated");
  Tcl_LinkVar(interp,temp,(char *) &Pow_Allocated,TCL_LINK_INT);

#endif

  Pow_CreateCommands(interp); 

  Tk_CreateItemType(&tkPowCurveType);
  return TCL_OK;
}

/* initialization only for Unix standalone */
int Pow_InitExec(Tcl_Interp *interp_instance) {
  Tcl_DString pow_env;
#if !(defined(__WIN32__) || defined(macintosh))
  char temp[1000];
#endif

  Tcl_DStringInit(&pow_env);


  interp = interp_instance;


  if (Visu_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }

#if defined(MAC_TCL) && defined(PLUGIN)


   strcpy(temp,"source -rsrc html_library\n");
   strcat(temp,"source -rsrc notebook\n");
   strcat(temp,"source -rsrc pow\n");
   strcat(temp,"source -rsrc powEdit\n");
   strcat(temp,"source -rsrc powRgn\n");
   strcat(temp,"source -rsrc powMovie\n");
   strcat(temp,"source -rsrc powScript\n");
   strcat(temp,"powInitGlobals\n");
   if( Tcl_GlobalEval(interp,temp) == TCL_ERROR ) {
   	return TCL_ERROR;
   }

#else


#endif  /*  MAC_TCL  and PLUGIN*/ 
  
  Tcl_InitHashTable(&PowDataTable, TCL_STRING_KEYS);
  Tcl_InitHashTable(&PowImageTable, TCL_STRING_KEYS);
  Tcl_InitHashTable(&PowVectorTable, TCL_STRING_KEYS);
  Tcl_InitHashTable(&PowCurveTable, TCL_STRING_KEYS);
  Tcl_InitHashTable(&PowGraphTable, TCL_STRING_KEYS);

  Tk_DefineBitmap(interp, Tk_GetUid("stretcharrow"), stretcharrow_bits,
            stretcharrow_width, stretcharrow_height);




#if !(defined(__WIN32__) || defined(macintosh))

  /*  Keeps track of whether Pow has allocated its PseudoColor cells or not  */
  strcpy(temp,"Pow_Allocated");
  Tcl_LinkVar(interp,temp,(char *) &Pow_Allocated,TCL_LINK_INT);

#endif

  Pow_CreateCommands(interp); 

  Tk_CreateItemType(&tkPowCurveType);
  return TCL_OK;
}

/* Create the Pow Commands */
int Pow_CreateCommands(Tcl_Interp *interp) {

  Tcl_CreateObjCommand(interp,"powWCSInitImage",PowWCSInitImage,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp,"powWCSInitCurve",PowWCSInitCurve,
                       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp, "powWCSexists", PowWCSexists,
		       (ClientData) NULL, 
		       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp, "powWCSisSwapped", PowWCSisSwapped,
		       (ClientData) NULL, 
		       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powGetImageOrigin",PowGetImageOrigin,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powGetImageOtherend",PowGetImageOtherend,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powGetImageUnits",PowGetImageUnits,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powDestroyData",PowDestroyData_Tcl,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powDestroyImage",PowDestroyImage_Tcl,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powDestroyVector",PowDestroyVector_Tcl,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powDestroyCurve",PowDestroyCurve_Tcl,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powDestroyGraph",PowDestroyGraph_Tcl,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powCreateImage",PowCreateImage_Tcl,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powCreateGraph",PowCreateGraph_Tcl,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powFindData",PowFindData_Tcl,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powFindCurvesMinMax",PowFindCurvesMinMax_Tcl,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powFetchDataLength",PowFetchDataLength,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powFetchCurveInfoHash",PowFetchCurveInfoHash,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powFetchVectorInfoHash",PowFetchVectorInfoHash,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powFetchImageInfoHash",PowFetchImageInfoHash,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powCreateVector",PowCreateVector_Tcl,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powCreateVectorEN",PowCreateVectorEN_Tcl,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powCreateCurve",PowCreateCurve_Tcl,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powCreateHisto",PowCreateHisto_Tcl,
		    (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powCreateDataFlip",PowCreateDataFlip_Tcl,
                    (ClientData) NULL,
                    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powCreateCurveFlip",PowCreateCurveFlip_Tcl,
                    (ClientData) NULL,
                    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powCreateData",PowCreateData_Tcl,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powCloneData",PowCloneData,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powRegisterData",PowRegisterData_Tcl,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powCreateDataFromList",PowCreateDataFromList,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);

  Tcl_CreateObjCommand(interp,"powCreateDataFromBuffer",
                       PowCreateDataFromBuffer,
                       (ClientData) NULL, 
                       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp,"powCreateDataFromChannel",
                       PowCreateDataFromChannel,
                       (ClientData) NULL, 
                       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp,"powCreateDataFromPtr",
                       PowCreateDataFromPtr,
                       (ClientData) NULL, 
                       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp,"powCreateStrFromPtr",
                       PowCreateStrFromPtr,
                       (ClientData) NULL, 
                       (Tcl_CmdDeleteProc *) NULL);

  Tcl_CreateCommand(interp,"powSetGraphMagstep",PowSetGraphMagstep,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powProcessCurve",PowProcessCurve,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powListGraphs",PowListGraphs,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powListCurves",PowListCurves,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powListImages",PowListImages,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powListVectors",PowListVectors,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powListData",PowListData,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powCleanUp",PowCleanUp,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powSetupColormap",PowSetupColormap,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powSetupPhotoImages",PowSetupPhotoImages,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powTestColormap",PowTestColormap,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powPutZoomedBlock",PowPutZoomedBlock,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powPhotoColorTable",PowPhotoColorTable,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateCommand(interp,"powReditherPhotoBlock",PowReditherPhotoBlock,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp,"powTestMacMemory",PowTestMacMemory,
                       (ClientData) NULL, 
                       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp,"powPhotoCmapStretch",PowPhotoCmapStretch,
                       (ClientData) NULL, 
                       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp, "powImageScale", PowImageScale,
		       (ClientData) NULL, 
		       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp, "powGetHisto", PowGetHisto,
		       (ClientData) NULL, 
		       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp,"powGetImageZ",PowGetImageZ,
                       (ClientData) NULL, 
                       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp,"powWorldPos",PowWorldPos,
                       (ClientData) NULL, 
                       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp,"powXYPx",PowXYPx,
                       (ClientData) NULL, 
                       (Tcl_CmdDeleteProc *) NULL);
  
  Tcl_CreateObjCommand(interp, "powGraphToCanvas", PowGraphToCanvas,
		       (ClientData) NULL, 
		       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp, "powCanvasToGraph", PowCanvasToGraph,
		       (ClientData) NULL, 
		       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp, "powGraphToPixel",  PowGraphToPixel,
		       (ClientData) NULL, 
		       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp, "powPixelToGraph",  PowPixelToGraph,
		       (ClientData) NULL, 
		       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp, "powResetWcsStructure",  PowResetWcsStructure,
		       (ClientData) NULL, 
		       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp, "powGraphVToPixelV",  PowGraphVToPixelV,
		       (ClientData) NULL, 
		       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp, "powPixelVToGraphV",  PowPixelVToGraphV,
		       (ClientData) NULL, 
		       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp, "powGetImageClipbox", PowGetImageClipbox,
		       (ClientData) NULL, 
		       (Tcl_CmdDeleteProc *) NULL);

  Tcl_CreateObjCommand(interp, "powExprGetData", PowExprDataInfo,
		       (ClientData) NULL, 
		       (Tcl_CmdDeleteProc *) NULL);

  Tcl_CreateObjCommand(interp, "powDataPtr", PowDataPtr_Tcl,
		       (ClientData) NULL, 
		       (Tcl_CmdDeleteProc *) NULL);

  Tcl_CreateObjCommand(interp, "powTestImage", PowTestImage,
		       (ClientData) NULL, 
		       (Tcl_CmdDeleteProc *) NULL);

  Tcl_CreateCommand(interp,"powDrawGridLines",PowDrawGridLines,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);

  Tcl_CreateCommand(interp,"powCreateContour",PowCreateContour,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);

  Tcl_CreateCommand(interp,"powGetTics",PowGetTics,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);

  Tcl_CreateCommand(interp,"powGetRegionStatistics", PowGetRegionStatistics, 
		    (ClientData) NULL, 
                    (Tcl_CmdDeleteProc*) NULL); 

#ifdef PLUGIN
  Tcl_CreateCommand(interp_instance,"singleBarFastGen",
		    (Tcl_CmdProc*)singleBarFastGen,
		    (ClientData) NULL, (Tcl_CmdDeleteProc*) NULL);
  Tcl_CreateCommand(interp_instance,"singleRollMe",
		    (Tcl_CmdProc*)singleRollMe,
		    (ClientData) NULL, (Tcl_CmdDeleteProc*) NULL);
  Tcl_CreateCommand(interp_instance,"c_given_RADecMJD_return_Roll",
		    (Tcl_CmdProc*)c_given_RADecMJD_return_Roll,
		    (ClientData) NULL, (Tcl_CmdDeleteProc*) NULL);
  
#endif /*PLUGIN */

  return TCL_OK;
}


void PowInit(char *powSetupColormapArgs, char *powInitArgs, int *status) {
  /*call this one from a standalone C or FORTRAN main (as opposed to a 
   tkAppInit file) */

  if (*status != 0) return;

    
  if (interp == NULL || Tcl_InterpDeleted(interp)) {
    /* the test condition above should ensure that we only create a new
       interpreter if we don't have one already */
    interp = Tcl_CreateInterp();
  
  
    if (Tcl_Init(interp) == TCL_ERROR) {
      *status =  TCL_ERROR;
      fprintf(stderr, "%s\n", Tcl_GetStringResult(interp));
      return;
    }
    if (Tk_Init(interp) == TCL_ERROR) {
      *status =  TCL_ERROR;
      fprintf(stderr, "%s\n", Tcl_GetStringResult(interp));
      return;
    }

    tty = isatty(0);
    Tcl_SetVar(interp, "tcl_interactive",
	       tty ? "1" : "0", TCL_GLOBAL_ONLY);

    Tcl_LinkVar(interp,"Pow_Done",(char *) &Pow_Done,TCL_LINK_INT);    /* currently used for event handling */


    *status = Pow_Init(interp);
  
    if (*status != 0) return;
  }
  
  if (Tcl_RegExpMatch(interp,powSetupColormapArgs,"[^ \t\n\r\f]") == 1) {
    /*if user supplied args for powSetupColormap are not pure whitespace...*/
    if(Tcl_VarEval(interp, "powSetupColormap ", powSetupColormapArgs,
		   (char *) NULL) == TCL_ERROR) {
      *status = TCL_ERROR;
      fprintf(stderr, "Error initializing POW.\n%s\n", Tcl_GetStringResult(interp));
    }
  }
  
  if(Tcl_VarEval(interp, "powInit ", powInitArgs, (char *) NULL) == TCL_ERROR) {
    *status = TCL_ERROR;
    fprintf(stderr, "Error initializing POW.\n%s\n", Tcl_GetStringResult(interp));
  }

}
