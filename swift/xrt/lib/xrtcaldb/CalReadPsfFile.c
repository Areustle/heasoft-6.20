/*
 *
 *
 *	CalReadPsfFile: --- Read Swif Calibration PSF Parameter File ---
 *
 *
 *	DESCRIPTION:
 *         This function reads the parameters from an XRT PSF Parameter file
 *         for an analitical computation of the PSF. The parameters are
 *         read into the structure: PsfPar_t (see xrtcaldb.h) - one for
 *         off-axis angle.
 *     
 *
 *	DOCUMENTATION:
 *         The Algoritm used to calculate the PSF has been taken
 *         from Sergio Campana PSF Report.
 *
 *
 *      FUNCTION CALL
 *          static BOOL CalReadPsfFile(BOOL FromCaldb,
 *					     const char *CalFileName,
 *                                           PsfPar_t *PsfParm)
 *	CHANGE HISTORY:
 *        0.1.0: - FT 06/03/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 */

#define CALREADPSF_C
#define CALREADPSF_VERSION			"0.1.0"

#define FITSERR(a, b)                               \
	if (status) {				    \
		char errtxt[FLEN_STATUS];	    \
		FCGERR(status, errtxt);		    \
		Error(CSTR(a), "FITSIO routine " CSTR(b)	\
		  " returned error #%d: %s", status, errtxt);	\
		goto read_end;					\
	}

		/********************************/
		/*        header files          */
		/********************************/

#include "CalReadPsfFile.h"

                 /********************************/
                 /*         definitions          */
                 /********************************/

/* input parameter names */

int CalReadPsfFile(int FromCaldb, const char *CalFileName,
		   PsfPar_t *PsfParm,
		   int OnAxis )
{
  /*
   * int FromCaldb - Logical: if true the most recent PSF parameter file
   *                  will be read from CALDB (not yet implemented)
   * const char *CalFileName - if the FromBaldb is set to false and the
   *                           CalFileName is empty, the file will be read
   *                           from a staging area [TBD]
   * PsfParm   - output structure array that will be fill by parameters for
   *             psf on/off axis computation. The number of structure to be
   *             be fill (see xrtcaldb.h for structure description)
   * OnAxis - Logical: if true only on-axis parameters will be fill. 
   */

  char           PsfFileName[MAXFNAME_LEN]; 
  char           Comment[FLEN_COMMENT];
  int            status, i, ii;
  int            numhdu;               /* Number of HDUs of the PSF File */
  int            exti;                 /*   index loops on extensions    */
  FitsFileUnit_t PsfFileUnit;
  FitsHeader_t   PsfHead;
  Bintable_t     PsfTable;
  char           TmpInstrUnit[7];


  TMEMSET0( &PsfTable, Bintable_t );
  TMEMSET0( &PsfHead,  FitsHeader_t );

  /* HEADAS call to set the running taskname and version */

  strcpy(TmpInstrUnit, KWVL_INSTRUME); /* XRT Instrument */

  if (CalGetFileName( FromCaldb, KWVL_PSFRAD_DSET, CalFileName ))
    return NOT_OK;

  headas_chat(NORMAL, "CalReadPsfFile_%s: Using file `%s' to obtain the Psf parameters: \n ", CALREADPSF_VERSION,
				PsfFileName);

  /*
   *  try to open calibration file and check whether it is a
   *  Swift XRT Psf file
   */
  status = 0;
  if( (PsfFileUnit = OpenReadFitsFile(PsfFileName)) 
	 <= (FitsFileUnit_t ) 0) {
    Error("CalReadPsfFile", "Could not open Psf Parameter File\n");
    return NOT_OK;
  }

  /* Check whether it is a Swift XRT File */
  ISXRTFILE_WARNING(NORMAL,PsfFileUnit,PsfFileName,"CalReadPsfFile");

  if (!KeyWordMatch(PsfFileUnit, KWNM_PSFRAD_DSET, S, KWVL_PSFRAD_DSET, CPNULL)) {
    Error("CalReadPsfFile", "`%s' is not a PSFRAD CALDB File", PsfFileName);
    return NOT_OK;
  }

#define KEYERR(a, b)				\
    if (status) {			    \
      Error("CalReadPsfFile", "Could not get "	  \
	  "value of header keyword `%s'", b);		\
      FITSERR(CalReadPsfFile, a);	    \
    }

  /* Get how many hdu are present in file */

  if ( fits_get_num_hdus(PsfFileUnit, &numhdu, &status))
    {
      headas_chat(MUTE,"CalReadPsfFile_%s: Unable to get number of hdus in file\n",CALREADPSF_VERSION);
      headas_chat(MUTE,"CalReadPsfFile_%s: The file '%s' could be corrupted\n",
		  CALREADPSF_VERSION,
		  PsfFileName );
      goto read_end;
    }

  /* 
   * Check if the number of extensions are equal to the 
   * expected ones (+primary header) 
   */
  if ( numhdu != (NUMB_PSF_HDU+1) ) {
      headas_chat(MUTE,
		  "CalReadPsfFile_%s: Number of PSF Calibration file extensions (%d)\n",CALREADPSF_VERSION,numhdu);
      headas_chat(MUTE,
		  "CalReadPsfFile_%s: does not match the expected ones (%d)\n",CALREADPSF_VERSION, NUMB_PSF_HDU+1   );
      goto read_end;
  }

  /* 
   * If OnAxis tre Read only the first extension of the PSF file
   * with on-axis parameters.
   */
  if( OnAxis ) numhdu = 2;
  PsfParm->numb_off_axis = 0; 

  /*
   * Loop on extensions and fill the PsfParm structure.
   */
  for ( exti=2; exti<=numhdu; exti++ ) {

    HDU_t HDU_Type;
    unsigned ReadRows;
    int      paridx;     /* Index loops on PsfParm array */
    int      rowidx;     /* Index loops on table rows    */
    int      pnameidx;   /* Index loops on Table Rows    */
    int      p_name_col; /* Number of P_NAME column in PSF File */ 
    int      p_value_col; /* Number of P_VAL column in PSF File */

    status = 0;

    /* 
     * move to the extension 
     */
    ffmahd(PsfFileUnit, exti, &HDU_Type, &status);
    FITSERR(CalReadPsfFile, ffmahd);

    /*
     *   get header and table
     */
    PsfHead = RetrieveFitsHeader(PsfFileUnit);
    ReadRows = (unsigned ) GetJVal(&PsfHead, "NAXIS2");
    GetBintableStructure(&PsfHead, &PsfTable, ReadRows, 0, NULL);

    ReadBintable(PsfFileUnit, &PsfTable, 0, UPNULL, 1, &ReadRows);  

    /*
     *    take OFFSET keyword from header extension
     */
    paridx = exti-2;
    PsfParm->params[paridx].offset = (DTYPE)GetDVal(&PsfHead, KWNM_OFFSET);

    /*
     *    take Parameter Name and values columns numbers
     */

    if ((p_name_col = ColNameMatch(CLNM_PSF_P_NAME, &PsfTable)) == -1) {
      headas_chat(MUTE, 
		  "CalReadPsfFile_%s: Error %s column does not exist in %s file. \n"
		  , CALREADPSF_VERSION, CLNM_PSF_P_NAME, PsfFileName);
      goto read_end;
    }
    if ((p_value_col = ColNameMatch(CLNM_PSF_P_VAL, &PsfTable)) == -1) {
      headas_chat(MUTE, 
		  "CalReadPsfFile_%s: Error %s column does not exist in %s file. \n"
		  , CALREADPSF_VERSION, CLNM_PSF_P_VAL, PsfFileName);
      goto read_end;
    }

    /*
     *     loop on Rows and take parameters
     */
    for ( rowidx=0; rowidx<ReadRows; rowidx++) {
      char PName[CLNM_PSF_P_NAME_MULT];
      int  multidx;            /*  index loops on P_VAL column Multiplicity  */

      if ( strncpy(PName,SVEC(PsfTable, rowidx, p_name_col),PsfTable.Multiplicity[p_name_col]) ) {
	if ( strcmp(PName,PSF_P_NAME_SIGMA) == 0 )
	  /*
	   *  Read Parameter for Sigma of the Gaussian Component
	   */
	  pnameidx = sigma_p;
	else if ( strcmp(PName,PSF_P_NAME_RL) == 0 )
	  /*
	   *  Read Parameters for radius of the Generalized Lorenz
	   */
	  pnameidx = r_l_p;
	else if ( strcmp(PName,PSF_P_NAME_M) == 0 )
	  /*
	   *  Read Parameters for the index of the Generalized Lorentz
	   */
	  pnameidx = m_e_p;
	else if ( strcmp(PName,PSF_P_NAME_RAT) == 0 )
	  /*
	   *  Read Ratio of the Gaussian Norm over the Generalized
	   */
	  pnameidx = r_e_p;
	else {
	  headas_chat(NORMAL, 
		      "CalReadPsfFile_%s: Warning skipped not allowed parameter '%s'\n" 
		      , CALREADPSF_VERSION, PName, PsfFileName);
	  continue;
	}
      }

      for (multidx=0;multidx<PsfTable.Multiplicity[p_value_col];multidx++) 
	PsfParm->params[paridx].coefs[pnameidx][multidx] =
	  DVECVEC(PsfTable, rowidx, p_value_col, multidx); 


    } /*    loop on rows    */
    /*
     * Write Info
     */
    headas_chat(CHATTY,"CalReadPsfFile_%s: Offset %lf\n",CALREADPSF_VERSION,
		PsfParm->params[paridx].offset);
    for(i=sigma_p;i<=r_e_p;i++) {
      if(i==sigma_p)
	headas_chat(CHATTY,"\tsigma\t");
      if(i==r_l_p)
	headas_chat(CHATTY,"\tr_l\t");
      if(i==m_e_p)
	headas_chat(CHATTY,"\tm_e\t");
      if(i==r_e_p)
	headas_chat(CHATTY,"\tr_e\t");
      for(ii=0;ii<MAXNUMB_PSF_COEFFS;ii++)
	headas_chat(CHATTY," %lf", PsfParm->params[paridx].coefs[i][ii]);
      headas_printf("\n");
    } /* loop on rows */

    PsfParm->numb_off_axis ++ ; 

  } /*    loop on extensions    */


  CloseFitsFile(PsfFileUnit);

  return OK;

 read_end:

  if (PsfFileUnit)
    CloseFitsFile(PsfFileUnit);

  if (PsfHead.first)
    ReleaseBintable(&PsfHead, &PsfTable);

  return NOT_OK;


} /* CalReadPsfFile */
