#include <unistd.h>
#include <math.h>
#include <ctype.h>
#include <time.h>
#undef RETURNFLOAT
#undef ASSIGNFLOAT
#include <fitsio.h>
#include <ftools.h>
#include <ftoolstruct.h>
#include <xpi.h>

/* Program version information */
struct {int Major, Minor;} Version = {3, 8};
#define VERSTRING "3.8"
#define VERDATE   "2009-09-08"

/* =================================================================== */
/* 
 * PCABACKEST
 * 
 * Background estimation program for XTE PCA data
 * 
 * This is the standard PCA background estimation program.  It takes
 * as input a PCA Standard2 data file for which background estimates
 * are desired, the XTE "filter file", and a set of model files which
 * contain interpolation coefficients.  Pcabackest performs the
 * interpolation for each PCU, layer, and anode.  If multiple model
 * components are present, then they are combined additively.  One of
 * the models, the radioactivation component, depends on the "SAA
 * Dosage History", which is optionally supplied as another input
 * parameter.  The output of the program is a Standard2-like file with
 * estimated background counts.  Various interpolation and correction
 * options are available.
 *
 *
 * Outline of operation:
 *   PCABackEst() is the main function
 *     * GetParameters to retrieve command line parameters
 *     * Create new output file
 *     * MakeModelCat() scans and filters the model file(s) -> ModelCat
 *     * PlanOutput() scans Std2 and FiltFile, and also enumerates
 *       possible internal models -> CD
 *          - CD->Catalog[0:N1-1]  - Std2 columns
 *          - CD->Catalog[N1:N2-1] - FiltFile columns
 *          - CD->Catalog[N2:N3-1] - internal model columns
 *       also "plans" the output file layout
 *     * InitBinTabHDU() - Create the output HDU
 *     * ApplyModel() - calculate model for each Std2 spectral column
 *         - ReadModel() - read/interpolate/calculate model; 
 *            foreach ModelCat model that matches Std2 column name:
 *              o read model descriptors (CTYPE<n> = model variables = Name[j])
 *              o CSVAL = "Special" value such as ExpDecay Lifetime
 *              o CDEDT = correct for dead time?
 *              o locate Name[j] in CD->Catalog[*].Name
 *              o if an internal model, then call CalcInternal()
 *                 which in turn calls CalcInternal_{name}() based 
 *                 on Name (these are things like ExpDecay
 *                 and so on)
 *              o otherwise read this variable from Std2 or FiltFile
 *              o if needed, call ComputeLiveFraction() and correct
 *                variable for deadtime
 *              o perform interpolation with Interpolate() and 
 *                apply DriftGain()
 *         - expand spect / gain corr
 *     * copy any other types of extension to output file
 *     * clean up
 *
 * $Id: pcabackest.c,v 3.32 2009/09/09 12:47:59 miket Exp $
 */


/* Special CALDB access macros */
#define calfix_ELEMS_6 ZTRINGV_ARGS(4)
#define calfix_ELEMLEN_6 ZTRINGV_ARGS(5)
#define calfix_ELEMS_8 ZTRINGV_ARGS(4)
#define calfix_ELEMLEN_8 ZTRINGV_ARGS(5)

#define CalFix(codenam,date,time,maxret,slen,filenam,extno,online,nret,nfound,\
	       status)\
  CCALLSFSUB11(CALFIX,calfix,STRING,STRING,STRING,INT,INT,PZTRINGV,INTV,\
               PZTRINGV,PCINT,PCINT,PCINT,codenam,date,time,maxret,slen,\
	       filenam,extno,online,nret,nfound,status)

/* Internal constants and macros */
#define Pi 3.141592653589793238462643383279
#define r2d(x) remainder(x*57.29577951, 360.0)
#define min(x,y) ((x<y)?x:y)
#define max(x,y) ((x>y)?x:y)
#ifndef TRUE
#define TRUE (1)
#endif

/* Special string constants */
#define PHANAME                 "Channel"      /* Name of PHA column */
#define ERRORNAME               "Data/Error"  
#define SPECTRUM_ID_STRING      "SpecPcu"      /* Std2 columns with spectra */
#define SPECTRUM_STRING         "Spec"
#define ERROR_STRING            "Errs"
#define LAYERS_COMBINED         "X1L^X1R^X2L^X2R^X3L^X3R" /* All layers DDL */
#define FULL_SPECTRUM           "0:256"                   /* All chans CPIX */
#define SHORT_SPECTRUM          "(0~4),(5:51),(52~141;2),(142~237;3),(238~249;4),(250~255)"
#define XENON_SPECTRUM_PREFIX   "Xe"           /* Std2 column prefixes */
#define PROPANE_SPECTRUM_PREFIX "Vp"
#define CALDB_MODEL_ID          "PCA_BKGD_MODEL"    /* Special CALDB EXTNAMEs*/
#define CALDB_GCOR_ID           "EDS_GCOR"
#define CALDB_SAAHIST_ID        "PCA_SAA_HIST_DATA"


/* "URP" - Defined error messages */
#define URP_MESSAGES 17

char *UrpMessages[URP_MESSAGES] =
{
#define PCABAT_URP_TIMEGAPS         (-100)
  "Input Standard Mode FITS file contains gaps which may be poorly reproduced in the output.",

#define PCABAT_URP_DATA_MODE        (-101)
  "\"Standard Mode 2\" data file is in a different data format.",

#define PCABAT_URP_MODEL_ODD        (-102)
  "Model file is not in the correct format.",

#define PCABAT_URP_BAD_INPUT        (-103)
  "Unable to get user input. (Bad parameter file?)",

#define PCABAT_URP_NO_MEMORY        (-104)
  "Insufficient memory.",

#define PCABAT_URP_NO_PARAM_DATA    (-105)
  "Unable to find data for required model parameter.",

#define PCABAT_URP_WRONG_PHA_CHAN   (-106)
  "Model contains wrong number of channels for spectrum.",

#define PCABAT_URP_MAXMOD_TOO_SMALL (-107)
  "Not all models loaded.  Use a larger value of \"maxmodels\".",

#define PCABAT_URP_OUT_OF_BOUNDS    (-108)
  "Model parameter out of bounds.",

#define PCABAT_URP_FILTER_NO_COVER  (-109)
  "Filter file does not cover the time span of the input.",

#define PCABAT_URP_FILTER_REBIN     (-110)
  "Filter file must have the same time resolution as the input.",

#define PCABAT_URP_NO_MODELS        (-111)
  "No model files were found",

#define PCABAT_URP_NEED_HXPMDATA    (-112)
  "Exponential decay model requires an SAA dose history file.",

#define PCABAT_URP_HXPMDATA_SHORT   (-113)
  "SAA dose history file contains no valid data for the time of your observation.",

#define PCABAT_URP_NOTIN_CALDB      (-114)
  "Search of calibration database returned no files.",

#define PCABAT_URP_BAD_GAIN_SPEC    (-115)
  "Gain drift correction not correctly specified in model.",

#define PCABAT_URP_BAD_MODEL_TIMES  (-116)
  "Model file is invalid for the time of your observation."
};


/*
 * Definitions which modify the function of CopyColDesc()
 */

#define COLDESCF_TIME 1
#define COLDESCF_ERROR 2

/*
 * Some string length definitions required by cxpi.h
 */

/*
 * Globally defined variables
 */

/*
 * ModelCat - inventory of models found from modelfile(s)
 */
struct ModelCat
{
  char Name[FLEN_VALUE];   /* Name of Std2 column this model applies to */
  fitsfile *Unit;          /* FITS unit of this file */
  int HDU;                 /* FITS HDU of this file */
  double GainDriftTZero, GainDriftSlope; /* Gain drift coefficients */
  char AltName[FLEN_VALUE];/* Textual description of model */
};


/*
 * ColumnCat - inventory of available independent variables
 *   (includes Standard2 columns, FiltFile columns, and internally
 *    calculated values)
 */
struct ColumnCat
{
  char Name[FLEN_VALUE]; /* Name of independent variable */
  fitsfile *Unit;        /* FITS unit number */
  int Column;            /* PCU number (or -1 for all PCUs) */
  int Elements, Rows;    /* Total num. of elements per row; num. of rows */
};


struct SpecCat
{
  char Name[FLEN_VALUE];
  int Column, Elements, Dest, PCU;
};

/*
 * ConvDesc - conversion descriptor
 *  - contains all components of a model calculation
 */
struct ConvDesc
{
  struct ColumnCat *Catalog;  /* Available independent variables */
  /* Input spectra and output spectra */
  struct SpecCat *SourceSpecCat, *DestSpecCat;
  int SourceSpectra, DestSpectra, Columns, CatColumns;
  double OutInterval, InInterval, Scale, ObsMJD;
  long InRows, OutRows, FiltRows;
  long Gain[5], Offset[5];

  /* Data pertaining to SAA dosage file and FiltFile */
  fitsfile *FiltUnit, *HEXTEPMUnit;
  char *HEXTEPMFile;
  int *FiltConv;
  int Flags;
};

/* Processing flags */
#define CD_COMBINE_XE_LAYERS   1
#define CD_256_CHANNEL_XE_SPEC 2
#define CD_CORRECT_XE_GAIN     4
#define CD_INTERPOLATE         8
#define CD_SYSTEMATIC_ERRORS  16

/* Is backgroud type internal to instrument vs. external?
 *   internal = particle backgrounds
 *   external = cosmic backgrounds
 */
#define MT_INTERNAL 1
#define MT_EXTERNAL 2
#define MT_BOTH     (MT_INTERNAL | MT_EXTERNAL)

#define MAXC_FNAME 1024
#define MAXF_FNAME 255
int BufLen_2 = MAXF_FNAME;

/* 
 * UserParams - values read from parfile
 */
struct UserParams
{
  char Std2Name[MAXC_FNAME], BackName[MAXC_FNAME], ModelName[MAXC_FNAME],
       GainFile[MAXC_FNAME], FilterFile[MAXC_FNAME], HEXTEPMFile[MAXC_FNAME];
  double Interval;
  int NoPropane, CombineLayers, FullSpectrum, CorrectGain, MaxModels,
      Interpolate, ModelType, Errors, Clobber, TimeSlop;
  fitsfile *HEXTEPMUnit;
};

char ProgramName[] = "pcabackest v" VERSTRING;
char KeyChars[] = "QWERTYUIOPASDFGHJKLZXCVBNM1234567890-_";
char DateString[30];
float *LiveFrac[5] = {NULL, NULL, NULL, NULL, NULL};

/*
 * Prototypes
 */

void AddHistory(char *String);
void ApplyModel(struct ConvDesc *CD, fitsfile *Std2Unit, fitsfile *BackUnit,
		struct ModelCat *ModelCat, int *Status);
int CalcInternal(struct ConvDesc *CD, char *Name, float *Param,
		 double Special, int *Status);
int CalcInternal_2LLD(struct ConvDesc *CD, struct ColumnCat *Catalog,
		       float *Param, double Special, int *Status);
int CalcInternal_ExpDecay(struct ConvDesc *CD, struct ColumnCat *Catalog,
			   float *Param, double Special, int *Status);
int CalcInternal_HighXe(struct ConvDesc *CD, struct ColumnCat *Catalog,
			 float *Param, double Special, int *Status);
int CalcInternal_Xenon(struct ConvDesc *CD, struct ColumnCat *Catalog,
			float *Param, double Special, 
			int Layers[6], int ChanLow, int ChanHigh,
			int *Status);
int CalcInternal_VpX1(struct ConvDesc *CD, struct ColumnCat *Catalog,
		       float *Param, double Special, int *Status);
int CalcInternal_VxN(struct ConvDesc *CD, struct ColumnCat *Catalog,
		       float *Param, double Special, int *Status);
int CalcInternal_VxRate(struct ConvDesc *CD, struct ColumnCat *Catalog,
			 float *Param, double Special, int *Status);
int CompareTimes(double MJD1, double MJD2, int *Status);
void ComputeLiveFraction(struct ConvDesc *CD, int *Status);
void CopyColDesc(fitsfile *Std2Unit, fitsfile *BackUnit, int StartRec,
		 int *ColNum, struct ConvDesc *CD, int Flags, int *Status);
void CopyPHDU(fitsfile *InUnit, fitsfile *OutUnit, int *Status);
void CorrectGain(struct ConvDesc *CD, int PCU, float *Data, float *Error);
void DriftGain(struct ConvDesc *CD, struct ModelCat *Model, float *Data,
	       float *Error, int Elements);
void DumpHistory(fitsfile *OutUnit, int *Status);
void ExpandSpectrum(float *ShortSpec, float *ShortErr, float *FullSpec,
		    float *FullErr);
void FreeInterpSpec(float *InterpSpec);
double GetObsMJD(fitsfile *Std2Unit, int *Status);
void GetParameters(struct UserParams *UP, int *Status);
void InitBinTabHDU(struct ConvDesc *CD, fitsfile *Std2Unit, fitsfile *BackUnit,
		   int *Status);
void Interpolate(float *Buffer, float *ErrBuff, double *IFrac, int Elements,
		 int Axes);
void LoadGainCorr(struct ConvDesc *CD, char *File, int Extension, int *Status);
float *MakeInterpSpec(float *ShortSpec);
struct ModelCat *MakeModelCat(fitsfile *Std2Unit, char *NameList,
			      int MaxModels, int BGType, int *Status);
void PCABackEst();
struct ConvDesc *PlanOutput(fitsfile *Std2Unit, fitsfile *FiltUnit,
			    struct UserParams *UP, int *Status);
void FreeConv(struct ConvDesc *);
void ReadModel(struct ConvDesc *CD, struct ModelCat *ModelCat, char *ParamName,
	       float *OutData, float *OutErrors, int Elements, int *Status);
int *RebinFilter(fitsfile *Std2Unit, fitsfile *FiltUnit, int Std2Rows,
		 int FiltRows, int *Status);
void Urp(int Index);
int ValidModelTime(fitsfile *ModelUnit, double MJD, int *Status);

#define INTROS 4
char *Intro[INTROS] =
{
  "                    +------------------------------+",
  "                    | XTE/PCA Background Estimator |",
  "                    |   Version " VERSTRING " (" VERDATE ")  |",
  "                    +------------------------------+"
};

/*
 * The comment inserted in FITS files by this program as identification
 */

#define COMMENTS 4
char Comment[COMMENTS][FLEN_CARD] = 
{
  "COMMENT                                           +------------------+",
  "COMMENT                                           |     XTE /PCA     |",
  "COMMENT                                           |  PCA Background  |",
  "COMMENT                                           +------------------+"
};


void PCABackEst()

/*********************\
*     PCABackEst      *
*     ----------      *
\*********************/

{
  int Status = 0, Type, Done;
  int i;
  char Value[FLEN_VALUE], CommBuff[FLEN_COMMENT];
  char StringBuff[MAXC_FNAME];
  struct UserParams UP;
  struct ConvDesc *Conv;
  struct ModelCat *ModelCat;
  fitsfile *Std2Unit, *BackUnit, *FiltUnit;

  C2FCBSTR("pcabackest", TASK.taskname,0);

  for (i = 0; i < INTROS; i++)
    Fcecho(Intro[i]);
  Fcecho(" ");

  /* Initialize to zero */
  UP.HEXTEPMUnit = 0;
  GetParameters(&UP, &Status);
  AddHistory("PCABackEst parameters:");
  sprintf(StringBuff, " Standard mode file: %s", UP.Std2Name);
  AddHistory(StringBuff);
  sprintf(StringBuff, " Filter file: %s", UP.FilterFile);
  AddHistory(StringBuff);
  if (UP.CorrectGain)
  {
    sprintf(StringBuff, " Gain file: %s", UP.GainFile);
    AddHistory(StringBuff);
  }
  if (strcasecmp(UP.HEXTEPMFile, "none"))
  {
    sprintf(StringBuff, " SAA history file: %s", UP.HEXTEPMFile);
    AddHistory(StringBuff);
  }
  Fcecho(" ");

  if (Status) {Urp(Status); return;}

  /** Open the FITS files */

  fits_open_file(&Std2Unit, UP.Std2Name, READONLY, &Status);
  if (Status) {
    sprintf(StringBuff, "ERROR: could not open %s",
	    UP.Std2Name);
    Fcerr(StringBuff);
  }
  if (strcasecmp(UP.FilterFile, "NONE") && !Status)
  {
    fits_open_file(&FiltUnit, UP.FilterFile, READONLY, &Status);
    fits_movabs_hdu(FiltUnit, 2, &Type, &Status);
    if (Status) {
      sprintf(StringBuff, "ERROR: could not open %s",
	      UP.FilterFile);
      Fcerr(StringBuff);
    }
  }
  else FiltUnit = NULL;
  if (!Status)
  {
    fits_create_file(&BackUnit, UP.BackName, &Status);
    if ((Status == FILE_NOT_CREATED) && (UP.Clobber))
    {
      if (!unlink(UP.BackName))
      {
	Status = 0;
	fits_clear_errmsg();
	fits_create_file(&BackUnit, UP.BackName, &Status);
      }
    }
    if (!Status)
    {
      CopyPHDU(Std2Unit, BackUnit, &Status);
      
      /** Load the first extension **/
      fits_movrel_hdu(Std2Unit, 1, &Type, &Status);
      fits_create_hdu(BackUnit, &Status);

      /** Load the models **/
      
      ModelCat = MakeModelCat(Std2Unit, UP.ModelName, UP.MaxModels,
			      UP.ModelType, &Status);
      
      Done = FALSE;
      while (!Done && !Status)
      {  
	if (Type == 2)
	{
	  fits_read_key(Std2Unit, TSTRING, "EXTNAME", Value, CommBuff,
			&Status);
	  if (!Status && !strcmp(Value, "XTE_SA"))
	  {
	    fits_read_key(Std2Unit, TSTRING, "DATAMODE", Value, CommBuff,
			  &Status);
	    if (!Status && !strncmp(Value, "Standard2", 9))
	    {
	      Conv = PlanOutput(Std2Unit, FiltUnit, &UP, &Status);
	      InitBinTabHDU(Conv, Std2Unit, BackUnit, &Status);
	      ApplyModel(Conv, Std2Unit, BackUnit, ModelCat, &Status);
	      if (!Status) fits_write_chksum(BackUnit, &Status);
	      if (LiveFrac[0])
	      {
		free(LiveFrac[0]);
		LiveFrac[0] = NULL;
	      }
	      FreeConv(Conv);
	    }
	    else
	      Urp(PCABAT_URP_DATA_MODE);
	  }
	  else
	    fits_copy_hdu(Std2Unit, BackUnit, 0, &Status);
	}
	else
	  fits_copy_hdu(Std2Unit, BackUnit, 0, &Status);
	if (!Status)
	{
	  fits_movrel_hdu(Std2Unit, 1, &Type, &Status);
	  if (Status == END_OF_FILE)
	  {
	    fits_clear_errmsg();
	    Status = 0;
	    Done = TRUE;
	  }
	  else
	    fits_create_hdu(BackUnit, &Status);
	}
      }
      
      /** Close open FITS files **/
      if (!Status) fits_close_file(BackUnit, &Status);
    }
    if (FiltUnit && !Status) fits_close_file(FiltUnit, &Status);
    if (!Status) fits_close_file(Std2Unit, &Status);
  }

  /** Report errors **/
  if (Status)
  {
    Urp(Status);
  }
}

void GetParameters(struct UserParams *UP, int *Status)
     
/*********************\
*    GetParameters    *
*    -------------    *
*      Gets the user settable parameters in the FTOOLS standard way.  These
* parameters may come from either a configuration file or the user may be
* prompted.
*    UP - The structure populated by this routine containing the input.
\*********************/

{
  char TmpStr[MAXC_FNAME];
  int Tmp, Matched;

  strcpy(UP->Std2Name, "");
  strcpy(UP->BackName, "");
  strcpy(UP->ModelName, "");
  strcpy(UP->GainFile, "");
  strcpy(UP->FilterFile, "");

  UP->Interval = 0.0;
  UP->NoPropane = UP->CombineLayers = UP->FullSpectrum = UP->CorrectGain =
    UP->MaxModels = UP->Interpolate = UP->ModelType = UP->Errors = 
    UP->Clobber = 0;

  Uclgst("infile", UP->Std2Name, Status);
  if (*Status) {*Status = PCABAT_URP_BAD_INPUT; return;}
  Uclgst("outfile", UP->BackName, Status);
  if (*Status) {*Status = PCABAT_URP_BAD_INPUT; return;}
  Uclgst("modelfile", UP->ModelName, Status);
  if (*Status) {*Status = PCABAT_URP_BAD_INPUT; return;}
  Uclgst("filterfile", UP->FilterFile, Status);
  if (*Status) {*Status = 0; strcpy(UP->FilterFile, "NONE");}
  if (strlen(UP->FilterFile) == 0) strcpy(UP->FilterFile, "NONE");
  strcpy(TmpStr, "\0");
  Uclgst("modeltype", TmpStr, Status);
  if (*Status) {*Status = 0; strcpy(TmpStr, "BOTH");}
  Matched = FALSE;
  if (!strncasecmp("INTERNAL", TmpStr, strlen(TmpStr)))
  {
    Matched = TRUE;
    UP->ModelType = MT_INTERNAL;
  }
  if (!strncasecmp("EXTERNAL", TmpStr, strlen(TmpStr)))
  {
    Matched = TRUE;
    UP->ModelType = MT_EXTERNAL;
  }
  if (!strncasecmp("BOTH", TmpStr, strlen(TmpStr)))
  {
    Matched = TRUE;
    UP->ModelType = MT_BOTH;
  }
  if (!Matched) {*Status = PCABAT_URP_BAD_INPUT; return;}
  Uclgsd("interval", &UP->Interval, Status);
  if (*Status) {*Status = PCABAT_URP_BAD_INPUT; return;}
  Uclgsb("propane", &Tmp, Status);
  if (*Status) {*Status = PCABAT_URP_BAD_INPUT; return;}
  UP->NoPropane = !Tmp;
  Uclgsb("layers", &Tmp, Status);
  if (*Status) {*Status = PCABAT_URP_BAD_INPUT; return;}
  UP->CombineLayers = !Tmp;
  Uclgsb("gaincorr", &Tmp, Status);
  if (*Status) {*Status = PCABAT_URP_BAD_INPUT; return;}
  UP->CorrectGain = Tmp;
  if (UP->CorrectGain)
  {
    UP->FullSpectrum = TRUE;
    Uclgst("gcorrfile", UP->GainFile, Status);
    if (*Status) {*Status = PCABAT_URP_BAD_INPUT; return;}
  }
  else
  {
    Uclgsb("fullspec", &Tmp, Status);
    if (*Status) {*Status = PCABAT_URP_BAD_INPUT; return;}
    UP->FullSpectrum = Tmp;
  }
  Uclgsi("maxmodels", &UP->MaxModels, Status);
  if (*Status) {*Status = 0; UP->MaxModels = 256;}
  Uclgsi("timeslop", &UP->TimeSlop, Status);
  if (*Status) {*Status = 0; UP->TimeSlop = 32;}
  Uclgst("saahfile", UP->HEXTEPMFile, Status);
  if (*Status) {*Status = 0; strcpy(UP->HEXTEPMFile, "none");}
  Uclgsb("interp", &Tmp, Status);
  if (*Status) {*Status = 0; Tmp = TRUE;}
  UP->Interpolate = Tmp;
  Uclgsb("syserr", &Tmp, Status);
  if (*Status) {*Status = 0; Tmp = TRUE;}
  UP->Errors = Tmp;
  Uclgsb("clobber", &Tmp, Status);
  if (*Status) {*Status = 0; Tmp = FALSE;}
  UP->Clobber = Tmp;
}

void CopyPHDU(fitsfile *InUnit, fitsfile *OutUnit, int *Status)
     
/*********************\
*      CopyPHDU       *
*      --------       *
* Parameters:
*    InUnit - The unit of an open FITS file from which the PHDU is to be
*             copied.
*    OutUnit - The unit of an open FITS file into which the PHDU is to be
*              copied.
*    Status - The FITSIO status flag
* Function:
*    Copies the Primary HDU from the FITS file referred to by "InUnit" to the
*    FITS file referred to by "OutUnit".  Some changes are made to the HDU 
*    during the copy including the stripping out of irrelevent comments and 
*    the updating of the DATE and CREATOR keywords.  This routine copies only
*    header information.
\*********************/

{
  int i, InsertedComment = FALSE, j, KeyLength, Matched;
  char Buffer[FLEN_CARD], KeyWord[FLEN_KEYWORD],
       Value[FLEN_VALUE], CommBuff[FLEN_COMMENT];
  
  for (i = 1; !*Status; i++)
  {
    fits_read_record(InUnit, i, Buffer, Status);
    KeyLength = min(strspn(Buffer, KeyChars), FLEN_KEYWORD - 1);
    strncpy(KeyWord, Buffer, KeyLength);
    KeyWord[KeyLength] = '\0';
    Matched = FALSE;
    if (!strcmp(KeyWord, "COMMENT"))
    {
      if (!InsertedComment)
      {
	for (j = 0; j < COMMENTS; j++)
	  fits_write_record(OutUnit, Comment[j], Status);
	InsertedComment = TRUE;
      }
      Matched = TRUE;
    }
    if (!strcmp(KeyWord, "DATE"))
    {
      fits_get_system_time(DateString, 0, Status);
      strcpy(CommBuff, "FITS file creation date (UTC/GMT)");
      fits_write_key(OutUnit, TSTRING, KeyWord, DateString, CommBuff, Status);
      Matched = TRUE;
    }
    if (!strcmp(KeyWord, "CREATOR"))
    {
      fits_parse_value(Buffer, Value, CommBuff, Status);
      fits_write_key(OutUnit, TSTRING, KeyWord, ProgramName, CommBuff, Status);
      Matched = TRUE;
    }
    if (!strcmp(KeyWord, "END"))
      Matched = TRUE;
    if (!Matched)
      fits_write_record(OutUnit, Buffer, Status);
  }
  if (*Status == 203)
  {
    *Status = 0;
    fits_clear_errmsg();
  }
  fits_set_hdustruc(OutUnit, Status);
  fits_write_chksum(OutUnit, Status);
}

#define MAX_COL_DIMS 10

struct ConvDesc *PlanOutput(fitsfile *Std2Unit, fitsfile *FiltUnit,
			    struct UserParams *UP, int *Status)

/*********************\
*     PlanOutput      *
*     ----------      *
* Parameters:
*    Std2Unit - The file pointer of an open Std 2 file
*    FiltUnit - The file pointer of an open filter file (Or NULL if there is
*               no filter file.)
*    UP - The user input returned by Get Parameters.
*    Status - The FITSIO Status flag.
* Function:
*    Populates the ConvDesc with information from the filter file, the standard
* mode 2 file and the user input.  The ConvDesc structure is used to limit the
* number of parameters passed between routines and the number of global
* variables by storing information in a big ol' structure.
\*********************/

{
  long Fields = 0, FiltFields = 0;
  int i, j, NewSpectrum, GainExt[1], NumFiles, NFound;
  char CommBuff[FLEN_COMMENT], Buffer[FLEN_CARD],
       StrValue[FLEN_VALUE], *DestSpec, TmpStrBuff[FLEN_VALUE],
       ObsDate[FLEN_VALUE], ObsTime[FLEN_VALUE],
       GainFiles[1][MAXC_FNAME], Online[1][MAXC_FNAME], MsgBuffer[128];
  struct ConvDesc *CD;
  double StartTime = 0.0, StopTime = 0.0, FiltStart = 0.0, FiltStop = 0.0,
    FiltInt = 0.0;

  CD = (struct ConvDesc *)malloc(sizeof(struct ConvDesc));
  memset(CD, 0, sizeof(struct ConvDesc));
  strcpy(ObsDate, "");
  strcpy(ObsTime, "");

  fits_read_key(Std2Unit, TLONG, "NAXIS2", &CD->InRows, CommBuff, Status);
  fits_read_key(Std2Unit, TLONG, "TFIELDS", &Fields, CommBuff, Status);
  fits_read_key(Std2Unit, TDOUBLE, "DELTAT", &CD->InInterval, CommBuff,
		Status);
  fits_read_key(Std2Unit, TDOUBLE, "TSTART", &StartTime, CommBuff, Status);
  fits_read_key(Std2Unit, TDOUBLE, "TSTOP", &StopTime, CommBuff, Status);
  if ((StopTime - StartTime != CD->InInterval * CD->InRows) &&
      (CD->InInterval != UP->Interval))
    Urp(PCABAT_URP_TIMEGAPS);
  CD->ObsMJD = GetObsMJD(Std2Unit, Status);

  if (FiltUnit && !*Status)
  {
    fits_read_key(FiltUnit, TLONG, "TFIELDS", &FiltFields, CommBuff, Status);
    fits_read_key(FiltUnit, TDOUBLE, "TSTART", &FiltStart, CommBuff, Status);
    fits_read_key(FiltUnit, TDOUBLE, "TSTOP", &FiltStop, CommBuff, Status);
    fits_read_key(FiltUnit, TLONG, "NAXIS2", &CD->FiltRows, CommBuff, Status);
    if (*Status) {
      Fcerr("ERROR: Filter file does not contain required keywords");
      return NULL;
    }
    fits_read_key(FiltUnit, TDOUBLE, "TIMEDEL", &FiltInt, CommBuff, Status);
    if (*Status == 202)
    {
      fits_clear_errmsg();
      *Status = 0;
      FiltInt = (FiltStart - FiltStop) / CD->FiltRows;
    }
    FiltInt = CD->InInterval;
    if ((StartTime < FiltStart - UP->TimeSlop) ||
	(StopTime > FiltStop + UP->TimeSlop))
    {
      sprintf(MsgBuffer,
	      "Filter time: %10.0f-%10.0f, Input time: %10.0f-%10.0f\n",
	      FiltStart, FiltStop, StartTime, StopTime);
      fits_write_errmsg(MsgBuffer);
      *Status = PCABAT_URP_FILTER_NO_COVER;
    }
    if (!*Status)
      CD->FiltConv = RebinFilter(Std2Unit, FiltUnit, CD->InRows, CD->FiltRows,
				 Status);
    CD->FiltUnit = FiltUnit;
  }
  else
  {
    CD->FiltRows = 0;
    CD->FiltConv = NULL;
    CD->FiltUnit = NULL;
  }

  if (UP->HEXTEPMUnit == 0) {
  if (strcasecmp(UP->HEXTEPMFile, "none"))
  {
    if (!strcasecmp(UP->HEXTEPMFile, "caldb"))
    {
      fits_read_key(Std2Unit, TSTRING, "DATE-OBS", ObsDate, CommBuff, Status);
      fits_read_key(Std2Unit, TSTRING, "TIME-OBS", ObsTime, CommBuff, Status);
      CalFix(CALDB_SAAHIST_ID, ObsDate, ObsTime, 1, MAXF_FNAME, GainFiles,
	     GainExt, Online, &NumFiles, &NFound, Status);
      if (NFound == 0)
      {
	*Status = PCABAT_URP_NOTIN_CALDB;
	fits_write_errmsg(
			 "SAA dose history file not in calibration database.");
      }
      if (!*Status)
	strcpy(UP->HEXTEPMFile, GainFiles[0]);
    }

    if (!*Status)
    {
      fits_open_file(&UP->HEXTEPMUnit, UP->HEXTEPMFile, READONLY, Status);
      fits_movabs_hdu(UP->HEXTEPMUnit, 2, NULL, Status);
      if (!ValidModelTime(UP->HEXTEPMUnit, CD->ObsMJD, Status))
	*Status = PCABAT_URP_HXPMDATA_SHORT;
    }
  }
  else CD->HEXTEPMUnit = NULL;
  }

  CD->HEXTEPMFile = UP->HEXTEPMFile;
  CD->HEXTEPMUnit = UP->HEXTEPMUnit;

  if (*Status)
    return NULL;
 /*
  * Construct the list of spectra to be modelled and the list of available
  * parameters in the input files.
  */

  CD->SourceSpectra = 0;
  CD->DestSpectra = 0;
  CD->CatColumns = Fields + FiltFields;
  CD->Columns = 0;
  CD->SourceSpecCat =
                   (struct SpecCat *)malloc(sizeof(struct ColumnCat) * Fields);
  CD->DestSpecCat =
                   (struct SpecCat *)malloc(sizeof(struct ColumnCat) * Fields);
  CD->Catalog = (struct ColumnCat *)malloc(sizeof(struct ColumnCat) * 
					   (Fields + FiltFields));
  CD->Flags = (UP->CombineLayers ? CD_COMBINE_XE_LAYERS : 0) | 
              (UP->FullSpectrum ? CD_256_CHANNEL_XE_SPEC : 0) |
              (UP->CorrectGain ? CD_CORRECT_XE_GAIN : 0) |
              (UP->Interpolate ? CD_INTERPOLATE : 0) |
	      (UP->Errors ? CD_SYSTEMATIC_ERRORS: 0);

  if (CD->Flags & CD_CORRECT_XE_GAIN)
  {
    if (!strcasecmp(UP->GainFile, "caldb"))
    {
      fits_read_key(Std2Unit, TSTRING, "DATE-OBS", ObsDate, CommBuff, Status);
      fits_read_key(Std2Unit, TSTRING, "TIME-OBS", ObsTime, CommBuff, Status);
      CalFix(CALDB_GCOR_ID, ObsDate, ObsTime, 1, MAXF_FNAME, GainFiles,
	     GainExt, Online, &NumFiles, &NFound, Status);
      if (NFound == 0)
      {
        *Status = PCABAT_URP_NOTIN_CALDB;
        fits_write_errmsg(
		      "No gain correction found in the calibration database.");
      }
      if (!*Status)
	strcpy(UP->GainFile, GainFiles[0]);
    }
    else
    {
      Fcpars(UP->GainFile, GainFiles[0], &GainExt[0], Status);
      if (!*Status)
	strcpy(UP->GainFile, GainFiles[0]);
      if (GainExt[0] < 0) GainExt[0] = 1;
    }
    if (*Status) return 0;
    sprintf(MsgBuffer, "Gain correction from %s", UP->GainFile);	
    Fcecho(MsgBuffer);
    LoadGainCorr(CD, UP->GainFile, GainExt[0], Status);
    if (*Status) return 0;
    strcpy(MsgBuffer, "     PCU        0     1     2     3     4");
    Fcecho(MsgBuffer);
    sprintf(MsgBuffer, "     Gain   %5ld %5ld %5ld %5ld %5ld", CD->Gain[0],
	    CD->Gain[1], CD->Gain[2], CD->Gain[3], CD->Gain[4]);
    Fcecho(MsgBuffer);
    sprintf(MsgBuffer, "     Offset %5ld %5ld %5ld %5ld %5ld\n", CD->Offset[0],
	    CD->Offset[1], CD->Offset[2], CD->Offset[3], CD->Offset[4]);
    Fcecho(MsgBuffer);
  }

  for (i = 0; i < Fields; i++)
  {
    fits_make_keyn("TTYPE", i + 1, Buffer, Status);
    fits_read_key(Std2Unit, TSTRING, Buffer, StrValue, CommBuff, Status);
    strcpy(CD->Catalog[i].Name, StrValue);
    CD->Catalog[i].Unit = Std2Unit;
    CD->Catalog[i].Column = i + 1;
    CD->Catalog[i].Rows = CD->InRows;

    /*
     * If the column is a spectral column then it is one we should model
     * the background for.
     */

    if (strstr(StrValue, SPECTRUM_ID_STRING))
    {
      DestSpec = StrValue;
      NewSpectrum = TRUE;
      if (UP->CombineLayers)
      {
	if (StrValue[0] == 'X')
	{
	  DestSpec = TmpStrBuff;
	  strcpy(TmpStrBuff, XENON_SPECTRUM_PREFIX);
	  strcat(TmpStrBuff, strstr(StrValue, SPECTRUM_STRING));
	  for (j = 0; j < CD->DestSpectra; j++)
	  {
	    if (!(strcmp(DestSpec, CD->DestSpecCat[j].Name)))
	    {
	      strcpy(CD->SourceSpecCat[CD->SourceSpectra].Name, StrValue);
	      CD->SourceSpecCat[CD->SourceSpectra].Column = i + 1;
	      CD->SourceSpecCat[CD->SourceSpectra].Dest = j + 1;
	      fits_make_keyn("TFORM", i + 1, Buffer, Status);
	      fits_read_key(Std2Unit, TSTRING, Buffer, StrValue, CommBuff,
			    Status);
	      CD->SourceSpecCat[CD->SourceSpectra].Elements = atoi(StrValue);
	      CD->SourceSpectra += 1;
	      NewSpectrum = FALSE;
	    }
	  }
	}
      }
      if (UP->NoPropane)
	if (!(strncmp(StrValue, PROPANE_SPECTRUM_PREFIX, 2)))
	  NewSpectrum = FALSE;
      if (NewSpectrum)
      {
	strcpy(CD->DestSpecCat[CD->DestSpectra].Name, DestSpec);
	CD->DestSpecCat[CD->DestSpectra].PCU =
                                         atoi(&DestSpec[strlen(DestSpec) - 1]);
        CD->DestSpecCat[CD->DestSpectra].Column = CD->DestSpectra + 1;
	strcpy(CD->SourceSpecCat[CD->SourceSpectra].Name, StrValue);
	CD->SourceSpecCat[CD->SourceSpectra].Column = i + 1;
	CD->SourceSpecCat[CD->SourceSpectra].Dest = CD->DestSpectra + 1;
	fits_make_keyn("TFORM", i + 1, Buffer, Status);
	fits_read_key(Std2Unit, TSTRING, Buffer, StrValue, CommBuff, Status);
	CD->DestSpecCat[CD->DestSpectra].Elements = 
	               CD->SourceSpecCat[CD->SourceSpectra].Elements =
		       atoi(StrValue);
	if ((CD->Flags & CD_256_CHANNEL_XE_SPEC) && 
	    (CD->DestSpecCat[CD->DestSpectra].Elements == 129))
	  CD->Columns += 256;
	else
	  CD->Columns += CD->DestSpecCat[CD->DestSpectra].Elements;
	CD->SourceSpectra += 1;
	CD->DestSpectra += 1;
      }
    }
  }
  for (i = 0; i < FiltFields; i++)
  {
    fits_make_keyn("TTYPE", i + 1, Buffer, Status);
    fits_read_key(FiltUnit, TSTRING, Buffer, StrValue, CommBuff, Status);
    strcpy(CD->Catalog[i + Fields].Name, StrValue);
    CD->Catalog[i + Fields].Unit = FiltUnit;
    CD->Catalog[i + Fields].Column = i + 1;
    CD->Catalog[i + Fields].Rows = CD->FiltRows;
  }

 /*
  * Initialize the parameters for rebinning the spectra in time.
  */

  CD->OutInterval = UP->Interval;
  CD->Scale = CD->OutInterval / CD->InInterval;
  CD->OutRows = ceil((double)CD->InRows / CD->Scale);


/*  printf("Conversion Description:\n");
  printf("Time interval %g / %g = %g\n", CD->OutInterval, CD->InInterval,
	 CD->Scale);
  printf("Rows %d -> %d\n", CD->InRows, CD->OutRows);
  printf("%d Spectra, %d Columns, %d Bytes per row.\n", CD->DestSpectra,
	 CD->CatColumns, CD->Columns); */
/*  printf("Catalog:\n");
  for (i = 0; i < CD->CatColumns; i++)
    printf("  \"%s\" Unit: 0x%8.8x Column: %d\n", CD->Catalog[i].Name,
	   CD->Catalog[i].Unit, CD->Catalog[i].Column); */
/*  for (i = 0; i < CD->SourceSpectra; i++)
    printf(" Column %d (%s) --> Column %d \n", CD->SourceSpecCat[i].Column,
	   CD->SourceSpecCat[i].Name, CD->SourceSpecCat[i].Dest); */
  return CD;
}

/*
 * FreeConv - free structure created by PlanOutput()
 *
 */
void FreeConv(struct ConvDesc * CD)
{
  if (CD == 0) return;
  if (CD->SourceSpecCat) free(CD->SourceSpecCat);
  if (CD->DestSpecCat)   free(CD->DestSpecCat);
  if (CD->Catalog)       free(CD->Catalog);

  /* Just to be sure... poke any bugs */
  memset(CD, 0, sizeof(struct ConvDesc));
  free(CD);

  return;
}


void InitBinTabHDU(struct ConvDesc *CD, fitsfile *Std2Unit, fitsfile *BackUnit,
		   int *Status)

/*********************\
*    InitBinTabHDU    *
*    -------------    *
* Parameters:
*    CD - The data structure created by PlanOutput
*    Std2Unit - The Source Standard mode 2 FITS file with the table HDU to be
*               copied as the current HDU.
*    BackUnit - The Output Background FITS file with and empty HDU as the
*               current HDU.
*    Status - The FITSIO status variable.
* Function:
*    Iniitializes the current HDU of the background FITS file referred to by 
*    "BackUnit" to store the background estimates.  Essentially all of the 
*    infortmation for the header comes from the standard mode 2 file referred
*    to by "Std2Unit".  
\*********************/

{
  int i, InsertedComment = FALSE, j, KeyLength,
      Matched, ColumnDesc, ColDescHead, OutColumn;
  char Buffer[FLEN_CARD], KeyWord[FLEN_KEYWORD],
    Value[FLEN_VALUE], CommBuff[FLEN_COMMENT];

  if (*Status)
    return;
 /*
  * Copy the header keywords for "Std2Unit to "BackUnit" stopping before the
  * first TFORMnnn keyword.  Also make changes to the header as appropriate.
  */

  ColumnDesc = FALSE;
  for (i = 1; (!*Status && !ColumnDesc); i++)
  {
    ffgrec(Std2Unit, i, Buffer, Status);
    KeyLength = min(strspn(Buffer, KeyChars), FLEN_KEYWORD - 1);
    strncpy(KeyWord, Buffer, KeyLength);
    KeyWord[KeyLength] = '\0';
    Matched = FALSE;
    for (j = 0; j < strlen(Value); j++)
      Value[j] = ' ';
    Value[strlen(Value) - 1] = '\0';
    
    if (!strcmp(KeyWord, "COMMENT") && !InsertedComment)
    {
      for (j = 0; j < COMMENTS; j++)
	ffprec(BackUnit, Comment[j], Status);
      DumpHistory(BackUnit, Status);
      InsertedComment = TRUE;
      while (!strcmp(KeyWord, "COMMENT"))
      {
	ffgrec(Std2Unit, ++i, Buffer, Status);
	strncpy(KeyWord, Buffer, strspn(Buffer, KeyChars));
	KeyWord[strspn(Buffer, KeyChars)] = '\0';
      }
    }
    if (!strcmp(KeyWord, "DATE"))
    {
      fits_get_system_time(DateString, 0, Status);
      strcpy(CommBuff, "FITS file creation date (UTC/GMT)");
      ffpkys(BackUnit, KeyWord, DateString, CommBuff, Status);
      Matched = TRUE;
    }
    if (!strcmp(KeyWord, "CREATOR"))
    {
      ffpsvc(Buffer, Value, CommBuff, Status);
      ffpkys(BackUnit, KeyWord, ProgramName, CommBuff, Status);
      Matched = TRUE;
    }
    if (!strcmp(KeyWord, "TFIELDS"))
    {
      ffpsvc(Buffer, Value, CommBuff, Status);
      ffpkyj(BackUnit, KeyWord, 1 + CD->DestSpectra *
             (CD->Flags & CD_SYSTEMATIC_ERRORS ? 2 : 1), CommBuff, Status);
      Matched = TRUE;
    }
    if (!strcmp(KeyWord, "NAXIS1"))
    {
      ffpsvc(Buffer, Value, CommBuff, Status);
      ffpkyj(BackUnit, KeyWord, 8 + CD->Columns * 4 *
             (CD->Flags & CD_SYSTEMATIC_ERRORS ? 2 : 1), CommBuff, Status);
      Matched = TRUE;
    }
    if (!strcmp(KeyWord, "NAXIS2"))
    {
      ffpsvc(Buffer, Value, CommBuff, Status);
      ffpkyj(BackUnit, KeyWord, CD->OutRows, CommBuff, Status);
      Matched = TRUE;
    }
    if (!strcmp(KeyWord, "HDUCLAS2"))
    {
      ffpsvc(Buffer, Value, CommBuff, Status);
      ffpkys(BackUnit, KeyWord, "BACKGROUND", CommBuff, Status);
      Matched = TRUE;
    }
    if (!strcmp(KeyWord, "DELTAT") || !strcmp(KeyWord, "TIMEDEL"))
    {
      ffpsvc(Buffer, Value, CommBuff, Status);
      ffpkyg(BackUnit, KeyWord, CD->OutInterval, 6, CommBuff, Status);
      Matched = TRUE;
    }
    if (!strcmp(KeyWord, "GAINAPP"))
      if (CD->Flags & CD_CORRECT_XE_GAIN)
      {
	ffpsvc(Buffer, Value, CommBuff, Status);
	ffpkyl(BackUnit, KeyWord, TRUE, CommBuff, Status);
	Matched = TRUE;
      }
    if (!strncmp(KeyWord, "TFORM", 5))
    {
      ColumnDesc = TRUE;
      Matched = TRUE;
    }
    if (!Matched)
      ffprec(BackUnit, Buffer, Status);
  }
  ColDescHead = i - 1;
  OutColumn = 0;
  CopyColDesc(Std2Unit, BackUnit, ColDescHead, &OutColumn, CD, COLDESCF_TIME,
	      Status);
  if (CD->Flags & CD_SYSTEMATIC_ERRORS)
    CopyColDesc(Std2Unit, BackUnit, ColDescHead, &OutColumn, CD,
		COLDESCF_ERROR, Status);
  ffrdef(BackUnit, Status);
  return;
}

void CopyColDesc(fitsfile *Std2Unit, fitsfile *BackUnit, int StartRec,
		 int *ColNum, struct ConvDesc *CD, int Flags, int *Status)

/*********************\
*     CopyColDesc     *
*     -----------     *
* Parameters:
*    Std2Unit - Source FITS file unit for column descriptions.
*    BackUnit - Destination FITS file unit for column descriptions.
*    StartRec - The number of the record in "Std2Unit" which contains the first
*               column description information.  This may be between 1 and the
*               number of the record with the first TFORM keyword.
*    ColNum - Number of last column description written to "BackUnit".
*    CD - Description of the conversion of input to output.
*    Flags - COLDESCF_TIME -> Copy the description for column 1 even if it
*                             doesn't appear in "ColList".  Column 1 usually
*                             contains the time stamp for the data rows.
*            COLDESCF_ERROR -> Replace the string 'Spec' with the string 'Errs'
*                              in all of the TTYPE keywords where it appears.
*    Status - The FITSIO status variable.
* Function:
*    Copies the column description information from the current table HDU of
*    "Std2Unit" to "BackUnit".  Also replaces the letter 'I' in TFORM keyword
*    values in these columns with 'E'.
* Note:
*    This routine makes certain assumptions about the structure of the column
*    descriptions that it is copying:
*        1) All of the keywords describing a certain FITS table column appear
*           in ajacent records of the FITS file.
*        2) The first FITS keyword in each group is the TFORM keyword.
* Creation: 1/31/95 Mike Stark
\*********************/

{
  int WriteColumn = FALSE, i, j, k, KeyLength, Column, *DestSpecList,
      MaxOutCol = 0, OutColumn=0, DestColumn=0;
  char KeyWord[FLEN_KEYWORD], Buffer[FLEN_CARD], *BufferValue,
       ColName[FLEN_VALUE], OutColName[FLEN_VALUE],
       Value[FLEN_VALUE], CommBuff[FLEN_COMMENT], *SubString,
       Descrip[256], DescripBuff[256];

  DestSpecList = (int *)malloc(sizeof(int) * CD->SourceSpectra);
  for (i = 0; i < CD->SourceSpectra; i++)
    DestSpecList[i] = CD->SourceSpecCat[i].Dest;

  for (i = StartRec; !*Status; i++)
  {
    fits_read_record(Std2Unit, i, Buffer, Status);
    KeyLength = min(strspn(Buffer, KeyChars), FLEN_KEYWORD - 1);
    strncpy(KeyWord, Buffer, KeyLength);
    KeyWord[KeyLength] = '\0';

   /*
    * Determine if this keyword is to be copied to the output file.  CONTINUE
    * keywords are copied if the line preceding them was copied and other lines
    * are coped if they describe a column which is listed in CD->SourceSpecList
    * for which DestSpecList is non-zero.  
    */

    for (j = KeyLength - 1; (j >= 0) && isdigit((int)KeyWord[j]); j--);
    Column = atoi(&KeyWord[++j]);

    if (Column)
    {
      WriteColumn = FALSE;

      /*
       * Always write the first column if the flag is set.
       */

      if ((Column == 1) && (Flags & COLDESCF_TIME))
      {
	WriteColumn = TRUE;
	OutColumn = *ColNum + 1;
      }

      for (j = 0; (j < CD->SourceSpectra) && !WriteColumn; j++)
	if ((Column == CD->SourceSpecCat[j].Column) && DestSpecList[j])
	{
	  for (k = j + 1; k < CD->SourceSpectra; k++)
	    if (DestSpecList[k] == DestSpecList[j])
	      DestSpecList[k] = 0;
	  WriteColumn = TRUE;
	  DestColumn = DestSpecList[j];
	  OutColumn = *ColNum + DestColumn + (Flags & COLDESCF_TIME ? 1 : 0);
	  if (OutColumn > MaxOutCol)
	    MaxOutCol = OutColumn;
	}
      if (WriteColumn)
      {
	sprintf(ColName, "%d", Column);
	sprintf(OutColName, "%d", OutColumn);
	if ((SubString = strstr(Buffer, ColName)))
	{
	  memset(SubString, (int)' ', strlen(ColName));
	  strncpy(SubString, OutColName, strlen(OutColName));
	}
      }
    }

    /*
     * Don't copy the coda item keywords which also have numeric suffixes.
     */

    if (WriteColumn)
      if (KeyWord[0] == 'D')
	WriteColumn = FALSE;
    
    /*
     * Don't be tricked into copying the END keyword.
     */

    if (WriteColumn)
      if (!strcmp(KeyWord, "END"))
	WriteColumn = FALSE;

    if (WriteColumn)
    {

      /*
       * TFORM keywords for the background file will be <n>E instead of <n>I
       * because background file spectra are stored as 4-byte real numbers.
       */

      if (!strncmp(KeyWord, "TFORM", 5) || !strncmp(KeyWord, "TDISP", 5))
      {
	fits_parse_value(Buffer, Value, CommBuff, Status);
	if ((SubString = strchr(Value, 'I')))
	  if ((BufferValue = strstr(Buffer, Value)))
	    if ((SubString = strchr(BufferValue, 'I')))
	      *SubString = 'E';
      }

      if ((!strncmp(KeyWord, "TDIM", 4) || !strncmp(KeyWord, "TFORM", 5)) &&
	  (CD->Flags & CD_256_CHANNEL_XE_SPEC))
      {
	fits_parse_value(Buffer, Value, CommBuff, Status);
	if ((SubString = strstr(Value, "129")))
	  if ((BufferValue = strstr(Buffer, Value)))
	    if ((SubString = strstr(BufferValue, "129")))
	      strncpy(SubString, "256", 3);
      }

      if (!strncmp(KeyWord, "TZERO", 5))
	WriteColumn = FALSE;
      
      /*
       * TDDES descriptor keywords need to be changed if the xenon layers are 
       * being combined to reflect this combination.
       */
      
      if (!strncmp(KeyWord, "TDDES", 5))
      {
	if (CD->Flags & (CD_COMBINE_XE_LAYERS | CD_256_CHANNEL_XE_SPEC))
	{
	  fits_parse_value(Buffer, Value, CommBuff, Status);
	  if ((SubString = strstr(Buffer, Value)))
	    strcpy(Descrip, &SubString[1]);
	  if ((SubString = strrchr(Descrip, (int)'\'')))
	    SubString[0] = '\0';
	  if (Descrip[strlen(Descrip) - 1] == '&')
	  {
	    fits_read_record(Std2Unit, i + 1, Buffer, Status);
	    while ((Descrip[strlen(Descrip) - 1] == '&') &&
		   !strncmp(Buffer, "CONTINUE", 8))
	    {
	      fits_parse_value(Buffer, Value, CommBuff, Status);
	      if ((SubString = strrchr(CommBuff, '\'')))
		SubString[0] = '\0';
	      if ((SubString = strchr(CommBuff, '\'')))
		strcpy(&Descrip[strlen(Descrip) - 1], &SubString[1]);
	      fits_read_record(Std2Unit, ++i + 1, Buffer, Status);
	    }
	  }
	  for (j = strlen(Descrip - 1); (Descrip[j] == ' ') && (j > 0); j--)
	    Descrip[j] = '\0';
	  if ((SubString = strstr(Descrip, "E[")))
	    if (SubString[2] == 'X')
	    {
	      if (CD->Flags & CD_COMBINE_XE_LAYERS)
	      {
		BufferValue = strchr(SubString, ']');
		strcpy(DescripBuff, BufferValue);
		strcpy(&SubString[2], LAYERS_COMBINED);
		strcat(Descrip, DescripBuff);
	      }
	      if (CD->Flags & CD_256_CHANNEL_XE_SPEC)
		if ((SubString = strstr(Descrip, "C[")))
		{
		  BufferValue = strchr(SubString, ']');
		  strcpy(DescripBuff, BufferValue);
		  strcpy(&SubString[2], FULL_SPECTRUM);
		  strcat(Descrip, DescripBuff);
		}
	    }
	  if ((SubString = strstr(KeyWord, ColName)))
	  {
	    memset(SubString, (int)' ', strlen(ColName));
	    strcpy(SubString, OutColName);
	  }
	  fits_write_key_longstr(BackUnit, KeyWord, Descrip, "", Status);
	  WriteColumn = FALSE;
	}
      }

      if (!strncmp(KeyWord, "1CPIX", 5))      
	if (CD->Flags & CD_256_CHANNEL_XE_SPEC)
	{
	  fits_parse_value(Buffer, Value, CommBuff, Status);
	  BufferValue = strstr(Buffer, Value);
	  memset(BufferValue, (int)' ', strlen(Value));
	  strncpy(BufferValue, "'(0:255) '", 10);
	}

      if (!strncmp(KeyWord, "TTYPE", 5))
      {
	fits_parse_value(Buffer, Value, CommBuff, Status);
	if (Column != 1)
	  if (strcmp(CD->DestSpecCat[DestColumn - 1].Name, Value))
	    if ((BufferValue = strstr(Buffer, Value) + 1))
	    {
	      memset(BufferValue, (int)' ', strlen(Value) + 1);
	      strcpy(BufferValue, CD->DestSpecCat[DestColumn - 1].Name);
	      strcpy(Value, CD->DestSpecCat[DestColumn - 1].Name);
	      BufferValue[strlen(BufferValue)] = '\'';
	    }
	if (Flags & COLDESCF_ERROR)
	  if ((SubString = strstr(Value, SPECTRUM_STRING)))
	    if ((BufferValue = strstr(Buffer, Value)))
	      if ((SubString = strstr(BufferValue, SPECTRUM_STRING)))
		strncpy(SubString, ERROR_STRING, 4);
      }
    }
    if (WriteColumn)
      fits_write_record(BackUnit, Buffer, Status);
  }
  *ColNum = MaxOutCol;
  if (*Status == 203)
  {
    *Status = 0;
    fits_clear_errmsg();
  }
}

void ApplyModel(struct ConvDesc *CD, fitsfile *Std2Unit, fitsfile *BackUnit,
		struct ModelCat *ModelCat, int *Status)

/*********************\
*     ApplyModel      *
*     ----------      *
* Parameters:
*    CD - The structure created by PlanOutput.
*    Std2Unit - FITS file unit of the Standard mode 2 file to search for
*               model parameters.
*    BackUnit - The FITS file unit of the background file the current HDU of
*               which has already been initialized.
*    ModelCat - The FITS unit of the file which contains the spectral models.
*    Status - The FISTIO status flag.
* Function:
*    Loops through the spectral columns of "Std2Unit" and applies the model
*    "ModelUnit" to each of them in turn.  The background spectrum is written
*    to "BackUnit".
* Note:
*    This routine assumes that the background file is initialized with a time-
*    stamp in the first column and "CD->DestSpectra" spectral columns followed
*    by "CD->DestSpectra" error columns.
\*********************/

{
  int Null, i, j, k, l, Elements;
  char MsgBuffer[128];
  float *Data = NULL, *Error = NULL, *TmpData = NULL, *TmpError = NULL;
  double *InTimeData, *OutTimeData;

  if (*Status) return;

 /*
  * Copy the time-stamp information in the first column. Not gonna work.
  */

  InTimeData = (double *)malloc(sizeof(double) * CD->InRows);
  ffgcvd(Std2Unit, 1, 1, 1, CD->InRows, 0, InTimeData, &Null, Status);
  if (CD->InInterval == CD->OutInterval)
    ffpcld(BackUnit, 1, 1, 1, CD->OutRows, InTimeData, Status);
  else
  {
    OutTimeData = (double *)malloc(sizeof(double) * CD->OutRows);
    for (i = 0; i < CD->OutRows; i++)
      OutTimeData[i] =
                    InTimeData[(int)((CD->OutInterval * i) / CD->InInterval)] +
	            fmod(CD->OutInterval * i, CD->InInterval);
    ffpcld(BackUnit, 1, 1, 1, CD->OutRows, OutTimeData, Status);
    free(OutTimeData);
  }
  free(InTimeData);

  for (i = 0; (i < CD->DestSpectra) && !*Status; i++)
  {
    strcpy(MsgBuffer, "Background for ");
    strncat(MsgBuffer, CD->DestSpecCat[i].Name, 100);
    Fcecho(MsgBuffer);
    Elements = CD->DestSpecCat[i].Elements;
    Data = (float *)malloc(sizeof(float) * CD->OutRows * Elements * 
			   (CD->Flags & CD_SYSTEMATIC_ERRORS ? 4 : 2));
    memset(Data, 0, sizeof(float) * CD->OutRows * Elements);
    TmpData = &Data[CD->OutRows * Elements];
    if (CD->Flags & CD_SYSTEMATIC_ERRORS)
    {
      Error = &TmpData[CD->OutRows * Elements];
      TmpError = &Error[CD->OutRows * Elements];
      memset(Error, 0, sizeof(float) * CD->OutRows * Elements);
    }
    else
      Error = TmpError = NULL;
    for (k = 0; (k < CD->SourceSpectra) && !*Status; k++)
      if (CD->SourceSpecCat[k].Dest == i + 1)
      {
	strcpy(MsgBuffer, "  Spectrum ");
	strncat(MsgBuffer, CD->SourceSpecCat[k].Name, 100);
	Fcecho(MsgBuffer);
	memset(TmpData, 0, sizeof(float) * CD->OutRows * Elements);
	if (CD->Flags & CD_SYSTEMATIC_ERRORS)
	  memset(TmpError, 0, sizeof(float) * CD->OutRows * Elements);
	ReadModel(CD, ModelCat, CD->SourceSpecCat[k].Name, TmpData, TmpError, 
		  Elements, Status);
	for (j = 0; j < CD->OutRows; j++)
	  for (l = 0; l < Elements; l++)
	  {
	    Data[Elements * j + l] += TmpData[Elements * j + l];
	    if (CD->Flags & CD_SYSTEMATIC_ERRORS)
	      Error[Elements * j + l] += TmpError[Elements * j + l] *
		                         TmpData[Elements * j + l];
	  }
      }

    if ((CD->Flags & CD_256_CHANNEL_XE_SPEC) && (Elements == 129))
    {
      TmpData = (float *)malloc(sizeof(float) * CD->OutRows * 256 * 2);
      TmpError = &TmpData[CD->OutRows * 256];
      if (CD->Flags & CD_SYSTEMATIC_ERRORS)
      {
	for (j = 0; j < CD->OutRows; j++)
	  ExpandSpectrum(&Data[j * Elements], &Error[j * Elements],
			 &TmpData[j * 256], &TmpError[j * 256]);
      }
      else
      {
	for (j = 0; j < CD->OutRows; j++)
	  ExpandSpectrum(&Data[j * Elements], NULL, &TmpData[j * 256], NULL);
      }
      if (CD->Flags & CD_CORRECT_XE_GAIN)
	for (j = 0; j < CD->OutRows; j++)
	  CorrectGain(CD, CD->DestSpecCat[i].PCU, &TmpData[j * 256], 
		      (CD->Flags & CD_SYSTEMATIC_ERRORS ? &TmpError[j * 256] :
		       NULL));
      ffpcle(BackUnit, i + 2, 1, 1, 256 * CD->OutRows, TmpData, Status);
      if (CD->Flags & CD_SYSTEMATIC_ERRORS)
	ffpcle(BackUnit, i + 2 + CD->DestSpectra, 1, 1, 256 * CD->OutRows,
	       TmpError, Status);
      free(TmpData);
    }
    else
    {
      ffpcle(BackUnit, i + 2, 1, 1, Elements * CD->OutRows, Data, Status);
      if (CD->Flags & CD_SYSTEMATIC_ERRORS)
	ffpcle(BackUnit, i + 2 + CD->DestSpectra, 1, 1, Elements * CD->OutRows,
	       Error, Status);
    }
    free(Data);
  }
}


void ReadModel(struct ConvDesc *CD, struct ModelCat *ModelCat, char *ParamName,
	       float *OutData, float *OutErrors, int Elements, int *Status)

/*********************\
*      ReadModel      *
*      ---------      *
* Parameters:
*    CD - The structure created by PlanOutput.
*    ModelCat - The catalog of available models made by MakeModelCat.
*    ParamName - The name of the spectrum for which the background is to be
*                modelled.
*    OutData - An initialized array of numbers with (CD->OutRows * Elements)
*              elements which will contain the background estimate upon return.
*    OutErrors - An initialized array of numbers with (CD->OutRows * Elements)
*                elements which will contain the background estimate upon
*                return.  If "Errors" is a NULL pointer, no background error
*                estimates will be returned.
*    Elements - The number of elements in the spectrum named by "ParamName".
*    Status - The FITSIO status flag.
* Function:
*    Searches the model catalog referred to by "ModelCat" and locates all
*    models which apply to the spectrum named "ParamName".  The values upon
*    which this model are based are located using CD->Catalog and used as
*    indices into the model array.
\*********************/

{
  char Comment[FLEN_COMMENT], **Name, ValueBuffer[FLEN_VALUE],
       TokenBuffer[80], MsgBuffer[256], TokenBuff2[80];
  int Type, Zeros, i, Offset, j, Length,
      StartBin, EndBin, k, AnyF, InterpAxes, DataOK, 
      ReportedError, PCU;
  long *Bins, *Bin, *LastBin, *Inc, Axes, *DeadCorr;
  double *RVal, *Delt, *TimeCorr, FirstBinExp, LastBinExp, *IFrac, **Interp,
         FirstMid = 0.0, *LowCut, *HighCut, *Special;
  struct ModelCat *Model;
  float *Data, *Errors, *Buffer, *ErrBuff, **Param=0, *TmpParam=0;

  if (*Status)
    return;

  /*
   *  Allocate some working space for the FITSIO routines.
   */

  if (!(Data = (float *)malloc(sizeof(float) * (OutErrors ? 2 : 1) *
			       (CD->OutRows) * Elements)))
  {
    *Status = PCABAT_URP_NO_MEMORY;
    return;
  }
  if (OutErrors)
    Errors = &Data[Elements * CD->OutRows];
  else
    Errors = NULL;

  /*
   *  Search the list of available models for models which affect the parameter
   *  "ParamName".
   */

  for (Model = ModelCat; Model->HDU && !*Status; Model++)
  {
    if (!strcmp(ParamName, Model->Name))
    {
      
      /*
       *  Load the description of the model from the model file into memory.
       */
      
      fits_movabs_hdu(Model->Unit, Model->HDU, &Type, Status);
      fits_read_key(Model->Unit, TLONG, "NAXIS", &Axes, Comment, Status);
      RVal = (double *)malloc(sizeof(double) * Axes * 7);
      memset(RVal, 0, sizeof(double) * Axes * 7);
      Delt = &RVal[Axes];
      TimeCorr = &Delt[Axes];
      IFrac = &TimeCorr[Axes];
      LowCut = &IFrac[Axes];
      HighCut = &LowCut[Axes];
      Special = &HighCut[Axes];
      Bins = (long *)malloc(sizeof(long) * Axes * 5);
      Bin = &Bins[Axes];
      LastBin = &Bin[Axes];
      Inc = &LastBin[Axes];
      DeadCorr = &Inc[Axes];
      Interp = (double **)malloc(sizeof(double *) * Axes);
      memset(Interp, 0, sizeof(double *) * Axes);
      for (i = 0; i < Axes; i++)
	Inc[i] = 1;
      Name = (char **)malloc(sizeof(char *) * Axes);
      Name[0] = (char *)malloc(FLEN_VALUE * (Axes));
      for (i = 1; i < Axes; i++)
	Name[i] = &Name[0][FLEN_VALUE * i];
      fits_read_key(Model->Unit, TSTRING, "DETNAM", ValueBuffer, Comment,
		    Status);
      PCU = atoi(&ValueBuffer[3]);
      fits_read_key(Model->Unit, TSTRING, "CTYPE1", Name[0], Comment, Status);
      ffgkyj(Model->Unit, "NAXIS1", &Bins[0], Comment, Status);

      /* First dimension should always be 'Channel' */
      if (strcasecmp(Name[0], PHANAME))
      {
	*Status = PCABAT_URP_MODEL_ODD;
	return;
      }
      if (Bins[0] != Elements)
      {
	*Status = PCABAT_URP_WRONG_PHA_CHAN;
	return;
      }
      if (Axes > 1)
      {
	ffgkys(Model->Unit, "CTYPE2", Name[1], Comment, Status);
	if (strcasecmp(Name[1], ERRORNAME))
	  Offset = 1;
	else
	{
	  ffgkyj(Model->Unit, "NAXIS2", &Bins[1], Comment, Status);
	  Offset = 2;
	}
      }
      else
	Offset = 1;
      InterpAxes = 0;
      for (i = Offset; i < Axes; i++)
      {
	ffkeyn("CTYPE", i + 1, TokenBuffer, Status);
	ffgkys(Model->Unit, TokenBuffer, Name[i], Comment, Status);
	if (*Status == 202)
	{
	  sprintf(MsgBuffer, "Model missing required keyword \"%s\"",
		  TokenBuffer);
	  ffpmsg(MsgBuffer);
	  return;
	}
	ffkeyn("CRVAL", i + 1, TokenBuffer, Status);
	ffgkyd(Model->Unit, TokenBuffer, &RVal[i], Comment, Status);
	if (*Status == 202)
	{
	  sprintf(MsgBuffer, "Model missing required keyword \"%s\"",
		  TokenBuffer);
	  ffpmsg(MsgBuffer);
	  return;
	}
	ffkeyn("CDELT", i + 1, TokenBuffer, Status);
	ffgkyd(Model->Unit, TokenBuffer, &Delt[i], Comment, Status);
	if (*Status == 202)
	{
	  sprintf(MsgBuffer, "Model missing required keyword \"%s\"",
		  TokenBuffer);
	  ffpmsg(MsgBuffer);
	  return;
	}
        if (!*Status)
	{
	  ffkeyn("CUNIT", i + 1, TokenBuffer, Status);
	  ffgkys(Model->Unit, TokenBuffer, ValueBuffer, Comment, Status);
	  if (*Status == 202)
	  {
	    ffcmsg();
	    TimeCorr[i] = 1.0;
	    *Status = 0;
	  }
	  else
	  {
	    if (strstr(ValueBuffer, "/s"))
	      TimeCorr[i] = CD->InInterval;
	    else
	      TimeCorr[i] = 1.0;
	  }
	}
        if (!*Status)
	{
	  ffkeyn("CLOCT", i + 1, TokenBuffer, Status);
	  ffgkyd(Model->Unit, TokenBuffer, &LowCut[i], Comment, Status);
	  if (*Status == 202)
	  {
	    ffcmsg();
	    LowCut[i] = -HUGE_VAL;
	    *Status = 0;
	  }
	}
        if (!*Status)
	{
	  ffkeyn("CHICT", i + 1, TokenBuffer, Status);
	  ffgkyd(Model->Unit, TokenBuffer, &HighCut[i], Comment, Status);
	  if (*Status == 202)
	  {
	    ffcmsg();
	    HighCut[i] = HUGE_VAL;
	    *Status = 0;
	  }
	}
        if (!*Status)
	{
	  ffkeyn("CSVAL", i + 1, TokenBuffer, Status);
	  ffgkyd(Model->Unit, TokenBuffer, &Special[i], Comment, Status);
	  if (*Status == 202)
	  {
	    ffcmsg();
	    Special[i] = 0.0;
	    *Status = 0;
	  }
	}
        if (!*Status)
	{
	  fits_make_keyn("CDEDT", i + 1, TokenBuffer, Status);
	  fits_read_key(Model->Unit, TSTRING, TokenBuffer, ValueBuffer,
			Comment, Status);
	  if (*Status == 202)
	  {
	    fits_clear_errmsg();
	    DeadCorr[i] = 0;
	    *Status = 0;
	  }
	  else
	    DeadCorr[i] = 1;
	}
	ffkeyn("NAXIS", i + 1, TokenBuffer, Status);
	ffgkyj(Model->Unit, TokenBuffer, &Bins[i], Comment, Status);
	if (*Status == 202)
	{
	  sprintf(MsgBuffer, "Model missing required keyword \"%s\"",
		  TokenBuffer);
	  ffpmsg(MsgBuffer);
	  return;
	}
        if (!*Status && (CD->Flags & CD_INTERPOLATE))
	{
	  ffkeyn("CMID", i + 1, TokenBuff2, Status);  
	  ffnkey(1, TokenBuff2, TokenBuffer, Status);  
	  ffgkyd(Model->Unit, TokenBuffer, &FirstMid, Comment, Status);
	  if (*Status == 202)
	  {
	    ffcmsg();
	    Interp[i] = NULL;
	    *Status = 0;
	  }
	  else
	  {
	    InterpAxes += 1;
	    Interp[i] = (double *)malloc(sizeof(double) * Bins[i]);
	    Interp[i][0] = FirstMid;
	    for (j = 1; j < Bins[i]; j++)
	    {
	      ffnkey(j + 1, TokenBuff2, TokenBuffer, Status);  
	      ffgkyd(Model->Unit, TokenBuffer, &Interp[i][j], Comment, Status);
	    }
	  }
	}
        else Interp[i] = NULL;
      }
      
      Buffer = (float *)malloc(sizeof(float) * Elements * (1 << InterpAxes) *
			       (Errors ? 2 : 1));
      if (Errors)
	ErrBuff = &Buffer[(1 << InterpAxes) * Elements];
      else
	ErrBuff = NULL;

      /*
       *  Load the data for the relevant parameters from the Standard mode
       *  2 file into memory.
       */
      
      if (Axes > Offset)
      {
	Param = (float **)malloc(sizeof(float *) * (Axes - Offset));
	TmpParam = (float *)malloc(sizeof(float) *
				   max(CD->InRows, CD->FiltRows));
	Param[0] = (float *)malloc(sizeof(float) * (Axes - Offset) *
				   CD->InRows);
	for (i = 0; i < Axes - Offset; i++)
	  Param[i] = &Param[0][CD->InRows * i];
      }
      for (i = Offset; i < Axes; i++)
      {
	int CalcDone = 0;

	/* Find the requested component in the catalog, which can
	   ultimately be from Std2, FiltFile ... */
	for (j = 0; (j < CD->CatColumns) &&
	            (strcmp(Name[i], CD->Catalog[j].Name)); j++);

	/* ... not found among the official Std2 or FiltFile names, so
	   try the internal calculator... */
	if (j == CD->CatColumns && *Status == 0)
	{
	  
	  if (CalcInternal(CD, Name[i], Param[i - Offset], Special[i],
			   Status) != 0)
	    {
	      /* ... we didn't find a matching calculated model! */
	      if (*Status == -1) {
		*Status = PCABAT_URP_NO_PARAM_DATA;
		sprintf(MsgBuffer, "\"%s\" is not available in input files.", Name[i]);
		ffpmsg(MsgBuffer);
	      } else {
		sprintf(MsgBuffer, "Unable to calculate \"%s\" model.", Name[i]);
		ffpmsg(MsgBuffer);
	      }		
	      return;
	    }
	  else 
	    {
	      /* ... we did find a calculated model */
	      CalcDone = 1;
	    }
	}

	if (! CalcDone )
	{
	  /* This is either from the Std2 or FiltFile */
	  ffgcve(CD->Catalog[j].Unit, CD->Catalog[j].Column, 1, 1,
		 CD->Catalog[j].Rows, 0, TmpParam, &Zeros, Status);
	  if (CD->Catalog[j].Unit == CD->FiltUnit)
	    for (k = 0; k < CD->InRows; k++)
	      Param[i - Offset][k] = TmpParam[CD->FiltConv[k]];
	  else
	    for (k = 0; k < CD->InRows; k++)
	      Param[i - Offset][k] = TmpParam[k];
	}

	/* Perform deadtime correction if requested */
	if (DeadCorr[i])
	{
	  if (!LiveFrac[0])
	    ComputeLiveFraction(CD, Status);
	  for (k = 0; k < CD->InRows; k++)
	    Param[i - Offset][k] /= LiveFrac[PCU][k];	  
	}
      }

      if (*Status)
	return;

      /* Get the model data from the file */

      strcpy(MsgBuffer, "    Applying ");
      ReportedError = FALSE;
      if (strlen(Model->AltName) == 0)
      {
        for (i = Offset; i < Axes; i++)
        {
	  Length = strlen(MsgBuffer);
	  sprintf(&MsgBuffer[Length], "%s ", Name[i]);
        }
      }
      else sprintf(&MsgBuffer[strlen(MsgBuffer)], "%s ", Model->AltName);
      strcat(MsgBuffer, "model.");
      if (Offset == 1)
	strcat(MsgBuffer, " (No error estimates)");
      Fcecho(MsgBuffer);
      
      memset(Data, 0, sizeof(float) * (OutErrors ? 2 : 1) * (CD->OutRows) * 
	     Elements);

      /*
       *  Examine the input file row-by-row to compute the background estimate.
       */

      for (i = 0; i < CD->InRows; i++)
      {

	/*
	 *  Compute which model bin is associated with the input data.
	 */

	for (j = 0; j < Offset; j++)
	  IFrac[j] = 0.0;

	DataOK = TRUE;
	for (j = Offset; j < Axes; j++)
	{
	  if (finite(Param[j - Offset][i]))
	  {
	    if ((Param[j - Offset][i] / TimeCorr[j] > LowCut[j]) &&
		(Param[j - Offset][i] / TimeCorr[j] < HighCut[j]))
	    {
	      if (Interp[j])
	      {
		for (k = 0; (k < Bins[j]) &&
		     (Param[j - Offset][i] / TimeCorr[j] > Interp[j][k]); k++);
		LastBin[j] = k + 1;
		Bin[j] = k;
		if (k <= 0)
		{
		  Bin[j] = 1;
		  LastBin[j] = 1;
		  IFrac[j] = 0.0;
		}
		else
		{
		  if (k >= Bins[j])
		  {
		    Bin[j] = Bins[j];
		    LastBin[j] = Bins[j];
		    IFrac[j] = 0.0;
		  }
		  else
		    IFrac[j] =
		      (Param[j - Offset][i] / TimeCorr[j] - Interp[j][k - 1]) /
			(Interp[j][k] - Interp[j][k - 1]);
		}
	      }
	      else
	      {
		IFrac[j] = 0.0;
		Bin[j] = (Param[j - Offset][i] / TimeCorr[j] - RVal[j]) /
		  Delt[j] + 1;
		if (Bin[j] > Bins[j])
		  Bin[j] = Bins[j];
		if (Bin[j] < 1)
		  Bin[j] = 1;
		LastBin[j] = Bin[j];
	      }
	    }
	    else
	      DataOK = FALSE;
	  }
	  else
	    DataOK = FALSE;
	}
	if (DataOK)
	{
	  Bin[0] = 1;
	  LastBin[0] = Elements;
	  if (Offset == 2)
	    LastBin[1] = Bin[1] = 1;

	  /*
	   *  Load the model data associated with this input data.
	   */

	  ffgsve(Model->Unit, 0, Axes, Bins, Bin, LastBin, Inc, 0, Buffer,
		 &AnyF, Status);
	  if (Errors && (Offset == 2))
	  {
	    LastBin[1] = Bin[1] = 2;
	    ffgsve(Model->Unit, 0, Axes, Bins, Bin, LastBin, Inc, 0, ErrBuff,
		   &AnyF, Status);
	  }
	  
	  /*
	   *  Calculate which output background estimates correspond to this
	   *  input data.
	   */
	  
/*	  for (m = 0; m < (1 << InterpAxes); m++)
	  {
	    printf("%d: ", m);
	    for (n = 10; n < 15; n++)
	      printf("%6.2f ", Buffer[m * Elements + n] * 100.0);
	    printf("\n");
	  }
	  printf("IFrac: ", m);
	  for (m = 0; m < Axes; m++)
	    printf("%6.2f ", IFrac[m]);
	  printf("\n"); */

	  Interpolate(Buffer, ErrBuff, IFrac, Elements, Axes);

/*	  printf("Interp: ");
	  for (n = 10; n < 15; n++)
	    printf("%6.2f ", Buffer[n] * 100.0);
	  printf("\n\n"); */

	  StartBin = (int) ((double)i / CD->Scale);
	  if (CD->Scale == 1.0)
	    EndBin = StartBin;
	  else
	    EndBin = (int) ((double)(i + 1) / CD->Scale);
	  if (StartBin == EndBin)
	  {
	    for (j = 0; j < Elements; j++)
	      Data[Elements * StartBin + j] += CD->InInterval * Buffer[j];
	  }
	  else
	  {
	    FirstBinExp = (StartBin + 1) * CD->OutInterval - CD->InInterval *
	                  (double)i;
	    LastBinExp = (double)(i + 1) * CD->InInterval -
	                 EndBin * CD->OutInterval; 
	    for (j = 0; j < Elements; j++)
	      Data[Elements * StartBin + j] += FirstBinExp * Buffer[j];
	    for (j = StartBin + 1; j < EndBin; j++)
	      for (k = 0; k < Elements; k++)
		Data[Elements * j + k] += CD->OutInterval * Buffer[k];
	    for (j = 0; j < Elements; j++)
	      Data[Elements * EndBin + j] += LastBinExp * Buffer[j];
	    if (Errors && (Offset == 2))
	    {
	      for (j = 0; j < Elements; j++)
		Errors[Elements * StartBin + j] += FirstBinExp * Buffer[j] *
		                                   ErrBuff[j];
	      for (j = StartBin + 1; j < EndBin; j++)
		for (k = 0; k < Elements; k++)
		  Errors[Elements * j + k] += CD->OutInterval * Buffer[k] *
		                              ErrBuff[k];
	      for (j = 0; j < Elements; j++)
		Errors[Elements * EndBin + j] += LastBinExp * Buffer[j] *
                                                 ErrBuff[j];
	    }
	  }
	}
      }
      for (i = 0; i < CD->OutRows; i++)
      {
	if (fabs(Data[Elements * i + 72]) + fabs(Data[Elements * i + 73]) > 200)
	{
	  printf("B: %d: ", i);
	  for (j = 71; j < 75; j++)
	  {
	    printf("%g ", Data[Elements * i + j]);
	  }
	  printf("\n");
	}
      }
      DriftGain(CD, Model, Data, Errors, Elements);
      for (i = 0; i < CD->OutRows; i++)
      {
	if (fabs(Data[Elements * i + 72]) + fabs(Data[Elements * i + 73]) > 200)
	{
	  printf("A: %d: ", i);
	  for (j = 71; j < 75; j++)
	  {
	    printf("%g ", Data[Elements * i + j]);
	  }
	  printf("\n");
	}
      }
      for (i = 0; i < CD->OutRows; i++)
	for (j = 0; j < Elements; j++)
	{
	  OutData[Elements * i + j] += Data[Elements * i + j];
	  if (Errors && (Offset == 2))
	    if (Data[Elements * i + j] != 0.0)
	      OutErrors[Elements * i + j] += Errors[Elements * i + j] /
		                             Data[Elements * i + j];
	}
      for (i = 0; i < Axes; i++)
	if (Interp[i])
	  free(Interp[i]);
      free(Buffer);
      free(Interp);
      free(RVal);
      free(Name[0]);
      free(Name);
      if (Axes > Offset)
      {
	free(Param[0]);
	free(TmpParam);
	free(Param);
      }
      free(Bins);
    }
  }
  free(Data);
}

void Interpolate(float *Buffer, float *ErrBuff, double *IFrac, int Elements,
		 int Axes)

/*********************\
*     Interpolate     *
*     -----------     *
* Function:
*    Interpolates between bins in a multidimensional array of dimension 
*    (2, 2, ...).  The output is written into the first "Element" bins of
*    the input arrays "Buffer" and "ErrBuff".
* Parameters:
*    Buffer - On input, data from the FITS file requiring interpolation.  The
*             format of this array is (Elements, 2, 2, ...) Where there are
*             "Axes" axes of dimension 2.  On output, the first "Elements" 
*             elements of the array contain the interpolated spectrum.
*    ErrBuff - Same as "Buffer" but contains the error values which are
*              interpolated somewhat differently.
*    IFrac - An array of dimension "Axes" which contains fractional values
*            which determine where the interpolation point is between the 
*            two bins of each axis.
*    Elements - The number of spectral elements in the data.
*    Axes - The number of axes on which to interpolate
\*********************/

{
  int i, j, k, Done = 0, IntAxes = 0;
  float Interp;

  for (i = 0; i < Axes; i++)
    if (IFrac[i] != 0.0)
    {
      IntAxes += 1;
/*      printf("%d: IFrac = %f\n", i, IFrac[i]); */
    }
  if (IntAxes == 0)
    return;

  for (i = 0; i < Axes; i++)
  {
    if (IFrac[i] != 0.0)
    {

      for (j = 0; j < (1 << IntAxes); j += (1 << (Done + 1)))
      {
/*	printf("    %d: %f\n", j, Buffer[j * Elements + 24]);
	printf("    %d: %f\n", (j + (1 << Done)), Buffer[(j + (1 << Done)) *
							 Elements + 24]); */
	for (k = 0; k < Elements; k++)
	{
	  if (finite(Buffer[j * Elements + k]))
	  {
	    if (finite(Buffer[(j + (1 << Done)) * Elements + k]))
	      Interp = Buffer[j * Elements + k] * (1.0 - IFrac[i]) +
		       Buffer[(j + (1 << Done)) * Elements + k] * IFrac[i];
	    else
	      Interp = Buffer[j * Elements + k];
	  }
	  else
	    Interp = Buffer[(j + (1 << Done)) * Elements + k];
	  if (ErrBuff)
	    if (Interp != 0.0)
	      ErrBuff[j * Elements + k] = (ErrBuff[j * Elements + k] *
					   Buffer[j * Elements + k] *
					   (1.0 - IFrac[i]) *
					   (1.0 - IFrac[i]) +
					   ErrBuff[(j + (1 << Done)) *
						   Elements + k] *
					   Buffer[(j + (1 << Done)) *
						  Elements +
						  k] * IFrac[i] * IFrac[i]) /
					  Interp;
	  Buffer[j * Elements + k] = Interp;
	}
      }
      Done += 1;
    }
  }
/*  printf("    Final = %f\n", Buffer[24]); */
}

int SpecSrc[256] =
{
  0,   0,   0,   0,   0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,
 12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,
 28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,
 44,  45,  46,  47,  48,  49,  50,  50,  51,  51,  52,  52,  53,  53,  54,  54,
 55,  55,  56,  56,  57,  57,  58,  58,  59,  59,  60,  60,  61,  61,  62,  62,
 63,  63,  64,  64,  65,  65,  66,  66,  67,  67,  68,  68,  69,  69,  70,  70,
 71,  71,  72,  72,  73,  73,  74,  74,  75,  75,  76,  76,  77,  77,  78,  78,
 79,  79,  80,  80,  81,  81,  82,  82,  83,  83,  84,  84,  85,  85,  86,  86,
 87,  87,  88,  88,  89,  89,  90,  90,  91,  91,  91,  92,  92,  92,  93,  93,
 93,  94,  94,  94,  95,  95,  95,  96,  96,  96,  97,  97,  97,  98,  98,  98,
 99,  99,  99, 100, 100, 100, 101, 101, 101, 102, 102, 102, 103, 103, 103, 104,
104, 104, 105, 105, 105, 106, 106, 106, 107, 107, 107, 108, 108, 108, 109, 109,
109, 110, 110, 110, 111, 111, 111, 112, 112, 112, 113, 113, 113, 114, 114, 114,
115, 115, 115, 116, 116, 116, 117, 117, 117, 118, 118, 118, 119, 119, 119, 120,
120, 120, 121, 121, 121, 122, 122, 122, 123, 123, 123, 124, 124, 124, 125, 125,
125, 125, 126, 126, 126, 126, 127, 127, 127, 127, 128, 128, 128, 128, 128, 128
};

int SpecChans[129] = 
{
  5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  4, 4, 4, 6
};

int SpecDest[129] =
{
    0,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,  16,  17,  18,
   19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  32,  33,
   34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  48,
   49,  50,  51,  52,  53,  54,  56,  58,  60,  62,  64,  66,  68,  70,  72,
   74,  76,  78,  80,  82,  84,  86,  88,  90,  92,  94,  96,  98, 100, 102,
  104, 106, 108, 110, 112, 114, 116, 118, 120, 122, 124, 126, 128, 130, 132,
  134, 136, 139, 142, 145, 148, 151, 154, 157, 160, 163, 166, 169, 172, 175,
  178, 181, 184, 187, 190, 193, 196, 199, 202, 205, 208, 211, 214, 217, 220,
  223, 226, 229, 232, 235, 238, 242, 246, 250
};

float SpecFrac[256] =
{
  0.2, 0.2, 0.2, 0.2, 0.2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,0.5, 0.5,
  0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
  0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
  0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
  0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
  0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.3333, 0.3333,
  0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333,
  0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333,
  0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333,
  0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333,
  0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333,
  0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333,
  0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333,
  0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333,
  0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333,
  0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333,
  0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333, 0.3333,
  0.3333, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25,
  0.25, 0.1667, 0.1667, 0.1667, 0.1667, 0.1667, 0.1667
};

void ExpandSpectrum(float *ShortSpec, float *ShortErr, float *FullSpec,
		    float *FullErr)

/*********************\
*    ExpandSpectrum   *
*    --------------   *
* Expands a 129 channel spectrum to a 256 channel spectrum dividing the
* counts in each channel according to the tables "SpecSrc" and "SpecFrac"
* above.
\*********************/

{
  int i;
  float *InterpSpec;

  InterpSpec = MakeInterpSpec(ShortSpec);
  for (i = 0; i < 256; i++)
  {
    FullSpec[i] = ShortSpec[SpecSrc[i]] * SpecFrac[i] * InterpSpec[i];
    if (FullErr && ShortErr)
      FullErr[i] = ShortErr[SpecSrc[i]] * SpecFrac[i] * SpecFrac[i] * 
	           InterpSpec[i] * InterpSpec[i];
  }
  FreeInterpSpec(InterpSpec);
}

/* int TmpFlag = 0; */

float *MakeInterpSpec(float *ShortSpec)

/*********************\
*    MakeInterpSpec   *
*    --------------   *
*     Defines the array which when multiplied by the SpecFrac array
* interpolates more smoothly from bin to bin.
\*********************/

{
  int i, j;
  float Rise, Run, Slope, Center, *InterpSpec;

  InterpSpec = malloc(sizeof(float) * 256);
  for (i = 0; i < 129; i++)
  {
    if ((i == 0) || (i == 128) || (SpecChans[i] == 1) || (ShortSpec[i] == 0))
    {
      for (j = 0; j < SpecChans[i]; j++)
	InterpSpec[SpecDest[i] + j] = 1.0;
    }
    else
    {
      float AvgSpec;
      float Sign = ShortSpec[i+1] * ShortSpec[i-1];
      Rise = ShortSpec[i + 1] / SpecChans[i + 1] -
	      ShortSpec[i - 1] / SpecChans[i - 1];
      Run = SpecDest[i + 1] + (SpecChans[i + 1] - 1.0) / 2.0 - 
	    SpecDest[i - 1] - (SpecChans[i - 1] - 1.0) / 2.0;
      /* XXX: question 2, there is a problem in the next statement when
	 ShortSpec[i+1] + ShortSpec[i-1] is near 0!!! */
      AvgSpec = (ShortSpec[i + 1] + ShortSpec[i - 1]);
      if (ShortSpec[i+1] == 0 && ShortSpec[i-1] == 0) {
	AvgSpec = 1;
	/* Sanity checking for this insane interpolation method */
      } else if (Sign < 0 && fabs(AvgSpec) < 0.2 * fabs(ShortSpec[i-1])) {
	AvgSpec = 0.2 * ShortSpec[i-1];
      } else if (Sign < 0 && fabs(AvgSpec) < 0.2 * fabs(ShortSpec[i+1])) {
	AvgSpec = 0.2 * ShortSpec[i+1];
      }
      Slope = Rise / Run / AvgSpec * (SpecChans[i + 1] + SpecChans[i - 1]);
      Center = SpecDest[i] + (SpecChans[i] - 1.0) / 2.0;
      for (j = 0; j < SpecChans[i]; j++)
	InterpSpec[SpecDest[i] + j] = 1.0 + Slope * (SpecDest[i] + j - Center);
/*
      for (j = 0; j < SpecChans[i]; j++)
	if (fabs(InterpSpec[SpecDest[i] +j]) > 20.0)
       	  printf("Bad: i:%d j:%d Slope:%g=%g / %g (%g / %d)   +[%g] -[%g]\n", i, j, Slope, Rise, Run, AvgSpec, SpecChans[i+1] + SpecChans[i-1], ShortSpec[i-1], ShortSpec[i+1]);
*/

    }
  }
/*  if (TmpFlag == 0)
  {
    for (i = 0; i < 256; i++) printf("%d: %g\n", i, InterpSpec[i]);
    TmpFlag = 1;
    } */
  return InterpSpec;
}

void FreeInterpSpec(float *InterpSpec)

{
  free(InterpSpec);
}

void LoadGainCorr(struct ConvDesc *CD, char *File, int Extension, int *Status)

/*********************\
*    LoadGainCorr     *
*    ------------     *
* Loads the EDS gain correction from the gain correction file.
\*********************/

{
  fitsfile *GainUnit;
  int i, Dummy;

  fits_open_file(&GainUnit, File, READONLY, Status);
  fits_movabs_hdu(GainUnit, Extension + 1, &Dummy, Status);
  for (i = 0; i < 5; i++)
  {
    fits_read_col(GainUnit, TLONG, i + 2, 2, 1, 1, 0, &CD->Gain[i], &Dummy,
		  Status);
    fits_read_col(GainUnit, TLONG, i + 7, 2, 1, 1, 0, &CD->Offset[i], &Dummy,
		  Status);
  }
  fits_close_file(GainUnit, Status);
}

void CorrectGain(struct ConvDesc *CD, int PCU, float *Data, float *Error)

/*********************\
*     CorrectGain     *
*     -----------     *
* Function:
*    Simulates the EDS gain correction.
* Parameters:
*    CD - The structure created by PlanOutput.
*    PCU - The number of the PCU
*    Data - The data spectrum used for input and output.
*    Error - The error spectrum.
\*********************/

{
  float Tmp[256], TmpErr[256];
  int i, NewInd;

  for (i = 0; i < 256; i++)
    Tmp[i] = TmpErr[i] = 0.0;
  for (i = 0; i < 256; i++)
  {
    NewInd = (i * (CD->Gain[PCU] + 256) + 128) / 256 + CD->Offset[PCU];
    if ((NewInd >= 0) && (NewInd < 256))
    {
      Tmp[NewInd] += Data[i];
      if (Error)
	TmpErr[NewInd] += Error[i];
    }
  }
  for (i = 0; i < 256; i++)
  {
    Data[i] = Tmp[i];
    if (Error)
      Error[i] = TmpErr[i];
  }
}

void DriftGain(struct ConvDesc *CD, struct ModelCat *Model, float *Data,
	       float *Error, int Elements)

/*********************\
*      DriftGain      *
*      ---------      *
*     Makes a fine correction to a spectrum based upon a gradual gain drift
* in the PCA.  This correction is such that the mean energy of channel 'i' in
* 256 channel space is:
*
*                ch(i, t) = ch(i, t0) * (1 + (t - t0) * Slope).
*
* Consequently, the counts 'dC' that spill from channel 'i' to the next
* channel is:
*
*              dC(i, t) = ch(i, t0) * (t - t0) * Slope * C(i, t).
* Parameters:
*    CD - The conversion description
*    Model - The model catalog entry for the model being processed.
*    Data - The estimate from this model to be corrected.  The data
*           are corrected in place.  On output this array contains the
*           results.
*    Error - The error array from the model.  Nothing is done with this 
*            informaiton.
*    Elements - The number of spectral bins in "Data".  This routine will
*               only function for 129 channel data.
\*********************/

{
  int i, j, Start, Stop, Direction, Channel;
  double Factor, Delta;
  float *InterpSpec;

  if ((Elements == 129) && (Model->GainDriftSlope != 0.0))
  {
    /* "Slope" is in units of (fractional change) per day */
    Factor = (CD->ObsMJD - Model->GainDriftTZero) * Model->GainDriftSlope;
    Direction = (Factor > 0) ? 1 : -1;
    Start = (Direction > 0) ? 127 : 1;
    Stop = (Direction > 0) ? 0 : 128;
    for (i = 0; i < CD->OutRows; i++)
    {
      /* XXX: question1, why is Data being used repeatedly here, even
	 though we are processing (Data+i*129)? */
      InterpSpec = MakeInterpSpec(Data+i*129);
      for (j = Start; j != Stop; j -= Direction)
      {
	Channel = (Direction == 1) ? SpecDest[j + 1] - 1 : SpecDest[j];
	Delta = Channel * fabs(Factor) * SpecFrac[Channel] *
	        InterpSpec[Channel];
	Data[Elements * i + j + Direction] += Delta * Data[Elements * i + j];
	Data[Elements * i + j] -= Delta * Data[Elements * i + j];
      }
      FreeInterpSpec(InterpSpec);
    }
  }
}


void Urp(int Index)

/*********************\
*         Urp         *
*         ---         *
*      Prints out the error message corresponding to the "Index" value.  If 
* "Index" is negative then it is an index into the PCABackEst internal error
* message list.  Otherwise it is a FITS status value and the error message is
* given by FITSIO.  Also prints the contents of the FITSIO message
* buffer.
\*********************/

{
  char ErrText[80], Buffer[256];

  strcpy(ErrText, "");
  if (Index <= 0)
  {
    if ((-Index < URP_MESSAGES + 100) && (-Index >= 100))
      strcpy(ErrText, UrpMessages[-Index - 100]);
  }
  else {
    ffgerr(Index, ErrText);
    fits_report_error( stdout, Index );
  }

  sprintf(Buffer, "Error - %s", ErrText);
  Fcerr(Buffer);
}

struct ModelCat *MakeModelCat(fitsfile *Std2Unit, char *NameList,
			      int MaxModels, int BGType, int *Status)

/*********************\
*    MakeModelCat     *
*    ------------     *
*      Make a catalog of the models in the files named in the "NameList" 
* argument. "caldb" is an acceptible value of this argument in which case the
* caldb is searched for model files.  If "NameList" is a list of files, only
* those models which are valid at the time of the Std2Unit file are included.
* Parameters:
*    Std2Unit - An open standard 2 file.
*    NameList - A list of file names to search for models.  "caldb" is OK.
*    MaxModels - The maximum number of models for which there is storage space
*    BGType - A flag to include internal or external models or both.
*    Status - The FITSIO status flag.
\*********************/

{
  struct ModelCat *Catalog;
  double ObsMJD;
  int Models = 0, i, *ModExt, NumFiles = 0, NegFlag, MaxElem_2, FirstExt,
      j, LastExt, Done, Type, ModelGood, NFound = 0;
  char ObsDate[FLEN_VALUE], ObsTime[FLEN_VALUE],
       CommBuff[FLEN_COMMENT], **ModNames, **Online, MsgBuffer[MAXC_FNAME+100],
       TmpName[MAXC_FNAME], ModelName[FLEN_VALUE],
       BkgdSrc[FLEN_VALUE];
  fitsfile *ModelUnit;
  int NGoodModels, BadTime, ModelStart;

  if (*Status) return 0;

  Catalog = (struct ModelCat *)malloc(sizeof(struct ModelCat) * MaxModels);
  ModExt = (int *)malloc(sizeof(int) * MaxModels);
  memset(ModExt, 0, sizeof(int) * MaxModels);
  ModNames = (char **)malloc(sizeof(char *) * MaxModels * 2);
  ModNames[0] = (char *)malloc(sizeof(char) * MaxModels * 2 * MAXC_FNAME);
  memset(ModNames[0], '\0', sizeof(char) * MaxModels * 2 * MAXC_FNAME);
  for (i = 1; i < MaxModels * 2; i++)
    ModNames[i] = &ModNames[i - 1][MAXC_FNAME];
  Online = &ModNames[MaxModels];

  if (!strcasecmp(NameList, "caldb") && !*Status)
  {
    fits_read_key(Std2Unit, TSTRING, "DATE-OBS", ObsDate, CommBuff, Status);
    fits_read_key(Std2Unit, TSTRING, "TIME-OBS", ObsTime, CommBuff, Status);
    CalFix(CALDB_MODEL_ID, ObsDate, ObsTime, MaxModels, MAXF_FNAME,
	   ModNames[0], ModExt, Online[0], &NumFiles, &NFound, Status);
    if (NFound == 0)
    {
      *Status = PCABAT_URP_NOTIN_CALDB;
      fits_write_errmsg("No model files found in the calibration database.");
    }
    if (NFound > MaxModels)
      Urp(PCABAT_URP_MAXMOD_TOO_SMALL);
  }
  else
  {
    MaxElem_2 = MaxModels;
    Fcgcls(NameList, ModNames[0], &NumFiles, &NegFlag);
    if (NumFiles == 0)
      *Status = PCABAT_URP_NO_MODELS;      
    for (i = 0; i < NumFiles; i++)
    {
      Fcpars(ModNames[i], TmpName, &ModExt[i], Status);
      strcpy(ModNames[i], TmpName);
    }
  }

  ObsMJD = GetObsMJD(Std2Unit, Status);

  Fcecho("Model data taken from the following files:");
  AddHistory(" Model data taken from the following files:");
  for (i = 0; (i < NumFiles) && !*Status; i++)
  {
    FirstExt = ModExt[i];
    LastExt = ModExt[i];
    if ((ModExt[i] == -1) || (ModExt[i] == -99))
    {
      FirstExt = 0;
      /* Note: LastExt is not the same as MaxModels because some
	 models may be rejected, and thus do not contribute to the
	 limit. */
      LastExt  = 100000;   /* Arbitrarily high number */
    }
    if ((i == 0) || strcmp(ModNames[i], ModNames[max(i - 1, 0)]))
    {
      sprintf(MsgBuffer, "   %s", ModNames[i]);
      Fcecho(MsgBuffer);
      AddHistory(MsgBuffer);
      fits_open_file(&ModelUnit, ModNames[i], READONLY, Status);
      if (*Status) {
	sprintf(MsgBuffer, "ERROR: could not open %s", ModNames[i]);
	Fcerr(MsgBuffer);
	Urp(PCABAT_URP_MODEL_ODD);
	return 0;
      }
    }
    Done = FALSE;
    BadTime = FALSE;  /* Flags if we discover a model with bad time range */
    NGoodModels = 0;  /* Number of good models in this file only */
    ModelStart = Models;
    for (j = FirstExt; (j < LastExt + 1) && !*Status && !Done; j++)
    {
      fits_movabs_hdu(ModelUnit, j + 1, &Type, Status);
      if (!*Status)
      {
	if (ModExt[i] == -99)
	{
	  if (ValidModelTime(ModelUnit, ObsMJD, Status))
	    ModelGood = TRUE;
	  else {
	    ModelGood = FALSE;
	    BadTime = TRUE;
	    *Status = 0;
	  }
	}
	else
	  ModelGood = TRUE;
	if (ModelGood)
	{
          fits_read_key(ModelUnit, TSTRING, "BGSRC", BkgdSrc, CommBuff,
			Status); 
	  if (*Status == 202)
	  {
	    /* Default case: model file did not specify BGSRC.  Assume BOTH */
	    fits_clear_errmsg();
	    *Status = 0;
	    /* ... but make sure this agrees with what the user specified at 
	       the command line. */
	    if ((BGType & MT_BOTH) != MT_BOTH)
	      ModelGood = FALSE;
	  }
	  else
	  {
	    /* If BGSRC != INTERNAL but BGType == MT_INTERNAL, fail */
	    if (strncasecmp("INTERNAL", BkgdSrc, strlen(BkgdSrc)) &&
		(BGType == MT_INTERNAL))
	      ModelGood = FALSE;
	    /* If BGSRC != EXTERNAL but BGType == MT_EXTERNAL, fail */
	    if (strncasecmp("EXTERNAL", BkgdSrc, strlen(BkgdSrc)) &&
		(BGType == MT_EXTERNAL))
	      ModelGood = FALSE;
	  }
	}
	if (ModelGood)
	{
	  ModelName[0] = 0;
	  fits_read_key(ModelUnit, TSTRING, "BTYPE", ModelName, CommBuff,
			Status); 
	  strcpy(Catalog[Models].Name, ModelName);
	  strcpy(Catalog[Models].AltName, "");
	  if (!*Status)
	  {
	    fits_read_key(ModelUnit, TSTRING, "ALTNAME", ModelName, CommBuff,
			  Status); 
	    if (*Status == 202)
	    {
	      fits_clear_errmsg();
	      *Status = 0;
	    }
	    else strcpy(Catalog[Models].AltName, ModelName);
	  }
	  if (!*Status)
	  {
	    fits_read_key(ModelUnit, TDOUBLE, "GDFTSLPE",
			  &Catalog[Models].GainDriftSlope, CommBuff, Status); 
	    if (*Status == 202)
	    {
	      fits_clear_errmsg();
	      Catalog[Models].GainDriftSlope = 0.0;
	      *Status = 0;
	    }
	    else 
	    {
	      fits_read_key(ModelUnit, TDOUBLE, "GDFTTZRO", 
			    &Catalog[Models].GainDriftTZero, CommBuff, Status);
	      if (*Status == 202)
	        *Status = PCABAT_URP_BAD_GAIN_SPEC;
	    }
	  }
	  Catalog[Models].Unit = ModelUnit;
	  Catalog[Models].HDU = j + 1;
	  Models += 1;
	  NGoodModels += 1;
	}
      }
      else
      {
	if (*Status == 107)
	{
	  fits_clear_errmsg();
	  *Status = 0;
	  Done = TRUE;
	}
      }
      if (Models == MaxModels)
      {
	Urp(PCABAT_URP_MAXMOD_TOO_SMALL);
	Done = TRUE;
      }
    }

    /* Make history of extension numbers used */
    if (NGoodModels > 0 && Models > ModelStart) {
      int nmod = Models-ModelStart;
      for (j = 0; j<nmod; j++) {
	if ((j % 10) == 0) {
	  strcpy(MsgBuffer, "     Exts ");
	}
	sprintf(MsgBuffer+strlen(MsgBuffer), "%4d,", 
		Catalog[j+ModelStart].HDU);
	if ((j % 10) == 9 || j == nmod-1) {
	  /* Remove trailing comma */
	  MsgBuffer[strlen(MsgBuffer)-1] = 0;
	  AddHistory(MsgBuffer);
	}
      }
    }

    /* Here is a case where the user specified a file with no good model times */
    if (NGoodModels == 0 && BadTime) {
      *Status = PCABAT_URP_BAD_MODEL_TIMES;
    }

  }
    
  Catalog = (struct ModelCat *)realloc(Catalog, sizeof(struct ModelCat) * 
				       (Models + 1));
  Catalog[Models].HDU = 0;

/*  for (i = 0; i < Models; i++)
   printf("%s, %d, %d\n", Catalog[i].Name, Catalog[i].Unit, Catalog[i].HDU); */
  free(ModNames[0]);
  free(ModNames);
  free(ModExt);
  
  return Catalog;
}

int ValidModelTime(fitsfile *ModelUnit, double MJD, int *Status)

/*********************\
*   ValidModelTime    *
*   --------------    *
*    Compares a model's valid time with the supplied MJD to determine if the
*    model is valid for that MJD.
\*********************/

{
  char CommBuff[FLEN_COMMENT], ModDate[FLEN_VALUE], 
       ModTime[FLEN_VALUE];
  double ModMJD = 0.0, Frac = 0.0;

  if (*Status) return FALSE;

  ffgkys(ModelUnit, "CVSD0001", ModDate, CommBuff, Status);
  if (*Status == 202)
  {
    *Status = 0;
    ffcmsg();
  }
  else
  {
    ffgkys(ModelUnit, "CVST0001", ModTime, CommBuff, Status);
    if (*Status == 202)
    {
      *Status = 0;
      ffcmsg();
      strcpy(ModTime, "00:00:00");
    }
    Dt2mjd(ModDate, TRUE, &ModMJD, Status);
    Tim2df(ModTime, TRUE, &Frac, Status);
    ModMJD += Frac;
    if (CompareTimes(ModMJD, MJD, Status))
      return FALSE;
  }
  ffgkys(ModelUnit, "CVED0001", ModDate, CommBuff, Status);
  if (*Status == 202)
  {
    *Status = 0;
    ffcmsg();
    return TRUE;
  }
  ffgkys(ModelUnit, "CVET0001", ModTime, CommBuff, Status);
  if (*Status == 202)
  {
    *Status = 0;
    ffcmsg();
    strcpy(ModTime, "23:59:59");
  }
  Dt2mjd(ModDate, TRUE, &ModMJD, Status);
  Tim2df(ModTime, TRUE, &Frac, Status);
  ModMJD += Frac;
  if (CompareTimes(MJD, ModMJD, Status))
    return FALSE;
  return TRUE;
}

int CompareTimes(double MJD1, double MJD2, int *Status)

/*********************\
*     CompareTimes    *
*     ------------    *
\*********************/

{
  if (*Status) return 0;

  return (MJD2 < MJD1) ? 1 : 0;
}

int *RebinFilter(fitsfile *Std2Unit, fitsfile *FiltUnit, int Std2Rows,
		 int FiltRows, int *Status)

/*********************\
*     RebinFilter     *
*     -----------     *
*      Locates the rows in the filter file which correspond to the times in 
* the standard mode 2 file.  Currently, the program assumes that the rows 
* are on the same time interval as the Standard mode 2 file.
\*********************/

{
  int *Rebin, i, j, Zeros;
  double *StdTime, *FiltTime;

  StdTime = (double *)malloc(sizeof(double) * Std2Rows);
  FiltTime = (double *)malloc(sizeof(double) * FiltRows);
  Rebin = (int *)malloc(sizeof(int) * Std2Rows);

  ffgcvd(Std2Unit, 1, 1, 1, Std2Rows, 0, StdTime, &Zeros, Status);
  ffgcvd(FiltUnit, 1, 1, 1, FiltRows, 0, FiltTime, &Zeros, Status);
  j = 0;
  for (i = 0; i < Std2Rows; i++)
  {
    for(; (FiltTime[j] < StdTime[i]) && (j < FiltRows - 1); j++);
    Rebin[i] = j;
  }
  free(StdTime);
  free(FiltTime);
/*  for (i = 0; i < Std2Rows; i++)
    printf("%d <-- %d\n", i, Rebin[i]); */
  return Rebin;
}

/*********************\
*    CalcInternal     *
*    ------------     *
* Parameters:         *
*    CD - main catalog descriptor
*    Name - name of variable to be computed
*    Param - upon return, will be filled with estimates
*    Special - "special" parameter requested by model
*    Status - The usual FITSIO status variable.
* Function:
*    This is the main dispatch routine for all internally calculated
*    model components.  Based on Catalog->Name, the proper model
*    function is invoked.
\*********************/
int CalcInternal(struct ConvDesc *CD, char *Name,
		 float *Param, double Special, int *Status)

{

  struct ColumnCat Catalog0, *Catalog;

  /* For compatibility with existing code, create a fake Catalog entry
     which has the name of this variable. */
  Catalog = &Catalog0;
  Catalog0 = CD->Catalog[0]; /* Crib from the first Std2 column */
  strcpy(Catalog->Name, Name);
  Catalog->Unit = 0;
  Catalog->Column = -1;

  if (!strcmp("Exp_Decay", Name))
    return CalcInternal_ExpDecay(CD, Catalog, Param, Special, Status);

  if (sscanf(Name, "HighXeCntPcu%d", &Catalog->Column) == 1)       /* HighXe*/
    return CalcInternal_HighXe(CD, Catalog, Param, Special, Status);
  if (sscanf(Name, "VxCntPcu%d", &Catalog->Column) == 1)           /* Vx */
    return CalcInternal_VxRate(CD, Catalog, Param, Special, Status);
  if (sscanf(Name, "Xe%*dLLDCntPcu%d", &Catalog->Column) == 1)     /* 2LLD */
    return CalcInternal_2LLD(CD, Catalog, Param, Special, Status);
  if (sscanf(Name, "VpX%*dCntPcu%d", &Catalog->Column) == 1)       /* VpX */
    return CalcInternal_VpX1(CD, Catalog, Param, Special, Status);
  if (sscanf(Name, "Vx%*dCntPcu%d", &Catalog->Column) == 1)        /* Vx246 */
    return CalcInternal_VxN(CD, Catalog, Param, Special, Status);

  /* 
     Check for special Xenon descriptors:
     X1LCntPcu = layer 1 left
     X1CntPcu  = layer 1 total
     XeCntPcu  = all layer total
     ..._nn_nn_CntPcu = for any of the above, specified channel range (0-129)

     NOTE: Careful to avoid the Xe<n>LLDCntPcu<n> parameters!

  */
  {
    int layers[6] = {0,0,0,0,0,0};
    int chlow = 0, chhigh = 128;
    int l, pos, i;
    char str[FLEN_VALUE];

    /* Parse the parameter name */
    pos = -1;
    if (sscanf(Name,"X%1d%[LR]", &l, str) == 2) {  /* X<n>L and X<n>R */
      i = 2*l + (str[0] == 'L')?(0):(1);
      layers[i] = 1;  /* explicit layer */
      pos = 3;
    } else if (sscanf(Name,"X%1d", &l) == 1) {     /* X<n> */
      i = 2*l;        
      /* left+right layers */
      layers[i] = layers[i+1] = 1; 
      pos = 2;
    } else if (strncmp(Name,"Xe",2) == 0) {        /* Xe */
      /* sum all layers */
      for (i=0;i<5;i++) { layers[i] = 1; } 
      pos = 2;
    }
    if (pos >= 0 && Name[pos] == '_') {            /* _chmin_chmax_ */
      /* Channel boundaries */
      int i, j, delta;
      if (sscanf(Name+pos, "_%d_%d_%n", &i, &j, &delta) == 2) {
	chlow = i; chhigh=j;
	pos += delta;
      }
    }

    if (pos >= 0) {
      /* Finally be sure the "CntPcu<n>" is present */
      if (sscanf(Name+pos, "CntPcu%d", &Catalog->Column) == 1) {
	return CalcInternal_Xenon(CD, Catalog, Param, Special, 
				  layers, chlow, chhigh, Status);
      }
    }
  }

  return -1;
}

/* Structure for caching of internally-calculated variables */
struct InternalCache {
  int Sequence;     /* Unique caching sequence number (used to determine age)*/
  int Cached;       /* Is this cache entry filled?  1=filled; 0=vacant */
  int Rows;         /* Number of rows in cache */
  char Name[FLEN_VALUE]; /* Name of cached model component */
  double Special;   /* The "special" parameter value */
  float *Param;     /* Pointer to cached data */
};

/*
 * CacheRetrieve - Retrieve data from cache
 * 
 * The user supplies a Catalog and Special value. If there is a cached
 * set, it must match Catalog->{Rows,Name} and Special.
 * 
 *   struct InternalCache *Cache - array of cache structures to search
 *   int NCacheEntries - number of elements in Cache
 *   struct ColumnCat *Catalog - background component to search for
 *   double Special - "special" value to search for
 *   float *Param - upon return, the cached data values.
 *
 * Returns: 1 if cached data was found
 *          0 if cached data was not found (and the cache is invalidated)
 *
 * 
 * NOTE: we might worry about what happens when the program asks to
 * 
 *   cache a model component while working on PCU<n> and then, we move
 *   on to PCU<n+1>.  Will the PCU<n> data be used for PCU<n+1>?  The
 *   answer is no, because the internal parameters all have names like
 *   *PcuN, with an explicit PCU number.  When we move on to the
 *   next PCU, the name will change, and the cache name will no longer
 *   match, forcing a recomputation.  
 * 
 *   In the case of the ExpDecay model, the component name "ExpDecay"
 *   does not depend on PCU, and that is OK since the same values can
 *   be used for each PCU.  The ExpDecay model depends on the
 *   "Special" (Lifetime) value so that 24 and 240 minute components
 *   are not confused.
 *
 */
int CacheRetrieve(struct InternalCache *Cache, int NCacheEntries,
		  struct ColumnCat *Catalog, double Special, 
		  float *Param)
{
  int i, j;

  /* printf("    (cache has %s:%d; we want %s:%d)\n", 
	 Cache->Name, Cache->Rows, Catalog->Name, Catalog->Rows); */
  /* Determine if cached data already exists */
  for (j = 0; j < NCacheEntries; j++) {
    if (Cache[j].Cached && Cache[j].Param &&
	Cache[j].Rows == Catalog->Rows &&
	Cache[j].Special == Special && 
	!strcmp(Catalog->Name,Cache[j].Name)) {
      for (i=0; i<Cache[j].Rows; i++) {
	Param[i] = Cache[j].Param[i];
      }
      /* printf("    (cache retrieved %d elements of %s)\n", Cache->Rows, Cache->Name); */
      return 1;
    }
  }

  /* Nope, not cached, be sure to reset the cache status variables */
  /* Cache->Cached = 0; */
  /* Cache->Name[0] = 0; */
  return 0;
}

/*
 * CacheSave - Save data to cache
 *
 * Returns: 0 upon success
 *          non-zero Status value upon failure
 *
 */
int CacheSave(struct InternalCache *Cache, int NCacheEntries,
	      struct ColumnCat *Catalog, double Special, 
	      float *Param)
{
  int i, j;
  int CurSeq;
  static int CacheSequence = 1; /* Monotonic sequence number */

  /* Increment to the next cache sequence number */
  CacheSequence ++;

  /* Find a free cache entry, or if none exist, then evict the oldest
     entry */
  CurSeq = CacheSequence;
  i = -1;
  for (j = 0; j < NCacheEntries; j++) {
    if (Cache[j].Cached == 0) break;
    if (Cache[j].Sequence < CurSeq) i = j;
  }
  /* We didn't find an empty slot, so use the oldest one */
  if (j >= NCacheEntries) {
    j = i;
  }
    

  /* Re-allocate the cache store if necessary */
  if (Cache[j].Rows != Catalog->Rows) {
    Cache[j].Rows = Catalog->Rows;
    if (Cache[j].Param) {
      Cache[j].Param = (float *) realloc(Cache[j].Param,
					 sizeof(Param[0])*Cache[j].Rows);
    } else {
      Cache[j].Param = (float *) malloc(sizeof(Param[0])*Cache[j].Rows);
    }
    if (Cache[j].Param == 0) {
      fits_write_errmsg("Could not allocate CalcInternal cache memory.");
      Cache[j].Rows = 0;
      return PCABAT_URP_NO_MEMORY;
    }
  }

  /* Transfer the data */
  for (i=0; i<Cache[j].Rows; i++) {
    Cache[j].Param[i] = Param[i];
  }
  Cache[j].Special = Special;

  /* Save the name, and flag the cache as ready */
  strcpy(Cache[j].Name, Catalog->Name);
  Cache[j].Cached = 1;
  Cache[j].Sequence = CacheSequence;
  /* printf("    (cache saved %d elements of %s in sequence %d)\n", 
     Cache->Rows, Cache->Name, Cache->Sequence); */

  return 0;
}


char *IntSpecID[6] =
{
  "1L", "1R", "2L", "2R", "3L", "3R"
};

int CalcInternal_HighXe(struct ConvDesc *CD, struct ColumnCat *Catalog,
			 float *Param, double Special, int *Status)

{
  int i, j, k, AnyF = 0, HighEl = 106;
  long SpecEl = 129, FPixels[2], LPixels[2], Incs[2];
  float *SpecData[6], Fudge;
  char MsgBuffer[256], Name[FLEN_VALUE];

  Fudge = 1.0 + Special;
  if ((Catalog->Column >= 0) && (Catalog->Column < 5))
  {
    FPixels[0] = 24;
    FPixels[1] = 1;
    LPixels[0] = 129;
    LPixels[1] = Catalog->Rows;
    Incs[0] = 1;
    Incs[1] = 1;
    
    SpecData[0] = malloc(sizeof(float) * HighEl * Catalog->Rows * 6);
    memset(SpecData[0], 0, sizeof(float) * HighEl * Catalog->Rows * 6);
    for (i = 1; i < 6; i++)
      SpecData[i] = &SpecData[i - 1][HighEl * Catalog->Rows];
    for (i = 0; i < 6; i++)
    {
      sprintf(Name, "X%sSpecPcu%d", IntSpecID[i], Catalog->Column);
      for (j = 0; (j < CD->CatColumns) && (strcmp(Name, CD->Catalog[j].Name));
	   j++);
      if (j == CD->CatColumns)
      {
	sprintf(MsgBuffer, "\"%s\" is not available in input files.", Name);
	fits_write_errmsg(MsgBuffer);
	*Status = PCABAT_URP_NO_PARAM_DATA;
	return *Status;
      }
      fits_read_subset_flt(CD->Catalog[j].Unit, CD->Catalog[j].Column, 1,
			   &SpecEl, FPixels, LPixels, Incs, 0.0, SpecData[i],
			   &AnyF, Status); 
    }
    for (i = 0; i < Catalog->Rows; i++)
    {
      Param[i] = 0.0;
      for (j = 0; j < 6; j++)
	for (k = 0; k < 30; k++)
	  Param[i] += SpecData[j][Catalog->Rows * HighEl + k];
      Param[i] *= Fudge;
    }
    free(SpecData[0]);
  }
  return *Status;
}

/* 
 * CalcInternal_Xenon - Sum GoodXenon data for selected layer/channels
 * 
 */
int CalcInternal_Xenon(struct ConvDesc *CD, struct ColumnCat *Catalog,
			float *Param, double Special, 
			int Layers[6], int ChanLow, int ChanHigh,
			int *Status)

{
  int i, j, k, AnyF = 0;
  long SpecEl = 129, FPixels[2], LPixels[2], Incs[2];
  float *SpecData[6], Fudge;
  char MsgBuffer[256], Name[FLEN_VALUE];
  int NumChan;
  static struct InternalCache Cache[2] = {{0, 0, 0, "", 0},
					  {0, 0, 0, "", 0}};

  /* Return cached data if capable */
  if (CacheRetrieve(Cache, 2, Catalog, Special, Param)) return *Status;
  
  NumChan = ChanHigh - ChanLow + 1;

  Fudge = 1.0 + Special;
  if ((Catalog->Column >= 0) && (Catalog->Column < 5))
  {
    FPixels[0] = ChanLow + 1;
    FPixels[1] = 1;
    LPixels[0] = ChanHigh + 1;
    LPixels[1] = Catalog->Rows;
    Incs[0] = 1;
    Incs[1] = 1;
    
    SpecData[0] = malloc(sizeof(float) * NumChan * Catalog->Rows * 6);
    memset(SpecData[0], 0, sizeof(float) * NumChan * Catalog->Rows * 6);
    for (i = 1; i < 6; i++)
      SpecData[i] = &SpecData[i - 1][NumChan * Catalog->Rows];

    /* SpecData[i][j*NumChan + k] is
         the ith layer
         the jth row in the table
         the kth energy bin in the spectrum
    */

    /* Only read data if necessary (if Layers[i] is set) */
    for (i = 0; i < 6; i++) if (Layers[i]) {
      sprintf(Name, "X%sSpecPcu%d", IntSpecID[i], Catalog->Column);
      for (j = 0; (j < CD->CatColumns) && (strcmp(Name, CD->Catalog[j].Name));
	   j++);
      if (j == CD->CatColumns)
      {
	sprintf(MsgBuffer, "\"%s\" is not available in input files.", Name);
	fits_write_errmsg(MsgBuffer);
	*Status = PCABAT_URP_NO_PARAM_DATA;
	return *Status;
      }
      fits_read_subset_flt(CD->Catalog[j].Unit, CD->Catalog[j].Column, 1,
			   &SpecEl, FPixels, LPixels, Incs, 0.0, SpecData[i],
			   &AnyF, Status); 
    }
    for (i = 0; i < Catalog->Rows; i++)
    {
      Param[i] = 0.0;
      for (j = 0; j < 6; j++) if (Layers[j]) {
	for (k = 0; k < NumChan; k++) {
	  Param[i] += SpecData[j][i * NumChan + k];
	}
      }
      Param[i] *= Fudge;
    }
    free(SpecData[0]);
  }

  /* Store the data in the cache for next time */
  if (*Status == 0) {
    *Status = CacheSave(Cache, 2, Catalog, Special, Param);
  }

  return *Status;
}


char *VxSpecName[3] = 
{
  "VxLCntPcu%d", "VxHCntPcu%d", "VxLVxHCntPcu%d"
};

int CalcInternal_VxRate(struct ConvDesc *CD, struct ColumnCat *Catalog,
			 float *Param, double Special, int *Status)

{
  int i, j, Zeros;
  float *TmpParam;
  char MsgBuffer[256], Name[FLEN_VALUE];

  if ((Catalog->Column >= 0) && (Catalog->Column < 5))
  {
    TmpParam = malloc(sizeof(float) * Catalog->Rows);
    memset(Param, 0, sizeof(float) * Catalog->Rows);
    for (i = 0; i < 3; i++)
    {
      memset(TmpParam, 0, sizeof(float) * Catalog->Rows);
      sprintf(Name, VxSpecName[i], Catalog->Column);
      for (j = 0; (j < CD->CatColumns) && (strcmp(Name, CD->Catalog[j].Name));
	   j++);
      if (j == CD->CatColumns)
      {
	sprintf(MsgBuffer, "\"%s\" is not available in input files.", Name);
	ffpmsg(MsgBuffer);
	*Status = PCABAT_URP_NO_PARAM_DATA;
	return *Status;
      }
      ffgcve(CD->Catalog[j].Unit, CD->Catalog[j].Column, 1, 1, Catalog->Rows,
	     0, TmpParam, &Zeros, Status);
      for (j = 0; j < Catalog->Rows; j++)
        Param[j] += TmpParam[j];
    }
    free(TmpParam);
  }

  return *Status;
}

char *VxNSpecName[6] = 
{
  "VxX3LCntPcu%d", "VxX3RCntPcu%d", 
  "VxX2LCntPcu%d", "VxX2RCntPcu%d", 
  "VxX1LCntPcu%d", "VxX1RCntPcu%d"
};

/* Veto+Xenon doubles rate, summed by layer starting from the bottom.
   Vx2: VxX3L + VxX3R
   Vx4: VxX2L + VxX2R + Vx2
   Vx6: VxX1L + VxX1R + Vx4
*/
int CalcInternal_VxN(struct ConvDesc *CD, struct ColumnCat *Catalog,
			 float *Param, double Special, int *Status)

{
  int i, j, Zeros;
  float *TmpParam;
  char MsgBuffer[256], Name[FLEN_VALUE];
  int vxn = 0;
  static struct InternalCache Cache = {0, 0, 0, "", 0};

  /* Return cached data if capable */
  if (CacheRetrieve(&Cache, 1, Catalog, Special, Param)) return *Status;

  /* Convert VxN to the integer N, which is also conveniently the
     number of values to sum */
  vxn = 4;
  if (Catalog->Name[2] == '2') vxn = 2;
  if (Catalog->Name[6] == '6') vxn = 6;

  if ((Catalog->Column >= 0) && (Catalog->Column < 5))
  {
    TmpParam = malloc(sizeof(float) * Catalog->Rows);
    memset(Param, 0, sizeof(float) * Catalog->Rows);
    for (i = 0; i < vxn; i++)
    {
      memset(TmpParam, 0, sizeof(float) * Catalog->Rows);
      sprintf(Name, VxNSpecName[i], Catalog->Column);
      for (j = 0; (j < CD->CatColumns) && (strcmp(Name, CD->Catalog[j].Name));
	   j++);
      if (j == CD->CatColumns)
      {
	sprintf(MsgBuffer, "\"%s\" is not available in input files.", Name);
	ffpmsg(MsgBuffer);
	*Status = PCABAT_URP_NO_PARAM_DATA;
	return *Status;
      }
      ffgcve(CD->Catalog[j].Unit, CD->Catalog[j].Column, 1, 1, Catalog->Rows,
	     0, TmpParam, &Zeros, Status);
      for (j = 0; j < Catalog->Rows; j++)
        Param[j] += TmpParam[j];
    }
    free(TmpParam);
  }

  /* Store the data in the cache for next time */
  if (*Status == 0) {
    *Status = CacheSave(&Cache, 1, Catalog, Special, Param);
  }

  return *Status;
}


char *VpX1SpecName[2] = 
{
  "VpX1LCntPcu%d", "VpX1RCntPcu%d"
};

int CalcInternal_VpX1(struct ConvDesc *CD, struct ColumnCat *Catalog,
		       float *Param, double Special, int *Status)

{
  int i, j, Zeros;
  float *TmpParam;
  char MsgBuffer[256], Name[FLEN_VALUE];

  if ((Catalog->Column >= 0) && (Catalog->Column < 5))
  {
    TmpParam = malloc(sizeof(float) * Catalog->Rows);
    memset(Param, 0, sizeof(float) * Catalog->Rows);
    for (i = 0; i < 2; i++)
    {
      memset(TmpParam, 0, sizeof(float) * Catalog->Rows);
      sprintf(Name, VpX1SpecName[i], Catalog->Column);
      for (j = 0; (j < CD->CatColumns) && (strcmp(Name, CD->Catalog[j].Name));
	   j++);
      if (j == CD->CatColumns)
      {
	sprintf(MsgBuffer, "\"%s\" is not available in input files.", Name);
	ffpmsg(MsgBuffer);
	*Status = PCABAT_URP_NO_PARAM_DATA;
	return *Status;
      }
      ffgcve(CD->Catalog[j].Unit, CD->Catalog[j].Column, 1, 1, Catalog->Rows,
	     0, TmpParam, &Zeros, Status);
      for (j = 0; j < Catalog->Rows; j++)
        Param[j] += TmpParam[j];
    }
    free(TmpParam);
  }

  return *Status;
}

#define LLDRates 7
#define LLDSpectra 4

char *LLDSpecName[LLDRates] = 
{
  "X1LX1RCntPcu%d", "X2LX2RCntPcu%d", "X3LX3RCntPcu%d", "X1LX2LCntPcu%d",
  "X1RX2RCntPcu%d", "X2LX3LCntPcu%d", "X2RX3RCntPcu%d"
};

int CalcInternal_2LLD(struct ConvDesc *CD, struct ColumnCat *Catalog,
		       float *Param, double Special, int *Status)

{
  int i, j, k, AnyF = 0, Zeros, SpecEl = 23, FirstRate, LowPCU, HighPCU, PCU;
  long SpecBins = 129, FPixels[2], LPixels[2], Incs[2];
  float *TmpParam, *SpecData[LLDSpectra];
  char MsgBuffer[256], Name[FLEN_VALUE];
  static struct InternalCache Cache = {0, 0, 0, "", 0};

  /* Return cached data if capable */
  if (CacheRetrieve(&Cache, 1, Catalog, Special, Param)) return *Status;

  if ((Catalog->Column >= -1) && (Catalog->Column < 5))
  {
    LowPCU = Catalog->Column;
    HighPCU = Catalog->Column + 1;
    if (Catalog->Column == -1)
    {
      LowPCU = 0;
      HighPCU = 3;
    }
    TmpParam = malloc(sizeof(float) * Catalog->Rows);
    memset(Param, 0, sizeof(float) * Catalog->Rows);
    FirstRate = 0;
    if (Catalog->Name[2] == '6')
      FirstRate = 1;
    for (PCU = LowPCU; PCU < HighPCU; PCU++)
    {
      for (i = FirstRate; i < LLDRates; i++)
      {
	memset(TmpParam, 0, sizeof(float) * Catalog->Rows);
	sprintf(Name, LLDSpecName[i], PCU);
	for (j = 0;
	     (j < CD->CatColumns) && (strcmp(Name, CD->Catalog[j].Name)); j++);
	if (j == CD->CatColumns)
        {
	  sprintf(MsgBuffer, "\"%s\" is not available in input files.", Name);
	  ffpmsg(MsgBuffer);
	  *Status = PCABAT_URP_NO_PARAM_DATA;
	  return *Status;
	}
	ffgcve(CD->Catalog[j].Unit, CD->Catalog[j].Column, 1, 1, 
	       Catalog->Rows, 0, TmpParam, &Zeros, Status);
	for (j = 0; j < Catalog->Rows; j++)
	  Param[j] += TmpParam[j];
      }

      if (Catalog->Name[2] == '9')
      {

	/*
	 * For the 9LLD rate, we also add the counts in the layer 2 and 3 
	 * spectra.
	 */
	
	FPixels[0] = 1;
	FPixels[1] = 1;
	LPixels[0] = SpecEl;
	LPixels[1] = Catalog->Rows;
	Incs[0] = 1;
	Incs[1] = 1;
	
	SpecData[0] = malloc(sizeof(float) * SpecEl * Catalog->Rows *
			     LLDSpectra);
	memset(SpecData[0], 0,
	       sizeof(float) * SpecEl * Catalog->Rows * LLDSpectra);
	for (i = 1; i < LLDSpectra; i++)
	  SpecData[i] = &SpecData[i - 1][SpecEl * Catalog->Rows];
	for (i = 0; i < LLDSpectra; i++)
        {
	  sprintf(Name, "X%sSpecPcu%d", IntSpecID[i + 2], PCU);
	  for (j = 0; (j < CD->CatColumns) &&
		 (strcmp(Name, CD->Catalog[j].Name)); j++);
	  if (j == CD->CatColumns)
	  {
	    sprintf(MsgBuffer, "\"%s\" is not available in input files.",
		    Name);
	    ffpmsg(MsgBuffer);
	    *Status = PCABAT_URP_NO_PARAM_DATA;
	    return *Status;
	  }
	  ffgsve(CD->Catalog[j].Unit, CD->Catalog[j].Column, 1, &SpecBins,
		 FPixels, LPixels, Incs, 0.0, SpecData[i], &AnyF, Status); 
	}
	for (i = 0; i < Catalog->Rows; i++)
	{
	  for (j = 0; j < LLDSpectra; j++)
	    for (k = 0; k < SpecEl; k++)
	      Param[i] += SpecData[j][i * SpecEl + k];
	}
	free(SpecData[0]);
      }
    }
    free(TmpParam);
  }

  /* Store the data in the cache for next time */
  if (*Status == 0) {
    *Status = CacheSave(&Cache, 1, Catalog, Special, Param);
  }

  return *Status;
}

int CalcInternal_ExpDecay(struct ConvDesc *CD, struct ColumnCat *Catalog,
			  float *Param, double Lifetime, int *Status)

{
  int i, j, Zeros, Col;
  char MsgBuffer[256], CommBuff[FLEN_COMMENT];
  double *Time, Early, Late, HXTime;
  float HXDose;
  int nfound = 0;
  static char HEXTEPMFile[1024] = "";
  static double *HEXTEPMTime = 0;
  static float  *HEXTEPMDose = 0;
  static long int HEXTEPMRows = 0;
  /* We can cache up to two sets of values, corresponding to up to two
     decay lifetimes. */
  static struct InternalCache Cache[2] = {{0, 0, 0, "", 0}, 
					  {0, 0, 0, "", 0}};

  if (*Status) return *Status;
  if (!CD->HEXTEPMUnit)
  {
    sprintf(MsgBuffer, "SAA History file is required for this model, but was not specified.");
    ffpmsg(MsgBuffer);
    *Status = PCABAT_URP_NEED_HXPMDATA;
    return *Status;
  }

  /* Return cached data if capable, and if the decay lifetime
     matches */
  if (CacheRetrieve(Cache, 2, Catalog, Lifetime, Param)) {
    /* fprintf(stderr, "NOTE: used cached value for %s:%f\n",
       Catalog->Name, Lifetime); */
    return *Status;
  }

  for (Col = 0; (Col < CD->CatColumns) &&
       (strcmp("Time", CD->Catalog[Col].Name)); Col++);
  if (Col == CD->CatColumns)
    {
      sprintf(MsgBuffer, "\"Time\" is not available in input files.");
      ffpmsg(MsgBuffer);
      *Status = PCABAT_URP_NO_PARAM_DATA;
      return *Status;
    }
  Time = (double *)malloc(sizeof(double) * CD->Catalog[Col].Rows);
  ffgcvd(CD->Catalog[Col].Unit, CD->Catalog[Col].Column, 1, 1,
	 CD->Catalog[Col].Rows, 0, Time, &Zeros, Status);
  Early = Time[0] - Lifetime * 10.0;
  Late = Time[CD->Catalog[Col].Rows - 1];

  /* Read the data the first time through */
  if (strcmp(HEXTEPMFile,CD->HEXTEPMFile) != 0) {

    /* Clear out any previous values (if any) */
    if (HEXTEPMTime) free(HEXTEPMTime);
    if (HEXTEPMDose) free(HEXTEPMDose);
    HEXTEPMTime = 0; HEXTEPMDose = 0; HEXTEPMRows = 0;

    /* Initialize the new store */
    strcpy(HEXTEPMFile, CD->HEXTEPMFile);
    ffgkyj(CD->HEXTEPMUnit, "NAXIS2", &(HEXTEPMRows), CommBuff, Status);
    sprintf(MsgBuffer, "      --- reading %ld SAA history entries",
	    HEXTEPMRows);
    Fcecho(MsgBuffer);

    /* Read the new file into memory */
    if (HEXTEPMRows > 0) {
      HEXTEPMTime = malloc((sizeof(double))*(HEXTEPMRows));
      HEXTEPMDose = malloc((sizeof(float))*(HEXTEPMRows));
      if (HEXTEPMTime == 0 || HEXTEPMDose == 0) {
	*Status = MEMORY_ALLOCATION;
	return *Status;
      }
      ffgcvd(CD->HEXTEPMUnit, 1, 1, 1, HEXTEPMRows, 0, HEXTEPMTime, &Zeros, Status);
      ffgcve(CD->HEXTEPMUnit, 2, 1, 1, HEXTEPMRows, 0, HEXTEPMDose, &Zeros, Status);
      if (*Status) return *Status;
    }
  }


  nfound = 0;
  for (i = 0; i < Catalog->Rows; i++)
    Param[i] = 0.0;
  for (i = 0; i < HEXTEPMRows; i++)
  {
    HXTime = HEXTEPMTime[i];
    if ((HXTime >= Early) && (HXTime <= Late))
    {
      HXDose = HEXTEPMDose[i];
      for (j = 0; j < Catalog->Rows; j++)
	if (HXTime < Time[j])
	  Param[j] = Param[j] + HXDose * exp((HXTime - Time[j]) / Lifetime);
      nfound ++;
    }
  }
  free(Time);

  /* Store the data in the cache for next time */
  if (*Status == 0) {
    *Status = CacheSave(Cache, 2, Catalog, Lifetime, Param);
  }

  /* 
     sprintf(MsgBuffer, "      --- found %d HEXTE entries\n", nfound);
     Fcecho(MsgBuffer);
  */

  return *Status;
}

char *DeadRate[29] = 
{
  "VLECntPcu%d", "VpX1LCntPcu%d", "VpX1RCntPcu%d", "VxX1LCntPcu%d",
  "VxX1RCntPcu%d", "VxX2LCntPcu%d", "VxX2RCntPcu%d", "VxX3LCntPcu%d",
  "VxX3RCntPcu%d", "X1LX1RCntPcu%d", "X2LX2RCntPcu%d", "X3LX3RCntPcu%d",
  "Q3VxVpXeCntPcu%d", "Q4VxVpXeCntPcu%d", "Q5VxVpXeCntPcu%d",
  "Q6VxVpXeCntPcu%d", "Q7VxVpXeCntPcu%d", "Q8VxVpXeCntPcu%d", "CALCntPcu%d",
  "VxLCntPcu%d", "VxHCntPcu%d", "VxLVxHCntPcu%d", "VpXe23CntPcu%d",
  "X1LX2LCntPcu%d", "X1RX2RCntPcu%d", "X2LX3LCntPcu%d", "X2RX3RCntPcu%d",
  "VxpXOR2XeCntPcu%d", "ZeroCntPcu%d"
};

#define DeadSpectra 7
char *DeadSpectrum[DeadSpectra] =
{
  "X1LSpecPcu%d", "X1LSpecPcu%d", "X1LSpecPcu%d", "X1LSpecPcu%d", 
  "X1LSpecPcu%d", "X1LSpecPcu%d", "VpSpecPcu%d"
};

void ComputeLiveFraction(struct ConvDesc *CD, int *Status)

/*********************\
* ComputeLiveFraction *
* ------------------- *
* Parameters:         *
*    CD - The description of the current conversion between a Standard 2
*         file and the output.
*    Status - The usual FITSIO status variable.
* Function
*    Computes the fraction of time which each PCU is "live" for each Standard
*    mode 2 data record.  This routine should be called once per file to
*    initialize the global array "LiveFrac".
\*********************/

{
  char Name[40];
  char MsgBuffer[256];
  int i, j, Factor, SpecEl, Zeros, k, PCU;
  unsigned short int *TmpParam, *SpecData;
  unsigned int *Param;

  Fcecho("                    ... Calculating dead-time correction.");
  LiveFrac[0] = malloc(sizeof(float) * CD->InRows * 5);
  memset(LiveFrac[0], 0, sizeof(float) * CD->InRows * 5);
  for (i = 1; i < 5; i++)
    LiveFrac[i] = &LiveFrac[i - 1][CD->InRows];
  Param = malloc(sizeof(int) * CD->InRows);
  TmpParam = malloc(sizeof(short int) * CD->InRows);
  SpecData = malloc(sizeof(short int) * 129 * CD->InRows);

  for (PCU = 0; PCU < 5; PCU++)
  {
    memset(Param, 0, sizeof(int) * CD->InRows);
    for (i = 0; i < 28; i++)
    {
      memset(TmpParam, 0, sizeof(short int) * CD->InRows);
      sprintf(Name, DeadRate[i], PCU);
      for (j = 0; (j < CD->CatColumns) && (strcmp(Name, CD->Catalog[j].Name));
	   j++);
      if (j == CD->CatColumns)
      {
	sprintf(MsgBuffer, "\"%s\" is not available in input files.", Name);
	fits_write_errmsg(MsgBuffer);
	*Status = PCABAT_URP_NO_PARAM_DATA;
	return;
      }
      fits_read_col(CD->Catalog[j].Unit, TUSHORT, CD->Catalog[j].Column, 1, 1,
		    CD->InRows, 0, TmpParam, &Zeros, Status);
      if (i == 0) Factor = 15; else Factor = 1;
      for (j = 0; j < CD->InRows; j++)
        Param[j] += TmpParam[j] * Factor;
    }

    for (i = 0; i < DeadSpectra; i++)
    {
      memset(SpecData, 0, sizeof(short int) * 129 * CD->InRows);
      if (i == 6) SpecEl = 33; else SpecEl = 129;
      sprintf(Name, DeadSpectrum[i], PCU);
      for (j = 0; (j < CD->CatColumns) &&
	   (strcmp(Name, CD->Catalog[j].Name)); j++);
      if (j == CD->CatColumns)
      {
	sprintf(MsgBuffer, "\"%s\" is not available in input files.", Name);
	fits_write_errmsg(MsgBuffer);
	*Status = PCABAT_URP_NO_PARAM_DATA;
	return;
      }
      fits_read_col(CD->Catalog[j].Unit, TUSHORT, CD->Catalog[j].Column, 1, 1,
		    CD->InRows * SpecEl, 0, SpecData, &Zeros, Status);
      for (j = 0; j < CD->InRows; j++)
	for (k = 0; k < SpecEl; k++)
	  Param[j] += SpecData[j * SpecEl + k];
    }
    for (i = 0; i < CD->InRows; i++)
      LiveFrac[PCU][i] = 1.0 - 10.0e-6 * Param[i] / CD->InInterval;
  }
  free(SpecData);
  free(TmpParam);
  free(Param);
}

/* This code is needed by IRAF */

#ifdef vms
#define F77CALL pcabackist
#endif
#ifdef unix
#define F77CALL pcabackist_
#endif

void F77CALL()

{ 
 void PCABackEst();

 PCABackEst();
}

char HistBuff[50][80];
int HistCount = 0;

void AddHistory(char *String)

{
  HistBuff[HistCount][72] = '\0';
  strncpy(HistBuff[HistCount++], String, 72);
}

void DumpHistory(fitsfile *OutUnit, int *Status)

{
  int i;

  if (*Status)
    return;

  for (i = 0; i < HistCount; i++)
    ffphis(OutUnit, HistBuff[i], Status);
}

double GetObsMJD(fitsfile *Std2Unit, int *Status)

{
  double ObsMJD, TStart, Frac;
  char CommBuff[FLEN_COMMENT];

  if (!*Status)
  {
    fits_read_key(Std2Unit, TDOUBLE, "MJDREF", &ObsMJD, CommBuff, Status);
    if (*Status == 202)
    {
      fits_clear_errmsg();
      *Status = 0;
      fits_read_key(Std2Unit, TDOUBLE, "MJDREFI", &ObsMJD, CommBuff, Status);
      fits_read_key(Std2Unit, TDOUBLE, "MJDREFF", &Frac, CommBuff, Status);
      ObsMJD += Frac;
    }
  }
  if (!*Status)
  {
    fits_read_key(Std2Unit, TDOUBLE, "TSTART", &TStart, CommBuff, Status);
    if (*Status == 202)
    {
      fits_clear_errmsg();
      *Status = 0;
      fits_read_key(Std2Unit, TDOUBLE, "TSTARTI", &TStart, CommBuff, Status);
      fits_read_key(Std2Unit, TDOUBLE, "TSTARTF", &Frac, CommBuff, Status);
      TStart += Frac;
    }
  }
  ObsMJD += TStart / 86400.0;
  return ObsMJD;
}
