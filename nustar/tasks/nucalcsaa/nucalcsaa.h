/*
 *	nucalcsaa.h: definitions and declarations for nucalcsaa
 *
 *	AUTHOR:
 *            ASDC - ASI Science Data Center
 *  
 *     
 */
#ifndef NUCALCSAA_H
#define NUCALCSAA_H

/******************************
 *        header files        *
 ******************************/
#include <stdio.h>	/* Note: for IRAF compatibility,
			standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h> /* for getpid */
#include <limits.h>
#include <float.h>

#include <sys/stat.h>   /* needed by 'mkdir' */
#include <sys/types.h>  /* needed by 'mkdir' */

/* headas headers */
#include <fitsio.h>	 /*Required to use c_fcerr, c_fcecho, etc. */
#include <pil.h>
#include <headas_gti.h>

/* nustar local headers */
#include "nu_highfits.h"
#include "nu_termio.h"
#include "nu_defs.h"
#include "nu_misc.h"
#include "nu_caldb.h"
#include "nustardasversion.h"


#ifndef errno
extern int errno;
#endif

/*******************************
*      defines / typedefs      *
********************************/

/* input parameter names */
#define PAR_HKFILE			"hkfile"
#define PAR_ORBITFILE			"orbitfile"
#define PAR_EVTFILE			"evtfile"
#define PAR_OUTFILE			"outfile"
#define PAR_SAAPARFILE			"saaparfile"
#define PAR_SAACALC			"saacalc"
#define PAR_SAAMODE			"saamode"
#define PAR_TENTACLE			"tentacle"
#define PAR_EVTEXPR			"evtexpr"

/* new input parameter */
#define PAR_OPTIMIZEDRMS		"optimizedrms"
#define PAR_TENTACLERMS			"tentaclerms"
#define PAR_ELIMINATESOURCE		"eliminatesource"
#define PAR_SOURCETHR			"sourcethr"
#define PAR_TENTACLEREGCUT		"tentacleregcut"    
#define PAR_OPTIMIZEDSANITYCHECK	"optimizedsanitychecks"    
#define PAR_TENTACLESANITYCHECK		"tentaclesanitychecks"    

/* miscellaneous */
#define BINTAB_ROWS		1000
#define LOWER_BOUND_NOT_FOUND	-1
#define N_POSITION_BIN		65
#define LEN_STEMOUT		13
#define SAACALC_NEW		2
#define SAACALC_OLD		1

/* Definitions of saaparfile columns */

/* SAA Ext */
#define CLNM_S_HalfAverageLength                 "HalfAverageLength"
#define CLNM_S_DecisionDistanceFactor            "DecisionDistanceFactor"
#define CLNM_S_DecisionThrLeft                   "DecisionThrLeft"
#define CLNM_S_DecisionThrRightFactor            "DecisionThrRightFactor"
#define CLNM_S_SAARampingTime                    "SAARampingTime"
#define CLNM_S_MinLong                           "MinLong"
#define CLNM_S_MaxLong                           "MaxLong"
#define CLNM_S_CutOffCountFactor                 "CutOffCountFactor"
#define CLNM_S_FltEvtHalfAverageLarge            "FltEvtHalfAverageLarge"
#define CLNM_S_FltEvtHalfAverageSmall            "FltEvtHalfAverageSmall"
#define CLNM_S_FltDecisionThrLeft                "FltDecisionThrLeft"
#define CLNM_S_FltDecisionThrRight               "FltDecisionThrRight"
#define CLNM_S_SingleRateDecisionThr             "SingleRateDecisionThr"
#define CLNM_S_FltEvtSingleRateSigma             "FltEvtSingleRateSigma"
/* TENTACLE Ext */
#define CLNM_T_FltEvtHalfAverageLarge            "FltEvtHalfAverageLarge"
#define CLNM_T_FltEvtHalfAverageSmall            "FltEvtHalfAverageSmall"
#define CLNM_T_DecisionDistanceFactor            "DecisionDistanceFactor"
#define CLNM_T_DecisionThrLeft                   "DecisionThrLeft"
#define CLNM_T_DecisionThrRightFactor            "DecisionThrRightFactor"
#define CLNM_T_LongitudeMin                      "LongitudeMin"
#define CLNM_T_LongitudeMax                      "LongitudeMax"
#define CLNM_T_AboveGlobalAverageThr             "AboveGlobalAverageThr"
#define CLNM_T_CutOffCountFactor                 "CutOffCountFactor"


/* Definitions of saaparfile v002 columns */
/* SAANEW Ext */
#define CLNM_SN_SuperStrictOffTimeInterval			"SuperStrictOffTimeBin"
#define CLNM_SN_Prior						"Prior"                    
#define CLNM_SN_SmoothSHLDLO					"SmoothSHLDLO"           
#define CLNM_SN_SmoothSOURCE					"SmoothSOURCE"            
#define CLNM_SN_MinAllowedVolatility				"MinAllowedVolatility"   
#define CLNM_SN_MinMaximum					"MinMaximum"              
#define CLNM_SN_MaxMinimum					"MaxMinimum"          
#define CLNM_SN_MinVariationLeft				"MinVariationLeft"         
#define CLNM_SN_MinVariationRight				"MinVariationRight"     
#define CLNM_SN_NonSAAMinLongOpt				"NonSAAMinLongOpt"     
#define CLNM_SN_NonSAAMaxLongOpt				"NonSAAMaxLongOpt"     
#define CLNM_SN_SpectrumMin					"SpectrumMin"        
#define CLNM_SN_SpectrumMax					"SpectrumMax"         
#define CLNM_SN_TimeOneSigma					"TimeOneSigma"         
#define CLNM_SN_MaxSearchDistance				"MaxSearchDistance"    
#define CLNM_SN_HintOfVariabilityCutOff				"HintOfVariabilityCutOff"
#define CLNM_SN_LogBinningBins					"LogBinningBins"    
#define CLNM_SN_PercentageEliminatedPixels			"PercentEliminatedPixels"
#define CLNM_SN_SuspiciousnessThreshold				"SuspiciousnessThreshold"
/* TENTACLENEW Ext */
#define CLNM_TN_TentacleTimeInterval				"TentacleTimeInterval"
#define CLNM_TN_TentacleNonSAAMinLong				"TentacleNonSAAMinLong"
#define CLNM_TN_TentacleNonSAAMaxLong				"TentacleNonSAAMaxLong"                  
#define CLNM_TN_TentacleLongitudeMin				"TentacleLongitudeMin"                  
#define CLNM_TN_TentacleLongitudeMax				"TentacleLongitudeMax"                    
#define CLNM_TN_TentacleMinLatitude					"TentacleMinLatitude"                     
#define CLNM_TN_TentacleAvgLongitude				"TentacleAvgLongitude"                    
#define CLNM_TN_TentacleVsRegionCrossingsBadCutoff		"TentacleRegCrossBadCut"      
#define CLNM_TN_TentacleVsRegionCrossingsReallyBadCutoff	"TentacleRegCrossBadCut2"
#define CLNM_TN_TentacleDurationCutOff				"TentacleDurationCutOff"                  
#define CLNM_TN_TentacleAdjacentOrNotCutOff			"TentacleAdjacentOrNotCut"
#define CLNM_TN_TentacleLongDuration				"TentacleLongDuration"
#define CLNM_TN_TentacleLongThreshold				"TentacleLongThreshold"
#define CLNM_TN_TentacleFluxThreshold				"TentacleFluxThreshold"
#define CLNM_TN_TentacleSuspiciousnessThreshold			"TentacleSuspiciousThr"

/* Orbit Column Name */
#define CLNM_SAA_A                 "SAA_A"
#define CLNM_SAA_B                 "SAA_B"

/* End -> Definitions of saaparfile columns */


typedef struct {
  unsigned TIME;
  unsigned SHLDLO;
  unsigned SHLDHI;
  unsigned NACCEPT;
  unsigned NREJECT;
  unsigned LIVETIME;
} HKCol_t;

typedef struct {
  DTYPE    Time;
  JTYPE    ShieldRateLow;
  JTYPE    ShieldRateHigh;
  ITYPE    NAcceptedEvents;
  ITYPE    NRejectedEvents;
  DTYPE    LiveTime;
} HKRow_t;


typedef struct {
  unsigned TIME;
  unsigned SAA;
  unsigned SAA_A;
  unsigned SAA_B;
  unsigned OCCULTED;
/*   unsigned DAY; */
  unsigned SLEW;
/*   unsigned GEOCOR; */
  unsigned GEODETIC;
} OrbitCol_t;

typedef struct
{
    /*! The bin edges*/
    double *m_BinEdges;
    unsigned int m_BinEdges_size;
    /*! The bin content*/
    double *m_BinnedData;
    unsigned int m_BinnedData_size;
    
} Baesian_t;

typedef struct {
  DTYPE    Time;
  BTYPE    SAAFlag;
  BTYPE    SAA_A;
  BTYPE    SAA_B;
  BTYPE    SAAFlagTentacle;
  BTYPE    occulted;
  BTYPE    slew;
  ETYPE    Latitude;
  ETYPE    Longitude;
} OrbitRow_t;

typedef struct {
  unsigned TIME;
  unsigned PI;
  unsigned DET_ID;
  unsigned RAWX;
  unsigned RAWY;
  /* Test MostlyEliminateSource */
  unsigned DET1X;
  unsigned DET1Y;
  unsigned X;
  unsigned Y;

} EvtCol_t;

typedef struct 
{
  DTYPE    Time;  
  DTYPE    energy;
  BTYPE    DetectorID;
  BTYPE    RawX;
  BTYPE    RawY;
  int      Det1X;
  int      Det1Y;
  int      X;
  int      Y;
} EVTRow_t;


typedef struct {
  char instrume[FLEN_VALUE];
  SPTYPE dateobs;
  SPTYPE timeobs;
  SPTYPE dateend;
  SPTYPE timeend;
} ObsInfo_t;


typedef struct {
  /* SAA Ext */
  unsigned S_HalfAverageLength;
  unsigned S_DecisionDistanceFactor;
  unsigned S_DecisionThrLeft;
  unsigned S_DecisionThrRightFactor;
  unsigned S_SAARampingTime;
  unsigned S_MinLong;
  unsigned S_MaxLong;
  unsigned S_CutOffCountFactor;
  unsigned S_FltEvtHalfAverageLarge;
  unsigned S_FltEvtHalfAverageSmall;
  unsigned S_FltDecisionThrLeft;
  unsigned S_FltDecisionThrRight;
  unsigned S_SingleRateDecisionThr;
  unsigned S_FltEvtSingleRateSigma;
  /* TENTACLE Ext */
  unsigned T_FltEvtHalfAverageLarge;
  unsigned T_FltEvtHalfAverageSmall;
  unsigned T_DecisionDistanceFactor;
  unsigned T_DecisionThrLeft;
  unsigned T_DecisionThrRightFactor;
  unsigned T_LongitudeMin;
  unsigned T_LongitudeMax;
  unsigned T_AboveGlobalAverageThr;
  unsigned T_CutOffCountFactor;
  
  /* SAANEW Ext */
  unsigned SN_SuperStrictOffTimeInterval;
  unsigned SN_Prior;
  unsigned SN_SmoothSHLDLO;
  unsigned SN_SmoothSOURCE;
  unsigned SN_MinAllowedVolatility;
  unsigned SN_MinMaximum;
  unsigned SN_MaxMinimum;
  unsigned SN_MinVariationLeft;
  unsigned SN_MinVariationRight;
  unsigned SN_NonSAAMinLongOpt;
  unsigned SN_NonSAAMaxLongOpt;
  unsigned SN_SpectrumMin;
  unsigned SN_SpectrumMax;
  unsigned SN_TimeOneSigma;
  unsigned SN_MaxSearchDistance;
  unsigned SN_HintOfVariabilityCutOff;
  unsigned SN_LogBinningBins;
  unsigned SN_PercentageEliminatedPixels;
  unsigned SN_SuspiciousnessThreshold;
  /* TENTACLENEW Ext */
  unsigned TN_TentacleTimeInterval;
  unsigned TN_TentacleNonSAAMinLong;
  unsigned TN_TentacleNonSAAMaxLong;
  unsigned TN_TentacleLongitudeMin;
  unsigned TN_TentacleLongitudeMax;
  unsigned TN_TentacleMinLatitude;
  unsigned TN_TentacleAvgLongitude;
  unsigned TN_TentacleVsRegionCrossingsBadCutoff;
  unsigned TN_TentacleVsRegionCrossingsReallyBadCutoff;
  unsigned TN_TentacleDurationCutOff;
  unsigned TN_TentacleAdjacentOrNotCutOff;
  unsigned TN_TentacleLongDuration;
  unsigned TN_TentacleLongThreshold;
  unsigned TN_TentacleFluxThreshold;
  unsigned TN_TentacleSuspiciousnessThreshold;

} SAAParCol_t;

typedef struct {
  /* SAA Ext */
  JTYPE S_HalfAverageLength;
  JTYPE S_DecisionDistanceFactor;
  DTYPE S_DecisionThrLeft;
  DTYPE S_DecisionThrRightFactor;
  JTYPE S_SAARampingTime;
  DTYPE S_MinLong;
  DTYPE S_MaxLong;
  DTYPE S_CutOffCountFactor;
  JTYPE S_FltEvtHalfAverageLarge;
  JTYPE S_FltEvtHalfAverageSmall;
  DTYPE S_FltDecisionThrLeft;
  DTYPE S_FltDecisionThrRight;
  DTYPE S_SingleRateDecisionThr;
  DTYPE S_FltEvtSingleRateSigma;
  /* TENTACLE Ext */
  JTYPE T_FltEvtHalfAverageLarge;
  JTYPE T_FltEvtHalfAverageSmall;
  JTYPE T_DecisionDistanceFactor;
  DTYPE T_DecisionThrLeft;
  DTYPE T_DecisionThrRightFactor;
  DTYPE T_LongitudeMin;
  DTYPE T_LongitudeMax;
  DTYPE T_AboveGlobalAverageThr;
  JTYPE T_CutOffCountFactor;
  
  /* SAANEW Ext */
  JTYPE SN_SuperStrictOffTimeInterval;
  JTYPE SN_Prior;
  JTYPE SN_SmoothSHLDLO;
  JTYPE SN_SmoothSOURCE;
  DTYPE SN_MinAllowedVolatility;
  DTYPE SN_MinMaximum;
  DTYPE SN_MaxMinimum;
  DTYPE SN_MinVariationLeft;
  DTYPE SN_MinVariationRight;
  DTYPE SN_NonSAAMinLongOpt;
  DTYPE SN_NonSAAMaxLongOpt;
  DTYPE SN_SpectrumMin;
  DTYPE SN_SpectrumMax;
  DTYPE SN_TimeOneSigma;
  JTYPE SN_MaxSearchDistance;
  DTYPE SN_HintOfVariabilityCutOff;
  JTYPE SN_LogBinningBins;
  DTYPE SN_PercentageEliminatedPixels;
  JTYPE SN_SuspiciousnessThreshold;
  /* TENTACLENEW Ext */
  JTYPE TN_TentacleTimeInterval;
  DTYPE TN_TentacleNonSAAMinLong;
  DTYPE TN_TentacleNonSAAMaxLong;
  DTYPE TN_TentacleLongitudeMin;
  DTYPE TN_TentacleLongitudeMax;
  DTYPE TN_TentacleMinLatitude;
  DTYPE TN_TentacleAvgLongitude;
  DTYPE TN_TentacleVsRegionCrossingsBadCutoff;
  DTYPE TN_TentacleVsRegionCrossingsReallyBadCutoff;
  DTYPE TN_TentacleDurationCutOff;
  DTYPE TN_TentacleAdjacentOrNotCutOff;  
  DTYPE TN_TentacleLongDuration;
  DTYPE TN_TentacleLongThreshold;
  DTYPE TN_TentacleFluxThreshold;
  JTYPE TN_TentacleSuspiciousnessThreshold;
} SAAParInfo_t;

typedef struct
{
    double *LogBinning;
    double *PositionRates;
    double *delta;
    int LogBinningBins;
    int RealLogBinningBins;
} mostly_t;


typedef struct {
  char        dirname[PIL_LINESIZE];
  char        fsel_infile[PIL_LINESIZE];
  char        fsel_outfile[PIL_LINESIZE];
  char        loc_evtfile[PIL_LINESIZE];
  char        loc_outfile[PIL_LINESIZE];
  } TmpFiles_t;

typedef struct {
    /*----------   CallDB Parameters ---------------------*/
    unsigned int SuperStrictOffTimeInterval;    
    int Prior;
    int SmoothSHLDLO;
    int SmoothSOURCE;
    double MinAllowedVolatility;
    double MinMaximum;
    double MaxMinimum;
    double MinVariationLeft;
    double MinVariationRight;
    double NonSAAMinLongOpt;
    double NonSAAMaxLongOpt;
    double SpectrumMin;
    double SpectrumMax;
    double TimeOneSigma;
    int MaxSearchDistance;
    double HintOfVariabilityCutOff;
    int LogBinningBins;
    double PercentageEliminatedPixels;
    int SuspiciousnessThreshold;
    
    unsigned int TentacleTimeInterval;    
    double TentacleNonSAAMinLong;
    double TentacleNonSAAMaxLong;
    double TentacleLongitudeMin;
    double TentacleLongitudeMax;
    double TentacleMinLatitude;
    double TentacleAvgLongitude;
    double TentacleVsRegionCrossingsBadCutoff;
    double TentacleVsRegionCrossingsReallyBadCutoff;
    double TentacleDurationCutOff;
    double TentacleAdjacentOrNotCutOff;
    double TentacleLongDuration;
    double TentacleLongThreshold;
    double TentacleFluxThreshold;
    int TentacleSuspiciousnessThreshold;
} GlobalCallDB_t;

typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        hkfile[PIL_LINESIZE];       /* Name of input Housekeeping FITSfile  */
    char        orbitfile[PIL_LINESIZE];    /* Name of input Orbit FITS file  */
    char        evtfile[PIL_LINESIZE];      /* Name of input Event FITS file  */
    char        outfile[PIL_LINESIZE];      /* Name of output Housekeeping FITS file */
    char        saaparfile[PIL_LINESIZE];
    int         saacalc;
    char        saamode[PIL_LINESIZE];
    char        evtexpr[PIL_LINESIZE];
    BOOL        tentacle;
    
    /* New Parameter */
    double 	MaxAllowedRMS;
    double	TentacleCutRMSThreshold;
    BOOL 	ElimiateSource;
    double 	sourcethr;
    BOOL	TentacleCutRMSRegionRestriction;
    BOOL 	SanityChecks;
    BOOL	TentacleCutRMSSanityChecks;
  } par;


/*----------------------------------------------------------------*/
  char taskname[FLEN_FILENAME];    /* Name and version of the task */
  Version_t nustardas_v;           /* NuSTARDAS version */
  BOOL hist;
  BOOL warning;
  char strhist[256];
  char date[25];

  ObsInfo_t     obsinfo;
  SAAParInfo_t  saaparinfo;
  TmpFiles_t    tmp;

} Global_t;

extern Global_t global;


/* Prototypes */
int nucalcsaa();
int nucalcsaa_work(void);
int nucalcsaa_getpar(void);
void nucalcsaa_info(void);
int nucalcsaa_checkinput(void);

int ReadHouseKeeping(char *filename, HKRow_t **hkinfo, int *hknrows);
int ReadEvent(char *filename, EVTRow_t **evtinfo, int *evtnrows);
int ReadOrbit(char *filename, OrbitRow_t **info, int *nrows);

int FindSAAs(HKRow_t *hkinfo, int hknrows, double *FilteredRate, OrbitRow_t *orbitinfo, int orbitnrows, struct gti_struct *gti, BTYPE *SoftSAA);
int FindSAATentacle(HKRow_t *hkinfo, int hknrows, double *FilteredRate, OrbitRow_t *orbitinfo, int orbitnrows, struct gti_struct *gti, BTYPE *SoftSAA, BTYPE *SoftTentacled);


int ComputeFilteredRate(char *filename, HKRow_t *hkinfo, int hknrows, double **rate);

void FindClosestOrbitIndex(OrbitRow_t *info, int nrows, double time, int *index);
int FindHkIndex(HKRow_t *hkinfo, int hknrows, double time, int *index);

int IsGTI(struct gti_struct  *gti, double time);

int WriteOutFile(char *infile, char *outfile, int nrows, BTYPE *SoftSAA, BTYPE *SoftTentacled);
int UpdateHK1FPMExt(FitsFileUnit_t inunit, FitsFileUnit_t ounit, int nrows, BTYPE *SoftSAA, BTYPE *SoftTentacled);

int GetObsInfo(char *filename, char *extname, ObsInfo_t *obsinfo);
int ReadSAAParInfo(char *filename, SAAParInfo_t *saaparinfo);

/* NEW */

/* For debuging */
void TestMostlyEliminateSource(EVTRow_t *evtinfo, int evtnrows,struct gti_struct *gti,int *ExcludedDetRawXY, int ExcludedDetRawXYrows);
void ShowCCD(int PositionsOnSource[N_POSITION_BIN][N_POSITION_BIN]);
void writeSAA(char *nameFile,HKRow_t *hkinfo,BTYPE *SoftSAA,int SoftSAA_size);
void writeVetD(char *nome_vett,DTYPE *vett,int vett_size);
void writeVetB(char *nome_vett,BTYPE *vett,int vett_size);
void writeVetJ(char *nome_vett,JTYPE *vett,int vett_size);
void writeVetI(char *nome_vett,int *vett,int vett_size);


int CopyCallDBParam(GlobalCallDB_t *globalCallDB);
void PrintCallDBParam(GlobalCallDB_t *globalCallDB);
void Mycopy(int vet_size,double *vet_sorg,double *vet_dest);
double MyMedian(int n,double *valori);
double MyMinElement(int vet_size,double *vet_sorg);
double MyMax(double a, double b);

/*
    Smooth array xx, translation of Hbook routine hsmoof.F
    based on algorithm 353QH twice presented by J. Friedman
    in Proc.of the 1974 CERN School of Computing, Norway, 11-24 August, 1974.
*/
void  MySmoothArray(int nn, double *xx, int ntimes);
int GetNormalized(Baesian_t *baesian,DTYPE **vett_dest, unsigned int *vett_dest_size);
int MyBayesianBlocks(int m_Minimum,int m_Maximum,int interval,double prior, DTYPE *vett_sorg,unsigned int vett_sorg_size,Baesian_t *baesian);
void MyFill(int val,mostly_t * ms);
int MyCount(double *vet_sorg,int vet_size);
int GetPeakHighEdge(Baesian_t *baesian,int bin);
int GetPeakLowEdge(Baesian_t *baesian,int bin);

/* Compute lower_bound on array */
int MostlyEliminateSource(EVTRow_t *evtinfo, int evtnrows,OrbitRow_t *orbitinfo, int orbitnrows,struct gti_struct *gti,int **ExcludedDetRawXY, int *nEliminateSource,BOOL Show);
int MyLowerBound(int *vet,int vet_size,int lb);
void MySort(int *vet,int vet_size);
int MyIntegral(BTYPE *vet,int vet_size);
int MyIntegralD(DTYPE *vet,int vet_size);

void ConvertRawPos(int RawX, int RawY, int DetectorID, int *PosX, int *PosY);
void ConvertPosRaw(int PosX, int PosY, int *DetectorID, int *RawX, int *RawY);

DTYPE GetMinTime(HKRow_t *hkinfo,int hknrows);
DTYPE GetMaxTime(HKRow_t *hkinfo,int hknrows);
DTYPE GetMinTimeEvt(EVTRow_t *evtinfo,int evtnrows);
DTYPE GetMaxTimeEvt(EVTRow_t *evtinfo,int evtnrows);
DTYPE GetMax(DTYPE *vett,int size_vett);
int GetIndexMax(double *vett,int size_vett,int index_start);
double GetPositionMax(mostly_t * ms);

int WithinSpecialGTI(DTYPE in);
int FindClosestIndex(int m_LastClostestIndex,DTYPE Time,HKRow_t *hkinfo,int hknrows);
/* AZ */
unsigned int GetIndex(double MinTime, double MaxTime, unsigned int NBins, double TestTime);

int FindSAAs_new(HKRow_t *hkinfo, int hknrows, EVTRow_t *evtinfo, int evtnrows, double *FilteredRate, OrbitRow_t *orbitinfo, int orbitnrows, struct gti_struct *gti, BTYPE *SoftSAA);
int FindSAATentacle_new(HKRow_t *hkinfo, int hknrows, EVTRow_t *evtinfo, int evtnrows,double *FilteredRate, OrbitRow_t *orbitinfo, int orbitnrows, struct gti_struct *gti, BTYPE *SoftSAA, BTYPE *SoftTentacled);

double GetRMS_ROOT(BTYPE *vett,int nBins,int minVal,int maxVal);
double GetMean_ROOT(BTYPE *vett,int minVal,int maxVal);

double GetMean(DTYPE *vett,int minVal,int maxVal);
double GetRMS(DTYPE *vett,int minVal,int maxVal);

void subString (const char* input, int offset, int len, char* dest);
void checkFMP(const char* input,char* dest);


#endif
