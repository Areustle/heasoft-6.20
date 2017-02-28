/*************************************************/
/* File:         xstar2table.h                   */
/* Description:  Header file for xstar2table.c   */
/*                                               */
/*************************************************/
#ifndef XSTAR2TABLE_H
#define XSTAR2TABLE_H

#include "fitsio2.h"     /* cfitsio defined constants             */
#include "fitsio.h" 
#include "xstartablelib.h"

/* I believe 1000 is the limiting length of the FTOOL command line but that
   may be wrong.  Check this value if xstar2table is having problems 
   generating XSTAR parameter strings. I'm letting XSTAR2TABLE manipulate 
   it as a MAXPARMLINELENGTH character string internally and will apply the
   test of 1000 characters in the Perl wrapper. */
#define MAXPARMLINELENGTH 2000

/************************** Error Codes *****************************/
#define MEM_ALLOC_FAILURE  600

/*************************** Structures *****************************/
/* Parameter list structure for XSTAR2TABLE */
typedef struct {
  char           xstarSpecFile[FLEN_FILENAME-1];
  char           atable_inFile[FLEN_FILENAME-1];
  char           atable_outFile[FLEN_FILENAME-1];
  char           mtable_File[FLEN_FILENAME-1];
} XSTAR2TABLE_Parms;

/* Structure for storing parameters of a single XSTAR run */
typedef struct {
  /* physical parameters */
  float     physical[NUMPHYSICALPARMS];
  char      *parmName[NUMPHYSICALPARMS];
  int       parmCount;
  /* program control parameters */
  char      spectrumName[FLEN_FILENAME-1];
  char      spectrumFile[FLEN_FILENAME-1];
  int       spectrumUnits; /* 0=energy, 1=photons */
  int       numberOfSteps;
  int       numberOfIterations;
  int       writeSwitch; /* 1=yes, 0=no */
  int       printSwitch; /* 1=yes, 0=no */
  int       stepSizeChoiceSwitch;
  int       numberOfPasses;
  int       constantPressureSwitch; /* 1=yes, 0=no */
  float    emult;
  float    taumax;
  float    xeemin;
  float    critf;
  float    vturbi;
  float    radexp;
  int      ncn2;
  char      modelName[FLEN_FILENAME-1];    
  int       loopcontrol;
} XSTAR_Parms;

/* Structure for storing spectra of a single XSTAR run */
typedef struct {
  long  nBins;
  float *energy;
  float *incident;
  float *transmitted;
  float *emit_inward;
  float *emit_outward;
} XSTAR_Spec;


/* Function prototypes */
void xstar2table(void);
int  Get_XSTAR2TABLE_Parms(XSTAR2TABLE_Parms*,int);
int  SliceEnergySpectra(float,float,XSTAR_Spec*,long*,long*,long*);

/* Manipulation of XSTAR spectral output file */
int  Read_XSTAR_Parmlist(fitsfile*, XSTAR_Parms*, int);
float GetPhysicalParameter(XSTAR_Parms*, char*);
int  GetInterParmIndex(char*, XSTAR_Parms*);
int  Read_XSTAR_Spectra(fitsfile*, XSTAR_Spec*, int);
int  ParseInteger(char*, int, char**,float*,char**);
float ParseFloat(char*,int, char**,float*,char**);
/* char* ParseString(char*,int,char**,char**,char**);*/
#endif
