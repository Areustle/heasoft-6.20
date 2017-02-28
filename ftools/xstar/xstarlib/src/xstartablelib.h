/*************************************************/
/* File:         xstartablelib.h                 */
/* Description:  Header file for xstartablelib.c */
/*                                               */
/*************************************************/

#ifndef XSTARTABLELIB_H
#define XSTARTABLELIB_H

#include "fitsio2.h"     /* cfitsio defined constants             */
#include "fitsio.h" 

/* define parameter variation type */
typedef enum {kConstant,kAdditive,kInterpolated} var_type;
/* define interpolation method type */
typedef enum {kLinear,kLogarithmic} method_type;

/* implimented in this lame fashion because ANSI C does not yet fully support
   type enforcement */
#define NUMPHYSICALPARMS 38

/****************** Error Codes & Messages ************************/
#define SPECTRA_WRITE_ERROR 601

/* Parameter list structure for physical input parameters */
typedef struct {
   char          name[FLEN_FILENAME-1];
   var_type      type;   /* 0=constant, 1=additive, 2=interpolated */
   method_type   method; /* 0=linear, 1=logarithmic */
   float         initialValue;
   float         delta;
   float         hardMinimum;
   float         softMinimum;
   float         softMaximum;
   float         hardMaximum;
   int           numberOfValues;
   float         *valueList;
   int           XSTARsequenceNumber;
} Parameter_rec;

/* Structure to include modeling control parameters */
typedef struct {
  char     spectrumName[FLEN_FILENAME-1];
  char     spectrumFile[FLEN_FILENAME-1];
  int      spectrumUnits; /* 0=energy, 1=photons */
  int      numberOfSteps;
  int      numberOfIterations;
  int      writeSwitch; /* 1=yes, 0=no */
  int      printSwitch; /* 1=yes, 0=no */
  int      stepSizeChoiceSwitch;
  int      numberOfPasses;
  int      constantPressureSwitch; /* 1=yes, 0=no */
  char     modelName[FLEN_FILENAME-1];
  int      numberOfParameters;
  int      redshift; /* Is redshift a paramter?  0=no, 1=yes */
  float    emult;
  float    taumax;
  float    xeemin;
  float    critf;
  float    vturbi;
  float    radexp;
  int      ncn2;
  float    energyLow, energyHigh;
  Parameter_rec  physical[NUMPHYSICALPARMS]; /* array of physical parameters */
} Parameter_struct;

/* Manipulation of ATABLE/MTABLE components */
int  Display_FITS_ParmTable(Parameter_struct*);

int  Read_FITS_ParmTable(fitsfile*, Parameter_struct*, int);

int  Write_FITS_Energies(fitsfile*, long, float*, float*, int);
int  Create_FITS_Spectra_Header(fitsfile*, long, long, long, long, char*, int);
int  Write_FITS_Spectra(fitsfile*, long, long, long, long, float*, int, float*, int);

#endif
