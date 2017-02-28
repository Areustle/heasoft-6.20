/* Header file for heasp routines
   Defines structures for RMF, RMFchain, ARF, PHA, BinFactors
   Includes prototypes for functions */

#include "fitsio.h"

/* define the RMF structure */

struct RMF {

  long NumberChannels;                            /* Number of spectrum channels */
  long NumberEnergyBins;                          /* Number of response energies */
  long NumberTotalGroups;                         /* Total number of response groups */
  long NumberTotalElements;                       /* Total number of response elements */
  long FirstChannel;                              /* First channel number */
  long isOrder;                                   /* If true grating order information included */

  long* NumberGroups; /*NumberEnergyBins*/        /* Number of response groups for this energy bin */
  long* FirstGroup;  /*NumberEnergyBins*/         /* First response group for this energy bin (counts from 0)*/

  long* FirstChannelGroup; /*NumberTotalGroups*/  /* First channel number in this group */
  long* NumberChannelGroups; /*NumberTotalGroups*//* Number of channels in this group */
  long* FirstElement; /*NumberTotalGroups*/       /* First response element for this group (counts from 0)*/
  long* OrderGroup; /*NumberTotalGroups*/         /* The grating order of this group */

  float* LowEnergy; /*NumberEnergyBins*/         /* Start energy of bin */
  float* HighEnergy; /*NumberEnergyBins*/        /* End energy of bin */

  float* Matrix; /*NumberTotalElements*/         /* Matrix elements */

  float* ChannelLowEnergy; /*NumberChannels*/    /* Start energy of channel */
  float* ChannelHighEnergy; /*NumberChannels*/   /* End energy of channel */

  float AreaScaling;                             /* Value of EFFAREA keyword */
  float ResponseThreshold;                       /* Minimum value in response */

  char EnergyUnits[FLEN_KEYWORD];                /* Units for energies - added to match active HEASP */
  char RMFUnits[FLEN_KEYWORD];                   /* Units for RMF  - added ti natcg actuve HEASP */


  char ChannelType[FLEN_KEYWORD];                /* Value of CHANTYPE keyword */
  char RMFVersion[FLEN_KEYWORD];                 /* MATRIX extension format version */
  char EBDVersion[FLEN_KEYWORD];                 /* EBOUNDS extension format version */
  char Telescope[FLEN_KEYWORD];                             
  char Instrument[FLEN_KEYWORD];
  char Detector[FLEN_KEYWORD];
  char Filter[FLEN_KEYWORD];
  char RMFType[FLEN_KEYWORD];                    /* Value of HDUCLAS3 keyword in MATRIX extension */
  char RMFExtensionName[FLEN_VALUE];             /* Value of EXTNAME keyword in MATRIX extension */
  char EBDExtensionName[FLEN_VALUE];             /* Value of EXTNAME keyword in EBOUNDS extension */

};



/* define the ARF structure */

struct ARF {

  long NumberEnergyBins;                         /* Number of response energies */

  float* LowEnergy; /*NumberEnergyBins*/         /* Start energy of bin */
  float* HighEnergy; /*NumberEnergyBins*/        /* End energy of bin */

  float* EffArea;    /*NumberEnergyBins*/        /* Effective areas */

  char ARFVersion[FLEN_KEYWORD];                 /* SPECRESP extension format version */
  char Telescope[FLEN_KEYWORD];                             
  char Instrument[FLEN_KEYWORD];
  char Detector[FLEN_KEYWORD];
  char Filter[FLEN_KEYWORD];
  char ARFExtensionName[FLEN_VALUE];             /* Value of EXTNAME keyword in SPECRESP extension */

};

/* define the PHA structure */

struct PHA {

  long NumberChannels;                           /* Number of spectrum channels */
  long FirstChannel;                             /* First channel number */

  float* Pha; /*NumberChannels*/                 /* PHA data */
  float* StatError; /*NumberChannels*/           /* Statistical error */
  float* SysError; /*NumberChannels*/            /* Statistical error */

  int*   Quality; /*NumberChannels*/             /* Data quality */
  int*   Grouping;  /*NumberChannels*/           /* Data grouping */
  int*   Channel;  /*NumberChannels*/            /* Channel number */


  float* AreaScaling; /*NumberChannels*/         /* Area scaling factor */
  float* BackScaling; /*NumberChannels*/         /* Background scaling factor */

  float Exposure;                                /* Exposure time */
  float CorrectionScaling;                       /* Correction file scale factor */
  int   DetChans;                                /* Content of DETCHANS keyword */


  int Poisserr;                                  /* If true, errors are Poisson */
  char Datatype[FLEN_KEYWORD];                   /* "COUNT" for count data and "RATE" for count/sec */
  char Spectrumtype[FLEN_KEYWORD];               /* "TOTAL", "NET", or "BKG" */

  char ResponseFile[FLEN_FILENAME];              /* Response filename */
  char AncillaryFile[FLEN_FILENAME];             /* Ancillary filename */
  char BackgroundFile[FLEN_FILENAME];            /* Background filename */
  char CorrectionFile[FLEN_FILENAME];            /* Correction filename */

  char ChannelType[FLEN_KEYWORD];                /* Value of CHANTYPE keyword */
  char PHAVersion[FLEN_KEYWORD];                 /* PHA extension format version */
  char Telescope[FLEN_KEYWORD];                             
  char Instrument[FLEN_KEYWORD];
  char Detector[FLEN_KEYWORD];
  char Filter[FLEN_KEYWORD];
  char Datamode[FLEN_KEYWORD];

  char *XSPECFilter[100];                       /* Filter keywords */
};




