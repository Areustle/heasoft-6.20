/*
   nu_defs.h  

   DESCRIPTION:
         This module provides a collection of definitions and macros
         useful for NuSTAR Files handle. 
      
   CHANGE HISTORY:
	 0.1.0: - NS 25/10/2010 - First version
	 0.1.1: - NS 12/01/2011 - CLNM_SURR,CARD_COMM_SURR,CLNM_SWTRIG and CARD_COMM_SWTRIG added
	 0.1.2: - NS 11/02/2011 - KWVL_EXTNAME_HK4FPM, KWVL_EXTNAME_PH2PI, CLNM_PI, CARD_COMM_PI, KWVL_PINULL,
                                  CLNM_CZT0TEMP, CLNM_CZT1TEMP, CLNM_CZT2TEMP, CLNM_CZT3TEMP, CLNM_TEMP
                                  and CLNM_SLOPE added
	 0.1.3: - NS 10/03/2011 - PIX_SIZE_MICRON, KWVL_EXTNAME_PIXPOS, CLNM_DET1X, CARD_COMM_DET1X
                                  CLNM_DET1Y, CARD_COMM_DET1Y, CLNM_GX, CLNM_GY, CLNM_SIGMAX, CLNM_SIGMAY
                                  CLNM_RAW2X, CARD_COMM_RAW2X,CLNM_RAW2Y,CARD_COMM_RAW2Y
                                  SUBPIX_NUM,BORDER_FB_OFF,CENTER_FB_OFF,DET1_PIXS,DET1_ROWS,DET1X_MIN,DET1X_MAX,DET1Y_MIN,DET1Y_MAX 
                                  RAW2_PIXS,RAW2_ROWS,RAW2X_MIN,RAW2X_MAX,RAW2Y_MIN,RAw2Y_MAX added
	 0.1.4: - NS 26/04/2011 - Added new keywords needed for the routines handling bad pixel
	 0.1.5: - NS 07/07/2011 - KWVL_EXTNAME_PHAPAR, CLNM_EVTTHR and CLNM_TIMERISE added
	 0.1.6: - NS 11/07/2011 - CLNM_GX, CLNM_GY, CLNM_SIGMAX and CLNM_SIGMAY removed
                                - CLNM_REF_DET1X, CLNM_REF_DET1Y and CLNM_PDF added
	 0.1.7: - NS 02/08/2011 - KWNM_TARG_ID, KWNM_HDUCLASS, KWNM_HDUCLAS1, KWNM_TIMEPIXR, KWNM_TIMESYS, KWNM_MJDREFI,
                                  KWNM_MJDREFF, KWNM_CLOCKAPP and KWNM_TIMEUNIT added
                                - RAW2_BORDER_OFF, RAW2_CENTER_OFF, KWVL_DET1NULL, KWVL_PINULL and CARD_COMM_TNULL added
                                - BORDER_FB_OFF modified
	 0.1.8: - NS 31/08/2011 - NEIGH_PIX_BADPOS, CLNM_BADPOS, CARD_COMM_BADPOS, CLNM_STATUS and CARD_COMM_STATUS added
	 0.1.9: - NS 15/09/2011 - NEIGH_PIX_HOTPOS, CLNM_HOTPOS, CARD_COMM_HOTPOS and EXT_FITS_HP added
	 0.2.0: - NS 30/09/2011 - DET2X_MIN, DET2X_MAX, DET2Y_MIN, DET2Y_MAX, PIX_SIZE_MM, SUBPIX_SIZE_MM and KWVL_DET2NULL added
                                - KWVL_EXTNAME_SYSTEM_ALIGNMENT, KWVL_EXTNAME_METROLOGY_ALIGNMENT and KWVL_EXTNAME_MAST_ASPECT added
	 0.2.1: - NS 10/10/2011 - DET_IMG_SCALE added
	 0.2.2: - NS 21/10/2011 - PI_MIN and PI_MAX added
	 0.2.3: - NS 17/11/2011 - Modified PSD met grid file dimensions
                                - KWVL_PSDID_PSD0 and KWVL_PSDID_PSD1 added
	 0.2.4: - NS 02/12/2011 - Modified some file name suffixs
	 0.2.5: - NS 06/12/2011 - CLNM_X0_INT, CARD_COMM_X0_INT, CLNM_Y0_INT, CARD_COMM_Y0_INT, CLNM_X1_INT,
                                  CARD_COMM_X1_INT, CLNM_Y1_INT and CARD_COMM_Y1_INT added
	 0.2.6: - NS 12/12/2011 - Added new keywords used by 'nuoptaxis' task
                                - Added new keywords used by 'numkarf' task
	 0.2.7: - NS 12/01/2012 - KWVL_EXTNAME_OFFAXIS_HISTO, CLNM_DURATION, CARD_COMM_DURATION, KWVL_EXTNAME_DET1_REFPOINT
                                  CLNM_X_DET1, CARD_COMM_X_DET1, CLNM_Y_DET1 and CARD_COMM_Y_DET1 added
	 0.2.8: - NS 16/02/2012 - Added new keywords used by 'nulivetime' task
                                - Added new keywords used by 'nuflagdepth' task
	 0.2.9: - NS 05/03/2012 - Added new keywords used by 'numkrmf' task
	 0.3.0: - NS 30/04/2012 - Added new keywords used by 'numkarf' task
	 0.3.1: - AG 26/06/2012 - Added 'CLNM_E3' and 'KWNM_GRADE_DEPTH' block used by 'nuflagdepth' task
	 0.3.2: - NS 12/07/2012 - Added new keywords used by 'nuflagevt' task
                                - 'GRADE_MIN' and 'GRADE_MAX' added 
	 0.3.3: - NS 04/10/2012 - Added new keywords used by 'nucalcsaa' task
	 0.3.4: - NS 24/10/2012 - Added new keywords used by 'nulccorr' task
                                - Added new keywords used by 'numkrmf' task
                                - Added new keywords used by 'nuattcorr' task
	 0.3.5: - NS 07/02/2013 - Added new keywords used by 'nuskypos' task
                                - Added new keywords used by 'numkarf' task
	 0.3.6: - NS 12/04/2013 - Added new keywords used by 'numkarf' and 'numetrology' task
	 0.3.7: - NS 06/06/2013 - Added new keywords used by 'nubackscale' and 'nulccorr' task
 	 0.3.7: - NS 17/07/2013 - Added new keywords used by 'numkarf' task
 	 0.3.8: - NS 12/12/2013 - Added new keywords used by 'nuexpomap' task
 	 0.3.8: - RF 07/06/2016 - Added new keywords used by 'nusplitsc' task
	 0.3.9: - RF 16/03/2016 - Added new keywords used by 'nucalcsaa' task
	 0.4.0: - RF 22/04/2016 - Added new keywords used by 'nuskypos' task


   DOCUMENTATION:
 	        [1] Definition of the Flexible Image Transport System, NOST 100-1.0
 		[2] FITSIO User Manual
 
*/                    

#ifndef NU_DEFS_H
#define NU_DEFS_H


/*---------------------------------------------------------------*/
/*                        COMMON DEFINITIONS                     */
/*---------------------------------------------------------------*/

#define DF_NONE                        "NONE"
#define DF_DEFAULT                     "DEFAULT"



/*---------------------------------------------------------------*/
/*                  Sub-Detector dimensions                      */
/*---------------------------------------------------------------*/


#define DET_PIXS      32    /* Sub-Detector width */
#define DET_ROWS      32    /* Sub-Detector height */

#define CAP_DIM       16    /* Number of capacitors */

#define PHAS_MOL       9    /* PHAS molteplicity */
#define NEIGH_X  -1,  0, +1, -1, 0, +1, -1,  0, +1
#define NEIGH_Y  -1, -1, -1,  0, 0,  0, +1, +1, +1

#define NEIGH_PIX_MOL       (PHAS_MOL-1)  /* Neighbours pixels (without the central pix) */
#define NEIGH_PIX_X  -1,  0, +1, -1, +1, -1,  0, +1
#define NEIGH_PIX_Y  -1, -1, -1,  0,  0, +1, +1, +1
#define NEIGH_PIX_BADPOS  128, 64, 32, 16, 8, 4, 2, 1
#define NEIGH_PIX_HOTPOS  NEIGH_PIX_BADPOS

#define RAWX_MIN       0
#define RAWX_MAX      31

#define RAWY_MIN       0
#define RAWY_MAX      31

#define S_CAP_MIN      0
#define S_CAP_MAX     15

/*---------------------------------------------------------------*/
/*        Focal Plane Bench Frame (FB) dimensions                */
/*---------------------------------------------------------------*/

#define SUBPIX_NUM         5   /* Number of sub-pixels for each physical pixel */

#define BORDER_FB_OFF      3.5
#define CENTER_FB_OFF      1

/* #define DET1_PIXS          (int)( (DET_PIXS*2 + BORDER_FB_OFF*2 + CENTER_FB_OFF)*SUBPIX_NUM )   /\* FB width (360) *\/ */
/* #define DET1_ROWS          (int)( (DET_ROWS*2 + BORDER_FB_OFF*2 + CENTER_FB_OFF)*SUBPIX_NUM )  /\* FB height (360) *\/ */
#define DET1_PIXS          360
#define DET1_ROWS          360

#define DET1X_MIN          1
#define DET1X_MAX          DET1_PIXS

#define DET1Y_MIN          1
#define DET1Y_MAX          DET1_ROWS


/*---------------------------------------------------------------*/
/*        Optics Bench Frame (OB) dimensions                */
/*---------------------------------------------------------------*/

#define DET2X_MIN          1
#define DET2X_MAX          600

#define DET2Y_MIN          1
#define DET2Y_MAX          600

/*---------------------------------------------------------------*/
/*                     RAW2 Frame dimensions                     */
/*---------------------------------------------------------------*/

#define RAW2_BORDER_OFF    3
#define RAW2_CENTER_OFF    1

#define RAW2_PIXS          ( DET_PIXS*2 + RAW2_BORDER_OFF*2 + RAW2_CENTER_OFF )   /* RAW2 Frame width  */
#define RAW2_ROWS          ( DET_ROWS*2 + RAW2_BORDER_OFF*2 + RAW2_CENTER_OFF )   /* RAW2 Frame height */

#define RAW2X_MIN          1
#define RAW2X_MAX          RAW2_PIXS

#define RAW2Y_MIN          1
#define RAw2Y_MAX          RAW2_ROWS



/*---------------------------------------------------------------*/
/*                       PSD dimensions                          */
/*---------------------------------------------------------------*/

#define PSD_PIXS      73    /* PSD width */
#define PSD_ROWS      73    /* PSD height */



/*---------------------------------------------------------------*/
/*                           physical units                      */
/*---------------------------------------------------------------*/

#define PIX_SIZE_MICRON           604.8
#define PIX_SIZE_MM               (PIX_SIZE_MICRON/1000)
#define SUBPIX_SIZE_MICRON        ( PIX_SIZE_MICRON/SUBPIX_NUM )
#define SUBPIX_SIZE_MM            ( SUBPIX_SIZE_MICRON/1000 )  /* 0.12096 mm */

#define DET_IMG_SCALE             1.209600E-01 

#define SUBPIX_SIZE_DEG           6.828076E-04       /* image scale (deg/pixel)    */
#define SUBPIX_SIZE_ARCMIN        (6.828076E-04*60)  /* image scale (arcmin/pixel) */


#define UNIT_SEC			"s"
#define UNIT_MIN			"min"
#define UNIT_MM				"mm"
#define UNIT_KM				"km"
#define UNIT_KMS			"km/s"
#define UNIT_DEG			"deg"
#define UNIT_ARCMIN			"arcmin"
#define UNIT_CHANN			"chan"
#define UNIT_PIXEL			"pixel"
#define UNIT_V				"V"
#define UNIT_KV				"kV"
#define UNIT_MUA			"uA"
#define UNIT_MA				"mA"
#define UNIT_MV				"mV"
#define UNIT_NBAR			"nbar"
#define UNIT_CNTSEC			"count/s"
#define UNIT_FLUX			"count/s/cm**2"
#define UNIT_ENERGY			"eV"
#define UNIT_ARCSEC			"arscsec"
#define UNIT_CT				"ct"
#define UNIT_NA				"NA"

#define CARD_COMM_PHYSUNIT	        "physical unit of field"


/*---------------------------------------------------------------*/
/*                        COMMON KEYWORDS                        */
/*---------------------------------------------------------------*/


#define KWNM_EXTNAME		"EXTNAME"  /* name of the Bintable extension */
#define CARD_COMM_EXTNAME	"name of this binary table extension"
#define KWVL_EXTNAME_EVT        "EVENTS"   /* name of the Bintable Events extension */
#define KWVL_EXTNAME_GRADES     "GRADES"   /* name of the Bintable Grade extension */
#define KWVL_EXTNAME_OFFSET     "OFFSET"   /* name of the Bintable Offset extension */
#define KWVL_EXTNAME_HK4FPM     "HK4FPM"   /* name of the Bintable Housekeeping extension */
#define KWVL_EXTNAME_HK1FPM     "HK1FPM"   
#define KWVL_EXTNAME_PH2PI      "GAIN"       /* name of the Bintable Gain extension */
#define KWVL_EXTNAME_PIXPOS     "PIXPOS"     /* name of the Bintable Pixel Position extension */
#define KWVL_EXTNAME_BADPIX     "BADPIX"     /* name of the Bintable Bad Pixels extension */
#define KWVL_EXTNAME_DISPIX     "DISPIX"     /* name of the Bintable Disabled Pixels extension */
#define KWVL_EXTNAME_MET_RAW    "MET_RAW"     /* name of the Metrology Bintable extension */
#define KWVL_EXTNAME_MET_CMP    "MET_CMP"     /* name of the Metrology Compressed Bintable extension */
#define KWVL_EXTNAME_PSDPOS     "PSDPOS"      /* name of the Position Sensing Detector Bintable extension */
#define KWVL_EXTNAME_METROLOGY  "METROLOGY"   /* name of the Metrology Grid Bintable extension */
#define KWVL_EXTNAME_PHAPAR     "PHAPAR"      /* name of the Bintable PHAPAR extension */
#define KWVL_EXTNAME_ATTITUDE   "ATTITUDE"    /* name of the Bintable ATTITUDE extension */
#define KWVL_EXTNAME_SYSTEM_ALIGNMENT     "SYSTEM_ALIGNMENT"     /* name of the Alignment SYSTEM extension */
#define KWVL_EXTNAME_METROLOGY_ALIGNMENT  "METROLOGY_ALIGNMENT"  /* name of the Alignment METROLOGY extension */
#define KWVL_EXTNAME_OPTICAL_AXIS         "OPTICAL_AXIS"         /* name of the Alignment OPTICAL_AXIS extension */
#define KWVL_EXTNAME_APERTURE_STOP        "APERTURE_STOP"         /* name of the Alignment APERTURE_STOP extension */
#define KWVL_EXTNAME_MAST_ASPECT          "MAST_ASPECT"          /* name of the Mast Aspect Solution Bintable extension */
#define KWVL_EXTNAME_SPECTRUM             "SPECTRUM"             /* name of the PHA 'SPECTRUM' Bintable extension */
#define KWVL_EXTNAME_OFF_AXIS             "OFF_AXIS"             /* name of the Off-Axis 'OFF_AXIS' Bintable extension */
#define KWVL_EXTNAME_OFFAXIS_HISTO        "OFFAXIS_HISTO"        /* name of the Off-Axis Histogram 'OFFAXIS_HISTO' Bintable extension */
#define KWVL_EXTNAME_GTI                  "GTI"                  /* name of the 'GTI' Bintable extension */
#define KWVL_EXTNAME_VIGNET               "VIGNET" 
#define KWVL_EXTNAME_REEF                 "REEF" 
#define KWVL_EXTNAME_SPECRESP             "SPECRESP" 
#define KWVL_EXTNAME_DET1_REFPOINT        "DET1_REFPOINT" 
#define KWVL_EXTNAME_APERTURE_STOP_CENTER "APERTURE_STOP_CENTER" 
#define KWVL_EXTNAME_APERTURE             "APERTURE"
#define KWVL_EXTNAME_OFFSET               "OFFSET"
#define KWVL_EXTNAME_ASP_HIST             "ASP_HIST"
#define KWVL_EXTNAME_ASP_HIST_COMP        "ASP_HIST_COMP"
#define KWVL_EXTNAME_CLC                  "CLC"
#define KWVL_EXTNAME_CLCFILTER            "CLCFILTER"
#define KWVL_EXTNAME_DEPTHCUT             "DEPTHCUT"
#define KWVL_EXTNAME_EVTCUT               "EVTCUT"
#define KWVL_EXTNAME_GRPRMF               "GRPRMF"
#define KWVL_EXTNAME_ORBIT                "ORBIT"
#define KWVL_EXTNAME_SAA                  "SAA"
#define KWVL_EXTNAME_TENTACLE             "TENTACLE"
#define KWVL_EXTNAME_SAANEW               "SAANEW"
#define KWVL_EXTNAME_TENTACLENEW          "TENTACLENEW"
#define KWVL_EXTNAME_RATE                 "RATE"
#define KWVL_EXTNAME_CHUOFFSET            "CHUOFFSET"
#define KWVL_EXTNAME_GHOST_RAYS           "GHOST_RAYS"
#define KWVL_EXTNAME_DETABS               "DETABS"
#define KWVL_EXTNAME_GRP2DPSF             "GRP2DPSF"

#define KWNM_EXTVER		"EXTVER"
#define CARD_COMM_EXTVER	"There shall be one instance of this extension field"

#define KWNM_TELESCOP		"TELESCOP" /* mission name */
#define CARD_COMM_TELESCOP	"Telescope (mission) name"
#define KWVL_TELESCOP		"NuSTAR"

#define KWNM_INSTRUME		"INSTRUME" /* instrument name */
#define CARD_COMM_INSTRUME	"Instrument name (FPMA or FPMB)"
#define KWVL_INSTRUME_FPM	"FPM"
#define KWVL_INSTRUME_FPMA	"FPMA"	           
#define KWVL_INSTRUME_FPMB	"FPMB"

#define KWNM_ORIGIN	       "ORIGIN"	          /* Source of FITS file */
#define CARD_COMM_ORIGIN       "Source of FITS file"

#define KWNM_OBS_ID             "OBS_ID"         /* Observation ID */
#define CARD_COMM_OBS_ID        "Observation ID"

#define KWNM_TARG_ID            "TARG_ID"   /* Target ID */

#define KWNM_RA_OBJ             "RA_OBJ"
#define KWNM_DEC_OBJ            "DEC_OBJ"

#define KWNM_RA_NOM             "RA_NOM"
#define CARD_COMM_RA_NOM        "[deg] R.A. Nominal"
#define KWNM_DEC_NOM            "DEC_NOM"
#define CARD_COMM_DEC_NOM        "[deg] Dec. Nominal"

#define KWNM_RA_PNT             "RA_PNT"
#define KWNM_DEC_PNT            "DEC_PNT"
#define KWNM_PA_PNT             "PA_PNT"

#define KWNM_DETNAM	       "DETNAM" /* CZT Detector ID (DET0,DET1,DET2 or DET3) */
#define CARD_COMM_DETNAM       "CZT Detector ID (0,1,2 or 3)"
#define KWVL_DETNAM_DET0       "DET0"
#define KWVL_DETNAM_DET1       "DET1"
#define KWVL_DETNAM_DET2       "DET2"
#define KWVL_DETNAM_DET3       "DET3"

#define KWNM_GRADE_DEPTH       "GRADE_DE" /* GRADE ID for DEPTHCUT (DET0_SINGLE,DET1_SINGLE,DET2_SINGLE,DET3_SINGLE,DET0_DOUBLE,DET1_DOUBLE,DET2_DOUBLE or DET3_DOUBLE) */
#define CARD_COMM_GRADE_DEPTH  "GRADE ID for DEPTHCUT (DET0_SINGLE,DET1_SINGLE,DET2_SINGLE,DET3_SINGLE,DET0_DOUBLE,DET1_DOUBLE,DET2_DOUBLE or DET3_DOUBLE)"
#define KWVL_GRADE_DEPTH_0S    "DET0_SINGLE"
#define KWVL_GRADE_DEPTH_1S    "DET1_SINGLE"
#define KWVL_GRADE_DEPTH_2S    "DET2_SINGLE"
#define KWVL_GRADE_DEPTH_3S    "DET3_SINGLE"
#define KWVL_GRADE_DEPTH_0D    "DET0_DOUBLE"
#define KWVL_GRADE_DEPTH_1D    "DET1_DOUBLE"
#define KWVL_GRADE_DEPTH_2D    "DET2_DOUBLE"
#define KWVL_GRADE_DEPTH_3D    "DET3_DOUBLE"

#define KWNM_DET_ID		"DET_ID" /* CZT Detector ID (0,1,2 or 3) */
#define CARD_COMM_DET_ID	"CZT Detector ID (0,1,2 or 3)"
#define KWVL_DET_ID_0	        0	 
#define KWVL_DET_ID_1	        1
#define KWVL_DET_ID_2	        2
#define KWVL_DET_ID_3	        3

#define KWNM_PSDID              "PSDID" /* Position Sensing Detector ID (PSD0 or PSD1) */ 
#define KWVL_PSDID_PSD0         "PSD0"
#define KWVL_PSDID_PSD1         "PSD1"

#define KWNM_CREATOR		"CREATOR"	   /* Program that created this FITS file	    */
#define CARD_COMM_CREATOR       "Program that created this FITS file"

#define KWNM_DATE		"DATE"		   /* FITS file creation date		            */
#define CARD_COMM_DATE		"file creation date (YYYY-MM-DDThh:mm:ss UTC)"

#define KWNM_DATEOBS		"DATE-OBS"	/* Date of the data start time (UT) 		    */
#define CARD_COMM_DATEOBS	"Start date/time, e.g. (YYYY-MM-DDThh:mm:ss UTC)"

#define KWNM_TIMEOBS		"TIME-OBS"	/* time (hh:mm:ss) of the data start time (UT)      */
#define CARD_COMM_TIMEOBS	"time (hh:mm:ss) of the data start time (UT)"

#define KWNM_DATEEND		"DATE-END"	/* Date of the data stop time (UT) 		    */		
#define CARD_COMM_DATEEND	"Stop date/time, e.g. (YYYY-MM-DDThh:mm:ss UTC)"

#define KWNM_TIMEEND		"TIME-END"	/* time (hh:mm:ss) of the data stop time (UT)      */
#define CARD_COMM_TIMEEND	"time (hh:mm:ss) of the data stop time (UT)"

#define KWNM_TSTART		"TSTART"	/* Observation start time       	            */

#define KWNM_TSTOP		"TSTOP"	        /* Observation end time         	            */

#define KWNM_NUFLAG		"NUFLAG"
#define CARD_COMM_NUFLAG	"Are events flagged (T/F)?"

#define KWNM_HDUCLASS		"HDUCLASS"
#define KWNM_HDUCLAS1		"HDUCLAS1"
#define KWNM_TIMEPIXR		"TIMEPIXR"

#define KWNM_TIMESYS		"TIMESYS"
#define CARD_COMM_TIMESYS	"Terrestrial Time"

#define KWNM_MJDREF		"MJDREF"

#define KWNM_MJDREFI		"MJDREFI"
#define CARD_COMM_MJDREFI	"MJD reference day 01 Jan 2010 00:00:00 UTC"

#define KWNM_MJDREFF 		"MJDREFF"
#define CARD_COMM_MJDREFF	"Fractional part of MJD ref (32.184secs+34 leap)"

#define KWNM_CLOCKAPP		"CLOCKAPP"

#define KWNM_TIMEUNIT		"TIMEUNIT"
#define CARD_COMM_TIMEUNIT	"Time unit for timing header keywords"

#define KWNM_TIMEREF		"TIMEREF"
#define CARD_COMM_TIMEREF	"reference time"

#define CARD_COMM_TNULL         "Illegal value for this column"

#define KWVL_DET1NULL           -4095L
#define KWVL_DET2NULL           -4095L
#define KWVL_PINULL             -4095L
#define PI_MIN                   0
#define PI_MAX                   4095

#define KWNM_REF_TIME            "REF_TIME"
#define KWNM_DURATION            "DURATION"

#define KWNM_ONTIME              "ONTIME"
#define CARD_COMM_ONTIME         "Exposure time in seconds"

#define KWNM_OBJECT              "OBJECT"  

#define KWNM_TELAPSE             "TELAPSE"  

#define KWNM_EQUINOX             "EQUINOX"

#define KWNM_NAXIS		 "NAXIS"

#define KWNM_X_OFFSET           "X-OFFSET"
#define KWNM_Y_OFFSET           "Y-OFFSET"

#define KWNM_DEADC               "DEADC"
#define CARD_COMM_DEADC          "Deadtime correction"

#define KWNM_DEADAPP             "DEADAPP"
#define CARD_COMM_DEADAPP        "Has DEADC been applied to data"

#define KWNM_DEADAPP             "DEADAPP"
#define CARD_COMM_DEADAPP        "Has DEADC been applied to data"

#define KWNM_EXPOSURE            "EXPOSURE"
#define CARD_COMM_EXPOSURE       "Exposure time"

#define KWNM_LIVETIME            "LIVETIME"
#define CARD_COMM_LIVETIME       "On-source time"

#define KWNM_BACKSCAL            "BACKSCAL"
#define CARD_COMM_BACKSCAL       "background file scaling factor"

#define KWNM_BUNIT               "BUNIT"
#define CARD_COMM_BUNIT          "Units of image array"

#define KWNM_VIGNAPP             "VIGNAPP"
#define CARD_COMM_VIGNAPP        "Is vignetting correction applied (T/F)?"

#define KWNM_VIGNEN              "VIGNEN"
#define CARD_COMM_VIGNEN         "Energy value used for vignetting correction (keV)"

#define KWNM_PIXBIN              "PIXBIN"
#define CARD_COMM_PIXBIN         "Aspect histogram bin size (pixels)"

#define KWNM_PERCENT             "PERCENT"
#define CARD_COMM_PERCENT        "DET1 instrument map probability threshold"

#define KWNM_NULCCO              "NULCCO"
#define CARD_COMM_NULCCO         "Has RATE been corrected on ground (T/F)?"

#define KWNM_TIMEDEL             "TIMEDEL"
#define KWNM_TIMEPIXR            "TIMEPIXR"
#define KWNM_TIMEZERO            "TIMEZERO"

#define KWNM_DEPTHCUT            "DEPTHCUT"
#define CARD_COMM_DEPTHCUT       "Events depth cut screening"

#define KWNM_NUATTCOR            "NUATTCOR"
#define CARD_COMM_NUATTCOR       "Has attitude file been corrected on-ground (T/F)?"


/*---------------------------------------------------------------*/
/*                          COLUMNS NAME                         */
/*---------------------------------------------------------------*/

#define CLNM_TIME		"TIME"	/* Event Time(seconds since Jan 2010 00:00:00 UTC) */
#define CARD_COMM_TIME          "Event Time(seconds since Jan 2010 00:00:00 UTC)"

#define CLNM_TIME_STOP		"TIME_STOP"

#define CLNM_NUMRISE	        "NUMRISE"

#define CLNM_DENRISE	        "DENRISE"

#define CLNM_POSTPHAS           "POSTPHAS"

#define CLNM_PREPHAS            "PREPHAS"

#define CLNM_RAWX		"RAWX"		/* X-position of central pixel in raw coordinates */
#define CARD_COMM_RAWX          "X-position of central pixel in raw coordinates"
#define CARD_COMM_BP_RAWX       "X-position in raw detector coordinates"

#define CLNM_RAWY		"RAWY"		/* Y-position of central pixel in raw coordinates */
#define CARD_COMM_RAWY          "Y-position of central pixel in raw coordinates"
#define CARD_COMM_BP_RAWY       "Y-position in raw detector coordinates"

#define CLNM_DET1X		 "DET1X"		/* Event X position Focal Plane Bench Frame */
#define CARD_COMM_DET1X          "Event X position Focal Plane Bench Frame"

#define CLNM_DET1Y		 "DET1Y"		/* Event Y position Focal Plane Bench Frame */
#define CARD_COMM_DET1Y          "Event Y position Focal Plane Bench Frame"

#define CLNM_DET2X		 "DET2X"		/* Event X position Optics Bench Frame */
#define CARD_COMM_DET2X          "Event X position Optics Bench Frame"

#define CLNM_DET2Y		 "DET2Y"		/* Event Y position Optics Bench Frame */
#define CARD_COMM_DET2Y          "Event Y position Optics Bench Frame"

#define CLNM_RAW2X		 "RAW2X"		/* Event X position RAW2X frame */
#define CARD_COMM_RAW2X          "Event X position RAW2X frame"

#define CLNM_RAW2Y		 "RAW2Y"		/* Event Y position RAW2Y frame */
#define CARD_COMM_RAW2Y          "Event Y position RAW2Y frame"

#define CLNM_S_CAP              "S_CAP"

#define CLNM_DET_ID             "DET_ID"

#define CLNM_RAWPHAS            "RAWPHAS"
#define CARD_COMM_RAWPHAS       "Pixel Pulse-height raw values"

#define CLNM_OFFPHAS            "OFFPHAS"
#define CARD_COMM_OFFPHAS       "Pixel Pulse-height after offset correction"

#define CLNM_TRPHAS             "TRPHAS"
#define CARD_COMM_TRPHAS        "Pixel Pulse-height after time of rise correction"

#define CLNM_PHAS               "PHAS"
#define CARD_COMM_PHAS          "Pixel Pulse-height after common mode correction"

#define CLNM_SURR               "SURR"
#define CARD_COMM_SURR          "Sum of pulse-heights for pixels below threshold"

#define CLNM_SURRPI             "SURRPI"
#define CARD_COMM_SURRPI        "Sum of pulse invariants for pixels below threshold"

#define CLNM_PHA                "PHA"
#define CARD_COMM_PHA           "Event Pulse-height after event recognition"
#define KWVL_PHANULL             -4095L
 
#define CLNM_GRADE              "GRADE"
#define CARD_COMM_GRADE         "Event Grade"
#define KWVL_GRADENULL           32
#define GRADE_MIN                0
#define GRADE_MAX                32

#define CLNM_SWTRIG             "SWTRIG"
#define CARD_COMM_SWTRIG        "Pixel software threshold Flag"

#define CLNM_GRADEID            "GRADEID"

#define CLNM_OFFSET             "OFFSET" 

#define CLNM_BADPOS             "BADPOS"
#define CARD_COMM_BADPOS        "Position of neighbor bad pixels"

#define CLNM_HOTPOS             "HOTPOS"
#define CARD_COMM_HOTPOS        "Position of neighbor hot/flickering pixels"

#define CLNM_PI                 "PI" 
#define CARD_COMM_PI            "Event Pulse Invariant"
#define KWVL_PINULL              -4095L

#define CLNM_PIS_GAIN           "PIS_GAIN" 
#define CARD_COMM_PIS_GAIN      "Pixel Pulse Invariant before charge loss correction"

#define CLNM_PI_CLC             "PI_CLC" 
#define CARD_COMM_PI_CLC        "Pixel Pulse Invariant after charge loss correction"

#define CLNM_CZT0TEMP           "CZT0TEMP" 
#define CLNM_CZT1TEMP           "CZT1TEMP" 
#define CLNM_CZT2TEMP           "CZT2TEMP" 
#define CLNM_CZT3TEMP           "CZT3TEMP" 

#define CLNM_SHLDLO             "SHLDLO"
#define CLNM_SHLDHI             "SHLDHI"
#define CLNM_NACCEPT            "NACCEPT"
#define CLNM_NREJECT            "NREJECT"

#define CLNM_LIVETIME           "LIVETIME"

#define CLNM_TEMP               "TEMP" 

#define CLNM_SLOPE              "SLOPE"

#define CLNM_REF_DET1X 	        "REF_DET1X"
#define CLNM_REF_DET1Y 	        "REF_DET1Y"
#define CLNM_PDF 	        "PDF"

#define CLNM_STATUS             "STATUS"
#define CARD_COMM_STATUS        "Event Quality Flag"

#define CLNM_BADFLAG   	        "BADFLAG"
#define CARD_COMM_BADFLAG       "Bad Pixel flag"

#define CLNM_XA_PSD0	        "XA_PSD0"
#define CLNM_XB_PSD0	        "XB_PSD0"
#define CLNM_YA_PSD0	        "YA_PSD0"
#define CLNM_YB_PSD0	        "YB_PSD0"
#define CLNM_XA_PSD1	        "XA_PSD1"
#define CLNM_XB_PSD1	        "XB_PSD1"
#define CLNM_YA_PSD1	        "YA_PSD1"
#define CLNM_YB_PSD1	        "YB_PSD1"
#define CLNM_LAS_ON	        "LAS_ON"
#define CLNM_CLAMP	        "CLAMP"

#define CLNM_X_PSD0	        "X_PSD0"
#define CARD_COMM_X_PSD0        "X-position of laser spot on PSD0"

#define CLNM_Y_PSD0	        "Y_PSD0"
#define CARD_COMM_Y_PSD0        "Y-position of laser spot on PSD0"

#define CLNM_X_PSD1	        "X_PSD1"
#define CARD_COMM_X_PSD1        "X-position of laser spot on PSD1"

#define CLNM_Y_PSD1	        "Y_PSD1"
#define CARD_COMM_Y_PSD1        "Y-position of laser spot on PSD1"

#define CLNM_X0_INT	        "X0_INT"
#define CARD_COMM_X0_INT	"Sum of X laser intensities values for PSD0"

#define CLNM_Y0_INT	        "Y0_INT"
#define CARD_COMM_Y0_INT	"Sum of Y laser intensities values for PSD0"

#define CLNM_X1_INT	        "X1_INT"
#define CARD_COMM_X1_INT	"Sum of X laser intensities values for PSD1"

#define CLNM_Y1_INT	        "Y1_INT"
#define CARD_COMM_Y1_INT	"Sum of Y laser intensities values for PSD1"

#define CLNM_METGRID_FLAG       "METGRID_FLAG"
#define CARD_COMM_METGRID_FLAG  "MET Grid flag(0 in grid,1 out grid,2 not corr)"

#define CLNM_X_STAGE	        "X_STAGE"
#define CLNM_Y_STAGE	        "Y_STAGE"
#define CLNM_X_PSD	        "X_PSD"
#define CLNM_Y_PSD	        "Y_PSD"
#define CLNM_DELTAX             "DELTAX"
#define CLNM_DELTAY             "DELTAY"

#define CLNM_EVTTHR             "EVTTHR"
#define CLNM_TIMERISE           "TIMERISE"

#define CLNM_Q_FB_MD0           "Q_FB_MD0"
#define CLNM_Q_FB_MD1           "Q_FB_MD1"
#define CLNM_V_FB_MD0           "V_FB_MD0"
#define CLNM_V_FB_MD1           "V_FB_MD1"
#define CLNM_Q_OB_ML0           "Q_OB_ML0"
#define CLNM_Q_OB_ML1           "Q_OB_ML1"
#define CLNM_V_OB_ML0           "V_OB_ML0"
#define CLNM_V_OB_ML1           "V_OB_ML1"
#define CLNM_Q_FB_OB            "Q_FB_OB"
#define CLNM_V_FB_OB            "V_FB_OB"
#define CLNM_L0_ORIG            "L0_ORIG"
#define CLNM_L1_ORIG            "L1_ORIG"
#define CLNM_L0_POINT           "L0_POINT"
#define CLNM_L1_POINT           "L1_POINT"
#define CLNM_Q_FPMA_DET1        "Q_FPMA_DET1"
#define CLNM_Q_FPMB_DET1        "Q_FPMB_DET1"
#define CLNM_V_FPMA_DET1        "V_FPMA_DET1"
#define CLNM_V_FPMB_DET1        "V_FPMB_DET1"
#define CLNM_Q_FB_FPMA          "Q_FB_FPMA"
#define CLNM_Q_FB_FPMB          "Q_FB_FPMB"
#define CLNM_V_FB_FPMA          "V_FB_FPMA"
#define CLNM_V_FB_FPMB          "V_FB_FPMB"
#define CLNM_Q_DET2A_OB         "Q_DET2A_OB"
#define CLNM_Q_DET2B_OB         "Q_DET2B_OB"
#define CLNM_V_DET2A_OB         "V_DET2A_OB"
#define CLNM_V_DET2B_OB         "V_DET2B_OB"

#define CLNM_T_FBOB             "T_FBOB"
#define CARD_COMM_T_FBOB        "Translation from Focal Plane to Optical Plane"

#define CLNM_Q_FBOB             "Q_FBOB"
#define CARD_COMM_Q_FBOB        "Quaternion from Focal Plane to Optical Plane"

#define CLNM_X_DET2A            "X_DET2A"
#define CLNM_Y_DET2A            "Y_DET2A"
#define CLNM_X_DET2B            "X_DET2B"
#define CLNM_Y_DET2B            "Y_DET2B"

#define CLNM_X_DET1A            "X_DET1A"
#define CLNM_Y_DET1A            "Y_DET1A"
#define CLNM_X_DET1B            "X_DET1B"
#define CLNM_Y_DET1B            "Y_DET1B"

#define CLNM_X_OA               "X_OA"
#define CARD_COMM_X_OA          "Optical Axis X position (SKY Frame)"

#define CLNM_Y_OA               "Y_OA"
#define CARD_COMM_Y_OA          "Optical Axis Y position (SKY Frame)"

#define CLNM_X	                "X"
#define CLNM_Y	                "Y"
#define CLNM_R                  "R"
#define CLNM_SHAPE              "SHAPE"
#define CLNM_COMPONENT          "COMPONENT"
#define CLNM_ROTANG             "ROTANG"

#define CLNM_OFF_AXIS           "OFF_AXIS"
#define CARD_COMM_OFF_AXIS      "Off-Axis Angle"

#define CLNM_PHI                "PHI"
#define CARD_COMM_PHI           "Source Azimuthal Angle in SKY frame"

#define CLNM_DELTAX_CEN         "DELTAX_CEN"
#define CARD_COMM_DELTAX_CEN    "Aperture Stop - Optical Axis X distance in OB frame"

#define CLNM_DELTAY_CEN         "DELTAY_CEN"
#define CARD_COMM_DELTAY_CEN    "Aperture Stop - Optical Axis Y distance in OB frame"

#define CLNM_RADIUS             "RADIUS"

#define CLNM_APERTURE           "APERTURE"

#define CLNM_GHOSTRAYS          "GHOST_RAYS"

#define CLNM_REEF               "REEF"
#define CLNM_RAD_LO             "RAD_LO"
#define CLNM_AZIMUTH            "AZIMUTH"
#define CLNM_ENERG_LO           "ENERG_LO"
#define CLNM_ENERG_HI           "ENERG_HI"
#define CLNM_THETA              "THETA"
#define CLNM_VIGNET             "VIGNET"
#define CLNM_SPECRESP           "SPECRESP"

#define CLNM_DURATION		"DURATION"
#define CARD_COMM_DURATION      "Temporal duration of the bin"

#define CLNM_X_DET1		 "X_DET1"
#define CARD_COMM_X_DET1         "Detector Reference Point X position (SKY Frame)"

#define CLNM_Y_DET1		 "Y_DET1"
#define CARD_COMM_Y_DET1         "Detector Reference Point Y position (SKY Frame)"

#define CLNM_DET2X_APSTOP	 "DET2X_APSTOP"
#define CARD_COMM_DET2X_APSTOP   "Aperture Stop Center DET2X position (OB Frame)"

#define CLNM_DET2Y_APSTOP	 "DET2Y_APSTOP"
#define CARD_COMM_DET2Y_APSTOP   "Aperture Stop Center DET2Y position (OB Frame)"

#define CLNM_X_APSTOP		 "X_APSTOP"
#define CARD_COMM_X_APSTOP       "Aperture Stop Center X position (SKY Frame)"

#define CLNM_Y_APSTOP		 "Y_APSTOP"
#define CARD_COMM_Y_APSTOP       "Aperture Stop Center Y position (SKY Frame)"

#define CLNM_X_OFFSET            "X_OFFSET"
#define CARD_COMM_X_OFFSET       "Offset in the X direction"

#define CLNM_Y_OFFSET            "Y_OFFSET"
#define CARD_COMM_Y_OFFSET       "Offset in the Y direction"

#define CLNM_X_BIN               "X_BIN"
#define CARD_COMM_X_BIN          "Offset bin (X direction)"

#define CLNM_Y_BIN               "Y_BIN"
#define CARD_COMM_Y_BIN          "Offset bin (Y direction)"

#define CLNM_TSTART              "TSTART"
#define CLNM_TSTOP               "TSTOP"

#define CLNM_REF_TIME            "REF_TIME"
#define CARD_COMM_REF_TIME       "Reference time of the bin"

#define CLNM_CLC                 "CLC"
#define CLNM_GR_SLOPE            "GR_SLOPE"
#define CLNM_GR_OFFSET           "GR_OFFSET"

#define CLNM_ELOW                "ELOW"
#define CLNM_EHIGH               "EHIGH"

#define CLNM_E1                  "E1"
#define CLNM_E2                  "E2"
#define CLNM_E3                  "E3" 

#define CLNM_XEXTENT             "XEXTENT"
#define CLNM_YEXTENT             "YEXTENT"

#define CLNM_RMFFILE             "RMFFILE"

#define CLNM_BASELINE1           "BASELINE1"
#define CLNM_PI_BASELINE         "PI_BASELINE"
#define CLNM_BASELINE2           "BASELINE2"
#define CLNM_PRIOR               "PRIOR"
#define CLNM_PRIOR1              "PRIOR1"
#define CLNM_PRIOR2              "PRIOR2"
#define CLNM_PRIOR3              "PRIOR3"
#define CLNM_PI_PRIOR            "PI_PRIOR"
#define CLNM_RESET               "RESET"
#define CLNM_RESET1              "RESET1"
#define CLNM_RESET2              "RESET2"
#define CLNM_RESET3              "RESET3"
#define CLNM_PI1_RESET           "PI1_RESET"
#define CLNM_PI2_RESET           "PI2_RESET"

#define CLNM_SAA                 "SAA"
#define CLNM_OCCULTED            "OCCULTED"
#define CLNM_SLEW                "SLEW"
#define CLNM_GEODETIC            "GEODETIC"

#define CLNM_SW_SAA              "SW_SAA"
#define CARD_COMM_SW_SAA         "ground software SAA flag"
#define CLNM_SW_TENTACLE         "SW_TENTACLE"
#define CARD_COMM_SW_TENTACLE    "ground software SAA tentacle flag"

#define CLNM_RATE                "RATE"
#define CLNM_RATE_ORIG           "RATE_ORIG"
#define CARD_COMM_RATE_ORIG      "Original bin count rate"
#define CLNM_ERROR               "ERROR"
#define CLNM_ERROR_ORIG          "ERROR_ORIG"
#define CARD_COMM_ERROR_ORIG     "Original bin count rate error"

#define CLNM_Q_CHU4SC            "Q_CHU4SC"

#define CLNM_DETABS              "DETABS"


/*---------------------------------------------------------------*/
/*                        FILE NAME SUFFIXS                      */
/*---------------------------------------------------------------*/

#define EXT_FITS_BP               "_bp.fits"
#define EXT_FITS_HP               "_hp.fits"
#define EXT_FITS_PSD              "_psd.fits"
#define EXT_FITS_PSDCORR          "_psdcorr.fits"
#define EXT_FITS_MAST             "_mast.fits"

#endif

