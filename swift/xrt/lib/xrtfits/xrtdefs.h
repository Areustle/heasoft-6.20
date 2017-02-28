/*
   xrtdefs.h  

   DESCRIPTION:
         This module provides a collection of definitions and macros
         useful for XRT FITS Files Files handle. 
      
   CHANGE HISTORY:
	 0.1.0: -               - First version
	 0.1.1: - AB 17/05/2006 - KWVL_EXTNAME_IMBAD set to IMBADPIX (was: PCBADPIX)
	 0.1.2: - AB 25/05/2006 - CARD_COMM_NEWWM1STCOL added for use in xrttimetag
	 0.1.3: - AB 26/05/2006 - PC_BAD_FRAME_BORDERS added for use in xrtinstrmap
	 0.1.4: - NS 31/10/2006 - KWVL_EXTNAME_ATT, KWNM_ATTFLAG, CARD_COMM_ATTFLAG
	                          DEFAULT_ATTFLAG  added for use in xrttimetag
	 0.1.5: - NS 02/07/2007 - KWNM_XRTVSUB and DEFAULT_XRTVSUB added
	                        - Added column names for threshold file
	                        - Added KWVL_EXTNAME_XRTVSUB
	 0.1.6: - NS 19/03/2008 - KWNM_RAWXCEN, CARD_COMM_RAWXCEN, KWNM_RAWYCEN and
	                          CARD_COMM_RAWYCEN added
	 0.1.7: - NS 05/03/2009 - KWNM_XPITYPE, CARD_COMM_XPITYPE, CLNM_PI_NOM
	                          and CARD_COMM_PI_NOM added
	 0.1.8: - NS 30/04/2009 - KWNM_XSTDPHAS, CARD_COMM_XSTDPHAS, KWNM_XPHASCO, CARD_COMM_XPHASCO,
 	                          CLNM_PHASO, CARD_COMM_PHASO, CLNM_TLMPHAS and CARD_COMM_TLMPHAS added
	                        - KWVL_EXTNAME_PHASCONF, CLNM_PHASCONF_TIME and CLNM_PHASCONF_PHASCONF added
	 0.1.9: - NS 28/03/2011 - WT_PSF_COEF and PC_PSF_COEF added
	 0.2.0: - RF 08/10/2015 - KWNM_XBADFRAM, CARD_COMM_XBADFRAM added


  
   DOCUMENTATION:
 	        [1] Definition of the Flexible Image Transport System, NOST 100-1.0
 		[2] FITSIO User Manual
 
*/                    

#define DF_NONE                        "NONE"
#define DF_DEFAULT                     "DEFAULT"

#define PHA_MIN                            0L
#define PHA_MAX                         4095L
#define PI_MIN                            0L
#define PI_MAX                          1023L
#define SAT_VAL                         4095  /* Saturation value */
#define BINTAB_ROWS                     1000
#define DEFAULT_LLD                       40  /* Default for Lower Level Discriminator */
#define CCDFRAME_MAX              4294967295U
#define CCDFRAME_MIN                       0
#define PC_TIMEDEL                       2.5 
#define PIXEL_SIZE                       2.36

#define XRTMODE_SIM                        2
#define XRTMODE_LIM                        3
#define XRTMODE_PU                         4
#define XRTMODE_LR                         5
#define XRTMODE_WT                         6
#define XRTMODE_PC                         7
#define XRTMODE_BIAS                       9


#define WAVE_WT_100                       6
#define WAVE_WT_200_1                    60
#define WAVE_WT_300                      61
#define WAVE_WT_400                      62
#define WAVE_WT_500                      63

#define WAVE_WT_200_2                   160

#define LAUNCH_SECS                122601600

#define PU_BAD_PIXS                   1850

#define TIMEDELT                         2
/*---------------------------------------------------------------*/
/*                       File Name siffix                        */
/*---------------------------------------------------------------*/


#define IMAGE_EXT                       ".img"
#define SPECTR_EXT                      ".pha"
#define EXPOMAP_EXT                     ".exp"
#define RESP_EXT                        ".rmf"
#define ANC_EXT                         ".arf"
#define LCURVE_EXT                      ".lc"
#define HTML_EXT                        ".html"
#define GIF_EXT                         ".gif"
#define HKP_EXT                         ".hkp"
#define ORBIT_EXT                       ".orbit"

/*---------------------------------------------------------------*/
/*                           physical units                      */
/*---------------------------------------------------------------*/

#define UNIT_SEC			"s"
#define UNIT_MIN			"min"
#define UNIT_MM				"mm"
#define UNIT_KM				"km"
#define UNIT_KMS			"km/s"
#define UNIT_DEG			"deg"
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

#define CARD_COMM_PHYSUNIT	        "physical unit of field"

#define AMP1                            1
#define AMP2                            2

#define PIX_SIZE_DEG           0.0006548089087209
#define PIX_SIZE_MM            0.04000000
/* ------------------------
   calibration sources 
---------------------------*/
/* center of the calibration source in detx/y 
 * 
 * Analysis performed by David Burrows using data from
 * 30 May 2003 DitL test
 * 
 */

/*

 #define CAL_SRC_CEN_1_X      35 
 #define CAL_SRC_CEN_1_Y      570
 #define CALIB_SRC_RADIUS_1   47

 #define CAL_SRC_CEN_2_X      573
 #define CAL_SRC_CEN_2_Y      561
 #define CALIB_SRC_RADIUS_2   48

 #define CAL_SRC_CEN_3_X      36
 #define CAL_SRC_CEN_3_Y      27
 #define CALIB_SRC_RADIUS_3   47

 #define CAL_SRC_CEN_4_X      576
 #define CAL_SRC_CEN_4_Y      20
 #define CALIB_SRC_RADIUS_4   44
*/
/*---------------------------------------------------------------*/
/*                         CCD dimensions                        */
/*---------------------------------------------------------------*/
#define CCD_PIXS      600
#define CCD_ROWS      600


#define CCD_CENTERX   300
#define CCD_CENTERY   300 

#define RAWX_MIN        0L
#define RAWX_MAX      599L

#define RAWY_MIN        0
#define RAWY_MAX      599

#define DETX_MIN        1
#define DETX_MAX      600
#define DETY_MIN        1
#define DETY_MAX      600
#define CENTER_DETX   300.5
#define CENTER_DETY   300.5


#define X_MIN           1L
#define X_MAX        1000L
#define Y_MIN           1L
#define Y_MAX        1000L
#define SIZE_X       1000
#define SIZE_Y       1000
#define CENTER_X     500.5
#define CENTER_Y     500.5


#define RA_MIN        0
#define RA_MAX        360
#define DEC_MIN      -90
#define DEC_MAX       90


#define KWNM_TIERRELA       "TIERRELA"
#define CARD_COMM_TIERRELA  "[s/s] Estimated relative clock rate error"

#define KWNM_TIERABSO       "TIERABSO"
#define CARD_COMM_TIERABSO  "[s] Estimated absolute clock offset error"

#define KWNM_ABERRAT      "ABERRAT"
#define CARD_COMM_ABERRAT "Has aberration been corrected for in sky coords"

#define KWNM_FOLLOWSU      "FOLLOWSU"
#define CARD_COMM_FOLLOWSU "Has the Sun position been recalculated?"


/* Photodiode timing frame dimensions */
#define PD_NPIXS               631
#define PD_NROW                602
#define FIRST_OFFSET             0       
#define LAST_OFFSET         379861    

/* Windowed timing frame dimensions  */
#define WT_NROW       600
#define WT_RAWY_MIN   0
#define WT_RAWY_MAX   599

#define WT_DETY_MIN   1
#define WT_DETY_MAX   600

#define RAWX_WT_MIN   50
#define RAWX_WT_MAX   549

#define WT_NCOL_MIN   100
#define WT_NCOL_MAX   500


#define WM1STCOL_MIN   RAWX_WT_MIN
#define WM1STCOL_MAX   250

#define DEFAULT_WM1STCOL   200
#define DEFAULT_NCOL       200

#define PC_BAD_FRAME_BORDERS   2  /* Additional rows/columns to be discarded at window's borders*/

#define PHAS_MOL        9    /* Phas molteplicity */
#define NEIGH_X  0, -1, 0, +1, -1, +1, -1, 0, +1
#define NEIGH_Y  0, -1, -1, -1, 0, 0, +1, +1, +1

/* ****************** */

#define TIME_PAR      15.    /* Time for a parallel transfer (microsec) */


/*---------------------------------------------------------------*/
/*                        COMMON KEYWORDS                        */
/*---------------------------------------------------------------*/
#define KWNM_EXTNAME		"EXTNAME"  /* name of the Bintable extension */
#define CARD_COMM_EXTNAME	"name of this binary table extension"

#define KWVL_EXTNAME_EVT	"EVENTS"   /* name of the Bintable Events 
                                             extension	                     */
#define KWVL_EXTNAME_GTI	"GTI"   /* name of the Bintable STDGTI 
                                             extension	                     */
#define KWVL_EXTNAME_STDGTI	"STDGTI"   /* name of the Bintable STDGTI */
#define KWVL_EXTNAME_SPEC	"SPECTRUM"   /* name of the SPECTRUM  extension	                     */
#define KWVL_EXTNAME_PCBAD      "PCBADPIX"   /* name of the Bintable badpixels extension                      */
#define KWVL_EXTNAME_IMBAD      "IMBADPIX"   /* name of the Bintable badpixels extension                      */

#define KWVL_EXTNAME_WTBAD      "WTBADPIX"   /* name of the Bintable badpixels extension */
#define KWVL_EXTNAME_BAD        "BADPIX"   /* name of the Bintable badpixels extension */
#define KWVL_EXTNAME_FRM        "FRAME"   /* name of the Bintable frame 
					     extension */
#define KWVL_EXTNAME_OBSH           "OBSHEAD" 
#define KWVL_EXTNAME_WTCOLOFFSET    "WTCOLOFFSET" 

#define KWVL_EXTNAME_SR         "SPECRESP"   /* name of the Bintable frame 
					     extension */

#define KWVL_EXTNAME_PCGRADES   "PCGRADES"   /* name of the Bintable grade 
					         extension */
#define KWVL_EXTNAME_WTGRADES   "WTGRADES"   /* name of the Bintable grade 
					         extension */

#define KWVL_EXTNAME_MATRIX    "MATRIX"   /* name of the Bintable matrix 
					         extension */
#define KWVL_EXTNAME_MIRROR    "EFFAREA"  
#define KWVL_EXTNAME_TRANS     "TRANSMISSION"
#define KWVL_EXTNAME_VIGCOEF   "VIG_COEF"
#define KWVL_EXTNAME_PSFCOEF   "PSF_COEF"
#define KWVL_EXTNAME_WTPSFCOEF "WT_PSF_COEF"
#define KWVL_EXTNAME_PCPSFCOEF "PC_PSF_COEF"
#define KWVL_EXTNAME_REGIONCAL "REGION_CAL"
#define KWVL_EXTNAME_REGIONFOV "REGION_FOV"
  
#define KWVL_EXTNAME_RATE       "RATE" 

#define KWVL_EXTNAME_ATT        "ATTITUDE"   /* name of the Bintable attitude extension */

#define KWNM_TELESCOP		"TELESCOP" /* mission name                   */
#define KWVL_TELESCOP		"SWIFT"
#define CARD_COMM_TELESCOP	"Telescope (mission) name"

#define KWNM_INSTRUME		"INSTRUME" /* instrument name                */
#define KWVL_INSTRUME		"XRT"	           
#define CARD_COMM_INSTRUME	"Instrument name"

#define KWNM_FILTER             "FILTER" 
#define KWVL_FILTER             "NONE"
#define CARD_COMM_FILTER        "Instrument filter"

#define KWNM_LONGSTRN           "LONGSTRN"  /*The OGIP long string convention may be used */
#define KWVL_LONGSTRN           "OGIP 1.0"
#define CARD_COMM_LONGSTRN      "The OGIP long string convention may be used"      
#define KWNM_RA_NOM             "RA_NOM"
#define KWNM_DEC_NOM            "DEC_NOM"
#define CARD_COMM_RA_NOM        "RA of nominal aspect point"
#define CARD_COMM_DEC_NOM        "Dec of nominal aspect point"

#define KWNM_RA_OBJ             "RA_OBJ"
#define KWNM_DEC_OBJ            "DEC_OBJ"
#define CARD_COMM_RA_OBJ        "[deg] RA Object"
#define CARD_COMM_DEC_OBJ       "[deg] Dec Object"

#define KWNM_ATTFLAG            "ATTFLAG"
#define CARD_COMM_ATTFLAG       "Attitude type flag"
#define DEFAULT_ATTFLAG         "100"

#define KWNM_XRTVSUB            "XRTVSUB"
#define DEFAULT_XRTVSUB         0

#define KWNM_RA_PNT             "RA_PNT"
#define KWNM_DEC_PNT            "DEC_PNT"
#define KWNM_PA_PNT             "PA_PNT"

#define CARD_COMM_RA_PNT         "[deg] RA pointing"          
#define CARD_COMM_DEC_PNT        "[deg] Dec pointing"         
#define CARD_COMM_PA_PNT         "[deg] Position angle (roll)"
/*---------------------------------------------------------------*/
/*             DATA MODES KEYWORDS  VALUES                       */
/*      (values for the KWNM_DATAMODE keyword)                   */
/*---------------------------------------------------------------*/


#define KWVL_DATAMODE_IM	"IMAGE"
#define CARD_COMM_DATAMODE_IM	"XRT CCD in Image Mode"

#define KWVL_DATAMODE_IMS	"SHORTIMA"       /* TBC */
#define CARD_COMM_DATAMODE_IMS	"XRT CCD in Short Image Mode"
#define KWVL_DATAMODE_IML	"LONGIMA"        /* TBC */
#define CARD_COMM_DATAMODE_IML	"XRT CCD in Long Image Mode"
#define KWVL_DATAMODE_PH	"PHOTON"           
#define CARD_COMM_DATAMODE_PH	"XRT CCD in PHOTON-counting Data mode"
#define KWVL_DATAMODE_BR	"BRIGHT2"           
#define CARD_COMM_DATAMODE_BR	"XRT BRIGHT2 Data mode"
#define KWVL_DATAMODE_PD	"LOWRATE"          
#define CARD_COMM_DATAMODE_PD	"XRT CCD in Low Rate Photodiode Data mode"
#define KWVL_DATAMODE_PDPU	"PILEDUP"          
#define CARD_COMM_DATAMODE_PDPU	"XRT CCD in Piled-up Photodiode Data mode"
#define KWVL_DATAMODE_TM	"WINDOWED"           
#define CARD_COMM_DATAMODE_TM	"XRT CCD in Widowed Timing mode"
#define KWVL_DATAMODE_RD	"RAWDATA"          /* TBC */
#define CARD_COMM_DATAMODE_RD	"XRT CCD Raw Data"
#define KWVL_DATAMODE_BM	"BIASMAP"             
#define CARD_COMM_DATAMODE_BM	"XRT CCD Bias Map"
#define CARD_COMM_DATAMODE	"XRT ReadOut Mode"

#define IMG_EMPTY_PIX           0
/*---------------------------------------------------------------*/
/*                  EVENT AND GTI Extension KEYWORDS             */
/*---------------------------------------------------------------*/

#define KWNM_HDUCLASS           "HDUCLASS"         /* Format conforms to OGIP/GSFC conventions     */
#define KWVL_HDUCLASS           "OGIP"        
#define CARD_COMM_HDUCLASS      "File conforms to OGIP/GSFC conventions"

#define KWNM_HDUCLAS1           "HDUCLAS1"         /* Extension contains Events                    */
#define KWVL_HDUCLAS1_E         "EVENTS"
#define KWVL_HDUCLAS1_R         "RESPONSE"
#define CARD_COMM_HDUCLAS1_E    "Extension contains Events"
#define CARD_COMM_HDUCLAS1_R    "File relates to response of instrument"

#define KWNM_HDUCLAS2           "HDUCLAS2"         /* Extension contains Events                    */
#define KWVL_HDUCLAS2_E_A       "ALL"              /* Photon Event List                            */
#define CARD_COMM_HDUCLAS2_E_A  "Extension contains all Events"

#define KWVL_HDUCLAS2_ST         "STANDARD"              /* Photon Event List                            */
#define CARD_COMM_HDUCLAS2_ST    "Standard GTI"

#define KWVL_HDUCLAS2_E_AC      "ACCEPTED"         /* Only Photon Event List                       */
#define CARD_COMM_HDUCLAS2_E_AC "Extension contains only accepted Events"

#define KWVL_HDUCLAS2_E_R       "REJECTED"         /* Only Photon Event List                       */
#define CARD_COMM_HDUCLAS2_E_R  "Extension contains only rejected Events"

#define KWVL_HDUCLAS2_S         "SPECRESP"
#define CARD_COMM_HDUCLAS2_S    "effective area data is stored"

#define KWNM_XRTFLAG            "XRTFLAG"
#define CARD_COMM_XRTFLAG	"Are events flagged (T/F)?"

#define KWNM_XRTHNFLG           "XRTHNFLG"
#define CARD_COMM_XRTHNFLG	"Flag events with neighbour bad (T/F)?"

#define KWNM_XRTMAXTP           "XRTMAXTP"
#define CARD_COMM_XRTMAXTP	"Temperature threshold to flag burned spot"

#define KWNM_XRTPHA             "XRTPHA"
#define CARD_COMM_XRTPHA	"Is PHA computed (T/F)?"

#define KWNM_XRTPI              "XRTPI"
#define CARD_COMM_XRTPI  	"Is PI computed (T/F)?"

#define KWNM_XPITYPE            "XPITYPE"  
#define CARD_COMM_XPITYPE       "PI correction type applied"

#define KWNM_XRTBPCNT           "XRTBPCNT"
#define CARD_COMM_XRTBPCNT      "# of pixels flagged"

#define KWNM_XRTPHACO           "XRTPHACO"
#define CARD_COMM_XRTPHACO	"Is PHA bias subtracted (T/F)?"

#define KWNM_XPHASCO            "XPHASCO"
#define CARD_COMM_XPHASCO       "Has PHAS been corrected on ground (T/F)?"

#define KWNM_XBADFRAM            "XBADFRAM"
#define CARD_COMM_XBADFRAM       "File with out of range RAW coord. (T/F)?"

#define KWNM_XSTDPHAS            "XSTDPHAS"
#define CARD_COMM_XSTDPHAS	"Is PHAS col in standard configuration (T/F)?"

#define KWNM_XRAWXCOR           "XRAWXCOR"
#define CARD_COMM_XRAWXCOR	"Is RAWX corrected (T/F)?"

#define KWNM_VIGNAPP            "VIGNAPP"
#define CARD_COMM_VIGNAPP	"Is vignetting correction applied (T/F)?"

#define KWNM_XRTVIGEN           "XRTVIGEN"
#define CARD_COMM_XRTVIGEN	"Energy value used for vignetting correction"

#define KWNM_XRTPSFCO           "XRTPSFCO"
#define CARD_COMM_XRTPSFCO	"Is PSF correction applied (T/F)?"

#define KWNM_XRTPSFEN           "XRTPSFEN"
#define CARD_COMM_XRTPSFEN	"Energy value used for psf correction"

#define KWNM_XRTDETXY           "XRTDETXY"
#define CARD_COMM_XRTDETXY 	"Are DETX/Y coordinates computed (T/F)?"

#define KWNM_XRTSDETX           "XRTSDETX"
#define CARD_COMM_XRTSDETX	"source detector x coordinate"

#define KWNM_XRTSDETY           "XRTSDETY"
#define CARD_COMM_XRTSDETY 	"source detector y coordinate"

#define KWNM_XRTSKYXY           "XRTSKYXY"
#define CARD_COMM_XRTSKYXY	"Are X/Y coordinates computed (T/F)?"

#define KWNM_XRTTIMES           "XRTTIMES"
#define CARD_COMM_XRTTIMES	"Are photon arrival times computed (T/F)?"

#define KWNM_XRTEVREC           "XRTEVREC"
#define CARD_COMM_XRTEVREC      "Event Recognition is already computed?"

#define KWNM_XRTNOREC           "XRTNOREC"
#define CARD_COMM_XRTNOREC      "Non reconstructed events percentage"

#define KWVL_HDUCLAS1_G         "GTI"
#define CARD_COMM_HDUCLAS1_G    "Good Time Interval" 
#define KWVL_HDUCLAS1_IM        "IMAGE"            /* Extension contains image */ 




#define KWNM_HDUVERS            "HDUVERS"
#define KWVL_HDUVERS            "1.1.0"
#define CARD_COMM_HDUVERS       "version of file format (see OGIP/92-002)"

#define KWNM_HDUDOC             "HDUDOC"
#define KWVL_HDUDOC             "OGIP memos CAL/GEN/92-002 & 92-002a"
#define CARD_COMM_HDUDOC        "Documents describing the format"

#define KWNM_HDUVERS1           "HDUVERS1"
#define KWVL_HDUVERS1           "1.0.0"
#define KWNM_HDUVERS2           "HDUVERS2"
#define KWVL_HDUVERS2           "1.1.0"   
#define CARD_COMM_HDUVERS1_2    "obsolete - included for backward compatibility"

#define KWNM_RESPFILE           "RESPFILE"
#define CARD_COMM_RESPFILE      "RMF file used to get the energies"

#define KWNM_DATAMODE		"DATAMODE"	  

#define KWNM_CREATOR		"CREATOR"	   /* Program that created this FITS file	    */
#define CARD_COMM_CREATOR       "Program that created this FITS file"

#define KWNM_TLM2FITS		"TLM2FITS"	   /* Program that created this FITS file	    */
#define CARD_COMM_TLM2FITS      "Telemetry converter version number"

#define KWNM_DATE		"DATE"		   /* FITS file creation date		            */
#define CARD_COMM_DATE		"FITS file creation date"

#define KWNM_SPLIT_TH           "XRTSPLIT"
#define CARD_COMM_SPLIT_TH      "split threshold level"

#define KWNM_SPLITTHR           "SPLITTHR"
#define CARD_COMM_SPLIT_TH      "split threshold level"

#define KWNM_XRTGRADE           "XRTGRADE"
#define CARD_COMM_XRTGRADE      "grade discriminator level"

#define KWNM_ORIGMODE           "ORIGMODE"
#define CARD_COMM_ORIGMODE      "Datamode before any conversion"

#define KWNM_BIASVAL            "XRTBIAS"
#define CARD_COMM_BIASVAL       "Bias value used to adjust PHA"

#define KWNM_EVENT_TH           "XRTEVTHR"
#define CARD_COMM_EVENT_TH      "Event threshold level"

#define KWNM_TOTEV              "XRTRAWEV"
#define CARD_COMM_TOTEV         "# of raw total events"

#define KWNM_XRTNULEV           "XRTNULEV"
#define CARD_COMM_XRTNULEV     "NULL events percentage"

#define KWNM_XRTSATEV           "XRTSATEV"
#define CARD_COMM_XRTSATEV      "Saturated events percentage"

#define KWNM_G0EVPER            "XRTG0EV"
#define CARD_COMM_G0EVPER       "Grade 0 events percentage"

#define KWNM_G3EVPER            "XRTG3EV"
#define CARD_COMM_G3EVPER       "Grade 3 events percentage"

#define KWNM_G4EVPER            "XRTG4EV"
#define CARD_COMM_G4EVPER       "Grade 4 events percentage"

#define KWNM_G7EVPER            "XRTG7EV"
#define CARD_COMM_G7EVPER       "Grade 7 events percentage"

#define KWNM_XRA_PNT            "XRA_PNT"
#define CARD_COMM_XRA_PNT       "[deg] XRT RA pointing"

#define KWNM_XDEC_PNT           "XDEC_PNT"
#define CARD_COMM_XDEC_PNT      "[deg] XRT Dec pointing"

#define KWNM_XRA_OBJ            "XRA_OBJ"
#define CARD_COMM_XRA_OBJ       "[deg] XRT RA Object"

#define KWNM_XDEC_OBJ           "XDEC_OBJ"
#define CARD_COMM_XDEC_OBJ      "[deg] XRT Dec Object"

#define KWNM_XRTDETX            "XRTDETX"
#define CARD_COMM_XRTDETX       "DETX used to tag times"

#define KWNM_XRTDETY            "XRTDETY"
#define CARD_COMM_XRTDETY       "DETY used to tag times"





/*---------------------------------------------------------------*/
/*                  EVENT Extension KEYWORDS                     */
/*---------------------------------------------------------------*/

#define KWNM_TSORTKEY           "TSORTKEY"         /* data are sorted by TIME column               */
#define KWVL_TSORTKEY           "TIME"        
#define CARD_COMM_TSORTKEY      "data are sorted by TIME column"

#define KWNM_OBS_MODE           "OBS_MODE"
#define KWVL_OBS_MODE_PG        "POINTING"        
#define CARD_COMM_OBS_MODE_PG   "stable pointing direction"
#define KWVL_OBS_MODE_S         "SLEW"        
#define CARD_COMM_OBS_MODE_S    "settling on target"
#define KWVL_OBS_MODE_ST        "SETTLING"        

#define KWNM_OBS_ID             "OBS_ID"           /* Swift FOM target number                      */
#define CARD_COMM_OBS_ID        "Observation ID"

#define KWNM_TARG_ID            "TARG_ID"           /* Swift FOM target number                      */
#define CARD_COMM_TARG_ID       "Target ID"

#define KWNM_SEG_NUM            "SEG_NUM"           /* Swift FOM target number                      */
#define CARD_COMM_SEG_NUM       "Segment number"




#define KWNM_OBJECT             "OBJECT"           /* TBD Name Format */
#define CARD_COMM_OBJECT        "Object name"

#define KWNM_WHALFWD            "WHALFWD"          /* Windowed Photon Counting half window width */
#define CARD_COMM_WHALFWD       "Window half width"
#define KWNM_WHALFHT            "WHALFHT"          /* Windowed Photon Counting half window height */
#define CARD_COMM_WHALFHT       "Window half height"

#define KWNM_LLD                "LLVLTHR"
#define CARD_COMM_LLD           "Lower level Threshold"

#define KWNM_WM1STCOL           "WM1STCOL"
#define CARD_COMM_WM1STCOL      "Windowed Mode first column"
#define CARD_COMM_NEWWM1STCOL   "xrttimetag-updated WT mode 1st col. Was: " 

#define KWNM_WMCOLNUM           "WMCOLNUM"
#define CARD_COMM_WMCOLNUM      "Windowed Mode number of columns"





/*---------------------------------------------------------------*/
/*                          GTI  KEYWORDS                        */
/*---------------------------------------------------------------*/

#define KWNM_HDUCLAS2          "HDUCLAS2"         /* File contains Good Time Intervals              */
#define KWVL_HDUCLAS2          "ALL"              /*No filtering selection*/ 
#define CARD_COMM_HDUCLAS2     "File contains Good Time Intervals" 

#define KWNM_ORIGIN	       "ORIGIN"	          /* origin of fits file         		    */
#define CARD_COMM_ORIGIN       "origin of fits file"




/*---------------------------------------------------------------*/
/*                       TIMING  KEYWORDS                        */
/*---------------------------------------------------------------*/
/*---------------------------------------------------------------*/
/*                   EVENT TIMING  KEYWORDS                      */
/*---------------------------------------------------------------*/

#define KWNM_CLOCKAPP		"CLOCKAPP"	/* Is mission time corrected for clock drift?	    */
#define KWVL_CLOCKAPP		"FALSE"
#define CARD_COMM_CLOCKAPP	"default"

#define KWNM_TASSIGN		"TASSIGN"	/* location of time assignment		            */
#define KWVL_TASSIGN		"SATELLITE"
#define CARD_COMM_TASSIGN	"time assigned by clock"

#define KWNM_DATEOBS		"DATE-OBS"	/* Date of the data start time (UT) 		    */
#define CARD_COMM_DATEOBS	"Date and time of observation start"

#define KWNM_TIMEOBS		"TIME-OBS"	/* time (hh:mm:ss) of the data start time (UT)      */
#define CARD_COMM_TIMEOBS	"time (hh:mm:ss) of the data start time (UT)"

#define KWNM_DATEEND		"DATE-END"	/* date of the data end time (UT)		    */
#define CARD_COMM_DATEEND	"date and time of observation stop"

#define KWNM_TIMEEND		"TIME-END"	/* time (hh:mm:ss) of the data end time		    */
#define CARD_COMM_TIMEEND	"time (hh:mm:ss) of the data end time"

/*---------------------------------------------------------------*/
/*               GTI AND EVENT TIMING KEYWORDS                   */
/*---------------------------------------------------------------*/


#define KWNM_TIMEREF		"TIMEREF"	/* Barycentric correction not applied to times      */
#define KWVL_TIMEREF		"LOCAL"		
#define CARD_COMM_TIMEREF       "Time reference (barycenter/local)"

#define KWNM_TIMESYS		"TIMESYS"	/* TBD Time measured from here	               	    */
#define CARD_COMM_TIMESYS       "Time system"

#define KWNM_EQUINOX		"EQUINOX"	/* TBD Time measured from here	               	    */
#define CARD_COMM_EQUINOX       "default"

#define KWNM_RADECSYS		"RADECSYS"	/* TBD Time measured from here	               	    */
#define CARD_COMM_RADECSYS      "default"


#define KWNM_TIMEUNIT		"TIMEUNIT"	/* unit for time related keywords       	    */
#define KWVL_TIMEUNIT		 UNIT_SEC
#define CARD_COMM_TIMEUNIT	"Time unit"

#define KWNM_TIMEDEL		"TIMEDEL"	/* unit for time related keywords       	    */
#define CARD_COMM_TIMEDEL	"time resolution of data (in seconds)"
#define KWNM_XRTFRTIM		"XRTFRTIM"	/* unit for time related keywords       	    */
#define CARD_COMM_XRTFRTIM	"time resolution of each frame (in seconds)"
#define KWNM_MJDREF		"MJDREF"	/* TBD MJD corresponding to  SC clock start	(1968.x)    */
#define CARD_COMM_MJDREF	"TDB MJD corresponding to  SC clock start 1968.x"
#define KWNM_MJDREFI		"MJDREFI"	/* TBD MJD corresponding to  SC clock start	(1968.x)    */
#define CARD_COMM_MJDREFI	"Reference MJD Integer part"

#define KWNM_MJDREFF		"MJDREFF"	/* TBD MJD corresponding to  SC clock start	(1968.x)    */
#define CARD_COMM_MJDREFF	"Reference MJD Fractional"
#define KWNM_MJD_OBS		"MJD-OBS"	/* Modified Julian date of the data start time 	    */
#define CARD_COMM_MJDOBS	"Modified Julian date of the data start time"

#define KWNM_TSTART		"TSTART"	/* data start time in mission time	            */
#define CARD_COMM_TSTART	"data start time in mission time " 

#define KWNM_TSTOP		"TSTOP"		/* data stop time in mission time	            */
#define CARD_COMM_TSTOP		"data stop time in mission time" 

#define KWNM_TELAPSE		"TELAPSE"	/* total time elapsed during obs.	            */
#define CARD_COMM_TELAPSE	 KWNM_TSTOP" - "KWNM_TSTART 

#define KWNM_ONTIME		"ONTIME"	/* sum of all good time intervals (s)		    */
#define CARD_COMM_ONTIME	"Sum of GTIs"

#define KWNM_XRTNFRAM		"XRTNFRAM"	/* sum of all good time intervals (s)		    */
#define CARD_COMM_XRTNFRAM	"Total frame considered"

#define KWNM_LIVETIME		"LIVETIME"	/* ONTIME adjusted for instrument response          */
#define CARD_COMM_LIVETIME      "ONTIME multiplied by DEADC"

#define KWNM_EXPOSURE		"EXPOSURE"	/* time for calculating counts/sec                  */
#define CARD_COMM_EXPOSURE      "Total exposure, with all known correction"
#define KWNM_DEADC		"DEADC"	/* time for calculating counts/sec                  */
#define CARD_COMM_DEADC         "dead time"


/*---------------------------------------------------------------*/
/*           Bintable Extensions names and numbers               */
/*---------------------------------------------------------------*/
#define PH_EVENTS_HDU			2   /* extension # of Photon Counting events table   */
#define PH_GTI_HDU			3   /* extension # of Photon Counting standard GTI table     */
/*---------------------------------------------------------------*/
/*                          COLUMNS NAME                         */
/*---------------------------------------------------------------*/
#define CARD_COMM_TLMAX       "max value for this column"
#define CARD_COMM_TLMIN     "min value for this column"



#define CLNM_TIME_RO		"ROTIME"		/* photon readout time 		             */
#define CARD_COMM_TIME_RO       "photon readout time"

#define CLNM_TIME		"TIME"		/* photon derived arrival time 		             */
#define CARD_COMM_TIME          "photon derived arrival time"

#define CLNM_ENDTIME		"ENDTIME"		/* photon derived arrival time 		             */
#define CLNM_TDHXI		"TDHXI"		/* Telemetry Data Header Index 		             */
#define CARD_COMM_TDHXI         "Telemetry Data Header Index"

#define CLNM_CCDFrame	        "CCDFrame"	/* CCD FRAME number      		             */
#define CARD_COMM_CCDFrame      "CCD FRAME number"
#define KWVL_CCDFRAME_NULL      2147483647

#define CLNM_X	                "X"	        /* projected X position of photon on sky             */
#define CARD_COMM_X             "projected X position of photon on sky"

#define CLNM_Y	                "Y"	        /* projected Y position of photon on sky             */
#define CARD_COMM_Y             "projected Y position of photon on sky"

#define CLNM_RAWX		"RAWX"		/* X position of photon in telemetry		     */
#define CARD_COMM_RAWX          "X position of photon in telemetry"

#define CLNM_RAWXTL		"RAWXTL"		/* X position of photon in telemetry		     */
#define CARD_COMM_RAWXTL        "Uncorrect WT X position of photon in telemetry"

#define CLNM_RAWY		"RAWY"		/* Y position of photon in telemetry		     */
#define CARD_COMM_RAWY          "Y position of photon in telemetry"

#define CLNM_DETX		"DETX"		/* Detector X position of photon                     */
#define CARD_COMM_DETX		"Detector X position of photon "
#define KWVL_DETNULL            -1 
#define KWVL_SKYNULL            -1 

#define CLNM_DETY		"DETY"		/* Detector Y position of photon                     */
#define CARD_COMM_DETY		"Detector Y position of photon "

#define CLNM_PHAS		"PHAS"		/* Array of Pulse Height Analyzer values	     */
#define CARD_COMM_PHAS          "Array of Pulse Height Analyzer values"

#define CLNM_PHASO	        "PHASO"	        /* */
#define CARD_COMM_PHASO         "Array of Pulse Height Analyzer on-board values"

#define CLNM_TLMPHAS	        "TLMPHAS"	        /* */
#define CARD_COMM_TLMPHAS       "Array of Pulse Height Analyzer on-board values"

#define CLNM_PHA                "PHA"
#define CARD_COMM_PHA           "Pulse Height Analyzer after event recognition" 
#define KWVL_PHANULL            -4095L

#define CARD_COMM_TNULL    "Illegal value for this column"


#define CLNM_RAWPHA                "RAWPHA"
#define CARD_COMM_RAWPHA           "PHA without bias subtracted" 

#define CLNM_EVTPHA                "EVTPHA"
#define CARD_COMM_EVTPHA           "Pulse Height Analyzer before event recognition" 

#define CLNM_BASELINE              "BaseLine"
#define CARD_COMM_BASELINE         "BASE LINE offset for pha data"
#define CLNM_R                     "R"

#define CLNM_OFFSET             "OFFSET" 
#define CARD_COMM_OFFSET        "OFFSET"
#define CLNM_PI                 "PI" 
#define CARD_COMM_PI            "Pulse Invariant"
#define KWVL_PINULL              -4095L
#define CLNM_PI_NOM             "PI_NOM" 
#define CARD_COMM_PI_NOM        "Pulse Invariant nominally corrected"


#define CLNM_GRADE              "GRADE"
#define CARD_COMM_GRADE         "Event Grade"
#define CLNM_GRADEID            "GRADEID"
#define KWVL_GRADENULL           IJ_NULL_VAL      
#define KWVL_PCGRADENULL         32
#define TOT_TIM_GRADE            18 /* total number of grades for timing */
#define TIM_GRADE_MAX            15
#define TIM_GRADE_MIN            0
#define PHO_GRADE_MAX            32
#define PHO_GRADE_MIN            0


#define CLNM_ABOVE              "PixsAbove"
#define CARD_COMM_ABOVE         "Pixels above split used"
#define CLNM_STATUS             "STATUS"
#define CARD_COMM_STATUS        "Event Quality Flag"

#define CLNM_START		"START"
#define CLNM_STOP		"STOP"
#define CARD_COMM_START         "Start Observation Time"
#define CARD_COMM_STOP          "Stop Observation Time"


#define CLNM_TYPE               "TYPE"
#define CARD_COMM_TYPE          "type of bad pixels"   
#define CLNM_XYEXTENT           "YEXTENT"
#define CARD_COMM_XYEXTENT      "lenght of badpixels occurence along the CCD"
#define CLNM_BADFLAG            "BADFLAG"
#define CARD_COMM_BADFLAG       "Bad Pixel flag"

#define CLNM_FrameHID           "FrameHID"
#define KWVL_FrameHID_NULL      2147483647


#define CLNM_RA          "RA"     
#define CLNM_Dec     	 "Dec"    
#define CLNM_Roll    	 "Roll"   
#define CLNM_InSAA   	 "InSAA"  
#define CLNM_InSafeM 	 "InSafeM"
#define CLNM_XRTAuto 	 "XRTAuto"
#define CLNM_XRTManul	 "XRTManul"
#define CLNM_XRTRed  	 "XRTRed"

#define CLNM_S2Rp1       "S2Rp1"
#define CLNM_S2Rp2       "S2Rp2"
#define CLNM_S2Rp3       "S2Rp3"

#define CLNM_LRBiasLv    "LRBiasLv"
#define CLNM_LRNoise     "LRNoise"

#define CLNM_RATE        "RATE"
#define CLNM_ERROR       "ERROR"

#define CLNM_TARGET             "TARGET"
#define KWVL_TARGET_NULL        -1

#define CLNM_ObsNum             "ObsNum"
#define KWVL_ObsNum_NULL        -1

#define CLNM_Vod1               "Vod1"
#define CLNM_Vod2               "Vod2"

#define CLNM_Vrd1               "Vrd1"
#define CLNM_Vrd2               "Vrd2"

#define CLNM_Vog1               "Vog1"
#define CLNM_Vog2               "Vog2"

#define CLNM_S1Rp1              "S1Rp1"
#define CLNM_S1Rp2              "S1Rp2"
#define CLNM_S1Rp3              "S1Rp3"

#define CLNM_R1pR               "R1pR"
#define CLNM_R2pR               "R2pR"


#define CLNM_Vgr                "Vgr"
#define CLNM_Vsub               "Vsub"
#define CLNM_Vbackjun           "Vbackjun"
#define CLNM_Vid                "Vid"
#define CLNM_Ip1                "Ip1"
#define CLNM_Ip2                "Ip2"
#define CLNM_Ip3                "Ip3"
#define CLNM_Sp1                "Sp1"
#define CLNM_Sp2                "Sp2"
#define CLNM_Sp3                "Sp3"
#define CLNM_CpIG               "CpIG"
#define CLNM_BaseLin1           "BaseLin1"
#define CLNM_BaseLin2           "BaseLin2"

#define CLNM_FSTS               "FSTS"
#define KWVL_FSTS_NULL           2147483647
#define CLNM_FSTSub             "FSTSub"
#define KWVL_FSTSub_NULL         19232
#define CLNM_FETS               "FETS"
#define KWVL_FETS_NULL           2147483647
#define CLNM_FETSub             "FETSub"
#define KWVL_FETSub_NULL         19232
#define CLNM_CCDEXPOS           "CCDExpos"
#define KWVL_CCDEXPOS_NULL       -1
#define CLNM_CCDEXPSB           "CCDExpSb"
#define KWVL_CCDEXPSB_NULL       19232
#define CLNM_EVTLLD             "EvtLLD"
#define KWVL_EVTLLD_NULL          -1

#define CLNM_PixGtLLD       "PixGtLLD"
#define KWVL_PixGtLLD_NULL   -1


#define CLNM_EvtULD             "EvtULD"
#define KWVL_EvtULD_NULL          -1
#define CLNM_PixGtULD         "PixGtULD"
#define KWVL_PixGtULD_NULL   -1
#define CLNM_SPLITTHR         "SplitThr"
#define KWVL_SPLITTHR_NULL     -1

#define CLNM_OuterThr         "OuterThr"
#define KWVL_OuterThr_NULL       -1

#define CLNM_SnglePix         "SnglePix"
#define KWVL_SnglePix_NULL    32767

#define CLNM_SngleSpl         "SngleSpl"
#define KWVL_SngleSpl_NULL     32767

#define CLNM_ThreePix         "ThreePix"
#define KWVL_ThreePix_NULL    32767
#define CLNM_FourPix          "FourPix"
#define KWVL_FourPix_NULL     32767


#define CLNM_WinHalfW          "WinHalfW"
#define KWVL_WinHalfW_NULL       -1
#define CLNM_WinHalfH          "WinHalfH"
#define KWVL_WinHalfH_NULL       -1

#define CLNM_Amp		     "Amp"		/* Align FITS buffer                    	     */
#define CARD_COMM_Amp           "Readout node number"
#define KWVL_Amp_NULL            0
#define CLNM_AMP                "AMP"	

#define CLNM_telemRow           "telemRow"
#define KWVL_telemRow_NULL         -1
#define CLNM_telemCol           "telemCol"
#define KWVL_telemCol_NULL           -1

#define CLNM_nPixels            "nPixels"
#define KWVL_nPixels_NULL       -1

#define CLNM_BASELINE              "BaseLine"
#define CARD_COMM_BASELINE         "BASE LINE offset for pha data"
#define KWVL_BASELINE_NULL       -1


#define CLNM_PixOverF           "PixOverF"
#define KWVL_PixOverF_NULL       -1
#define CLNM_PixUnder           "PixUnder"
#define KWVL_PixUnder_NULL       -1

#define CLNM_BiasExpo            "BiasExpo"
#define KWVL_BiasExpo_NULL       32767


#define CLNM_LRHPIXCT           "LRHPixCt"
#define KWVL_LRHPIXCT_NULL           -1

#define CLNM_LRBPixCt          "LRBPixCt"
#define KWVL_LRBPixCt_NULL     2147483647

#define CLNM_LRSumBPx          "LRSumBPx"
#define CLNM_LRSoSBPx          "LRSoSBPx"

#define CLNM_LRBiasPx           "LRBiasPx"
#define KWVL_LRBiasPx_NULL      -1

#define CLNM_PDBIASTHR         "PDBIASTHR"
#define KWVL_PDBIASTHR_NULL       -1

#define CLNM_PDBIASLVL          "PDBIASLVL"

#define CLNM_LDPNUM             "LDPNUM"
#define KWVL_LDPNUM_NULL        -1

#define CLNM_LDPPAGE            "LDPPAGE"
#define KWVL_LDPPAGE_NULL       -1

#define CLNM_HPIXCT             "HPixCt"
#define KWVL_HPIXCT_NULL           -1

#define CLNM_BiasPx           "BiasPx"
#define KWVL_BiasPx_NULL      -1

#define CLNM_IMBLVL             "IMBLvl"
#define KWVL_IMBLVL_NULL        -1

#define CLNM_FCWSCol            "FCWSCol"
#define KWVL_FCWSCol_NULL         -1
#define CLNM_FCWSRow            "FCWSRow"
#define KWVL_FCWSRow_NULL           -1
#define CLNM_FCWNCols           "FCWNCols"
#define KWVL_FCWNCols_NULL           -1
#define CLNM_FCWNRows           "FCWNRows"
#define KWVL_FCWNRows_NULL           -1
#define CLNM_FCWThres           "FCWThres"
#define KWVL_FCWThres_NULL          -1

#define CLNM_FrstFST            "FrstFST"
#define CLNM_FrstFSTS           "FrstFSTS"
#define CLNM_WMCOLNUM           "WMColNum"
#define CLNM_WM1STCOL           "WM1stCol"
#define CLNM_LastFST            "LastFST"
#define CLNM_LastFSTS           "LastFSTS"

#define CLNM_BROW1              "BRow1"
#define CLNM_BROW1CO            "BRow1CO"
#define CLNM_BROW1LEN           "BRow1Len"
#define CLNM_BROW1AMP           "BRow1Amp"

#define CLNM_BROW2              "BRow2"
#define CLNM_BROW2CO            "BRow2CO"
#define CLNM_BROW2LEN           "BRow2Len"
#define CLNM_BROW2AMP           "BRow2Amp"

#define CLNM_CCDTemp            "CCDTemp"

#define CLNM_XRTMode            "XRTMode"
#define KWVL_XRTMode_NULL       0
#define CLNM_PixGtULD           "PixGtULD"

#define CLNM_Settled            "Settled"
#define CLNM_In10Arcm            "In10Arcm"

#define CLNM_WaveID             "WaveID"
#define CLNM_WAVE             "WAVE"
#define KWVL_WaveID_NULL        0


#define CLNM_EVTLOST            "EVTLOST"




#define CLNM_ErpCntRt           "ErpCntRt"
#define CLNM_XPosTAM1           "XPosTAM1"
#define CLNM_YPosTAM1           "YPosTAM1"
#define CLNM_XPosTAM2           "XPosTAM2"
#define CLNM_YPosTAM2           "YPosTAM2"

#define CLNM_DNCCDTemp          "DNCCDTemp"
#define KWVL_DNCCDTemp_NULL     -1




#define CLNM_AREA              "AREA"
#define CLNM_ENERGY            "ENERGY"
#define CLNM_ENERG_HI          "ENERG_HI"
#define CLNM_ENERG_LO          "ENERG_LO"
#define CLNM_SPECRESP          "SPECRESP"
#define CLNM_TRANSMIS          "TRANSMIS"
#define CLNM_PAR0              "PAR0"
#define CLNM_PAR1              "PAR1"
#define CLNM_PAR2              "PAR2"
#define CLNM_COEF0             "COEF0"
#define CLNM_COEF1             "COEF1"
#define CLNM_COEF2             "COEF2"
#define CLNM_COEF3             "COEF3"

#define CLNM_FRACEXPO                "CORRFACT"



/*---------------------------------------------------------------*/
/*                          IMAGING KEYWORDS                     */
/*---------------------------------------------------------------*/
#define KWNM_CRVAL1		"CRVAL1"
#define CARD_COMM_CRVAL1        "Coord of X ref pixel"

#define KWNM_CRVAL2		"CRVAL2"
#define CARD_COMM_CRVAL2        "Coord of Y ref pixel"

#define KWNM_RAWXCEN		"RAWXCEN"
#define CARD_COMM_RAWXCEN       "RAWX CENter image"

#define KWNM_RAWYCEN		"RAWYCEN"
#define CARD_COMM_RAWYCEN       "RAWY CENter image"

#define KWNM_NAXIS		"NAXIS"
#define KWNM_NAXIS1		"NAXIS1"
#define CARD_COMM_NAXIS1        "length of data axis 1"

#define KWNM_NAXIS2		"NAXIS2"
#define CARD_COMM_NAXIS2        "length of data axis 2"

#define KWNM_BLANK		"BLANK"
#define CARD_COMM_BLANK         "Null pixel value"

#define KWNM_CRPIX1		"CRPIX1"
#define CARD_COMM_CRPIX1        "X axis reference pixel"

#define KWNM_CRPIX2		"CRPIX2"
#define CARD_COMM_CRPIX2        "Y axis reference pixel"

#define KWNM_CDELT1		"CDELT1"
#define CARD_COMM_CDELT1        "X axis increment"

#define KWNM_CDELT2		"CDELT2"
#define CARD_COMM_CDELT2        "Y axis increment"

#define KWNM_CTYPE1		"CTYPE1"
#define CARD_COMM_CTYPE1        "X Coordinate type"
#define KWVL_CTYPE1_RAW         "RAWX"
#define KWVL_CTYPE1_DET         "DETX"
#define KWVL_CTYPE1_SKY         "RA---TAN"

#define KWNM_CTYPE2		"CTYPE2"
#define CARD_COMM_CTYPE2        "Y Coordinate type"
#define KWVL_CTYPE2_RAW         "RAWY"
#define KWVL_CTYPE2_DET         "DETY"
#define KWVL_CTYPE2_SKY         "DEC--TAN"
#define KWNM_CUNIT1             "CUNIT1"
#define KWNM_CUNIT2             "CUNIT2"
#define CARD_COMM_CUNIT         "WCS axis unit"


#define KWNM_AMPNUM             "AMPNUM"
#define KWNM_AMP                "AMP"


/*---------------------------------------------------------------*/
/*                              GAIN FILE                        */
/*---------------------------------------------------------------*/

/*
 *   KEYWORDS
 */ 
#define KWVL_EXTNAME_PH2PI    "GAIN"
#define KWNM_NOM_GAIN         "NOM_GAIN"

/*
 *   COLUMNS
 */
#define CLNM_GC0              "GC0"
#define CLNM_GC1              "GC1"
#define CLNM_GC2              "GC2"
#define CLNM_GC3              "GC3"
#define CLNM_GC4              "GC4"
#define CLNM_GC5              "GC5"
#define CLNM_CCDTEMP          "CCDTEMP"

#define GAINCOEFF_NUM              6        /* */

/*---------------------------------------------------------------*/
/*                              BIAS FILE                        */
/*---------------------------------------------------------------*/

/*
 *   KEYWORDS
 */
#define KWVL_EXTNAME_BIAS          "BIAS"

/*
 *   COLUMNS
 */
#define CLNM_BIAS                  "BIAS"

/* DS keywords */

#define KWNM_MTYPE      "MTYPE"           /* Data type */
#define KWNM_MFORM      "MFORM" /* names of the start and stop columns */
#define KWNM_METYP      "METYP" /* Data descriptor type: Range, binned data */

#define CARD_COMM_MTYPE "Data type"

/* DM keywords */
#define KWNM_DSTYP         "DSTYP"
#define KWNM_DSFORM        "DSFORM"
#define KWNM_DSVAL         "DSVAL" 

#define CARD_COMM_DSTYP         "Data subspace descriptor: name"
#define CARD_COMM_DSFORM        "Data subspace descriptor: datatype"
#define CARD_COMM_DSVAL         "Data subspace descriptor: value"



/* ------------------------------------- */
/*              PHA keywords             */
/* ------------------------------------- */

#define  KWNM_X_OFFSET   "X-OFFSET"
#define  KWNM_Y_OFFSET   "Y-OFFSET"

#define  KWNM_SRC_DETX   "SRC_DETX"
#define  KWNM_SRC_DETY   "SRC_DETY"


#define  KWNM_BIASONBD   "BIASONBD"

/*---------------------------------------------------------------*/
/*                              THRESHOLDS FILE                        */
/*---------------------------------------------------------------*/

/*
 *   KEYWORDS
 */ 
#define KWVL_EXTNAME_XRTVSUB    "XRTVSUB"

/*
 *   COLUMNS
 */
#define CLNM_XRTVSUB         "XRTVSUB"
#define CLNM_PCEVENT         "PCEVENT"
#define CLNM_PCSPLIT         "PCSPLIT"
#define CLNM_WTEVENT         "WTEVENT"
#define CLNM_WTSPLIT         "WTSPLIT"
#define CLNM_PDEVENT         "PDEVENT"
#define CLNM_PDSPLIT         "PDSPLIT"
#define CLNM_PCEVTTHR        "PCEVTTHR"
#define CLNM_PCSPLITTHR      "PCSPLITTHR"
#define CLNM_WTBIASTH        "WTBIASTH"
#define CLNM_PDBIASTH        "PDBIASTH"

/*---------------------------------------------------------------*/
/*                              PHASCONF FILE                        */
/*---------------------------------------------------------------*/

/*
 *   KEYWORDS
 */ 
#define KWVL_EXTNAME_PHASCONF       "PHASCONF"

/*
 *   COLUMNS
 */

#define CLNM_PHASCONF_TIME           "TIME"
#define CLNM_PHASCONF_PHASCONF       "PHASCONF"

