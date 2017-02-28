/*
 *	xrtdasversion.c: --- routines to retrive packages versions
 *                           for FITS History ---
 *
 *	DESCRIPTION:
 *              A complete history of SWXRTDAS versions (see the
 *              XRTDAS HISTORY at the GetSWXRTDASVersion function.
 *              The XRT Data Analysis Software Version Numbering
 *              scheme depends on the Software Development Stage.
 *              During Software Development the scheme mean is:
 *                0.x.y
 *                ^ ^ ^
 *                | | |--> Software tounovers for SMR/SPR (the number
 *                | |      can take two digit if needed)
 *                | |----> Software Build Release (Build 1-5)
 *                |------> Always ZERO before the last Build.
 *                  
 *              After the first Complete release:
 *                x.y.z
 *                ^ ^ ^
 *		  | | |--> minor internal change invisible to the
 *	          | |      user
 *	          | +----> change that is visible to the user or
 *		  |        z-overflow
 *		  |         * new or changed functionality
 *		  |         * changed format of I/O files
 *		  |         * changed user interface
 *		  +------> major revision change or y-overflow - starting
 *                         from 1.
 *
 *
 *      FUNCTIONS:
 *              void GetSWXRTDASVersion(Version_t *) 
 *                                  this funcion returns the SWXRTDAS version.
 *              void GetFTOOLSVersion(Version_t *)
 *                                  this funcion returns the FTOOLS version
 *                                  set when the program is running.
 *              void GetHEADASVersion(Version_t *)
 *                                  this funcion returns the HEADAS version
 *                                  set when the program is running.
 *
 *	CHANGE HISTORY:
 *              0.1.0 - 06/03/2002 - FT First release: swxrtdas_26May02_v0.1.0
 *              0.1.1 - 12/04/2002 - swxrtdas_12Apr02_v0.1.1
 *              0.1.2 - 17/04/2002 - swxrtdas_18Apr02_v0.1.2
 *              0.1.3 - 23/04/2002 - swxrtdas_03May02_v0.1.3
 *              0.2.0 - 25/07/2002 - swxrtdas_25Jul02_v0.2.0
 *              0.2.1 - 30/08/2002 - swxrtdas_30Aug02_v0.2.1
 *              0.4.0 - 25/02/2003 - swxrtdas_25Feb03_v0.4.0
 *              0.5.0 - 15/05/2003 - swxrtdas_15May03_v0.5.0
 *              0.6.0 - 05/08/2003 - swxrtdas_05Aug03_v0.6.0
 *              0.7.0 - 28/11/2003 - swxrtdas_28Nov03_v0.7.0
 *              0.8.0 - 14/05/2004 - swxrtdas_17May04_v0.8.0
 *              0.8.1 - 28/05/2004 - swxrtdas_28May04_v0.8.1
 *              0.8.2 - 25/06/2004 - swxrtdas_25Jun04_v0.8.2
 *              0.8.3 - 06/07/2004 - swxrtdas_06Jul04_v0.8.3
 *              0.9.0 - 09/08/2004 - swxrtdas_09Aug04_v0.9.0
 *              0.9.1 - 01/09/2004 - swxrtdas_01Sep04_v0.9.1
 *              1.0.0 - 15/10/2004 - swxrtdas_08Oct04_v1.0.0
 *              1.0.1 - 19/10/2004 - swxrtdas_19Oct04_v1.0.1
 *              1.1.0 - 16/12/2004 - swxrtdas_16Dec04_v1.1.0
 *              1.2.0 - 11/02/2005 - swxrtdas_11Feb05_v1.2.0
 *              1.2.1 - 18/02/2005 - swxrtdas_18Feb05_v1.2.0
 *              1.2.2 - 04/03/2005 - swxrtdas_04Mar05_v1.2.2
 *              1.4.0 - 24/03/2005 - swxrtdas_24Mar05_v1.4.0
 *              1.5.0 - 22/06/2005 - swxrtdas_22Jun05_v1.5.0
 *              1.5.1 - 15/07/2005 - swxrtdas_15Jul05_v1.5.1
 *              1.5.2 - 21/07/2005 - swxrtdas_21Jul05_v1.5.2
 *              1.6.0 - 16/09/2005 - swxrtdas_16Sep05_v1.6.0
 *              1.7.0 - 28/10/2005 - swxrtdas_03Nov05_v1.7.0
 *              1.7.1 - 15/11/2005 - swxrtdas_15Nov05_v1.7.1
 *              1.8.0 - 17/03/2006 - swxrtdas_17Mar06_v1.8.0
 *              1.9.0 - 05/06/2006 - swxrtdas_05Jun06_v1.9.0
 *              1.9.1 - 21/09/2006 - swxrtdas_05Jun06_v1.9.1
 *              2.0.0 - 08/11/2006 - swxrtdas_08Nov06_v2.0.0
 *              2.0.1 - 20/11/2006 - swxrtdas_20Nov06_v2.0.1
 *              2.1.0 - 15/05/2007 - swxrtdas_15May07_v2.0.2
 *              2.1.1 - 01/06/2007 - swxrtdas_01Jun07_v2.1.1
 *              2.1.2 - 14/06/2007 - swxrtdas_14Jun07_v2.1.2
 *              2.2.0 - 16/07/2007 - swxrtdas_16Jul07_v2.2.0
 *              2.2.1 - 14/09/2007 - swxrtdas_14Sep07_v2.2.1
 *              2.2.2 - 19/11/2007 - swxrtdas_19Nov07_v2.2.2
 *              2.3.0 - 14/05/2008 - swxrtdas_14May08_v2.3.0
 *              2.4.0 - 17/10/2008 - swxrtdas_17Oct08_v2.4.0
 *              2.4.1 - 17/12/2008 - swxrtdas_17Dec08_v2.4.1
 *              2.4.2 - 20/03/2009 - swxrtdas_20Mar09_v2.4.2
 *              2.4.3 - 08/05/2009 - swxrtdas_08May09_v2.4.3
 *              2.5.0 - 15/07/2009 - swxrtdas_15Jul09_v2.5.0
 *              2.5.1 - 30/10/2009 - swxrtdas_30Oct09_v2.5.1
 *              2.6.0 - 27/07/2010 - swxrtdas_27Jul10_v2.6.0
 *              2.7.0 - 01/04/2011 - swxrtdas_01Apr11_v2.7.0
 *              2.8.0 - 10/02/2012 - swxrtdas_10Feb12_v2.8.0
 *              2.9.0 - 24/04/2013 - swxrtdas_24Apr13_v2.9.0
 *              2.9.1 - 12/06/2013 - swxrtdas_12Jun13_v2.9.1
 *              2.9.2 - 20/01/2014 - swxrtdas_20Jan14_v2.9.2
 *              2.9.3 - 20/01/2014 - swxrtdas_19Feb14_v2.9.3
 *              3.0.0 - 20/06/2014 - swxrtdas_20Jun14_v3.0.0
 *              3.1.0 - 19/06/2015 - swxrtdas_19Jun15_v3.1.0
 *              3.1.1 - 17/07/2015 - swxrtdas_17Jul15_v3.1.1
 *              3.2.0 - 22/01/2016 - swxrtdas_22Jan16_v3.2.0
 *              3.3.0 - 18/10/2016 - swxrtdas_18Oct16_v3.3.0
 *              
 *
 *	AUTHOR	:
 *		ISAC Italian Swift Archive Center (FRASCATI)xrtdasversion.c
 */

#define XRTDASVERSION_C
#define XRTDASVERSION_VERSION	"3.3.0"


			/********************************/
			/*        header files          */
			/********************************/

#include "xrtdasversion.h"

			/********************************/
			/*           globals            */
			/********************************/

#define STR_LEN 125
			/********************************/
			/*       internal functions     */
			/********************************/

			/********************************/
			/*       external functions     */
			/********************************/

/*
 *	SYNOPSIS:
 *		void GetSWXRTDASVersion(Version_t verstr)
 *      XRTDAS HISTORY:
 *
 *      v.0.1.0 - 15/03/02 BUILD 1 release
 *      v.0.1.1 - 12/04/02 - SPR_XRT_001 - Fatal Compilation Error on
 *                                         Solaris 
 *                           modules affected:
 *                           xrt/lib/xrtmisc/lowlevel.h
 *                           xrt/lib/xrtmisc/lowlevel.c
 *      v.0.1.2 - 17/04/02 - SPR_XRT_002 - Compilation and run-time problems 
 *                           on DEC Alpha machines: 
 *                            * Compilation and Floating Exception during 
 *                              'xrtflagpix' run on alpha OSF4
 *                           modules affected:
 *                           xrt/tasks/xrtflagpix/xrtflagpix.c
 *                           xrt/tasks/xrtflagpix/xrtflagpix.h
 *                            * Compilation Error during 'ut-xrt001'
 *                              run on alpha OSF4
 *                           modules affected:
 *                           xrt/ut/build1/ut-xrt001.in
 *      v.0.1.3 - 23/05/02 - SPR_XRT_003 - build error
 *                           The Makefiles for xrtph2br and xrtflagpix contain
 *                           explicit dependencies on system header files 
 *                           (pil.h and/or fitsio.h). In the context of a
 *                            full, top-down build dependency fails.
 *                           modules affected:
 *                           xrt/tasks/xrtflagpix/Makefile
 *                           xrt/tasks/xrtph2br/Makefile
 *      v.0.2.0 - 15/07/02 BUILD 2 release
 *                        -- minor changes on modules:
 *                         xrt/tasks/xrtph2br/xrtph2br.c
 *                         xrt/tasks/xrtflagpix/xrtflagpix.c
 *                        -- removed wrapper from C to FORTRAN
 *                         modules affected:
 *                         xrt/lib/highfits/basic.h
 *                         xrt/lib/highfits/highfits.c
 *                         xrt/lib/highfits/highfits.h
 *                         deleted modules:
 *                         xrt/lib/highfits/cfitsio.h
 *                         xrt/lib/highfits/cfitsio_ftools.h
 *                         xrt/lib/highfits/pctype.h
 *                         xrt/lib/highfits/xrtfitsio.h
 *                        -- handle compressed files
 *                         modules affected:
 *                         xrt/lib/xrtmisc/misc.c
 *                        -- handle Upper and Lower case for input
 *                           defaults: NONE, DEFAULT, CALDB
 *                         modules affected:
 *                         xrt/tasks/xrtflagpix/xrtflagpix.c
 *                         xrt/tasks/xrtflagpix/xrtflagpix.h
 *                        -- customized perl scripts to use pquery2 in place
 *                         of "getparam.pl" utiles.
 *                         modules affected:
 *                         xrt/tasks/xrtpipeline/Makefile
 *                         deleted modules:
 *                         xrt/ut/build1/ut-xrt001.in
 *                         xrt/tasks/xrtpipeline/xrtpipeline.in
 *                         xrt/ut/Makefile
 *                         xrt/ut/ut-xrtlib.in
 *                         new modules:
 *                         xrt/ut/build1/Makefile
 *                         xrt/ut/build1/ut-xrt001
 *                         xrt/ut/ut-xrtlib
 *                         xrt/tasks/xrtpipeline/xrtpipeline
 *                        -- new tasks 'ut-xrt002' 'xrtcalcpi'
 *                         xrt/ut/build2/Makefile
 *                         xrt/ut/build2/ut-xrt002
 *                         xrt/tasks/xrtcalcpi/Makefile
 *                         xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                         xrt/tasks/xrtcalcpi/xrtcalcpi.h
 *                         xrt/tasks/xrtcalcpi/xrtcalcpi.html
 *                         xrt/tasks/xrtcalcpi/xrtcalcpi.par
 *                        -- update xrtpipeline to handle new tasks
 *                           'coordinator', 'calcpi' and generate 
 *                           new products
 *                         modules affected:
 *                         xrt/tasks/xrtpipeline/xrtpipeline
 *                         xrt/tasks/xrtpipeline/xrtpipeline.par
 *      v.0.2.1 - 30/08/02 - SPR_XRT_004 - ftchecksum call error 
 *                        -- removed the "ok" parameter from ftchecksum call
 *                         modules affected:
 *                         xrt/tasks/xrtpipeline/xrtpipeline
 *      v.0.4.0 - 25/02/03 BUILD 4 release
 *                        -- Added column "unsigned 1 byte integer" 
 *                           Type handle
 *                         module affected:
 *                         xrt/lib/highfits/highfits.c
 *                        -- Modified CALDB functions to locate filename
 *                           by observation date and time
 *                         modules affected:
 *                          xrt/lib/xrtcaldb/CalGetFileName.c
 *                          xrt/lib/xrtcaldb/CalGetFileName.h
 *                          xrt/lib/xrtcaldb/CalQueryDataBase.c
 *                          xrt/lib/xrtcaldb/xrtcaldb.h
 *                        -- Added functions to locate suitable CALDB 
 *                           file name
 *                         modules affected:
 *                         xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                         xrt/tasks/xrtcalcpi/xrtcalcpi.h
 *                         xrt/tasks/xrtflagpix/xrtflagpix.c
 *                         xrt/tasks/xrtflagpix/xrtflagpix.h
 *                       -- Added TZERO and TSCALE handle
 *                         modules affected:
 *                         xrt/lib/highfits/highfits.h
 *                         xrt/lib/highfits/highfits.c
 *                       -- Updated Makefiles following
 *                          new HEASARC Makefile system
 *                          modules affected:
 *                          all Makefiles
 *                       -- Updated to handle input ASCII filenames list
 *                         modules affected:
 *                         xrt/tasks/xrtflagpix/xrtflagpix.c
 *                         xrt/tasks/xrtflagpix/xrtflagpix.h
 *                         xrt/tasks/xrtph2br/xrtph2br.c
 *                         xrt/tasks/xrtph2br/xrtph2br.h
 *                       -- Added Windowed Photon Counting and
 *                          Windowed Timing readout mode handle
 *                         modules affected:
 *                         xrt/tasks/xrtflagpix/xrtflagpix.c
 *                         xrt/tasks/xrtflagpix/xrtflagpix.h
 *                       -- new task 'xrttimetag'
 *                         xrt/tasks/xrttimetag/Makefile
 *                         xrt/tasks/xrttimetag/xrttimetag.c
 *                         xrt/tasks/xrttimetag/xrttimetag.h
 *                         xrt/tasks/xrttimetag/xrttimetag.html
 *                         xrt/tasks/xrttimetag/xrttimetag.par
 *                       -- added handle of X type columns of all
 *                         multiplicity (nX) and a routine to check
 *                         if a X type elemnt bit is set.
 *                         modules affected:
 *                         xrt/lib/highfits/highfits.c
 *                         xrt/lib/highfits/highfits.h
 *                         xrt/lib/highfits/basic.h
 *                         xrt/lib/xrtfits/xrtfits.c
 *                         xrt/lib/xrtfits/xrtfits.h
 *                         xrt/tasks/xrtflagpix/xrtflagpix.c
 *                       -- extended ReadBintable to read unsigned short
 *                         nteger and unsigned integer columns
 *                         modules affected:
 *                         xrt/lib/highfits/highfits.c
 *                         xrt/lib/highfits/highfits.h
 *                         xrt/lib/highfits/basic.h
 *                       -- new task 'xrtmkarf'
 *                         xrt/tasks/xrtmkarf/Makefile
 *                         xrt/tasks/xrtmkarf/xrtmkarf.c
 *                         xrt/tasks/xrtmkarf/xrtmkarf.h
 *                         xrt/tasks/xrtmkarf/xrtmkarf.html
 *                         xrt/tasks/xrtmkarf/xrtmkarf.par
 *                       -- new script 'xrtfilter'
 *                         xrt/tasks/xrtfilter/Makefile
 *                         xrt/tasks/xrtfilter/xrtfilter
 *                         xrt/tasks/xrtfilter/xrtfilter.html
 *                         xrt/tasks/xrtfilter/xrtfilter.par
 *                       -- new script 'xrtscreen'
 *                         xrt/tasks/xrtscreen/Makefile
 *                         xrt/tasks/xrtscreen/xrtscreen
 *                         xrt/tasks/xrtscreen/xrtscreen.html
 *                         xrt/tasks/xrtscreen/xrtscreen.par
 *                       -- new perl library
 *                         xrt/lib/xrtperl/libswxrtperl.pl
 *                       -- changes to xrtpipeline to support Build 4
 *                          and to use the xrt perl library
 *                          modules affected:
 *                         xrt/tasks/xrtpipeline/xrtpipeline
 *                         xrt/tasks/xrtpipeline/xrtpipeline.par
 *      v.0.5.0 - 15/05/03 BUILD 5 release
 *                       -- new task 'xrtevtrec'
 *                         xrt/tasks/xrtevtrec/Makefile
 *                         xrt/tasks/xrtevtrec/xrtevtrec.c
 *                         xrt/tasks/xrtevtrec/xrtevtrec.h
 *                         xrt/tasks/xrtevtrec/xrtevtrec.html
 *                         xrt/tasks/xrtevtrec/xrtevtrec.par
 *                       -- new task 'xrtimage'
 *                         xrt/tasks/xrtimage/Makefile
 *                         xrt/tasks/xrtimage/xrtimage.c
 *                         xrt/tasks/xrtimage/xrtimage.h
 *                         xrt/tasks/xrtimage/xrtimage.html
 *                         xrt/tasks/xrtimage/xrtimage.par
 *                       -- new task 'xrtpdcorr'
 *                         xrt/tasks/xrtpdcorr/Makefile
 *                         xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                         xrt/tasks/xrtpdcorr/xrtpdcorr.h
 *                         xrt/tasks/xrtpdcorr/xrtpdcorr.html
 *                         xrt/tasks/xrtpdcorr/xrtpdcorr.par
 *                       -- Added Windowed Timing and Photodiode
 *                          readout modes handle.
 *                         modules affected:
 *                         xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                         xrt/tasks/xrtcalcpi/xrtcalcpi.h
 *                         xrt/tasks/xrtcalcpi/xrtcalcpi.par
 *                       -- Added check on frame read out time. 
 *                         modules affected:
 *                         xrt/tasks/xrttimetag/xrttimetag.c
 *                         xrt/tasks/xrttimetag/xrttimetag.h
 *                       -- Added 'gti' input parameter
 *                         modules affected:
 *                         xrt/tasks/xrtscreen/xrtscreen
 *                         xrt/tasks/xrtscreen/xrtscreen.par
 *                         xrt/tasks/xrtpipeline/xrtpipeline
 *                         xrt/tasks/xrtpipeline/xrtpipeline.par
 *                       -- Added check on PHAS column format
 *                         modules affected:
 *                         xrt/tasks/xrtph2br/xrtph2br.c
 *                       -- Added output file DATASUM and CHECKSUM keywords
 *                         modules affected:
 *                         xrt/tasks/xrtmkarf/xrtmkarf.c
 *                       -- Added 'bptable' parameter and disable
 *                          the read of input Bias Map
 *                         modules affected:
 *                         xrt/tasks/xrtfilter/xrtfilter.c
 *                         xrt/tasks/xrtfilter/xrtfilter.h
 *                         xrt/tasks/xrtfilter/xrtfilter.par
 *                       --  Added screening parameters Vod1,Vod2,Vrd1,Vrd2
 *                         modules affected:
 *                         xrt/tasks/xrtfilter/xrtfilter
 *                       --  Added check on 'fkeypar' executable
 *                           and 'CheckExec' subroutine
 *                         modules affected:
 *                         xrt/lib/xrtperl/libswxrtperl.pl
 *                       -- Changed verbosity levels
 *                         modules affected:
 *                         xrt/lib/xrtmisc/xrt_termio.h
 *                       -- Added Imaging Keywords 
 *                         modules affected:
 *                         xrt/lib/xrtfits/xrtdefs.h
 *                       -- Updated CALDB Data Types
 *                         modules affected:
 *                         xrt/lib/xrtcaldb/xrtcaldb.h
 *                       -- Fixed Build 4 Solaris Bug
 *                         modules affected:
 *                         xrt/lib/highfits/highfits.c
 *                       -- changes to xrtpipeline to support Build 5
 *                          modules affected:
 *                         xrt/tasks/xrtpipeline/xrtpipeline
 *                         xrt/tasks/xrtpipeline/xrtpipeline.par
 *
 *             v.0.6.0 - 05/08/03 BUILD 6 release
 *                       -- Bug fixed on popen failure
 *                         module affected:
 *                         xrt/lib/xrtcaldb/CalQueryDataBase.c
 *                       -- Added handle of 'TNULL' value for columns of 
 *                          integer data types I and J 
 *                         module affected:
 *                         xrt/lib/highfits/highfits.c
 *                         xrt/tasks/xrtevtrec/xrtevtrec.c
 *                       -- Added photodiode readout modes, deleted 3x3 and 
 *                          modified 3x1 algorithm for Windowed readout mode
 *                          Added routines and flags to delete NULL PHA rows
 *                          and to add 'PHAS' and 'PixsAbove' columns 
 *                          Added routine to insert statistics keywords in 
 *                          output FITS file
 *                         modules affected:
 *                         xrt/tasks/xrtevtrec/Makefile
 *                         xrt/tasks/xrtevtrec/xrtevtrec.c
 *                         xrt/tasks/xrtevtrec/xrtevtrec.h
 *                         xrt/tasks/xrtevtrec/xrtevtrec.html
 *                         xrt/tasks/xrtevtrec/xrtevtrec.par
 *                         xrt/lib/xrtdefs.h
 *                       -- Added routine 'CopyRows' to use instead of
 *                          'CpRow'(obsolete)
 *                         modules affected:
 *                         xrt/lib/highfits/highfits.h
 *                         xrt/lib/highfits/highfits.c
 *                       -- New modules:
 *                         xrt/lib/xrtbpfunc/xrtbpfunc.c
 *                         xrt/lib/xrtbpfunc/xrtbpfunc.h
 *                        modules affected:
 *                         xrt/lib/Makefile
 *                         xrt/tasks/xrtflagpix.c
 *                         xrt/tasks/xrtflagpix.h
 *                       -- Added routines to flag bad pixels for Imaging
 *                          readout mode
 *                          Added handling of 'CTYPE' keywords
 *                          Nothing will be done if 'bias' is zero
 *                          Put 'XRTBPCNT' keyword in CHDU with number of 
 *                          pixels flagged
 *                          Added function to create a new bad pixels 
 *                          primary header
 *                        modules affected:
 *                          xrt/tasks/xrtimage/xrtimage.c
 *                          xrt/tasks/xrtimage/xrtimage.h
 *                          xrt/tasks/xrtimage/xrtimage.par
 *                       -- Added routine to interpolate CALDB bias values
 *                          for Photodiode readout modes 
 *                         modules affected:
 *                          xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                          xrt/tasks/xrtpdcorr/xrtpdcorr.h
 *                          xrt/tasks/xrtimage/xrtimage.c
 *                          xrt/tasks/xrtimage/xrtimage.h
 *                       -- Moved routine to read telescope definition file 
 *                          to compute DETX in Windowed Timing Mode
 *                         modules affected:
 *                          xrt/tasks/xrtflagpix/xrtflagpix.c
 *                          xrt/tasks/xrtflagpix/xrtflagpix.h
 *                          xrt/tasks/xrttimetag/xrttimetag.c
 *                          xrt/tasks/xrttimetag/xrttimetag.h
 *                       -- String allocation changes in function 
 *                          'CalQueryDataBase'
 *                         modules affected:
 *                          xrt/lib/xrtcaldb/CalQueryDataBase.c
 *                       -- Added routine to compute grade using CALDB pattern
 *                          file with XMM grade definition
 *                         modules affected:
 *                          xrt/tasks/xrtph2br/xrtph2br.c
 *                          xrt/tasks/xrtph2br/xrtph2br.h
 *                          xrt/tasks/xrtph2br/xrtph2br.par
 *                          xrt/tasks/xrtevtrec/xrtevtrec.c
 *                          xrt/tasks/xrtevtrec/xrtevtrec.h
 *                          xrt/tasks/xrtevtrec/xrtevtrec.par
 *                       -- Used HEADAS routine 'HDgtcalf' to query CALDB
 *                          Added check on date and time keywords
 *                          for format and content
 *                         modules affected:                        
 *                          xrt/lib/xrtcaldb/CalGetFileName.c  
 *                          xrt/lib/xrtcaldb/CalReadTeldef.c  
 *                          xrt/lib/xrtcaldb/CalReadTeldef.h  
 *                          xrt/lib/xrtcaldb/xrtcaldb.h  
 *                          xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                          xrt/tasks/xrtcalcpi/xrtcalcpi.h
 *                          xrt/tasks/xrtflagpix/xrtflagpix.c
 *                          xrt/tasks/xrtimage/xrtimage.c
 *                          xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                          xrt/tasks/xrtph2br/xrtph2br.c
 *                          xrt/tasks/xrtph2br/xrtph2br.h
 *                       -- deleted modules:   
 *                          xrt/lib/xrtcaldb/CalGetFileName.h  
 *                          xrt/lib/xrtcaldb/CalQueryDataBase.c 
 *                       -- Used 'hd_ran2' HEADAS routine instead of 'xrt_ran2'
 *                         modules affected:                        
 *                          xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                          xrt/tasks/xrtcalcpi/xrtcalcpi.h
 *                       -- Added name of used CALDB files in the history
 *                          of processed hdu
 *                         modules affected:
 *                          xrt/tasks/xrtflagpix/xrtflagpix.c
 *                          xrt/tasks/xrtflagpix/xrtflagpix.h
 *                          xrt/tasks/xrtimage/xrtimage.c
 *                          xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                          xrt/tasks/xrtevtrec/xrtevtrec.c
 *                          xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                          xrt/tasks/xrttimetag/xrttimetag.c
 *                          xrt/tasks/xrtmkarf/xrtmkarf.c
 *                       -- Added new input files 
 *                          Added parameters for input files
 *                          Added routine to query CALDB
 *                          Added check on supported readout mode
 *                          temporary only PHOTON COUNTING is supported.
 *                         modules affected:                        
 *                          xrt/tasks/xrtmkarf.c                          
 *                          xrt/tasks/xrtmkarf.h                          
 *                          xrt/tasks/xrtmkarf.par
 *                       -- Added Task 'xrtproducts'
 *                         modules affected:                        
 *                          xrt/tasks/xrtproducts/Makefile
 *                          xrt/tasks/xrtproducts/xrtproducts
 *                          xrt/tasks/xrtproducts/xrtproducts.par
 *                          xrt/tasks/xrtproducts/xrtproducts.html
 *                       -- Bug fixed in 'xrtfilter' and Added 'ranom' and
 *                          'decnom' input parameters
 *                         modules affected:                        
 *                          xrt/tasks/xrtfiletr/xrtfilter
 *                          xrt/tasks/xrtfiletr/xrtfilter.par
 *                       -- Minor changes
 *                         modules affected:                        
 *                          xrt/tasks/xrtscreen/xrtscreen
 *                       -- Updated xrtpipeline to handle archive directory
 *                          tree and tasks of XRTDAS Build 6 release
 *                         modules affected:                        
 *                          xrt/tasks/xrtpipeline/xrtpipeline
 *                          xrt/tasks/xrtpipeline/xrtpipeline.par
 *                       -- Changes perl library changes
 *                         modules affected:                        
 *                          xrt/lib/xrtperl/libswxrtperl.pl
 *
 *             v.0.7.0 - 28/11/03 BUILD 7 release
 *                       -- Added new input parameters to
 *                          'xrtscreen' task and the read
 *                          screening parameters from CALDB
 *                          files 
 *                         modules affected:                        
 *                          xrt/tasks/xrtscreen/xrtscreen
 *                          xrt/tasks/xrtscreen/xrtscreen.par
 *                          xrt/tasks/xrtscreen/xrtscreen.html
 *                          xrt/tasks/xrtpipeline/xrtpipeline
 *                       -- Replaced call to headas_parstamp() with HDpar_stamp()
 *                          routine suggested in Swift Build6 report 31/09/2003
 *                        modules affected:
 *                         xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                         xrt/tasks/xrtevtrec/xrtevtrec.c
 *                         xrt/tasks/xrtflagpix/xrtflagpix.c
 *                         xrt/tasks/xrtimage/xrtimage.c
 *                         xrt/tasks/xrtmkarf/xrtmkarf.c
 *                         xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                         xrt/tasks/xrtph2br/xrtph2br.c
 *                         xrt/tasks/xrttimetag/xrttimetag.c
 *                       -- Implemented new algorithm to assign event grades
 *                          for Timing Modes using 7x1 patterns
 *                          (XRT Calibration Meeting Frascati 17-18/09/2003)
 *                        modules affected:
 *                         xrt/tasks/xrtevtrec/xrtevtrec.c
 *                         xrt/tasks/xrtevtrec/xrtevtrec.h
 *                         xrt/tasks/xrtevtrec/xrtevtrec.par
 *                         xrt/lib/xrtfits/xrtdefs.h
 *                       -- Added Data Subspace Descriptor Keywords handling
 *                        modules affected:
 *                         xrt/tasks/xrtevtrec/xrtevtrec.c
 *                         xrt/tasks/xrtph2br/xrtph2br.c
 *                         xrt/lib/highfits/highfits.c
 *                       -- Modified routine to flag calibration sources
 *                        modules affected:
 *                         xrt/tasks/xrtflagpix/xrtflagpix.c
 *                         xrt/tasks/xrtflagpix/xrtflagpix.h
 *                         xrt/tasks/xrtimage/xrtimage.c
 *                         xrt/lib/xrtfits/xrtdefs.h
 *                       -- new task 'xrthkproc'
 *                         xrt/tasks/xrthkproc/Makefile
 *                         xrt/tasks/xrthkproc/xrthkproc.c
 *                         xrt/tasks/xrthkproc/xrthkproc.h
 *                         xrt/tasks/xrthkproc/xrthkproc.html
 *                         xrt/tasks/xrthkproc/xrthkproc.par
 *                       -- new task 'xrtimage2cnt'
 *                         xrt/tasks/xrtimage2cnt/Makefile
 *                         xrt/tasks/xrtimage2cnt/xrtimage2cnt.c
 *                         xrt/tasks/xrtimage2cnt/xrtimage2cnt.h
 *                         xrt/tasks/xrtimage2cnt/xrtimage2cnt.html
 *                         xrt/tasks/xrtimage2cnt/xrtimage2cnt.par
 *                       -- Used HD gti routines to handle GTI
 *                        modules affected:
 *                         xrt/tasks/xrtimage/xrtimage.c
 *                         xrt/tasks/xrtimage/xrtimage.h
 *                       -- Implemeted creation of ARF for TIMING modes
 *                        modules affected:
 *                         xrt/tasks/xrtmkarf/xrtmkarf.c
 *                       -- Added routine to compute bias value for 
 *                          LR using housekeeping file
 *                       -- Implemented fit methods to compute bias
 *                          value
 *                        modules affected:
 *                         xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                         xrt/tasks/xrtpdcorr/xrtpdcorr.h
 *                         xrt/tasks/xrtpdcorr/xrtpdcorr.par
 *                         xrt/lib/xrtfits/xrtdefs.h
 *                       -- Added routine to write or update 
 *                          GTI extension and TIME keywords
 *                        modules affected:
 *                         xrt/tasks/xrttimetag/xrttimetag.c
 *                         xrt/tasks/xrttimetag/xrttimetag.h
 *                         xrt/tasks/xrttimetag/xrttimetag.par
 *                       -- Added new lib module:
 *                         xrt/lib/xrtfunc/xrttimefunc/xrttimefunc.c
 *                         xrt/lib/xrtfunc/xrttimefunc/xrttimefunc.h
 *                        modules affected:
 *                         xrt/tasks/xrthkproc/xrthkproc.c
 *                         xrt/tasks/xrttimetag/xrttimetag.c
 *                       -- Added new lib module:
 *                         xrt/lib/xrtmisc/slatec.f
 *                         xrt/lib/xrtmisc/slatec.h
 *                         xrt/lib/xrtmisc/slatec_c.c
 *                        modules affected:
 *                         xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                       -- Added products creation for LOWRATE and WINDOWED
 *                          readout modes
 *                        modules affected:
 *                         xrt/tasks/xrtproducts/xrtproducts
 *                       -- Added new task 'xrttam'
 *                         xrt/tasks/xrttam/xrttam
 *                         xrt/tasks/xrttam/xrttam.html
 *                         xrt/tasks/xrttam/xrttam.par
 *                      -- Modified 'xrtfilter' to reade parameters
 *                         from the calibration file to generate the
 *                         makefilter ASCII configuration file
 *                        modules affected:
 *                          xrt/tasks/xrtfilter/xrtfilter
 *                          xrt/tasks/xrtfilter/xrtfilter.par
 *                      -- Changes on libxrtperl library
 *                      -- Modified 'xrtpipeline' to support 
 *                         Build 7 tasks
 *                        modules affected:
 *                          xrt/tasks/xrtpipeline/xrtpipeline
 *                          xrt/tasks/xrtpipeline/xrtpipeline.par
 *
 *           v.0.8.0 - 14/05/04 BUILD 8 release
 *
 *                     --  Added routines to compute source DETX/Y 
 *                         starting from RA and Dec
 *                     --  Implemented routines to fill DETX/Y columns
 *                         and to add and to fill only for WT X/Y columns
 *                         using "attitude" routines
 *                     --  Added parameters 'attfile', 'ra', 'dec', 'ranom'
 *                         and 'decnom' and deleted 'detx' and 'dety'
 *                         modules affected:                        
 *                          xrt/tasks/xrttimetag/xrttimetag.c
 *                          xrt/tasks/xrttimetag/xrttimetag.h
 *                          xrt/tasks/xrttimetag/xrttimetag.par
 *                          xrt/tasks/xrttimetag/xrttimetag.html
 *                          xrt/tasks/xrthkproc/xrthkproc.c
 *                          xrt/tasks/xrthkproc/xrthkproc.h
 *                          xrt/tasks/xrthkproc/xrthkproc.par
 *                     --  Added parameter 'alignfile' to 'xrtfilter' and
 *                         changed default values to 'orbmode' and 'orbname'
 *                         parameters; 
 *                         modules affected:                        
 *                          xrt/tasks/xrtfilter/xrtfilter
 *                          xrt/tasks/xrtfilter/xrtfilter.par
 *                          xrt/tasks/xrtpipeline/xrtpipeline
 *                          xrt/tasks/xrtpipeline/xrtpipeline.par
 *                     -- Modified PSF file format
 *                        modules affected:
 *                          xrt/tasks/xrtmkarf/xrtmkarf.c
 *			    xrt/tasks/xrtmkarf/xrtmkarf.h
 *			    xrt/tasks/xrtmkarf/xrtmkarf.par
 *			    xrt/tasks/xrtmkarf/xrtmkarf.html
 *                     -- Added parameter 'detx' and 'dety' 
 *                        modules affected:
 *                          xrt/tasks/xrtmkarf/xrtmkarf.c
 *			    xrt/tasks/xrtmkarf/xrtmkarf.h
 *			    xrt/tasks/xrtmkarf/xrtmkarf.par
 *		   	    xrt/tasks/xrtmkarf/xrtmkarf.html
 *                     -- Changed taskname from 'xrtph2br' to 
 *                        'xrtpcgrade'
 *                        modules added:
 *                        xrt/tasks/xrtpcgrade/xrtpcgrade.c
 *                        xrt/tasks/xrtpcgrade/xrtpcgrade.h
 *                        xrt/tasks/xrtpcgrade/xrtpcgrade.html
 *                        xrt/tasks/xrtpcgrade/xrtpcgrade.par
 *                        modules deleted:
 *                        xrt/tasks/xrtph2br/xrtph2br.c
 *                        xrt/tasks/xrtph2br/xrtph2br.h
 *                        xrt/tasks/xrtph2br/xrtph2br.html
 *                        xrt/tasks/xrtph2br/xrtph2br.par
 *                     -- Moved routine from 'xrtflagpix' to 'highfits'
 *                        modules affected:
 *                        xrt/tasks/xrtflagpix/xrtflagpix.c
 *                        xrt/tasks/xrtflagpix/xrtflagpix.h
 *                        xrt/lib/highfits/highfits.c
 *                        xrt/lib/highfits/highfits.h
 *                     -- Added routines to search hot and flickering
 *                        pixels
 *                        modules affected:
 *                        xrt/lib/xrtbpfunc/xrtbpfunc.c
 *                        xrt/lib/xrtbpfunc/xrtbpfunc.h
 *                     -- Added routines to compute the incomplete
 *                        Gamma Function
 *                        modules added:
 *                        xrt/lib/xrtmisc/xrt_gammq.c 
 *                        xrt/lib/xrtmisc/xrt_gammq.h
 *                     -- Modified GetBadPix prototype
 *                        modules affected:
 *                        xrt/lib/xrtbpfunc/xrtbpfunc.c
 *                        xrt/lib/xrtbpfunc/xrtbpfunc.h
 *                     -- Added CalGetExtNum routine
 *                        modules affected:
 *                        xrt/lib/xrtcaldb/CalGetFileName.c
 *                        xrt/lib/xrtcaldb/xrtcaldb.h
 *                     -- Deleted input files list handling
 *                        modules affected:
 *                        all
 *                     -- Modified to handle new bad pixels fits file 
 *                        format
 *                        modules affected:
 *                        xrt/tasks/xrtflagpix/xrtflagpix.c
 *                        xrt/tasks/xrtflagpix/xrtflagpix.h
 *                        xrt/tasks/xrtflagpix/xrtflagpix.html
 *                        xrt/tasks/xrtflagpix/xrtflagpix.par
 *                        xrt/tasks/xrtimage/xrtimage.c
 *                        xrt/tasks/xrtimage/xrtimage.h
 *                        xrt/tasks/xrtimage/xrtimage.html
 *                        xrt/tasks/xrtimage/xrtimage.par
 *                     -- Added 'IsThisEvDatamode' routine
 *                        modules affected:
 *                        xrt/lib/highfits/highfits.c
 *                        xrt/lib/highfits/highfits.html
 *                        xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                        xrt/tasks/xrtevtrec/xrtevtrec.c
 *                     -- Added 'history' handle; erased 'useevtrangefile' and
 *                        'usehkrangefile'; added 'cleanup' input parameter;
 *                        Make the query to CALDB using Observation start
 *                        date; Changes on input parameters
 *                        modules affected:
 *                        xrt/tasks/xrtscreen
 *                        xrt/tasks/xrtscreen.par 
 *                     -- Handle only RA/DEC input parameters for source
 *                        position and call 'aspect' on attitude file corrected
 *                        with TAM values to get Optical Axis direction;
 *                        Added 'xrttam' and 'xrthotpix' calls;
 *                        Updated calls to tasks with changes needed for
 *                        Build 8 software version; Added check on 'maketime'
 *                        input file, if only one record available create
 *                        GTI without use 'maketime'; Added cleanup input 
 *                        parameter; 
 *                        modules affected:
 *                        xrt/tasks/xrtpipeline
 *                        xrt/tasks/xrtpipeline.par
 *                     -- Modified input parameters description and some input 
 *                        parameters name
 *                        modules affetcted:
 *                        all
 *                     -- Added routine to compute housekeeping time
 *                        and 'HKTIME' column
 *                        xrt/tasks/xrthkproc/xrthkproc.c
 *                        xrt/tasks/xrthkproc/xrthkproc.h
 *                     -- Deleted modules:
 *                        xrt/lib/xrtcaldb/CalReadTeldef.c                    
 *                        xrt/lib/xrtcaldb/CalReadTeldef.h
 *
 *
 *           v.0.8.1 - 26/05/04 BUILD 8 release
 *                           
 *                     -- Implemented change requested by SDC
 *                        to change the access mode to 
 *                        MJDREF keywords checking first
 *                        MJDREFI and MKDREFF ones
 *                        modules affetcted:
 *                        xrt/lib/xrtfits/xrtdefs.h
 *                        xrt/tasks/xrttimetag/xrttimetag.c
 *                        xrt/tasks/xrttimetag/xrttimetag.h
 *                        xrt/tasks/xrthkproc/xrthkproc.c
 *                        xrt/tasks/xrthkproc/xrthkproc.h
 *                        xrt/tasks/xrttam/xrttam
 *                        xrt/tasks/xrtproducts/xrtproducts
 *
 *           v.0.8.2 - 25/06/2004
 *
 *                     -- Implemented changes on 'xrttdrss'
 *                        due to tests at GCN
 *                        modules affected:
 *                        xrt/tasks/xrttdrss/xrttdrss
 *                     -- Bugs fixed after test at SSC
 *                        (See "Build 8 XRT software failure report"
 *                         sent via mail by Mike Tripicco)
 *                        modules affected:
 *                        all except:
 *                        xrt/tasks/xrttdrss/xrttdrss
 *
 *           v.0.8.3 - 06/07/2004
 *                     -- Modified to fix gcc3.3 bug
 *                        (See "Build 8 XRT software failure report"
 *                        sent via mail by Mike Tripicco)
 *                        modules affected:
 *                        xrt/lib/xrtmisc/lowlevel.c
 *
 *           v.0.9.0 -  BUILD 9 release 
 *
 *                     -- Modified to fix endian bug on Solaris and Mac OS X
 *                        (See "Build 8 status report"
 *                        sent via mail by Mike Tripicco)
 *                        modules affected:
 *                        xrt/tasks/xrttimetag/xrttimetag.c
 *                     -- Changed BIAS datatype from J to D
 *                        modules affected:
 *                        xrt/tasks/xrtpdcorr/xrtpdcorr.c  
 *                        xrt/tasks/xrtpdcorr/xrtpdcorr.h  
 *                        xrt/tasks/xrtpdcorr/xrtpdcorr.par
 *                        xrt/tasks/xrtimage/xrtimage.c  
 *                        xrt/tasks/xrtimage/xrtimage.h  
 *			  xrt/tasks/xrtimage/xrtimage.par
 *                     -- Read Calibration sources information from
 *                        CALDB file (new input parameter 'srcfile')
 *                        and parameter 'srcfile'
 *                        modules affected:
 *                        xrt/tasks/xrtimage/xrtimage.c  
 *                        xrt/tasks/xrtimage/xrtimage.h  
 *			  xrt/tasks/xrtimage/xrtimage.par
 *                        xrt/lib/xrtfits/xrtdefs.h
 *                        new modules:
 *                        xrt/lib/xrtcaldb/ReadCalFile.c
 *                     -- Added routines to create or update 
 *                        GTI extension
 *                        modules affected:
 *                        xrt/tasks/xrtimage/xrtimage.c  
 *                        xrt/tasks/xrtimage/xrtimage.h  
 *			  xrt/tasks/xrtimage/xrtimage.par
 *                        xrt/lib/xrtfunc/xrttimefunc.c
 *                        xrt/lib/xrtfunc/xrttimefunc.h
 *                        xrt/lib/xrtfits/xrtdefs.h
 *                      -- Added routines to create or update 
 *                        BADPIX extension
 *                        modules affected:
 *                        xrt/tasks/xrtflagpix/xrtflagpix.c  
 *                        xrt/tasks/xrtflagpix/xrtflagpix.h  
 *                        xrt/tasks/xrtflagpix/xrtflagpix.par
 *                        xrt/tasks/xrthotpix/xrthotpix.c
 *                        xrt/lib/xrtbpfunc/xrtbpfunc.c  
 *                        xrt/lib/xrtbpfunc/xrtbpfunc.h
 *                     -- Added routine to search bad pixels around 
 *                        3x3
 *                        modules affected:
 *                        xrt/tasks/xrtflagpix/xrtflagpix.c  
 *                        xrt/tasks/xrtflagpix/xrtflagpix.h  
 *                        xrt/tasks/xrtflagpix/xrtflagpix.par
 *                     -- Renamed 'hkfile' parameter in 'hdfile'
 *                        modules affected:
 *                        xrt/tasks/xrthkproc/xrthkproc.c  
 *                        xrt/tasks/xrthkproc/xrthkproc.h
 *                        xrt/tasks/xrthkproc/xrthkproc.par
 *                        xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                        xrt/tasks/xrtpdcorr/xrtpdcorr.h
 *                        xrt/tasks/xrtpdcorr/xrtpdcorr.par
 *                        xrt/tasks/xrttimetag/xrttimetag.c
 *                        xrt/tasks/xrttimetag/xrttimetag.h
 *                        xrt/tasks/xrttimetag/xrttimetag.par
 *                        xrt/tasks/xrttimetag/xrttimetag.html
 *                     -- Added check on OBS_MODE keyword
 *                        modules affected:
 *                        xrt/tasks/xrtimage/xrtimage.c  
 *                        xrt/tasks/xrtimage/xrtimage.h 
 *                     -- Cahnged call to 'swifttrans' into 'swiftxform';
 *                        Removed input oarameters and added parameters;
 *                        needed for Build 9 tasks release; Added check
 *                        on ABS_MODE to doscriminate if the data are
 *                        POINTING, SLEW or SETTLING; 
 *                        Added Clean of event L1a and L2 event files by coluns no more
 *                         useful. The files cleaning is performed if the input parameter
 *                        'cleancols' is set to 'yes' 
 *                        modules affected:
 *                        xrt/tasks/xrtpipeline
 *                        xrt/tasks/xrtpipeline.par
 *             
 *           v.0.9.1 - 01/09/2004
 *
 *                   - Improved naming convention handle with the possibility
 *                     to introduce one or more characters before the two digit
 *                     level flag
 *                     Modules affected:
 *                     xrt/tasks/xrtpipeline
 *                   - Added new parameters 'calcpos', 'unit' and 'intensity'
 *                     and possibility to calculate only error radius
 *                     modules affected:
 *                     xrt/tasks/xrtcentroid
 *                     xrt/tasks/ut_xrtcentroid_im
 *                     xrt/tasks/ut_xrtcentroid_pc
 *                   - Modified keywords and keyword comments in output file
 *                     (DN -> TOTALDN  FLUX -> SRCFLUX)
 *                     modules affected:
 *                     xrt/tasks/xrttdrss
 *                   - Bug fixed when PILEDUP arf file is created
 *                     modules affected:
 *                     xrt/tasks/xrtmkarf.c
 *                             
 *             
 *           v.1.0.0 - 15/10/2004
 *
 *                   - Changes need for PD telemetry data changes
 *                     modules affected:
 *                       xrt/tasks/xrtpdcorr.c
 *                       xrt/tasks/xrtpdcorr.par
 *                   - Changed CALDB Badpixel Table  and On Ground Badpixels
 *                     query to take into account the change of the CCNM0001 value 
 *                     and a new CBD20001 keyword added to distingush 
 *                     ONBOARD badpixels from ONGROUND ones 
 *                     modules affected:
 *                       xrt/tasks/xrtflagpix/xrtflagpix.c
 *                       xrt/tasks/xrtflagpix/xrtimage.c
 *                       xrt/lib/xrtcaldb/xrtcaldb.h
 *                   - Bug fixed GetFitsPointer subroutine (this should fix the
 *                     Solaris problem given by 'xrttdrss' and 'xrttam'
 *                     tasks during Build 9).
 *                     subroutines added
 *                     modules affected:
 *                       xrt/lib/xrtperl/libswxrtperl.pl
 *                   - Added check on WS1TCOL and NTCOL keyword
 *                     and changed the default value of the WT
 *                     window dimension
 *                     modules affected:
 *                       xrt/lib/xrtfits/xrtdefs.h
 *                       xrt/tasks/xrttimetag/xrttimetag.c 
 *                       xrt/tasks/xrttimetag/xrtflagpix.c 
 *                   - Removed interpolation reading bias from CALDB
 *                     modules affected:
 *                       xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                       xrt/tasks/xrtimage/xrtimage.c     
 *                       xrt/tasks/xrttdrss/xrttdrss
 *                   - Changed CALDB query conditions and changed
 *                     RESPFILE keyword value.
 *                     modules affected:
 *                       xrt/tasks/xrtmkarf/xrtmkarf.c
 *                   - Bugs fixed
 *                     modules affected:
 *                       xrt/lib/xrtbpfunc/xrtbpfunc.c
 *                       xrt/lib/xrtfunc/xrttimefunc.c
 *                       xrt/tasks/xrtcentroid/xrtcentroid
 *                       xrt/tasks/xrtproducts/xrtproducts
 *                   - Added check on rawx/y range (only for timing) 
 *                     and set STATUS column to EV_BAD when out of range
 *                     modules affected:
 *                       xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                   - Added check on XRTTIMES keyword before 
 *                     adding the empty row. Added check on XRT OBSMODE 
 *                     and if it is in SLEW or In10arcmin
 *                     use ccd center detx/dety to tag time. Added 
 *                     warning message if source is out of ccd 
 *                     modules affected:
 *                       xrt/tasks/xrthkproc/xrthkproc.c
 *                   - Bug fixed in 'outdir' specification. The output directory
 *                     cannot be a subdirectory of the input one. Added DEFAULT 
 *                     to 'orbfile' input parameter; with DEFAULT
 *                     the pipeline searches the orbit file under the
 *                     input directory tree. Changed 'applytamcorr' default
 *                     to "no". Added warnings if the tam corrction
 *                     is requested by user and the attitude is not re-calculated
 *                     ('runaspect=no'). Changed xrtproducts input 'heigh' from 10 to 100
 *                     Bug fixed on Image Level 2 file name definition (error
 *                     found when the input file was not compressed)
 *                     modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline 
 *                   - Added parameter (prefr=0 and postfr=1) at the maketime call.
 *                     Bug fixed when no gti generation requested but only gti
 *                     file supplied by users: wrong gtifile list created. 
 *                     changed imput parameters handling. Now 'gtiexpr', 'exprgrade', 'expr'
 *                     parameters define the kind of selection to be applied.
 *                     Added check on PI colun existence.
 *                     modules affected:
 *                      xrt/tasks/xrtscreen/xrtscreen
 *                      xrt/tasks/xrtscreen/xrtscreen.par
 *                   - added XRTTAM keyword to the output file
 *                     modules affected:
 *                      xrt/tasks/xrttam/xrttam
 *                   - Added initialization of $fptr and $status to 0,
 *                     this change has been suggested by Bob Wiegand
 *                     (undefined variables give warnings with
 *                     Astro::FITS::CFITSIO library)
 *                     modules affected:
 *                       xrt/lib/xrtperl/libswxrtperl.pl
 *                       xrt/tasks/xrttam/xrttam
 *                       xrt/tasks/xrtdrss/xrtdrrss
 *                       xrt/tasks/xrtscreen/xrtscreen
 *                       xrt/tasks/xrtpipeline/xrtpipeline
 *                   - Added  check on BIASONBORD keyword to the
 *                     "Check if PHA is already computed" check
 *                     modules affected:
 *                       xrt/tasks/xrtevtrec/xrtevtrec.c
 *                       xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                   - Changed GTI calculation method for Timeing
 *                     Modes
 *                     modules affected:
 *                       xrt/tasks/xrttimetag/xrttimetag.c
 *
 *           v.1.0.1 - 19/10/2004
 *
 *                   - Bugs fixed on Alpha ("Segmentation Fault"
 *                     with xrttimetag and xrtpdcorr tasks)
 *                    modules affected:
 *                       xrt/tasks/xrttimetag/xrttimetag.c
 *                       xrt/tasks/xrttimetag/xrttimetag.h 
 *                       xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                       xrt/lib/xrtfunc/xrttimefunc.c
 *
 *           v.1.1.0 - 16/12/2004
 *
 *                    - Added CALDB query for s/c alignment teldef 
 *                      file
 *                     modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                    - Deleted parameters 'skyxnull' and 'skyynull'
 *                     modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline 
 *                    - Modified routines to build bias histogram using 
 *                      readout time
 *                     modules affected:
 *                      xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                      xrt/tasks/xrtpdcorr/xrtpdcorr.h
 *                    - Modified algorithm to calculate photon arrival 
 *                      time for WT first frame  
 *                     modules affected:
 *                      xrt/lib/xrtfunc/xrttimefunc.c
 *                      xrt/lib/xrtfunc/xrttimefunc.h
 *                      xrt/tasks/xrttimetag/xrttimetag.c
 *                      xrt/tasks/xrttimetag/xrttimetag.h
 *                      xrt/tasks/xrthkproc/xrthkproc.c
 *                      xrt/tasks/xrthkproc/xrthkproc.h
 *                    - Bugs fixed in routines to append/update
 *                      badpixels extension in event file
 *                     modules affected:
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                      xrt/tasks/xrtflagpix/xrtflagpix.h
 *                      xrt/tasks/xrthotpix/xrthotpix.c
 *                      xrt/tasks/xrthotpix/xrthotpix.h
 *                      xrt/lib/xrtbpfunc/xrtbpfunc.c
 *                      xrt/lib/xrtbpfunc/xrtbpfunc.h
 *                    - Bug fixed when a negative Dec is input
 *                     modules affected:
 *                      xrt/lib/xrtperl/libswxrtperl.pl
 *
 *           v.1.2.0 - 11/02/2005
 *           v.1.2.1 - 18/02/2005
 *           v.1.2.2 - 04/03/2005
 *
 *                   - Added check on time NULL values
 *                     modules affected:
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                   - Minor changes
 *                     modules affected:
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                      xrt/tasks/xrtflagpix/xrtflagpix.h
 *                      xrt/lib/xrtperl/libswxrtperl.pl
 *                   - improved task efficiency
 *                     modules affected:
 *                      xrt/tasks/xrttimetag/xrttimetag.c
 *                      xrt/tasks/xrttimetag/xrttimetag.h
 *           v.1.4.0 - 24/03/2005 
 *           v.1.5.0 - 05/07/2005
 *                   -- Added routine to overwrite STATUS bit of the 
 *                       searched bad pixels only
 *                     modules affected:
 *                      xrt/lib/xrtbpfunc/xrtbpfunc.c
 *                   -- Revised bad pixels bit 
 *                     modules affected:
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                      xrt/tasks/xrtflagpix/xrtflagpix.h
 *                      xrt/tasks/xrthotpix/xrthotpix.c
 *                      xrt/tasks/xrthotpix/xrthotpix.h 
 *                      xrt/lib/xrtbpfunc/xrtbpfunc.h
 *                   -- Added chance to avoid to flag calibration sources
 *                     modules affected:
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                      xrt/tasks/xrtflagpix/xrtflagpix.h
 *                   -- Added new input parameter 'trfile' and 
 *                       check on WT frame dimension to create WT GTI
 *                     modules affected:
 *                      xrt/tasks/xrttimetag/xrttimetag.c
 *                      xrt/tasks/xrttimetag/xrttimetag.h
 *                      xrt/tasks/xrttimetag/xrttimetag.par
 *                   --  Changes needed to handle version 8.9 of the 
 *                       on-board software:
 *                       - Added new input parameter 'biasdiff' and chance
 *                         to re-correct on ground lowrate and piled up 
 *                         mode files.
 *                       - Modified 'biasth' input parameter range
 *                     modules affected:
 *                      xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                      xrt/tasks/xrtpdcorr/xrtpdcorr.h
 *                      xrt/tasks/xrtpdcorr/xrtpdcorr.par
 *                      xrt/tasks/xrtimage/xrtimage.c
 *                      xrt/tasks/xrtimage/xrtimage.h
 *                      xrt/tasks/xrtimage/xrtimage.par
 *                      xrt/lib/xrtfits/xrtdefs.h
 *                   -- Added trailer file as pipeline input and 
 *                       to the xrttimetg call (needed for WT data 
 *                       processing)
 *                      Added 'allempty' and 'extempty' input parameters 
 *                       to swiftxform call and set the 'copyall' swiftxform 
 *                       input parameter to yes. Changes
 *                  	 requested by the new swiftxform task version
 *                      Set the default 'pdbiasth' to '300'. The 'old' 
 *                       value -99 cannot be used because the LLVLTHR 
 *                       keyword is no more available in the event file
 *                      Added 'pdbiasdiff' input parameter which 
 *                       corresponds to the xrtpdcorr 'biasdiff' input one. 
 *                       If the 'pdbiasdiff' parameter is set to 0 
 *                       the xrtpdcorr task does not correct the bias 
 *                       subtracted on-board
 *                      Added 'aspectgtiexpr' to allow user to specify the
 *                       espression to generate GTIs for the aspect run
 *                      Do not create the .mkf file if only images have 
 *                       to be processed for that observation and 
 *                       'imgtiecreen=no'
 *                      Changed GetAttOrbFileName Call
 *			Changed corrected HK Header output file name construction
 *			 (SetHkFileCorr subroutine)
 *			Changed Makefilter Output file Name construction
 *			Added 'imbiasdiff' and 'immaxtemp' input parameters 
 *			 (needed to the xrtimage task) and 'hdfile' to the 
 *			 xrtimage call
 *			Substituted the input parameter 'inarffile' with 
 *			 'pcinarffile', 'wtinarffile' and 'pdinarffile' to allow 
 *			 to input different ARF for different modes
 *			Added 'pcmaxtemp' input parameter (needed to the
 *			 xrtflagpix task)
 *			Added call to the 'xrtwtcorr' task and 
 *			 'wtbiasnevents' and 'wtbiasdiff' input parameters needed
 *                     modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline 
 *                   -- Added 'CheckRow' funcion taken from 'xrtscreen' task
 *                       needed also to 'xrtpipeline'
 *                     modules affected:
 *                      xrt/tasks/xrtscreen/xrtscreen
 *                      xrt/lib/xrtperl/libswxrtperl.pl
 *                   -- Added check on Filter File and HK Header file
 *                       to check if some rows have the TIME column
 *                       set to NULL before screen the file from
 *                       rows with TIME set to NULL. 'CleanTimeNulls'
 *                       subroutine added to the XRT Perl library
 *                     modules affected:
 *                      xrt/tasks/xrtscreen/xrtscreen
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtfilter/xrtfilter
 *                      xrt/lib/xrtperl/libswxrtperl.pl
 *                   -- Modified GetGtiValues to merge GTI from consecutive 
 *                      frames
 *                     modules affected:
 *                      xrt/tasks/xrttimetag/xrttimetag.c
 *                      xrt/tasks/xrttimetag/xrttimetag.h
 *                   -- Added check on 'infile' Bintable rows
 *                      and exit with warning if it is empty
 *                     modules affected:
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                      xrt/tasks/xrtflagpix/xrtflagpix.h
 *                      xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                      xrt/tasks/xrtpdcorr/xrtpdcorr.h
 *                   -- new modules
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.c
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.h
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.par
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.html
 *                   -- Added chance to flag burned spot (maxtemp!=0.0) if 
 *                      ccdtemp greater than 'maxtemp' (mew input parameter)
 *                      modules affected:
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                      xrt/tasks/xrtflagpix/xrtflagpix.h
 *                      xrt/tasks/xrtflagpix/xrtflagpix.par
 *                      xrt/lib/xrtbpfunc/xrtbpfunc.h
 *
 *
 *           v.1.5.1 - 15/07/2005
 *                 
 *                   -- Added check on 'infile' Bintable rows
 *                      and exit with warning if it is empty
 *                     modules affected:
 *                      xrt/tasks/xrttimetag/xrttimetag.c
 *                      xrt/tasks/xrttimetag/xrttimetag.h
 *                      xrt/tasks/xrtevtrec/xrtevtrec.c
 *                      xrt/tasks/xrtevtrec/xrtevtrec.h
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.h
 *                      xrt/tasks/xrtpcgrade/xrtpcgrade.c
 *                      xrt/tasks/xrtpcgrade/xrtpcgrade.h
 *                   -- Added new routine ReadBintableWithNULL to perform 
 *                      check on udefined value reading hdfile 
 *                     modules affected:
 *                       xrt/tasks/xrthkproc/xrthkproc.c
 *                       xrt/tasks/xrthkproc/xrthkproc.h
 *                       xrt/tasks/xrtimage/xrtimage.c
 *                       xrt/lib/xrtdefs.h
 *                       xrt/lib/highfits/highfits.c
 *                       xrt/lib/highfits/highfits.h
 *                       xrt/tasks/xrtwtcorr/xrtwtcorr.c
 *                       xrt/tasks/xrttimetag/xrttimetag.c
 *                       xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                       xrt/tasks/xrtpcgrade/xrtpcgrade.c
 *                       xrt/tasks/xrtflagpix/xrtflagpix.c
 *                       xrt/tasks/xrtevtrec/xrtevtrec.c
 *                       xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *
 *
 *           v.1.5.2 - 22/07/2005
 *                 
 *                   -- Change needed to avoid an error
 *                      on Cygwin platform due to an ftcalc
 *                      bug on this platform
 *                     modules affected:
 *                      xrt/tasks/xrtfilter/xrtfilter
 *                   -- Modified some TNULL values
 *                      of '*xhd*hk' unsigned columns
 *                     modules affected:
 *                      xrt/lib/xrtfits/xrtdefs.h
 *
 *           v.1.6.0 - 16/09/2005
 *
 *                   -- Exit with warning if 'infile' is empty
 *                     modules affected:
 *                      xrt/tasks/xrthotpix/xrthotpix.c
 *                      xrt/tasks/xrthotpix/xrthotpix.h
 *                   -- Exit with error if CALDB input file and/or hk file
 *                      are empty
 *                     modules affected:
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                      xrt/tasks/xrthkproc/xrthkproc.c
 *                      xrt/tasks/xrtpcgrade/xrtpcgrade.c
 *                      xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.c
 *                   -- Create output file when task exits with 
 *                      warning
 *                     modules affected:
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                      xrt/tasks/xrtevtrec/xrtevtrec.c
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                      xrt/tasks/xrtpcgrade/xrtpcgrade.c
 *                      xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                      xrt/tasks/xrttimetag/xrttimetag.c
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.c
 *                      xrt/lib/xrtmisc/xrt_termio.h
 *                      xrt/lib/xrtmisc/xrt_termio.c  
 *                   -- Update bad pixels list
 *                     modules affected:
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                      xrt/lib/xrtbpfunc/xrtbpfunc.c
 *                   -- Bug fixed checking if hd range time covers
 *                      attitude time
 *                   -- Do not consider bias map and NULL frames 
 *                      in the calculation of the TAM Detector corrections
 *                   -- Added 'alignfile' and 'interpolation' new 'attcombine'
 *                      parameters
 *                     modules affected:
 *                      xrt/tasks/xrttam/xrttam 
 *                   -- Added 'alignfile' and 'interpolation' input parameters
 *                      to 'xrttam' call
 *                   -- Added interpolation input parameter to 'coordinator' 
 *                      call
 *                     modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                   -- Correct the pixel value of Postage Stamp Images
 *                      when Message Version equal to 2 
 *                     modules affected:
 *                      xrt/tasks/xrttdrss/xrttdrss
 *
 *           v.1.7.0 - 03/11/2005
 *
 *                   -- Deleted BP_POINT bad pixels flag
 *                     modules affected:
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                   -- Added 'hotneigh' input parameter to decide if to flag
 *                      events with a hot and/or flickering pixels in the 3x3
 *                     modules affected:
 *                      xrt/tasks/xrthotpix/xrthotpix.c 
 *                      xrt/tasks/xrthotpix/xrthotpix.h
 *                      xrt/tasks/xrthotpix/xrthotpix.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                   -- Added new input parameter 'expofile'
 *                     modules affected:
 *                      xrt/tasks/xrtproducts/xrtproducts
 *                      xrt/tasks/xrtproducts/xrtproducts.par 
 *                   -- Read the FLUXFACT keyword to get the conversion factor 
 *                   -- Added 'dnthr' input parameter    
 *                     modules affected:
 *                      xrt/tasks/xrttdrss/xrttdrss.par
 *                      xrt/tasks/xrttdrss/xrttdrss
 *                   -- new modules:
 *                      xrt/tasks/xrtexpocalc/xrtexpocalc.c
 *                      xrt/tasks/xrtexpocalc/xrtexpocalc.h
 *                      xrt/tasks/xrtexpocalc/xrtexpocalc.html
 *                      xrt/tasks/xrtexpocalc/xrtexpocalc.par
 *                      xrt/tasks/xrtexpocalc/xrtexpocorr.c
 *                      xrt/tasks/xrtexpocorr/xrtexpocorr.h
 *                      xrt/tasks/xrtexpocorr/xrtexpocorr.html
 *                      xrt/tasks/xrtexpocorr/xrtexpocorr.par
 *                      xrt/tasks/xrtexpomap/xrtexpomap
 *                      xrt/tasks/xrtexpomap/xrtexpomap.html
 *                      xrt/tasks/xrtexpomap/xrtexpomap.par
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.par
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.c
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.html
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *
 *    v.1.7.1 - 11/11/2005
 *
 *                     -- Bugs fixed after test at SSC
 *                        (as reported via mail by Mike Tripicco (09/11/2005))
 *                      modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline.html
 *                      xrt/tasks/xrtpipeline/ xrtpipeline.par 
 *                      xrt/lob/xrtcaldb/CalGetFileName.c
 *                      xrt/tasks/xrtmkarf/xrtmkarf.c
 *                     -- Exit with error if datamode is Wt and 
 *                        xtr.hk file not found
 *                      modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                     -- Bug fixed on 'outfile' input parameter handling
 *                      modules affected:
 *                      xrt/tasks/xrtfilter/xrtfilter
 *                     -- Changed algorithm to recognized end of orbit
 *                        using TIME and ENDTIME xhd.hk columns
 *                      modules affected:
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.c
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.h 
 *                      xrt/lib/xrtfits/xrtdefs.h
 *                     -- Changed MJDREFI keyword datatype
 *                      modules affected:
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.c
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.h
 *                      xrt/tasks/xrtexpomap/xrtexpomap 
 *                     -- Modified standard exposure map File Name
 *                      modules affected:
 *                      xrt/lib/xrtperl/libswxrtperl.pl
 *                      xrt/tasks/xrtexpomap/xrtexpomap
 *                      xrt/tasks/xrtpipeline/xrtpipeline 
 * 
 *      15/11/2005     -- Changed declaration of the 'expo' array
 *                        to avoid crashing on OS-X (Panther) as 
 *                        reported in the M. Tripicco mail  (15/11/2005)
 *                      modules affected:
 *                      xrt/tasks/xrtmkarf/xrtmkarf.c
 *
 *    v.1.8.0 - 17/03/2006
 *                   -- new modules:
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *                      xrt/tasks/xrtlccorr/xrtlccorr.h
 *                      xrt/tasks/xrtlccorr/xrtlccorr.par
 *                      xrt/tasks/xrtlccorr/xrtlccorr.html
 *                      xrt/tasks/xrtlccorr/ut_xrtlccorr
 *                      xrt/tasks/xrttdrss2/xrttdrss2
 *                      xrt/tasks/xrttdrss2/xrttdrss2.html
 *                      xrt/tasks/xrttdrss2/xrttdrss2.par
 *                      xrt/tasks/xrttdrss2/ut_xrttdrss2
 *
 *                   -- Bug fixed checking HPIXCT/LRHPixCt column value
 *                   modules affected:
 *                      xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                   -- Added messages to displayed script execution
 *                      beginning and end time
 *                   -- use IMPOSERROR HDU instead of TDPOSERROR to 
 *                      calculate error
 *                   modules affected:
 *                      xrt/tasks/xrttdrss/xrttdrss
 *                   -- Use ra, dec and radius 'xrtproducts' input parameters
 *                      only if (wt/pc)regionfile set to DEFAULT
 *                   modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                   -- Bug fixed when fills the last row for the 
 *                      first time
 *                   modules affected:
 *                      xrt/tasks/xrthkproc/xrthkproc.c
 *                   -- If biasdiff input parameter set to 0 nothing
 *                      will be done and exit with warning
 *                   modules affected:
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.c
 *                   -- Build exposure map for WT mode
 *                   modules affected:
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.h
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.c
 *                      xrt/tasks/xrtexpomap/xrtexpomap
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *                      xrt/tasks/xrtproducts/xrtproducts
 *                   -- gzipped output temporary files if cleanup 
 *                      set to no
 *                   -- Modified standard output filenames
 *                      Used TCRVALn instead of RA_PNT and DEC_PNT
 *                   modules affected:
 *                      xrt/tasks/xrtexpomap/xrtexpomap
 *                   -- Modified to handle TDRSS Photon Counting 
 *                      event file format
 *                   modules affected:
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                   -- Added new input par 'colfile' to correct 
 *                      telemetred rawx and add and fill RAWXTL colunm, 
 *                      if needed
 *                  modules affected:
 *                      xrt/tasks/xrttimetag/xrttimetag.c
 *                      xrt/tasks/xrttimetag/xrttimetag.h
 *                      xrt/tasks/xrttimetag/xrttimetag.html
 *                      xrt/tasks/xrttimetag/xrttimetag.par
 *                      xrt/tasks/xrttimetag/ut_xrttimetag_lr
 *                      xrt/tasks/xrttimetag/ut_xrttimetag_wt
 *                      xrt/lib/xrtfits/xrtdefs.h
 *                      xrt/lib/xrtcaldb/xrtcaldb.h 
 *                      xrt/lib/xrtcaldb/ReadCalFile.c
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.par
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.c
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.html
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.h
 *                      xrt/tasks/xrtwtcorr/ut_xrtwtcorr
 *
 *                   -- Modified to recognize on board software version
 *                      from 'IMBLVL' column values
 *                   modules affected:
 *                      xrt/tasks/xrtimage/xrtimage.c
 *                   -- Changed the WT part of read_pha to account for 
 *                      different geometries of the extraction region. 
 *                   -- Added the possibility of reading and using the WT
 *                      exposure map
 *                   modules affected
 *                      xrt/tasks/xrtmkarf/xrtmkarf.c
 *                   -- Screen mkf filter file for attitude GTI
 *                      if gtiexpr set
 *                   modules affected:
 *                      xrt/tasks/xrtfilter/xrtfilter.par
 *                      xrt/tasks/xrtfilter/xrtfilter
 *                   -- Modified xrtpipeline to support Build 18 changes
 *
 *    v.1.9.0 - 20/05/2006
 *                   -- badpixel files "TIME" column handling implemented.
 *                   modules affected:
 *                      xrt/lib/xrtbpfunc/xrtbpfunc.c
 *                      xrt/lib/xrtbpfunc/xrtbpfunc.h
 *                      xrt/lib/xrtfits/xrtdefs.h
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                      xrt/tasks/xrtflagpix/xrtflagpix.h
 *                      xrt/tasks/xrtimage/xrtimage.c
 *                   -- 64-bit architecture support                       
 *                   modules affected:
 *                      xrt/lib/xrtfits/xrtdefs.h
 *                      xrt/lib/highfits/highfits.h
 *                   -- Clean the unfiltered event file before
 *                      run the stage 1 on an event file
 *                      processed by prevous run.
 *                   modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                   -- Do not flag the WT border events as bad
 *                   modules affected:
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                      xrt/tasks/xrtflagpix/xrtflagpix.h
 *                   -- Pixels outside the Data Mode window
 *                      are now considered as not exposed
 *                   modules affected:
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.c
 *                      xrt/lib/xrtfits/xrtdefs.h
 *                   -- Correct the WM1STCOLNUM keyword in the
 *                      WT event file when the shift of the
 *                      columns is needed.
 *                   modules affected:
 *                      xrt/tasks/xrttimetag/xrttimetag.c
 *                      xrt/tasks/xrttimetag/xrttimetag.h
 *                      xrt/lib/xrtfits/xrtdefs.h
 *                      
 *    v.1.9.1 - 21/09/2006
 *                   -- Do not use the ENDTIME record value to update 
 *                      the TSTOP keyword if the frame has XRTMode set 
 *                      to 1 (telemetry xrt bias bogus mode)
 *                   modules affected:
 *                      xrt/tasks/xrthkproc/xrthkproc.c
 *
 *    v.2.0.0 - 08/11/2006
 *                   -- Query to CALDB for teldef file with 'DATE-OBS' and
 *                      'TIME-OBS' of input file
 *                   modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrt/xrttam/xrttam
 *                      xrt/tasks/xrt/xrtexpomap/xrtexpomap
 *                      xrt/tasks/xrt/xrthkproc/xrthkproc.c
 *                      xrt/tasks/xrt/xrthkproc/xrthkproc.h
 *                      xrt/tasks/xrt/xrttimetag/xrttimetag.c
 *                      xrt/tasks/xrt/xrttimetag/xrttimetag.h
 *                      xrt/tasks/xrt/lib/xrtperl/libswxrtperl.pl
 *                   -- Added 'ATTFLAG' keyword in output file hdu when
 *                      using attitude for coordinates transformation
 *                   modules affected:
 *                      xrt/tasks/xrt/xrttimetag/xrttimetag.c
 *                      xrt/tasks/xrt/xrttimetag/xrttimetag.h
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrt/xrttdrss2/xrttdrss2
 *                      xrt/lib/xrtfits/xrtdefs.h
 *                      xrt/tasks/xrt/lib/xrtperl/libswxrtperl.pl
 *                   -- Added new method 'M20P' for WT bias correction
 *                   modules affected:
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.c
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.h
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.par
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.html
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *
 *    v.2.0.1 - 22/11/2006
 *                   -- Bug fixed in xrttdrss2 found on
 *                      data taken not in a standard configuration 
 *                      (mixing slew and pointing images).
 *                   modules affected:
 *                      xrt/tasks/xrttdrss2/xrttdrss2
 *
 *    v.2.1.0 - 15/05/2007 BUILD 21 release
 *                   -- Added 'npixels' and 'biasth' input parameters                 
 *                   modules affected:
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.c
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.h
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.par
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.html
 *                      xrt/tasks/xrtwtcorr/ut_xrtwtcorr
 *                   -- Added 'wtnpixels' and 'wtbiasth' input parameters                 
 *                   modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline.html
 *                   -- Added check on ACS flags with setted=1 in10arcm=0
 *                      configuration
 *                   modules affected:
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.h
 *                      xrt/tasks/xrtevtrec/xrtevtrec.c
 *                      xrt/tasks/xrtevtrec/xrtevtrec.h
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                      xrt/tasks/xrtflagpix/xrtflagpix.h
 *                      xrt/tasks/xrtpcgrade/xrtpcgrade.c
 *                      xrt/tasks/xrtpcgrade/xrtpcgrade.h
 *                      xrt/tasks/xrttimetag/xrttimetag.c
 *                      xrt/tasks/xrttimetag/xrttimetag.h
 *                      xrt/tasks/xrthkproc/xrthkproc.c
 *                      xrt/tasks/xrtscreen/xrtscreen
 *                   -- Added 'acsscreen' parameter to screen events with
 *                      ACS flags in non standard pointing configuration
 *                   modules affected:
 *                      xrt/tasks/xrtscreen/xrtscreen
 *                      xrt/tasks/xrtscreen/xrtscreen.par
 *                      xrt/tasks/xrtscreen/xrtscreen.html
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline.html
 *                   -- Bug fixed in TSTART and TSTOP check of hd file
 *                      and added new keywords in output file
 *                   modules affected:
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.c
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.h
 *                   -- If WMCOLNUM or WM1STCOL in trfile have values out of
 *                      range is used a default value
 *                   modules affected:
 *                      xrt/tasks/xrttimetag/xrttimetag.c
 *                   -- Vignetting correction applied to raw instrument map
 *                   modules affected:
 *                      xrt/tasks/xrtexpocorr/xrtexpocorr.c
 *                      xrt/tasks/xrtexpocorr/xrtexpocorr.h
 *                      xrt/tasks/xrtexpocorr/xrtexpocorr.html
 *                      xrt/tasks/xrtexpomap/xrtexpomap
 *                      xrt/tasks/xrtexpomap/xrtexpomap.html
 *                   -- Changed HotPixels search algorithm and added
 *                      'gradeiterate' parameter
 *                   modules affected:
 *                      xrt/tasks/xrthotpix/xrthotpix.c
 *                      xrt/tasks/xrthotpix/xrthotpix.h
 *                      xrt/tasks/xrthotpix/xrthotpix.par
 *                      xrt/tasks/xrthotpix/xrthotpix.html
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline.html
 *                   -- Changed input parameters in xrthotpix call
 *                   modules affected:
 *                      xrt/tasks/xrttdrss2/xrttdrss2
 *                   -- WT mode supported in light curve correction
 *                   modules affected:
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *                      xrt/tasks/xrtlccorr/xrtlccorr.h
 *                      xrt/tasks/xrtlccorr/xrtlccorr.html
 *                      xrt/tasks/xrtlccorr/ut_xrtlccorr_wt
 *                      xrt/tasks/xrtlccorr/ut_xrtlccorr_pc
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                   -- Changed 'pccorrectlc' parameter to 'correctlc'
 *                   modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline.html
 *                   -- Added 'regionfile' parameter
 *                   modules affected:
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *                      xrt/tasks/xrtlccorr/xrtlccorr.h
 *                      xrt/tasks/xrtlccorr/xrtlccorr.par
 *                      xrt/tasks/xrtlccorr/xrtlccorr.html
 *                   -- New Task 'xrtpccorr'
 *                      xrt/tasks/xrtpccorr/Makefile
 *                      xrt/tasks/xrtpccorr/xrtpccorr.c
 *                      xrt/tasks/xrtpccorr/xrtpccorr.h
 *                      xrt/tasks/xrtpccorr/xrtpccorr.par
 *                      xrt/tasks/xrtpccorr/xrtpccorr.html
 *                      xrt/tasks/xrtpccorr/ut_xrtpccorr
 *                   -- New Task 'xrtpcbias'
 *                      xrt/tasks/xrtpcbias/Makefile
 *                      xrt/tasks/xrtpcbias/xrtpcbias
 *                      xrt/tasks/xrtpccorr/xrtpcbias.par
 *                      xrt/tasks/xrtpccorr/xrtpcbias.html
 *                   -- Added 'xrtpcbias' task call
 *                   modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline.html
 *                      xrt/tasks/xrtpipeline/ut_xrtpipeline_pc
 *                   -- Added 'pcbiascorr' parameter
 *                   modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline.html
 *                   -- Copy the 'BIASDIFF' extension in L2 event
 *                      files if exists
 *                   modules affected:
 *                      xrt/tasks/xrtscreen/xrtscreen
 *
 *    v.2.1.1 - 01/06/2007 BUILD 21 release
 *                   -- Added check on XPHASCO keyword to
 *                      verify if PHAS is already corrected
 *                   modules affected:
 *                      xrt/tasks/xrtpcbias/xrtpcbias
 *                   -- Modified Hot and Flickering pixels search
 *                      algorithm
 *                   modules affected:
 *                      xrt/tasks/xrthotpix/xrthotpix.c
 *                   -- Added 'checkattitude' input parameter
 *                   modules affected:
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.c
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.h
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.html
 *                      xrt/tasks/xrtexpomap/xrtexpomap
 *                      xrt/tasks/xrtexpomap/xrtexpomap.html
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline.html
 *                   -- Bug fixed when read WT intrument map
 *                      Bug fixed when create wmap for BOX and -BOX shape
 *                   modules affected:
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *                   -- Changed the WT part of read_pha to fix bug
 *                      when read exposure map
 *                   modules affected:
 *                      xrt/tasks/xrtmkarf/xrtmkarf.c
 *    v.2.1.2 - 14/06/2007
 *                   -- Bugs fixed after test at SSC
 *                      (as reported via mail by Mike Tripicco (11/06/2007))
 *                   modules affected:
 *                      xrt/tasks/xrtpccorr/xrtpccorr.c
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *    v.2.2.0 - 16/07/2007 BUILD 22 release
 *                   -- Query to CALDB for hkrange file with 'XRTVSUB' of input file
 *                   modules affected:
 *                      xrt/tasks/xrtscreen/xrtscreen
 *                   -- Query to CALDB for gain file with 'XRTVSUB' of input file
 *                   modules affected:
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.h
 *                   -- Query to CALDB for rmf file depending on time and 'XRTVSUB' of input file
 *                   modules affected:
 *                      xrt/tasks/xrtmkarf/xrtmkarf.c
 *                      xrt/tasks/xrtmkarf/xrtmkarf.h
 *                      xrt/tasks/xrtproducts/xrtproducts
 *                   -- Query to CALDB for arf file depending on time and 'XRTVSUB' of input file
 *                   modules affected:
 *                      xrt/tasks/xrtmkarf/xrtmkarf.c
 *                      xrt/tasks/xrtmkarf/xrtmkarf.h
 *                   -- Added 'thrfile' parameter to get threshold levels from
 *                      input file or CALDB
 *                   modules affected:
 *                      xrt/tasks/xrtevtrec/xrtevtrec.c
 *                      xrt/tasks/xrtevtrec/xrtevtrec.h
 *                      xrt/tasks/xrtevtrec/xrtevtrec.par
 *                      xrt/tasks/xrtpcgrade/xrtpcgrade.c
 *                      xrt/tasks/xrtpcgrade/xrtpcgrade.h
 *                      xrt/tasks/xrtpcgrade/xrtpcgrade.par
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                      xrt/tasks/xrtflagpix/xrtflagpix.h
 *                      xrt/tasks/xrtflagpix/xrtflagpix.par
 *                      xrt/tasks/xrtpcbias/xrtpcbias
 *                      xrt/tasks/xrtpcbias/xrtpcbias.par
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.c
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.h
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.par
 *                      xrt/tasks/xrtpdcorr/xrtpdcorr.c
 *                      xrt/tasks/xrtpdcorr/xrtpdcorr.h
 *                      xrt/tasks/xrtpdcorr/xrtpdcorr.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                   -- Changed 'bpfile'="CALDB" and 'thrfile'="CALDB"
 *                      parameters in xrtflagpix task call
 *                   modules affected:
 *                      xrt/tasks/xrttdrss2/xrttdrss2
 *                   -- Bug fixed when reading orbits info from mkf file and changed
 *                      datatype of BIASDIFF, BIASDIFF2, BIASDIFF4, BIASDIFF7, BIASDIFF9
 *                      and NEVENTS columns written in BIASDIFF ext
 *                   modules affected:
 *                      xrt/tasks/xrtpccorr/xrtpccorr.c
 *                   -- Flag as BAD events outside of FOV region
 *                   modules affected:
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                      xrt/tasks/xrtflagpix/xrtflagpix.h
 *                   -- Modified IsNumeric routine to accept numbers with '+' prefix
 *                   modules affected:
 *                      xrt/lib/xrtperl/libswxrtperl.pl
 *                   -- Added keywords handle for new CALDB thresholds file
 *                   modules affected:
 *                      /xrt/lib/xrtfits/xrtdefs.h
 *                   -- Added routine to read new CALDB thresholds file
 *                   modules affected:
 *                      /xrt/lib/xrtcaldb/xrtcaldb.h
 *                      /xrt/lib/xrtcaldb/ReadCalFile.c
 *    v.2.2.1 - 14/09/2007
 *                   -- Bug fixed when reading 'XRTVSUB' keyword
 *                   modules affected:
 *                      xrt/tasks/xrtmkarf/xrtmkarf.c
 *                   -- Clean 'XPHASCO' keyword when restore PHAS column;
 *                      Delete 'BIASDIFF' extension from previous run
 *                   modules affected:
 *                      xrt/tasks/xrtpcbias/xrtpcbias
 *                   -- Exit with warning if mkffile not include infile range time
 *                   modules affected:
 *                      xrt/tasks/xrtpccorr/xrtpccorr.c
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *    v.2.2.2 - 19/11/2007
 *                   -- Bug fixed checking RA and DEC input parameters
 *                   modules affected:
 *                      xrt/lib/xrtperl/libswxrtperl.pl
 *                   -- Query to CALDB for posfile time-dependent
 *                   modules affected:
 *                      xrt/tasks/xrtcentroid/xrtcentroid
 *                   -- Bug fixed in xrtexpocorr task call
 *                   modules affected:
 *                      xrt/tasks/xrtexpomap/xrtexpomap
 *                   -- Added 'enfile' input parameter
 *                   modules affected:
 *                      xrt/tasks/xrtfilter/xrtfilter
 *                      xrt/tasks/xrtfilter/xrtfilter.html
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.html
 *                   -- Added GTI selection based on spacecraft parameters
 *                      from new extension 'SAC' in hkrangefile
 *                   modules affected:
 *                      xrt/tasks/xrtscreen/xrtscreen
 *                      xrt/tasks/xrtscreen/xrtscreen.html
 *                   -- Bug fixed in 'TIMEDEL' keyword updating
 *                   modules affected:
 *                      xrt/tasks/xrttimetag/xrttimetag.c
 *                   -- Input exposure file must be set to NONE if psfflag=no
 *                   modules affected:
 *                      xrt/tasks/xrtmkarf/xrtmkarf.c
 *    v.2.3.0 - 14/05/2008
 *                   -- Modified to handle new gain CALDB file format
 *                   modules affected:
 *                      xrt/tasks/xrtcalcpi/xrtcacpi.c
 *                      xrt/tasks/xrtcalcpi/xrtcacpi.h
 *                   -- Added 'biascorr' input parameter
 *                   modules affected:
 *                      xrt/tasks/xrtpcbias/xrtpcbias
 *                      xrt/tasks/xrtpcbias/xrtpcbias.par
 *                      xrt/tasks/xrtpcbias/xrtpcbias.html
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                   -- PHAS correction not applied for telemetred bad pixels
 *                   modules affected:
 *                      xrt/tasks/xrtpccorr/xrtpccorr.c
 *                      xrt/tasks/xrtpccorr/xrtpccorr.h
 *                   -- Added 'dateobs' and 'timeobs' input parameters
 *                   modules affected:
 *                      xrt/tasks/xrtcentroid/xrtcentroid
 *                      xrt/tasks/xrtcentroid/xrtcentroid.par
 *                   -- Cut raw instrument map and add sky instrument map in 
 *                      clean up file list
 *                   modules affected:
 *                      xrt/tasks/xrtexpomap/xrtexpomap
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                   -- Create raw instrument map centered in input region;
 *                      Handle input raw instrument map of any dimension;
 *                   modules affected:
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *                      xrt/tasks/xrtlccorr/xrtlccorr.h
 *                      xrt/tasks/xrtlccorr/xrtlccorr.par
 *                      xrt/tasks/xrtlccorr/xrtlccorr.html
 *                   -- Changed WT default region for products extraction
 *                   modules affected:
 *                      xrt/tasks/xrtproducts/xrtproducts
 *                      xrt/tasks/xrtproducts/xrtproducts.par
 *                      xrt/tasks/xrtproducts/xrtproducts.html
 *                   -- Implemented 'xrtlccorr' task changes;
 *                      Implemented 'xrtproducts' task changes;
 *                      Handle MAC OSX "AppleDouble" files problem
 *                   modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                   -- New Task 'xrtgrblc' from HEASARC
 *                      xrt/tasks/xrtgrblc/Makefile
 *                      xrt/tasks/xrtgrblc/xrtgrblc
 *                      xrt/tasks/xrtgrblc/xrtgrblc.par
 *                      xrt/tasks/xrtgrblc/xrtgrblc.html
 *                      xrt/tasks/xrtgrblc/libswxrtgrblc.pm
 *                      xrt/tasks/xrtgrblc/ut_xrtgrblc
 *                   -- Replaced 'makefilter' call with 'newmakefilter' call;
 *                      Removed 'tprec' input parameter
 *                   modules affected:
 *                      xrt/tasks/xrtfilter/xrtfilter
 *                      xrt/tasks/xrtfilter/xrtfilter.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *    v.2.4.0 - 17/10/2008
 *                   -- Fixed compatibility problem with new perl 5.10
 *                   modules affected:
 *                      xrt/tasks/xrtscreen/xrtscreen
 *                      xrt/tasks/xrtfilter/xrtfilter
 *                      xrt/tasks/xrtpcbias/xrtpcbias
 *                   -- Replaced 'newmakefilter' task call with 
 *                      'makefilter' (new task version!) call
 *                   modules affected:
 *                      xrt/tasks/xrtfilter/xrtfilter
 *                   -- ACS flags with safehold=1 => obsmode=SLEW
 *                   modules affected:
 *                      xrt/tasks/xrtscreen/xrtscreen
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                      xrt/tasks/xrttimetag/xrttimetag.c
 *                      xrt/tasks/xrtpcgrade/xrtpcgrade.c
 *                      xrt/tasks/xrtevtrec/xrtevtrec.c
 *                      xrt/tasks/xrthkproc/xrthkproc.c
 *                   -- Bug fixed with compressed input files
 *                   modules affected:
 *                      xrt/tasks/xrtpcbias/xrtpcbias
 *                   -- PHAS correction not applied for evt from calibration source
 *                   modules affected:
 *                      xrt/tasks/xrtpccorr/xrtpccorr.c
 *                   -- Not interpolate offset value of two successive rows in CALDB gain file
 *                   modules affected:
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                   -- Removed nframe input parameter and added wtnframe and pcnframe parameters
 *                   modules affected:
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *                      xrt/tasks/xrtlccorr/xrtlccorr.h
 *                      xrt/tasks/xrtlccorr/xrtlccorr.par
 *                      xrt/tasks/xrtlccorr/xrtlccorr.html
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline.html
 *                      xrt/tasks/xrtgrblc/xrtgrblc
 *                   -- New Task 'xrtgrblcspec' from HEASARC
 *                      xrt/tasks/xrtgrblcspec/Makefile
 *                      xrt/tasks/xrtgrblcspec/xrtgrblcspec
 *                      xrt/tasks/xrtgrblcspec/xrtgrblcspec.par
 *                      xrt/tasks/xrtgrblcspec/xrtgrblcspec.html
 *                      xrt/tasks/xrtgrblcspec/ut_xrtgrblcspec
 *                   -- New version of 'xrtgrblc' from HEASARC
 *                   modules affected:
 *                      xrt/tasks/xrtgrblc/Makefile
 *                      xrt/tasks/xrtgrblc/xrtgrblc
 *                      xrt/tasks/xrtgrblc/xrtgrblc.par
 *                      xrt/tasks/xrtgrblc/xrtgrblc.html
 *                      xrt/tasks/xrtgrblc/libswxrtgrblc.pm
 *                      xrt/tasks/xrtgrblc/ut_xrtgrblc
 *    v.2.4.1 - 17/12/2008
 *                   -- Added XRA_PNT and XDEC_PNT keywords in output file
 *                   modules affected:
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.c
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.h
 *                   -- Get ranom and decnom values from XRA_PNT and XDEC_PNT keywords
 *                      instead of RA_PNT and DEC_PNT
 *                   modules affected:
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *                      xrt/tasks/xrtlccorr/xrtlccorr.h
 *                   -- Get ranom and decnom values from TCRVL<n> keywords
 *                   modules affected:
 *                      xrt/tasks/xrtpccorr/xrtpccorr.c
 *                      xrt/tasks/xrtpccorr/xrtpccorr.h
 *                   -- New version of 'xrtgrblc' from HEASARC
 *                   modules affected:
 *                      xrt/tasks/xrtgrblc/xrtgrblc
 *                      xrt/tasks/xrtgrblc/xrtgrblc.par
 *                      xrt/tasks/xrtgrblc/xrtgrblc.html
 *                      xrt/tasks/xrtgrblc/libswxrtgrblc.pm
 *                      xrt/tasks/xrtgrblc/ut_xrtgrblc
 *    v.2.4.2 - 20/03/2009
 *                   -- Bug fixed in xselect task call
 *                   modules affected:
 *                      xrt/tasks/xrtscreen/xrtscreen
 *                      xrt/tasks/xrtproducts/xrtproducts
 *                   -- Added energy dependecy in PI correction for charge traps;
 *                      Handle new gain CALDB file format
 *                   modules affected:
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.h
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.par
 *                   -- Added keywords handle for new CALDB gain file format
 *                   modules affected:
 *                      xrt/lib/xrtfits/xrtdefs.h
 *                   -- Added 'picorrtype' and 'savepinom' input parameter
 *                   modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                   -- New version of 'xrtgrblc' from HEASARC
 *                   modules affected:
 *                      xrt/tasks/xrtgrblc/xrtgrblc
 *                      xrt/tasks/xrtgrblc/xrtgrblc.par
 *                      xrt/tasks/xrtgrblc/xrtgrblc.html
 *                      xrt/tasks/xrtgrblc/libswxrtgrblc.pm
 *                      xrt/tasks/xrtgrblc/ut_xrtgrblc
 *                   -- New version of 'xrtgrblcspec' from HEASARC
 *                   modules affected:
 *                      xrt/tasks/xrtgrblcspec/xrtgrblcspec
 *                      xrt/tasks/xrtgrblcspec/xrtgrblcspec.par
 *                      xrt/tasks/xrtgrblcspec/xrtgrblcspec.html
 *                      xrt/tasks/xrtgrblcspec/ut_xrtgrblcspec
 *    v.2.4.3 - 08/05/2009
 *                   -- New Task 'xrtphascorr'
 *                      xrt/tasks/xrtphascorr/Makefile
 *                      xrt/tasks/xrtphascorr/xrtphascorr.c
 *                      xrt/tasks/xrtphascorr/xrtphascorr.h
 *                      xrt/tasks/xrtphascorr/xrtphascorr.par
 *                      xrt/tasks/xrtphascorr/xrtphascorr.html
 *                      xrt/tasks/xrtphascorr/ut_xrtphascorr
 *                      xrt/tasks/Makefile
 *                      xrt/tasks/ut_xrtALL
 *                   -- Added 'xrtphascorr' task call
 *                   modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline.html
 *                   -- Added keywords handle for new CALDB PHAS configuration file
 *                   modules affected:
 *                      /xrt/lib/xrtfits/xrtdefs.h
 *                   -- Added routine to read new CALDB PHAS configuration file
 *                   modules affected:
 *                      /xrt/lib/xrtcaldb/xrtcaldb.h
 *                      /xrt/lib/xrtcaldb/ReadCalFile.c
 *    v.2.5.0 - 15/07/2009
 *                   -- Replaced xspec11 with xspec call
 *                   modules affected:
 *                      xrt/tasks/xrtproducts/xrtproducts
 *                   -- New version of 'xrtgrblcspec' from HEASARC
 *                   modules affected:
 *                      xrt/tasks/xrtgrblcspec/xrtgrblcspec
 *                      xrt/tasks/xrtgrblcspec/xrtgrblcspec.par
 *                      xrt/tasks/xrtgrblcspec/xrtgrblcspec.html
 *                      xrt/tasks/xrtgrblcspec/ut_xrtgrblcspec
 *                   -- New version of 'xrtgrblc' from HEASARC
 *                   modules affected:
 *                      xrt/tasks/xrtgrblc/xrtgrblc
 *                      xrt/tasks/xrtgrblc/xrtgrblc.par
 *                      xrt/tasks/xrtgrblc/xrtgrblc.html
 *                      xrt/tasks/xrtgrblc/ut_xrtgrblc
 *    v.2.5.1 - 30/10/2009
 *                   -- Modified IsNumeric function to accept '.xx' numeric format;
 *                      Modified CallQuzCif function to set retrieve=yes in quzcif call
 *                   modules affected:
 *                      xrt/lib/xrtperl/libswxrtperl.pl
 *                   -- Minor changes to lcurve and xselect task call
 *                   modules affected:
 *                      xrt/tasks/xrtproducts/xrtproducts
 *                   -- If empty light curve doesn't execute xrtlccorr task 
 *                   modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                   -- Modified to optimize the execution time
 *                   modules affected:
 *                      xrt/tasks/xrtexpomap/xrtexpomap
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.c
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *    v.2.6.0 - 27/07/2010
 *                   -- Flags as BAD events on WT window's borders
 *                   modules affected:
 *                      xrt/tasks/xrtflagpix/xrtflagpix.c
 *                   -- Added creation of instrument map also for WT events in SETTLING mode
 *                   modules affected:
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.c
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                   -- Added correction of light curve also for WT events in SETTLING mode
 *                   modules affected:
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                   -- Modified default value for 'vigflag' input parameter from 'yes' to 'no'
 *                   modules affected:
 *                      xrt/tasks/xrtexpomap/xrtexpomap.par
 *                      xrt/tasks/xrtexpomap/xrtexpomap.html
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline.html
 *                   -- Handle extended sources while creating arf file (only for PC mode)
 *                      Modified off-axis angle computation
 *                      Added 'extended', 'boxsize', 'offaxis' and 'cleanup' input parameters
 *                   modules affected:
 *                      xrt/tasks/xrtmkarf/xrtmkarf.c
 *                      xrt/tasks/xrtmkarf/xrtmkarf.h
 *                      xrt/tasks/xrtmkarf/xrtmkarf.par
 *                      xrt/tasks/xrtmkarf/xrtmkarf.html
 *                   -- Added generation of ARF for extended sources (only for PC mode)
 *                      Added 'extended' and 'arfboxsize' input parameters
 *                      Changed default 'binsize' value for WT in SETTLING mode
 *                   modules affected:
 *                      xrt/tasks/xrtproducts/xrtproducts
 *                      xrt/tasks/xrtproducts/xrtproducts.par
 *                      xrt/tasks/xrtproducts/xrtproducts.html
 *                   -- Use uvot attitude file for data in SETTLING mode (only for WT data mode)
 *                      Modified default value for 'createexpomap', 'useexpomap' and 'correctlc' input parameters
 *                      Added 'stattfile' and 'stwtbinsize' input parameters
 *                      Added 'extended' and 'arfboxsize' input parameters                    
 *                   modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline.html
 *    v.2.7.0 - 01/04/2011
 *                   -- Modified PI correction algorithm
 *                      Handle new gain CALDB file format
 *                      Added 'userctipar','beta1','beta2','ecti','offsetniter' and 'savepioffset' input parameters
 *                   modules affected:
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.h
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.par
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.html
 *                   -- Handle new psf CALDB file format
 *                   modules affected:
 *                      xrt/tasks/xrtmkarf/xrtmkarf.c
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *                      xrt/tasks/lib/xrtfits/xrtdefs.h
 *                   -- Modified selection of good frames while reading hkfile
 *                   modules affected:
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.c
 *    v.2.8.0 - 10/02/2012
 *                   -- Query to CALDB for psf file datamode-dependent
 *                   modules affected:
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *                      xrt/tasks/xrtmkarf/xrtmkarf.c
 *                   -- Modified 'TIMEDEL' keyword updating
 *                   modules affected:
 *                      xrt/tasks/xrttimetag/xrttimetag.c
 *                   -- Modified computation of corrected GC1/GC2 coefficients
 *                   modules affected:
 *                      xrt/tasks/xrtcalcpi/xrtcalcpi.c
 *                      xrt/lib/xrtcaldb/xrtcaldb.h
 *    v.2.9.0 - 24/04/2013
 *                   -- Handle SLEW and SETTLING mode while executing 'xrtpcbias' task
 *                   modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *    v.2.9.1 - 12/06/2013
 *                   -- Changed handle of 'attitude' file
 *                   modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                   -- Bug fixed in ComputeBiasMedian function
 *                   modules affected:
 *                      xrt/tasks/xrtpccorr/xrtpccorr.c
 *    v.2.9.2 - 20/01/2014
 *                   -- Added check of the 'ATTFLAG' keyword of the input attitude file
 *                   modules affected:
 *                      xrt/tasks/xrttam/xrttam
 *                      xrt/tasks/xrtfilter/xrtfilter
 *                      xrt/tasks/xrttdrss2/xrttdrss2
 *                      xrt/tasks/xrtpccorr/xrtpccorr.c
 *                      xrt/tasks/xrthkproc/xrthkproc.c
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *                      xrt/tasks/xrtlccorr/xrtlccorr.h
 *                   -- Added check of the 'ATTFLAG' keyword of the input attitude file;
 *                      Added consistency check of the attitude and event input files;
 *                      Added ATTFLAG keyword in the output exposure map image
 *                   modules affected:
 *                      xrt/tasks/xrtexpomap/xrtexpomap
 *                   -- Added ATTFLAG keyword in primary hdu of output file;
 *                      Added check of the 'ATTFLAG' keyword of the input attitude file;
 *                      Removed GetAttitudeATTFLAG routine (moved in highfits library)
 *                   modules affected:
 *                         xrt/tasks/xrttimetag/xrttimetag.c
 *                         xrt/tasks/xrttimetag/xrttimetag.h
 *                         xrt/lib/highfits/highfits.c
 *                         xrt/lib/highfits/highfits.h
 *                   -- Added check of the 'ATTFLAG' keyword of the input attitude file;
 *                      Use 'sw00000000000pat.fits' attitude file for data in SETTLING moded;
 *                      Changed RunSwiftXform and RunCoordinator to add 'ATTFLAG' keyword also in primary header
 *                   modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *    v.2.9.3 - 19/02/2014
 *                   -- Modified check of the 'ATTFLAG' keyword of the input attitude file
 *                   modules affected:
 *                      xrt/tasks/xrttam/xrttam
 *                      xrt/tasks/xrtfilter/xrtfilter
 *                      xrt/tasks/xrttdrss2/xrttdrss2
 *                      xrt/tasks/xrtexpomap/xrtexpomap
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpccorr/xrtpccorr.c
 *                      xrt/tasks/xrthkproc/xrthkproc.c
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *                      xrt/tasks/xrttimetag/xrttimetag.c
 *                   -- Handle different type of the ATTFLAG keyword
 *                   modules affected:
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *                      xrt/tasks/xrtinstrmap/xrtinstrmap.c
 *                      xrt/lib/highfits/highfits.c
 *                      xrt/lib/highfits/highfits.h
 *    v.3.0.0 - 20/06/2014
 *                   -- Update DATE-OBS keyword of the output summed sky instrument map;
 *                      replaced 'nframe' input parameter with 'pcnframe' and 'wtnframe'
 *                   modules affected:
 *                      xrt/tasks/xrtexpomap/xrtexpomap
 *                      xrt/tasks/xrtexpomap/xrtexpomap.html
 *                      xrt/tasks/xrtexpomap/xrtexpomap.par
 *                   -- Modified default naming convention of output instrument map file
 *                   modules affected:
 *                      xrt/tasks/xrtlccorr/xrtlccorr.c
 *                      xrt/tasks/xrtlccorr/xrtlccorr.h
 *                   -- Fixed memory bug for the 64-bit architecture on Mac OS X 10.9.x.
 *                   modules affected:
 *                      xrt/tasks/xrtmkarf/xrtmkarf.c
 *                   -- Added extraction of background products for PC and WT mode;
 *                      added correction of the source light curve;
 *                      modified default naming convention of output.
 *                   modules affected:
 *                      xrt/tasks/xrtproducts/xrtproducts
 *                      xrt/tasks/xrtproducts/xrtproducts.par
 *                      xrt/tasks/xrtproducts/xrtproducts.html
 *                   -- Removed use of deprecated 'define' PERL function
 *                      Updated xrtproducts task call;
 *                      Added 'extractproducts' input parameter;
 *                      Added support of multiple 'datamode' values comma separated;
 *                      Replaced 'exponframe' input parameter with 'expopcnframe' and 'expowtnframe'
 *                   modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                      xrt/tasks/xrtpipeline/xrtpipeline.par
 *                      xrt/tasks/xrtpipeline/xrtpipeline.html
 *                   -- New version of 'xrtgrblc' from HEASARC
 *                   modules affected:
 *                      xrt/tasks/xrtgrblc/xrtgrblc
 *                      xrt/tasks/xrtgrblc/xrtgrblc.par
 *                      xrt/tasks/xrtgrblc/xrtgrblc.html
 *    v.3.1.0 - 19/06/2015
 *                   -- Fixed memory bug when the number of sub-boxes used to divide the extraction region is larger than 100
 *                   modules affected:
 *                      xrt/tasks/xrtmkarf/xrtmkarf.c
 *                   -- Modified 'expofile' input parameter type from hidden to asked
 *                   modules affected:
 *                      xrt/tasks/xrtmkarf/xrtmkarf.par
 *                      xrt/tasks/xrtmkarf/xrtmkarf.html
 *                   -- Modified default value of 'lccorravg', 'pcreglist' and 'wtreglist' input parameters
 *                   modules affected:
 *                      xrt/tasks/xrtgrblc/xrtgrblc.par
 *                   -- Update 'SOFTVER' and 'CALDBVER' keywords in output event files;
 *                      Updated xrtproducts task call
 *                   modules affected:
 *                      xrt/tasks/xrtpipeline/xrtpipeline
 *                   -- Update DATE-END and MJD-OBS keywords of the output summed sky instrument map
 *                   modules affected:
 *                      xrt/tasks/xrtexpomap/xrtexpomap
 *                   -- Modified 'expofile' input parameter type from hidden to asked;
 *                      Modified default value of 'correctlc' input parameter
 *                   modules affected:
 *                      xrt/tasks/xrtproducts/xrtproducts
 *                      xrt/tasks/xrtproducts/xrtproducts.par
 *                      xrt/tasks/xrtproducts/xrtproducts.html
 *    v.3.1.1 - 17/07/2015
 *                   -- Fixed bug in exposure map pixels numbering
 *                   modules affected:
 *                      xrt/tasks/xrtmkarf/xrtmkarf.c
 *    v.3.2.0 - 22/01/2016
 *                   -- Handle RAWX/RAWY out of range events. 
 *                   modules affected:
 *                      xrt/lib/xrtfits/xrtdefs.h
 *                      xrt/tasks/xrtgrblc/xrtgrblc
 *                      xrt/tasks/xrtpccorr/xrtpccorr.c
 *                      xrt/tasks/xrtpccorr/xrtpccorr.h
 *                   -- Changed dataset.
 *                   modules affected:
 *                      xrt/tasks/xrtscreen/ut_xrtscreen_im
 *                   -- Added GTI selection on CCDFrame RAWX/RAWY out of range events
 *                   modules affected:
 *                      xrt/tasks/xrtscreen/xrtscreen
 *
 *    v.3.3.0 - 18/10/2016
 *                   -- Remove parameter applytamcorr,tamfile,tamrefx1,tamrefy1,tamrefx2,tamrefy2,attcorrout
 *                   modules affected:
 *                      /xrt/tasks/xrtpipeline/ut_xrtpipeline_im
 *                      /xrt/tasks/xrtpipeline/ut_xrtpipeline_ir
 *                      /xrt/tasks/xrtpipeline/ut_xrtpipeline_pc
 *                      /xrt/tasks/xrtpipeline/ut_xrtpipeline_pu
 *                      /xrt/tasks/xrtpipeline/ut_xrtpipeline_wt
 *                      /xrt/tasks/xrtpipeline/xrtpipeline
 *                      /xrt/tasks/xrtpipeline/xrtpipeline.par
 *                   -- Bug fix for median calculation of the last frames. 
 *                   modules affected:
 *                      xrt/tasks/xrtwtcorr/xrtwtcorr.c
 *                   -- Removed duplicate key TELESCOP,fixed a bug in the writing keys INSTRUME and RADECSY.
 *                   modules affected:
 *                      /xrt/tasks/xrtscreen/xrtscreen
 *                   -- Performing cleanup.
 *                   modules affected:
 *                      /xrt/tasks/xrtgrblc/xrtgrblc
 *                   --Added subfunction getUtcfInit, get UTCFINIT keyword from 'attfile' or 'enfile' input
 *                   modules affected:
 *                      /xrt/tasks/xrtfilter/xrtfilter
 *                   --Added parameter timeadj=DEFAULT to the call task prefilter
 *                   modules affected:
 *                      /xrt/tasks/xrtfilter/xrtfilter
 *
 *                      
 *	DEPENDENCIES:
 *
 *	<none>
 */
void GetSWXRTDASVersion(Version_t verstr)
{

  strcpy(verstr,"swxrtdas_18Oct16_v3.3.0");

} /* GetSWXRTDASVersion */

/*
 *	SYNOPSIS:
 *		void GetSWXRTDASVersion(Version_t verstr)
 *
 *	DEPENDENCIES:
 *
 *	<none>
 */
void GetFTOOLSVersion(Version_t verstr)
{
  char  FtoolsParFileName[MAXFNAME_LEN]; /* LHEASOFT ftools.par */ 
  char  tasknamev[128];                  /* needed by HEADAS get_toolname */
  char  *cptr;                           /* ptr to LHEASOFT environment variable */

  verstr[0] = '\0';
  strcpy(tasknamev,"GetFTOOLSVersion");

  /* If HEASOFT package is set */
  if ( (cptr = getenv("LHEASOFT")) ) {

    headas_chat(4,"%s: LHEASOFT env. variable set to %s\n",tasknamev,cptr);
    sprintf(FtoolsParFileName,"%s/syspfiles/ftools.par",cptr);

    if (FileExists(FtoolsParFileName)) {

      FILE *ptrparfile;                 /* Pointer to the parameter file */
      char buffer[STR_LEN];             /* buffer for fgets()            */

      if ( (ptrparfile = fopen(FtoolsParFileName,"r")) ) {
	int found = 0;

	/* loop on file while 'version' substring found */
	while (!feof(ptrparfile) && !ferror(ptrparfile) && !found) {
	  int i;                        /* string character index */
	  char *ptrstr;                 /* ptr to the version number substring */
	  char cfind = '\"';            /* The version number is between double quotes */

	  fgets( buffer, STR_LEN, ptrparfile);

	  if ( strstr(&buffer[0],"version") ) {

	    /* set the pointer to the first '"' */
	    if ((ptrstr = index( &buffer[0],cfind)) ) {
	      ptrstr++;

	      /* find the second '"' */
	      for ( i=1; i<strlen(buffer) && ptrstr[i]!=cfind ; i++ )
		continue;

	      if ( i < strlen(buffer) ) {
		found = 1;
		ptrstr[i] = '\0';
		sprintf(verstr,"lheadas_%s",ptrstr); 
		headas_chat(4,"%s: %s",tasknamev,verstr);
	      }
	    }
	  } 
	} /* while !feof && !found */

	fclose(ptrparfile);

	if ( !found ) {
	  headas_chat(3,"%s: %s\n",tasknamev,"Warning cannot retrieve version from 'ftools.par' file\n",
		      FtoolsParFileName);

	} 
      } /* if !found substring */ 
      else 
	headas_chat(3,"%s: %s\n",tasknamev,"Warning '%s' parameter file not found\n",
		      FtoolsParFileName);
      
    } /* if Parameter File Exists */ 
    else 
      headas_chat(3,"%s: %s\n",tasknamev,"Warning '%s' parameter file not found\n",
		    FtoolsParFileName);
    
  } /* If LHEADAS variable set */
  else 
    headas_chat(3,"%s: %s\n", tasknamev, "HEADAS Environment not set\n"); 
  
} /* GetFTOOLSVersion */

/*
 *	SYNOPSIS:
 *		void GetSWXRTDASVersion(Version_t verstr)
 *      DESCRIPTION:
 *              TBD - HEADAS Version not available at the moment. 
 *
 *	DEPENDENCIES:
 *
 *	<none>
 */
void GetHEADASVersion(Version_t verstr)
{
  strcpy(verstr,"headas_v0.0.0");
} /* GetSWXRTDASVersion */

