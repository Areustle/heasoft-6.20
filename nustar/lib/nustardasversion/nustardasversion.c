/*
 *	nustardasversion.c: --- routines to retrive packages versions
 *                              for FITS History ---
 *
 *	DESCRIPTION:
 *              A complete history of NuSTARDAS versions (see the
 *              NuSTARDAS HISTORY at the GetNuSTARDASVersion function.
 *              The NUSTAR Data Analysis Software Version Numbering
 *              scheme depends on the Software Development Stage.
 *              During Software Development the scheme mean is:
 *                0.x.y
 *                ^ ^ ^
 *                | | |--> Software tounovers for SMR/SPR (the number
 *                | |      can take two digit if needed)
 *                | |----> Software Build Release
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
 *              void GetNuSTARDASVersion(Version_t *) 
 *                                  this funcion returns the NuSTARDAS version.
 *              void GetFTOOLSVersion(Version_t *)
 *                                  this funcion returns the FTOOLS version
 *                                  set when the program is running.
 *
 *	CHANGE HISTORY:
 *              0.0.1 - 25/10/2010 - nustardas_25Oct10_v0.0.1
 *              0.0.2 - 10/01/2011 - nustardas_10Jan11_v0.0.2
 *              0.0.3 - 11/02/2011 - nustardas_11Feb11_v0.0.3
 *              0.0.4 - 02/03/2011 - nustardas_02Mar11_v0.0.4
 *              0.0.5 - 16/03/2011 - nustardas_16Mar11_v0.0.5
 *              0.1.0 - 16/05/2011 - nustardas_16May11_v0.1.0
 *              0.1.1 - 26/05/2011 - nustardas_26May11_v0.1.1
 *              0.1.2 - 15/07/2011 - nustardas_15Jul11_v0.1.2
 *              0.1.3 - 05/08/2011 - nustardas_05Aug11_v0.1.3
 *              0.2.0 - 07/10/2011 - nustardas_07Oct11_v0.2.0
 *              0.2.1 - 11/10/2011 - nustardas_11Oct11_v0.2.1
 *              0.2.2 - 11/11/2011 - nustardas_11Nov11_v0.2.2
 *              0.3.0 - 16/12/2011 - nustardas_16Dec11_v0.3.0
 *              0.4.0 - 27/01/2012 - nustardas_27Jan12_v0.4.0
 *              0.4.1 - 17/02/2012 - nustardas_17Feb12_v0.4.1
 *              0.4.2 - 06/03/2012 - nustardas_06Mar12_v0.4.2
 *              0.4.3 - 29/03/2012 - nustardas_29Mar12_v0.4.3
 *              0.5.0 - 25/05/2012 - nustardas_25May12_v0.5.0
 *              0.6.0 - 27/07/2012 - nustardas_27Jul12_v0.6.0
 *              0.6.1 - 18/09/2012 - nustardas_18Sep12_v0.6.1
 *              0.7.0 - 08/10/2012 - nustardas_08Oct12_v0.7.0
 *              0.7.1 - 15/10/2012 - nustardas_15Oct12_v0.7.1
 *              0.8.0 - 30/10/2012 - nustardas_30Oct12_v0.8.0
 *              0.8.1 - 07/11/2012 - nustardas_07Nov12_v0.8.1
 *              0.8.2 - 26/11/2012 - nustardas_26Nov12_v0.8.2
 *              0.9.0 - 14/12/2012 - nustardas_14Dec12_v0.9.0
 *              1.0.0 - 15/03/2013 - nustardas_15Mar13_v1.0.0
 *              1.0.1 - 21/03/2013 - nustardas_21Mar13_v1.0.1
 *              1.1.0 - 24/04/2013 - nustardas_24Apr13_v1.1.0
 *              1.1.1 - 09/05/2013 - nustardas_09May13_v1.1.1
 *              1.2.0 - 14/06/2013 - nustardas_14Jun13_v1.2.0
 *              1.2.1 - 23/07/2013 - nustardas_23Jul13_v1.2.1
 *              1.3.0 - 07/10/2013 - nustardas_07Oct13_v1.3.0
 *              1.3.1 - 09/12/2013 - nustardas_09Dec13_v1.3.1
 *              1.4.0 - 15/04/2014 - nustardas_15Apr14_v1.4.0
 *              1.4.1 - 28/05/2014 - nustardas_28May14_v1.4.1
 *              1.5.0 - 28/05/2015 - nustardas_28May15_v1.5.0
 *              1.5.1 - 09/06/2015 - nustardas_09Jun15_v1.5.1
 *              1.6.0 - 13/04/2016 - nustardas_14Apr16_v1.6.0
 *              1.7.0 - 06/10/2016 - nustardas_06Oct16_v1.7.0
 *              1.7.1 - 06/12/2016 - nustardas_06Dec16_v1.7.1
 *              
 *
 *	AUTHOR	:
 *		ASDC - ASI Science Data Center
 */

#define NUSTARDASVERSION_C
#define NUSTARDASVERSION_VERSION	"1.7.0"


			/********************************/
			/*        header files          */
			/********************************/

#include "nustardasversion.h"

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
 *		void GetNuSTARDASVersion(Version_t verstr)
 *      NuSTARDAS HISTORY:
 *
 *      v.0.0.1 - 25/10/10 First beta version of nucalcpha task
 *      v.0.0.2 - 10/01/11 
 *                   -- Added 'cleancols' input parameter;
 *                      Handle CALDB query for 'offsetfile' and 'gradefile' input parameters;
 *                      Modified handling of "DENRISE = 0" events;
 *                      Modified handling of events below software threshold;
 *                   modules affected:
 *                      nustar/tasks/nucalcpha/nucalcpha.c
 *                      nustar/tasks/nucalcpha/nucalcpha.h
 *                      nustar/tasks/nucalcpha/nucalcpha.par
 *                      nustar/tasks/nucalcpha/nucalcpha.html
 *                   -- Added functions ad definitions for CALDB query
 *                   modules affected:
 *                      nustar/lib/caldb/nu_caldb.c
 *                      nustar/lib/caldb/nu_caldb.h
 *                      nustar/lib/highfits/nu_defs.h
 *      v.0.0.3 - 11/02/11 
 *                   -- New task 'nucalcpi'
 *                   modules affected:
 *                      nustar/tasks/nucalcpi/nucalcpi.c
 *                      nustar/tasks/nucalcpi/nucalcpi.h
 *                      nustar/tasks/nucalcpi/nucalcpi.par
 *                      nustar/tasks/nucalcpi/nucalcpi.html
 *                      nustar/tasks/nucalcpi/Makefile
 *                      nustar/tasks/Makefile
 *                      nustar/lib/caldb/nu_caldb.h
 *                      nustar/lib/highfits/nu_defs.h
 *      v.0.0.4 - 02/03/11 
 *                   -- Added 'temperature' input parameter
 *                   modules affected:
 *                      nustar/tasks/nucalcpi/nucalcpi.c
 *                      nustar/tasks/nucalcpi/nucalcpi.h
 *                      nustar/tasks/nucalcpi/nucalcpi.par
 *                      nustar/tasks/nucalcpi/nucalcpi.html
 *      v.0.0.5 - 16/03/11 
 *                   -- New task 'nucalcpos'
 *                   modules affected:
 *                      nustar/tasks/nucalcpos/nucalcpos.c
 *                      nustar/tasks/nucalcpos/nucalcpos.h
 *                      nustar/tasks/nucalcpos/nucalcpos.par
 *                      nustar/tasks/nucalcpos/nucalcpos.html
 *                      nustar/tasks/nucalcpos/Makefile
 *                      nustar/tasks/Makefile
 *                      nustar/lib/caldb/nu_caldb.h
 *                      nustar/lib/highfits/nu_defs.h
 *                      nustar/lib/misc/nu_misc.c
 *                      nustar/lib/misc/nu_misc.h
 *      ###  BUILD 1  ###
 *      v.0.1.0 - 16/05/11
 *                   -- New task 'nuflagbad'
 *                   modules affected:
 *                      nustar/tasks/nuflagbad/nuflagbad.c
 *                      nustar/tasks/nuflagbad/nuflagbad.h
 *                      nustar/tasks/nuflagbad/nuflagbad.par
 *                      nustar/tasks/nuflagbad/nuflagbad.html
 *                      nustar/tasks/nuflagbad/Makefile
 *                      nustar/tasks/Makefile
 *                      nustar/lib/badpix/nu_badpix.c
 *                      nustar/lib/badpix/nu_badpix.h
 *                      nustar/lib/caldb/nu_caldb.h
 *                      nustar/lib/highfits/nu_defs.h
 *                      nustar/lib/Makefile
 *                   -- New task 'nupipeline'
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                      nustar/tasks/nupipeline/Makefile
 *                      nustar/tasks/Makefile
 *                   -- New perl library 'libnustarperl.pl'
 *                   modules affected:
 *                      nustar/lib/perl/libnustarperl.pl
 *                      nustar/lib/Makefile
 *      v.0.1.1 - 26/05/11
 *                   -- New task 'nucalcpos'
 *                   modules affected:
 *                      nustar/tasks/nucalcpos/nucalcpos.c
 *                      nustar/tasks/nucalcpos/nucalcpos.h
 *                      nustar/tasks/nucalcpos/nucalcpos.par
 *                      nustar/tasks/nucalcpos/nucalcpos.html
 *                      nustar/tasks/nucalcpos/Makefile
 *                      nustar/tasks/Makefile
 *                      nustar/lib/highfits/nu_defs.h
 *      v.0.1.2 - 15/07/11
 *                   -- Added Unit Test
 *                   modules affected:
 *                      nustar/lib/Makefile
 *                      nustar/lib/perl/ut_nustar.pm
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                      nustar/tasks/nucalcpha/Makefile
 *                      nustar/tasks/nucalcpha/ut_nucalcpha_fpma
 *                      nustar/tasks/nucalcpha/ut_nucalcpha_fpmb
 *                      nustar/tasks/nucalcpi/Makefile
 *                      nustar/tasks/nucalcpi/ut_nucalcpi_fpma
 *                      nustar/tasks/nucalcpi/ut_nucalcpi_fpmb
 *                      nustar/tasks/nucalcpos/Makefile
 *                      nustar/tasks/nucalcpos/ut_nucalcpos_fpma
 *                      nustar/tasks/nucalcpos/ut_nucalcpos_fpmb
 *                      nustar/tasks/nuflagbad/Makefile
 *                      nustar/tasks/nuflagbad/ut_nuflagbad_fpma
 *                      nustar/tasks/nuflagbad/ut_nuflagbad_fpmb
 *                      nustar/tasks/nupipeline/Makefile
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpma
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpmb
 *                   -- Handle reprocessing of data
 *                   modules affected:
 *                      nustar/lib/badpix/nu_badpix.c
 *                      nustar/lib/badpix/nu_badpix.h
 *                      nustar/tasks/nuflagbad/nuflagbad.c
 *                      nustar/tasks/nuflagbad/nuflagbad.h
 *                   -- Handle new CALDB file 'PHAPAR';
 *                      Handle new format of pixel location file
 *                   modules affected:
 *                      nustar/lib/caldb/nu_caldb.h
 *                      nustar/lib/highfits/nu_defs.h
 *                   -- Added 'phaparfile' input parameter
 *                   modules affected:
 *                      nustar/tasks/nucalcpha/nucalcpha.c
 *                      nustar/tasks/nucalcpha/nucalcpha.h
 *                      nustar/tasks/nucalcpha/nucalcpha.par
 *                      nustar/tasks/nucalcpha/nucalcpha.html
 *                   -- Added 'metflag' and 'initseed' input parameters;
 *                      Handle new format of pixel location file
 *                   modules affected:
 *                      nustar/tasks/nucalcpos/nucalcpos.c
 *                      nustar/tasks/nucalcpos/nucalcpos.h
 *                      nustar/tasks/nucalcpos/nucalcpos.par
 *                      nustar/tasks/nucalcpos/nucalcpos.html
 *                   -- Added 'nucalcpos' task call
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *      v.0.1.3 - 05/08/11
 *                   -- Added 'runcalcpha' and 'inpsdfilecor' input parameters;
 *                      Update 'SOFTVER' keyword in output event files 
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                   -- Added 'inpsdfilecorr' input parameter;
 *                      Added new keywords in 'outpsdfile' and 'outpsdfilecor' output files;
 *                      Handle NULL values in DET1X and DET1Y columns;
 *                      Handle new allowed range of DET1X, DET1Y, RAW2X and RAW2Y values
 *                   modules affected:
 *                      nustar/tasks/nucalcpos/nucalcpos.c
 *                      nustar/tasks/nucalcpos/nucalcpos.h
 *                      nustar/tasks/nucalcpos/nucalcpos.par
 *                      nustar/tasks/nucalcpos/nucalcpos.html
 *                      nustar/lib/highfits/nu_defs.h
 *                   -- PI values for events with grade>12 (excluded grade 26,26,28 and 29) are set to NULL;
 *                      Events with grade=[26|27|28|29] are treated as grade=0 events in PI computation
 *                   modules affected:
 *                      nustar/tasks/nucalcpi/nucalcpi.c
 *      ###  BUILD 2  ###
 *      v.0.2.0 - 07/10/11
 *                   -- New task 'nuhotpix'
 *                   modules affected:
 *                      nustar/tasks/nuhotpix/Makefile
 *                      nustar/tasks/nuhotpix/nuhotpix.c
 *                      nustar/tasks/nuhotpix/nuhotpix.h
 *                      nustar/tasks/nuhotpix/nuhotpix.par
 *                      nustar/tasks/nuhotpix/nuhotpix.html
 *                      nustar/tasks/nuhotpix/ut_nuhotpix_fpma
 *                      nustar/tasks/nuhotpix/ut_nuhotpix_fpmb
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                      nustar/lib/Makefile
 *                      nustar/lib/misc/nu_gammq.c
 *                      nustar/lib/misc/nu_gammq.h
 *                      nustar/lib/badpix/nu_badpix.c
 *                      nustar/lib/badpix/nu_badpix.h
 *                      nustar/lib/highfits/nu_defs.h
 *                   -- New task 'nucoord'
 *                   modules affected:
 *                      nustar/tasks/nucoord/Makefile
 *                      nustar/tasks/nucoord/nucoord
 *                      nustar/tasks/nucoord/nucoord.par
 *                      nustar/tasks/nucoord/nucoord.html
 *                      nustar/tasks/nucoord/ut_nucoord_fpma
 *                      nustar/tasks/nucoord/ut_nucoord_fpmb
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                      nustar/lib/perl/libnustarperl.pl
 *                   -- Handle 'BADPOS' and 'HOTPOS' columns of input event file
 *                   modules affected:
 *                      nustar/tasks/nucalcpha/nucalcpha.c
 *                      nustar/tasks/nucalcpha/nucalcpha.h
 *                      nustar/tasks/nucalcpha/nucalcpha.html
 *                      nustar/lib/highfits/nu_defs.h
 *                   -- Modified input parameter order
 *                   modules affected:
 *                      nustar/tasks/nucalcpi/nucalcpi.par
 *                      nustar/tasks/nucalcpi/nucalcpi.html
 *                   -- Handle new allowed range of DET1X, DET1Y, RAW2X and RAW2Y values
 *                      Added 'alignfile' and 'mastaspectfile' input parameters
 *                      Added calculation of DET2X and DET2Y columns in output event file
 *                   modules affected:
 *                      nustar/tasks/nucalcpos/nucalcpos.c
 *                      nustar/tasks/nucalcpos/nucalcpos.h
 *                      nustar/tasks/nucalcpos/nucalcpos.par
 *                      nustar/tasks/nucalcpos/nucalcpos.html
 *                      nustar/tasks/nucalcpos/ut_nucalcpos_fpma
 *                      nustar/tasks/nucalcpos/ut_nucalcpos_fpmb
 *                      nustar/tasks/nucalcpos/Makefile
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpma
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpmb
 *                      nustar/lib/highfits/nu_defs.h
 *                      nustar/lib/caldb/nu_caldb.h
 *                      nustar/lib/misc/nu_misc.c
 *                      nustar/lib/misc/nu_misc.h
 *                   -- Add new column 'BADPOS' to output event file
 *                   modules affected:
 *                      nustar/tasks/nuflagbad/nuflagbad.c
 *                      nustar/tasks/nuflagbad/nuflagbad.h
 *                      nustar/tasks/nuflagbad/nuflagbad.html
 *      v.0.2.1 - 11/10/11
 *                   -- Add 'TLMIN*' and 'TLMAX*' keywords in BADPIX extension for RAWX and RAWY columns
 *                   modules affected:
 *                      nustar/lib/badpix/nu_badpix.c
 *                   -- Bug fixed when 'outfile' equal to 'infile' parameter;
 *                      Handle input event file without 'BADPOS' or 'HOTPOS' columns;
 *                      Add 'TUNIT*' keyword for PHA column
 *                   modules affected:
 *                      nustar/tasks/nucalcpha/nucalcpha.c
 *                   -- Bug fixed when 'outfile' equal to 'infile' parameter
 *                   modules affected:
 *                      nustar/tasks/nucalcpi/nucalcpi.c
 *                   -- Add 'TLMIN*', 'TLMAX*', 'TCDLT*', 'TCTYP*' and 'TCUNI*' keywords for DET1* and DET2* columns
 *                   modules affected:
 *                      nustar/tasks/nucalcpos/nucalcpos.c
 *                   -- Bug fixed on 'outbpfile' default name handling;
 *                      Bug fixed when 'outfile' equal to 'infile' parameter
 *                   modules affected:
 *                      nustar/tasks/nuflagbad/nuflagbad.c
 *                   -- Bug fixed on 'outhpfile' default name handling;
 *                      Bug fixed when 'outfile' equal to 'infile' parameter
 *                   modules affected:
 *                      nustar/tasks/nuhotpix/nuhotpix.c
 *                   -- Added 'runcalcpi' input parameter;
 *                      Removed 'nucalcpos' task;
 *                      Added 'nucoord' task;
 *                      Added 'nuhotpix' task
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpma
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpmb
 *      v.0.2.2 - 11/11/11
 *                   -- New task 'nufilter'
 *                   modules affected:
 *                      nustar/tasks/nufilter/Makefile
 *                      nustar/tasks/nufilter/nufilter
 *                      nustar/tasks/nufilter/nufilter.par
 *                      nustar/tasks/nufilter/nufilter.html
 *                      nustar/tasks/nufilter/ut_nufilter_fpma
 *                      nustar/tasks/nufilter/ut_nufilter_fpmb
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                   -- New task 'nuscreen'
 *                   modules affected:
 *                      nustar/tasks/nuscreen/Makefile
 *                      nustar/tasks/nuscreen/nuscreen
 *                      nustar/tasks/nuscreen/nuscreen.par
 *                      nustar/tasks/nuscreen/nuscreen.html
 *                      nustar/tasks/nuscreen/ut_nuscreen_fpma
 *                      nustar/tasks/nuscreen/ut_nuscreen_fpmb
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                   -- Add 'TLMIN*' and 'TLMAX*' keywords for PI column
 *                   modules affected:
 *                      nustar/tasks/nucalcpi/nucalcpi.c
 *                   -- Improved time execution performances
 *                   modules affected:
 *                      nustar/tasks/nucalcpos/nucaclcpos.c
 *                   -- Added 'nufilter' and 'nuscreen' tasks
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpma
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpmb
 *                      nustar/lib/perl/libnustarperl.pl
 *      ###  BUILD 3  ###
 *      v.0.3.0 - 16/12/11
 *                   -- New task 'numetrology'
 *                   modules affected:
 *                      nustar/tasks/numetrology/Makefile
 *                      nustar/tasks/numetrology/numetrology.c
 *                      nustar/tasks/numetrology/numetrology.h
 *                      nustar/tasks/numetrology/numetrology.par
 *                      nustar/tasks/numetrology/numetrology.html
 *                      nustar/tasks/numetrology/ut_numetrology
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                   -- New task 'nuoptaxis'
 *                   modules affected:
 *                      nustar/tasks/nuoptaxis/Makefile
 *                      nustar/tasks/nuoptaxis/nuoptaxis.c
 *                      nustar/tasks/nuoptaxis/nuoptaxis.h
 *                      nustar/tasks/nuoptaxis/nuoptaxis.par
 *                      nustar/tasks/nuoptaxis/nuoptaxis.html
 *                      nustar/tasks/Makefile
 *                   -- New task 'numkarf'
 *                   modules affected:
 *                      nustar/tasks/numkarf/Makefile
 *                      nustar/tasks/numkarf/numkarf.c
 *                      nustar/tasks/numkarf/numkarf.h
 *                      nustar/tasks/numkarf/numkarf.par
 *                      nustar/tasks/numkarf/numkarf.html
 *                      nustar/tasks/numkarf/ut_numkarf_fpma
 *                      nustar/tasks/numkarf/ut_numkarf_fpmb
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                   -- New task 'nuproducts'
 *                   modules affected:
 *                      nustar/tasks/nuproducts/Makefile
 *                      nustar/tasks/nuproducts/nuproducts
 *                      nustar/tasks/nuproducts/nuproducts.par
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                      nustar/tasks/nuproducts/ut_nuproducts_fpma
 *                      nustar/tasks/nuproducts/ut_nuproducts_fpmb
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                   -- Moved metrology processing from nucalcpos to numetrology new task
 *                   modules affected:
 *                      nustar/tasks/nucalcpos/nucalcpos.c
 *                      nustar/tasks/nucalcpos/nucalcpos.h
 *                      nustar/tasks/nucalcpos/nucalcpos.par
 *                      nustar/tasks/nucalcpos/nucalcpos.html
 *                      nustar/tasks/nucalcpos/ut_nucalcpos_fpma
 *                      nustar/tasks/nucalcpos/ut_nucalcpos_fpmb
 *                      nustar/tasks/nucoord/nucoord
 *                      nustar/tasks/nucoord/nucoord.par
 *                      nustar/tasks/nucoord/nucoord.html
 *                      nustar/tasks/nucoord/ut_nucoord_fpma
 *                      nustar/tasks/nucoord/ut_nucoord_fpmb
 *                      nustar/tasks/nupipeline/nupipeline
 *                   -- Added 'nuoptaxis' task call
 *                   modules affected:
 *                      nustar/tasks/nucoord/nucoord
 *                      nustar/tasks/nucoord/nucoord.par
 *                      nustar/tasks/nucoord/nucoord.html
 *                      nustar/tasks/nucoord/ut_nucoord_fpma
 *                      nustar/tasks/nucoord/ut_nucoord_fpmb
 *                   -- Added 'numetrology' task call;
 *                      Handle standard naming convention of input and output files
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpma
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpmb
 *      ###  BUILD 4  ###
 *      v.0.4.0 - 27/01/12
 *                   -- New task 'nuexpomap'
 *                   modules affected:
 *                      nustar/tasks/nuexpomap/nuexpomap.c
 *                      nustar/tasks/nuexpomap/nuexpomap.h
 *                      nustar/tasks/nuexpomap/nuexpomap.par
 *                      nustar/tasks/nuexpomap/ut_nuexpomap_fpma
 *                      nustar/tasks/nuexpomap/ut_nuexpomap_fpmb
 *                      nustar/tasks/nuexpomap/Makefile
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                   -- New task 'nuskypos'
 *                   modules affected:
 *                      nustar/tasks/nuskypos/nuskypos.c
 *                      nustar/tasks/nuskypos/nuskypos.h
 *                      nustar/tasks/nuskypos/nuskypos.par
 *                      nustar/tasks/nuskypos/Makefile
 *                      nustar/tasks/Makefile
 *                   -- Added 'nuproducts' task call
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpma
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpmb
 *                   -- Replaced 'nuoptaxis' with 'nuskypos' task call
 *                   modules affected:
 *                      nustar/tasks/nucoord/nucoord
 *                      nustar/tasks/nucoord/nucoord.par
 *                      nustar/tasks/nucoord/nucoord.html
 *                      nustar/tasks/nucoord/ut_nucoord_fpma
 *                      nustar/tasks/nucoord/ut_nucoord_fpmb
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpma
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpmb
 *                   -- Handle new Off-Axis Histogram file format;
 *                      Handle new algorithm for PSF correction
 *                   modules affected:
 *                      nustar/tasks/numkarf/numkarf.c
 *                      nustar/tasks/numkarf/numkarf.h
 *                      nustar/tasks/numkarf/numkarf.par
 *                      nustar/tasks/numkarf/numkarf.html
 *                      nustar/tasks/numkarf/ut_numkarf_fpma
 *                      nustar/tasks/numkarf/ut_numkarf_fpmb
 *      v.0.4.1 - 17/02/12
 *                   -- New task 'nulivetime'
 *                   modules affected:
 *                      nustar/tasks/nulivetime/nulivetime.c
 *                      nustar/tasks/nulivetime/nulivetime.h
 *                      nustar/tasks/nulivetime/nulivetime.par
 *                      nustar/tasks/nulivetime/Makefile
 *                      nustar/tasks/Makefile
 *                   -- Removed 'PHA' and 'SURR' columns in output evt file
 *                   modules affected:
 *                      nustar/tasks/nucalcpha/nucalcpha.c
 *                      nustar/tasks/nucalcpha/nucalcpha.h
 *                   -- Handle charge loss correction for PI computation
 *                   modules affected:
 *                      nustar/tasks/nucalcpi/nucalcpi.c
 *                      nustar/tasks/nucalcpi/nucalcpi.h
 *                      nustar/tasks/nucalcpi/nucalcpi.par
 *                      nustar/tasks/nucalcpi/nucalcpi.html
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                   -- Modified to read block of rows from input mastaspectfile
 *                   modules affected:
 *                      nustar/tasks/nucalcpos/nucalcpos.c
 *                      nustar/tasks/nucalcpos/nucalcpos.h
 *                   -- Update exposure map using 'DEADC' keyword value 
 *                   modules affected:
 *                      nustar/tasks/nuexpomap/nuexpomap.c
 *                      nustar/tasks/nuexpomap/nuexpomap.h
 *                   -- Handle extended sources while creating arf file
 *                   modules affected:
 *                      nustar/tasks/numkarf/numkarf.c
 *                      nustar/tasks/numkarf/numkarf.h
 *                      nustar/tasks/numkarf/numkarf.par
 *                      nustar/tasks/numkarf/numkarf.html
 *                   -- Added 'nulivetime' task call
 *                   modules affected:
 *                      nustar/tasks/nuscreen/nuscreen
 *                      nustar/tasks/nuscreen/nuscreen.par
 *                      nustar/tasks/nuscreen/nuscreen.html
 *                      nustar/tasks/nupipeline/nupipeline
 *      v.0.4.2 - 06/03/12
 *                   -- New task 'nuflagdepth'
 *                   modules affected:
 *                      nustar/tasks/nuflagdepth/nuflagdepth.c
 *                      nustar/tasks/nuflagdepth/nuflagdepth.h
 *                      nustar/tasks/nuflagdepth/nuflagdepth.par
 *                      nustar/tasks/nuflagdepth/ut_nuflagdepth_fpma
 *                      nustar/tasks/nuflagdepth/ut_nuflagdepth_fpmb
 *                      nustar/tasks/nuflagdepth/Makefile
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                   -- New task 'numkrmf'
 *                   modules affected:
 *                      nustar/tasks/numkrmf/numkrmf.c
 *                      nustar/tasks/numkrmf/numkrmf.h
 *                      nustar/tasks/numkrmf/numkrmf.par
 *                      nustar/tasks/numkrmf/ut_numkrmf_fpma
 *                      nustar/tasks/numkrmf/ut_numkrmf_fpmb
 *                      nustar/tasks/numkrmf/Makefile
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                   -- Removed task 'nuoptaxis'
 *                   modules affected:
 *                      nustar/tasks/nuoptaxis/Makefile
 *                      nustar/tasks/nuoptaxis/nuoptaxis.c
 *                      nustar/tasks/nuoptaxis/nuoptaxis.h
 *                      nustar/tasks/nuoptaxis/nuoptaxis.par
 *                      nustar/tasks/nuoptaxis/nuoptaxis.html
 *                      nustar/tasks/Makefile
 *                   -- Added Vignetting correction
 *                   modules affected:
 *                      nustar/tasks/nuexpomap/nuexpomap.c
 *                      nustar/tasks/nuexpomap/nuexpomap.h
 *                      nustar/tasks/nuexpomap/nuexpomap.par
 *                      nustar/tasks/nuexpomap/nuexpomap.html
 *                   -- Handle new format of ARF, 2D-PSF and vignetting CALDB files
 *                   modules affected:
 *                      nustar/tasks/numkarf/numkarf.c
 *                      nustar/tasks/numkarf/numkarf.h
 *                      nustar/tasks/numkarf/numkarf.par
 *                      nustar/tasks/numkarf/numkarf.html
 *                   -- Added 'numkarf' and 'numkrmf' call in 'nuproducts' task
 *                   modules affected:
 *                      nustar/tasks/nuproducts/nuproducts
 *                      nustar/tasks/nuproducts/nuproducts.par
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                   -- Added 'nuexpomap' and 'nuflagdepth' tasks call
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *      v.0.4.3 - 29/03/12
 *                   -- Handle events with TIME before mast aspect data
 *                   modules affected:
 *                      nustar/tasks/nucalcpos/nucalcpos.c
 *                   -- Modified to handle large input/output files;
 *                      Handle 'MET_CMP' extension of input metrology file
 *                   modules affected:
 *                      nustar/tasks/numetrology/numetrology.c
 *                      nustar/tasks/numetrology/numetrology.h
 *                   -- Bug fixed and minor changes
 *                      nustar/tasks/numkarf/numkarf.c
 *                      nustar/tasks/numkarf/numkarf.h
 *                   -- Added new keywords in output optaxisfile and det1reffile
 *                   modules affected:
 *                      nustar/tasks/nuskypos/nuskypos.c
 *                      nustar/tasks/nuskypos/nuskypos.h
 *                   -- Added new input parameters
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *      ###  BUILD 5  ###
 *      v.0.5.0 - 25/05/12
 *                   -- Added consistence check of input files
 *                   modules affected:
 *                      nustar/tasks/nucalcpi/nucalcpi.c
 *                      nustar/tasks/nucalcpi/nucalcpi.h
 *                      nustar/tasks/nucalcpos/nucalcpos.c
 *                      nustar/tasks/nucalcpos/nucalcpos.h
 *                      nustar/tasks/nucoord/nucoord
 *                      nustar/tasks/nulivetime/nulivetime.c
 *                      nustar/tasks/nulivetime/nulivetime.h
 *                   -- Handle bad pixels while creating output DET1 Instrument Map File;
 *                      handle azimuth angle in vignetting correction
 *                   modules affected:
 *                      nustar/tasks/nuexpomap/nuexpomap.c
 *                      nustar/tasks/nuexpomap/nuexpomap.h
 *                   -- Added 'psdcorfile' input parameter
 *                   modules affected:
 *                      nustar/tasks/nufilter/nufilter
 *                      nustar/tasks/nufilter/nufilter.par
 *                      nustar/tasks/nufilter/nufilter.html
 *                   -- Handle new keywords in BADPIX extension
 *                   modules affected:
 *                      nustar/tasks/nuflagbad/nuflagbad.c
 *                      nustar/tasks/nuhotpix/nuhotpix.c
 *                   -- Handle source azimuthal angle in PSF correction;
 *                      handle 'ANNULUS' and 'ELLIPSE' extraction region for extended="no";
 *                      added 'phibin' input parameter
 *                   modules affected:
 *                      nustar/tasks/numkarf/numkarf.c
 *                      nustar/tasks/numkarf/numkarf.h
 *                      nustar/tasks/numkarf/numkarf.par
 *                      nustar/tasks/numkarf/numkarf.html
 *                   -- Added 'phibin' input parameter
 *                   modules affected:
 *                      nustar/tasks/nuproducts/nuproducts
 *                      nustar/tasks/nuproducts/nuproducts.par
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                   -- Added 'cleancols' input parameter;
 *                      added screening of border events with grade=[24|25]
 *                   modules affected:
 *                      nustar/tasks/nuscreen/nuscreen
 *                      nustar/tasks/nuscreen/nuscreen.par
 *                      nustar/tasks/nuscreen/nuscreen.html
 *                   -- Modified nufilter and nuscreen task call;
 *                      handle standard naming convention for nuexpomap output files;
 *                      added 'phibin' input parameter
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *      ###  BUILD 6  ###
 *      v.0.6.0 - 27/07/12
 *                   -- Removed task 'nuflagdepth'
 *                   modules affected:
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                      nustar/tasks/nuflagdepth/Makefile
 *                      nustar/tasks/nuflagdepth/nuflagdepth.c
 *                      nustar/tasks/nuflagdepth/nuflagdepth.h
 *                      nustar/tasks/nuflagdepth/nuflagdepth.par
 *                      nustar/tasks/nuflagdepth/nuflagdepth.html
 *                   -- New task 'nuflagevt'
 *                   modules affected:
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                      nustar/tasks/nuflagevt/Makefile
 *                      nustar/tasks/nuflagevt/nuflagevt.c
 *                      nustar/tasks/nuflagevt/nuflagevt.h
 *                      nustar/tasks/nuflagevt/nuflagevt.par
 *                      nustar/tasks/nuflagevt/nuflagevt.html
 *                      nustar/tasks/nuflagevt/ut_nuflagevt_fpma
 *                      nustar/tasks/nuflagevt/ut_nuflagevt_fpmb
 *                   -- Update BADPIX extensions of output cleaned event file
 *                   modules affected:
 *                      nustar/tasks/nuscreen/nuscreen
 *                   -- Handle 'DEFAULT' value for input/output files;
 *                      New input parameters
 *                   modules affected:
 *                      nustar/tasks/nuproducts/nuproducts
 *                      nustar/tasks/nuproducts/nuproducts.par
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                      nustar/tasks/nupipeline/nupipeline
 *                   -- Replaced 'nuflagdepth' with 'nuflagevt' task;
 *                      Added 'fpma_evtcutfile' and 'fpmb_evtcutfile' input parameters;
 *                      Replaced 'runflagdepth' with 'runflagevt' input parameter;
 *                      Added "fpm[a|b]_hkrangefile[01|02|03|04|05]" and "gtiexpr[01|02|03|04|05]" input parameters;
 *                      Updated 'nuproducts' task call;
 *                      Added 'copyattitude' input parameter;
 *                      Handle "OBJECT" value for 'pntra' and 'pntdec' input parameters
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpma
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpmb
 *                   -- Corrected the scan of the PSF file extensions;
 *                      Improved performance if 'psfflag'="no";
 *                      Replaced 'fltime' ftool call with 'FilterTimesByGTI' routine
 *                   modules affected:
 *                      nustar/tasks/numkarf/numkarf.c
 *                      nustar/tasks/nuexpomap/nuexpomap.c
 *                      nustar/lib/misc/nu_misc.c
 *                      nustar/lib/misc/nu_misc.h
 *                   -- Bug fixed when creating 'outhpfile'
 *                   modules affected:
 *                      nustar/tasks/nuhotpix/nuhotpix.c
 *                   -- Add 'TUNIT*' keyword for 'X' and 'Y' columns
 *                   modules affected:
 *                      nustar/tasks/nucoord/nucoord
 *                   -- Add 'TLMIN*' and 'TLMAX*' keywords for GRADE column
 *                   modules affected:
 *                      nustar/tasks/nucalcpha/nucalcpha.c
 *      v.0.6.1 - 18/09/12
 *                   -- Handle long path in input/output file
 *                   modules affected:
 *                      nustar/tasks/nucoord/nucoord
 *                      nustar/tasks/nuexpomap/nuexpomap.c
 *                      nustar/tasks/nuexpomap/nuexpomap.h
 *                      nustar/tasks/nufilter/nufilter
 *                      nustar/tasks/numkarf/numkarf.c
 *                      nustar/tasks/numkarf/numkarf.h
 *                      nustar/tasks/numkrmf/numkrmf.c
 *                      nustar/tasks/numkrmf/numkrmf.h
 *                      nustar/tasks/nuproducts/nuproducts
 *                      nustar/tasks/nuscreen/nuscreen
 *                      nustar/lib/misc/nu_termio.c
 *                      nustar/lib/misc/nu_termio.h
 *                      nustar/lib/perl/libnustarperl.pl
 *                   -- Modified depth cut flagging conditions
 *                   modules affected:
 *                      nustar/tasks/nuflagevt/nuflagevt.c
 *                   -- Do not write rows with X_PSD* and/or Y_PSD* out of calibrated range in output corrected psd file
 *                   modules affected:
 *                      nustar/tasks/numetrology/numetrology.c
 *                   -- Modified default values of input parameters
 *                   modules affected:
 *                      nustar/tasks/numetrology/numetrology.html
 *                      nustar/tasks/numetrology/numetrology.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                      nustar/tasks/nuproducts/nuproducts.par
 *      ###  BUILD 7  ###
 *      v.0.7.0 - 08/10/12
 *                   -- New task 'nucalcsaa'
 *                   modules affected:
 *                      nustar/tasks/nucalcsaa/nucalcsaa.c
 *                      nustar/tasks/nucalcsaa/nucalcsaa.h
 *                      nustar/tasks/nucalcsaa/nucalcsaa.par
 *                      nustar/tasks/nucalcsaa/nucalcsaa.html
 *                      nustar/tasks/nucalcsaa/ut_nucalcsaa_fpma
 *                      nustar/tasks/nucalcsaa/ut_nucalcsaa_fpmb
 *                      nustar/tasks/nucalcsaa/Makefile
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                      nustar/lib/caldb/nu_caldb.h
 *                      nustar/lib/highfits/nu_defs.h
 *                   -- Added 'nucalcsaa' task call
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpma
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpmb
 *      v.0.7.1 - 15/10/12
 *                   -- New task 'nuskytodet'
 *                   modules affected:
 *                      nustar/tasks/nuskytodet/nuskytodet.c
 *                      nustar/tasks/nuskytodet/nuskytodet.h
 *                      nustar/tasks/nuskytodet/nuskytodet.par
 *                      nustar/tasks/nuskytodet/nuskytodet.html
 *                      nustar/tasks/nuskytodet/ut_nuskytodet_fpma
 *                      nustar/tasks/nuskytodet/ut_nuskytodet_fpmb
 *                      nustar/tasks/nuskytodet/Makefile
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *      ###  BUILD 8  ###
 *      v.0.8.0 - 30/10/12
 *                   -- New task 'nulccorr'
 *                   modules affected:
 *                      nustar/tasks/nulccorr/nulccorr.c
 *                      nustar/tasks/nulccorr/nulccorr.h
 *                      nustar/tasks/nulccorr/nulccorr.par
 *                      nustar/tasks/nulccorr/nulccorr.html
 *                      nustar/tasks/nulccorr/ut_nulccorr_fpma
 *                      nustar/tasks/nulccorr/ut_nulccorr_fpmb
 *                      nustar/tasks/nulccorr/Makefile
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                      nustar/lib/caldb/nu_caldb.h
 *                      nustar/lib/highfits/nu_defs.h
 *                   -- New task 'nuattcorr'
 *                   modules affected:
 *                      nustar/tasks/nuattcorr/nuattcorr.c
 *                      nustar/tasks/nuattcorr/nuattcorr.h
 *                      nustar/tasks/nuattcorr/nuattcorr.par
 *                      nustar/tasks/nuattcorr/nuattcorr.html
 *                      nustar/tasks/nuattcorr/ut_nuattcorr
 *                      nustar/tasks/nuattcorr/Makefile
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                      nustar/lib/caldb/nu_caldb.h
 *                      nustar/lib/highfits/nu_defs.h
 *                   -- Added 'nuattcorr' task call
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                   -- Added 'prefilter' ftool call
 *                   modules affected:
 *                      nustar/tasks/nufilter/nufilter
 *                      nustar/tasks/nufilter/nufilter.par
 *                      nustar/tasks/nufilter/nufilter.html
 *                   -- RMF Grouping File CALDB query dependent on 'DEPTHCUT' keyword value
 *                   modules affected:
 *                      nustar/tasks/numkrmf/numkrmf.c
 *                      nustar/tasks/numkrmf/numkrmf.h
 *                   -- Added 'nulccorr' task call in 'nuproducts' task
 *                   modules affected:
 *                      nustar/tasks/nuproducts/nuproducts
 *                      nustar/tasks/nuproducts/nuproducts.par
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                   -- Handle '!' option in the hkrangefile 'RANGE' column;
 *                      Added 'depthcut' input parameter;
 *                      Handle '06' observing mode
 *                   modules affected:
 *                      nustar/tasks/nuscreen/nuscreen
 *                      nustar/tasks/nuscreen/nuscreen.par
 *                      nustar/tasks/nuscreen/nuscreen.html
 *                   -- Added input parameters needed by new version of nufilter, nuproducts and nuscreen tasks;
 *                      Added 'nuattcorr' task call;
 *                      Removed 'copyattitude' input parameter;
 *                      Handle '06' observing mode;
 *                      Added "fpm[a|b]_hkrangefile06" and "gtiexpr06" input parameters
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *      v.0.8.1 - 07/11/12
 *                   -- Replaced 'rename' call with 'RenameFile' routine
 *                   modules affected:
 *                      nustar/lib/misc/nu_misc.c
 *                      nustar/lib/misc/nu_misc.h
 *                      nustar/tasks/nuattcorr/nuattcorr.c
 *                      nustar/tasks/nucalcsaa/nucalcsaa.c
 *                      nustar/tasks/nuflagevt/nuflagevt.c
 *                      nustar/tasks/nuflagbad/nuflagbad.c
 *                      nustar/tasks/nuhotpix/nuhotpix.c
 *                      nustar/tasks/numkarf/numkarf.c
 *                      nustar/tasks/numkrmf/numkrmf.c
 *                      nustar/tasks/nucalcpos/nucalcpos.c
 *                      nustar/tasks/nucalcpha/nucalcpha.c
 *                      nustar/tasks/nucalcpi/nucalcpi.c
 *                      nustar/tasks/nuexpomap/nuexpomap.c
 *                      nustar/tasks/nulccorr/nulccorr.c
 *      v.0.8.2 - 26/11/12
 *                   -- Bugs fixed
 *                   modules affected:
 *                      nustar/tasks/nulccorr/nulccorr.c
 *                      nustar/tasks/nucalcsaa/nucalcsaa.c
 *                      nustar/tasks/nucalcpi/nucalcpi.c
 *      ###  BUILD 9  ###
 *      v.0.9.0 - 14/12/12
 *                   -- New task 'nubkgcorr'
 *                   modules affected:
 *                      nustar/tasks/nubkgcorr/Makefile
 *                      nustar/tasks/nubkgcorr/nubkgcorr.c
 *                      nustar/tasks/nubkgcorr/nubkgcorr.h
 *                      nustar/tasks/nubkgcorr/nubkgcorr.par
 *                      nustar/tasks/nubkgcorr/nubkgcorr.html
 *                      nustar/tasks/nubkgcorr/ut_nubkgcorr_fpma
 *                      nustar/tasks/nubkgcorr/ut_nubkgcorr_fpmb
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                   -- Added 'skyx', 'skyy' and 'skysize' input parameters
 *                   modules affected:
 *                      nustar/tasks/nuexpomap/nuexpomap.c
 *                      nustar/tasks/nuexpomap/nuexpomap.h
 *                      nustar/tasks/nuexpomap/nuexpomap.html
 *                      nustar/tasks/nuexpomap/nuexpomap.par
 *                   -- Handle different extraction regions for extended="yes";
 *                      Handle new definition of PA_PNT keyword (PA_PNT_new=270-PA_PNT_old);
 *                      Handle new input parameters of nuexpomap task
 *                   modules affected:
 *                      nustar/tasks/numkarf/numkarf.c
 *                      nustar/tasks/numkarf/numkarf.h
 *                      nustar/tasks/numkarf/numkarf.par
 *                      nustar/tasks/numkarf/numkarf.html
 *                   -- Added 'cmprmf' input parameter
 *                   modules affected:
 *                      nustar/tasks/numkrmf/numkrmf.c
 *                      nustar/tasks/numkrmf/numkrmf.h
 *                      nustar/tasks/numkrmf/numkrmf.html
 *                      nustar/tasks/numkrmf/numkrmf.par
 *                   -- Added 'nubkgcorr' task call;
 *                      Added 'initseed', 'runbkgcorr' and 'cmprmf' input parameters
 *                   modules affected:
 *                      nustar/tasks/nuproducts/nuproducts
 *                      nustar/tasks/nuproducts/nuproducts.par
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                   -- Added input parameters needed by new version of nuexpomap task;
 *                      Added input parameters needed by new version of nuproducts task
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *      ###  BUILD 10  ###
 *      v.1.0.0 - 15/03/13
 *                   -- Handle '--enable-mac_32bit' configuartion option
 *                   modules affected:
 *                      nustar/BUILD_DIR/configure
 *                      nustar/BUILD_DIR/configure.in
 *                      nustar/BUILD_DIR/hd_config_info
 *                   -- Added 'usrgtifile' input parameter
 *                   modules affected:
 *                      nustar/tasks/nubkgcorr/nubkgcorr.c
 *                      nustar/tasks/nubkgcorr/nubkgcorr.h
 *                      nustar/tasks/nubkgcorr/nubkgcorr.par
 *                      nustar/tasks/nubkgcorr/nubkgcorr.html
 *                   -- Modified 'phi' computation in vignetting correction
 *                   modules affected:
 *                      nustar/tasks/nuexpomap/nuexpomap.c
 *                   -- Add new columns in output optaxisfile
 *                   modules affected:
 *                      nustar/tasks/nuskypos/nuskypos.c
 *                      nustar/tasks/nuskypos/nuskypos.h
 *                      nustar/tasks/nuskypos/nuskypos.html
 *                   -- Handle Aperture Stop correction;
 *                      Handle Ghost Rays correction;
 *                      Modified 'phi_det1' computation in vignetting correction
 *                   modules affected:
 *                      nustar/tasks/numkarf/Makefile
 *                      nustar/tasks/numkarf/numkarf.c
 *                      nustar/tasks/numkarf/numkarf.h
 *                      nustar/tasks/numkarf/numkarf.par
 *                      nustar/tasks/numkarf/numkarf.html
 *                   -- Skip 'addrmf' run in the case of a single rmf file
 *                   modules affected:
 *                      nustar/tasks/numkrmf/numkrmf.c
 *                      nustar/tasks/numkrmf/numkrmf.h
 *                   -- Modified 'xspec', 'xselect' and 'ximage' ftool call;
 *                      Added input parameters needed by new version of numkarf task;
 *                      Added input parameters needed by new version of nubkgcorr task;
 *                   modules affected:
 *                      nustar/tasks/nuproducts/nuproducts
 *                      nustar/tasks/nuproducts/nuproducts.par
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                   -- Update 'CALDBVER' keyword in output event files;
 *                      Added input parameters needed by new version of nuproducts task.
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *      v.1.0.1 - 21/03/13
 *                   -- Modified default value of 'apstoflag' and 'grflag' input parameters
 *                   modules affected:
 *                      nustar/tasks/numkarf/numkarf.par
 *                      nustar/tasks/numkarf/numkarf.html
 *                      nustar/tasks/nuproducts/nuproducts.par
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                   -- Modified default value of 'arfapstoflag' and 'arfgrflag' input parameters
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *      ###  BUILD 11  ###
 *      v.1.1.0 - 24/04/13
 *                   -- Handle rows with X_PSD* and/or Y_PSD* out of calibrated range in output corrected psd file
 *                   modules affected:
 *                      nustar/tasks/numetrology/numetrology.c
 *                      nustar/tasks/numetrology/numetrology.h
 *                   -- Handle DETABS correction
 *                   modules affected:
 *                      nustar/tasks/numkarf/numkarf.c
 *                      nustar/tasks/numkarf/numkarf.h
 *                      nustar/tasks/numkarf/numkarf.par
 *                      nustar/tasks/numkarf/numkarf.html
 *                   -- Added input parameters needed by new version of numkarf task
 *                   modules affected:
 *                      nustar/tasks/nuproducts/nuproducts
 *                      nustar/tasks/nuproducts/nuproducts.par
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                      Added input parameters needed by new version of nuproducts task.
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *      v.1.1.1 - 09/05/13
 *                   -- Modified default values of input parameters
 *                   modules affected:
 *                      nustar/tasks/numkarf/numkarf.par
 *                      nustar/tasks/numkarf/numkarf.html
 *                      nustar/tasks/nuproducts/nuproducts.par
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *      ###  BUILD 12  ###
 *      v.1.2.0 - 14/06/13
 *                   -- Removed task 'nubkgcorr'
 *                   modules affected:
 *                      nustar/tasks/nubkgcorr/Makefile
 *                      nustar/tasks/nubkgcorr/nubkgcorr.c
 *                      nustar/tasks/nubkgcorr/nubkgcorr.h
 *                      nustar/tasks/nubkgcorr/nubkgcorr.par
 *                      nustar/tasks/nubkgcorr/nubkgcorr.html
 *                      nustar/tasks/nubkgcorr/ut_nubkgcorr_fpma
 *                      nustar/tasks/nubkgcorr/ut_nubkgcorr_fpmb
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                   -- New task 'nubackscale'
 *                   modules affected:
 *                      nustar/tasks/nubackscale/Makefile
 *                      nustar/tasks/nubackscale/nubackscale.c
 *                      nustar/tasks/nubackscale/nubackscale.h
 *                      nustar/tasks/nubackscale/nubackscale.par
 *                      nustar/tasks/nubackscale/nubackscale.html
 *                      nustar/tasks/nubackscale/ut_nubackscale_fpma
 *                      nustar/tasks/nubackscale/ut_nubackscale_fpmb
 *                      nustar/tasks/Makefile
 *                      nustar/tasks/ut_nustarALL
 *                   -- Added input parameters needed by new version of nuproducts task
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpma
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpmb
 *                   -- Replaced 'nubkgcorr' with 'nubackscale' task;
 *                      Added input parameters needed by new version of nulccorr task
 *                   modules affected:
 *                      nustar/tasks/nuproducts/nuproducts
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                      nustar/tasks/nuproducts/nuproducts.par
 *                      nustar/tasks/nuproducts/ut_nuproducts_fpma
 *                      nustar/tasks/nuproducts/ut_nuproducts_fpmb
 *                   -- Handle PSF and EXPOSURE correction
 *                   modules affected:
 *                      nustar/tasks/nulccorr/nulccorr.c
 *                      nustar/tasks/nulccorr/nulccorr.h
 *                      nustar/tasks/nulccorr/nulccorr.par
 *                      nustar/tasks/nulccorr/nulccorr.html
 *                      nustar/tasks/nulccorr/ut_nulccorr_fpma
 *                      nustar/tasks/nulccorr/ut_nulccorr_fpmb
 *                   -- Handle attitude reprocessing
 *                   modules affected:
 *                      nustar/tasks/nuattcorr/nuattcorr.c
 *                   -- Bug fixed while using 'usrgtifile' input file
 *                   modules affected:
 *                      nustar/tasks/nuscreen/nuscreen
 *                   -- Modified default value of 'grflag' input parameter;
 *                      Handle memory allocation failure
 *                   modules affected:
 *                      nustar/tasks/numkarf/numkarf.c
 *                      nustar/tasks/numkarf/numkarf.par
 *                      nustar/tasks/numkarf/numkarf.html
 *                   -- Added 'usrgtifile' input parameter
 *                   modules affected:
 *                      nustar/tasks/numkrmf/numkrmf.c
 *                      nustar/tasks/numkrmf/numkrmf.h
 *                      nustar/tasks/numkrmf/numkrmf.par
 *                      nustar/tasks/numkrmf/numkrmf.html
 *      v.1.2.1 - 23/07/13
 *                   -- Handle new input parameters of nuexpomap task
 *                   modules affected:
 *                      nustar/tasks/nubackscale/nubackscale.c
 *                      nustar/tasks/nubackscale/nubackscale.h
 *                      nustar/tasks/nubackscale/nubackscale.par
 *                      nustar/tasks/nubackscale/nubackscale.html
 *                   -- Modified algorithm to handle bad pixels while creating output DET1 Instrument Map File
 *                   modules affected:
 *                      nustar/tasks/nuexpomap/nuexpomap.c
 *                      nustar/tasks/nuexpomap/nuexpomap.h
 *                      nustar/tasks/nuexpomap/nuexpomap.html
 *                      nustar/tasks/nuexpomap/nuexpomap.par
 *                   -- Handle energy-dependent CALDB query for 2D-PSF file;
 *                      Handle new input parameters of nuexpomap task
 *                   modules affected:
 *                      nustar/tasks/nulccorr/nulccorr.c
 *                      nustar/tasks/nulccorr/nulccorr.h
 *                      nustar/tasks/nulccorr/nulccorr.par
 *                      nustar/tasks/nulccorr/nulccorr.html
 *                   -- Handle energy-dependent 2d-PSF file in PSF correction;
 *                      Handle new input parameters of nuexpomap task
 *                   modules affected:
 *                      nustar/tasks/numkarf/numkarf.c
 *                      nustar/tasks/numkarf/numkarf.h
 *                      nustar/tasks/numkarf/numkarf.par
 *                      nustar/tasks/numkarf/numkarf.html
 *                   -- Added input parameters needed by new version of nulccorr, numkarf and nubackscale tasks
 *                      nustar/tasks/nuproducts/nuproducts
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                      nustar/tasks/nuproducts/nuproducts.par
 *                   -- Added input parameters needed by new version of nuproducts and nuexpomap task
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                      nustar/tasks/nupipeline/nupipeline.par
 *      ###  BUILD 13  ###
 *      v.1.3.0 - 07/10/13
 *                   --  Handle bad values of 'grflag' and 'psfflag' input parameters for extended sources 
 *                   modules affected:
 *                      nustar/tasks/numkarf/numkarf.c
 *                   -- Added 'psdcal' input parameter
 *                   modules affected:
 *                      nustar/tasks/numetrology/numetrology.c
 *                      nustar/tasks/numetrology/numetrology.h
 *                      nustar/tasks/numetrology/numetrology.par
 *                   -- Added computation of out of calibrated PSD grid exposure time
 *                   modules affected:
 *                      nustar/tasks/nuscreen/nuscreen
 *                   -- Added input parameters needed by new version of numetrology task;
 *                      Added reporting of the Metrology laser spots out of calibrated PSD grid time.
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                      nustar/tasks/nupipeline/nupipeline.html
 *      v.1.3.1 - 09/12/13
 *                   -- Set DEADC keyword to 1.0 in output corrected light curve
 *                   modules affected:
 *                      nustar/tasks/nulccorr/nulccorr.c
 *                      nustar/tasks/nulccorr/nulccorr.html
 *                   -- Added 'barycorr' ftools call;
 *                      Added PI filtering command in xselect execution
 *                   modules affected:
 *                      nustar/tasks/nuproducts/nuproducts
 *                      nustar/tasks/nuproducts/nuproducts.par
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                   -- Added input parameters needed by new version of nuproducts task
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                      nustar/tasks/nupipeline/nupipeline.par
 *      ###  BUILD 14  ###
 *      v.1.4.0 - 15/04/14
 *                   -- Added 'inexpomapfile' input parameter;
 *                      Handle compressed source and background PHA input files
 *                   modules affected:
 *                      nustar/tasks/nubackscale/nubackscale.c
 *                      nustar/tasks/nubackscale/nubackscale.h
 *                      nustar/tasks/nubackscale/nubackscale.par
 *                      nustar/tasks/nubackscale/nubackscale.html
 *                   -- Adjusted how grade-gain is applied for grade 0 and grades 9-20
 *                   modules affected:
 *                      nustar/tasks/nucalcpi/nucalcpi.c
 *                   -- Added 'indet2instrfile' input parameter;
 *                      Improved performances while executing 'fchecksum' of output sky instrument map file;
 *                      Handle compressed DET1 Reference Pixel input file
 *                   modules affected:
 *                      nustar/tasks/nuexpomap/nuexpomap.c
 *                      nustar/tasks/nuexpomap/nuexpomap.h
 *                      nustar/tasks/nuexpomap/nuexpomap.html
 *                      nustar/tasks/nuexpomap/nuexpomap.par
 *                   -- Added 'inskyinstrfile' and 'inaspecthistofile' input parameters
 *                   modules affected:
 *                      nustar/tasks/nulccorr/nulccorr.c
 *                      nustar/tasks/nulccorr/nulccorr.h
 *                      nustar/tasks/nulccorr/nulccorr.par
 *                      nustar/tasks/nulccorr/nulccorr.html
 *                   -- Handle compressed input event file
 *                   modules affected:
 *                      nustar/tasks/nulivetime/nulivetime.c
 *                   -- Added 'inexpomapfile' input parameter;
 *                      Handle compressed Optical Axis input file
 *                   modules affected:
 *                      nustar/tasks/numkarf/numkarf.c
 *                      nustar/tasks/numkarf/numkarf.h
 *                      nustar/tasks/numkarf/numkarf.par
 *                      nustar/tasks/numkarf/numkarf.html
 *                   -- Handle compressed RMF input file
 *                   modules affected:
 *                      nustar/tasks/numkrmf/numkrmf.c
 *                   -- Added 'grppha' ftool call;
 *                      Added SOFTVER and CALDBVER info in output files;
 *                      Added 'nuexpomap' task call;
 *                      Handle 'nuexpomap' task output files while calling 'nubackscale', 'nulccorr' and 'numkarf' tasks
 *                   modules affected:
 *                      nustar/tasks/nuproducts/nuproducts
 *                      nustar/tasks/nuproducts/nuproducts.par
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                   -- Handle compressed TLE input file;
 *                      Removed use of deprecated 'define' PERL function
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *      v.1.4.1 - 28/05/14
 *                   -- Added 'usrgtibarycorr' input parameter
 *                   modules affected:
 *                      nustar/tasks/nuproducts/nuproducts
 *                      nustar/tasks/nuproducts/nuproducts.par
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                   -- Added input parameters needed by new version of nuproducts task
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                      nustar/tasks/nupipeline/nupipeline.par
 *      ###  BUILD 15  ###
 *      v.1.5.0 - 28/05/15
 *                   -- Bug fixed while using remote CALDB
 *                   modules affected:
 *                      nustar/tasks/numkrmf/numkrmf.c
 *                      nustar/lib/misc/nu_misc.c
 *                      nustar/lib/misc/nu_misc.h
 *                   -- Modified default values of 'pilow' and 'pihigh' input parameters
 *                   modules affected:
 *                      nustar/tasks/nuproducts/nuproducts.par
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                      nustar/tasks/nupipeline/nupipeline.par
 *                   -- Handle empty Level 2 Event File;
 *                      Update 'CALDBVER' keyword also in EVENTS ext of the output event files
 *                   modules affected:
 *                      nustar/tasks/nupipeline/nupipeline
 *                   -- Added 'pilowarf', 'pihigh' and 'flatflag' input parameters
 *                   modules affected:
 *                      nustar/tasks/numkarf/numkarf.c
 *                      nustar/tasks/numkarf/numkarf.h
 *                      nustar/tasks/numkarf/numkarf.par
 *                      nustar/tasks/numkarf/numkarf.html
 *                   -- Added input parameters needed by new version of numkarf task
 *                      nustar/tasks/nuproducts/nuproducts
 *                      nustar/tasks/nuproducts/nuproducts.par
 *                      nustar/tasks/nuproducts/nuproducts.html
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/nupipeline.html
 *                      nustar/tasks/nupipeline/nupipeline.par
 *      v.1.5.1 - 09/06/15
 *                   -- Bug fixed in 'addarf' execution when using flat distribution
 *                   modules affected:
 *                      nustar/tasks/numkarf/numkarf.c
 *                      nustar/tasks/numkarf/numkarf.h
 *      ###  BUILD 16  ###
 *      v.1.6.0 - 14/04/16
 *                   -- New SAA calculation algorithm Strict/Optimized/Tentacle, currently debugging.
 *                   modules affected:
 *                      nustar/tasks/nucalcsaa/nucalcsaa.c
 *                      nustar/tasks/nucalcsaa/nucalcsaa.h 
 *                      nustar/tasks/nucalcsaa/nucalcsaa.par 
 *                   -- Add nusplitsc task
 *                   modules affected:
 *                      nustar/tasks/nusplitsc/nusplitsc.c 
 *                      nustar/tasks/nusplitsc/nusplitsc.h 
 *                      nustar/tasks/nusplitsc/nusplitsc.par
 *                      nustar/tasks/nusplitsc/ut_nusplitsc_fpma
 *                      nustar/tasks/nusplitsc/ut_nusplitsc_fpma
 *                   -- Bug Fix nuflagbad
 *                   modules affected:
 *                      nustar/tasks/nuflagbad/nuflagbad.c  
 *                   -- Bug Fix nuhotpix
 *                   modules affected:
 *                      nustar/tasks/nuhotpix/nuhotpix.c 
 *                   -- nufilter Added key 'MJFREFI' and 'MJFREFF'in extension +0 and +1 on 'attfile'                
 *                   modules nufilter:
 *                      nustar/tasks/nufilter/nufilter
 *                   -- nuscreen Added key 'OBSMODE' in 'EVENTS' extension                 
 *                   modules nuscreen:
 *                      nustar/tasks/nuscreen/nuscreen        
 *                   -- Modify ut_nustarALL, add ut_nusplitsc_fpma ut_nusplitsc_fpmb 
 *                   modules ut_nustarALL:
 *                      nustar/tasks/ut_nustarALL  
 *                   -- Added input parameters needed by new version of nucalcsaa task and new task nusplitsc                      
 *                   modules nupipeline:
 *                      nustar/tasks/nupipeline/nupipeline                        
 * 
 * 
 *      ###  BUILD 17  ###
 *      v.1.7.0 - 06/10/16
 *                   -- Internal run of fselect disabled for the case saamode=NONE & tentacle=no
 *                   modules affected:
 *                      nustar/tasks/nucalcsaa/nucalcsaa.c
 *                      nustar/tasks/nucalcsaa/nucalcsaa.par 
 *                   -- bug corrected when reading a string with ‘&’ characters in the input parameter ‘statusexpr’
 *                   modules affected: 
 *                      nustar/tasks/nuscreen/nuscreen
 *                   -- Handling of multiple TLE files in the archive
 *                   -- bug corrected when reading a string with ‘&’ characters in the input parameter ‘statusexpr’
 *                   -- bug corrected for the case runmetrology=no: searching mast aspect and psdcor files in the ‘event_cl’ directory (was in the ‘auxil’ one)
 *                   modules affected: 
 *                      nustar/tasks/nupipeline/nupipeline
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpma
 *                      nustar/tasks/nupipeline/ut_nupipeline_fpmp
 * 
 *      ###  BUILD 17  ###
 *      v.1.7.1 - 06/12/16
 *                   -- Added parameter timeadj=DEFAULT to the call task prefilter 
 *                   modules affected:
 *                     nustar/tasks/nufilter/nufilter
 * 
 *	DEPENDENCIES:
 *
 *	<none>
 */
void GetNuSTARDASVersion(Version_t verstr)
{
  strcpy(verstr,"nustardas_06Dec16_v1.7.1");

} /* GetNuSTARDASVersion */



/*
 *	SYNOPSIS:
 *		void GetFTOOLSVersion(Version_t verstr)
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



