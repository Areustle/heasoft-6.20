C      SUBROUTINE INFO(input)
C
C
C  $Id: info.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=======================================================================
c     Effect: Provide on-line help for LIKE program
c
c-----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     character input*50  Command text typed by user on console
c
c-----------------------------------------------------------------------
c  LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
c            Updated: by JRM
c
c=======================================================================
C  $Log: info.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2006/04/17 20:54:28  irby
C  Updates for compliance with g95 and gfortran:
C
C  - Replace call to lnblnk with equivalent call to len_trim.
C    lnblnk (a routine which used to live in libg2c) is not currently
C    available with g95 or gfortran.
C
C  - Change calls to "perror" (also libg2c) to fcerr or c_fcerr.
C
C  - Change calls to IDATE (libg2c) to new libgro routine GIDATE.
C
C  - Fix non-integer loop variables.
C
C  Revision 1.1  2002/04/16 20:27:32  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.4  1996/07/29  17:04:45  jae
c fixed line near MF switch to be within char*72
c
c Revision 5.3  1996/07/19  15:52:47  jae
c Repaired typo (else -> elseif) on line 232
c
c Revision 5.2  1996/07/18  15:37:33  jae
c Repaired error on line 152 reported by cvm and
c updated with new command listings
c
c Revision 5.1  1996/02/29  20:48:08  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:37:30  jae
c Subroutine Module for like V5.00
c
c   Changes:                                                           
c
c-----------------------------------------------------------------------
      SUBROUTINE INFO(input)
C  Common blocks included
       include '../COMMON/cnfrep.copy'
        INCLUDE  '../COMMON/errrep.copy'

        save

	character(80) id
	common /id/id
	character input*50,input2*100
	id = '$Id: info.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
	LOC='INFO'
        nnumcar = len_trim(input)
	     do k=1,nnumcar
          	if(ICHAR(input(k:k)).ge.97.and.
     & ICHAR(input(k:k)).le.122)input(k:k) = char(ICHAR(input(k:k))-32)
             enddo
	 if(input(1:3).eq.'CTL')goto 101
         IF (input(2:2).EQ.' '.or.  
     &   input(2:4).EQ.'ELP') THEN
           WRITE(6,'('' EGRET SOURCE Likelihood analysis:'',
     &               '' Choose A Function: '',/,
     1 '' AO= adjust auto-output flag (*);'',/,
     1 '' B = evaluate GAS Gbias (*);'',/,
     1 '' C = evaluate counts at a specified point (*);'',/,
     1 '' COR reset PSF counts to ~0 and set PSF map to null;'',/,
     1 '' E = Re-select FITS energy range;'',/,
     1 '' G = Evaluate GAS Gmult (*);'',/,
     1 '' L = Evaluate likelihood  (*);'',/,
     1 '' M = Produce likelihood TS or parameter map (*);'',/,
     1 '' O = Output maps or profile plot (*);'',/,
     1 '' P = Adjust PSF parameters (*);'',/,
     1 '' R = Reset PSF parameters (*);'',/,
     1 '' Q = Quit;'',/,
     1 '' S = Specify ROI range;'',/,
     1 '' Z = Read LIKE commands from a script;'',/,
     1 '' ? = Print this Menu.'',/,
     1 '' Functions followed by (*) have more on-line information'',/,
     1 '' which may be obtained with ?#, where # is the'',/,
     1 '' function of interest.'')')

	  ELSEIF (input(2:3).EQ.'AO') THEN
           WRITE(6,'(
     &     "AO command toggles auto-output flag.",/,
     &     "AOL to view current value of auto-output flag.",/)')
	  ELSEIF (input(2:2).EQ.'C') THEN
           WRITE(6,'(
     &     "Cxx commands are used to evaluate source counts.",/,
     &     "C is used to evaluate Counts with other param. fixed.",/,
     &     "CC causes execution with extant parameters.",/,
     &     "CB is used to evaluate Counts and Gbias simultaneously.",/,
     &     "CBB for eval of Counts and Gbias with extant parameters.",/,
     &     "COR is used to set PSF counts to ~0 and PSF map to null")')
	  ELSEIF (input(2:2).EQ.'B') THEN
           WRITE(6,'(
     &     "Bx commands are used to evaluate Gbias.",/,
     &     "B is used to evaluate Gbias with other param. fixed.",/,
     &     "BB causes execution with extant parameters.")')
	  ELSEIF (input(2:2).EQ.'G') THEN
           WRITE(6,'(
     &     "Gxx commands are used to evaluate Gmult. ",/,
     &     "G is to fit Gmult. with other param. fixed",/,
     &     "GG causes execution with extant parameters.",/,
     &     "GB is used to evaluate Gmult and Gbias simultaneously.",/,
     &     "GBB for eval of Gmult and Gbias with extant parameters.",/,
     &     "GBC is for a simultaneous fit of Gmult, Gbias, & Counts.",/,
     &     "GC is used to evaluate Gmult and Counts simultaneously.")')
	  ELSEIF (input(2:2).EQ.'J') THEN
           WRITE(6,'("Restart the Likelihood session")')
	  ELSEIF ((input(2:2).EQ.'L'.and.input(3:3).NE.'P')) THEN
           WRITE(6,'(
     &     "Lx is for a likelihood ratio test.",/,
     &     "L allows for position adjustment.",/,
     &     "LC will do likelihood ratio test for input catalog.",/,  
     &     "LE will do likelihood position analysis.",/,  
     &     "LL causes execution with extant position.",/,
     &     "LM will simultaneously solve for Gm, Gb, and mult. PSFs.",
     &      /,"LN test sig. of the current source as an additional",
     &     " source.",/,
     & "LPx[y..[z..]] invokes likelihood position (LP)",
     & " estimate commands,",/,"	error analysis and",
     & " parameter controll")')
	  ELSEIF (input(2:3).EQ.'LP') THEN
	    nnumcar = len_trim(input)
           if(nnumcar.eq.3)WRITE(6,'(
     & "LPx[y..[z..]] invokes likelihood position (LP)",
     & " estimate commands,",/,"	error analysis and",
     & " parameter controll",/,
     & " LPE[x] find 68% and 95% errors for single source",/,
     & "        or active PSF",/,
     & " LPF[x] find max TS for sources in file.",/,
     & " LPO[x] optimize max TS positions for active PSF.",/,
     & " LPI	Enter LPI submenu to issue a LPI sub-command",/,
     & " LPM[x] create fine map for single source or active PSF",/,
     & " LPR[x] create report for all source locations in PSF map.",/,
     & " LPS[x] find max TS for single a input position.")')
	   if(nnumcar.gt.3)then
	     write(6,*)' '
	     if(input(4:4).eq.'S')then
		write(6,'("LPS:  determines max TS position for an",/,
     &			  "      extant test source",/,
     &			  "LPSN: Same as LPS with calculation of a",/,
     &			  "      GRO J2000 sourcename",/,
     &			  "LPS&: Same as LPSN with extant source",/,
     &			  "      added to the PSF source map",/,/)')
	     elseif(input(4:4).eq.'O')then
		write(6,'("LPO:  calculates max TS position for all",/,
     &			  "      active test sources",/,
     &			  "LPON: Same as LPO with calculation of a",/,
     &			  "      GRO J2000 sourcename for each active",/,
     &			  "      Source",/,/)')
	     elseif(input(4:4).eq.'E')then
		write(6,'("LPE:  performs error analysis on an",/,
     &			  "      extant test source",/,
     &			  "LPEA: performs error analysis on all",/,
     &			  "      active PSF sources",/,/)')
	     elseif(input(4:4).eq.'F')then
		write(6,'("LPF:  Reads a PSF file or (x,y) file",/,
     &			  "      and adds the sources to the PSF map",/,
     &			  "      after position optimization",/,
     &			  "LPFR: Same as LPF except if the file is a",/,
     &			  "      PSF file then the sources are added",/,
     &			  "      to the PSF map without position",/,
     &			  "      optimization",/,/)')
	     elseif(input(4:4).eq.'M')then
		write(6,'("LPM:  Outputs a finemap with GRO_J2000 name",/,
     &			  "      for an extant test source",/,
     &			  "LPMA: Outputs a finemap with GRO_J2000 name",/,
     &			  "      for all active PSF sources",/,/)')
	     elseif(input(4:4).eq.'M')then
		write(6,'("LPR:  Outputs a likelihood analysis report",/,
     &			  "      for all sources in the PSF map",/,
     &			  "LPRA: Outputs a likelihood analysis report",/,
     &			  "      for all active PSF sources",/,/)')
	     elseif(input(4:4).eq.'I')then
6257		write(6,
     & '("LPI: Invokes a sub-menu with the following",/,
     & " available commands:",/,/)')
         write(*,'(" A[] to change activity value;")')
         write(*,'(" B   to change AMOEBA ",
     & "angular convergence tolerance;")')
         write(*,'(" C   to change JLOC_POS ",
     & "angular convergence tolerance;")')
         write(*,'(" D   to issue a system command;")')
         write(*,'(" F   to find source ID from source list file;")')
         write(*,'(" I[nnn] to list counts map Filename and ",
     & "Energy range;")')
         write(*,'(" M   to run automated mapping survey;")')
         write(*,'(" N   to change name;")')
         write(*,'(" O[] for output of automated residual map ",
     & "generation;")')
         write(*,'(" P   to ID position;")')
         write(*,'(" Q   or <cr> to quit;")')
         write(*,'(" R   to READ(FILE) LPEA input;")')
         write(*,'(" S[] to sort PSF array;")')
         write(*,'(" T   to change minimum acceptable TS value;")')
         write(*,'(" U[] to unidentify a PSF ID;")')
         write(*,'(" W   to WRITE(FILE) LPEA Output;")')
         write(*,'(" X[] to delete PSFs with TS below minimum value;")')
         write(*,'(" Z   to change zenith aspect cone value;",/)')
c	 write(*,'("Enter sub-command for more information, <cr> or Q",
c     & " to exit LPI help:"$)')
		write(6,'(" ")')
	     endif
	    endif
	  ELSEIF (input(2:2).EQ.'M') THEN
           WRITE(6,'(
     &     "Mx commands are for mapping.",/,
     &     "MS will make a map of the likelihood TS, ",
     &      " Gmult, and Gbias);",/,
     &     "MG will make a map of Gmultand Counts with fixed Gbias;",/,
     &     "MH will make a TS, Gbias, and Counts map with fixed Gmult;",
     &      /,"MF will make a fine map of likelihood.")')
	  ELSEIF (input(2:2).EQ.'O') THEN
           WRITE(6,'(
     &    "Oxx commands are for output.",/,
     &    "OMRC writes residual counts (EGRET counts-model);",/,
     &    "OMRF will write the residual flux: res. cnts/exposure;",/,
     &    "OMR writes the normalized residual: res. cnts/sqrt(cnts);",/,
     &     "OMG will write the diffuse model as flux;",/,
     &     "OMGC will write the diffuse model as counts;",/,
     &     "OMC will write the counts map;",/,
     &     "OME will write the exposure map.",/,
     &     "OP will produce a profile plot for the ROI.")')
	  ELSEIF (input(2:2).EQ.'P') THEN
           WRITE(6,'(
     &    "Px commands adjust parameters.",/,
     &    "PA will invoke menu to adjust the other PSF map.",/,
     &    "PB will reset, or add Gbias to the diffuse model.",/,
     & "PG will adjust the spectral index used to construct the PSF",/,
     &    "PR will adjust range of likelihood analysis (Ranal).",/,
     &    "PMx allows for miscellaneous parameter adjustment:",/,
     &    " PMC to change coordinate system label.",/,
     &    " PMF to read an array into the PSF array.",/,
     &    " PML to change limit confidence.",/,
     &    " PMM to change map output files.",/,
     &    " PMN to adjust Gmult_nom or Gbias_nom.",/,
     &    " PMP to toggle the PUBLISH flag.",/,
     &    " PMV to toggle VERBOSE flag.",/,
     &    " PMR to change parameter restrictions.",/,
     &    " PMS to toggle  SPECTRAL flag.",/,
     &    " PMT to adjust TS_min & TS_max.",/,
     &    " PMX to change Counts, Gmult, Gbias, or position",$)')
	  ELSEIF(input(2:2).eq.'R')then
           WRITE(6,'(
     &    "Rx commands reset PSF parameters.",/,
     &    "RA will reset activity value prior to last optimization.",/,
     &    "RC will reset counts value prior to last optimization.",/,
     &    "RF will reset all values prior to last optimization.",/,
     &    "RP will reset position value prior to last optimization.",/,
     &    "RS will reset spectral index value prior to",
     & " last optimization.",/)')
	  ELSE
           WRITE(6,'("The only help for ",a,
     &               " currently available is to be obtained ",
     &               "with ? alone.")') input(2:2)
         ENDIF ! End specific function help.
      RETURN
101     in_dex1 = index(MISC_DIR, ' ') - 1
	if(in_dex1.lt.1)in_dex1=1
	input2 = 
     & 'more '//MISC_DIR(1:in_dex1)//'/LIKEHTML/like_CTL_desc.txt'
	call system(input2)
	return
      END
