C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
C        SUBROUTINE INPUT_ID_POS(input)
C
C
C  $Id: input_id.f,v 1.5 2013/05/21 19:08:25 irby Exp $
C======================================================================
C*     Effect: Upon user request psfrep.copy and locpos.copy values are
c*	       adjusted.  See below argument definitions for a Table of
c*	       subcommand effect.
C*             
c
c----------------------------------------------------------------------
c     	Subroutine Argument Desriptions
c   	^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c 	character(50) 	INPUT(1:8)      value 'LPI[D  n]' where n is
c					an optional subcommand.  The
c					characters LPI are not optional.
c					If D is specified the routine
c					looks at input(6:6) for subcommand
c					character.  Note that LPMS will
c					also call this routine with the
c					effect of two distinct calls:
c					'LPID#m' followed by 'LPID#s'
c
c-----------TABLE 1. Subcommand values and effect
c value 	effect					affected parameter
c
c   A[]		change activity value			SCR_PARMS(8,n)
c   B  		change AMOEBA convergence tolerance	tolj
c   C  		change JLOC_POS convergence tolerance	actj_flg
c   D  		issue a system command			-none-
c   F  		find source ID from source list file	-several-
c   I           list Map Filenames and Energy range     -none-
c   M  		run automated mapping survey		-several-
c   N  		change name				SCR_NAMES(n)
c   O[]		Output residual maps			-none-
c   P  		ID position				-several-
c   Q or <cr> 	quit ( return to main )			-none-
c   R  		READ(FILE) LPEA style input file	-ALL arrays-
c   S[]		sort PSF sources by GRO J2000 name	-ALL arrays-
c   T  		change minimum acceptable TS value	psfminj
c   U  		unidentify a source ID			-several-
c   W  		WRITE(FILE) LPEA style output file	-none-
c   X[]		delete sources if TS < psfminj		-ALL arrays-
c		within user defined region
c   Z[]		change zenith aspect cone value		aspj
c
c------------Table 2. Subcommand switches
c Subcommand	switch	Effect/Notes
c
c   A		I	Set identified sources to activity
c		U	Set unidentified sources activity
c		Z	Set activity with zenith cone cut
c		T	Set activity with minimal TS
c		R	Set activity within local ROI
c			Note I and U are mutually exclusive
c
c		O	Set activity using region outside local ROI
c			or zenith cone.  Should be used with commands
c 			R or Z
c		+	if both R and Z are used use Z to determine 
c			region for activity setting.  This is a stupid 
c			override and won't be advertised
c		-	Do not change previous activity if present criteria
c			presents a set_activity_false flag
c
c   O		C	Output a residual counts map
c		
c
c-----------------------------------------------------------------------
C LIKE Version: 5.0 DELIVERED: November 1st 1994, Programmer J.A. ESPOSITO
C+             UPDATED:    by  JAE	
c
C=======================================================================
C  $Log: input_id.f,v $
C  Revision 1.5  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.4  2005/08/26 19:36:35  irby
C  Purely cosmetic changes to allow compilation with gfortran/g95, mostly
C  involving fixes to lines longer than 72 chars that were being truncated,
C  but in the case of the CGRO code, also moving misplaced (tabbed) line
C  continuation characters to their appropriate position in column 6.
C
C  Revision 1.3  2002/12/26 17:42:53  irby
C  Fix read/write statements (for f90 compatibility): add/remove commas and
C  fix lines over 72 chars.
C
C  Revision 1.2  2002/04/18 19:34:09  irby
C  Bug fixes for OSF build:
C
C  - change "dreal" to acceptable replacement "dble"
C  - changed null string comparisons '' to ' ' - this may not be the best
C    solution.
C  - removed malloc.h from like.h (unnecessary, and not available under Darwin)
C  - Makefile generated using: mkmk version 1.81
C
C  Revision 1.1  2002/04/16 20:27:32  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.40  1997/04/09  17:52:42  jae
c 1) Commented out lines which request a 'C'
c    if some sources are not optimized by LM,
c    LPO or LPE when inputing a file with
c    subcommand 'W'
c
c 2) Placed a hard fix to the AT command for RCH.
c    The subcommand 'ATH' will check and set activity
c    according to the comparison of svn_TS(*) to psfminj
c    and then ignore any other A switch combinations.
c    This temorarily repairs the error in the AT command.
c
c Revision 5.39  1997/04/07  03:58:45  jae
c Repaired error in LPMS (LPI sub M) call to
c lpi_zt(tinput(1:20)) from below 2005.  The variable
c tinput(1:2) was not set to sinput(1:2) !!  This has
c been fixed before goto 59 (below 2005).
c
c Revision 5.38  1997/03/10  18:15:57  jae
c Added switch to LPI sub X command.  If
c an A is used (eg XTA [format X[T,A,...]])
c then only a message is output to the console
c if a test source is NOT LPON or LM optimized.
c Thus the test source WILL be deleted !  This
c corrects for test sources which thrrew errors
c during optimization and could not be optimized.
c
c Revision 5.37  1996/11/13  22:00:19  jae
c Revised command LPI sub Z and LPI sub T to
c      call subroutine LPI_ZT(char C[6]) where
c      the command from previous versions of this
c      subroutine (LPI) has been moved.
c
c Revision 5.36  1996/11/13  20:16:55  jae
c Repaired typo 'tinput2 -> tinput' in file
c
c Revision 5.35  1996/11/13  20:06:51  jae
c Repaired error in use of 'F' choice in LPI sub R
c     command.
c Repaired error for default switch 'XT' == 'X ' in
c      LPI sub X command.  Default for LPI sub X is
c set to 'XT'.
c
c Revision 5.34  1996/11/12  21:27:39  jae
c No changes
c
c Revision 5.33  1996/11/11  17:34:31  jae
c Repaired goto's in LPI_ZP
c Updated LPI_W to include renormalization
c    of the counts by read in flux and
c    map exposure of the viewing period.
c    If read in flux is zero then counts are
c    set to zero.
c
c Revision 5.32  1996/10/31  16:14:13  jae
c Repaired LPI sub M errors concerning
c activity saving and resetting. Array
c jsav_act(600) moved to COMMON/locpos.copy
c
c Also repaired jpsfinfo.f for errors
c
c Revision 5.31  1996/10/14  14:31:34  jae
c Repaired error (typo) in variable name
c sinput -> sinput2 after read statement in
c section staring with 20005 continue.  The
c call to subroutine to_upper had the wrong
c input variable thus lower case input would
c not work.
c
c Revision 5.30  1996/10/09  18:06:00  jae
c Repaired error after line "662 continue"
c where is used NSOURCE rather than nsrc to
c calculate 'do loop variable' kk.
c
c Revision 5.29  1996/08/26  21:42:14  jae
c Moved code for sorting to file jsort.f
c This reduces the number of code lines in
c this routine and replaces them with a call
c to jsort(tinput(1:50))
c
c Revision 5.28  1996/08/20  14:41:37  jae
c fixed typo tmpfn2 -> tmpfnm2 in command
c LPI sub R/W
c
c Revision 5.27  1996/08/20  14:35:44  jae
c repaired typo (LPI sub W/R open statement
c variable ifile=input --> file=tmpfnm2)
c
c Revision 5.26  1996/08/14  15:32:33  jae
c Changed call auto_out(3) -> call auto_out(4)
c Changed multi-line conver to uppercase code
c to call to_upper(x)
c lengthened variables tmpfnm2 and created
c char*2 tst_input for testing before final
c call to auto_out(4)
c
c Revision 5.25  1996/08/14  14:39:35  jae
c For command LPI sub M added call to auto_out(3)
c if autooutput flag is true.  Final call to auto_out(3)
c is performed independent of autooutput flag.
c
c Repaired error in LPI sub X[n]: assignment of loop
c variable kk = NSOURCE - jj changed to
c kk = NSOURCE - jj + 1
c
c Revision 5.24  1996/08/09  19:16:55  jae
c Fixed typo on line 2059 (didn't close paren)
c
c Revision 5.23  1996/08/09  18:43:40  jae
c Added PSF number to screen output of LPI
c sub PD (or PD+).
c
c Revision 5.22  1996/08/09  18:39:13  jae
c Added command LPI sub PD which displays
c to console a list of source position vs.
c ID position (plus the distance) for i
c identified sources. switch '+' (e.g. PD+)
c will only display sources which are different
c the identification position by more than
c 0.01 degrees.
c
c Revision 5.21  1996/08/08  15:40:29  jae
c repaired a stupid typo on line ~783 (left in
c an uncommented line to measure char length of
c a line)
c
c Revision 5.20  1996/08/08  15:34:57  jae
c Repaired typo's resulting from last edits
c On line 783 error caused by two typos, first on
c line 783 (needed opening/closing double quotes)
c and line ~785 needed to be removed.
c
c Revision 5.19  1996/08/07  19:53:48  jae
c Changed input for command LPI sub R to ask
c if source should be renormalized if energy
c is different than internal counts map, reset
c to ~0 (actually to 0.25) with a null PSF map
c or read in as the counts are in the file.
c This changes a two answer switch to a three
c answer decision.  Note that if the energy is
c the same for the file and map the user is only
c asked to reset counts or leave them as they are.
c The default is 'reset counts to ~0 and null PSF
c map'.
c
c Revision 5.18  1996/07/18  16:25:22  jae
c Repaired LPI sub Innn command, added input
c *** output for IP command
c
c Revision 5.17  1996/07/18  15:04:15  jae
c Repaired error on lines 225 and 2468 which
c were reported by cvm
c
c Revision 5.16  1996/06/04  17:16:35  jae
c Added sv_params([1,2],jj) to sorting routine.
c This variable contains Gmult and Gbias.
c
c Also added repair to FITS_DIR for LPI sub I
c command if FITS_DIR not defined then
c FITS_DIR=keepfitsdir (common in locpos.copy)
c
c Revision 5.15  1996/04/30  20:55:13  jae
c same as previous fix: changed goto 20044 to goto 20049
c after line 2004 to avoid renaming problems
c
c Revision 5.14  1996/04/30  20:50:11  jae
c repaired infinite loop for lpi sub m with #R switch.
c The renaming of Gmultfilename and Gbiasfilename to
c a lower number caused problems
c
c Revision 5.13  1996/04/29  22:20:27  jae
c repaired missing closing quote on line 2740
c
c Revision 5.12  1996/04/29  21:40:04  jae
c No Changes
c
c Revision 5.11  1996/04/29  21:00:19  jae
c repaired errors (typo's) in subroutine
c
c Revision 5.10  1996/04/24  18:02:59  jae
c Used new common block variable 'script_on'
c to prevent need for keyboard input when
c using script files.  Also, streamlined the code
c and slightly changed activity criteria for
c LPI sub M command (this latter is only for
c sources found)
c
c Revision 5.9  1996/04/24  13:37:58  jae
c Updated 'M' subcommand to assure re-setting
c of the original activity values
c
c Revision 5.8  1996/04/08  16:10:20  jae
c Added sorting by galactic coordinates
c Repaired reading of opposite coordinate
c system in lpi sub r
c
c Revision 5.7  1996/03/14  17:59:13  jae
c Fixed typo: jflag3 --> flag3 for verbose output
c
c Revision 5.6  1996/03/14  17:14:22  jae
c Repaired value of in_dex4 (no longer set for
c input(6:50).  Also added more debug verbose lines.
c
c Revision 5.5  1996/03/13  16:59:16  jae
c Fixed syntax error on line 238
c
c Revision 5.4  1996/03/13  16:39:10  jae
c Added error checking,verbose output lines
c
c Revision 5.3  1996/03/12  21:10:33  jae
c Repaired error on input of LPID#x
c
c Revision 5.2  1996/03/06  21:20:24  jae
c Updated code: FITS_DIR read with GETENV only
c once at program start in like.f
c
c Revision 5.1  1996/02/29  20:48:09  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:37:32  jae
c Subroutine Module for like V5.00
c
C
c
c-------------------------------------------------------------------------
        SUBROUTINE INPUT_ID_POS(input)
c Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/xrrrep.copy'
      INCLUDE  '../COMMON/fitrep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save
c
        real           MAPVAL
	real           svo_TS(600)

        integer        CTLMSZ1_sav,CTLMSZ2_sav,FITMSZ1,FITMSZ2

        character(260)  tinput
        character(100)  tmpfnm, tmpfnm2, moreinput
	character(80)   id, fits_dir
        character(50)   input,sv_Gb_file,sv_N_file
        character(18)   ainput
        character(2)    sinput2, tst_input
        character(1)    sinput

        logical        FEXIST,fflg1,fflg2,flag3,coord_sys_is_right
	logical        tmp_id_flg,jroi,jroi_wrap,jwarn,j360rng
	logical        determined,renamefile
	logical        rename_file,autoouttmp,flag_all

	common /id/id


	id = '$Id: input_id.f,v 1.5 2013/05/21 19:08:25 irby Exp $'
        LOC='ID_POS'

	fits_dir = data_dir
c
	do kl=1,25
	   if (input(kl:kl+3).eq.'    ') goto 4001
	   call to_upper(input(kl:kl))
	   tinput(kl:kl)=' '
	enddo

	if (jae_input_id_pos) then
	   write(*,*)'In subroutine input_id_pos'
	   write(*,'("input= ",a)') input(1:20)
	endif

4001	determined=JFALSE
	jroi=JFALSE
	fflg1=JFALSE
	fflg2=JFALSE
	jwarn=JFALSE
	j360rng=JFALSE

	if (Jpass1.eq.0) then
	   coord_sys_is_right=JFALSE
	   Jpass1=1
	endif

	rename_file=.false.
	flag3=.false.

        if (input(1:4).eq.'LPMS') input='LPID#M'

5       ainput='                  '
	renamefile=JFALSE
	jmapflg = .false.
	determined=JFALSE

	do jc = 1,26
	   if (jc.le.10) moreinput((jc-1)*10+1:jc*10)='          '
	   if (jc.le.10) tmpfnm((jc-1)*10+1:jc*10)='          '
	   tinput((jc-1)*10+1:jc*10)='          '
	enddo

	numin=0
	jwarn=JFALSE
	jroi=JFALSE
	fflg1=JFALSE
	fflg2=JFALSE

	if (flag3) input='LPID Q'

        dt = CTLSCL/2.
	jroi_wrap=JFALSE
	if (input(1:4).ne.'LPID') then
  	   if (jae_input_id_pos) write(*,*)'In NOT LPID segment'
           write(*,'(" Enter a choice from below:")')
           print *,' '
           write(*,'(" A[] to change activity value;")')
           write(*,'(" B   to change AMOEBA ",
     &          "angular convergence tolerance;")')
           write(*,'(" C   to change JLOC_POS ",
     &          "angular convergence tolerance;")')
           write(*,'(" D   to issue a system command;")')
           write(*,'(" F   to find source ID from source list file;")')
           write(*,'(" I[] to list counts map Filename and ",
     &          "Energy range;")')
           write(*,'(" K   to rebuild the PSF test source map;")')
           write(*,'(" M   to run automated mapping survey;")')
           write(*,'(" N   to change PSF source name;")')
           write(*,'(" O[] for output of automated residual map ",
     &          "generation;")')
           write(*,'(" P   to ID position;")')
           write(*,'(" Q   or <cr> to quit;")')
           write(*,'(" R   to READ(FILE) LPIW input;")')
           write(*,'(" S[] to sort PSF array by GRO J2000 name;")')
           write(*,'(" T   to change minimum acceptable TS value;")')
           write(*,'(" U[] to unidentify a PSF ID;")')
           write(*,'(" W   to WRITE(FILE) LPIW Output;")')
           write(*,
     *          '(" X[] to delete PSFs with TS below minimum value;")')
           write(*,'(" Z   to change zenith aspect cone value;")')
           write(*,'(" :--->"$)')
           sinput2='  '
           sinput=' '
	   jmapflg=.false.
           read(LU(12),'(a)')tinput
	   numin = index(tinput, ' ') - 1
	   if (numin.eq.0) return
	   sinput2(1:2)=tinput(1:2)

        else
	   if (jae_input_id_pos) write(*,*)'In LPID segment'
	   jmapflg=.false.
  	   tinput=input(6:26)
	   in_dex4 = index(tinput, ' ') - 1
           sinput2=tinput(1:2)

	   if (jae_input_id_pos) then
	      write(*,*)' '
	      write(*,*)'tinput,sinput2 are set as:'
	      write(*,'("  tinput: ",a)') tinput(1:20)
	      write(*,'("  sinput2:",a)') sinput2
	      write(*,*)' '
	   endif

	   if (in_dex4.gt.1) then
	      numin=in_dex4
	   else
	      numin=1
	   endif

           if (sinput2.eq.'  ') sinput2='..'
        endif

 503    do kl=1,25
	   CALL TO_UPPER(tinput(kl:kl))
	enddo

        sinput=tinput(1:1)
	call to_upper(sinput)
	call to_upper(sinput2(1:1))
	call to_upper(sinput2(2:2))

	if (jae_input_id_pos) then
	   write(*,*)'----------------------------------'
	   write(*,'("sinput:",a,"  sinput2:",a)') sinput,sinput2
	   write(*,'("tinput:",a)') tinput(1:20)
	   write(*,'("numin:",i3)') numin
cDave B.		write(*,'("numin:",l3)') numin
	   write(*,*)' '
	   write(*,*)'-----------------------------------'
	endif

        if (sinput.eq.'Q'.or.sinput.eq.' ') return

	if (input(5:5).eq.'#') then
           flag3=.true.
        else
           flag3=.false.
        endif

	if (jae_input_id_pos) then
	   write(*,*)' '
	   write(*,'("flag3:",l2)') flag3
	   write(*,'("input5:",a)') input(5:5)
	   write(*,*)'-----------------------------------'
	endif

	input(1:8)='        '
        if (sinput.eq.'M') goto 2000
	if (sinput.eq.'K') goto 3002
	if (sinput.eq.'O') then
	   call OMAP(tinput)
	   goto 5
	endif

        if (sinput.eq.'S') then
           if (NSOURCE.lt.2) then
              print *,' There are less than two sources in PSF'
              print *,' '
              input='        '
              goto 5
           endif

	   CALL JSORT(tinput(1:10))
           input='        '
           goto 5
        endif
c
        if (sinput.eq.'I') then
	   if ((numin.gt.1.and.index(tinput,'IS').eq.0) .or.
     &          index(tinput,'IP').ne.0) goto 510
	   write(*,'(" ")')
	   write(*,'("LIKE version: V",a)') JVER
	   call net_info('+',1)
	   write(PWD1,'(100(" "))')
	   J = GETCWD(PWD1)
	   in_d = index(PWD1, ' ')

	   if (in_d.ne.0) print *,'Present working directory: ',
     &                             PWD1(1:in_d)

	   if (index(tinput,'IP').ne.0) goto 5102

	   in_d = index(FITS_DIR, ' ') - 1

	   if (FITS_DIR(1:in_d).ne.keepfitsdir(1:in_d)) then
	      if (jae_input_id_pos) then
		 write(*,*)' '
		 write(*,*)'FITS_DIR ERROR'
		 write(*,'("FITS_DIR:",a)') FITS_DIR(1:in_d)
		 in_d=0
	      else
		 in_d = index(keepfitsdir, ' ')
		 FITS_DIR=keepfitsdir(1:in_d)//'   '
	      endif
	   endif

           if (in_d.ne.0) then
	      print *,'FITS directory: ', FITS_DIR(1:in_d)
	   else
	      in_d = index(keepfitsdir, ' ')
	      FITS_DIR=keepfitsdir(1:in_d)//'   '
	      if (jae_input_id_pos)
     &             write(6,*)'FITS_DIR not defined ! repairing'
	      print *,'FITS directory: ', FITS_DIR(1:in_d)
	   endif

	   in_d = index(CTLFILE, ' ')

           if (in_d.ne.0) print *,'CTL File:     ',CTLFILE(1:in_d)

	   in_d = index(CMAPFILE, ' ')

           if (in_d.ne.0) print *,'Counts File:  ',CMAPFILE(1:in_d)

	   in_d = index(GMAPFILE, ' ') 

           if (in_d.ne.0) print *,'Gas map File: ',GMAPFILE(1:in_d)

	   write(*,'("Energy range: ",I6," to ",I6)')CTLEMN,CTLEMX
	   print *,' '
	   print *,' Number of PSFs in map: ',NSOURCE
	   write(LU(1),*)' '
	   write(LU(1),'(" Restrict Gmult: ",l2)') Restrict_Gmult

	   if (Restrict_Gmult) write(LU(1),'(" Gmult: ",f5.1)') Gmult

           write(LU(1),'(" Restrict Gbias: ",l2)') Restrict_Gbias

	   if (Restrict_Gbias) write(LU(1),'(" Gbias: ",f5.1)') Gbias

	   write(LU(1),'(" Gmult notification range: ",f5.2,
     &          " to ",f5.2)')Gmult_min,Gmult_max
	   write(LU(1),'(" Gbias notification range: ",f5.2,
     &          " to ",f5.2)')Gbias_min,Gbias_max

	   if (.not.Restrict_notice) then
	      write(LU(1),'(" User is notified if range is violated")')
	   else
	      write(LU(1),
     &              '(" User is NOT notified if range is violated")')
	   endif

	   write(LU(1),*)' '

 510       if (numin.gt.1) CALL JPSFINFO(tinput)

	   write(*,'(" ")')

	   if (numin.gt.1) goto 5

	   write(*,'(" <cr> to continue :"$)')
	   read(LU(12),'(a)') input
	   numcar = index(input, ' ') - 1
	   input=' '
	   numcar=0
           goto 5
5102	   print *,' '
	   print *,'Aspect zenith cone set at ',aspj,' degrees'
	   write(*,'("Pointing direction set to: "$)')
	   if (coord_sys.eq.'C') then
	      write(*,'("RA = ",f7.3," DEC = ",f7.3)')SC_RAJ,SC_DECJ
	   else
	      write(*,'("l = ",f7.3," b = ",f7.3)')SC_LJJ,SC_BJJ
	   endif

	   print *,'TS limit is set to: ',psfminj
	   print *,'Number of PSFs: ',NSOURCE
	   print *,'Auto-output flag set to: ',autooutput
	   in_d = index(NEWMAPFILE, ' ') - 1

	   if (in_d.gt.0) print *,'Generic output filename: ',
     &          NEWMAPFILE(1:in_d)
	   goto 5
        endif
c
511     if (sinput.eq.'R'.or.sinput.eq.'W') then
	   jwarn=.false.

	   if (jae_input_id_pos) then
	      print *,'sinput: ',sinput
	      print *,'jmapflg: ',jmapflg
	   endif

	   if (jmapflg) goto 512

           input='        '
           if (NSOURCE.lt.1.and.sinput.eq.'W') then
              goto 3001
           else
              fflg1=JTRUE
              fflg2=JTRUE
              print *,'PSF LPO   LPE'
              print *,' '

              if (sinput.ne.'R') then
                 do jj=1,NSOURCE
                    fflg1=sv_dstsv(1,jj).and.fflg1
                    fflg2=sv_dstsv(2,jj).and.fflg2
                    write(*,'(I3,"  "$)')jj
                    print *,sv_dstsv(1,jj),'    ',sv_dstsv(2,jj),
     *                   '    ',SRC_NAMES(jj)
                 enddo
              endif

              if (.not.(fflg1.and.fflg2).and.sinput.eq.'W') then
                 if (.not.fflg1)
     &              print *,' some sources have not been ',
     *                'position optimized'

                 if (.not.fflg2)
     &              print *,' some sources have not been ',
     *                'error optimized'

c                write(*,
c     &            '(" Enter C to continue output operation:"$)')
c                read(LU(12),'(a)') sinput2(1:1)

                 numcar = index( sinput2(1:1),' ' ) - 1

c                if(numcar.eq.0)goto 5
c                if(sinput2(1:1).ne.'c'.and.
c     &             sinput2(1:1).ne.'C')
c     &                   goto 5
              endif
           endif
c
           if (sinput.eq.'R') write(*,'(" Enter input filename: "$)')
           if (sinput.eq.'W') write(*,'(" Enter output filename: "$)')

           read(LU(12),'(a)') tmpfnm2
	   numcar = index(tmpfnm2, ' ') - 1

           if (numcar.eq.0) then
              print *,' '
              goto 5
           endif

           INQUIRE(file=tmpfnm2(1:numcar),EXIST=FEXIST)
           if (.not.FEXIST) then
              if (sinput.eq.'R') then
                 print *,' '
                 print *,' file ',tmpfnm2(1:numcar),
     &                ' does not exist'
                 print *,' in this directory (ls -l follows)'
                 CALL SYSTEM('pwd')
                 CALL SYSTEM('ls -l')
                 print *,' '
                 print *,' Use LPID subcommand D to list files'
                 print *,' '
                 tmpfnm2='        '
                 goto 5
              endif
           endif

           if (FEXIST) then
              if (sinput.eq.'W') then
                 print *,' '
                 print *,' file ',tmpfnm2(1:numcar),' EXISTS !'
                 print *,' Enter:'
                 print *,'  D to overwrite;'
                 print *,'  A to abort this operation;'
                 write(*,'(" ---->"$)')
                 read(LU(12),'(A)')sinput2(1:1)
                 if (sinput2(1:1).ne.'d'.and.sinput2(1:1).ne.
     &                'D') goto 5
                 tmpfnm = 'rm -f '//tmpfnm2(1:numcar)
                 CALL SYSTEM(tmpfnm)
              endif

              if (sinput.eq.'R'.and.NSOURCE.ge.1) then
                 print *,' There are ',NSOURCE,' sources in the PSF map'
                 print *,' Enter <cr> to add to the existing PSF map'
                 print *,' Enter  D to empty the existing PSF map:'
                 write(*,'(" ---->"$)')
                 read(LU(12),'(a)') sinput2(2:2)
                 numcar = index( sinput2(2:2),' ' ) - 1

                 if (sinput2(2:2).eq.'d'.or.sinput2(2:2).eq.'D') then
                    CALL MAPRST(BMAP,CTLMSZ1,CTLMSZ2)
                    NSOURCE=0
                 endif
              endif
	   else
	      if (sinput.eq.'W') goto 512
              print *,'File: ',tmpfnm2(1:numcar),'does not exist !'
	      print *,'Check ypur filename.'
              print *,' '
              goto 5 
           endif

512        continue
           if (sinput.eq.'W') then
	      if (jae_input_id_pos) then
		 print *,'sinput: ',sinput
		 print *,'jmapflg: ',jmapflg
              endif

              open(43,file=tmpfnm2(1:numcar),err=57)
              write(43,'("FULL PSF INFORMATION ",A,2I7," Z8")')
c - Dave B.                   write(43,'("FULL PSF INFORMATION ",A,2I7," Z")')
     &             coord_sys,CTLEMN,CTLEMX

              do jj=1,NSOURCE
                 write(43,*)SRC_PARMS(jj,1),
     &                SRC_PARMS(jj,2),SRC_PARMS(jj,3),SRC_PARMS(jj,4),
     &                SRC_PARMS(jj,8),svn_TS(jj),sv_flx(1,jj),
     *                sv_flx(2,jj),
     &                sv_cnts(1,jj),sv_cnts(2,jj),sv_expose(jj),
     *                sv_upperlim(jj),
     &                sv_params(1,jj),sv_params(2,jj),sv_true_x(jj),
     *                sv_true_y(jj),
     &                best_x(jj),best_y(jj),sv_err68(jj),sv_err95(jj),
     *                sv_gal_long(jj),
     &                sv_gal_lat(jj),sv_cel_long(jj),sv_cel_lat(jj)
                 write(43,*)(sv_dstsv(nn,jj),nn=1,10)
                 write(43,'(A)')SRC_NAMES(jj)
              enddo

              close(unit=43)

              if (jmapflg) goto 22012
              goto 5
           endif

           if (sinput.eq.'R' ) then
              open(43,file=tmpfnm2(1:numcar),err=57)
              read(43,'(a)') input
              numcar = index(input, ' ') - 1

              if (input(1:20).ne.'FULL PSF INFORMATION') then
                 print *,'FILE TYPE DESIGNATOR IS IN ERROR !'
                 print *,'NUMCAR:',numcar,' INPUT: ',input(1:numcar)
                 print *,'DESIGNATOR SHOULD BE: FULL PSF INFORMATION'
                 print *,' Please check the file !'
                 close(unit=43)
                 input='                                            '
                 goto 5
              endif

              in_dexG=index(input,'G')
              in_dexC=index(input,'C')
              in_dexGC=in_dexG+in_dexC
              in_dexE=index(input,'Z')-in_dexC-in_dexG
              lCTLEMN=-1
              lCTLEMX=-1

              if (in_dexE.gt.5) read(input(in_dexC+in_dexG+1:50),*)
     &             lCTLEMN,lCTLEMX
              if (in_dexG.ne.0.or.in_dexC.ne.0) then
 		 if (coord_sys.ne.input(in_dexGC:in_dexGC)) then
		    jwarn=JTRUE
		    write(*,'("Coordinate system of file is NOT: ",
     &                          a)') coord_sys
		    write(*,*)'Converting coordinates of file !'
		 endif

              else
                 print *,' '
		 print *,' File contains no coordinate system'
                 print *,' information.  No protection is possible'
                 print *,' '
              endif

              lCTLfx=-1
              eCTLnm=1
              nflxfx=0
              input=jblank//jblank//'          '

              if (lCTLEMN.ne.CTLEMN.or.lCTLEMX.ne.CTLEMX) then
515		 write(*,'("Renormalize counts from energy range: ",
     &                 f8.1," to ",f8.1)') lCTLEMN,lCTLEMX
		 write(*,'("to the present energy range ",$)')
   		 write(*,'(I7," to ",I7, "enter Y;")')CTLEMN, CTLEMX
		 write(*,'("Reset source counts using flux from",$)')
		 write(*,'(" the file being read enter F;")')
		 write(*,'("Reset all counts to 0, enter R;",/,
     &                "Read in counts as they are enter N;",/,
     *                "Enter (R,y,f,n):"$)')
		 read(LU(12),'(a)') input
		 numcar = index(input, ' ') - 1

		 if (numcar.gt.0) then
		    call to_upper(input(1:1))
		 else
		    input(1:1)='R'
		 endif

		 if (input(1:1).ne.'Y'.and.input(1:1).ne.'N'.and.
     &                input(1:1).ne.'R'.and.input(1:1).ne.'F') goto 515
		 if (input(1:1).eq.'Y') lCTLfx=1
		 if (input(1:1).eq.'F') nflxfx=1
		 eCTLnm=1.

		 if (lCTLfx.gt.0)
     &              eCTLnm=(float(CTLEMX-CTLEMN)/float(CTLEMN*CTLEMX))/
     &              (float(lCTLEMX-lCTLEMN)/float(lCTLEMN*lCTLEMX))
		 goto 516
              endif

 5151         write(*,'("Reset all counts to 0,enter R")')
              write(*,'("Reset source counts using flux from"$)')
              write(*,'(" the file being read enter F")')
              write(*,'("Read in counts as they are enter N;")')
              write(*,'("Enter (N,r,f):"$)')
              read(LU(12),'(a)') input
              numcar = index(input, ' ') - 1

              if (numcar.gt.0) then
		 call to_upper(input(1:1))
              else
		 input(1:1)='N'
              endif

              if (input(1:1).eq.'F') nflxfx=1
              if (input(1:1).ne.'N'.and.
     &            input(1:1).ne.'R'.and.input(1:1).ne.'F') goto 5151
c
516           nsrc=NSOURCE

              if (input(1:1).eq.'Y') then
	 	lCTLfx=1
              elseif (input(1:1).eq.'R') then
		 lCTLfx=0
              else
		 lCTLfx=-1
              endif

              jj=NSOURCE
              do ll=nsrc+1,nsrc+500
	 	 jj=jj+1
                 kk=jj
                 read(43,*,end=55,err=54)SRC_PARMS(jj,1),
     &                SRC_PARMS(jj,2),SRC_PARMS(jj,3),SRC_PARMS(jj,4),
     &                SRC_PARMS(jj,8),svn_TS(jj),sv_flx(1,jj),
     *                sv_flx(2,jj),
     &                sv_cnts(1,jj),sv_cnts(2,jj),sv_expose(jj),
     *                sv_upperlim(jj),
     &                sv_params(1,jj),sv_params(2,jj),sv_true_x(jj),
     *                sv_true_y(jj),
     &                best_x(jj),best_y(jj),sv_err68(jj),sv_err95(jj),
     *                sv_gal_long(jj),
     &                sv_gal_lat(jj),sv_cel_long(jj),sv_cel_lat(jj)
		 SRC_PARMS(jj,3)=SRC_PARMS(jj,3)*eCTLnm
		 sv_cnts(1,jj)=sv_cnts(1,jj)*eCTLnm
		 sv_cnts(2,jj)=sv_cnts(2,jj)*eCTLnm
		 sv_flx(1,jj)=sv_flx(1,jj)*eCTLnm
		 sv_flx(2,jj)=sv_flx(2,jj)*eCTLnm
                 read(43,*,end=53,err=54)(sv_dstsv(nn,jj),nn=1,10)
                 read(43,'(a)',err=54,end=53) SRC_NAMES(jj)
		 numcar = index(SRC_NAMES(jj), ' ') - 1

		 if (lCTLfx.ge.0) sv_dstsv(1,jj)=JFALSE
		 if (lCTLfx.eq.0) SRC_PARMS(jj,3)=0.25

                 tmpL=SRC_PARMS(jj,1)
                 tmpB=SRC_PARMS(jj,2)
	  	 if (jwarn) then
		    if (coord_sys.eq.'G') then
		       pL=tmpL
		       pB=tmpB
		       sv_cel_long(jj)=pL
		       sv_cel_lat(jj)=pB
		       CALL CELGALD('CG',pL,pB,tmpL,tmpB,iiret)
                       SRC_PARMS(jj,1)=tmpL
                       SRC_PARMS(jj,2)=tmpB
		       sv_cel_long(jj)=tmpL
		       sv_cel_lat(jj)=tmpB	     
		       pL=best_x(jj)
		       pB=best_y(jj)
		       CALL CELGALD('CG',pL,pB,tmpL,tmpB,iiret)
		       best_x(jj)=tmpL
		       best_y(jj)=tmpB
                       tmpL=SRC_PARMS(jj,1)
                       tmpB=SRC_PARMS(jj,2)
                    else
                       pL = tmpL
                       pB = tmpB
		       sv_gal_long(jj)=tmpL
		       sv_gal_lat(jj)=tmpB
                       CALL CELGALD('GC',tmpL,tmpB,pL,pB,iiret)
                       SRC_PARMS(jj,1)=tmpL
                       SRC_PARMS(jj,2)=tmpB
		       sv_cel_long(jj)=tmpL
		       sv_cel_lat(jj)=tmpB
		       pL=best_x(jj)
		       pB=best_y(jj)
                       CALL CELGALD('GC',tmpL,tmpB,pL,pB,iiret)
		       best_x(jj)=tmpL
		       best_y(jj)=tmpB
                       tmpL=SRC_PARMS(jj,1)
                       tmpB=SRC_PARMS(jj,2)
                    endif
		 else
 		    if (coord_sys.eq.'G') then
		       pL=tmpL
		       pB=tmpB
		       sv_cel_long(jj)=pL
		       sv_cel_lat(jj)=pB	     
		       CALL CELGALD('CG',pL,pB,tmpL,tmpB,iiret)
                       sv_gal_long(jj)=tmpL
                       sv_gal_lat(jj)=tmpB
		    else
		       pL=tmpL
		       pB=tmpB
		       sv_gal_long(jj)=pL
		       sv_gal_lat(jj)=pB	     
		       CALL CELGALD('GC',pL,pB,tmpL,tmpB,iiret)
                       sv_cel_long(jj)=pL
                       sv_cel_lat(jj)=pB
		    endif

		    tmpL=SRC_PARMS(jj,1)
		    tmpB=SRC_PARMS(jj,2)
		 endif

		 if (nflxfx.eq.1) then
		    call mapixl(tmpL,tmpB,iiL1,iiB1)
		    if (signal.ne.' ') then
                       tmpexp=mapval(emap,iiL1,iiB1,CTLMSZ1,CTLMSZ2)
                       sv_cnts(1,jj)=sv_flx(1,jj)*tmpexp
                       sv_cnts(2,jj)=sv_flx(2,jj)*tmpexp
                       SRC_PARMS(jj,3)=sv_cnts(1,jj)
		    endif
                 endif

                 CALL L_CHECK(tmpL)

                 if (tmpL.le.ROIEND(1)+dt.and.tmpL.ge.ROIORG(1)-dt.and.
     &               tmpB.le.ROIEND(2)+dt.and.tmpB.ge.ROIORG(2)-dt) then
                    NSOURCE=NSOURCE+1
                    write(*,'(i3,2x,A,2f10.3,f10.3," "$)')jj,
     &                   SRC_NAMES(jj),tmpL,tmpB,SRC_PARMS(jj,3)
		    moreinput=SRC_NAMES(jj)
		    tmp_id_flg=.true.

		    if (moreinput(1:3).eq.'GRO'.or.
     &                   moreinput(1:3).eq.'gro') tmp_id_flg=.false.
                    print *,sv_dstsv(1,jj),sv_dstsv(2,jj),tmp_id_flg,
     &                   svn_TS(jj)

                    if (NSOURCE.gt.499) goto 52

                 else
	 	    srcN=SRC_NAMES(jj)
		    print *,' SOURCE: ',srcN(1:numcar),' is outside the ROI'
                    jj=jj-1
                 endif
              enddo

	      close(unit=43)

52            print *,' Maximum number of PSFs (<= 500) reached'
              print *,' Building PSFmap for first 500 PSFs'
              goto 55
53            print *,' EOF during/at READ: ',kk,
     *             ' Building PSFmap up to entry ',kk-1
              goto 55

54            close(unit=43)

              print *,
     *             ' Error during READ: ',kk,' Please check the file !'
              NSOURCE=kk-1
              print *,' There are NOW ',NSOURCE,' PSFs in MAP'
              CALL MAPRST(BMAP,CTLMSZ1,CTLMSZ2)

              if (NSOURCE.gt.0.and.lCTLfx.ne.0) call PSFBLD
              print *,' '
              input='        '
              goto 5
55            close(unit=1)

              if (NSOURCE.eq.0) then
	  	 print *,'ERROR: ',Nsource,' sources read !'
		 write(*,*)'Returning to menu !'
              endif

              if (jwarn) write(*,'("Source coordinates were",
     &             " converted to coordinate system: ",a)') coord_sys
              write(*,*)'Building the PSF source map'

              liktotaled=.false.
              CALL MAPRST(BMAP,CTLMSZ1,CTLMSZ2)
              if (NSOURCE.gt.0.and.lCTLfx.ne.0) call PSFBLD
           endif

           input='        '
           goto 5
57         print *,' ERROR OPENING FILE: ',input(1:numcar)
           print *,' ABORTING SUBCOMMAND: ',sinput
           print *,' '
           input='        '
           goto 5
        endif

        if (sinput.eq.'D') then
           CALL JGETSYSTEM
c	   CALL GETENV('BROWSER',browser_tmp)
c	   in_dex1 = index(browser_tmp, ' ') - 1
c	   IF(BROWSER(1:in_dex1).ne.browser_tmp(1:in_dex1))then
c		BROWSER=browser_tmp
c		browser_back=.true.
c		if(BROWSER(1:4).eq.'lynx')browser_back=.false.
c	   endif
           goto 5
        endif

59      if (sinput.eq.'Z'.or.sinput.eq.'T') then
	   CALL LPI_ZT(tinput(1:20))
           if (jmapflg) goto 20005
	   goto 5
	endif
c
c 1 A+X+N
c            
        if (sinput.eq.'N'.or.sinput.eq.'A'.or.sinput.eq.'X') then
	   if (index(tinput(1:6),'A').ne.0.and.
     &          tinput(1:1).eq.'X') then
	      flag_all=.true.
	   else
	      flag_all=.false.
	   endif

	   if (sinput.eq.'A'.and.index(tinput,'I').gt.0.and.
     &          index(tinput,'U').gt.0) then
	      print *,'(I)dentified and (U)nidentified are'
	      print *,'mutually exclusive activity switches.'
              print *,'They cannot be used simultaneously !'
              print *,' '

              do jj=1,260
                 tinput(jj:jj)=' '
              enddo
              goto 5
           endif

           if (coord_sys.eq.'C') then
              pL=SC_RAJ
              pB=SC_DECJ
           else
              pL=SC_LJJ
              pB=SC_BJJ
           endif

           jroi = JFALSE
           jroi_wrap = JFALSE
           tmpdist2L = 0
           tmpdist1L = 0
           tmpdist0L = 1
           tmpdist0B = 180
           tmpdist1B = 0
           tmpdist2B = 0

           if (NSOURCE.lt.1) goto 3001
           nnum=0
           input=jblank//jblank//'          '
           xactivej=0              
           ctltmpo1=CTLORG(1)-dt
           ctltmpo2=CTLORG(2)-dt
           ctltmpe1=CTLEND(1)+dt
           ctltmpe2=CTLEND(2)+dt
c 1 A+N-X goto
c                
           if (sinput.eq.'X') goto 615
cc
cc 1 AI+AU+AT-> goto 609
cc       
           if (sinput2(2:2).eq.'S'.or.(numin.ne.1.and.
     &          sinput2(2:2).ne.'L'.and.sinput.ne.'N')) goto 609
 605       if (sinput.ne.'N')		  
     &          write(*,'(" Enter PSF number, A for All (<cr> ",
     &          "to abort): ",$)')
           if (sinput.eq.'N') write(*,'(" Enter PSF number,",
     &          " <cr> to abort: ",$)')
           read(LU(12),'(a)',end=5) input
           numcar = index(input, ' ') - 1
           if (numcar.eq.0) goto 5
           if (input(1:1).eq.'A'.or.input(1:1).eq.'a') then
              if (sinput.eq.'N') goto 607
              nnum=0
           else
              read(input,*,err=607,end=607)nnum
           endif

           if (nnum.lt.0.or.nnum.gt.NSOURCE) goto 608
           goto 609
 607       write(*,'("INPUT ERROR -- Input not integer")')
           write(*,'(A)')input(1:numcar)
           write(*,'(" ")')
           goto 605
 608       write(*,'("INPUT ERROR -- Input outside range: "$)')
           write(*,'(" 0 <= ",I3," <= ",I3)')nnum,NSOURCE
           write(*,'(" ")')
           goto 605
 609       continue
ccc              
           if (sinput.eq.'N') goto 668
C                          
c 1 A                      
c                       
c     print *,'nnum: ',nnum
           numcar=0
C                           
c                         
           write(*,'("PSF #   Activity         NAME"$)')
           write(*,'("             OPT   ERR    ID      TS"$)')
           write(*,'("      PARMS-8  ASPECT")')
           nnum1=0
           nnum2=0
           nnum3=0

           if (index(tinput,'L').gt.0) xactivej=1
c                      
cc 2A do
           determined=JTRUE
           
           do jj=1,NSOURCE
              if (SRC_PARMS(jj,8).gt.0.9) nnum3=nnum3+1
              if (SRC_parms(jj,8).lt.0.8.and.
     &             SRC_PARMS(jj,8).gt.0.7) nnum2=nnum2+1
              if (nnum.ne.0.and.jj.ne.nnum) goto 610
              ISRC_PARMS8=(2.1*SRC_PARMS(jj,8))
              write(*,'(I5,4x,I4,7x,A," "$)')
     &             jj,ISRC_PARMS8,SRC_NAMES(jj)
              srcL=SRC_PARMS(jj,1)
              srcB=SRC_PARMS(jj,2)
              aspect=gtcirc(srcL,srcB,pL,pB)
c
              write(*,'("   ",L1,"    ",L1,"     ",L1,"  ",F8.2,4x,
     &             3x,F4.2," ",f7.2)')sv_dstsv(1,jj),sv_dstsv(2,jj),
     *             sv_dstsv(6,jj),
     &             svn_TS(jj),SRC_PARMS(jj,8),aspect
              if (SRC_PARMS(jj,8).lt.0.7) nnum1=nnum1+1
              if (jae_input_id_pos.and.determined) then
                 determined=JFALSE
                 call intent(determined)
              endif
 610       enddo
           
           determined=JFALSE
c
c 1A enddo
c
           write(*,'(" Total PSFs: ",I3)')NSOURCE
           if (nnum.eq.0) then
              write(*,'(" Inactive: ",I3," Fixed: ",I3,
     &             " Active: ",I3)')nnum1,nnum2,nnum3
           else
              if (SRC_PARMS(jj,8).lt.0.05) then
                 write(*,'("Source:",I4," is Inactive")')jj
              elseif (SRC_PARMS(jj,8).lt.0.76.and.SRC_PARMS(jj,8)
     &                .gt.0.74) then
                 write(*,'("Source:",I4," is in Fixed activity")')jj
              elseif (SRC_PARMS(jj,8).gt.0.8) then
                 write(*,'("Source:",I4," is Fully Active")')jj
              else
                 write(*,'("Source:", I4," displays ERROR activity",
     &                ":  value->",F5.3)')jj,SRC_PARMS(jj,8)
              endif
           endif

           write(*,'(" ")')
           if (index(tinput,'S').gt.0) goto 5
c
c 1A 
c                    
c                      
           xactivej = 1.
c               
c               
 614       write(*,'("Enter an activity",
     &          " value from list below:")')
           print *,' '
           print *,' 0 -> Inactive:0.00'
           print *,' 1 -> position fixed optimization:0.75'
           print *,' 2 -> full positional optimization:1.00'
           print *,'<cr> to abort to menu'
           write(*,'(" Enter your choice: "$)')
           read(LU(12),'(a)') sinput
           numsin = index(sinput, ' ') - 1

           if (numsin.eq.0) goto 5
c                    
cc 2A -> if                      
           if (sinput.lt.'0'.or.sinput.gt.'2') then
              print *,' '
              goto 614
           else
              read(sinput,*)nactivej
ccc 3A -> if                      
              if (nactivej.eq.0) then
                 xactivej = 0
              elseif (nactivej.eq.1) then
                 xactivej = 0.75
              else
                 xactivej = 1.
              endif
cc                               
CC 2A                      
           endif
c                     
c 1A                 
c                     
c                           
c                      
c   here is where x must get in
c                  
c Bring condition 'X' back in here
c                
 615       continue
c                 
c 1A+X               
c
           nsrc=NSOURCE
cccc                    
c If 'XL' (delete PSFs with TS < chosen TS) then give user option to
c set the local region of interest for this cut.  This permits having
c different TS limits for various regions
cccc                        
cccc 1A+X                
cccc
           ctltmpo1=ROIORG(1)
           call L_CHECK(ctltmpo1)
           ctltmpo1=ctltmpo1-dt
           ctltmpo2=ROIORG(2)-dt
           ctltmpe1=ROIEND(1)
           call L_CHECK(ctltmpe1)
           ctltmpe1=ctltmpe1+dt
           ctltmpe2=ROIEND(2)+dt

           if (.not.fullmap.and.ctltmpo1.gt.ctltmpe1) then
              ctltmpo1=ctltmpo1-360
           endif

           if (fullmap.and.ctltmpo1.lt.-179.and.
     &          ctltmpe1.gt.179.) then
              ctltmpe1=ctltmpe1+180
              ctltmpo1=ctltmpo1+180
           endif
            
           if (ctltmpo1.lt.0.and.ctltmpe1.lt.0) then
              ctltmpe1=ctltmpe1+360
              ctltmpo1=ctltmpo1+360
           endif
cc                       
cc 2A+X -> if ROI asked             
cc                      
           if (index(tinput,'R') .gt. 0) jroi = JTRUE
           if (jroi) then
              write(*,*)' '
 621          if (fullmap) then
                 print *,'Map covers all L(RA)'
              endif
              print *,'ROI range is: ',ROIORG(1)-dt,' to ',
     &             ROIEND(1)+dt
              write(*,*)' Use either [0,360] or [-180,180]'
              write(*,*)' coordinates'
              print *,' '
              write(*,'("Enter Minimum ",$)')
              write(*,'("Long(RA) (abort A) [default: ",$)')
              write(*,'(f7.2,"]: ",$)') ROIORG(1)-dt
              read(LU(12),'(a)') ainput
              numcar = index(ainput, ' ') - 1
ccc 3A+X                              
              if (numcar.eq.0) then
                 ctltmpo1=ROIORG(1)-dt
                 goto 6301
              endif
cc 2A+X                     
              if (ainput(1:1).eq.'a'.or.
     &             ainput(1:1).eq.'A')goto 5
              read(ainput,*,err=621,end=621)ctltmpo1
ccccc                         
ccccc 3A+X  
ccccc                      
 6301         continue
cccc                     
cccc 2A+X                  
cccc                         
 631          write(*,*)' '
              
              if (ctltmpo1.lt.ROIORG(1)-dt.and..not.fullmap) then
                 print *,'Value: ',ctltmpo1, is too low !'
                 print *,'Map ROI range is:'
                 print *,ROIORG(1)-dt,' < L(RA) < ',ROIEND(1)+dt
                 print *,'Re-enter the value'
                 print *,' '
                 ctltmpo1=ROIORG(1)-dt
                 goto 621
              endif

              if ((ctltmpo1.gt.ROIEND(1)+dt.and..not.fullmap).or.
     &             ctltmpo1.gt.360.or.ctltmpo1.lt.-180) then
                 print *,'Value: ',ctltmpo1,' is too large'
                 print *,'or is less than -180 !'
                 print *,'Map ROI range is:'
                 print *,ROIORG(1)-dt,' < L(RA) < ',ROIEND(1)+dt
                 print *,'Re-enter the value'
                 print *,' '
                 ctltmpo1=ROIORG(1)-dt
                 goto 621
              endif

              if (ctltmpo1.lt.0) then
                 j360rng=.false.
                 write(*,'("Use (-180,180)")')
              else
                 j360rng=.true.
                 write(*,'("Use (0,360)")')
              endif

              print *,' '
              write(*,'("Enter Maximum ",$)')
              write(*,'("Long(RA) (abort A) [default: ",$)')
              write(*,'(f7.2,"]: ",$)')ROIEND(1)+dt
              read(LU(12),'(a)') ainput
              numcar = index(ainput, ' ') - 1

              if (numcar.eq.0) then
                 ctltmpe1=ROIEND(1)+dt
                 goto 6381
              endif

              if (ainput(1:1).eq.'a'.or.ainput(1:1).eq.'A')goto 5
              read(ainput,*,err=631,end=631)ctltmpe1
 6381         continue

              if (ctltmpe1.gt.ROIEND(1)+dt.and..not.j360rng) then
                 print *,'Value: ',ctltmpe1, is too large !'
                 print *,'Map ROI range is:'
                 print *,ROIORG(1)-dt,' < L(RA) < ',ROIEND(1)+dt
                 print *,'Re-enter the value'
                 print *,' '
                 ctltmpo1=ROIORG(1)-dt
                 goto 621
              endif

              if (ctltmpe1.lt.-180.or.ctltmpe1.gt.360.or.
     *             (ctltmpe1.lt.ROIORG(1)-dt.and..not.fullmap)) then
                 print *,'Value: ',ctltmpE1,' is too low or'
                 print *,'exceeds 360.'
                 print *,'Map ROI range is:'
                 print *,ROIORG(1)-dt,' L(RA) < ',ROIEND(1)+dt
                 print *,'Re-enter the value'
                 print *,' '
                 ctltmpe1=ROIEND(1)+dt
                 goto 631
              endif
               
 639          continue
              if (ctltmpe1.le.ctltmpo1) then
                 write(*,'("ERROR: Long(RA) maximum is",
     &                " smaller than Long(RA) minimum: ",
     *                f7.2," !< ",f7.2)') ctltmpo1,ctltmpe1
                 print *,'Map ROI range is:'
                 print *,ROIORG(1)+dt,' < L(RA) < ',ROIEND(1)+dt
                 print *,'Re-enter both values'
                 write(*,*)' '
                 goto 621
              endif

 641          print *,' '
              write(*,'("Enter Minimum ",$)')
              write(*,'("Lat(Dec) (abort A) [default: ",$)')
              write(*,'(f7.2,"]: ",$)')ROIORG(2)-dt
              read(LU(12),'(a)') ainput
              numcar = index(ainput, ' ') - 1

              if (numcar.eq.0) then
                 ctltmpo2=ROIORG(2)-dt
                 goto 651
              endif

              if (ainput(1:1).eq.'a'.or.ainput(1:1).eq.'A') goto 5
              read(ainput,*,err=641,end=641)ctltmpo2

              if (ctltmpo2.lt.CTLORG(2)-dt) then
                 print *,' Error: minimum below map ROI boundary'
                 print *,'Map ROI range is:'
                 print *,ROIORG(2)+dt,' < B(DEC) < ',ROIEND(2)+dt
                 ctltmpo2=ROIORG(2)-dt
                 goto 641
              endif

 651          print *,' '
              write(*,'("Enter Maximum ",$)')
              write(*,'("Lat(Dec) (abort A) [default: ",$)')
              write(*,'(f7.2,"]: ",$)')ROIEND(2)+dt
              read(LU(12),'(a)') ainput
              numcar = index(ainput, ' ') - 1

              if (numcar.eq.0) then
                 ctltmpe2=ROIEND(2)+dt
                 goto 660
              endif

              if (ainput(1:1).eq.'a'.or.ainput(1:1).eq.'A') goto 5
              read(ainput,*,err=651,end=651)ctltmpe2
              
              if (ctltmpe2.gt.CTLEND(2)+dt) then
                 print *,' Error: maximum above map ROI boundary'
                 print *,'Map ROI range is:'
                 print *,ROIORG(2)-dt,' < B(DEC) < ',ROIEND(2)+dt
                 ctltmpe2=ROIEND(2)+dt
                 goto 651
              endif
cccc  
cccc 2A+X   after local endif's
cccc                         
           endif
ccc                    
ccc 1A+x                  
ccc 2A+X -> DO                          
 660       continue
           if (sinput.eq.'X'.and.tinput(2:7).eq.'      ') then
              sinput2='XT'
           endif

           tmpdist0B=gtcirc(0.0,ctltmpo2,0.0,ctltmpe2)

           if (.not.fullmap) then
              tmpdist0L=gtcirc(ctltmpo1,0.0,ctltmpe1,0.0)
           else
              tmpdist0L=gtcirc(ctltmpo1,0.0,ctltmpe1,0.0)
              if (tmpdist0L .lt. 180) tmpdist0L=ctltmpe1-ctltmpo1
           endif

           if (jae_input_id_pos) then
              print *,'tmpdist0L:',tmpdist0L
              print *,ctltmpo1,' <? ',ctltmpe1
              print *,'xactivej:',xactivej
           endif

           print *,' '
           do jj=1,nsrc
              if (nnum.ne.0.and.jj.ne.nnum) goto 663
              kk=nsrc-jj+1
cc                        
              srcjtmp1=SRC_PARMS(jj,1)
              srcjtmp2=SRC_PARMS(jj,2)
              call L_check(srcjtmp1)

              if (srcjtmp1.lt.0.and.j360rng) srcjtmp1=srcjtmp1+360.

              tmpdist0B=gtcirc(0.0,ctltmpo2,0.0,ctltmpe2)
              tmpdist1L=gtcirc(ctltmpo1,0.0,srcjtmp1,0.0)
              tmpdist2L=gtcirc(ctltmpe1,0.0,srcjtmp1,0.0)
              tmpdist3L=MAX(tmpdist1L,tmpdist2L)
              tmpdist1B=gtcirc(0.0,ctltmpo2,0.0,srcjtmp2)
              tmpdist2B=gtcirc(0.0,ctltmpe2,0.0,srcjtmp2)
              tmpdist3B=MAX(tmpdist1B,tmpdist2B)

              if (jae_input_id_pos) then
                 print *,'PSF:',jj
                 print *,' L:',srcjtmp1,'  B:',srcjtmp2
                 print *,'activity:',SRC_PARMS(jj,8)
                 print *,'tmpdist0L:',tmpdist0L
                 print *,'tmpdist1L:',tmpdist1L
                 print *,'tmpdist2L:',tmpdist2L
                 print *,'tmpdist3L:',tmpdist3L
                 print *,'tmpdist0B:',tmpdist0B
                 print *,'tmpdist1B:',tmpdist1B
                 print *,'tmpdist2B:',tmpdist2B
                 print *,'tmpdist3B:',tmpdist3B
              endif
cc
cc
CC 2A (X -> GOTO 662)
              if (sinput.eq.'X') goto 662
              ndxx = index(tinput, ' ') 

              if (jae_input_id_pos) then
                 print *,'L: ',ctltmpo1,ctltmpe1,':',srcjtmp1
                 print *,'B: ',ctltmpo2,ctltmpe2,':',srcjtmp2
              endif

cc 3A  IF            
              if (index(tinput,'-').gt.0) jroi_wrap=JTRUE
              if (index(tinput,'Z').ne.0)
     &             aspect=gtcirc(srcjtmp1,srcjtmp2,pL,pB)
              if (jae_input_id_pos) write(*,*)
     &             'aspect: ',aspect
              
              if (tmpdist3L.le.tmpdist0L.and.
     *             tmpdist3B.le.tmpdist0B) then
ccc
c
c ***************	Passed ROI IF    ******************
c
ccc                       
                 jwarn=JFALSE

                 if (jae_input_id_pos) then
                    print *,' In ROI switch-tinput:',tinput(1:ndxx)
                 endif
ccc
c
c Fix for Hartman
c
ccc   
                 if (tinput(1:3).eq.'ATH') then
                    if (svn_TS(jj).ge.psfminj) then
                       SRC_PARMS(jj,8)=xactivej
                    else
                       if (.not.jroi_wrap) SRC_PARMS(jj,8)=0
                    endif
                    
                    goto 663
                 endif

ccc
CCCC 4A IF                
CCCC                        
                 if (index(tinput,'I').gt.0) then
cccccc 5A IF                 
                    jwarn=JTRUE
                    if (jae_input_id_pos) print *,'switch I'
                    if (index(tinput,'O').eq.0) then
                       if (sv_dstsv(6,jj)) then
                          SRC_PARMS(jj,8)=xactivej
                       else
                          if (.not.jroi_wrap) SRC_PARMS(jj,8)=0
                       endif

                    else
                       if (.not.jroi_wrap) SRC_PARMS(jj,8)=0
                    endif
                 endif

cccc 4A  ELSEIF                 
                 if (index(tinput,'O').gt.0.and.
     &                index(tinput,'Z').eq.0) then
cccc  
cccccc 5A IF                 
                    jwarn=JTRUE
                    if (jae_input_id_pos) print *,'switch O*!Z'
                    if (.not.jroi_wrap) SRC_PARMS(jj,8)=0
                 endif
cccc 4A  ELSEIF                 
                 if (index(tinput,'U').gt.0) then
ccccc 5A  IF                 
                    jwarn=JTRUE
                    if (jae_input_id_pos) print *,'switch U'
                    if (index(tinput,'O').eq.0) then
                       if (.not.sv_dstsv(6,jj)) then             
                          SRC_PARMS(jj,8)=xactivej
                       else
                          if (.not.jroi_wrap) SRC_PARMS(jj,8)=0
                       endif
                    else
                       if (.not.jroi_wrap) SRC_PARMS(jj,8)=0
                    endif
                 endif
cccc 4A  ELSEIF              
                 if (index(tinput,'T').gt.0.and.
     &                index(tinput,'O').eq.0) then
ccccc 5A  IF                 
                    jwarn=JTRUE
                    if (jae_input_id_pos) print *,'switch T'
                    if (svn_TS(jj).ge.psfminj) then
                       SRC_PARMS(jj,8)=xactivej
                    else
                       if (.not.jroi_wrap) SRC_PARMS(jj,8)=0
                    endif
                 endif
cccc 4A ELSEIF
                 if (index(tinput,'Z').gt.0) then
ccccc 5A  IF
                    jwarn=JTRUE
                    if (jae_input_id_pos)print *,'switch Z'
                    if (aspect.le.aspj.and.index(tinput,'O').eq.0)
     &                   SRC_PARMS(jj,8)=xactivej
                    if (aspect.ge.aspj.and.index(tinput,'O').ne.
     &                   0)SRC_PARMS(jj,8)=xactivej
                    if (jae_input_id_pos)print *,'activity:',
     &                   SRC_PARMS(jj,8)

                    if (aspect.gt.aspj.and.index(tinput,'O').eq.0) then
                       if (jroi_wrap) then
                          continue
                       else
                          SRC_PARMS(jj,8)=0
                       endif
                    endif

                    if (jae_input_id_pos) print *,'activity:',
     &                   SRC_PARMS(jj,8)
                    if (aspect.lt.aspj.and.index(tinput,'O').ne.0) then
                       if (jroi_wrap) then
                          continue
                       else
                          SRC_PARMS(jj,8)=0
                       endif
                    endif

                    if (jae_input_id_pos)print *,'activity:',
     &                   SRC_PARMS(jj,8)
                 endif

                 if (.not.jwarn) then
                    if (jae_input_id_pos)print *,
     &                   'Other: ->',sinput2(1:2),'<-'
                    SRC_PARMS(jj,8)=xactivej
                 endif
ccc                           
ccc 3A                      
cccc                        
              else
c     
                 if (jae_input_id_pos) then
                    write(*,'("aspect: ",f5.1)')aspect
                    print *,'Failed ROI switch: ->',
     &                   tinput(1:ndxx),'<-'
                 endif

                 if (numin.ne.1.and.index(tinput,'O').eq.0.and.
     &                .not.jroi_wrap.and.index(tinput,'Z').eq.0.and.
     &                index(tinput,'T').eq.0) SRC_PARMS(jj,8)=0
                 if (index(tinput,'O').eq.0.and.
     &                index(tinput,'Z').ne.0.and.aspect.le.
     &                aspj.and.index(tinput,'+').ne.0)
     *               SRC_PARMS(jj,8)=xactivej
                 if (index(tinput,'O').ne.0.and.
     &                index(tinput,'Z').ne.0.and.aspect.ge.
     &                aspj.and.index(tinput,'+').ne.0)
     *               SRC_PARMS(jj,8)=xactivej
                 if (index(tinput,'O').gt.0.and.
     &                index(tinput,'Z').eq.0.and.
     *                index(tinput,'T').eq.0.and.
     &                index(tinput,'I').eq.0.and.
     *                index(tinput,'U').eq.0)
     &               SRC_PARMS(jj,8)=xactivej

                 if (index(tinput,'O').ne.0.and.
     &                index(tinput,'T').ne.0) then
                    if (svn_TS(jj).ge.psfminj) then
                       SRC_PARMS(jj,8)=xactivej
                    else
                       if (.not.jroi_wrap) SRC_PARMS(jj,8)=0
                    endif
                 endif

                 if (numin.eq.1)
     &                SRC_PARMS(jj,8)=MIN(xactivej,0.75)
                 if (index(tinput,'I').gt.0) then
                    if (index(tinput,'O').ne.0) then
                       if (sv_dstsv(6,jj)) then
                          SRC_PARMS(jj,8)=xactivej
                       else
                          if (.not.jroi_wrap) SRC_PARMS(jj,8)=0
                       endif
                    else
                       if (.not.jroi_wrap) SRC_PARMS(jj,8)=0
                    endif
                 endif

                 if (index(tinput,'U').gt.0) then
                    if (index(tinput,'O').ne.0) then
                       if (.not.sv_dstsv(6,jj)) then             
                          SRC_PARMS(jj,8)=xactivej
                       else
                          if (.not.jroi_wrap) SRC_PARMS(jj,8)=0
                       endif
                    else
                       if (.not.jroi_wrap) SRC_PARMS(jj,8)=0
                    endif
                 endif
              endif
              
              if (jae_input_id_pos) then
                 print *,'activity:',SRC_PARMS(jj,8)
              endif

              if (jae_input_id_pos) then
                 determined=JFALSE
                 call intent(determined)
                 if (determined) jae_input_id_pos=JFALSE
                 determined=JFALSE
              endif

CC                       
CC 2A                      
CC    END OF A SWITCH
              goto 663
ccc                          
ccc 2X
ccc
 662          continue                           
ccc                         
ccc 3X  IF                  
              kk=nsrc-jj+1
              srcjtmp1=SRC_PARMS(kk,1)
              srcjtmp2=SRC_PARMS(kk,2)
              call L_check(srcjtmp1)
              if (srcjtmp1.lt.0.and.j360rng) srcjtmp1=srcjtmp1+360.
ccc                           
              ndelsrc=0

              if (jae_input_id_pos) then
                 write(6,'(//,A," position: ",f7.2,2x,f6.2)')
     &                srcN,srctmpj1,srctmpj2
                 write(6,'("Pointing Dir: ",F7.2,2x,f6.2)')
     &                pL,pB
                 write(6,'(" Aspect: ",f7.2)')
     &                gtcirc(srctmpj1,srctmpj2,pL,pB)
              endif

              if (.not.sv_dstsv(1,kk)) then
                 print *,'PSF: ',kk,' has not been LPON optimized'
                 if (.not.flag_all) goto 663
              endif

 6625         ndelsrc=0
              if (jae_input_id_pos)
     &             write(*,'("startup delete status: ",i3)') ndelsrc
              if (svn_TS(kk).lt.psfminj.and.
     &             index(tinput,'T').ne.0) ndelsrc=1
ccccc 4X  IF              
              if (jae_input_id_pos)
     &             write(*,'("TS delete status: ",i3)') ndelsrc
              jroi_wrap=JFALSE      
              if (index(tinput,'O').gt.0) jroi_wrap=JTRUE
              
              if (jae_input_id_pos) then
                 write(6,'("---jroi: ",L1," outside: ",L1,"---")')
     &                jroi,jroi_wrap
              endif

              if (jroi) then
                 if (tmpdist3L.le.tmpdist0L.and.
     &                srcjtmp2.ge.ctltmpo2.and.
     *                srcjtmp2.le.ctltmpe2) then
                    if (jroi_wrap) ndelsrc=1
                 else
                    if (.not.jroi_wrap) ndelsrc=1
                 endif
              endif

              if (jae_input_id_pos)
     &             write(*,'("jroi test delete status: ",i3)') ndelsrc

              if (index(tinput,'Z').gt.0) then
                 aspect=gtcirc(srcjtmp1,srcjtmp2,pL,pB)
                 if (aspect.gt.aspj) then
                    if (.not.jroi_wrap) ndelsrc=1
                 endif
                 if (aspect.le.aspj) then
                    if (jroi_wrap) ndelsrc=1
                 endif
              endif

              if (jae_input_id_pos)
     &             write(*,'("aspect test delete status: ",i3)') ndelsrc
cccc 3X                        
cccccc 4X  IF                 
 664          if (ndelsrc.eq.1) then
                 print *,'Removing PSF: ',kk,' TS: ',
     &                svn_TS(kk),'     ',SRC_NAMES(kk)
                 CALL PSFREMOVE(kk)
              endif
cccc 3X                         
ccc 2X  
 663          continue
           enddo
cccc
cccc 1A+X             
cccc
CC 2A                 
CC                   
           write(*,'("PSF #   Activity         NAME"$)')
           write(*,'("             OPT   ERR    ID      TS"$)')
           write(*,'("        PARMS-8  ASPECT")')
           nnum1=0
           nnum2=0
           nnum3=0
cc		      
cc
           determined=JTRUE                 

           do jj=1,NSOURCE
              if (SRC_PARMS(jj,8).gt.0.9) nnum3=nnum3+1
              if (SRC_parms(jj,8).lt.0.8.and.
     &             SRC_PARMS(jj,8).gt.0.7) nnum2=nnum2+1
              if (SRC_PARMS(jj,8).lt.0.7) nnum1=nnum1+1
              if (nnum.ne.0.and.jj.ne.nnum) goto 667
              ISRC_PARMS8=(2.1*SRC_PARMS(jj,8))
              write(*,'(I5,4x,I4,7x,A," "$)')
     &             jj,ISRC_PARMS8,SRC_NAMES(jj)
              srcL=SRC_PARMS(jj,1)
              srcB=SRC_PARMS(jj,2)
              aspect=gtcirc(srcL,srcB,pL,pB)
              
              write(*,'("   ",L1,"    ",L1,"     ",L1,"  ",F8.2,7x,
     &             F4.2," ",f7.2)')sv_dstsv(1,jj),sv_dstsv(2,jj),
     *             sv_dstsv(6,jj),
     &             svn_TS(jj),SRC_PARMS(jj,8),aspect
              
              if (jae_input_id_pos.and.determined) then
                 determined=JFALSE
                 call intent(determined)
              endif
 667       enddo

           determined=JFALSE
           write(*,'(" Total PSFs: ",I3)')NSOURCE
           write(*,'(" Inactive: ",I3," Fixed: ",I3,
     &          " Active: ",I3)')nnum1,nnum2,nnum3
           write(*,'(" ")')
c                     
c 1A+X  (ON X or A-> GOTO 5)
C
           goto 5
c
c 1N 
c
 668       continue
CC                      
CC 2N IF                   
CC                        
           if (sv_dstsv(6,nnum)) then
              write(*,'("PSF: ",I3," is identified as: ",A)')
     &             nnum,SRC_NAMES(nnum)
              write(*,'(" Enter GRO<cr> to unidentify this PSF")')
           endif
C                        
C 1N                  
C                               
           write(*,'("Enter the source name(cr to abort): "$)')
           read(LU(12),'(a)') ainput
           numcar = index(ainput, ' ') - 1

           if (numcar.eq.0) goto 5
c 2N               
           if (((ainput(1:1).eq.'.'.or.ainput(1:1).eq.' ').and.
     &          numcar.eq.1).or.ainput(1:3).eq.'GRO') then
              srcL_tmp=srcL
              srcB_tmp=srcB

              if (ainput(1:3).eq.'GRO') then
                 sv_true_x(nnum)=0
                 sv_true_y(nnum)=0
                 sv_dstsv(6,nnum)=JFALSE
              endif	

              srcL=SRC_PARMS(nnum,1)
              srcB=SRC_PARMS(nnum,2)
              srcN='GRO'
              CALL GETSRCNM()
              SRC_NAMES(nnum)=srcN
              goto 5
           endif
c1 N              
           SRC_NAMES(nnum)=ainput(1:numcar)
           goto 5
        endif
c
cccccccccccccccccccccccccccccccccccccccccccccccccc
c-end of this 'X' 'A' 'N' switch
cccccccccccccccccccccccccccccccccccccccccccccccccc
c
C
        if (sinput.eq.'P'.or.sinput.eq.'U') then
           if (NSOURCE.lt.1) goto 3001
           if (sinput2.eq.'PI')
     &          write(*,'("You have selected to set the current",
     &          " source positions equal to the ID ",
     *          "position variables")')

           if (sinput2.eq.'PA')
     &          write(*,'("You have selected to set the ID position",
     &          " variables equal to the current source positions",/,
     &          " and mark the source as identified.")')

 670       input='        '
           
           if (sinput2.eq.'UA') then
              nnum=0
              goto 672
           endif
           
           write(*,'(" Enter PSF number or A for all PSFs "$)')
           write(*,'("(<cr> to abort): "$)')
           read(LU(12),'(a)') moreinput
           numcar = index(moreinput, ' ') - 1

           if (numcar.eq.0) goto 5
           
           if (moreinput.eq.'A'.or.moreinput.eq.'a') then
              nnum=0
           else
              read(moreinput,*,err=671,end=671)nnum
           endif

           goto 672
 671       write(*,'(/,"ERROR: ",A," is not readable as",
     &          " an integer",/,"Enter PSF number again",
     &          " ")')moreinput(1:numcar)
           goto 670
 672       continue
			
           if (sinput.eq.'U') then
              if (nnum.lt.0.or.nnum.gt.NSOURCE) then
                 write(*,'("PSF number out of range: 1 to",
     *                i4,/)')NSOURCE
                 goto 670
              endif

              if (nnum.gt.0) then
                 num1=nnum
                 num2=nnum
              else
                 num1=1
                 num2=NSOURCE
              endif

              do nnum=num1,num2
                 sv_true_x(nnum)=0
                 sv_true_y(nnum)=0
                 sv_dstsv(6,nnum)=JFALSE
                 srcL=SRC_PARMS(nnum,1)
                 srcB=SRC_PARMS(nnum,2)
                 srcN='GRO '
                 CALL GETSRCNM()
                 write(*,'("PSF: ",I3," Sourcename: ",A18)')
     &                nnum,srcN
                 SRC_NAMES(nnum)=srcN
              enddo

              goto 5
           endif

 7         continue
           num1=nnum
           num2=nnum

           if ((sinput2.eq.'PI'.or.
     &          sinput2.eq.'PA'.or.sinput2.eq.'PD').and.
     &          nnum.eq.0) then
              num1=1
              num2=NSOURCE

           elseif (sinput2.ne.'PI'.and.
     &             sinput2.ne.'PA'.and.sinput2.ne.'PD'.and.
     *             nnum.eq.0)then
              write(*,'("Warning: You selected ALL",
     &             " as an option.",/,
     *             "Are you sure this is what you want ?",
     &             /,"There are ",I3," sources to do by hand",/,
     &             " <cr> to accept or <A> to abort: "$)')NSOURCE
              read(LU(12),'(a)') ainput
              numcar = index(ainput, ' ') - 1
              if (numcar.eq.1.and.(ainput(1:1).eq.'a'.or.
     &             ainput(1:1).eq.'A')) goto 5
              if (numcar.ne.0) goto 670 
           else
              num1=nnum
              num2=nnum
           endif

           if (num1.lt.1.or.num2.gt.NSOURCE) then
              write(*,'("PSF number out of range: 1 to",
     &             i4,/)')NSOURCE
              goto 670
           endif

           do nnum=num1,num2
              tmpx = sv_true_x(nnum)
              tmpy = sv_true_y(nnum)

              if (sinput2.eq.'PA'.or.sinput2.eq.'PD') then
                 tmpx=SRC_PARMS(nnum,1)
                 tmpy=SRC_PARMS(nnum,2)
              endif

              if (sinput2.eq.'PD') goto 701
              if (.not.sv_dstsv(6,nnum).and.
     &             sinput2.eq.'PI') then
                 write(*,'("This source has NOT been identified. PSF:",
     &                I4)')nnum
                 goto 705
              endif

              if (sv_dstsv(6,nnum)) then
                 print *,'This PSF has been identified as: ',
     &                SRC_NAMES(nnum)
              else
                 print *,'This PSF has NOT been identified'
              endif

 701          if (coord_sys.eq.'C') then
                 if (sinput2.eq.'PI'.or.sinput2.eq.'PD') then
                    CALL CELGALD('GC',Pra,Pdec,tmpx,tmpy,jj)
                 elseif (sinput2.eq.'PA'.or.sinput2.eq.'PD')then
                    CALL CELGALD('CG',tmpx,tmpy,Pra,Pdec,jj)
                 endif

                 tmpx = Pra
                 tmpy = Pdec
                 if (sinput2.eq.'PD') goto 705
                 write(*,'("PSF",I4," Current ID position: ",
     &                f8.3,f7.3)')nnum,tmpx,tmpy

                 if (sinput2.eq.'PI'.and.sv_dstsv(6,nnum)) then
                    SRC_PARMS(nnum,1)=tmpx
                    SRC_PARMS(nnum,2)=tmpy
                    write(*,'("Moved ID position",
     &                   " to PSF list position. PSF: ",I4,/)')nnum
                    goto 705
                 endif

                 if (sinput2.eq.'PA') then
                    sv_dstsv(6,nnum)=JTRUE
                    sv_true_x(nnum)=tmpx
                    sv_true_y(nnum)=tmpy
                    write(*,'("Moved PSF list position",
     &                   " to ID position. PSF: ",I4,/)')nnum
                    goto 705
                 endif

                 write(*,'("Enter true RA,Dec (<A>",
     &                " to abort,",/,
     *                "<S> to skip, <cr> to accept",2f8.2,"): "$)')
     &                Pra,Pdec
              else
                 write(*,'("PSF",I4," Current ID position: ",
     &                f8.3,f7.3)')nnum,tmpx,tmpy
                 
                 if (sinput2.eq.'PI'.and.sv_dstsv(6,nnum)) then
                    SRC_PARMS(nnum,1)=tmpx
                    SRC_PARMS(nnum,2)=tmpy
                    write(*,'("Moved ID position",
     &                   " to PSF list position. PSF:",I4,/)')nnum
                    goto 705
                 endif

                 if (sinput2.eq.'PA') then
                    sv_dstsv(6,nnum)=JTRUE
                    sv_true_x(nnum)=tmpx
                    sv_true_y(nnum)=tmpy
                    write(*,'("Moved PSF list position",
     &                   " to ID position. PSF: ",I4,/)')nnum
                    goto 705
                 endif

                 write(*,'("Enter true Long,Lat(<A>",
     &                " to abort,",/,
     *                "<S> to skip, <cr> to accept",2f8.2,"): "$)')
     &                tmpx,tmpy
              endif
c
              read(LU(12),'(a)') ainput
              numcar = index(ainput, ' ') - 1
              
              if (numcar.eq.0) goto 703
              if (numcar.eq.1.and.(ainput(1:1).eq.'a'.or.
     &             ainput(1:1).eq.'A')) goto 5
              if (numcar.eq.1.and.(ainput(1:1).eq.'s'.or.
     &             ainput(1:1).eq.'S')) goto 705
              
              read(ainput,*,end=702,err=702)tmpx,tmpy
              goto 703
 702          print *,'ERROR: input error. try again....'
              goto 701
 703          continue

              if (coord_sys.eq.'C') then
                 Pra=tmpx
                 Pdec=tmpy
                 CALL CELGALD('CG',Pra,Pdec,tmpx,tmpy,jj)
              endif

              in_dex4=index(SRC_NAMES(nnum),'   ')-1

              if (in_dex4.eq.0) in_dex4=1
              if (in_dex4.lt.0) in_dex4=18

              srcN=SRC_NAMES(nnum)
 7032         write(*,'("Enter the ID sourcename",
     &             " (<A> to abort, <S> to skip)",/,
     &             " <cr> to accept current name ( ",A,
     &             " ):"$)')srcN(1:in_dex4)
              read(LU(12),'(a)') ainput
              numcar = index(ainput, ' ')

              if (numcar.gt.1.and.ainput(1:1).eq.' ') then
                 print *,' '
                 print *,'Leading spaces are not permitted !'
                 print *,' '
                 goto 7032
              endif	

              if (numcar.eq.1.and.(ainput(1:1).eq.'a'.or.
     &             ainput(1:1).eq.'A')) goto 5
              if (numcar.eq.1.and.(ainput(1:1).eq.'s'.or.
     &             ainput(1:1).eq.'S')) goto 705
              if (ainput(1:3).eq.'GRO'.and.numcar.eq.3) then
                 srcN='GRO'
                 if (coord_sys.eq.'C') then
                    srcL=Pra
                    srcB=Pdec
                 else
                    srcL=tmpx
                    srcB=tmpy
                 endif
                 
                 CALL GETSRCNM()
                 ainput=srcN
              endif

              if (numcar.gt.0) SRC_NAMES(nnum)=ainput
              sv_true_x(nnum)=tmpx
              sv_true_y(nnum)=tmpy
              sv_dstsv(6,nnum)=JTRUE
 705          continue

              if (sinput2.eq.'PD'.and.sv_dstsv(6,nnum)) then
                 rad=gtcirc(tmpx,tmpy,sv_true_x(nnum),
     &                sv_true_y(nnum))
                 if (index(tinput,'+').ne.0.and.rad.lt.0.01) goto 706
                 write(*,'(I3,1x,"Src Pos.: ",2(f7.2,1x)," ID Pos.:",
     &                2(f7.2,1x)," Dist.: ",f7.3)')nnum,tmpx,tmpy,
     *                sv_true_x(nnum),
     &                sv_true_y(nnum),rad
              endif
c
 706          continue
           enddo
           goto 5
        endif
c 
        if (sinput.eq.'F') call jlistproc
c     
        if (sinput.eq.'C'.or.sinput.eq.'B') then
 8         print *,' '
           if (sinput.eq.'C') print *,' FOR JOPT_POS '
           if (sinput.eq.'B') print *,' FOR AMOEBA '
           write(*,'(" Enter the new angular convergence",
     &          " tolerance(cr for "$)')
           if (sinput.eq.'C')
     *          write(*,'(f6.4,", A to abort): "$)')actj_flg
           if (sinput.eq.'B') write(*,'(f6.4,", A to abort): "$)')tolj
           read(LU(12),'(a)')ainput
           numcar = index(ainput, ' ') - 1
           if (numcar.eq.0) goto 5
           if (ainput(1:1).eq.'A'.or.ainput(1:1).eq.'a') goto 5

           if (sinput.eq.'C') then
              read(ainput,*,end=8,err=8)actj_flg
              if (actj_flg.ge.0.0005) goto 5
              print *,' '
              print *,' ERROR: value to low or negative < 0.0005!'
              actj_flg=0.01
              print *,' tolerance set to',actj_flg
              print *,' '
              goto 8
           else
              read(ainput,*,end=8,err=8)tolj
              if (tolj.ge.0.0001) goto 5
              print *,' '
              print *,' ERROR: value to low or negative < 0.0001 !'
              tolj=0.005
              print *,' tolerance set to',tolj
              print *,' '
              goto 8
           endif
        endif

        input='        '
        goto 5
ccccc
c
c  From this point to end of subroutine is code for auto new source
c	locating (lpi sub m  OR  lpms)  Subroutines called are MAPSRCJ,
c	FITS IO Routines, MAPVAL,VALMAP,ROISET,MAPRST,MAPMAX,MAPCOR,
C	ERROR,PSFADD,LIKTOT,JOPT_POS,GETSRCNM,MAPRITROI,PIXEL_SELECT.
C 	PSMMAT,PSFBLD AND GTCIRC.
c	
c
ccccc
 2000   continue
        tttflg=JFALSE
        tttflg2=JFALSE
        jroi=JFALSE
        determined=JFALSE
        inval=1
        jmapflg=.true.
        jtmpcnt=0

        do jj=1,500
           jsav_act(jj)=1.
        enddo

        if (NSOURCE.ge.1) then
           do jj=1,NSOURCE
              jsav_act(jj)=SRC_PARMS(jj,8)
              if (jsav_act(jj).gt.0.5) then
                 jtmpcnt=jtmpcnt+1
                 SRC_PARMS(jj,8)=0.75
              else
                 SRC_PARMS(jj,8)=0.0
              endif
           enddo

           nsrcsav=NSOURCE
        endif

        full_auto_lp=JTRUE
        mxxiter=jtmpcnt
        
        if (mxxiter.gt.50) then
           mxxiter=50
        elseif (mxxiter.lt.10) then
           mxxiter=10
        endif

        input='        '
        print *,' '
        svpsfminj1=psfminj
        svaspj1=aspj
20005   print *,' Zenith aspect cone is: ',aspj,' degrees'
        if (coord_sys.eq.'G') then
           print *,' Centered on L,B=',SC_LJJ,' ',SC_BJJ
        else
           print *,' Centered on RA,Dec=',SC_RAJ,' ',SC_DECJ
        endif
        print *,' '
c
c
        if (psfminj.lt.4) psfminj = 4
        print *,' Minimum acceptable TS value is: ',psfminj
        print *,' The value of MinTS must be >= 4 in this routine'
        print *,' '
        print *,' Enter <cr> to accept these value;'
        print *,' Enter T to change the minimum acceptable TS value;'
        print *,' Enter Z to change the Zenith Aspect cone;'
        print *,' Enter ZP to change the Pointing Direction;'
        print *,' Enter ZR to center the Pointing Direction in the',
     &          ' ROI;'
        print *,' Enter ZC to reset the Pointing Direction to the'
        print *,'          original pointing direction;'
        write(*,'(" Enter A to exit to LPI menu: "$)')
        read(LU(12),'(a)') sinput2
        numcar = index(sinput2, ' ') - 1
        call to_upper(sinput2(1:1))
        call to_upper(sinput2(2:2))
        jmapflg=.true.

        if (numcar.eq.0) goto 20010

        sinput=sinput2(1:1)
        tinput(1:2)=sinput2(1:2)
        if (sinput2(1:1).eq.'A') goto 2220
        if (sinput2(1:1).eq.'T'.or.sinput2(1:1).eq.'Z') goto 59
c
20010   exp_min=0.05
        sinput2(1:1)='M'
        aspect_max=aspj
        LOOPJsv=0
        LOOPJTSTsv=0
        aspj_sv=aspj
        tmpfnm=LMAPFILE
        sv_Gb_file=GBIASFILE
        sv_N_file=NEWMAPFILE
20006   GBIASFILE='Gbias.MS.       '
        NEWMAPFILE='Gmult.MS.      '
20011   nnum=1
        jmapflg=.true.
csb        LMAPFILE=jblank//jblank//'          '
        in_dex1=1
        in_dex2 = index(CMAPFILE, ' ')
        ind4=index(CMAPFILE,'counts.')

        if (ind4.gt.0) then
           ind4=index(CMAPFILE,'.')+1
           if (ind4.ge.in_dex2-1) ind4=1
        else
           ind4=1
        endif

        if (in_dex2.gt.1) then
           ind3 = index(GMAPFILE, ' ')
           LMAPFILE=CMAPFILE(ind4:in_dex2-1)//
     *          GMAPFILE(ind3:ind3+1)
c           LMAPFILE=CMAPFILE(ind4:in_dex2-1)//'.'//
c     *          GMAPFILE(ind3:ind3+1)
           in_dex2 = index(LMAPFILE, ' ') - 1
        endif

        if (jae_input_id_pos) then
           numcar = index(CMAPFILE, ' ')
           write(*,*)' '
           print *,' I1: ',in_dex1,'  I2: ',in_dex2
           print *,'CMAPFILE: ',CMAPFILE(1:numcar)
           write(*,*)' '
        endif

        if (in_dex1.gt.0.and.in_dex2.ge.in_dex1) then
           write(LMAPFILE,'(A,".MS.1 ")')LMAPFILE(in_dex1:in_dex2)
           print *,' '
        else
           LMAPFILE=CMAPFILE(1:in_dex2-1)//'.MS.1'
        endif

20013   if (jae_input_id_pos) then
           INQUIRE(FILE=LMAPFILE,EXIST=FEXIST)
           write(*,*)' '
           write(*,'("FEXIST: ",l2)') FEXIST
           numcar = index(LMAPFILE, ' ') - 1
           if (numcar.le.0) numcar=1
           write(*,'(" OUTPUT FILENAME:>",A," inval:",I5)')
     &          LMAPFILE(1:numcar),inval
           write(*,*)' '
        endif

 2001   do kk=1,49
           if (LMAPFILE(1:1).eq.' ') then
              LMAPFILE(1:49)=LMAPFILE(2:50)
C     
              LMAPFILE(50:50)=' '
           else 
              goto 20015
           endif
        enddo

20015   determined=JFALSE
        numcar = index(LMAPFILE, ' ') - 1
        FEXIST=.false.
        INQUIRE(FILE=LMAPFILE,EXIST=FEXIST)
        IF (FEXIST) then
           numcar = index(LMAPFILE, ' ') - 1

           if (numcar.le.0) then
              inval=1
              goto 20011
           endif

           write(*,'("Output filename:",A)')LMAPFILE(1:numcar)
           write(*,'("THIS FILE EXISTS !")')
           if (renamefile) goto 2002
           inval=inval+1
           write(*,'("renaming file with extension number:",I4)')inval
           input='#I'
           determined=JTRUE
           goto 20030
        ENDIF

 2002   input=jblank//jblank//'          '
        in_dex1=index(LMAPFILE,'.MS.')
        numcar = index(LMAPFILE, ' ') - 1
        if (numcar.le.0) numcar=1
        renamefile=JFALSE
        if (jroi) goto 2004

        write(*,'(//," Current Filename->",A)')LMAPFILE(1:numcar)
        write(*,'(//," Enter new filename ( < char*50 );")')
        write(*,'(" Enter #A for main menu;")')
        write(*,'(" Enter #R to READ this file;")')
        write(*,'(" Enter #L for list of *MS.* files;")')
        write(*,'(" Enter #I to change extension number;")')
        write(*,'(" Enter #S to restart LPIsubM command;")')
        write(*,
     &       '(" Enter #C to accept this as the OUTPUT file: ",$)')
        read(LU(12),'(a)') input
        numcar = index(input, ' ') - 1

        if (numcar.le.1.and.input(1:1).ne.'#') then
           write(*,'("You typed in one character:>",
     &          a)')input(1:1)
           write(*,*)'This will become your output filename'
           write(*,*)'(C)ontinue if you are sure you want this or'
           write(*,*)'any other key to start over'
           determined=JFALSE
           write(*,*)' '
           call intent(determined)
           if (.not.determined) goto 20011
           determined=JFALSE
        elseif (numcar.eq.1.and.input(1:1).eq.'#') then
           write(*,*)' '
           write(*,'("Improper one character input>",
     &          a)')input(1:1)
           write(*,*)'restarting LPM command'
           write(*,*)' '
           goto 20011 
        endif

        if (input(1:1).eq.'#') call to_upper(input(2:2))
        tst_input=input(1:2)
        if (numcar.eq.0) goto 2002
 2003   if (input(1:2).eq.'#S') goto 20011
        if (input(1:2).eq.'#A') then
           LMAPFILE=tmpfnm
           goto 2210
        endif

20030   if (input(1:2).eq.'#I') then
           if (.not.determined) then
              renamefile=JTRUE
20031         write(*,
     &             '(//,
     *             "Enter new extension number ( 1 <= n <= 9999):"$)')
              read(LU(12),*,err=20031)inval
           endif

           numcar=index(LMAPFILE,'.MS.')
           if (inval.ge.1.and.numcar.gt.0) then
              itlog10=int(log10(float(inval)+0.0001))
              if (itlog10.lt.1) then
                 write(LMAPFILE,
     &                '(A,I1.1)')LMAPFILE(1:numcar+3),inval
              elseif (itlog10.eq.1) then
                 write(LMAPFILE,
     &                '(A,I2.2)')LMAPFILE(1:numcar+3),inval
              elseif (itlog10.eq.2) then
                 write(LMAPFILE,
     &                '(A,I3.3)')LMAPFILE(1:numcar+3),inval
              elseif (itlog10.eq.3) then
                 write(LMAPFILE,
     &                '(A,I4.4)')LMAPFILE(1:numcar+3),inval
              elseif (itlog10.ge.4) then
                 print *,' '
                 print *,' The input value is too big: ',inval
              endif
           elseif (inval.le.0) then
              write(LMAPFILE,
     &             '(A,"0           ")')LMAPFILE(1:numcar+3)
           else
              numcar = index(CMAPFILE, ' ')
              LMAPFILE=
     &             CMAPFILE(ind4:in_dex2-1)//'.'//
     *             GMAPFILE(ind3:ind3+1)//'.MS.100'
              inval=100
           endif
           goto 20013
        endif

        if (input(1:2).eq.'#C') then
           numcar = index(LMAPFILE, ' ')
           if (in_dex1.eq.0) then
              print *,'Output filename: ',LMAPFILE(1:numcar)
           else
              print *,'Output filename: ',LMAPFILE(1:numcar)
           endif
           goto 2004
        endif

        indxa=index(input,'#')
        indxb = index(input, ' ')

        if (input(1:1).eq.'#'.and.indxb.eq.3) then
           if (input(2:2).eq.'L') then
              print *,' '
              CALL SYSTEM('ls -l *MS.*')
              print *,' '
              goto 2001
           endif

           if (input(2:2).eq.'R') then
              MAPFILE=LMAPFILE
              TMPTYP='LSTA'
              NFILE=0
              jroi=JTRUE
              CALL MAPRST(TMPMAP,CTLMSZ1,CTLMSZ2)
              CALL MAPRST(XRRMAP,CTLMSZ1,CTLMSZ2)
              CTLMSZ1_sav=CTLMSZ1
              CTLMSZ2_sav=CTLMSZ2
              CTLORG1_sav=CTLORG(1)
              CTLORG2_sav=CTLORG(2)
              CTLEND1_sav=CTLEND(1)
              CTLEND2_sav=CTLEND(2)
              CTLSCL_sav=CTLSCL
              CALL FITRED(XRRMAP,TMPTYP,TMPDOC,NFILE,.false.)
c
c---> temporary line to fix error in hdrred.f
c
              TMPTYP=MAPTYPE
c--->
              if (NFILE.eq.1.and.TMPTYP.eq.'Like') then
                 CALL FITRED(XRRMAP,TMPTYP,TMPDOC,NFILE,.false.)
              else
                 print *,' '
                 SIGNAL='N'
                 SIGMSG='ERROR: Input file has more than one image'
                 CALL ERROR(0,LOC)
                 goto 2001
              endif

              FITSCL=CTLSCL
              IF (FITSCL.NE.CTLSCL) then
                 SIGNAL='s'
                 SIGMSG='Error: FITSCL does not equal CTLSCL'
                 CALL ERROR(0,LOC)
                 goto 2001
              endif

              if (SIGNAL.eq.' ') then
                 ROIORG(1)=CTLORG(1)
                 ROIORG(2)=CTLORG(2)
                 ROIEND(1)=CTLEND(1)
                 ROIEND(2)=CTLEND(2)
                 FITMSZ1=CTLMSZ1
                 FITMSZ2=CTLMSZ2
                 print *,' FITMSZ1: ',FITMSZ1,' FITMSZ2: ',FITMSZ2
              endif

              CTLSCL=CTLSCL_sav
              CTLMSZ1=CTLMSZ1_sav
              CTLMSZ2=CTLMSZ2_sav
              CTLORG(1)=CTLORG1_sav
              CTLORG(2)=CTLORG2_sav
              CTLEND(1)=CTLEND1_sav
              CTLEND(2)=CTLEND2_sav

              IF (SIGNAL.NE.' ') then
                 CALL ERROR(0,LOC)
                 goto 2220
              endif

              pL1=ROIORG(1)
              pB1=ROIORG(2)
              pL2=ROIEND(1)
              pB2=ROIEND(2)
              CALL ROISET(pL1,pB1,pL2,pB2,ROIORG,ROIEND,ROIPRG,ROIPND)
              IFMTMP=0
              TTESY=-1

              do IFM2=1,FITMSZ2
                 pB=ROIORG(2)+FITSCL*(IFM2-1)
                 do IFM1=1,FITMSZ1
                    IMAPX=(IFM2-1)*FITMSZ1 + IFM1
                    CTLMSZ1=FITMSZ1
                    CTLMSZ2=FITMSZ2
                    YTTES=MAPVAL(XRRMAP,IFM1,IFM2,CTLMSZ1,CTLMSZ2)
                    CTLMSZ1=CTLMSZ1_sav
                    CTLMSZ2=CTLMSZ2_sav
                    pL=ROIORG(1)+FITSCL*(IFM1-1)
                    ILL= 1+ INT(0.500+(pL-CTLORG(1))/CTLSCL)
                    ILB= 1+ INT(0.500+(pB-CTLORG(2))/CTLSCL)
                    CALL VALMAP(YTTES,TMPMAP,ILL,ILB,CTLMSZ1,CTLMSZ2)
                 enddo
              enddo

              CTLMSZ1=CTLMSZ1_sav
              CTLMSZ2=CTLMSZ2_sav
              CALL MAPRST(XRRMAP,CTLMSZ1,CTLMSZ2)
              CALL MAPMAX(TMPMAP,IsrcL,IsrcB,bigTS,CTLMSZ1,CTLMSZ2)
              print *,' '
              print *,' IL,IB in FITS CTL units'
              print *,' IL, IB: ',IsrcL,',',IsrcB,' bigTS: ',bigTS
              print *,' '
              in_dex1=index(LMAPFILE,'.MS.')-1
              if(in_dex1.le.0)in_dex1=numcar
              LOOPJ=0
              numcar=in_dex1
              if (ROIPRG(1).eq.1.and.ROIPRG(2).eq.1.and.
     &             ROIPND(1).eq.CTLMSZ1.and.
     *             ROIPND(2).eq.CTLMSZ2) goto 2004
20034         print *,' '
              print *,' ROI has been set by FITS file.'
              print *,' Enter R to reset ROI to CTL;'
              write(*,'(" Enter <cr> to accept ROI:"$)')
              read(LU(12),'(a)') tinput
              nnh = index(tinput, ' ') - 1
              if (nnh.eq.0) goto 2004
              if (nnh.gt.1) goto 20034
              if (tinput(1:1).eq.'r') tinput(1:1)='R'
              if (nnh.eq.1.and.tinput(1:1).ne.'R'.and.
     &             tinput(1:1).ne.'r') goto 20034
              call ROISET(CTLORG(1),CTLORG(2),CTLEND(1),CTLEND(2),
     &             ROIORG,ROIEND,ROIPRG,ROIPND)
              goto 20006
           endif
        endif

        LMAPFILE=input(1:numcar)
        renamefile=JTRUE
        goto 2001
 2004   FEXIST=.false.
        INQUIRE(FILE=LMAPFILE,EXIST=FEXIST)
        in_dex1 = index(LMAPFILE, ' ') - 1
        
        if (jroi.and.in_dex1.gt.0) goto 20049
20042   if (in_dex1.le.0) then
           print *,' '
           write(*,'("The filename has no length !")')
           write(*,'("Please enter a new output filename ",
     &          "or #A to abort")')
           print *,' '
           goto 2001
        endif

        in_dex1 = index(LMAPFILE, ' ') - 1
        if (FEXIST) then
20043      print *,' '
           write(*,'("File: ",A," exists !")')LMAPFILE(1:in_dex1)
           write(*,'("Enter E to erase the old file")')
           write(*,'("Enter A to abort to LPI menu")')
           write(*,'("Enter <cr> to rename file: "$)')
           read(LU(12),'(a)') moreinput
           numcai = index(moreinput, ' ') - 1

           if (numcai.eq.0) then
              in_dex1=-1
              goto 2001
           endif

           if (moreinput(1:1).eq.'A'.or.moreinput(1:1).eq.
     &          'a') goto 2220
           if (moreinput(1:1).eq.'E'.or.moreinput(1:1).eq.
     &          'e') then
              tinput='/bin/rm -f '//LMAPFILE
              CALL SYSTEM(tinput)
              goto 20044
           endif

           write(*,'(/,"Invalid input. Try again....",/)')
           goto 20043
        endif

20044   i=index(LMAPFILE,'.MS.')
        jindex=index(GBIASFILE,'.MS.')-1
        jindexb = index(GBIASFILE, ' ') - 1

        if (i.gt.0) then
           GBIASFILE=GBIASFILE(1:jindex)//LMAPFILE(i:in_dex1)
        endif

        jindex = index(GBIASFILE, ' ') - 1
        if (jindex.gt.0) INQUIRE(file=GBIASFILE,EXIST=FEXIST)
        if (jindex.le.0) then
           write(*,'("The GBIAS filename has no length !")')
           write(*,'("Please enter a new output filename ",
     &          "or <cr> to abort"):')
           write(*,'("Filename: "$)')
           read(LU(12),'(a)') GBIASFILE
           numcai = index(GBIASFILE, ' ') - 1
           if(numcai.eq.0)goto 2220
           goto 20044
        endif

        INQUIRE(file=GBIASFILE,EXIST=FEXIST)
        jindex = index(GBIASFILE, ' ') - 1
        if (FEXIST) then
20045      write(*,'("File: ",A," exists !")')GBIASFILE(1:jindex)
           write(*,'("Enter E to erase the old file")')
           write(*,'("Enter A to abort to menu")')
           write(*,'("Enter <cr> to rename file: "$)')
           read(LU(12),'(a)') moreinput
           numcai = index(moreinput, ' ') - 1

           if (numcai.eq.0) then
              numcar=-1
              goto 20044
           endif

           if (moreinput(1:1).eq.'A'.or.moreinput(1:1).eq.
     &          'a') goto 2220
           if (moreinput(1:1).eq.'E'.or.moreinput(1:1).eq.
     &          'e') then
              tinput='/bin/rm -f '//GBIASFILE(1:jindex)
              CALL SYSTEM(tinput)
              goto 20047
           endif

           write(*,'(/,"Invalid input. Try again....",/)')
           goto 20045
        endif

20047   continue
        jindex2=index(NEWMAPFILE,'.MS.')-1

        if (i.gt.0) then
           NEWMAPFILE=NEWMAPFILE(1:jindex2)//LMAPFILE(i:in_dex1)
        endif

        jindex2 = index(NEWMAPFILE, ' ') - 1
        if (jindex2.gt.0) INQUIRE(file=NEWMAPFILE,EXIST=FEXIST)
        if (jindex2.le.0) then
           write(*,'("The NEWMAP filename has no length !")')
           write(*,'("Please enter a new output filename ",
     &          "or <cr> to abort"):')
           write(*,'("Filename: "$)')
           read(LU(12),'(a)') NEWMAPFILE
           numcai = index(NEWMAPFILE, ' ') - 1
           if (numcai.eq.0) goto 2220
           goto 20047
        endif

        if (FEXIST) then
20048      write(*,*)'Gmult mapfile exists !'
           write(*,'("File: ",A)')NEWMAPFILE(1:jindex2)
           write(*,'("Enter E to erase the old file")')
           write(*,'("Enter A to abort to menu")')
           write(*,'("Enter <cr> to rename file: "$)')
           read(LU(12),'(a)') moreinput
           numcai = index(moreinput, ' ') - 1
           if (numcai.eq.0) then
              NEWMAPFILE='          '
              jindex2=-1
              goto 20047
           endif

           if (moreinput(1:1).eq.'A'.or.moreinput(1:1).eq.
     &          'a') goto 2220
           if (moreinput(1:1).eq.'E'.or.moreinput(1:1).eq.
     &          'e') then
              tinput='/bin/rm -f '//NEWMAPFILE(1:numcar)
              CALL SYSTEM(tinput)
              goto 20049
           endif
           
           write(*,'(/,"Invalid input. Try again....",/)')
           goto 20048
        endif

20049   continue
        write(*,'(" Last Chance to abort this proceedure",
     &       " at this level!")')
        write(*,'("Enter A to abort, C to continue:"$)')
        read(LU(12),'(a)') tinput
        nnh = index(tinput, ' ') - 1
            
        if (tinput(1:1).eq.'a'.or.tinput(1:1).EQ.'A') goto 2220
        if (tinput(1:1).ne.'C'.and.tinput(1:1).ne.'c') goto 20049
        if (jroi.eqv..false.) call MAPSRCJ(JTRUE)
        if (signal.ne.' ') then
           CALL ERROR(0,LOC)
           goto 2220
        endif
        
 2005   LOOPJ=0
        if (coord_sys.eq.'C') then
           pL=SC_RAJ
           pB=SC_DECJ
        else
           pL=SC_LJJ
           pB=SC_BJJ
        endif
        
        if (in_dex1.le.0) in_dex1=index(LMAPFILE, ' ') - 1
        print *,'Working on map ->',LMAPFILE(1:in_dex1),
     &       ' This may take a while'
 2006   continue
        LOOPJ=LOOPJ+1
 2008   CALL MAPMAX(TMPMAP,IsrcL,IsrcB,bigTS,CTLMSZ1,CTLMSZ2)
        CALL MAPCOR(IsrcL,IsrcB,srcLpxcnt,srcBpxcnt)
        CALL ERROR(0,LOC)
        xjsrcL=srcLpxcnt
        xjsrcB=srcBpxcnt
        srcL=xjsrcL
        srcB=xjsrcB
        CALL PIXEL_SELECT(JFALSE,jblank)
        TS=-1.
        signal=' '
        CALL SRCTEST(.false.)

        if (abs(bigTS-TS).gt.0.1) then
           CALL VALMAP(TS,TMPMAP,IsrcL,IsrcB,CTLMSZ1,CTLMSZ2)
           goto 2008
        endif

        if (NSOURCE.ge.1) then
           do jj=1,NSOURCE
              tmpL=SRC_PARMS(jj,1)
              tmpB=SRC_PARMS(jj,2)
              tmpz=gtcirc(srcL,srcB,tmpL,tmpB)
              if (jsav_act(jj).gt.0.5) svo_TS(jj)=svn_TS(jj)
              if (jsav_act(jj).gt.0.5) SRC_PARMS(jj,8)=0.75
              if (tmpz.gt.2.*Ranal.and.jsav_act(jj).gt.0.5) then
                 SRC_PARMS(jj,8)=0.0
              elseif (tmpz.le.1.05*Ranal.and.
     *                jsav_act(jj).gt.0.5) then
                 SRC_PARMS(jj,8)=jsav_act(jj)
              endif
           enddo
        endif       
    
        WRITE(LU(1),'("HIGHEST SIGNIFICANCE (TS=",f7.1,
     &       ")  IS AT: ",2f6.1)') bigTS,xjsrcL,xjsrcB

        if (bigTS.lt.0.9*psfminj) then
           CALL JFNDPOS(JFALSE,JFALSE)
           if (signal.ne.' ') then
              call ERROR(0,LOC)
              goto 2200
           endif

           if (TS.lt.0.9*psfminj) goto 2200
           xjsrcL=srcL
           xjsrcB=srcB
        endif

        if (NSOURCE.gt.499) then
           print *,'Number of sources exceeds maximum value: ',
     &          NSOURCE
           LMAPFILE=tmpfnm
           input=jblank//jblank//'          '
           goto 2210
        endif

        NSOURCE=NSOURCE+1
        jsav_act(NSOURCE)=1
        SRC_PARMS(NSOURCE,1)=xjsrcL
        SRC_PARMS(NSOURCE,2)=xjsrcB
        SRC_PARMS(NSOURCE,3)=Counts
        cnts=Counts
        SRC_PARMS(NSOURCE,4)=2.0
        SRC_PARMS(NSOURCE,8)=1
        SRC_NAMES(NSOURCE)='GRO J'
        srcN='GRO J'
        svo_TS(NSOURCE)=TS
c     
        CALL PSFADD(Counts,bmap,CTLMSZ1,CTLMSZ2)
        LikTotaled=JFALSE
        CALL LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
c
c
c
        nksf1=0
        nksf2=0
        print *,' Setting source activity'

        if (NSOURCE.gt.1) then
           do jjj=1,NSOURCE-1
              if (SRC_PARMS(jjj,8).gt.0.8.and.jjj.gt.nsrcsav) then
                 aTSj=abs(svo_TS(jjj)-svn_TS(jjj))
                 if (aTSj.lt.0.2.and.svon_zen(jjj).lt.0.1)
     *                SRC_PARMS(jjj,8)=0.75
              endif
              
              tstp8=SRC_PARMS(jjj,8)
              if (tstp8.gt.0.8) nksf1=nksf1+1
              if (tstp8.gt.0.6.and.tstp8.lt.0.8) nksf2=nksf2+1
           enddo
        endif

        print *,' '
        print *,' There are ',nksf1,' fully active sources'
        print *,' There are ',nksf2,' position fixed sources'
        print *,' '
        autoouttmp=autooutput
        autooutput=JFALSE
        jnmflg=JTRUE
        CALL JOPT_POS(JFALSE)
        autooutput=autoouttmp
        jnmflg=JFALSE

        if (NSOURCE.gt.1) then
           if (gtcirc(SRC_PARMS(NSOURCE-1,1),SRC_PARMS(NSOURCE-1,2),
     &          SRC_PARMS(NSOURCE,1),
     *          SRC_PARMS(NSOURCE,2)).lt.0.05) then
              LOOPJSV=LOOPJSV+1
           else
              LOOPJSV=0
           endif

20085      if (LOOPJSV.ge.2) then
              print *,' '
              print *,'-------> ERROR <-----------'
              print *,' '
              print *,' Program may be in an infinite loop !!!!!'
              print *,' For last ',LOOPJSV+1,' source positions:'
              print *,' '

              do kkk=NSOURCE-LOOPJSV,NSOURCE
                 print *,' N:',kkk,' L(RA)= ',SRC_PARMS(kkk,1),
     &                ' B(DEC)= ',SRC_PARMS(kkk,2),
     *                ' Name:',SRC_NAMES(kkk)
              enddo

              NSOURCE=NSOURCE-1
              call PSFBLD
              goto 2200
           endif
        endif

        print *,' '
        print *,' LOOP: ',LOOPJ
        print *,' New source moved to: ',xjsrcL,',',xjsrcB,
     *       '  Counts: ',cnts
        print *,' New TS: ',svn_TS(NSOURCE)
        print *,' '
c
        if (script_on) then
           LOOPTST=MOD(LOOPJ,10)
        else
           LOOPTST=MOD(LOOPJ,50)
        endif

        if (LOOPTST.eq.0.and..not.script_on) then
           print *,' Completed ',LOOPJ,' loops'
           write(*,
     *          '("C to continue, any other key to abort auto MS: "$)')
           read(LU(12),'(a)') input
           numcar = index(input, ' ') - 1
           
           if (numcar.eq.1.and.input(1:1).eq.'C') goto 2010
           if (numcar.gt.0) then
              LMAPFILE=tmpfnm
              goto 2200
           endif
        endif

        if (LOOPTST.eq.0.and.script_on) then
           print *,' Completed ',LOOPJ,' loops'
        endif
 2010   continue

        if (NSOURCE.gt.0) then
           do jjj=1,NSOURCE
              SRC_PARMS(jjj,8)=jsav_act(jjj)
           enddo
        endif

        if (autooutput) call auto_out(4)
        jjj=25
        if (NSOURCE.le.50) jjj=10
        if (NSOURCE.le.25) jjj=5
        if (NSOURCE.le.10) jjj=2
        if (MOD(NSOURCE-nsrcsav,jjj).eq.0) CALL PSFBLD
        goto 2006
 2200   continue

        if (NSOURCE.gt.0) then
           do jjj=1,NSOURCE
              SRC_PARMS(jjj,8)=jsav_act(jjj)
           enddo
        endif

        moreinput=jblank//jblank//'          '
        moreinput='.MS.'
        in_dex2=index(LMAPFILE,'.MS.')
        
        if (in_dex2.eq.0) then
           LMAPFILE=
     &          LMAPFILE(1:(index(LMAPFILE,' ')-1))//
     *          moreinput(1:4)//'1'
           goto 2200
        endif

        if (in_dex2.ne.0) then
           in_dex3 = index(LMAPFILE, ' ') - 1
           moreinput=jblank//jblank//'          '
           read(LMAPFILE(in_dex2+4:in_dex3+1),*,err=22005)inval
           inval=inval+1

           if (inval.gt.0.and.inval.lt.10) then
              write(moreinput,'(".MS.",i1)')inval
           elseif (inval.ge.10.and.inval.lt.100) then
              write(moreinput,'(".MS.",i2)')inval
           elseif (inval.ge.100) then
              write(moreinput,'(".MS.",i3)',err=22005)inval
           elseif (inval.ge.1000.and.inval.le.9999) then
              write(moreinput,'(".MS.",i4)',err=22005)inval
           endif

           in_dex1=in_dex2-1
           in_dex4 = index(moreinput, ' ') - 1
           tmpfnm2=LMAPFILE(1:in_dex1)//moreinput(1:in_dex4-1)
           INQUIRE(file=tmpfnm2,EXIST=FEXIST)
           if (FEXIST) goto 2200
           goto 22010
22005      moreinput=jblank//jblank//'          '
           inval=65
           input=jblank//jblank//'          '
           in_dex3 = index(LMAPFILE, ' ') - 1
22007      moreinput=LMAPFILE(1:in_dex3)//char(inval)//input(1:3)
           rename_file=JTRUE
           in_dex4 = index(moreinput, ' ') - 1
           INQUIRE(file=moreinput(1:in_dex4),EXIST=FEXIST)

           if (FEXIST) then
              inval=inval+1
              if (inval.eq.91) inval=97
              if (inval.gt.122) then
                 input='tmp'
                 inval=65
                 goto 22007
              endif
           endif

           moreinput=LMAPFILE(in_dex2:in_dex3)//
     &          char(inval)//input(1:3)//' '
           in_dex4 = index(moreinput, ' ') - 1
        else
           moreinput='.MS.2   '
           in_dex4 = index(moreinput, ' ') - 1
        endif

22010   in_dex1=index(LMAPFILE,'.MS.')-1
        if (in_dex1.le.0) in_dex1=index(LMAPFILE,' ')-1
        in_dex4 = index(moreinput, ' ') - 1
        LMAPFILE=LMAPFILE(1:in_dex1)//moreinput(1:in_dex4)
        jindex=index(GBIASFILE,'.MS.')-1
        if (jindex.le.0) jindex=index(GBIASFILE,' ') - 1
        if (jindex.le.0) GBIASFILE='Gbias'
        GBIASFILE=GBIASFILE(1:jindex)//moreinput(1:in_dex4)
        jindex2=index(NEWMAPFILE,'.MS.')-1
        if (jindex2.le.0) jindex2=index(NEWMAPFILE, ' ') - 1
        if (jindex2.le.0) NEWMAPFILE='Gmult'
        NEWMAPFILE=NEWMAPFILE(1:jindex2)//moreinput(1:in_dex4)
        input='lpiw.'//LMAPFILE(1:in_dex1+in_dex4)
        INQUIRE(file=input,EXIST=FEXIST)

        if (FEXIST) then
           input='/bin/rm -i '//input(1:in_dex1+in_dex4+5)
           call system(input)
        endif

        sinput='W'
        jmapflg=.true.

        if (jae_input_id_pos)then
           print *,' '
           print *,' Saving LPIW output'
           print *,' jmapflg: ',jmapflg
           print *,' save file: ',input(1:in_dex1+in_dex4+5)
           print *,' Lmapfile: ',LMAPFILE(1:in_dex1+in_dex4)
           print *,' Gmultfile: ',NEWMAPFILE(1:5+in_dex4)
           print *,' Gbiasfile: ',GBIASFILE(1:5+in_dex4)
           print *,' '
        endif

        goto 511
22012   sinput='M'
        if (jae_input_id_pos) then
           print *,' '
           print *,' Return from LPIW output'
           print *,' jmapflg: ',jmapflg
           print *,' Lmapfile: ',LMAPFILE(1:in_dex1+in_dex4)
           print *,' Gmultfile: ',NEWMAPFILE(1:5+in_dex4)
           print *,' Gbiasfile: ',GBIASFILE(1:5+in_dex4)
           print *,' '
        endif

        input=jblank//jblank
        CALL MAPSRCJ(JFALSE)
c
 2210   if (NSOURCE.gt.nsrcsav) then
           CLAT=srcB
           CALL PSMMAT(lshift,bshift)
           LikTotaled=.false.
           write(*,*)'Rebuilding the PSF map'
           call PSFBLD
        endif

        print *,' '

        if (rename_file) then
           print *,' The final output files have been ',
     *          'renamed to avoid'
           print *,' overwritting.  The new output filenames are:'
        else
           print *,'The final output filenames are:'
        endif

        jindex = index(LMAPFILE, ' ')
        print *,'TS file: ',LMAPFILE(1:jindex)
        jindex = index(LMAPFILE, ' ')
        print *,'GMULT file: ',NEWMAPFILE(1:jindex)
        jindex = index(GBIASFILE, ' ')
        print *,'GBIAS file: ',GBIASFILE(1:jindex)
        print *,' '
        LMAPFILE=tmpfnm
        moreinput=jblank//jblank//'          '
        GBIASFILE=sv_Gb_file
        NEWMAPFILE=sv_N_file

        if(NSOURCE.gt.0)then
           do jj=1,NSOURCE
              SRC_PARMS(jj,8)=jsav_act(jj)
           enddo
        endif

        print *,' Restoring parameters:'
        psfminj=svpsfminj1
        aspj=svaspj1
        print *,'Zenith aspect cone: ',aspj,' degrees'
        print *,'Minimum acceptable TS value: ',psfminj
        if (jroi) call ROISET(CTLORG(1),CTLORG(2),CTLEND(1),
     *       CTLEND(2),ROIORG,ROIEND,ROIPRG,ROIPND)
        rename_file=JFALSE
        goto 3003
 2220   LMAPFILE=tmpfnm
        moreinput=jblank//jblank//'          '
        GBIASFILE=sv_Gb_file
        NEWMAPFILE=sv_N_file
        print *,' Restoring parameters:'
        psfminj=svpsfminj1
        aspj=svaspj1
        print *,'Zenith aspect cone: ',aspj,' degrees'
        print *,'Minimum acceptable TS value: ',psfminj
        if (jroi) call ROISET(CTLORG(1),CTLORG(2),CTLEND(1),
     *       CTLEND(2),ROIORG,ROIEND,ROIPRG,ROIPND)
        rename_file=JFALSE
        goto 3003
 3001   print *,' '
        print *,'There are NO PSF sources defined'
        print *,' '
        goto 5
 3002   print *,'Rebuilding the PSF map'
        CALL PSFBLD
        goto 5
 3003   continue
        if (tst_input.ne.'#A') call auto_out(4)
        
        if (NSOURCE.gt.0) then
           do jj=1,NSOURCE
              SRC_PARMS(jj,8)=jsav_act(jj)
           enddo
        endif
        
        goto 5
        end
c
