C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C	SUBROUTINE PSFADJ
C
C
C  $Id: psfadj.f,v 1.4 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++   effect: Provide  adjustment of the other PSF map for LIKE program.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C=======================================================================
C%   Changes:
c	4/29/94 JRM List if N<31
c
C  $Log: psfadj.f,v $
C  Revision 1.4  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.3  2005/08/26 19:36:35  irby
C  Purely cosmetic changes to allow compilation with gfortran/g95, mostly
C  involving fixes to lines longer than 72 chars that were being truncated,
C  but in the case of the CGRO code, also moving misplaced (tabbed) line
C  continuation characters to their appropriate position in column 6.
C
C  Revision 1.2  2002/12/26 17:42:55  irby
C  Fix read/write statements (for f90 compatibility): add/remove commas and
C  fix lines over 72 chars.
C
C  Revision 1.1  2002/04/16 20:27:42  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c MISC_DIR is now read from like.par.  Deleted call to getenv to get this
c variable - Sandhia Bansal 12/01
c
c Revision 5.12  1998/06/09  17:00:52  jae
c Fixed minor typo from last edit.  Just I/O
c header line for output PSF files.
c
c Revision 5.11  1998/05/21  16:03:41  jae
c Another very minor screen output I/O fixup.
c
c Revision 5.10  1998/05/21  15:49:07  jae
c Made slight modification to a descriptive output line for
c screen dump of test source list.
c
c Revision 5.9  1998/05/20  17:34:15  jae
c Added screen output of Intensity if the test source
c has been optimized.
c
c Revision 5.8  1998/04/07  18:26:38  jae
c Repaired typo on line 569 which prevented build
c
c Revision 5.7  1998/04/07  18:13:52  jae
c Updated (revised) code which determined on PA_I command
c if the file coordinate system matched the coordinate
c system currently in use.  This is done by identifying the
c character strings ' L ' or ' RA ' on line 2 of the input
c PSF file.  If the strings are not found the user is notified
c and NO INPUT or conversion occurs.  If the one of the strings
c is found the correct action (conversion/no conversion of file
c coordinates) is taken.
c Also, changed the parameters used for aspect cone distance
c ( center of cone is now at variables (SC_LJJ,SC_BJJ) or
c (SC_RAJ,SC_DECJ) rather than the fixed map center).
c
c Revision 5.6  1998/02/23  15:38:48  jae
c Appended flux value to end of each output line of PSF output file.
c Note that the TS and Flux are only outputed when the source has been
c optimized.
c
c Revision 5.5  1996/04/23  17:48:33  jae
c changed line after 15 continue to skip
c 'bad' input rather than abort.  'bad' input
c is reported to user on console.
c made 'I' input of file do enddo block as do ... 17    enddo
c for goto statement.
c
c Revision 5.4  1996/03/12  20:55:11  jae
c Corrected reading PSF file to convert coordinate
c system if file is NOT in the counts map coordinate
c system.  Also took out some old commented out lines
c
c Revision 5.3  1996/03/05  16:04:00  jae
c Repaired output of psf file and alligned the output in said
c file of the TS variable
c
c Revision 5.2  1996/02/29  20:52:27  jae
c *** empty log message ***
c
c Revision 5.1  1996/02/27  23:19:28  jae
c Repaired bug in the PA sub O command.  The
c output unit number for the TS was wrong (6 instead
c of 8) which caused no line feed to be written.  jae
c
c Revision 5.0  1996/02/13  21:55:46  jae
c Subroutine Module for like V5.00
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	SUBROUTINE PSFADJ

C	Common blocks used:
	INCLUDE  '../COMMON/likrep.copy'
	INCLUDE  '../COMMON/ctlrep.copy'
	INCLUDE  '../COMMON/cnfrep.copy'
	INCLUDE  '../COMMON/bmprep.copy'
        INCLUDE  '../COMMON/errrep.copy'
	INCLUDE  '../COMMON/emprep.copy'
	INCLUDE  '../COMMON/gasrep.copy'
	INCLUDE  '../COMMON/psmrep.copy'
cj--->
cj---> next three lines added by JAE 23/03/94: common block and local variable
cj--->
        INCLUDE  '../COMMON/roirep.copy'
	INCLUDE  '../COMMON/locpos.copy'

        save

	character(80) id
	common /id/id
	character exdir*18
cj--->
cj---> End of changed code  /JAE
c
        character input*80,coord_phr*10
c       CHARACTER ex_dir*60
	LOGICAL FEXIST,determined, convert
	real mapval

	id = '$Id: psfadj.f,v 1.4 2013/05/21 19:08:26 irby Exp $'
        LOC='PSFADJ'


        write(6,'("You have elected to alter the other PSF map.")')
 1	continue

	if (NSOURCE.lt.1) then
	   write(6,*)'No sources in other PSF map.'

	elseif (NSOURCE.le.30) then
	   write(6,'("The current other PSF map contains:")')
	   write(6,'("  #   NAME                  POSITION    ",
     1	         "   CNTS   Sp. I.   Active      TS      Intensity")')
	   do nsrc=1,NSOURCE
	      theLong=SRC_PARMS(nsrc,1)

	      if (theLong.lt.0.) theLong=theLong+360.

	      write(6,'(
     1	           i3,3x,A18,2f7.2,f10.1,f6.2,f9.2$)')
     2             nsrc,SRC_NAMES(nsrc),theLong,
     3             (SRC_PARMS(nsrc,ip),ip=2,4),SRC_PARMS(nsrc,8)

	      if (sv_dstsv(1,nsrc)) then
		 write(6,'(5x,F8.2,2x,E13.6)')svn_TS(nsrc),sv_flx(1,nsrc)
	      else
		 write(6,*)' '
	      endif
	   enddo

	else
	   write(6,'(
     1	         "The other PSF map contains",I5,
     2	         " sources. Use L for a listing.")')NSOURCE
	endif

 100	continue
	if (NSOURCE.lt.1) then
	   write(6,'("PSF_ADJ:A,D,I,Q,Z,?>>",$)')
	else
	   write(6,'("PSF_ADJ:A,D,I,L,M[n],O,Q,R[n],T,X[n],Z,.,?>>",$)')
	endif

	READ(LU(12),'(A)') input
        if (input.eq.'?') then
	   write(6,'(
     1     "        Other PSF Map Sub-menu:",/,
     2     "Enter:",/,
     &     " A to Add the current psf to the other psf map;",/,
     3     " D to add a Different psf;",/,
     4     " I to build the other psf from an Input disk file;",/,
     5     " L list the contents of other psf map;",/,
     6     " M to toggle psf active Mark of PSF [number n];",/,
     7     " MA0 to set all psf active Marks to inactive;",/,
     8     " MA1 to set all psf active Marks to active;",/,
     9     " MAF to set all psf active Marks to fixed active;",/,
     1     " MAR to set all psf active Marks within ROI to active",/,
     2     "   and set all psf active Marks outside ROI to inactive;",/,
     3     " O to write psf parameters to an Output disk file;",/,
     4     " Q to Quit (return to main menu without making change;",/,
     5     " R to Replace a specific psf [number n];",/,
     6     " T to Transfer the other psf map to the gasmap;",/,  
     7     " X to remove (X-out) a specific psf [number n];",/,  
     8     " Z to change the PSF Zenith angle;",/,  
     9     " . to clear the other psf map;",/,
     1     " ? print this list.")')
	   goto 100

	elseif (input.eq.'z'.or.input.eq.'Z') then
	   write(lu(1),'(
     1     "The current PSF is built from the Zenith angle selection:",
     2      /,9I1)')THETA
	   write(lu(1),*)'Input new Zenith angle selection:'
	   READ(LU(12),'(9I1)') THETA
	   CALL PSMGET
	   CALL ERROR(1,LOC)
	   LikTotaled=.false.
	   goto 100

	elseif (input.eq.'l'.or.input.eq.'L') then
	   write(6,'("The current other PSF map contains:")')
	   if (NSOURCE.lt.1) then
	      write(6,*)'No sources in other PSF map.'
	   else
	      write(6,'("  #   NAME                  POSITION    ",
     1           "   CNTS   Sp. I.   Active      TS      Intensity")')
	      do nsrc=1,NSOURCE
		 theLong=SRC_PARMS(nsrc,1)
		 if (theLong.lt.0.) theLong=theLong+360.
		 write(6,'(
     1             i3,3x,A18,2f7.2,f10.1,f6.2,f9.2$)')
     2             nsrc,SRC_NAMES(nsrc),theLong,
     3             (SRC_PARMS(nsrc,ip),ip=2,4),SRC_PARMS(nsrc,8)
		 if (sv_dstsv(1,nsrc)) then
		    write(6,'(5x,F8.2,2x,E13.6)')svn_TS(nsrc),sv_flx(1,nsrc)
		 else
		    write(6,*)' '
		 endif
	      enddo
	   endif
	   goto 100

	elseif (input.eq.'.') then
	   write(6,*)
	   call MAPRST(BMAP,CTLMSZ1,CTLMSZ2)
	   write(lu(1),*) 'Setting other PSF map to null.'
cj--->
cj---> added LPxx array reset to zero state
cj--->
	   do ll=1,NSOURCE
	      sv_true_x(ll) = 0
	      sv_true_y(ll) = 0
	      sv_err68(ll)= 0
	      sv_err95(ll)= 0
	      sv_sigsv(ll)= ' '
	      best_choice(ll)='  '
	      best_x(ll) = 0
	      best_y(ll) = 0
	      sv_params(1,ll)=-1
	      sv_params(2,ll)=-1
	      sv_flx(1,ll) = 0
	      sv_flx(2,ll) = 0
	      sv_cnts(1,ll) = 0
	      sv_cnts(2,ll) = 0
	      sv_upperlim(ll) = 0
	      sv_expose(ll) = 0
	      sv_tmp_68_y(ll)=0
	      sv_tmp_68_x(ll)=0
	      sv_tmp_95_x(ll)=0
	      sv_tmp_95_y(ll)=0
	      svn_TS(ll)=0

	      do kk=1,10
		 sv_dstsv(kk,ll)=.false.
	      enddo
	   enddo
cj--->
cj---> End of changed code  /JAE
cj--->^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
cj--->
	   NSOURCE=0
	   LikTotaled=.false.
	   goto 1

	elseif (input.eq.'A'.or.input.eq.'a'
     1          .or.input.eq.'D'.or.input.eq.'d') then
	   nsrc=NSOURCE+1

	   if (nsrc.gt.500) then
	      write(6,*) 'Sorry, only 500 PSFs at once in other PSF map'
	      goto 1
	   endif

	   if (input.eq.'D'.or.input.eq.'d') then
c       Different parameters
	      write(6,'("Please enter PSF parameters.",
     1           " The current PSF parameters are  default.",/,
     2           "Input number of counts (cr for",e13.5,"):",$)')Counts
	      READ(LU(12),'(A)')input

	      if (input.ne.' ') then
		 read(input,*,end=1111)Counts
	      endif

	      call pixel_select(.true.,'PSF center          ')

	      if (signal.ne.' ') then
		 CALL ERROR(0,LOC)
		 goto 1
	      endif

	      write(6,'("PSF spectral index (cr for ",f4.2,"):",
     1                  $)')gamma
	      READ(LU(12),'(A)')input
	      if (input.ne.' ') then
		 read(input,*,end=1111)gamma
		 CALL PSMGET
		 CALL ERROR(1,LOC)
	      endif
	   endif		! end differnt parm section
	   
	   SRC_PARMS(nsrc,8)=1. ! active
	   call PSFADD(Counts,bmap,CTLMSZ1,CTLMSZ2)

	   SRC_PARMS(nsrc,1)=srcL
	   SRC_PARMS(nsrc,2)=srcB
	   SRC_PARMS(nsrc,3)=Counts
	   SRC_PARMS(nsrc,4)=gamma
	   SRC_NAMES(nsrc)=srcN
	   NSOURCE=NSOURCE+1
cj--->
cj---> Added LPxx array reset to zero state
c
	   sv_err68(NSOURCE)= 0
	   sv_err95(NSOURCE)= 0
	   sv_sigsv(NSOURCE)= ' '
	   best_choice(NSOURCE)='  '
	   best_x(NSOURCE) = 0
	   best_y(NSOURCE) = 0
	   sv_params(1,NSOURCE)=-1
	   sv_params(2,NSOURCE)=-1
	   sv_true_x(NSOURCE)=0
	   sv_true_y(NSOURCE)=0
	   sv_flx(1,NSOURCE) = 0
	   sv_flx(2,NSOURCE) = 0
	   sv_cnts(1,NSOURCE) = 0
	   sv_cnts(2,NSOURCE) = 0
	   sv_upperlim(NSOURCE) = 0
	   sv_expose(NSOURCE) = 0
	   sv_tmp_68_x(NSOURCE)=0
	   sv_tmp_68_y(NSOURCE)=0
	   sv_tmp_95_x(NSOURCE)=0
	   sv_tmp_95_y(NSOURCE)=0
	   svn_TS(NSOURCE)=0

	   do ll = 1,10
	      sv_dstsv(ll,nsrc)=.false.
	   enddo

	   exdir = SRC_NAMES(NSOURCE)

	   if (exdir.eq.' '.or.exdir.eq.' '.or.exdir(1:1).eq.'.'.or.
     1         exdir(1:3).eq.'GRO') goto 1

	   sv_dstsv(6,NSOURCE)=.true.
	   sv_true_x(NSOURCE) = SRC_PARMS(NSOURCE,1)
	   sv_true_y(NSOURCE) = SRC_PARMS(NSOURCE,2)
cj--->
cj---> End of changed code  /JAE
cj--->^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c  
	   LikTotaled=.false.
	   goto 1

	elseif (input(1:1).eq.'R'.or.input(1:1).eq.'r') then
c       Replace PSF
	   if (NSOURCE.eq.0) goto 3
	   read(input(2:5),*,end=2)Npsf
	   goto 4
 2	   write(6,'("Enter PSF to replace, (1 thru",I3,")",$)')NSOURCE
	   READ(LU(12),'(A)') input
	   read(input,*,end=1111)Npsf
 4	   write(lu(1),*) 'Replacing PSF number',Npsf
	   call psfreplace(Npsf)
	   call error(0,loc)
	   LikTotaled=.false.
cj--->
cj---> Added LPxx array reset to zero state
cj--->
	   sv_err68(Npsf)= 0
	   sv_err95(Npsf)= 0
	   best_choice(Npsf)='  '
	   sv_sigsv(Npsf)= ' '
	   best_x(Npsf) = 0
	   best_y(Npsf) = 0
	   sv_params(1,Npsf)=-1
	   sv_params(2,Npsf)=-1
	   sv_true_x(Npsf)=0
	   sv_true_y(Npsf)=0
	   sv_flx(1,Npsf) = 0
	   sv_flx(2,Npsf) = 0
	   sv_cnts(1,Npsf) = 0
	   sv_cnts(2,Npsf) = 0
	   sv_upperlim(Npsf) = 0
	   sv_expose(Npsf) = 0
	   sv_tmp_68_x(Npsf)=0
	   sv_tmp_68_y(Npsf)=0
	   sv_tmp_95_x(Npsf)=0
	   sv_tmp_95_y(Npsf)=0
	   svn_TS(Npsf)=0
	   LikTotaled=.false.

	   do ll = 1,10
	      sv_dstsv(ll,npsf)=.false.
	   enddo
cj--->
cj---> End of changed code  /JAE
cj--->^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c
	   goto 1

	elseif (input.eq.'T'.or.input.eq.'t') then
	   if (NSOURCE.eq.0) goto 3
	   print *,'You have asked to transfer the the other PSFs',
     1             ' to the diffuse model map - an irrevocable step.'
	   call intent(determined)

	   if (.not.determined) goto 100
	   write(lu(1),'(
     1           "Do you want to first set the diffuse",
     2           " model to null (cr for yes)? ",$)')

	   READ(LU(12),'(A)')input
	   if (input.eq.' ') then
	      call MAPRST(GASMAP,CTLMSZ1,CTLMSZ2)
	      write(lu(1),*)'Setting GASMAP to null'
	      GASDOC(1)='Diffuse  model set to null.'
	      do n=1,9
		 GASDOC(n)=' '
	      enddo
	   endif

	   write(lu(1),*)'Adding other PSF map to diffuse  model.'
	   write(lu(1),*)'Notice: Gmult applies to these counts now.'
	   CALL MAPSUM(gasmap,BMAP,CTLMSZ1,CTLMSZ2)
	   call error(1,loc)

c       SHOULD call DOCADD for PSF values
	   write(lu(1),'(
     1      "Do you want to now reset the other PSF map? (Y or N) ",$)')
	   READ(LU(12),'(A)')input
	   if (input.eq.'y'.or.input.eq.'Y') then
	      call MAPRST(BMAP,CTLMSZ1,CTLMSZ2)
	      write(lu(1),*) 'Setting other PSF map to null.'
cj--->
cj---> Added LPxx array reset to zero state
cj--->
	      do ll=1,NSOURCE
		 sv_true_x(ll) = 0
		 sv_true_y(ll) = 0
		 sv_err68(ll)= 0
		 sv_err95(ll)= 0
		 best_choice(ll)='  '
		 sv_sigsv(ll)= ' '
		 best_x(ll) = 0
		 best_y(ll) = 0
		 sv_params(1,ll)=-1
		 sv_params(2,ll)=-1
		 sv_flx(1,ll) = 0
		 sv_flx(2,ll) = 0
		 sv_cnts(1,ll) = 0
		 sv_cnts(2,ll) = 0
		 sv_upperlim(ll) = 0
		 sv_expose(ll) = 0
		 sv_tmp_68_y(ll)=0
		 sv_tmp_68_x(ll)=0
		 sv_tmp_95_x(ll)=0
		 sv_tmp_95_y(ll)=0
		 svn_TS(ll)=0
		 do kk=1,10
		    sv_dstsv(kk,ll)=.false.
		 enddo
	      enddo
cj--->
cj---> End of changed code  /JAE
cj--->^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c
	      NSOURCE=0
	   endif

	   LikTotaled=.false.
	   goto 1

	elseif (input(1:1).eq.'X'.or.input(1:1).eq.'x') then
c       Remove PSF
	   if (NSOURCE.eq.0) goto 3
	   read(input(2:5),*,end=12)Npsf
	   goto 14
 12	   write(6,
     1          '("Enter PSF to remove, (1 thru",I3,")",$)')NSOURCE
	   READ(LU(12),'(A)') input
	   read(input,*,end=1111)Npsf
 14	   call psfremove(Npsf)
	   LikTotaled=.false.
	   goto 1

	elseif (input(1:1).eq.'M'.or.input(1:1).eq.'m') then
c       Toggle activation of source
	   if (NSOURCE.eq.0) goto 3
	   if (input(2:2).eq.'A'.or.input(2:2).eq.'a') then
	      if (input(3:3).eq.'0') then
c       set all psf active Marks to inactive
		 do nsrc=1,NSOURCE
		    SRC_PARMS(nsrc,8)=0.
		 enddo

	      elseif (input(3:3).eq.'1'.or.input(3:3).eq.'F'.or.
     1                input(3:3).eq.'f') then
c       set all psf active Marks to active
		 do nsrc=1,NSOURCE
		    if (input(3:3).eq.'1') then
		       SRC_PARMS(nsrc,8)=1.
		    else
		       SRC_PARMS(nsrc,8)=0.75
		    endif
		 enddo

	      elseif (input(3:3).eq.'r'.or.input(3:3).eq.'R') then
cj---> Added command MAR to turn on sources within ROI and
cj---> turn off sources outside of ROI
		 do nsrc=1,NSOURCE
		    thelong=SRC_PARMS(nsrc,1)
		    thelat=SRC_PARMS(nsrc,2)

		    if (thelong.lt.ROIORG(1)-CTLSCL/2..or.
     1                  thelong.gt.ROIEND(1)+CTLSCL/2..or.
     2                  thelat.lt.ROIORG(2)-CTLSCL/2..or.
     3                  thelat.gt.ROIEND(2)+CTLSCL/2.) then
		       SRC_PARMS(nsrc,8)=0.
		    else
		       SRC_PARMS(nsrc,8)=1.
		    endif
		 enddo
	      endif
	      goto 1
	   endif

	   read(input(2:5),*,end=22)Npsf
	   goto 24
 22	   write(6, '(
     1        "Enter PSF for which to toggle activation, (1 thru",I3,")"
     2        ,$)')NSOURCE

	   READ(LU(12),'(A)') input
	   read(input,*,end=1111)Npsf
 24	   if (Npsf.gt.NSOURCE) then
	      write(6,*)'Sorry, this source has not been entered.'
	      goto 1
	   endif

	   if (SRC_PARMS(Npsf,8).gt.0.5) then
	      SRC_PARMS(Npsf,8)=0.
	      write(6,*)'De-activating source',Npsf
	   else
	      SRC_PARMS(Npsf,8)=1.
	      write(6,*)'Activating source',Npsf
	   endif

	   goto 1

	elseif (input.eq.'O'.or.input.eq.'o') then
	   if (NSOURCE.eq.0) goto 3
	   write(6,'("Enter disk file name for writing other PSF",
     1               " parameters:",$)')
	   READ(LU(12),'(A)') input
	   open(8,file=input)

	   if (coord_sys.eq.'C') then
	      coord_phr='RA     DEC'
           else
	      coord_phr=' L       B'
	   endif

	   write(8,'(
     1	     "Other PSF MAP PARAMETERS for ",I5,"< E <",I5)')
     2       CTLEMN,CTLEMX
	   write(8,'("      NAME                ",A,
     1       "         CNTS   Sp. I.   Active      TS      Intensity")')
     2       coord_phr
	   do nsrc=1,NSOURCE
	      theLong=SRC_PARMS(nsrc,1)

	      if (theLong.lt.0.) theLong=theLong+360.

	      write(8,'(
     1              6x,A18,2f8.3,f10.1,f6.2,f9.2$)')
     2              SRC_NAMES(nsrc),theLong,(SRC_PARMS(nsrc,ip),ip=2,4),
     3              SRC_PARMS(nsrc,8)

	      if (sv_dstsv(1,nsrc)) then
		 write(8,'(5x,F8.2,2x,E13.6)')svn_TS(nsrc),sv_flx(1,nsrc)
	      else
		 write(8,*)' '
	      endif
	   enddo
	   close(8)
	   goto 100

	elseif (input.eq.'I'.or.input.eq.'i') then
	   write(6,*)'Enter name of disk file for reading other PSF',
     1               ' parameters (cr for standard).'
	   READ(LU(12),'(A)') input
	   IF (input.eq.' ') THEN
	      in_dex = index(misc_dir, ' ') - 1

	      if (in_dex.lt.2) then
		 print *,'environmental variable not set' 
		 goto 100
	      else
		 input=misc_dir(1:in_dex)//'/psf_'
	      endif

	      if (coord_sys.eq.'C') then
		 input=input(1:in_dex+5)//'celestial'
	      else
		 input=input(1:in_dex+5)//'galactic'
	      endif

	      write(6,*)'Reading ',input(1:in_dex+14)
	   endif

	   INQUIRE (FILE=input,EXIST=FEXIST)
	   IF (FEXIST) THEN
	      open(8,file=input)
           ELSE
	      WRITE(lu(1),*)'File does not exist.'
	      goto 100
	   ENDIF

	   NSOURCE=0
	   convert=.false.
	   read(8,'(A)')input	!read header
	   read(8,'(A)')input
c
cj--> Changed next two if-then-endif blocks so that format must be
cj--> established on line 2 of the input PSF file before checking
cj--> correctness of the coordinate system of the file. 07-APR-98
cj-->							JAE
c
cj--> If file format on line 2 is not valid goto 100 submenu.
c
	   if (index(input,' L ').eq.0.and.index(input,' RA ').eq.0)
     1        then
	      write(6,'("Coordinate system of PSF file cannot",
     1                  " be determined")')
	      write(6,'("PSF file will NOT be read.  Please ",
     1                  "check your file format on line 2.")')
	      write(6,*)' '
	      goto 100
	   endif
c
cj--> If either statement is true then file is in opposite coordinate
cj--> system: set convert flag for either Cel->Gal or Gal->Cel 
cj--> conversion of the input test sources in the PSF file.  Inform
cj--> user of the conversion.
c
	   if ((coord_sys.eq.'C'.and.index(input,' RA ').eq.0).or.
     1         (coord_sys.eq.'G'.and.index(input,' L ').eq.0)) then
	      write(6,*)
     1              'This PSF list is in the wrong coordinate system.'
	      write(6,*)
     1              'The PSF positions will be converted.'
	      convert=.true.
	   endif

	   do nsrc=1,500
	      NSOURCE=NSOURCE+1
	      read(8,'(A)',end=5)input
	      read(input(7:25),'(A)',end=15,err=15)SRC_NAMES(NSOURCE)
	      read(input(26:80),*,end=15,err=15)
     1            (SRC_PARMS(NSOURCE,n),n=1,4), SRC_PARMS(NSOURCE,8)
cj--->
cj---> reset LPxx arrays and if source name indicates an ID then set flag in
cj---> sv_dstsv(6,NSOURCE) and position in sv_true_x(NSOURCE) and 
cj---> sv_true_y(NSOURCE) arrays.  ID is indicated if:
cj---> SRC_NAMES(NSOURCE) does NOT equal an element of { '.', '', ' ', 'GRO'}
cj--->
cj convert to oposite coordinate system if file coordinate system is
cj incorrect
c
	      if (convert) then
		 tmpx=SRC_PARMS(NSOURCE,1)
		 tmpy=SRC_PARMS(NSOURCE,2)
		 if (coord_sys.eq.'C') then
		    CALL CELGALD('GC',SRC_PARMS(NSOURCE,1),
     1                   SRC_PARMS(NSOURCE,2),tmpx,tmpy,n)
		 else
		    CALL CELGALD('CG',tmpx,tmpy,
     1                   SRC_PARMS(NSOURCE,1),SRC_PARMS(NSOURCE,2),n)
		 endif
	      endif

	      sv_err68(NSOURCE)= 0
	      sv_err95(NSOURCE)= 0
	      sv_sigsv(NSOURCE)= ' '
	      best_choice(NSOURCE)='  '
	      best_x(NSOURCE) = 0
	      best_y(NSOURCE) = 0
	      sv_params(1,NSOURCE)=-1
	      sv_params(2,NSOURCE)=-1
	      sv_flx(1,NSOURCE) = 0
	      sv_flx(2,NSOURCE) = 0
	      sv_cnts(1,NSOURCE) = 0
	      sv_cnts(2,NSOURCE) = 0
	      sv_upperlim(NSOURCE) = 0
	      sv_expose(NSOURCE) = 0
	      sv_tmp_68_x(NSOURCE)=0
	      sv_tmp_68_y(NSOURCE)=0
	      sv_tmp_95_x(NSOURCE)=0
	      sv_tmp_95_y(NSOURCE)=0
	      svn_TS(NSOURCE)=0

	      do ll = 1,10
		 sv_dstsv(ll,NSOURCE)=.false.
	      enddo	

c       check longitude

	      call L_CHECK(SRC_PARMS(NSOURCE,1))
	      if (signal.ne.' ') then

c       L is off the map
		 NSOURCE=NSOURCE-1
		 SIGNAL=' '
		 goto 16
	      else

c     	longitude OK, check latitude

		 call mapixl(SRC_PARMS(NSOURCE,1),SRC_PARMS(NSOURCE,2),I,J)
		 if (SIGNAL.ne.' ') then

c       not on map
		    NSOURCE=NSOURCE-1
		    SIGNAL=' '
		    goto 16
		 else

c       latitude OK, check exposure

		    if (mapval(emap,I,J,CTLMSZ1,CTLMSZ2).lt.1.0) then

c	no exposure here
		       NSOURCE=NSOURCE-1
		       goto 16
		    endif

C	EXPOSURE OK check distance from pointing

		    if (coord_sys.eq.'C') then
		       pL=SC_RAJ
		       pB=SC_DECJ

		    else
		       pL=SC_LJJ
		       pB=SC_BJJ
		    endif

		    srcL=SRC_PARMS(NSOURCE,1)
		    srcB=SRC_PARMS(NSOURCE,2)
		    aspect=gtcirc(pL,pB,srcL,srcB)

		    if (aspect.gt.aspj) then

c	Outside of requested aspect cone
		       NSOURCE=NSOURCE-1
		       goto 16
		    endif
		 endif
	      endif

	      goto 16
 15	      write(6,'("Unparsed PSF line:",a)') input(1:60)
	      NSOURCE=NSOURCE-1
	      goto 17
 16	      continue 
 17	   enddo

 5	   NSOURCE=NSOURCE-1
	   close(8)
	   call MAPRST(bmap,CTLMSZ1,CTLMSZ2)
	   call psfbld
	   LikTotaled=.false.
	   goto 1

	elseif (input.eq.'Q'.or.input.eq.'q'.or.input.eq.' ') then
	   return

	else
	   write(6,*)'Unsupported option, try again.'
	   goto 100
        endif

	RETURN

 3	write(6,*)'Sorry, no sources have been entered.'
	goto 100
 1111	write(6,*)'Invalid input, try again.'

	RETURN
	END
c
