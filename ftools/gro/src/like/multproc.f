C	SUBROUTINE MULTPROC(FLAG)
C
C
C  $Id: multproc.f,v 1.3 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++     effect: The number of Counts for each source in the OTHER PSF list 
C++	      is iteratively determined until convergence is reached.
C++	      The generation of a catalog of detected sources may then be
C++           requested (with or without position analysis).	
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C=======================================================================
C  $Log: multproc.f,v $
C  Revision 1.3  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2005/08/26 19:36:35  irby
C  Purely cosmetic changes to allow compilation with gfortran/g95, mostly
C  involving fixes to lines longer than 72 chars that were being truncated,
C  but in the case of the CGRO code, also moving misplaced (tabbed) line
C  continuation characters to their appropriate position in column 6.
C
C  Revision 1.1  2002/04/16 20:27:40  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/23  15:31:44  jae
c Added call to auto_out(1) to enable autooutput
c of PSF list to LM format file if enabled.
c
c Revision 5.1  1996/02/29  20:52:13  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:29  jae
c Subroutine Module for like V5.00
c
C%   Changes:
c	jrm 1/7/93 assume most likely counts if no convergence
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	SUBROUTINE MULTPROC(FLAG)

C	Common blocks used:
	INCLUDE  '../COMMON/likrep.copy'
	INCLUDE  '../COMMON/psmrep.copy'
	INCLUDE  '../COMMON/cnfrep.copy'
	INCLUDE  '../COMMON/bmprep.copy'
        INCLUDE  '../COMMON/errrep.copy'
	INCLUDE  '../COMMON/fitrep.copy'
	INCLUDE  '../COMMON/locpos.copy'

        save

	character(80) id
	common /id/id
        CHARACTER input*60,break*80,change*45,coord_phr*10
	logical determined,sv_Restrict_Gmult,FLAG,sv_spectral
	data break/
     &'--------------------------------------------------------------'/

	id = '$Id: multproc.f,v 1.3 2013/05/21 19:08:26 irby Exp $'
        LOC='MULTPROC'


	NSOURCE_active=0
	sv_Restrict_Gmult=Restrict_Gmult

	do nsrc=1,NSOURCE
	   if (SRC_PARMS(nsrc,8).gt.0.5) then
	      if(NSOURCE_active.eq.0)Nfirst_active=nsrc
	      NSOURCE_active=NSOURCE_active+1
	   endif
	enddo

	sv_spectral = spectral
	spectral = .false.
	write(6,'(
     1    "The current OTHER PSF map contains:")')
	write(6,'("  #   NAME                  POSITION    ",
     1    "   CNTS   Sp. I.   Active")')

	do nsrc=1,NSOURCE
	   theLong=SRC_PARMS(nsrc,1)

	   if (theLong.lt.0.) theLong=theLong+360.
	   write(6,'(
     1	i3,3x,A18,2f7.2,f10.1,f6.2,f9.2)')
     2	nsrc,SRC_NAMES(nsrc),theLong,
     3	(SRC_PARMS(nsrc,ip),ip=2,4),SRC_PARMS(nsrc,8)
	enddo

	if (.not.flag) then
	   if (NSOURCE_active.lt.1) then
	      write(*,*)' '
	      write(*,*)'There are no active sources'
	      write(*,*)' '
	      goto 1112
	   endif

	   write(*,*)' '
	   goto 2
	endif

	write(6,'("Requested to simultaneously solve for the",
     1    i3," which are active.")') NSOURCE_active
	
	if (NSOURCE_active.lt.1) then
	   write(*,*)' '
	   write(*,*)'There are no active sources'
	   write(*,*)' '
	   goto 1112
	endif

c       write(6,'("OK? (cr for OK)",$)')
	call intent(determined)
	if (.not.determined) goto 1112
	
	if (conv_crit.lt.0.0001) conv_crit=0.05

	write(6,'("Enter convergence criteria (in sigma, cr for",
     1    f5.2,")",$)') conv_crit

	read(LU(12),'(A)')input
	if (input.ne.' ') read(input,*,err=1111)conv_crit

	n_iter_max=25
	write(6,'("Enter number of iterations (cr for",
     1    I5,")",$)') n_iter_max

	read(LU(12),'(A)')input
	if (input.ne.' ') then
	   read(input,*,err=1111)n_iter_max
	endif
	
	calc_uncert=.true.	!required for sigma calc
	report=.true.
	
	do iterate=1,n_iter_max
	   WRITE(6,*)break
	   big_del=0.
	   jk=0
	   do nsrc=1,NSOURCE
	      srcN(1:18)=SRC_NAMES(nsrc)

	      if (SRC_PARMS(nsrc,8).gt.0.5) then
c       analyze this source
		 jk=jk+1
		 srcL=SRC_PARMS(nsrc,1)
		 srcB=SRC_PARMS(nsrc,2)
		 Counts=0
		 call PSFREPLACE(nsrc)

		 if (signal.ne.' ') then
		    call error(0,loc)
		    goto 14
		 endif

		 Counts_last=Counts
		 theLong=srcL

		 if (theLong.lt.0.) theLong=theLong+360.

		 WRITE(6,*)break
		 WRITE(6,*)break
		 WRITE(LU(1),'( i3," Analyzing: ",A18," at:",2f7.2,
     1            " Active source: ",i3," of ",i3," Iteration: ",i3)')
     2            nsrc,SRC_NAMES(nsrc),theLong,srcB,jk,NSOURCE_active,
     3            iterate

		 if (gmap_null) then
		    Restrict_Gmult=.true.
		    CALL SRCTEST(.false.)
		    Restrict_Gmult=sv_Restrict_Gmult
		 else
		    CALL SRCTEST(.false.)
		 endif

		 jj=nsrc

		 if (signal.ne.' ') goto 14

		 sv_flx(1,jj)=1.e8*Counts/pss_expose
		 sv_flx(2,jj)=1.e8*dCounts/pss_expose
		 sv_cnts(1,jj)=Counts
		 sv_cnts(2,jj)=dCounts
		 sv_expose(jj)=pss_expose
		 svn_TS(jj) = TS
		 sv_params(1,jj)=Gmult
		 sv_params(2,jj)=Gbias
		 sv_upperlim(jj)=1.e8*Counts_limit/pss_expose

		 if (TS.ge.25.) then
		    sv_dstsv(3,jj)=JTRUE
		 else
		    sv_dstsv(3,jj)=JFALSE
		 endif

		 sv_dstsv(1,jj)=.true.
		 SRC_PARMS(jj,5)=1
		 sv_sigsv(jj) = signal

 14		 if (signal.ne.' ') then
		    call error(0,loc)
		    Counts=0.
		    sv_flx(1,jj)=0
		    sv_flx(2,jj)=0
		    sv_cnts(1,jj)=0
		    sv_cnts(2,jj)=0
		    sv_expose(jj)=0
		    svn_TS(jj) = 0
		    sv_params(1,jj)=0
		    sv_params(2,jj)=0
		    sv_upperlim(jj)=0
		    sv_dstsv(3,jj)=JFALSE
		    sv_dstsv(1,jj)=.false.
		    SRC_PARMS(jj,5)=-1
		    goto 15
		 endif

		 delta=abs((Counts-Counts_last)/dCounts)

		 if (delta.gt.big_del) then
		    big_del=delta
		    write(change,'(A18," at",2f9.2)') srcN,theLong,srcB
		 endif

		 call PSFREPLACE(nsrc)
		 call error(0,loc)
	      endif
 15	      continue
	   enddo
	   
	   write(6,'("Iteration",I3,
     1          "; Biggest change (",f8.2," sigma) was in ",A40)')
     2          iterate,big_del,change

	   if (autooutput) call auto_out(1)
	   if (big_del.lt.conv_crit) then
	      WRITE(6,*)break
	      write(6,*)'Finally, convergence!'
	      goto 101
	   endif
	enddo

	WRITE(6,*)break
	write(6,*)'Did not converge!!!'
	goto 1112
 101	continue
	spectral = sv_spectral
	write(6,'(
     1    "Do you want to do further analysis (cr for yes)?",$)')
	read(LU(12),'(a)') input
	numcar = index(input, ' ') - 1

	if (numcar.ne.0.and.input.ne.'y'.and.input.ne.'Y') then
	   if(spectral)goto 202
	   return
	endif

 2	if (publish) then
	   write(6,'(
     1	         "Disk file of output for 2nd catalog?",
     2	         " (cr for LM-catalog):)",$)')
	   READ(LU(12),'(a)') input
	   numcar = index(input, ' ') - 1

	   if (numcar.eq.0) then
	      input='LM-catalog-out'
	      numcar = index(input, ' ') - 1
	   endif

	   open(73,file=input(1:numcar))
	   write(*,'("FILE:",A,
     1      " contains publish catalog format output")') input(1:numcar)
	endif

	if (catalog.or..not.publish) then
	   write(6,'(
     1	       "Disk file of final analysis output ?",
     2	       " (cr for LM-catalog-out):)",$)')
	   READ(LU(12),'(a)') input
	   numcar = index(input, ' ') - 1

	   if (numcar.eq.0) then
	      input='LM-analysis-out'
	      numcar = index(input, ' ') - 1
	   endif

	   open(18,file=input(1:numcar))
	   write(*,'("FILE: ",A," contains final analysis output")')
     1          input(1:numcar)
	endif

	call CATPROC(.true.,catalog)
c       
        if (coord_sys.eq.'C') then
	   coord_phr='RA     DEC'
        else
	   coord_phr=' L       B'
        endif

	WRITE(6,*)break
	write(6,*)'Final simultaneous result:'
        WRITE(6,'("  Name               ",A,"    sqrt(TS)",
     1       "  Flux+/- 1 sigma    (U.L.)",
     2       "       Cnts   +/- 1 sigma  (U.L.)",
     3       "    Gmult    Gbias  Ranal  Asp. EXP        lnL")')
     4       coord_phr
c       
	if (publish) then
	  WRITE(73,*)break
	  write(73,'("Name             RA      Dec       l      b    ",
     1	  "A    B   Phi  Mor     F     DF     g  Dg   Counts sqrtTS   V"
     2	  ,"P   ID   Other names               Note  Ref")')
	  write(73,*)' '
	endif
c       
 202	do nsrc=1,NSOURCE
	   if (SRC_PARMS(nsrc,8).gt.0.5) then
c       analyze this source
	      cnts_sv=SRC_PARMS(nsrc,3)
	      srcL=SRC_PARMS(nsrc,1)
	      srcB=SRC_PARMS(nsrc,2)
	      Counts=0.
	      jpos(NSOURCE+1)=nsrc
	      call PSFREPLACE(nsrc)

	      if (signal.ne.' ') then
		 call error(0,loc)
		 Counts=0.
		 SRC_PARMS(nsrc,3)=0
		 write(*,*)'Rebuilding point source PSF map'
		 CALL PSFBLD
	      else
		 call CATPROC(.false.,catalog)
		 if (signal.ne.' ') then
		    call error(0,loc)
		    Counts=cnts_sv
		 endif
	      endif

	      call PSFREPLACE(nsrc)

	      if (signal.ne.' ') then
		 call error(0,loc)
		 SRC_PARMS(nsrc,3)=cnts_sv
		 write(*,*)'Rebuilding point source PSF map'
		 CALL PSFBLD
	      endif
	   endif
	enddo
	
	if (publish) close(73)
	close(18)
	RETURN

 1111	write(lu(1),*)'Invalid input, try again.'
 1112	spectral = sv_spectral

	RETURN
	END
