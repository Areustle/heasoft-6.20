c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE JLISTPROC
C
C
C  $Id: jlistpro.f,v 1.4 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C*     Effect: A source list of the form src.list is compared to sources
C*	       in the psfrep.copy and locpos.copy array by position.  
C*	       The user is prompted for action on possible source 
c*	       identification.
c
c----------------------------------------------------------------------
c     	Subroutine Argument Desriptions
c   	^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c	No arguments are passed
c
C*     Common Block Parameters Affected 
c      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
C*  If an identification is possible and the user selects a given ID
C*  then the parameters affected are:
C*
C* COMMON BLOCK	    parameter		contents
C* locpos.copy	    sv_dstsv(6,n)	logical: value True -> ID
C* locpos.copy	    sv_true_x(n)	real: Long(RA) of ID position
C* locpos.copy	    sv_true_y(n)	real: Lat(DEC) of ID position
C* psfrep.copy	    SRC_NAMES(n)	char*18: Name of ID source
c-----------------------------------------------------------------------
C MISC_DIR is now read from like.par.  Deleted call to getenv to get this
C variable - Sandhia Bansal 12/01
C LIKE Version: 5.0 DELIVERED: March 25th1994, Programmer J.A. ESPOSITO
C+             UPDATED:    by  JAE
c
C=======================================================================
C  $Log: jlistpro.f,v $
C  Revision 1.4  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.3  2002/12/26 17:42:54  irby
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
C  Revision 1.1  2002/04/16 20:27:34  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:48:22  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:22  jae
c Subroutine Module for like V5.00
c
C
c-----------------------------------------------------------------------

      SUBROUTINE JLISTPROC

C     Common blocks used:
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/fitrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save

      character(80) id
      common /id/id
c
      CHARACTER input*120,coord_phr*10,break*80
c     CHARACTER ex_dir*60, srcNhold*18,ss1*1
      CHARACTER srcNhold*18,ss1*1
      real aspectARR(20),x_srcL(20),x_srcB(20)
      character(18) x_srcN(20)
      LOGICAL FEXIST,in_OTHER,the_flag,jauto,jactive
      LOGICAL srclistflg
      LOC='JLISTPRO'
      data break/
     &'--------------------------------------------------------------'/
c
c
      id = '$Id: jlistpro.f,v 1.4 2013/05/21 19:08:25 irby Exp $'

      FEXIST=JFALSE
      srclistflg=JFALSE
c
      jauto=JFALSE
      jactive=JFALSE
      njpl=0
      do lol=1,18
         srcNhold(lol:lol)=' '
      enddo
c     
      if(coord_sys.eq.'C') then
         coord_phr='RA     DEC'
      else
         coord_phr=' L       B'
      endif
c
      write(6,*)
     &     'Will find possible identifications from a catalog.'
      write(6,'("Counts map: ",a)') cmapfile(1:50)
      write(6,'("Diffuse map: ",a)') gmapfile(1:50)
      WRITE(6,'
     &     ("Analysis parameters: gamma=",f4.1,";Ranal=",f4.1)')
     &     gamma,Ranal
      write(6,*)
     &     'These PSFs are in the OTHER PSF map:'
      if(NSOURCE.lt.1) then
         write(6,*)'No sources in other PSF map.'
         goto 1002
      else
         write(6,*)
     &        '  #   NAME                  POSITION    ',
     &        '   CNTS   Sp. I.    BEST POSITION   Activity'
c 
         do nsrc=1,NSOURCE
            if(sv_dstsv(2,nsrc))then
               xbest=best_x(nsrc)
               ybest=best_y(nsrc)
            else
               xbest=SRC_PARMS(nsrc,1)
               ybest=SRC_PARMS(nsrc,2)
               sv_err95(nsrc)=0
               sv_err68(nsrc)=0
            endif
            theLong=SRC_PARMS(nsrc,1)
            if(xbest.lt.0.)xbest=xbest+360.
            if(theLong.lt.0.)theLong=theLong+360.
            write(6,'(
     &           i3,3x,A18,2f7.2,f10.1,f6.2,5x,2f7.2,1x,f4.2)')
     &           nsrc,SRC_NAMES(nsrc),theLong,
     &           (SRC_PARMS(nsrc,ip),ip=2,4),
     *           xbest,ybest,SRC_PARMS(nsrc,8)
         enddo
      endif
c
      write(6,*)
     &     'Strong sources should be modeled.'
      write(6,'(
     &     "          Type C to continue, ",
     *     "cr to return to main menu:",$)')
      READ(5,'(a)') input
      numcar = index(input, ' ') - 1
      if(numcar.eq.0)goto 1002
      if(input.eq.' '.or.(input(1:1).ne.'c'.and.
     &     input(1:1).ne.'C'))goto 1002
c     
 1    continue
 1014 print *,' '
      njpl=4
      write(6,'(
     &     "     Enter Source number for Identification;",/,
     &     "     Enter C to check ALL PSFs;",/,
     &     "     Enter A to check ALL ACTIVE PSFs;",/,
     &     "     Enter <cr> to ABORT: "$)')
      READ(LU(12),'(a)') input
      numcar = index(input, ' ') - 1
      if(numcar.eq.0)goto 1002
      call TO_UPPER(input(1:1))
      if(input(1:1).eq.'C'.or.input(1:1).eq.'A')then
         nscrtmp=0
         if(input(1:1).eq.'C')jactive=JTRUE
      else
         read(input,*,err=1001,end=1001)nsrctmp
      endif
      if(nsrctmp.lt.0.or.nsrctmp.gt.NSOURCE)goto 1001
c     
      if(nsrctmp.eq.0)then
         NSOURCE1=1
         NSOURCE2=NSOURCE
         input='     '
         write(*,'("Do you wish AUTOMATIC ID of closest ",
     &        "source in list ?",/,
     *        "[<cr> for default: No] Enter N/y: "$)')
         read(LU(12),'(a)') input
         numcar = index(input, ' ') - 1
         call TO_UPPER(input(1:1))
         if(numcar.ne.0.and.input(1:1).eq.'Y')then
            jauto=JTRUE
         endif
      else
         NSOURCE1=nsrctmp
         NSOURCE2=nsrctmp
      endif
c
 1015 njpl=5
c     
      aspj3=5.00
      write(6,'(" Enter maximum degrees from source",/,
     &     " Enter as negative value only if this distance is secondary"
     &     ," to known PSF contours, Enter <A> to abort",
     &     " (<cr> for ",f4.0,"): "$)')aspj3
      read(5,'(A)')input
      if(input.ne.' ')read(input,*,err=1001,end=1001)aspj3
c
c
      in_OTHER=JFALSE
      if(aspj3.lt.0)then
         in_OTHER=JTRUE
         aspj3=-aspj3
      endif
c     
      write(6,'(
     &     "Disk file of input catalog? (cr for src.list)",$)')
      READ(LU(12),'(a)') input
      numcar = index(input, ' ') - 1
c     
      if(numcar.eq.0)then
c     CALL GETENV('MISC_DIR',ex_dir)
csb-12/01
c misc_dir is now read from like.par file    
c
         in_dex = index(misc_dir, ' ') - 1
         if(in_dex.lt.1)then
            print *,'Error in MISC_DIR name'
c     print *,'MISC_DIR environmental variable has not been',
c     & ' set or has a leading space character'
         else
	    srclistflg=JTRUE
            input=misc_dir(1:in_dex)//'/src.list'
         endif
      endif
c     
      INQUIRE (FILE=input,EXIST=FEXIST)
      IF(FEXIST) THEN
         in_dex = index(input, ' ') - 1
         open(8,file=input(1:in_dex),err=2007)
         goto 2008
 2007    write(*,'("Error opening file->",a)') input(1:in_dex)
         close(unit=8)
         goto 1002
 2008    continue
      ELSE
         WRITE(lu(1),*)'File does not exist.'
         in_dexj = index(input, ' ')
         write(lu(1),'(" FILENAME->",A)')input(1:in_dexj)
         write(*,*)
         goto 1002
      ENDIF
c     
      Inam1=1
      Inam2=18
      if(coord_sys.eq.'C') then
         Ilong1=85
         Ilong2=90
         Ilat1=94
         Ilat2=99
      else
         Ilong1=67
         Ilong2=72
         Ilat1=76
         Ilat2=81
      endif
c     
      if(input(in_dex+2:in_dex+9).eq.'src.list') then
         input=' '
      else
         write(6,'("Is this in src.list format (cr for yes)?:)",$)')
         READ(5,'(a)') input
         numcar = index(input, ' ') - 1
      endif
      njpl=0
c
 1010 if(numcar.eq.0)goto 1016
 1011 njpl=1
      write(6,'(
     &     "Character range for source name? (default is",2I3,"):",$)')
     &     Inam1,Inam2
      READ(5,'(A)') input
      if(input.ne.' ')read(input,*,err=1001,end=1001)Inam1,Inam2
      if(Inam2-Inam1+1.gt.18)then
         print *,' Too many characters in name (<=18)'
         goto 1001
      endif
c
c
 1012 njpl=2
      if(coord_sys.eq.'C') then
         write(6,'(
     &        "Character range for RA? (default is",2I4,"):",$)')
     &        Ilong1,Ilong2
      else
         write(6,'(
     &        "Character range for longitude? (default is",2I4,"):",$)')
     &        Ilong1,Ilong2
      endif
      READ(5,'(A)') input
      if(input.ne.' ')read(input,*,err=1001,end=1001)Ilong1,Ilong2
c     
c
 1013 njpl=3
      if(coord_sys.eq.'C') then
         write(6,'(
     &        "Character range for declination? ",
     *        "(default is",2I4,"):",$)')
     &        Ilat1,Ilat2
      else
         write(6,'(
     &        "Character range for latitude? (default is",2I4,"):",$)')
     &        Ilat1,Ilat2
      endif
      READ(5,'(A)') input
      if(input.ne.' ')read(input,*,err=1001,end=1001)Ilat1,Ilat2
c     
c     endif ! end cat parm adj exclude
      goto 1016
c
 1001 if(njpl.le.0.or.njpl.gt.5)then
         write(*,*)'Input error: Aborting'
         goto 1002
      endif
      write(6,*)'Invalid input, try again.'
      goto (1011,1012,1013,1014,1015)njpl
      
 1016 njpl=0
c     
      do nsrc=NSOURCE1,NSOURCE2
         do i=1,20
            aspectARR(i)=1000.
         enddo
c     
         if(jactive.and.SRC_PARMS(nsrc,8).lt.0.74)goto 2014 
         if(sv_dstsv(2,nsrc))then
            the_flag=.true.
         else
            the_flag=.false.
         endif
         if(the_flag)then
            xbest=best_x(nsrc)
            ybest=best_y(nsrc)
         else
            xbest=SRC_PARMS(nsrc,1)
            ybest=SRC_PARMS(nsrc,2)
         endif
         if(xbest.lt.0)xbest=xbest+360
c     
         NJC=0
 605     continue
         read(8,'(a)',end=7,err=7) input
         numcar = index(input, ' ') - 1
         if(numcar.lt.5)goto 605
         if(srclistflg)then
            if(input(1:30).eq.'Key to classification numbers ')
     &           goto 7
            srcN=input(1:14)
            if(coord_sys.eq.'C')then
               read(input(84:numcar),*,err=2)srcL,srcB
            else
               read(input(66:numcar),*,err=2)srcL,srcB
            endif
            goto 3
         else
            SIGMSG='READ(8) srcN ERROR'
c     
            read(input(Inam1:Inam2),'(A)',end=2,err=2)srcN
            SIGMSG='READ(8) srcL ERROR'
c     
            read(input(Ilong1:Ilong2),*,end=2,err=2)srcL
            SIGMSG='READ(8) srcB ERROR'
c     
            read(input(Ilat1:Ilat2),*,end=2,err=2)srcB
            goto 3
         endif

 2       continue
         if(verbose)then
            write(6,*)SIGMSG
            write(6,*)
     &           'Cannot parse catalog entry:'
            write(6,*)input
            goto 605
         endif
c     
 3       continue
c     
         aspect1=gtcirc(srcL,srcB,xbest,ybest)
         if(the_flag.and.in_OTHER)then
            if(aspect1.gt.sv_err95(nsrc))goto 605
         else
            if(aspect1.gt.aspj3)goto 605
         endif
c     
         if(aspect1.ge.aspectARR(20))goto 605
         NJC=NJC+1
         if(NJC.gt.20)NJC=20
         aspectARR(20)=aspect1
         x_srcL(20)=srcL
         x_srcB(20)=srcB
         x_srcN(20)=srcN
         if(NJC.ge.1)then 
            do jk=20,2,-1
               CALL REAL_COMP(aspectARR(jk-1),aspectARR(jk),iitst)
               if(iitst.eq.1)then
                  call swapreal(aspectARR(jk-1),aspectARR(jk))
                  call swapreal(x_srcL(jk-1),x_srcL(jk))
                  call swapreal(x_srcB(jk-1),x_srcB(jk))
                  call swapchar(x_srcN(jk-1),x_srcN(jk),18)
               else
                  goto 605
               endif
            enddo
         endif
         goto 605
c
 7       continue
c     
         write(6,'("  #   NAME                  POSITION    "$)')
         write(6,'("   CNTS       BEST POSITION    ID     68%  95%"$)')
         write(6,'("        TS")')
c     
         print *,' '
         theLong=SRC_PARMS(nsrc,1)
         if(theLong.lt.0.)theLong=theLong+360.
         write(6,'(
     &        i3,3x,A18,2f7.2,f10.2,5x,2f7.2$)')
     &        nsrc,SRC_NAMES(nsrc),theLong,
     &        (SRC_PARMS(nsrc,ip),ip=2,3),xbest,ybest
         if(sv_dstsv(6,nsrc))then
            ss1='T'
         else
            ss1='F'
         endif
         if(sv_dstsv(2,nsrc))then
            write(6,'("     ",A1,4x,f4.0,1x,f4.0,f11.1)')ss1,
     &           sv_err68(nsrc),sv_err95(nsrc),svn_TS(nsrc)
         else
            write(6,'("     ",A1,13x,f11.1)')ss1,svn_TS(nsrc)
         endif
c
         print *,' '
         print *,' '
         write(6,
     &        '(" #  NAME                  POSITION    ",
     *        "Distance(Arcmin)")')
c
         print *,' '
c     
         if(NJC.eq.0)goto 8
         do joj=1,NJC
            write(6,'(i2,2x,A18,2f7.2,8x,f5.0,6x)')
     &           joj,x_srcN(joj),x_srcL(joj),x_srcB(joj),
     *           60*aspectARR(joj)
         enddo
c     
 707     print *,' '
	 if(jauto)then
            ncho=1
            goto 708
	 endif
         
	 input='          '
         write(6,
     &        '(" Enter number for choice or <cr> to skip this ID: "$)')
         read(LU(12),'(a)') input
	 numjid = index(input, ' ') - 1
	 if(numjid.eq.0)goto 10
         if(input(1:1).lt.'0'.or.input(1:1).gt.'9')goto 9
         read(input,*,err=9,end=9)ncho
         if(ncho.gt.20)goto 9
         if(ncho.le.0)goto 9
c
 708     continue
         tmpx=x_srcL(ncho)
         tmpy=x_srcB(ncho)
         SRC_NAMES(nsrc)=x_srcN(ncho)
         sv_dstsv(6,nsrc)=.true.
         if(coord_sys.eq.'C')then
            Pra=tmpx
            Pdec=tmpy
            CALL CELGALD('CG',Pra,Pdec,tmpx,tmpy,jhj)
         endif
         sv_true_x(nsrc)=tmpx
         sv_true_y(nsrc)=tmpy
c     
         goto 10
 9       print *,' INPUT ERROR try again'
         goto 7
 8       if(NJC.eq.0)then
            write(6,*)' No Sources found '
            print *,' '
            print *,' '
         endif
 10      continue
c
	 rewind(8)
 2014 enddo
c
 1002 continue
      SIGMSG='                      '
c     
      return
      END
c
