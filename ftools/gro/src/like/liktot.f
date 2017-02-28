C        SUBROUTINE LIKTOT(map,bmap,gasmap,emap
C     &                   ,IMAPWIDTH,IMAPHEIGHT)
C
C
C  $Id: liktot.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++           effect: LIKTOT finds the totals within Ranal which are used
C++			in the likelihood calculation: sum of the diffuse 
C++			prediction (GAST), Counts
C++                     the exposure (EXPT), and the PSF (PSFT), 
C++                     and the counts in the BMPMAP which represents other
C++                     sources (OTHER_SRCT) for
C++                     a radius of Ranal around srcLpxcnt,srcBpxcnt
C++                     A table is prepared describing pixels within Ranal
C++			with a row for each column of pixels:
C++			column 1, longitude pixel number; 
C++			column 2, first latitude pixel number; 
C++			column 3, last latitude pixel number; 
C++			column 4, longitude pixel number of PSF.
C++			The corresponding parameters Lanal describe
C++			number of columns in the Ranal region; and
C++			IBoffset the number of pixels offset in lat. 
C++			between map & PSF. The LikTotaled flag indicates
C++			that totals are current.
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     REAL   map,bmap,gasmap,emap data maps 
c     INTEGER   IMAPWIDTH,IMAPHEIGHT working map size
c
C=======================================================================
c  LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated:    by  JRM
C=======================================================================
C  $Log: liktot.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:36  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:51:39  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:43  jae
c Subroutine Module for like V5.00
c
C%   Changes:                                                           
c	jrm 1/94 fix - if(abs(srcB)+Ranal.gt.65)...
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE LIKTOT(map,bmap,gasmap,emap
     &     ,IMAPWIDTH,IMAPHEIGHT)

C     Common blocks used:
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/likrep.copy'

      save

      character(80) id
      common /id/id
      REAL ANALORG(2)
C     coords of pixel at beginning of ANAL ROI
      REAL ANALEND(2)
C     coords of pixel at end of ANAL ROI
      INTEGER ANALPRG(2)
C     pixel indeces of ANALORG in MAP
      INTEGER ANALPND(2)
C     pixel indeces of ANALEND in MAP
      REAL MAP(IMAPWIDTH,IMAPHEIGHT)
      REAL BMAP(IMAPWIDTH,IMAPHEIGHT)
      REAL GASMAP(IMAPWIDTH,IMAPHEIGHT)
      REAL EMAP(IMAPWIDTH,IMAPHEIGHT)
      logical first,psfLok
      COMMON /total_old/ Ranal_old,srcLpxcnt_old,srcBpxcnt_old
      data Ranal_old /-100./,  LOC/'LIKTOT  '/

      id = '$Id: liktot.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      if(LikTotaled)return      !Already done

      if(abs(Ranal_old-Ranal).gt.0.001.or.
     &     abs(srcLpxcnt_old-srcLpxcnt).gt.0.001.or.
     &     abs(srcBpxcnt_old-srcBpxcnt).gt.0.001) then
c     new position or radius, setup ANALysis table for this pixel
         if(abs(srcB)+Ranal.gt.65)then !is 65 smaller than necessary?
            xL1=CTLORG(1)
            xL2=CTLEND(1)
	 else
            deltaL=Ranal/cos(srcB*PI180)
            xL1=srcL-deltaL
            xL2=srcL+deltaL
         endif
         
         B1=srcB-Ranal
         B2=srcB+Ranal
         call ROISET(xL1,B1,xL2,B2,ANALORG,ANALEND,ANALPRG,ANALPND)
c     CALL ERROR(0,LOC) !debug
	 if(SIGNAL.ne.' ')SIGNAL=' '
         Lanal=0
         
         jL=ANALPRG(1) 
c     begin longitude loop
 100     CONTINUE
         NB=0
         first=.true.
c     begin latitude loop
         DO 90 jB = ANALPRG(2),ANALPND(2)
            if(EMAP(jL,jB).lt.1.e-3) then
c     there is no exposure here
               if(.not.first) goto 91
            else
c     find pixel coordinates
               CALL MAPCOR (jL,jB,pL,pB)
               CALL ERROR(1,LOC) !debug
               if(GTCIRC(pL,pB,srcLpxcnt,srcBpxcnt).gt.Ranal) then
C     this pixel is outside of analysis circle
                  if(.not.first) goto 91
               else
C     this pixel is within analysis circle
                  NB=NB+1
                  if(first) then
                     jB_first=jB
c     calculate PSF B index offset
                     deltaB=pB-srcBpxcnt
                     IBoffset=PSFORG(2) + (deltaB/PSFSCL) -jB
                     first=.false.
                  endif
               endif
            endif
 90      continue
c     end latitude loop
         
 91      continue
         if(.not.first) then
c     write column data to table
            Lanal=Lanal+1
            if(Lanal.gt.720) then
               signal='S'
               sigmsg=
     &              'Cant support >720 longitude columns in ' //
     *              'analysis region.'
               return
            endif
            ANAL(1,Lanal)=jL
            ANAL(2,Lanal)=jB_first
            ANAL(3,Lanal)=jB_first+NB-1
c     calculate PSF L index for column
            deltaL=pL-srcLpxcnt
            if(deltaL.gt.180.)deltaL=deltaL-360.
            if(deltaL.lt.-180.)deltaL=deltaL+360.
            ANAL(4,Lanal)=PSFORG(1) + (deltaL/PSFSCL)
c     write(6,'("debug ",5i6)') lanal,(anal(n,lanal),n=1,4)
         endif
         
         CALL MAPINC(jL,IEND,ANALPRG(1),ANALPND(1)) ! DO ALL COLUMNS IN THE ROI
	 IF(SIGNAL.NE.' ') CALL ERROR(1,LOC)
         IF(IEND.NE.1) GOTO 100
         
         Ranal_old=Ranal
         srcLpxcnt_old=srcLpxcnt
         srcBpxcnt_old=srcBpxcnt

c     end if(new radius or position) code
      endif

c     Calculate totals
      GAST=0.
      EXPT=0.
      PSFT=0.
      OTHER_SRCT=0.
      CountT=0.
      
      do NL=1,Lanal
         if(ANAL(4,NL).GE.1.and.ANAL(4,NL).LE.PSFSZ) then
	    psfLok=.true.
         else
	    psfLok=.false.
         endif
         do jB=ANAL(2,NL),ANAL(3,NL)
	    GAST = GAST + GASMAP(ANAL(1,NL),jB)
	    EXPT = EXPT + EMAP(ANAL(1,NL),jB)
            OTHER_SRCT = OTHER_SRCT + BMAP(ANAL(1,NL),jB)
            CountT = CountT + MAP(ANAL(1,NL),jB)
	    if(psfLok) then
               jBPSF=jB+IBoffset
               if(jBPSF.GE.1.and.jBPSF.LE.PSFSZ) then
                  PSFT = PSFT + PSF(ANAL(4,NL),jBPSF)
c     WRITE(6,*)ANAL(4,NL),jBPSF,PSF(ANAL(4,NL),jBPSF)
               endif
	    endif
         enddo
      enddo
      
      if(verbose)then
         WRITE(6,*)'VERBOSE(LIKTOT)',
     &        'srcL,srcB,PSFT,GAST,EXPT,OTHER_SRCT,CountT:',
     &        srcL,srcB,PSFT,GAST,EXPT,OTHER_SRCT,CountT
      endif
      
      LikTotaled=.true.
      RETURN
      END
