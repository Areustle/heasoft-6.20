C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      SUBROUTINE EGRPSF
C
C
C  $Id: egrpsf.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C*    EGRPSF = PROC(IEL,IEH,GAMMA,SARFIL,EDPFIL,PSDFIL) RETURNS (PSFARR)
C*    Effect:  derives an averaged PSF for the energy range
C*             indicated by IEL,IEH and a gamma-ray spectrum
C*             power law with exponent GAMMA. The input gamma-
C*             ray spectrum and the energy dependance of the
C*             sensitive area and the Energy Dispersion
C*             is used for weighting in accordance
C*             with the counts distribution expected.
C*             The averaging conditions for inclination, azimuth
C*             are coded in this routine.
C=======================================================================
C LIKE Version: 4.8 DELIVERED: November 8th 1993, Programmer J.R. MATTOX
C=======================================================================
C
C%  CHANGES:
c
c	JAE 18-01-95 	changed PHIV(3) to PHIV(16)
C  $Log: egrpsf.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/12/26 17:21:30  irby
C  Fix open statement for f90 compatibility.
C
C  Revision 1.1  2002/04/16 20:27:29  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.4  1999/02/23  22:30:56  dlb
c corrected an oversight in earlier mod to remove the spectral break feature.
c
c Revision 5.3  1999/02/22  20:33:02  jae
c Fixed typo error: comment symbol in collumn 2
c
c Revision 5.2  1999/02/22  19:41:34  dlb
c Revision to correct the energy dispersion integration per JRM.  DLB.
c
c Revision 5.1  1996/02/29  20:47:25  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:44  jae
c Subroutine Module for like V5.00
c
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

C  NOTE.  The changes to the code made to accommodate the correct energy
*         dispersion were patterned after JRM's code which contains
*         the option for a spectral break.  This feature is not implemented
*         yet in the version of LIKE used at GSFC.  Rather than 
*         removing that code here, GAMMA1 is set to GAMMA, and the second
*         index, GAMMA2, is set to 0 in the initial statements of the
*         routine.
************************************************************************

      SUBROUTINE EGRPSF

C     Common blocks included
      include '../COMMON/ctlrep.copy' ! Added 1/21/99.  DLB
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save

      character(80) id
      common /id/id

c     INCLUDE (CALREP)
C     Local variables:

C     Axis definitions for Calibration files:
      INTEGER  INCLAX(9)
      logical jjflag,jjtest(4)
      real ect(10),zct(4)
      REAL     AZIMAX(3),ENERAX(21),SPECWT(21) ! DLB
      REAL     EDP(102,20),PSF(102,20),RINGSR(100),INTGRL(100)

      DATA     ENERAX/15,20,30,35,50,60,70,100,150,200,300,500,700,1000,
     1     2000,3000,4000,6000,7000,10000,14286/
      
      DATA     INCLAX/0,5,10,15,20,25,30,35,40/
                                ! Removed 1/21/99
      DATA     AZIMAX/0,22.5,45/

      data	zct/0,10,20,30/

      data	ect/15,20,35,60,100,200,500,1000,3000,10000/

C     Intermediate storage, relative # of counts at energy:
      REAL     SARIR(20),EDPIR(2040),PSFIR(2040)
      REAL*8   DSUM,DAR(101)

C     length of transfered data string in READ
C     vectors with angles selected for averaging:
      INTEGER  THETAV(9),PHIV(16)
      EQUIVALENCE (EDPIR,EDP),(PSFIR,PSF)
C     
      id = '$Id: egrpsf.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      LOC='EGRPSF'


      if (.not.egret_psf) return
C
      GAMMA1 = GAMMA            ! These two statements would be removed if
      GAMMA2 = 0.0              ! spectral break feature were implemented.
                                ! They would be set elsewhere.
      
      PI=PI180*180
      Fctlemx = float( ctlemx ) !  Added 1/21/99  DLB
      Fctlemn = float( ctlemn ) !    "      "      "
      jjtest(4)=.true.
C
      IF (IEL.LT.1.OR.IEH.GT.20.OR.IEL.GT.IEH) THEN
         SIGNAL='D'
         WRITE(SIGMSG,'(''Energy range indices not acceptable: '',2I5)')
     &        IEL,IEH
         RETURN
      ENDIF

C     conversion factors from steradian to probab/channel
c
      if (JAE_SR_TYPE.eq.1)then
         RIF=2*PI*PI180*PSFINC
         RINGSR(1)=((PSFINC*PI180)**2.)*PI
         DO IR=2,100
            RRAD=PSFINC*(IR-0.5)*PI180
            RINGSR(IR)=RIF*SIN(RRAD)
         ENDDO

c     JAE method (JAE_SR_TYPE = 1)
      ELSEIF (JAE_SR_TYPE.eq.0) then
         SROLD=0
         DO IR=1,100
            SRNEW=(1-COS(PSFINC*(IR)*PI180))*2*PI
            RINGSR(IR)=SRNEW-SROLD
            SROLD=SRNEW
         ENDDO
      ENDIF
C
C  ---------------  Replaced   (1/21/99  DLB)  ------------------
C     calculate relative weight depending on # of gammas incident

C     DO K=1,19
C     IF (GAMMA.LT.1.01.or.IEL.eq.20) THEN
C     SPECWT(K)=1
C     ELSE
C     SPECWT(K)=1./ENERAX(K)**(GAMMA-1.)-1./ENERAX(K+1)**(GAMMA-1.)
C     ENDIF
C     ENDDO

C  --------------- With    --------------------------------------

C     Calculate relative weight for each true energy for spectrum

      IF (GAMMA1.LT.1.1.or.(GAMMA2.LT.1.1.and.GAMMA2.gt.0.)) THEN
         SIGNAL='G'
         WRITE(SIGMSG,'(''Not programmed for  gamma < 1.'')') 
         print *,SIGMSG
         print *,'PSF not changed.'
         return
      ENDIF
 
      DO K=2,20                 !Loop over energy
         e_min=sqrt(ENERAX(K)*ENERAX(K-1))
         e_max=sqrt(ENERAX(K)*ENERAX(K+1))

         if (GAMMA2.lt.0.1) then
c     a single power law
            SPECWT(K)= e_min**(1.-GAMMA1) - e_max**(1.-GAMMA1)

         else
c     a broken power law
            if (ENERAX(K).lt.BENERGY) then
c     below break energy
               SPECWT(K)= e_min**(1.-GAMMA1) - e_max**(1.-GAMMA1)

            else
c     above break energy
               SPECWT(K)=((GAMMA1-1)/(GAMMA2-1))*
     &              BENERGY**(GAMMA2-GAMMA1)*
     &              (e_min**(1.-GAMMA2) - e_max**(1.-GAMMA2))
            endif
         endif
      ENDDO

C  --------------- End of Replace  ------------------------------

C     Fill vector with inclination angles selected for averaging
      I=0

      DO k=1,9
         IF (THETA(K).NE.0) THEN
            I=I+1
            THETAV(I)=k
         ENDIF
      ENDDO

      NTHETA=I

C     Fill vector with azimuth angles selected for averaging
      I=0
      DO k=1,JAE_2ND_CALIB
         IF (PHI(K).NE.0) THEN
            I=I+1
            PHIV(I)=K
         ENDIF
      ENDDO
      NPHI=I

C ................
C     this section performs the summations over the measured energies
C     corresponding to the selected energy interval and the summation
C     over all possible true energies contributing through the
C     energy dispersion to the selected energy interval
C.................

      DO k=1,101
         DAR(K)=0.D0
      ENDDO
      
C     NPASS=0          !  Removed  1/21/99  DLB
      
      if (JOUT_P_TYPE.ge.1.and.JOUT_P_TYPE.le.3)
     &     open(unit=73,file=LMAPFILE)

      ccphi=22.5
      ccthet=5.0

C     averaging over selected Azimuths
      DO J=1,NPHI
         JJ=PHIV(J)

C     averaging over selected inclination angles
         DO I=1,NTHETA
            II=THETAV(I)

C     get sensitive area and PSF and EDP for all true energies
C     section is for standard dir. access Fortran READ
C     NRRECS= II+9*((JJ-1)+3*(ITRMOD-1))
            NRRECS= II+9*((JJ-1)+JAE_2ND_CALIB*(ITRMOD-1))
            READ(LU(21),REC=NRRECS,IOSTAT=IOS) SARIR
c     call reflect( SARIR,4,80 )
            if (JOUT_P_TYPE.eq.3) then
               xxphi=(JJ-1)*ccphi
               xxthet=(II-1)*ccthet
               if (xxphi.gt.5.and.xxthet.lt.2.and..not.jjtest(4))
     &              goto 15
               if (xxphi.gt.5.and.xxthet.lt.2.) jjtest(4)=.false.
               do ir=1,20
                  xxener=ENERAX(ir)
                  write(73,*)xxphi,xxthet,xxener,SARIR(ir)
               enddo
            endif

 15         do IK=1,20
               NRREC = ik+20*((II-1)+9*((JJ-1)+ 
     &              JAE_2ND_CALIB*(ITRMOD-1)))
               READ(LU(22),REC=NRREC,IOSTAT=IOS) (EDP(ir,IK),ir=1,102)
c     call reflect( EDP(1,ik),4,408 )
               READ(LU(23),REC=NRREC,IOSTAT=IOS) (PSF(ir,IK),ir=1,102)
c     call reflect( PSF(1,ik),4,408 )
               if (JOUT_P_TYPE.eq.1.or.JOUT_P_TYPE.eq.2) then
                  jjflag=.true.

                  do kl=1,3
                     jjtest(kl)=.false.
                  enddo

		  xxphi=(JJ-1)*ccphi
		  xxthet=(II-1)*ccthet
		  xxener=ENERAX(IK)
		  if (xxphi.gt.5.and.xxthet.lt.2.and..not.jjtest(4))
     &                 goto 21

                  if(xxthet.lt.2.and.xxphi.gt.5)jjtest(4) = .false.

                  do kl=1,10
                     if(abs(xxener-ect(kl)).lt.1)jjtest(1)=.true.
                  enddo

                  do kl=1,4
                     if(abs(xxthet-zct(kl)).lt.1)jjtest(2)=.true.
                  enddo

                  jjflag=jjflag.and.jjtest(1).and.jjtest(2)

                  if (.not.jjflag) goto 21

                  xxtest=abs(xxthet-10)
                  yytest=abs(xxthet-30)
                  zztest=abs(xxthet-40)
                  eetest=abs(xxener-15)

                  if (eetest.lt.1.and.xxtest.lt.1) goto 21
                  if (eetest.lt.1.and.yytest.lt.1) goto 21
                  if (eetest.lt.1.and.zztest.lt.1) goto 21

                  xxtest=abs(xxthet-20)

                  if (eetest.lt.1.and.xxtest.lt.1.and.xxphi.gt.5)
     &                 goto 21

                  eetest=abs(xxener-20)
                  xxtest=abs(xxphi/22.5)+0.1
                  IITST=(xxtest)
                  IITST2=mod(IITST,2)

                  if (eetest.lt.1.and.IITST2.eq.0.and.
     &                 xxthet.gt.29)goto 21

                  eetest=abs(xxener-10000)
                  xxtest=abs(xxphi-22.5)+0.1
                  IITST=(xxtest)
                  IITST2=mod(IITST,2)

                  if (eetest.lt.1.and.IITST2.eq.1.and.
     &                 xxthet.gt.39) goto 21

		  do ir=1,100
                     yythet=0.2*float(ir-1)+0.1
                     if (JOUT_P_TYPE.eq.1) then
                        write(73,*)xxphi,xxthet,xxener,yythet,
     &                       PSF(ir+1,IK)
                     elseif (JOUT_P_TYPE.eq.2) then
                        write(73,*)xxphi,xxthet,xxener,yythet,
     &                       EDP(ir+1,IK)
                     endif
	          enddo         ! End ir loop
               endif
 21         enddo               ! End of ik loop

C     End of data retrieval section
            
C    --------------------  Replaced   1/21/99  DLB   ----------------
     
C     Loop over true energies
            DO LT=2,20          ! 20 MeV - 10 GeV
               EDPPRB=0
C     Loop over measured energy range
               DO IM=2,101
                  Emeas=ENERAX(lt)*0.02*float(im-2)
                  if (Emeas.le.Fctlemx.and.Emeas.ge.Fctlemn) then
c     include this measured energy
                     EDPPRB=EDPPRB+EDP(IM,LT)
c     print *,lt,im,Emeas,EDP(IM,LT)
                  endif
               ENDDO            ! End IM loop
               
c     ACCUMULATE WEIGHTED PSF
               DO K=1,100
                  DAR(K)=DAR(K)
     &                 +PSF(K+1,LT)*SARIR(LT)*SPECWT(LT)*EDPPRB
               ENDDO
            ENDDO               ! End of LT loop
C     end of loop over  true energies
            
C     --------------------  End Replace   1/21/99  ------------------

C     end of angle loops
         ENDDO                  ! End of I loop
      ENDDO                     ! End of J loop


c     if(IEL.eq.IEH.or.IEL.eq.20)print *,'NPASS:',npass
      
c     DO I=1,3                        ! Removed loop 1/21/99 DLB
c     ELAPST(I)=ELAPST(I)/1.0E6
c     ENDDO
C     rescale the array content according to passes
c     DO K=1,101                      ! Removed loop 1/21/99  DLB
c     if(IEL.eq.IEH.or.IEL.eq.20)print *,(k-1)*0.2,' ',DAR2(k)
c     DAR2(K)=DAR2(K)/NPASS
c     ENDDO

      DSUM=0
      DO K=1,100
c     DSUM=DSUM+DAR2(K)*RINGSR(K)    ! Changed 1/22/99  DLB
         DSUM=DSUM+DAR(K)*RINGSR(K)
      ENDDO
C*****
C     WRITE(LU(9),'('' DAR2(101)    : '',D10.3)') DAR2(101)
C     DSUM=DSUM+DAR2(101) bin 101 contains counts at present, no use !
C     renormalisation including channel 101 not possible at present
C*****
C     Renormalisation using channels 1 -100
      testsum=0
c     print *,'DAR = ',dar
c     print *,'DSUM = ',dsum

      DO K=1,100
c     PSFARR(K)=DAR2(K)/DSUM   !  Changed 1/22/99  DLB
         PSFARR(K)=DAR(K)/DSUM
         testsum=testsum+PSFARR(K)
         if (jae_egrpsf) then
            if (IEL.eq.IEH.or.IEL.eq.20) print *,(k-1)*0.2,' ',PSFARR(k)
         endif
      ENDDO
      if(jae_egrpsf)print *,'Sum after normalization:',testsum


c     write(*,'(/5x,''Theta:''/(8x,10f6.1))') (0.2*ii-0.1,ii=1,100)
c     write(*,'(/5x,''Dsum = '',f10.5)') dsum
c     write(*,'(/5x,''ringsr:''/(8x,10f8.5))') ringsr
c     write(*,'(/5x,''DAR :''/(8x,10f8.3))') dar
c     write(*,'(/5x,''PSFARR:''/(8x,10f11.5))') psfarr

      DSUM=0
      DO k=1,100
         DSUM=DSUM+PSFARR(K)*RINGSR(K)
      ENDDO

      INTGRL(1)=PSFARR(1)*RINGSR(1)
      DO I=2,100
         INTGRL(I)=INTGRL(I-1)+PSFARR(I)*RINGSR(I)
      ENDDO
      
c     write(*,'(/5x,''Integral of PSF = '',f8.3)') dsum
c     write(*,'(/5x,''INTGRL:''/(8x,10f8.3))') intgrl

C
c     WRITE(LU(1),*) 'EGRET PSF creation completed'

      if (JOUT_P_TYPE.ge.1.and.JOUT_P_TYPE.le.3) then
         close(unit=73)
      endif

      JOUT_P_TYPE=0

      RETURN
      END
c
