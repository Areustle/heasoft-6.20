C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      SUBROUTINE SASPSF
C
C
C  $Id: saspsf.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C*   POINT SOURCE ANALYSIS SYSTEM (SOURCE)
C*   Spec:
C*     SASPSF= PROC(PCOEF,PWIDTH) RETURND(PSFARR)
C*              EFFECT: PSF array is filled with normalized sas2 PSF
C*                      derived from gauss function representation
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:    by  JRM
C=======================================================================
C  $Log: saspsf.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:43  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:53:20  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:17  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE SASPSF
C
C  Common blocks used:
C
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'

      save
C
      character(80) id
      REAL CBGAUS
      REAL*8 DSUM
      REAL RINGSR(100),INTGRL(100)
      DIMENSION PCOEFS(6,3),PWIDTH(6)

      common /id/id
        
C
C Fit to sas-2 point spread for two  energy ranges.                     
C
      DATA PCOEFS/-1.1,102.6,-21.5,132.8,0.,0.,                
     1     16.935,24.980,178.90,25.746,475.74,0.,                
     1     .00  ,.00  ,.00  ,.00  ,.00  ,.00/                
      
      DATA PWIDTH/16.,8.,4.,2.,1.,0.5/
        
      id = '$Id: saspsf.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      LOC='SASPSF'
C
C     check for valid energy range
C
      IF (CTLEMN.eq.35.and.CTLEMX.eq.100) THEN
         CTLIER=1
      ELSEIF (CTLEMN.eq.100.and.CTLEMX.eq.5000) THEN
         CTLIER=2
      ELSE
         SIGNAL='D'
         WRITE(SIGMSG,'("ENERGY RANGE",2f8.0," not supported.")')
     &        CTLEMN,CTLEMX
         RETURN
      ENDIF
C
C     calculate sr of rings corresponding to channels
C     
      PI=PI180*180
      RIF=2*PI*PI180*PSFINC
      DO I=1,100
         RRAD=PSFINC*(I-0.5)*PI180
         RINGSR(I)=RIF*SIN(RRAD)
      ENDDO
C     evaluate cos-b PSFARRF as sum of gaussians
      DSUM=0
      DO I=1,100
         R=PSFINC*(I-0.5)
         PSFARR(I) = PCOEFS(1,CTLIER)*CBGAUS(R,PWIDTH(1))+
     1        PCOEFS(2,CTLIER)*CBGAUS(R,PWIDTH(2))+
     1        PCOEFS(3,CTLIER)*CBGAUS(R,PWIDTH(3))+
     1        PCOEFS(4,CTLIER)*CBGAUS(R,PWIDTH(4))+
     1        PCOEFS(5,CTLIER)*CBGAUS(R,PWIDTH(5))+
     1        PCOEFS(6,CTLIER)*CBGAUS(R,PWIDTH(6))
         if (PSFARR(I).lt.0.) PSFARR(I)=0.
         DSUM=DSUM+PSFARR(I)*RINGSR(I)
      ENDDO

      PSFARR(1)=PSFARR(1)/DSUM
      INTGRL(1)=PSFARR(1)*RINGSR(1)
      DO I=2,100
         PSFARR(I)=PSFARR(I)/DSUM
         INTGRL(I)=0.
         INTGRL(I)=INTGRL(I-1)+PSFARR(I)*RINGSR(I)
      ENDDO
C
      WRITE(LU(9),'(''RENORMALIZED for channels 1 to 100:'')')
      WRITE(LU(9),'(''PSMCAL PSFARR 1-101 :'',19(/6E10.3))')
     &     (PSFARR(M),M=1,101)
C     
      WRITE(LU(9),*) 'Ring SR for PSF distribution:'
      WRITE(LU(9),'(6E10.3/)') (RINGSR(I),I=1,81,20),RINGSR(100)
      WRITE(LU(9),*) 'Differential COS-B PSF distribution:'
      WRITE(LU(9),'(6E10.3/)') (PSFARR(I),I=1,81,20),PSFARR(100)
      WRITE(LU(9),*) 'Integral COS-B PSF distribution:'
      WRITE(LU(9),'(6E10.3/)') (INTGRL(I),I=1,81,20),INTGRL(100)
C     
      RETURN
      END
