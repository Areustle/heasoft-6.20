C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      SUBROUTINE COSPSF
C
C
C  $Id: cospsf.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C*   POINT SOURCE ANALYSIS SYSTEM (SOURCE)
C*   Spec:
C*     COSPSF= PROC(PCOEF,PWIDTH) RETURND(PSFARR)
C*              EFFECT: PSF array is filled with normalized COS-B PSF
C*                      derived from gauss function representation
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: 07 Jun 1991    by  HRM
C=======================================================================
C  $Log: cospsf.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:29  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:47:18  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:28  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE COSPSF

C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'

      save

C
      character(80) id
      common /id/id
      REAL CBGAUS
      REAL*8 DSUM
      REAL RINGSR(100),INTGRL(100)
      DIMENSION PCOEFS(6,3),PWIDTH(6)
C
C     Fit to COSB point spread for three energy ranges.
      DATA PCOEFS/.0225,.0421,.6853,0.0196,0.000,0.000,
     1     .0040,.0131,.7584,1.1123,0.0101,0.00,
     1     .0073,.0031,.3156,1.8327,4.0726,.4073/
C
      DATA PWIDTH/16.,8.,4.,2.,1.,0.5/
C
      id = '$Id: cospsf.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      LOC='COSPSF'
C
C     check for valid energy range
      IF (CTLEMN.eq.70.and.CTLEMX.eq.150) THEN
         CTLIER=1
      ELSEIF (CTLEMN.eq.150.and.CTLEMX.eq.300) THEN
         CTLIER=2
      ELSEIF (CTLEMN.eq.300.and.CTLEMX.eq.5000) THEN
         CTLIER=3
      ELSE
         SIGNAL='D'
         WRITE(SIGMSG,'("ENERGY RANGE ",2f8.1,
     &        " is not supported.")') CTLEMN,CTLEMX
         RETURN
      ENDIF

C     calculate sr of rings corresponding to channels
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
C     
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
c
