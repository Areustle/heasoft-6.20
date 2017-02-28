c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
C      SUBROUTINE AMOEBAJ(P,Y,MP,NP,NDIM,FTOL,ITER,jflag)
C
C
C  $Id: amoebaj.f,v 1.3 2013/05/21 19:08:25 irby Exp $
c
C=======================================================================
C++        EFFECT: AMOEBAJ is adapted from the amoeba.c function
C++        from Numerical Recipes, Press etal., 1992,
C++        for the EGRET LIKE program. When modifyed, the original code
C++        is left as a comment. Press etal. have given the Compton SSC
C++	   a limited license to distribute these routines for use with
C++	   the EGRET LIKE program. NEITHER THIS FILE NOR ANY OF ITS
C++	   CONTENTS ARE TO PLACED IN AN ANONYMOUS FTP DIRECTORY, NOR
C++	   OTHERWISE BE MADE PUBLIC. To do so would violate the 
C++	   copyright of Press etal. includes:

C=============================================================================
C*      effect: The position of the maximum Test Statistic (TS) is located
C			
C*                      
C=============================================================================
C LIKE Version: 5.0 DELIVERED: March 25th 1994, Programmer J.A. ESPOSITO
C+             UPDATED:     by  JAE
C=============================================================================
C* Subroutine Argument List
C* ^^^^^^^^^^^^^^^^^^^^^^^^
C* P(3,2)  real array of initial verticies, P(n,1)=Long(RA), P(n,2)=Lat(DEC)
C* Y(3)    real vector of -TS values corresponding to P(n,.) verticies
C* MP      integer = 3
C* NP	   integer = 2
C* NDIM    integer = 2
C* FTOL    real tolerance of AMOEBA search
C* ITER    integer number of iterations performed
C* jflag   logical, if true produces enhanced screen output (VERBOSE)
C*============================================================================
C* Returned Values
C* ^^^^^^^^^^^^^^^
C* P(3,2)  contains three verticies which enclose the minimum value of -TS
C* Y(3)    contains the corresponding values of -TS
C* ITER    contains the number of iterations performed
c*============================================================================
C%   Changes:
c
c       Numerical Recipies routine.  added variable: jflag == verbose is
c                                    passed to FUNKJ.
c
c                                    FUNKJ is NOT part of the variable list
c
c       NOTE: The dimensionality is set to 2-dim rather than using
c             MP and NP, ITMAX set low at 50.
c             If ITER > 50 returns 'M' in signal. -TS returned is the best 
c             value.
c                            JAE
C  $Log: amoebaj.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/04/18 19:34:08  irby
C  Bug fixes for OSF build:
C
C  - change "dreal" to acceptable replacement "dble"
C  - changed null string comparisons '' to ' ' - this may not be the best
C    solution.
C  - removed malloc.h from like.h (unnecessary, and not available under Darwin)
C  - Makefile generated using: mkmk version 1.81
C
C  Revision 1.1  2002/04/16 20:27:27  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  17:07:44  jae
c included use of jae_amoebaj flag
c
c Revision 5.1  1996/02/29  20:46:39  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:35:58  jae
c Subroutine Module for like V5.00
c
c==========================================================================
      SUBROUTINE AMOEBAJ(P,Y,MP,NP,NDIM,FTOL,ITER,jflag)

      PARAMETER (NMAXJ=2,ALPHAJ=1.0,BETAJ=0.5,GAMMAj=2.0,ITMAX=50)
      DIMENSION P(3,2),Y(3),PR(2),PRR(2),PBAR(2)

C     *  Common blocks included:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/xrrrep.copy'
      
      save

      character(80) id
      common /id/id
      logical jflag


      id = '$Id: amoebaj.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      LOC='AMOEBAJ'

      if (jae_amoebaj)write(*,'("In routine ",a)') LOC
c
      MPTS=NDIM+1
      ITER=0
c
      if (jae_amoebaj)then
         do klp = 1,3
            print *,p(klp,1),p(klp,2),y(klp)
         enddo
      endif
c
 1    ILO=1

      IF (Y(1).GT.Y(2))THEN
         IHI=1
         INHI=2
      ELSE
         IHI=2
         INHI=1
      ENDIF

      DO 11 I=1,MPTS
         IF (Y(I).LT.Y(ILO)) ILO=I
         IF (Y(I).GT.Y(IHI))THEN
            INHI=IHI
            IHI=I
         ELSE IF (Y(I).GT.Y(INHI))THEN
            IF (I.NE.IHI) INHI=I
         ENDIF
 11   CONTINUE

      RTOL=2.*ABS(Y(IHI)-Y(ILO))/(ABS(Y(IHI))+ABS(Y(ILO)))

      IF (RTOL.LT.FTOL)RETURN
      IF (ITER.ge.ITMAX)then
         SIGNAL ='M'
         return
      endif

      ITER=ITER+1

      if (jae_amoebaj)then
         print *,' '
         print *,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
         print *,'AMOEBA ITERATION: ',iter
         print *,' Saved -TS minima:'
         print *,' '
         do llj=1,3
            print *,'Position:',llj,P(llj,1),P(llj,2),
     &           ' -TS:',Y(llj)
         enddo
         print *,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
      endif

      DO 12 J=1,NDIM
         PBAR(J)=0.
 12   CONTINUE

      DO 14 I=1,MPTS
         IF (I.NE.IHI)THEN
            DO 13 J=1,NDIM
               PBAR(J)=PBAR(J)+P(I,J)
 13         CONTINUE
         ENDIF
 14   CONTINUE
c
      DO 15 J=1,NDIM
         PBAR(J)=PBAR(J)/NDIM
         PR(J)=(1.+ALPHAJ)*PBAR(J)-ALPHAJ*P(IHI,J)
 15   CONTINUE

      YPR=FUNKJ(PR,jflag)

      if (signal.ne.' '.and.YPR.gt.1)return

      IF (YPR.LE.Y(ILO))THEN
         DO 16 J=1,NDIM
            PRR(J)=GAMMAJ*PR(J)+(1.-GAMMAJ)*PBAR(J)
 16      CONTINUE

         YPRR=FUNKJ(PRR,jflag)

         if (signal.ne.' '.and.YPRR.gt.1)return

         IF (YPRR.LT.Y(ILO))THEN
            DO 17 J=1,NDIM
               P(IHI,J)=PRR(J)
 17         CONTINUE
            Y(IHI)=YPRR
         ELSE
            DO 18 J=1,NDIM
               P(IHI,J)=PR(J)
 18         CONTINUE
            Y(IHI)=YPR
         ENDIF

      ELSE IF (YPR.GE.Y(INHI))THEN
         IF (YPR.LT.Y(IHI))THEN
            DO 19 J=1,NDIM
               P(IHI,J)=PR(J)
 19         CONTINUE
            Y(IHI)=YPR
         ENDIF

         DO 21 J=1,NDIM
            PRR(J)=BETAJ*P(IHI,J)+(1.-BETAJ)*PBAR(J)
 21      CONTINUE

         YPRR=FUNKJ(PRR,jflag)

         if (signal.ne.' '.and.YPRR.gt.1)return
         IF (YPRR.LT.Y(IHI))THEN
            DO 22 J=1,NDIM
               P(IHI,J)=PRR(J)
 22         CONTINUE
            Y(IHI)=YPRR
         ELSE
            DO 24 I=1,MPTS
               IF (I.NE.ILO)THEN
                  DO 23 J=1,NDIM
                     PR(J)=0.5*(P(I,J)+P(ILO,J))
                     P(I,J)=PR(J)
 23               CONTINUE
                  Y(I)=FUNKJ(PR,jflag)
                  if (signal.ne.' '.and.Y(I).gt.1)return
               ENDIF
 24         CONTINUE
         ENDIF

      ELSE
         DO 25 J=1,NDIM
            P(IHI,J)=PR(J)
 25      CONTINUE
         Y(IHI)=YPR
      ENDIF

      GO TO 1
      END
c
