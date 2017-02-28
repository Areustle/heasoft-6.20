C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C		SUBROUTINE LUDCMP(A,N,NP,INDX,D)
C
C
C  $Id: ludcmp.f,v 1.2 2013/05/21 19:08:26 irby Exp $
c
C=======================================================================
C
C       NUMERICAL RECIPES code used by EGRET LIKE point source analysis
C=======================================================================
C++        EFFECT:
C++        Adapted from Numerical Recipes, Press etal., 1992,
C++        for the EGRET LIKE program. When modifyed, the original code
C++        is left as a comment. Press etal. have given the Compton SSC
C++	   a limited license to distribute these routines for use with
C++	   the EGRET LIKE program. NEITHER THIS FILE NOR ANY OF ITS
C++	   CONTENTS ARE TO PLACED IN AN ANONYMOUS FTP DIRECTORY, NOR
C++	   OTHERWISE BE MADE PUBLIC. To do so would violate the 
C++	   copyright of Press etal. 
c
c+ Description:	NUMERICAL RECIPES matrix LU decomposition code
C
C
C
C  $Log: ludcmp.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:37  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:51:43  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:52  jae
c Subroutine Module for like V5.00
c
C
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE LUDCMP(A,N,NP,INDX,D)

C     Common blocks used:
      INCLUDE '../COMMON/cnfrep.copy'
      INCLUDE '../COMMON/errrep.copy'

      save

      character(80) id
      common /id/id
      PARAMETER (NMAX=100,TINY=1.0E-20)
      DIMENSION A(NP,NP),INDX(N),VV(NMAX)

      id = '$Id: ludcmp.f,v 1.2 2013/05/21 19:08:26 irby Exp $'

      D=1.
      DO 12 I=1,N
         AAMAX=0.
         DO 11 J=1,N
            IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
 11      CONTINUE
c     IF (AAMAX.EQ.0.) PAUSE 'Singular matrix.'
         IF (AAMAX.EQ.0.) then
            signal='S'
            sigmsg='OPTIMIZE: information matrix is singular.'
            return
         endif
         VV(I)=1./AAMAX
 12   CONTINUE
      DO 19 J=1,N
         IF (J.GT.1) THEN
            DO 14 I=1,J-1
               SUM=A(I,J)
               IF (I.GT.1)THEN
                  DO 13 K=1,I-1
                     SUM=SUM-A(I,K)*A(K,J)
 13               CONTINUE
                  A(I,J)=SUM
               ENDIF
 14         CONTINUE
         ENDIF
         AAMAX=0.
         DO 16 I=J,N
            SUM=A(I,J)
            IF (J.GT.1)THEN
               DO 15 K=1,J-1
                  SUM=SUM-A(I,K)*A(K,J)
 15            CONTINUE
               A(I,J)=SUM
            ENDIF
            DUM=VV(I)*ABS(SUM)
            IF (DUM.GE.AAMAX) THEN
               IMAX=I
               AAMAX=DUM
            ENDIF
 16      CONTINUE
         IF (J.NE.IMAX)THEN
            DO 17 K=1,N
               DUM=A(IMAX,K)
               A(IMAX,K)=A(J,K)
               A(J,K)=DUM
 17         CONTINUE
            D=-D
            VV(IMAX)=VV(J)
         ENDIF
         INDX(J)=IMAX
         IF(J.NE.N)THEN
            IF(A(J,J).EQ.0.)A(J,J)=TINY
            DUM=1./A(J,J)
            DO 18 I=J+1,N
               A(I,J)=A(I,J)*DUM
 18         CONTINUE
         ENDIF
 19   CONTINUE
      IF(A(N,N).EQ.0.)A(N,N)=TINY
      RETURN
      END
c
