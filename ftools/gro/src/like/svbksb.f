c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      SUBROUTINE SVBKSB(U,W,V,M,N,MP,NP,B,X)
C
C
C  $Id: svbksb.f,v 1.2 2013/05/21 19:08:27 irby Exp $
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
c Description:	
c
c
c
C  $Log: svbksb.f,v $
C  Revision 1.2  2013/05/21 19:08:27  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:44  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  19:34:29  jae
c added COMMON cnfrep.copy and lines for LOC
c
c Revision 5.1  1996/02/29  20:53:31  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:30  jae
c Subroutine Module for like V5.00
c
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE SVBKSB(U,W,V,M,N,MP,NP,B,X)

      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'

      save

      character(80) id
      common /id/id
      PARAMETER (NMAX=100)
      DIMENSION U(MP,NP),W(NP),V(NP,NP),B(MP),X(NP),TMP(NMAX)

      id = '$Id: svbksb.f,v 1.2 2013/05/21 19:08:27 irby Exp $'
      LOC='SVBKSB'


      if (jae_svbksb) write(*,'("In routine ",a)') LOC

      DO 12 J=1,N
         S=0.
         IF (W(J).NE.0.) THEN
            DO 11 I=1,M
               S=S+U(I,J)*B(I)
 11         CONTINUE
            S=S/W(J)
         ENDIF

         TMP(J)=S
 12   CONTINUE

      DO 14 J=1,N
         S=0.
         DO 13 JJ=1,N
            S=S+V(J,JJ)*TMP(JJ)
 13      CONTINUE
         X(J)=S
 14   CONTINUE

      RETURN
      END
c
