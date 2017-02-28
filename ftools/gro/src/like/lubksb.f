C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      SUBROUTINE LUBKSB(A,N,NP,INDX,B)
C
C
C  $Id: lubksb.f,v 1.2 2013/05/21 19:08:26 irby Exp $
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
c+ Description:	NUMERICAL RECIPES matrix substitution code
C
C
C
C  $Log: lubksb.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:37  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  19:03:59  jae
c added COMMON cnfrep.copy and line for LOC
c
c Revision 5.1  1996/02/29  20:51:42  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:50  jae
c Subroutine Module for like V5.00
c
C

      SUBROUTINE LUBKSB(A,N,NP,INDX,B)

      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'

      save

      character(80) id
      common /id/id
      DIMENSION A(NP,NP),INDX(N),B(N)

      id = '$Id: lubksb.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      LOC='LUBKSB'

      if(jae_lubksb)write(*,'("In routine ",a)') LOC

      II=0
      DO 12 I=1,N
         LL=INDX(I)
         SUM=B(LL)
         B(LL)=B(I)
         IF (II.NE.0)THEN
            DO 11 J=II,I-1
               SUM=SUM-A(I,J)*B(J)
 11         CONTINUE
         ELSE IF (SUM.NE.0.) THEN
            II=I
         ENDIF
         B(I)=SUM
 12   CONTINUE
      DO 14 I=N,1,-1
         SUM=B(I)
         IF(I.LT.N)THEN
            DO 13 J=I+1,N
               SUM=SUM-A(I,J)*B(J)
 13         CONTINUE
         ENDIF
         B(I)=SUM/A(I,I)
 14   CONTINUE
      RETURN
      END
c
