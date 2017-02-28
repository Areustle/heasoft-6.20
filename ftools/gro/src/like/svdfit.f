c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      SUBROUTINE SVDFIT(X,Y,SIG,NDATA,A,MA,U,V,W,MP,NP,CHISQ)
C
C
C  $Id: svdfit.f,v 1.2 2013/05/21 19:08:27 irby Exp $
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
c Description: NUMERICAL RECIPES single value decomposition linear fit
c
c  $Log: svdfit.f,v $
c  Revision 1.2  2013/05/21 19:08:27  irby
c  Change character*n to character(n) to silence warnings: "Obsolescent
c  feature: Old-style character length".
c
c  Revision 1.1  2002/04/16 20:27:44  irby
c  New GRO tool 'like'.  Submitted by S.Bansal.
c
c Revision 5.1  1996/02/29  20:53:36  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:33  jae
c Subroutine Module for like V5.00
c
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE SVDFIT(X,Y,SIG,NDATA,A,MA,U,V,W,MP,NP,CHISQ,FUNCS)
C
C     Common blocks used:
C     
      INCLUDE  '../COMMON/errrep.copy'
      include '../COMMON/cnfrep.copy'

      save

      character(80) id
      common /id/id
      PARAMETER(NMAX=1000,MMAX=3,TOL=1.E-5)
      DIMENSION X(NDATA),Y(NDATA),SIG(NDATA),A(MA),V(NP,NP),
     *     U(MP,NP),W(NP),B(NMAX),AFUNC(MMAX)


      id = '$Id: svdfit.f,v 1.2 2013/05/21 19:08:27 irby Exp $'

      DO 12 I=1,NDATA
         CALL basis_funcs(X(I),AFUNC,MA)
         TMP=1./SIG(I)
         DO 11 J=1,MA
            U(I,J)=AFUNC(J)*TMP
 11      CONTINUE
         B(I)=Y(I)*TMP
 12   CONTINUE
      
      CALL SVDCMP(U,NDATA,MA,MP,NP,W,V)
      if (signal.ne.' ') return
      WMAX=0.

      DO 13 J=1,MA
         IF(W(J).GT.WMAX)WMAX=W(J)
 13   CONTINUE

      THRESH=TOL*WMAX

      DO 14 J=1,MA
         IF (W(J).LT.THRESH) W(J)=0.
 14   CONTINUE

      CALL SVBKSB(U,W,V,NDATA,MA,MP,NP,B,A)
      CHISQ=0.

      DO 16 I=1,NDATA
         CALL basis_funcs(X(I),AFUNC,MA)
         SUM=0.
         DO 15 J=1,MA
            SUM=SUM+A(J)*AFUNC(J)
 15      CONTINUE
         CHISQ=CHISQ+((Y(I)-SUM)/SIG(I))**2
 16   CONTINUE

      RETURN
      END
