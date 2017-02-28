C      FUNCTION DBRENT(AX,BX,CX,XMIN)
C
C
C  $Id: dbrent.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++  Spec:                                                              
C++        Adapted from Numerical Recipes, Press etal., e.g.,    
C++        modified so that function F and devivative are found 
C++        simultaneously by CHOOD.                                    
C++                                                                
C++        AX,BX,CX    initial search range, BX between AX and CX
C++        XMIN        abscissa of minimum                                  
C++        TOL         fractional precision 
C=======================================================================
C LIKE Version: 4.6  DELIVERED: June 1993      JMATTOX
C+             UPDATED:    by  JRM
C=======================================================================
C  $Log: dbrent.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:29  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  20:02:34  jae
c added lines for LOC
c
c Revision 5.1  1996/02/29  20:47:22  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:38  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      FUNCTION DBRENT(AX,BX,CX,XMIN)

c     Common blocks included
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'

      save

      character(80) id
      common /id/id
      PARAMETER (ITMAX=100,ZEPS=1.0E-10,TOL=1.e-5)
      LOGICAL OK1,OK2


      id = '$Id: dbrent.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      LOC='DBRENT'

      if (jae_dbrent)write(*,'("In routine ",a)') LOC

      A=MIN(AX,CX)
      B=MAX(AX,CX)
      V=BX
      W=V
      X=V
      E=0.
      Counts=x

      call CHOOD(map,bmap,gasmap,emap,FX,DX,CTLMSZ1,CTLMSZ2)

      FV=FX
      FW=FX
      DV=DX
      DW=DX

      DO 11 ITER=1,ITMAX
         XM=0.5*(A+B)
         TOL1=TOL*ABS(X)+ZEPS
         TOL2=2.*TOL1

         IF (ABS(X-XM).LE.(TOL2-.5*(B-A))) GOTO 3
         IF (ABS(E).GT.TOL1) THEN
            D1=2.*(B-A)
            D2=D1
            IF (DW.NE.DX) D1=(W-X)*DX/(DX-DW)
            IF (DV.NE.DX) D2=(V-X)*DX/(DX-DV)
            U1=X+D1
            U2=X+D2
            OK1=((A-U1)*(U1-B).GT.0.).AND.(DX*D1.LE.0.)
            OK2=((A-U2)*(U2-B).GT.0.).AND.(DX*D2.LE.0.)
            OLDE=E
            E=D
            IF (.NOT.(OK1.OR.OK2)) THEN
               GO TO 1
            ELSE IF (OK1.AND.OK2) THEN
               IF (ABS(D1).LT.ABS(D2)) THEN
                  D=D1
               ELSE
                  D=D2
               ENDIF
            ELSE IF (OK1) THEN
               D=D1
            ELSE
               D=D2
            ENDIF
            IF (ABS(D).GT.ABS(0.5*OLDE))GO TO 1
            U=X+D
            IF (U-A.LT.TOL2 .OR. B-U.LT.TOL2) D=SIGN(TOL1,XM-X)
            GOTO 2
         ENDIF

 1       IF (DX.GE.0.) THEN
            E=A-X
         ELSE
            E=B-X
         ENDIF

         D=0.5*E

 2       IF (ABS(D).GE.TOL1) THEN
            U=X+D
            Counts=u
            call CHOOD(map,bmap,gasmap,emap,FU,DU,CTLMSZ1,CTLMSZ2)
         ELSE
            U=X+SIGN(TOL1,D)
            Counts=u
            call CHOOD(map,bmap,gasmap,emap,FU,DU,CTLMSZ1,CTLMSZ2)
            IF (FU.GT.FX)GO TO 3
         ENDIF

         IF (FU.LE.FX) THEN
            IF (U.GE.X) THEN
               A=X
            ELSE
               B=X
            ENDIF
            V=W
            FV=FW
            DV=DW
            W=X
            FW=FX
            DW=DX
            X=U
            FX=FU
            DX=DU
         ELSE
            IF (U.LT.X) THEN
               A=U
            ELSE
               B=U
            ENDIF
            IF (FU.LE.FW .OR. W.EQ.X) THEN
               V=W
               FV=FW
               DV=DW
               W=U
               FW=FU
               DW=DU
            ELSE IF (FU.LE.FV .OR. V.EQ.X .OR. V.EQ.W) THEN
               V=U
               FV=FU
               DV=DU
            ENDIF
         ENDIF

 11   CONTINUE

      signal='D'
      sigmsg='DBRENT: exceeded maximum iterations.'
 3    XMIN=X
      DBRENT=FX

      RETURN
      END
c
