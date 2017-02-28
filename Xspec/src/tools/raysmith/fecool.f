**==fecool.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION FECOOL(Dene,T,Ipr)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL ABIn , ABUnd , ABUnj , BIN , BINmin , BINsyz , c2 , c3 , 
     &     CNC , Dene , fc , fct , FECOOL , FEForb , FEIii , FEIiio , 
     &     FEInf , FEPerm , PTOt , st
      REAL T
      INTEGER Ipr , NBIn , NJ
C*** End of declarations inserted by SPAG
 
C  COOLING DUE TO FE II AND FE III ASSUMING THAT CONCE=CONCE(MG)
C  IF IRON IS NOT EXPLICITLY CALCULATED.
C  NUSSBAUMER AND STOREY FE II OMEGA'S AND ROBB FE III
 
      INCLUDE 'rayspec.inc'
 
      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn
      COMMON /COM   / CNC(12,30) , PTOt(12,220) , ABIn(MXBINS) , 
     &                BIN(MXBINS)
      COMMON /FE    / FEInf , FEPerm , FEForb , FEIii , FEIiio
 
      st = SQRT(T)
      c2 = CNC(6,2)
      c3 = CNC(6,3)
      IF ( NJ(11).EQ.26 ) c2 = CNC(11,2)
      IF ( NJ(11).EQ.26 ) c3 = CNC(11,3)
      fct = 1.6E11*10.**(ABUnj(11)-12.)*8.63E-6*c2/st
      FEInf = fct*(.31*.048*EXP(-553./T)+.42*.99*EXP(-11400./T))
      FEInf = FEInf*580.*st/(Dene+580.*st)
      FEPerm = fct*3.4*4.47*EXP(-55000./T)
      FEForb = fct*0.1*2.9*EXP(-33000./T)*1.2E6*st/(Dene+1.2E6*st)
      fct = 1.6E11*10.**(ABUnj(11)-12.)*8.63E-6*c3/st
      FEIii = fct*.30*.054*EXP(-625./T)*1590.*st/(Dene+1590.*st)
      FEIiio = fct*.43*2.66*EXP(-30840./T)*521000.*st/(Dene+521000.*st)
 
      fc = FEInf + FEPerm + FEForb + FEIii + FEIiio
      FECOOL = fc
      IF ( Ipr.NE.0 ) PRINT 99001 , fc , FEInf , FEPerm , FEForb , 
     &                      FEIii , FEIiio
      RETURN
99001 FORMAT (
     &   ' IRON COOLING; FC,II IR, II UV, II OPT, III IR, III      OPT '
     &   ,6F9.6)
      END
 
