      SUBROUTINE BINT(RRE,THETA,PHI,BR,BT,BP)
C***********************************************************************
C*  Institute:     MPE               *                *                *
C*                                   *                *   SUBROUTINE   *
C*  DDDDDD         AA       LLL      *       GRO      *     P4BINT     *
C*  DDDDDDD      AA  AA     LLL      *     COMPTEL    *                *
C*  DD    DD    AA    AA    LLL      *                * First Version  *
C*  DD    DD    AA    AA    LLL      ******************                **
C*  DD    DD    AAAAAAAA    LLL      *                * Author:         *
C*  DD    DD    AAAAAAAA    LLL      *                * Georg           *
C*  DD    DD    AA    AA    LLL      *    COMPASS     * Weidenspointner *
C*  DD    DD    AA    AA    LLL      *                *                **
C*  DDDDDDD     AA    AA    LLLLLLLL *                * Date :         *
C*  DDDDDD      AA    AA    LLLLLLLL *                *  04-12-95      *
C*                                   *                *                *
C*  Data        Access      Layer    *                *                *
C***********************************************************************
C*                                                                     *
C*  Function: called from P4SHLL                                       *
C*            contains ALLMAG calculating local magnetic B-field       *
C*            and READC initializing the magnetic field model          *
C*                                                                     *
C*  Note: code was provided by Kevin Bennett, ESTEC                    *
C*                                                                     *
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
C   Description: tbd
C***********************************************************************

      IMPLICIT NONE

C INPUTS:
      real*8  rre, theta, phi
C OUTPUTS:
      real*8  br, bt, bp
C LOCAL:
      real*8  sit, cot, sip, cop

      SIT = DSIN(theta)
      COT = DCOS(theta)
      SIP = DSIN(phi)
      COP = DCOS(phi)
      CALL ALLMAG(RRE, SIT, COT, SIP, COP, BR, BT, BP)

      return
      end
C-----------------------------------------------------------------------


C-----------------------------------------------------------------------
      SUBROUTINE ALLMAG(RKM, ST, CT, SPH, CPH, BR, BT, BP)

C *  GEOCENTRIC VERSION OF GEOMAGNETIC FIELD ROUTINE
C *  DOUBLE PRECISION DECK FOR CII-HB / DPS05
C *  LONG DECK, THROUGH NMAX=13, FIXED INDICES WITHOUT DO LOOPS
C *  EXECUTION TIME PER CALL FACTOR OF THREE LESS THAN SHORT DECK
C *  PROGRAM DESIGNED AND TESTED BY E G STASSINOPOULOS AND G D MEAD,
C *  CODE 641, NASA GODDARD SPACE FLT CTR, GREENBELT, MD 20771
C ----  MODIFIED JUNE 82 BY L.JENSEN ESTEC/TMA: FIELD MODEL COEFFICIENTS
C ----  ARE READ IN FROM DATA-FILE RATHER THAN STORED IN CORE.
C  **         RKM      DISTANCE IN EARTH RADII TO EARTH'S CENTRE.
C  **         ST,CT    SIN & COS OF GEOCENTRIC COLATITUDE
C  **         SPH,CPH  SIN & COS OF EAST LONGITUDE
C  **  OUTPUT: BR,BT,BP GEOCENTRIC FIELD COMPONENTS IN GAUSS
C  **  NOTE: FOR GREATEST EFFICIENCY, COMPLETE ALL CALCULATIONS WITH
C            ONE MODEL AND ONE TIME BEFORE CHANGING MODELS OR TIME.
C
C  JUN 92: D. HEYNDERICKX (BIRA) : MOVED SCHMIDT NORMALISATION TO
C                                     SUBROUTINE READC
C                                  FIELD COEFFICIENTS AND NMAX IN 
C                                     COMMON COEFF

      IMPLICIT NONE

      REAL*8 G(13,13), RKM, ST, CT, SPH, CPH, BR, BT, BP, AR, AOR
      REAL*8 P21, P22, SP2, CP2, DP21, DP22, C2, SP3, CP3, P31, P32, P33
      REAL*8 DP31, DP32, DP33, C3, SP4, CP4, P41, P42, P43, P44, DP41
      REAL*8 DP42, DP43, DP44, C4, SP5, CP5, P51, P52, P53, P54, P55
      REAL*8 DP51, DP52, DP53, DP54, DP55, C5, SP6, CP6, P61, P62, P63
      REAL*8 P64, P65, P66, DP61, DP62, DP63, DP64, DP65, DP66, C6
      REAL*8 SP7, CP7, P71, P72, P73, P74, P75, P76, P77, DP71, DP72
      REAL*8 DP73, DP74, DP75, DP76, DP77, C7, SP8, CP8, P81, P82, P83
      REAL*8 P84, P85, P86, P87, P88, DP81, DP82, DP83, DP84, DP85, DP86
      REAL*8 DP87, DP88, C8, SP9, CP9, P91, P92, P93, P94, P95, P96, P97
      REAL*8 P98, P99, DP91, DP92, DP93, DP94, DP95, DP96, DP97, DP98
      REAL*8 DP99, C9, SP10, CP10, P101, P102, P103, P104, P105, P106
      REAL*8 P107, P108, P109, P1010, DP101, DP102, DP103, DP104, DP105
      REAL*8 DP106, DP107, DP108, DP109, DP1010, C10, SP11, CP11, P111
      REAL*8 P112, P113, P114, P115, P116, P117, P118, P119, P1110
      REAL*8 P1111, DP111, DP112, DP113, DP114, DP115, DP116, DP117
      REAL*8 DP118, DP119, DP1110, DP1111, C11, SP12, CP12, P121, P122
      REAL*8 P123, P124, P125, P126, P127, P128, P129, P1210, P1211
      REAL*8 P1212, DP121, DP122, DP123, DP124, DP125, DP126, DP127
      REAL*8 DP128, DP129, DP1210, DP1211, DP1212, C12, SP13, CP13, P131
      REAL*8 P132, P133, P134, P135, P136, P137, P138, P139, P1310
      REAL*8 P1311, P1312, P1313, DP131, DP132, DP133, DP134, DP135
      REAL*8 DP136, DP137, DP138, DP139, DP1310, DP1311, DP1312, DP1313
      REAL*8 C13
      INTEGER*4 NMAX

      COMMON /COEFF/ G, NMAX

C                                                           N= 2
      P21=CT
      P22=ST
      AR=1.0D0/RKM
      SP2=SPH
      CP2=CPH
      DP21=-P22
      DP22=P21
      AOR=AR*AR*AR
      C2=G(2,2)*CP2+G(1,2)*SP2
      BR=-(AOR+AOR)*(G(2,1)*P21+C2*P22)
      BT=AOR*(G(2,1)*DP21+C2*DP22)
      BP=AOR*(G(1,2)*CP2-G(2,2)*SP2)*P22
      IF (NMAX .LE. 2) GO TO 1
C                                                           N= 3
      SP3=(SP2+SP2)*CP2
      CP3=(CP2+SP2)*(CP2-SP2)
      P31=P21*P21-0.333333333D0
      P32=P21*P22
      P33=P22*P22
      DP31=-P32-P32
      DP32=P21*P21-P33
      DP33=-DP31
      AOR=AOR*AR
      C2=G(3,2)*CP2+G(1,3)*SP2
      C3=G(3,3)*CP3+G(2,3)*SP3
      BR=BR-3.0D0*AOR*(G(3,1)*P31+C2*P32+C3*P33)
      BT=BT+AOR*(G(3,1)*DP31+C2*DP32+C3*DP33)
      BP=BP-AOR*((G(3,2)*SP2-G(1,3)*CP2)*P32+2.0D0*(G(3,3)*SP3-G(2,3)*
     &CP3)*P33)
      IF (NMAX .LE. 3) GO TO 1
C                                                           N= 4
      SP4=SP2*CP3+CP2*SP3
      CP4=CP2*CP3-SP2*SP3
      P41=P21*P31-0.26666666D0*P21
      DP41=P21*DP31+DP21*P31-0.26666666D0*DP21
      P42=P21*P32-0.2D0*P22
      DP42=P21*DP32+DP21*P32-0.2D0*DP22
      P43=P21*P33
      DP43=P21*DP33+DP21*P33
      P44=P22*P33
      DP44=3.0D0*P43
      AOR=AOR*AR
      C2=G(4,2)*CP2+G(1,4)*SP2
      C3=G(4,3)*CP3+G(2,4)*SP3
      C4=G(4,4)*CP4+G(3,4)*SP4
      BR=BR-4.0D0*AOR*(G(4,1)*P41+C2*P42+C3*P43+C4*P44)
      BT=BT+AOR*(G(4,1)*DP41+C2*DP42+C3*DP43+C4*DP44)
      BP=BP-AOR*((G(4,2)*SP2-G(1,4)*CP2)*P42+2.0D0*(G(4,3)*SP3-G(2,4)*
     &CP3)*P43+3.0D0*(G(4,4)*SP4-G(3,4)*CP4)*P44)
      IF (NMAX .LE. 4) GO TO 1
C                                                           N= 5
      SP5=(SP3+SP3)*CP3
      CP5=(CP3+SP3)*(CP3-SP3)
      P51=P21*P41-0.25714285D0*P31
      DP51=P21*DP41+DP21*P41-0.25714285D0*DP31
      P52=P21*P42-0.22857142D0*P32
      DP52=P21*DP42+DP21*P42-0.22857142D0*DP32
      P53=P21*P43-0.14285714D0*P33
      DP53=P21*DP43+DP21*P43-0.14285714D0*DP33
      P54=P21*P44
      DP54=P21*DP44+DP21*P44
      P55=P22*P44
      DP55=4.0D0*P54
      AOR=AOR*AR
      C2=G(5,2)*CP2+G(1,5)*SP2
      C3=G(5,3)*CP3+G(2,5)*SP3
      C4=G(5,4)*CP4+G(3,5)*SP4
      C5=G(5,5)*CP5+G(4,5)*SP5
      BR=BR-5.0D0*AOR*(G(5,1)*P51+C2*P52+C3*P53+C4*P54+C5*P55)
      BT=BT+AOR*(G(5,1)*DP51+C2*DP52+C3*DP53+C4*DP54+C5*DP55)
      BP=BP-AOR*((G(5,2)*SP2-G(1,5)*CP2)*P52+2.0D0*(G(5,3)*SP3-G(2,5)*
     &CP3)*P53+3.0D0*(G(5,4)*SP4-G(3,5)*CP4)*P54+4.0D0*(G(5,5)*SP5-
     &G(4,5)*CP5)*P55)
      IF (NMAX .LE. 5) GO TO 1
C                                                           N= 6
      SP6=SP2*CP5+CP2*SP5
      CP6=CP2*CP5-SP2*SP5
      P61=P21*P51-0.25396825D0*P41
      DP61=P21*DP51+DP21*P51-0.25396825D0*DP41
      P62=P21*P52-0.23809523D0*P42
      DP62=P21*DP52+DP21*P52-0.23809523D0*DP42
      P63=P21*P53-0.19047619D0*P43
      DP63=P21*DP53+DP21*P53-0.19047619D0*DP43
      P64=P21*P54-0.11111111D0*P44
      DP64=P21*DP54+DP21*P54-0.11111111D0*DP44
      P65=P21*P55
      DP65=P21*DP55+DP21*P55
      P66=P22*P55
      DP66=5.0D0*P65
      AOR=AOR*AR
      C2=G(6,2)*CP2+G(1,6)*SP2
      C3=G(6,3)*CP3+G(2,6)*SP3
      C4=G(6,4)*CP4+G(3,6)*SP4
      C5=G(6,5)*CP5+G(4,6)*SP5
      C6=G(6,6)*CP6+G(5,6)*SP6
      BR=BR-6.0D0*AOR*(G(6,1)*P61+C2*P62+C3*P63+C4*P64+C5*P65+C6*P66)
      BT=BT+AOR*(G(6,1)*DP61+C2*DP62+C3*DP63+C4*DP64+C5*DP65+C6*DP66)
      BP=BP-AOR*((G(6,2)*SP2-G(1,6)*CP2)*P62+2.0D0*(G(6,3)*SP3-G(2,6)*
     &CP3)*P63+3.0D0*(G(6,4)*SP4-G(3,6)*CP4)*P64+4.0D0*(G(6,5)*SP5-
     &G(4,6)*CP5)*P65+5.0D0*(G(6,6)*SP6-G(5,6)*CP6)*P66)
      IF (NMAX .LE. 6) GO TO 1
C                                                           N= 7
      SP7=(SP4+SP4)*CP4
      CP7=(CP4+SP4)*(CP4-SP4)
      P71=P21*P61-0.25252525D0*P51
      DP71=P21*DP61+DP21*P61-0.25252525D0*DP51
      P72=P21*P62-0.24242424D0*P52
      DP72=P21*DP62+DP21*P62-0.24242424D0*DP52
      P73=P21*P63-0.21212121D0*P53
      DP73=P21*DP63+DP21*P63-0.21212121D0*DP53
      P74=P21*P64-0.16161616D0*P54
      DP74=P21*DP64+DP21*P64-0.16161616D0*DP54
      P75=P21*P65-0.09090909D0*P55
      DP75=P21*DP65+DP21*P65-0.09090909D0*DP55
      P76=P21*P66
      DP76=P21*DP66+DP21*P66
      P77=P22*P66
      DP77=6.0D0*P76
      AOR=AOR*AR
      C2=G(7,2)*CP2+G(1,7)*SP2
      C3=G(7,3)*CP3+G(2,7)*SP3
      C4=G(7,4)*CP4+G(3,7)*SP4
      C5=G(7,5)*CP5+G(4,7)*SP5
      C6=G(7,6)*CP6+G(5,7)*SP6
      C7=G(7,7)*CP7+G(6,7)*SP7
      BR=BR-7.0D0*AOR*(G(7,1)*P71+C2*P72+C3*P73+C4*P74+C5*P75+C6*P76+
     &C7*P77)
      BT=BT+AOR*(G(7,1)*DP71+C2*DP72+C3*DP73+C4*DP74+C5*DP75+C6*DP76+C7*
     &DP77)
      BP=BP-AOR*((G(7,2)*SP2-G(1,7)*CP2)*P72+2.0D0*(G(7,3)*SP3-G(2,7)*
     &CP3)*P73+3.0D0*(G(7,4)*SP4-G(3,7)*CP4)*P74+4.0D0*(G(7,5)*SP5-
     &G(4,7)*CP5)*P75+5.0D0*(G(7,6)*SP6-G(5,7)*CP6)*P76+6.0D0*(G(7,7)*
     &SP7-G(6,7)*CP7)*P77)
      IF (NMAX .LE. 7) GO TO 1
C                                                           N= 8
      SP8=SP2*CP7+CP2*SP7
      CP8=CP2*CP7-SP2*SP7
      P81=P21*P71-0.25174825D0*P61
      DP81=P21*DP71+DP21*P71-0.25174825D0*DP61
      P82=P21*P72-0.24475524D0*P62
      DP82=P21*DP72+DP21*P72-0.24475524D0*DP62
      P83=P21*P73-0.22377622D0*P63
      DP83=P21*DP73+DP21*P73-0.22377622D0*DP63
      P84=P21*P74-0.18881118D0*P64
      DP84=P21*DP74+DP21*P74-0.18881118D0*DP64
      P85=P21*P75-0.13986013D0*P65
      DP85=P21*DP75+DP21*P75-0.13986013D0*DP65
      P86=P21*P76-0.07692307D0*P66
      DP86=P21*DP76+DP21*P76-0.07692307D0*DP66
      P87=P21*P77
      DP87=P21*DP77+DP21*P77
      P88=P22*P77
      DP88=7.0D0*P87
      AOR=AOR*AR
      C2=G(8,2)*CP2+G(1,8)*SP2
      C3=G(8,3)*CP3+G(2,8)*SP3
      C4=G(8,4)*CP4+G(3,8)*SP4
      C5=G(8,5)*CP5+G(4,8)*SP5
      C6=G(8,6)*CP6+G(5,8)*SP6
      C7=G(8,7)*CP7+G(6,8)*SP7
      C8=G(8,8)*CP8+G(7,8)*SP8
      BR=BR-8.0D0*AOR*(G(8,1)*P81+C2*P82+C3*P83+C4*P84+C5*P85+C6*P86+
     &C7*P87+C8*P88)
      BT=BT+AOR*(G(8,1)*DP81+C2*DP82+C3*DP83+C4*DP84+C5*DP85+C6*DP86+C7*
     &DP87+C8*DP88)
      BP=BP-AOR*((G(8,2)*SP2-G(1,8)*CP2)*P82+2.0D0*(G(8,3)*SP3-G(2,8)*
     &CP3)*P83+3.0D0*(G(8,4)*SP4-G(3,8)*CP4)*P84+4.0D0*(G(8,5)*SP5-
     &G(4,8)*CP5)*P85+5.0D0*(G(8,6)*SP6-G(5,8)*CP6)*P86+6.0D0*(G(8,7)*
     &SP7-G(6,8)*CP7)*P87+7.0D0*(G(8,8)*SP8-G(7,8)*CP8)*P88)
      IF (NMAX .LE. 8) GO TO 1
C                                                           N= 9
      SP9=(SP5+SP5)*CP5
      CP9=(CP5+SP5)*(CP5-SP5)
      P91=P21*P81-0.25128205D0*P71
      DP91=P21*DP81+DP21*P81-0.25128205D0*DP71
      P92=P21*P82-0.24615384D0*P72
      DP92=P21*DP82+DP21*P82-0.24615384D0*DP72
      P93=P21*P83-0.23076923D0*P73
      DP93=P21*DP83+DP21*P83-0.23076923D0*DP73
      P94=P21*P84-0.20512820D0*P74
      DP94=P21*DP84+DP21*P84-0.20512820D0*DP74
      P95=P21*P85-0.16923076D0*P75
      DP95=P21*DP85+DP21*P85-0.16923076D0*DP75
      P96=P21*P86-0.12307692D0*P76
      DP96=P21*DP86+DP21*P86-0.12307692D0*DP76
      P97=P21*P87-0.06666666D0*P77
      DP97=P21*DP87+DP21*P87-0.06666666D0*DP77
      P98=P21*P88
      DP98=P21*DP88+DP21*P88
      P99=P22*P88
      DP99=8.0D0*P98
      AOR=AOR*AR
      C2=G(9,2)*CP2+G(1,9)*SP2
      C3=G(9,3)*CP3+G(2,9)*SP3
      C4=G(9,4)*CP4+G(3,9)*SP4
      C5=G(9,5)*CP5+G(4,9)*SP5
      C6=G(9,6)*CP6+G(5,9)*SP6
      C7=G(9,7)*CP7+G(6,9)*SP7
      C8=G(9,8)*CP8+G(7,9)*SP8
      C9=G(9,9)*CP9+G(8,9)*SP9
      BR=BR-9.0D0*AOR*(G(9,1)*P91+C2*P92+C3*P93+C4*P94+C5*P95+C6*P96+
     &C7*P97+C8*P98+C9*P99)
      BT=BT+AOR*(G(9,1)*DP91+C2*DP92+C3*DP93+C4*DP94+C5*DP95+C6*DP96+C7*
     &DP97+C8*DP98+C9*DP99)
      BP=BP-AOR*((G(9,2)*SP2-G(1,9)*CP2)*P92+2.0D0*(G(9,3)*SP3-G(2,9)*
     &CP3)*P93+3.0D0*(G(9,4)*SP4-G(3,9)*CP4)*P94+4.0D0*(G(9,5)*SP5-
     &G(4,9)*CP5)*P95+5.0D0*(G(9,6)*SP6-G(5,9)*CP6)*P96+6.0D0*(G(9,7)*
     &SP7-G(6,9)*CP7)*P97+7.0D0*(G(9,8)*SP8-G(7,9)*CP8)*P98+8.0D0*
     &(G(9,9)*SP9-G(8,9)*CP9)*P99)
      IF (NMAX .LE. 9) GO TO 1
C                                                           N=10
      SP10=SP2*CP9+CP2*SP9
      CP10=CP2*CP9-SP2*SP9
      P101=P21*P91-0.25098039D0*P81
      DP101=P21*DP91+DP21*P91-0.25098039D0*DP81
      P102=P21*P92-0.24705882D0*P82
      DP102=P21*DP92+DP21*P92-0.24705882D0*DP82
      P103=P21*P93-0.23529411D0*P83
      DP103=P21*DP93+DP21*P93-0.23529411D0*DP83
      P104=P21*P94-0.21568627D0*P84
      DP104=P21*DP94+DP21*P94-0.21568627D0*DP84
      P105=P21*P95-0.18823529D0*P85
      DP105=P21*DP95+DP21*P95-0.18823529D0*DP85
      P106=P21*P96-0.15294117D0*P86
      DP106=P21*DP96+DP21*P96-0.15294117D0*DP86
      P107=P21*P97-0.10980392D0*P87
      DP107=P21*DP97+DP21*P97-0.10980392D0*DP87
      P108=P21*P98-0.05882352D0*P88
      DP108=P21*DP98+DP21*P98-0.05882352D0*DP88
      P109=P21*P99
      DP109=P21*DP99+DP21*P99
      P1010=P22*P99
      DP1010=9.0D0*P109
      AOR=AOR*AR
      C2=G(10,2)*CP2+G(1,10)*SP2
      C3=G(10,3)*CP3+G(2,10)*SP3
      C4=G(10,4)*CP4+G(3,10)*SP4
      C5=G(10,5)*CP5+G(4,10)*SP5
      C6=G(10,6)*CP6+G(5,10)*SP6
      C7=G(10,7)*CP7+G(6,10)*SP7
      C8=G(10,8)*CP8+G(7,10)*SP8
      C9=G(10,9)*CP9+G(8,10)*SP9
      C10=G(10,10)*CP10+G(9,10)*SP10
      BR=BR-10.0D0*AOR*(G(10,1)*P101+C2*P102+C3*P103+C4*P104+C5*P105+
     &C6*P106+C7*P107+C8*P108+C9*P109+C10*P1010)
      BT=BT+AOR*(G(10,1)*DP101+C2*DP102+C3*DP103+C4*DP104+C5*DP105+C6*
     &DP106+C7*DP107+C8*DP108+C9*DP109+C10*DP1010)
      BP=BP-AOR*((G(10,2)*SP2-G(1,10)*CP2)*P102+2.0D0*(G(10,3)*SP3-
     &G(2,10)*CP3)*P103+3.0D0*(G(10,4)*SP4-G(3,10)*CP4)*P104+4.0D0*
     &(G(10,5)*SP5-G(4,10)*CP5)*P105+5.0D0*(G(10,6)*SP6-G(5,10)*CP6)*
     &P106+6.0D0*(G(10,7)*SP7-G(6,10)*CP7)*P107+7.0D0*(G(10,8)*SP8-
     &G(7,10)*CP8)*P108+8.0D0*(G(10,9)*SP9-G(8,10)*CP9)*P109+9.0D0*
     &(G(10,10)*SP10-G(9,10)*CP10)*P1010)
      IF (NMAX .LE. 10) GO TO 1
C                                                           N=11
      SP11=(SP6+SP6)*CP6
      CP11=(CP6+SP6)*(CP6-SP6)
      P111=P21*P101-0.25077399D0*P91
      DP111=P21*DP101+DP21*P101-0.25077399D0*DP91
      P112=P21*P102-0.24767801D0*P92
      DP112=P21*DP102+DP21*P102-0.24767801D0*DP92
      P113=P21*P103-0.23839009D0*P93
      DP113=P21*DP103+DP21*P103-0.23839009D0*DP93
      P114=P21*P104-0.22291021D0*P94
      DP114=P21*DP104+DP21*P104-0.22291021D0*DP94
      P115=P21*P105-0.20123839D0*P95
      DP115=P21*DP105+DP21*P105-0.20123839D0*DP95
      P116=P21*P106-0.17337461D0*P96
      DP116=P21*DP106+DP21*P106-0.17337461D0*DP96
      P117=P21*P107-0.13931888D0*P97
      DP117=P21*DP107+DP21*P107-0.13931888D0*DP97
      P118=P21*P108-0.09907120D0*P98
      DP118=P21*DP108+DP21*P108-0.09907120D0*DP98
      P119=P21*P109-0.05263157D0*P99
      DP119=P21*DP109+DP21*P109-0.05263157D0*DP99
      P1110=P21*P1010
      DP1110=P21*DP1010+DP21*P1010
      P1111=P22*P1010
      DP1111=10.0D0*P1110
      AOR=AOR*AR
      C2=G(11,2)*CP2+G(1,11)*SP2
      C3=G(11,3)*CP3+G(2,11)*SP3
      C4=G(11,4)*CP4+G(3,11)*SP4
      C5=G(11,5)*CP5+G(4,11)*SP5
      C6=G(11,6)*CP6+G(5,11)*SP6
      C7=G(11,7)*CP7+G(6,11)*SP7
      C8=G(11,8)*CP8+G(7,11)*SP8
      C9=G(11,9)*CP9+G(8,11)*SP9
      C10=G(11,10)*CP10+G(9,11)*SP10
      C11=G(11,11)*CP11+G(10,11)*SP11
      BR=BR-11.0D0*AOR*(G(11,1)*P111+C2*P112+C3*P113+C4*P114+C5*P115+
     &C6*P116+C7*P117+C8*P118+C9*P119+C10*P1110+C11*P1111)
      BT=BT+AOR*(G(11,1)*DP111+C2*DP112+C3*DP113+C4*DP114+C5*DP115+C6*
     &DP116+C7*DP117+C8*DP118+C9*DP119+C10*DP1110+C11*DP1111)
      BP=BP-AOR*((G(11,2)*SP2-G(1,11)*CP2)*P112+2.0D0*(G(11,3)*SP3-
     &G(2,11)*CP3)*P113+3.0D0*(G(11,4)*SP4-G(3,11)*CP4)*P114+4.0D0*
     &(G(11,5)*SP5-G(4,11)*CP5)*P115+5.0D0*(G(11,6)*SP6-G(5,11)*CP6)*
     &P116+6.0D0*(G(11,7)*SP7-G(6,11)*CP7)*P117+7.0D0*(G(11,8)*SP8-
     &G(7,11)*CP8)*P118+8.0D0*(G(11,9)*SP9-G(8,11)*CP9)*P119+9.0D0*
     &(G(11,10)*SP10-G(9,11)*CP10)*P1110+10.0D0*(G(11,11)*SP11-
     &G(10,11)*CP11)*P1111)
      IF (NMAX .LE. 11) GO TO 1
C                                                           N=12
      SP12=SP2*CP11+CP2*SP11
      CP12=CP2*CP11-SP2*SP11
      P121=P21*P111-0.25062656D0*P101
      DP121=P21*DP111+DP21*P111-0.25062656D0*DP101
      P122=P21*P112-0.24812030D0*P102
      DP122=P21*DP112+DP21*P112-0.24812030D0*DP102
      P123=P21*P113-0.24060150D0*P103
      DP123=P21*DP113+DP21*P113-0.24060150D0*DP103
      P124=P21*P114-0.22807017D0*P104
      DP124=P21*DP114+DP21*P114-0.22807017D0*DP104
      P125=P21*P115-0.21052631D0*P105
      DP125=P21*DP115+DP21*P115-0.21052631D0*DP105
      P126=P21*P116-0.18796992D0*P106
      DP126=P21*DP116+DP21*P116-0.18796992D0*DP106
      P127=P21*P117-0.16040100D0*P107
      DP127=P21*DP117+DP21*P117-0.16040100D0*DP107
      P128=P21*P118-0.12781954D0*P108
      DP128=P21*DP118+DP21*P118-0.12781954D0*DP108
      P129=P21*P119-0.09022556D0*P109
      DP129=P21*DP119+DP21*P119-0.09022556D0*DP109
      P1210=P21*P1110-0.04761904D0*P1010
      DP1210=P21*DP1110+DP21*P1110-0.04761904D0*DP1010
      P1211=P21*P1111
      DP1211=P21*DP1111+DP21*P1111
      P1212=P22*P1111
      DP1212=11.0D0*P1211
      AOR=AOR*AR
      C2=G(12,2)*CP2+G(1,12)*SP2
      C3=G(12,3)*CP3+G(2,12)*SP3
      C4=G(12,4)*CP4+G(3,12)*SP4
      C5=G(12,5)*CP5+G(4,12)*SP5
      C6=G(12,6)*CP6+G(5,12)*SP6
      C7=G(12,7)*CP7+G(6,12)*SP7
      C8=G(12,8)*CP8+G(7,12)*SP8
      C9=G(12,9)*CP9+G(8,12)*SP9
      C10=G(12,10)*CP10+G(9,12)*SP10
      C11=G(12,11)*CP11+G(10,12)*SP11
      C12=G(12,12)*CP12+G(11,12)*SP12
      BR=BR-12.0D0*AOR*(G(12,1)*P121+C2*P122+C3*P123+C4*P124+C5*P125+
     &C6*P126+C7*P127+C8*P128+C9*P129+C10*P1210+C11*P1211+C12*P1212)
      BT=BT+AOR*(G(12,1)*DP121+C2*DP122+C3*DP123+C4*DP124+C5*DP125+C6*
     &DP126+C7*DP127+C8*DP128+C9*DP129+C10*DP1210+C11*DP1211+C12*DP1212)
      BP=BP-AOR*((G(12,2)*SP2-G(1,12)*CP2)*P122+2.0D0*(G(12,3)*SP3-
     &G(2,12)*CP3)*P123+3.0D0*(G(12,4)*SP4-G(3,12)*CP4)*P124+4.0D0*
     &(G(12,5)*SP5-G(4,12)*CP5)*P125+5.0D0*(G(12,6)*SP6-G(5,12)*CP6)*
     &P126+6.0D0*(G(12,7)*SP7-G(6,12)*CP7)*P127+7.0D0*(G(12,8)*SP8-
     &G(7,12)*CP8)*P128+8.0D0*(G(12,9)*SP9-G(8,12)*CP9)*P129+9.0D0*
     &(G(12,10)*SP10-G(9,12)*CP10)*P1210+10.0D0*(G(12,11)*SP11-
     &G(10,12)*CP11)*P1211+11.0D0*(G(12,12)*SP12-G(11,12)*CP12)*P1212)
      IF (NMAX .LE. 12) GO TO 1
C                                                           N=13
      SP13=(SP7+SP7)*CP7
      CP13=(CP7+SP7)*(CP7-SP7)
      P131=P21*P121-0.25051759D0*P111
      DP131=P21*DP121+DP21*P121-0.25051759D0*DP111
      P132=P21*P122-0.24844720D0*P112
      DP132=P21*DP122+DP21*P122-0.24844720D0*DP112
      P133=P21*P123-0.24223602D0*P113
      DP133=P21*DP123+DP21*P123-0.24223602D0*DP113
      P134=P21*P124-0.23188405D0*P114
      DP134=P21*DP124+DP21*P124-0.23188405D0*DP114
      P135=P21*P125-0.21739130D0*P115
      DP135=P21*DP125+DP21*P125-0.21739130D0*DP115
      P136=P21*P126-0.19875776D0*P116
      DP136=P21*DP126+DP21*P126-0.19875776D0*DP116
      P137=P21*P127-0.17598343D0*P117
      DP137=P21*DP127+DP21*P127-0.17598343D0*DP117
      P138=P21*P128-0.14906832D0*P118
      DP138=P21*DP128+DP21*P128-0.14906832D0*DP118
      P139=P21*P129-0.11801242D0*P119
      DP139=P21*DP129+DP21*P129-0.11801242D0*DP119
      P1310=P21*P1210-0.08281573D0*P1110
      DP1310=P21*DP1210+DP21*P1210-0.08281573D0*DP1110
      P1311=P21*P1211-0.04347826D0*P1111
      DP1311=P21*DP1211+DP21*P1211-0.04347826D0*DP1111
      P1312=P21*P1212
      DP1312=P21*DP1212+DP21*P1212
      P1313=P22*P1212
      DP1313=12.0D0*P1312
      AOR=AOR*AR
      C2=G(13,2)*CP2+G(1,13)*SP2
      C3=G(13,3)*CP3+G(2,13)*SP3
      C4=G(13,4)*CP4+G(3,13)*SP4
      C5=G(13,5)*CP5+G(4,13)*SP5
      C6=G(13,6)*CP6+G(5,13)*SP6
      C7=G(13,7)*CP7+G(6,13)*SP7
      C8=G(13,8)*CP8+G(7,13)*SP8
      C9=G(13,9)*CP9+G(8,13)*SP9
      C10=G(13,10)*CP10+G(9,13)*SP10
      C11=G(13,11)*CP11+G(10,13)*SP11
      C12=G(13,12)*CP12+G(11,13)*SP12
      C13=G(13,13)*CP13+G(12,13)*SP13
      BR=BR-13.0D0*AOR*(G(13,1)*P131+C2*P132+C3*P133+C4*P134+C5*P135+
     &C6*P136+C7*P137+C8*P138+C9*P139+C10*P1310+C11*P1311+C12*P1312+
     &C13*P1313)
      BT=BT+AOR*(G(13,1)*DP131+C2*DP132+C3*DP133+C4*DP134+C5*DP135+C6*
     &DP136+C7*DP137+C8*DP138+C9*DP139+C10*DP1310+C11*DP1311+C12*DP1312+
     &C13*DP1313)
      BP=BP-AOR*((G(13,2)*SP2-G(1,13)*CP2)*P132+2.0D0*(G(13,3)*SP3-
     &G(2,13)*CP3)*P133+3.0D0*(G(13,4)*SP4-G(3,13)*CP4)*P134+4.0D0*
     &(G(13,5)*SP5-G(4,13)*CP5)*P135+5.0D0*(G(13,6)*SP6-G(5,13)*CP6)*
     &P136+6.0D0*(G(13,7)*SP7-G(6,13)*CP7)*P137+7.0D0*(G(13,8)*SP8-
     &G(7,13)*CP8)*P138+8.0D0*(G(13,9)*SP9-G(8,13)*CP9)*P139+9.0D0*
     &(G(13,10)*SP10-G(9,13)*CP10)*P1310+10.0D0*(G(13,11)*SP11-
     &G(10,13)*CP11)*P1311+11.0D0*(G(13,12)*SP12-G(11,13)*CP12)*P1312+
     &12.0D0*(G(13,13)*SP13-G(12,13)*CP13)*P1313)

    1 BR = BR / 1.0D5
      BT = BT / 1.0D5
      BP = BP / ST / 1.0D5

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE READC(MODEL, TM, ORDER, TZERO, MODLBL, GMAGMO, MMOFLG,
     &                 COLAT, ELONG)

C*********************************************************************
C     INITIALIZE FIELD COEFFIENTS FOR USE BY ROUTINE ALLMAG.         *
C     ( IN THE APPROPRIATE VERSION !!! )                             *
C     ---------------------------------------------------------------*
C     THE ROUTINE READS THE GEOMAGNETIC FIELD MODELS IN              *
C     "CAIN-STANDARD" FROM LOGICAL UNIT "11".                        *
C     ERROR-MESSAGES AND DEBUG OUTPUT APPEARS ON LOGICAL UNIT "06"   *
C     A VARIETY OF ERROR-CONDITIONS WILL CAUSE THE ROUTINE TO STOP   *
C     PROGRAM EXECUTION.                                             *
C                                                                    *
C     LAYOUT OF DATA-FILE.                                           *
C     --------------------                                           *
C                                                                    *
C                                                                    *
C     PARAMETERS.                                                    *
C     -----------                                                    *
C     MODEL    THE NUMBER OF THE MODEL TO BE READ IN. (THE MODELS    *
C              ARE STORED IN SEQUENCE ON THE FILE).                  *
C     GG       THE FIELD COEFFICIENTS AT TIME EQUAL T0               *
C     GGT      THE 1.ST ORDER TIME-VARIATION OF THE COEFFICIENTS     *
C     GGTT     THE 2.ND -     -              -  -   -                *
C              (ALL COEFFICIENT MATRICES ARE OF DIMENSION 13,13)     *
C     NMAX     THE ORDER OF THE GEOMAGNETIC FIELD MODEL              *
C              (NOTE !! THE MAXIMUM FORTRAN-INDEX IN THE FIELD-      *
C              COEFFICIENT MATRIX BECOMES NMAX+1).                   *
C     TZERO    THE TIME (IN DECIMAL YEARS) FROM WHICH THE FIELD-     *
C              COEFFICIENTS ARE TO BE EXTRAPOLATED.                  *
C                                                                    *
C     MODLBL   NAME OF THE CHOSEN MODEL IN A32 - FORMAT              *
C*********************************************************************
C
C JUN 92: D. HEYNDERICKX (BIRA/IASB) : MOVED GAUSS NORMALISATION FROM ALLMAG
C                                      FIELD COEFFICIENTS AND NMAX IN
C                                        COMMON COEFF

      IMPLICIT NONE

      REAL*4 GGBD, GGTBD
      REAL*8 GJC(27,2), GGSFC(65,2), GGSFC1(65,2), GGSFC2(65,2)
      REAL*8 RAD, TZERO, T0, T, TM, ELONG, COLAT, GMAGMO
      INTEGER*4 NMAX, ORDER, NMAXMX, IDIM, I, J, M, N, JJ, MODEL
      INTEGER*4 NM, MMOFLG, IDIM2, NMOD, NMAXBD
      character(32) MODLBL, LABJC, LABGS, LBL

C-----DEFINE PROGRAM STORAGE LIMITATIONS
      PARAMETER (NMAXMX = 12, IDIM = (NMAXMX+1), IDIM2 = IDIM * IDIM)
      PARAMETER (NMOD = 15)
      REAL*8 GG(IDIM,IDIM), GGT(IDIM,IDIM), GGTT(IDIM,IDIM)
      REAL*8 G(IDIM,IDIM), SHMIT(IDIM,IDIM)

      COMMON /DGRF/ T0(NMOD), LBL(NMOD), GGBD(NMOD,13,13),
     &              GGTBD(NMOD,13,13), NMAXBD(NMOD)
      COMMON /COEFF/ G, NMAX

      DATA      SHMIT /IDIM2*0.0D0/
      DATA      LABJC /'Jensen & Cain 1960               '/
      DATA      LABGS /'GSFC 12/66 120 Term              '/

      data      ((gjc(i,j),j=1,2),i=1,27) /30411.2,0.,2147.4,-5798.9,
     &            2403.5,0.,-5125.3,3312.4,-1338.1,-157.9,-3151.8,0.,
     &            6213.0,1487.0,-2489.8,-407.5,-649.6,21.0,-4179.4,0.,
     &            -4529.8,-1182.5,-2179.5,1000.6,700.8,43.0,-204.4,
     &            138.5,1625.6,0.,-3440.7,-79.6,-1944.7,-200.0,-60.8,
     &            459.7,277.5,242.1,69.7,-121.8,-1952.3,0.,-485.3,
     &            -575.8,321.2,-873.5,2141.3,-340.6,105.1,-11.8,22.7,
     &            -111.6,111.5,-32.5/  !Jensen & Cain 1960 model coefficients

      data    ((ggsfc(i,j),j=1,2),i=1,65) / -30401.2,0.0,-2163.8,5778.2,
     &          -1540.1,0.0,2997.9,-1932.0,1590.3,202.9,1307.1,0.0,
     &          -1988.9,-425.4,1276.8,227.8,881.2,-133.8,949.3,0.0,
     &          803.5,160.3,502.9,-274.3,-397.7,2.3,266.5,-246.6,-233.5,
     &          0.0,355.7,5.1,228.4,117.8,-28.8,-114.8,-157.9,-108.9,
     &          -62.2,82.4,49.2,0.0,57.5,-12.1,-0.8,104.4,-238.3,56.6,
     &          -1.5,-23.4,-2.0,-14.8,-108.9,-13.3,72.2,0.0,-53.7,-53.7,
     &          7.9,-27.4,15.6,-8.1,-24.3,7.0,-3.6,24.3,15.5,-22.5,3.6,
     &          -21.4,8.5,0.0,6.5,5.4,-9.3,-11.7,-9.6,4.2,-6.1,-15.3,
     &          5.5,4.6,-8.1,21.9,13.0,-0.7,7.4,-17.1,10.4,0.0,5.8,
     &          -22.4,7.5,13.8,-15.1,6.3,12.1,-3.0,4.7,-1.9,0.2,9.0,1.6,
     &          11.5,0.9,0.1,0.2,-1.5,-2.9,0.0,-0.9,-0.1,-2.2,4.5,0.8,
     &          -1.0,-2.8,2.6,6.4,-4.4,4.7,-1.3,-0.2,-3.6,1.8,4.0,2.0,
     &          1.0,1.1,-2.0 /       ! GSFC 12/66

      data    ((ggsfc1(i,j),j=1,2),i=1,65) / 14.03,0.00,8.76,-3.71,
     &          -23.29,0.00,-0.09,-14.31,-4.56,-16.62,-0.93,0.00,-10.62,
     &          5.20,2.31,2.53,-5.89,-6.98,1.45,0.00,0.90,-2.19,-1.75,
     &          -0.14,0.66,1.88,-3.01,-6.52,1.61,0.00,0.60,2.24,3.34,
     &          1.59,-0.04,-2.61,-0.60,0.50,1.76,-0.12,-0.42,0.00,0.82,
     &          0.05,0.82,0.09,2.35,2.55,0.83,-1.19,0.01,0.33,0.23,0.84,
     &          -0.57,0.00,-0.34,-0.96,-1.44,0.01,-0.90,0.43,0.03,0.75,
     &          -0.60,-0.33,-0.17,0.49,-0.64,0.90,0.35,0.00,0.50,-0.50,
     &          1.70,-0.21,-0.11,0.03,0.34,-0.79,-0.07,0.05,0.43,0.10,
     &          -0.15,-0.36,-0.42,-0.43,-0.10,0.00,-0.13,0.66,-1.20,
     &          0.54,0.08,0.03,-0.08,0.35,-0.39,-0.03,-0.36,-0.01,0.47,
     &          0.45,0.37,-0.05,-0.46,0.75,-0.01,0.00,-0.13,-0.61,0.88,
     &          -0.64,-0.18,0.02,0.17,0.05,-0.02,-0.63,0.05,-0.07,0.17,
     &          0.07,0.16,-0.03,0.31,-0.02,-0.23,-0.45 /
                                     ! GSFC 12/66 first order time variation

      data    ((ggsfc2(i,j),j=1,2),i=1,65) / -0.062,0.000,0.114,-0.043,
     &          -0.154,0.000,-0.018,0.054,-0.253,-0.016,-0.123,0.000,
     &          -0.027,0.095,0.028,-0.007,-0.183,0.079,0.001,0.000,
     &          -0.044,0.004,0.017,0.056,0.007,-0.035,-0.097,-0.047,
     &          0.045,0.000,0.001,-0.046,0.075,0.007,0.008,-0.007,0.015,
     &          0.001,0.056,-0.024,-0.006,0.000,0.015,0.020,0.010,
     &          -0.011,0.050,0.015,-0.011,-0.029,0.026,0.029,0.023,
     &          -0.010,-0.014,0.000,-0.006,-0.014,-0.034,0.016,-0.004,
     &          0.014,-0.006,0.005,-0.027,-0.008,-0.001,0.016,-0.004,
     &          0.011,0.006,0.000,0.008,-0.015,0.039,-0.012,-0.008,
     &          0.005,0.015,-0.011,-0.002,0.000,0.005,-0.003,-0.008,
     &          -0.009,-0.007,-0.003,-0.005,0.000,-0.001,0.022,-0.027,
     &          0.007,0.005,-0.002,-0.007,0.009,-0.006,0.006,-0.009,
     &          -0.001,0.006,0.009,0.005,-0.004,-0.009,0.019,-0.003,
     &          0.000,-0.003,-0.012,0.020,-0.014,-0.008,0.001,0.007,
     &          0.001,0.001,-0.011,0.001,-0.001,0.001,0.001,0.005,
     &          -0.001,0.004,0.001,-0.002,-0.006 /
                                     ! GSFC 12/66 second order time variation

      RAD = 45.0D0 / DATAN(1.0D0)
C***********************************************************************
C INITIALIZATION BLOCK, ONLY PASSED DURING FIRST CALL OF THE ROUTINE
C***********************************************************************
      IF((SHMIT(1,1) .NE. -1.0D0) .AND. (MODEL .NE. 1)) THEN
        SHMIT(1,1) = -1.0D0
        DO N=2,13
          SHMIT(N,1) = (2*N-3) * SHMIT(N-1,1) / (N-1)
          JJ = 2
          DO M=2,N
            SHMIT(N,M) = SHMIT(N,M-1)*DSQRT(DBLE((N-M+1)*JJ)/(N+M-2))
            SHMIT(M-1,N) = SHMIT(N,M)
            JJ = 1
          END DO
        END DO
      END IF

      IF (MODEL .EQ. 1) THEN
C**********************************************************************
C INTERNAL READ OF JENSEN & CAIN 1960 MODEL COEFFICIENTS
C**********************************************************************
        TZERO = 1960.0D0
        MODLBL = LABJC
        NMAX = 7
        NM = 1
        DO N=2,NMAX
          G(N,1) = GJC(NM,1)
          DO M=2,N
            G(N,M) = GJC(NM+M-1,1)
            G(M-1,N) = GJC(NM+M-1,2)
          END DO
          NM = NM+N
        END DO
      ELSE IF (MODEL .EQ. 2) THEN
C***********************************************************************
C INTERNAL READ OF GSFC 12/66 MODEL COEFFICIENTS
C***********************************************************************
        NMAX = 11
        TZERO = 1960.0D0
        MODLBL = LABGS
        NM = 1
        DO N=2,NMAX
          GG(N,1) = GGSFC(NM,1)
          GGT(N,1) = GGSFC1(NM,1)
          GGTT(N,1) = GGSFC2(NM,1)
          DO M=2,N
            GG(N,M) = GGSFC(NM+M-1,1)
            GG(M-1,N) = GGSFC(NM+M-1,2)
            GGT(N,M) = GGSFC1(NM+M-1,1)
            GGT(M-1,N) = GGSFC1(NM+M-1,2)
            GGTT(N,M) = GGSFC2(NM+M-1,1)
            GGTT(M-1,N) = GGSFC2(NM+M-1,2)
          END DO
          NM = NM + N
        END DO
      ELSE
C**********************************************************************
C READ IN COEFFICIENTS FOR DGRF/IGRF MODEL.
C**********************************************************************
        GG(1,1) = 0.0D0
        GGT(1,1) = 0.0D0
        GGTT(1,1) = 0.0D0
        DO I=1,NMOD
          IF (T0(I) .GT. TM) GO TO 1
        END DO
  1     I = I - 1
        NMAX = NMAXBD(I) + 1
        TZERO = T0(I)
        MODLBL = LBL(I)
C        WRITE(*,*) 'Model chosen: ',MODLBL
        DO M=1,NMAX
          DO N=1,NMAX
            GG(M,N) = GGBD(I,M,N)
            GGT(M,N) = GGTBD(I,M,N)
            GGTT(M,N) = 0.0D0
          END DO
        END DO
      END IF
C**********************************************************************
C UPDATE COEFFICIENTS TO NEW EPOCH AND APPLY SCHMIDT NORMALISATION
C**********************************************************************
      IF (MODEL .NE. 1) THEN
        T = TM - TZERO
        DO N=1,NMAX
          DO M=1,NMAX
            G(N,M) = (GG(N,M)+T*GGT(N,M)+T*T*GGTT(N,M)) * SHMIT(N,M)
          END DO
        END DO
      END IF

      ORDER = NMAX
      ELONG = RAD * DATAN(G(1,2)/G(2,2))
      COLAT = RAD * DATAN(DSQRT(G(1,2)**2+G(2,2)**2)/G(2,1))
      IF (MMOFLG .EQ. 0) THEN
C  old standard geomagnetic dipole moment (used by McIllwain etc.):
        GMAGMO = 0.311653D0
      ELSE
C  compute the updated moment (dipole represented by 1st 3 terms):
        GMAGMO = 1.0D-5 * DSQRT(G(2,1)**2 + G(1,2)**2 + G(2,2)**2)
      END IF

      RETURN
      END

C***********************************************************************
C*  Institute:     MPE               *                *                *
C*                                   *                *   SUBROUTINE   *
C*  DDDDDD         AA       LLL      *       GRO      *     P4IGRF     *
C*  DDDDDDD      AA  AA     LLL      *     COMPTEL    *                *
C*  DD    DD    AA    AA    LLL      *                * First Version  *
C*  DD    DD    AA    AA    LLL      ******************                **
C*  DD    DD    AAAAAAAA    LLL      *                * Author:         *
C*  DD    DD    AAAAAAAA    LLL      *                * Georg           *
C*  DD    DD    AA    AA    LLL      *    COMPASS     * Weidenspointner *
C*  DD    DD    AA    AA    LLL      *                *                **
C*  DDDDDDD     AA    AA    LLLLLLLL *                * Date :         *
C*  DDDDDD      AA    AA    LLLLLLLL *                *  04-12-95      *
C*                                   *                *                *
C*  Data        Access      Layer    *                *                *
C***********************************************************************
C*                                                                     *
C*  Function: contains IGRF data                                       *
C*                                                                     *
C*  Note: code was provided by Kevin Bennett, ESTEC                    *
C*                                                                     *
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
C   Description: tbd
C***********************************************************************


      BLOCK DATA DGRFBD

      IMPLICIT NONE

      REAL*8 TZERO(15)
      REAL*4 GG(15,13,13), GGT(15,13,13)
      INTEGER*4 NMAX(15), J, K
      character(32) LABEL(15)

      COMMON /DGRF/ TZERO, LABEL, GG, GGT, NMAX

      DATA NMAX(1)   /10/
      DATA TZERO(1)  /1945.0/
      DATA LABEL(1)  /'DGRF 1945                       '/
      DATA ((GG(1,J,K),K=1,11),J=1,11)  /
     &        0.0,   5810.0,  -1702.0,   -499.0,    144.0,    -12.0,
     &        6.0,    -45.0,     12.0,    -27.0,      5.0,
     &   -30594.0,  -2285.0,    477.0,    186.0,   -276.0,     95.0,
     &      100.0,    -18.0,    -21.0,     17.0,      1.0,
     &    -1244.0,   2990.0,   1578.0,    -11.0,    -55.0,    -67.0,
     &       16.0,      2.0,    -12.0,     29.0,    -20.0,
     &     1282.0,  -1834.0,   1255.0,    913.0,   -178.0,   -119.0,
     &       -9.0,      6.0,     -7.0,     -9.0,     -1.0,
     &      944.0,    776.0,    544.0,   -421.0,    304.0,     82.0,
     &      -16.0,     28.0,      2.0,      4.0,     -6.0,
     &     -253.0,    346.0,    194.0,    -20.0,   -142.0,    -82.0,
     &      -39.0,    -17.0,     18.0,      9.0,      6.0,
     &       59.0,     57.0,      6.0,   -246.0,    -25.0,     21.0,
     &     -104.0,    -22.0,      3.0,      6.0,     -4.0,
     &       70.0,    -40.0,      0.0,      0.0,    -29.0,    -10.0,
     &       15.0,     29.0,    -11.0,      1.0,     -2.0,
     &       13.0,      7.0,     -8.0,     -5.0,      9.0,      7.0,
     &      -10.0,      7.0,      2.0,      8.0,      0.0,
     &        5.0,    -21.0,      1.0,    -11.0,      3.0,     16.0,
     &       -3.0,     -4.0,     -3.0,     -4.0,     -2.0,
     &       -3.0,     11.0,      1.0,      2.0,     -5.0,     -1.0,
     &        8.0,     -1.0,     -3.0,      5.0,     -2.0/
      DATA ((GGT(1,J,K),K=1,11),J=1,11)  /
     &        0.0,      1.0,    -21.6,      4.6,     -1.6,      3.0,
     &       -1.4,      2.0,     -1.4,      0.6,      1.6,
     &        8.0,      7.0,    -19.2,      4.0,     -0.4,      1.6,
     &       -0.2,      0.2,     -0.2,      0.4,     -0.6,
     &      -19.4,      1.6,     -0.4,     -7.0,      3.6,     -4.0,
     &        3.4,     -0.4,      2.4,     -3.4,      2.0,
     &        3.0,    -11.0,      3.8,     -3.4,     -6.4,     -0.6,
     &       -0.6,      0.8,     -2.8,      2.2,      0.6,
     &        2.0,      3.2,     -3.2,      2.6,     -0.2,     -0.4,
     &        0.8,      1.6,     -2.0,     -0.4,      0.6,
     &        2.6,      0.6,      3.4,      0.0,     -1.0,      1.2,
     &        1.8,     -0.2,     -0.2,     -0.2,      0.0,
     &       -1.0,      0.0,     -0.4,     -0.2,      1.8,     -1.8,
     &       -0.2,      1.2,     -1.4,      0.4,      0.2,
     &       -1.0,     -3.0,      0.4,      0.2,     -2.2,      0.6,
     &       -2.0,     -2.0,     -1.2,     -2.4,      1.6,
     &        1.8,      1.6,      0.8,      0.8,      0.4,      1.6,
     &       -0.6,     -0.4,     -0.6,     -3.0,      2.2,
     &       -0.4,      2.8,     -0.4,     -2.8,      1.4,     -2.2,
     &       -0.4,      0.4,      1.2,      2.4,      2.0,
     &       -1.0,     -1.4,     -0.4,      2.2,      0.2,      1.0,
     &        0.8,      0.8,      1.0,      1.0,      1.0/

      DATA NMAX(2)   /10/
      DATA TZERO(2)  /1950.0/
      DATA LABEL(2)  /'DGRF 1950                       '/
      DATA ((GG(2,J,K),K=1,11),J=1,11)  /
     &        0.0,   5815.0,  -1810.0,   -476.0,    136.0,      3.0,
     &       -1.0,    -35.0,      5.0,    -24.0,     13.0,
     &   -30554.0,  -2250.0,    381.0,    206.0,   -278.0,    103.0,
     &       99.0,    -17.0,    -22.0,     19.0,     -2.0,
     &    -1341.0,   2998.0,   1576.0,    -46.0,    -37.0,    -87.0,
     &       33.0,      0.0,      0.0,     12.0,    -10.0,
     &     1297.0,  -1889.0,   1274.0,    896.0,   -210.0,   -122.0,
     &      -12.0,     10.0,    -21.0,      2.0,      2.0,
     &      954.0,    792.0,    528.0,   -408.0,    303.0,     80.0,
     &      -12.0,     36.0,     -8.0,      2.0,     -3.0,
     &     -240.0,    349.0,    211.0,    -20.0,   -147.0,    -76.0,
     &      -30.0,    -18.0,     17.0,      8.0,      6.0,
     &       54.0,     57.0,      4.0,   -247.0,    -16.0,     12.0,
     &     -105.0,    -16.0,     -4.0,      8.0,     -3.0,
     &       65.0,    -55.0,      2.0,      1.0,    -40.0,     -7.0,
     &        5.0,     19.0,    -17.0,    -11.0,      6.0,
     &       22.0,     15.0,     -4.0,     -1.0,     11.0,     15.0,
     &      -13.0,      5.0,     -1.0,     -7.0,     11.0,
     &        3.0,     -7.0,     -1.0,    -25.0,     10.0,      5.0,
     &       -5.0,     -2.0,      3.0,      8.0,      8.0,
     &       -8.0,      4.0,     -1.0,     13.0,     -4.0,      4.0,
     &       12.0,      3.0,      2.0,     10.0,      3.0/
      DATA ((GGT(2,J,K),K=1,11),J=1,11)  /
     &        0.0,      1.0,    -17.6,      2.8,     -0.6,      2.4,
     &       -1.6,     -3.0,      1.0,      2.6,     -3.4,
     &       10.8,      7.0,    -18.0,      2.0,      0.8,      1.4,
     &       -0.6,     -1.4,      1.4,     -1.4,      0.4,
     &      -19.8,      1.0,      1.0,     -7.4,      2.8,     -2.2,
     &        3.0,     -0.8,      1.0,     -1.0,      0.4,
     &        1.0,    -11.0,      2.8,     -2.8,     -4.0,      0.2,
     &       -0.8,     -0.4,     -0.4,      0.8,     -0.8,
     &        0.8,      0.8,     -3.6,      2.2,     -2.6,     -0.4,
     &        0.0,     -1.6,      2.2,     -0.8,     -0.2,
     &        2.2,      2.2,      3.8,     -0.6,     -1.0,      1.4,
     &        1.2,     -0.4,      1.2,      0.4,     -1.0,
     &       -1.4,      0.0,     -0.2,      0.0,      1.6,     -1.0,
     &       -0.4,     -0.4,      0.0,     -0.2,      0.0,
     &        0.0,     -0.2,      0.0,      1.8,      1.6,     -0.8,
     &        0.8,     -0.2,      0.8,      1.0,      0.2,
     &       -2.2,     -1.2,     -0.4,     -2.6,     -1.0,     -1.0,
     &        1.2,      0.2,      2.0,      2.4,     -2.4,
     &        0.2,      3.2,     -0.6,      4.0,     -1.6,     -0.2,
     &        1.2,      0.8,     -0.2,     -0.6,     -2.2,
     &        1.0,     -1.8,      0.0,     -2.2,      0.2,      0.6,
     &       -1.6,     -1.0,      0.8,     -2.4,     -0.6/

      DATA NMAX(3)   /10/
      DATA TZERO(3)  /1955.0/
      DATA LABEL(3)  /'DGRF 1955                       '/
      DATA ((GG(3,J,K),K=1,11),J=1,11)  /
     &        0.0,   5820.0,  -1898.0,   -462.0,    133.0,     15.0,
     &       -9.0,    -50.0,     10.0,    -11.0,     -4.0,
     &   -30500.0,  -2215.0,    291.0,    216.0,   -274.0,    110.0,
     &       96.0,    -24.0,    -15.0,     12.0,      0.0,
     &    -1440.0,   3003.0,   1581.0,    -83.0,    -23.0,    -98.0,
     &       48.0,     -4.0,      5.0,      7.0,     -8.0,
     &     1302.0,  -1944.0,   1288.0,    882.0,   -230.0,   -121.0,
     &      -16.0,      8.0,    -23.0,      6.0,     -2.0,
     &      958.0,    796.0,    510.0,   -397.0,    290.0,     78.0,
     &      -12.0,     28.0,      3.0,     -2.0,     -4.0,
     &     -229.0,    360.0,    230.0,    -23.0,   -152.0,    -69.0,
     &      -24.0,    -20.0,     23.0,     10.0,      1.0,
     &       47.0,     57.0,      3.0,   -247.0,     -8.0,      7.0,
     &     -107.0,    -18.0,     -4.0,      7.0,     -3.0,
     &       65.0,    -56.0,      2.0,     10.0,    -32.0,    -11.0,
     &        9.0,     18.0,    -13.0,     -6.0,      7.0,
     &       11.0,      9.0,     -6.0,    -14.0,      6.0,     10.0,
     &       -7.0,      6.0,      9.0,      5.0,     -1.0,
     &        4.0,      9.0,     -4.0,     -5.0,      2.0,      4.0,
     &        1.0,      2.0,      2.0,      5.0,     -3.0,
     &       -3.0,     -5.0,     -1.0,      2.0,     -3.0,      7.0,
     &        4.0,     -2.0,      6.0,     -2.0,      0.0/
      DATA ((GGT(3,J,K),K=1,11),J=1,11)  /
     &        0.0,     -5.8,    -13.8,      9.6,      0.4,      0.2,
     &       -0.2,     -1.0,      0.2,     -1.4,      1.6,
     &       15.8,      9.2,    -17.0,      1.6,     -0.8,      3.0,
     &        0.6,     -0.8,      0.2,      0.0,      0.2,
     &      -23.0,     -0.2,      1.8,     -9.4,      5.2,     -3.8,
     &        2.4,     -0.4,      0.4,     -1.0,      1.6,
     &        0.0,     -9.6,      0.2,     -0.8,     -5.0,      1.4,
     &       -0.8,     -0.2,      1.0,     -1.2,      0.8,
     &       -0.2,      0.8,     -1.2,      0.6,     -4.2,      0.6,
     &        0.2,     -1.0,      0.2,     -0.2,     -0.2,
     &        1.4,      0.4,      2.4,     -0.6,     -0.8,      1.2,
     &        1.4,      0.4,      0.0,     -0.2,      0.0,
     &       -0.2,      0.2,     -0.4,      2.0,      1.4,     -1.8,
     &       -1.2,      0.2,      1.0,      0.2,      0.4,
     &        0.4,      0.0,      0.6,      1.0,      0.0,      0.8,
     &        1.6,     -2.0,     -1.4,      1.2,     -0.2,
     &        0.8,     -0.6,      0.4,      0.6,     -0.8,      0.0,
     &        0.4,      0.8,     -0.2,      0.0,      0.2,
     &        0.0,     -0.6,      0.8,     -0.8,     -0.2,      0.0,
     &       -0.4,     -0.8,      0.2,     -1.2,     -0.8,
     &        0.8,      0.4,      1.0,     -0.4,      0.4,     -0.6,
     &        0.4,      0.6,     -1.4,      0.8,      0.0/

      DATA NMAX(4)   /10/
      DATA TZERO(4)  /1960.0/
      DATA LABEL(4)  /'DGRF 1960                       '/
      DATA ((GG(4,J,K),K=1,11),J=1,11)  /
     &        0.0,   5791.0,  -1967.0,   -414.0,    135.0,     16.0,
     &      -10.0,    -55.0,     11.0,    -18.0,      4.0,
     &   -30421.0,  -2169.0,    206.0,    224.0,   -278.0,    125.0,
     &       99.0,    -28.0,    -14.0,     12.0,      1.0,
     &    -1555.0,   3002.0,   1590.0,   -130.0,      3.0,   -117.0,
     &       60.0,     -6.0,      7.0,      2.0,      0.0,
     &     1302.0,  -1992.0,   1289.0,    878.0,   -255.0,   -114.0,
     &      -20.0,      7.0,    -18.0,      0.0,      2.0,
     &      957.0,    800.0,    504.0,   -394.0,    269.0,     81.0,
     &      -11.0,     23.0,      4.0,     -3.0,     -5.0,
     &     -222.0,    362.0,    242.0,    -26.0,   -156.0,    -63.0,
     &      -17.0,    -18.0,     23.0,      9.0,      1.0,
     &       46.0,     58.0,      1.0,   -237.0,     -1.0,     -2.0,
     &     -113.0,    -17.0,      1.0,      8.0,     -1.0,
     &       67.0,    -56.0,      5.0,     15.0,    -32.0,     -7.0,
     &       17.0,      8.0,    -20.0,      0.0,      6.0,
     &       15.0,      6.0,     -4.0,    -11.0,      2.0,     10.0,
     &       -5.0,     10.0,      8.0,      5.0,      0.0,
     &        4.0,      6.0,      0.0,     -9.0,      1.0,      4.0,
     &       -1.0,     -2.0,      3.0,     -1.0,     -7.0,
     &        1.0,     -3.0,      4.0,      0.0,     -1.0,      4.0,
     &        6.0,      1.0,     -1.0,      2.0,      0.0/
      DATA ((GGT(4,J,K),K=1,11),J=1,11)  /
     &        0.0,     -3.0,     -9.8,      2.0,      2.6,      0.6,
     &       -0.2,     -1.2,     -0.8,     -0.8,     -0.4,
     &       17.4,     10.0,    -18.4,      3.2,      1.8,      0.6,
     &        0.2,      0.2,      0.4,      0.6,      0.0,
     &      -21.4,     -1.0,      0.8,     -7.0,      2.0,     -1.8,
     &        1.6,      0.8,      0.4,      1.0,      0.4,
     &       -1.0,     -9.2,      0.6,     -4.4,     -2.8,      3.4,
     &       -2.4,     -0.2,      0.4,     -0.8,      0.8,
     &        0.0,      0.8,     -5.0,      0.8,     -3.4,      0.0,
     &        0.6,      0.6,      0.0,     -0.4,      0.2,
     &        0.6,     -0.8,      2.4,     -1.0,     -0.2,      0.2,
     &        2.0,     -1.0,      0.2,      0.2,     -0.2,
     &       -0.2,      0.6,      1.4,      1.8,      1.0,      0.6,
     &        0.4,      1.0,     -0.8,      0.4,     -0.2,
     &        1.6,     -0.2,     -0.2,     -0.4,      1.2,      0.2,
     &       -0.8,     -1.4,      0.6,     -0.8,     -0.6,
     &       -0.4,     -0.2,      0.0,     -0.6,     -0.4,     -0.4,
     &        0.8,      0.2,     -0.8,     -0.8,      0.0,
     &        0.8,      0.8,      0.4,     -0.8,      1.8,     -1.0,
     &        0.0,      1.4,     -0.4,     -0.2,      0.2,
     &       -0.6,      0.0,     -0.4,     -1.0,     -0.2,      0.0,
     &       -0.4,     -0.2,      0.6,      0.0,      0.0/

      DATA NMAX(5)   /10/
      DATA TZERO(5)  /1965.0/
      DATA LABEL(5)  /'DGRF 1965                       '/
      DATA ((GG(5,J,K),K=1,11),J=1,11)  /
     &        0.0,   5776.0,  -2016.0,   -404.0,    148.0,     19.0,
     &      -11.0,    -61.0,      7.0,    -22.0,      2.0,
     &   -30334.0,  -2119.0,    114.0,    240.0,   -269.0,    128.0,
     &      100.0,    -27.0,    -12.0,     15.0,      1.0,
     &    -1662.0,   2997.0,   1594.0,   -165.0,     13.0,   -126.0,
     &       68.0,     -2.0,      9.0,      7.0,      2.0,
     &     1297.0,  -2038.0,   1292.0,    856.0,   -269.0,    -97.0,
     &      -32.0,      6.0,    -16.0,     -4.0,      6.0,
     &      957.0,    804.0,    479.0,   -390.0,    252.0,     81.0,
     &       -8.0,     26.0,      4.0,     -5.0,     -4.0,
     &     -219.0,    358.0,    254.0,    -31.0,   -157.0,    -62.0,
     &       -7.0,    -23.0,     24.0,     10.0,      0.0,
     &       45.0,     61.0,      8.0,   -228.0,      4.0,      1.0,
     &     -111.0,    -12.0,     -3.0,     10.0,     -2.0,
     &       75.0,    -57.0,      4.0,     13.0,    -26.0,     -6.0,
     &       13.0,      1.0,    -17.0,     -4.0,      3.0,
     &       13.0,      5.0,     -4.0,    -14.0,      0.0,      8.0,
     &       -1.0,     11.0,      4.0,      1.0,      0.0,
     &        8.0,     10.0,      2.0,    -13.0,     10.0,     -1.0,
     &       -1.0,      5.0,      1.0,     -2.0,     -6.0,
     &       -2.0,     -3.0,      2.0,     -5.0,     -2.0,      4.0,
     &        4.0,      0.0,      2.0,      2.0,      0.0/
      DATA ((GGT(5,J,K),K=1,11),J=1,11)  /
     &        0.0,     -7.8,     -6.2,      7.6,      3.8,      1.4,
     &       -0.2,     -1.8,      0.0,      0.2,     -0.2,
     &       22.8,     10.2,    -17.8,      2.2,      0.6,      2.2,
     &        0.0,      0.0,     -0.6,      0.2,      0.0,
     &      -23.8,      0.6,      3.4,     -6.2,      2.6,     -2.6,
     &        0.8,     -0.4,     -0.6,     -0.2,      0.2,
     &       -2.0,    -10.6,     -2.8,     -3.6,     -2.0,      1.2,
     &       -1.0,      0.4,     -0.2,      0.0,     -0.4,
     &       -1.0,     -0.8,     -3.6,     -1.0,     -3.6,      0.4,
     &        0.4,     -0.6,      0.4,      0.0,      0.0,
     &        0.6,      0.2,      1.6,     -2.2,     -0.6,      1.2,
     &        1.6,      0.0,     -0.6,      0.0,      0.0,
     &       -0.4,      0.6,      1.4,      3.2,     -0.4,      0.4,
     &       -0.2,      0.2,     -0.6,      0.2,      0.2,
     &       -0.6,      0.0,     -0.6,      0.2,      0.8,      0.8,
     &        0.0,     -0.6,      0.2,      0.4,      0.0,
     &        0.2,      0.2,      0.4,      0.2,     -0.6,     -0.6,
     &        0.2,      0.0,     -0.2,      0.0,      0.2,
     &        0.0,      0.0,      0.0,      0.2,      0.0,      0.0,
     &        0.2,     -0.4,      0.0,      0.2,      0.4,
     &       -0.2,      0.0,      0.0,      0.0,      0.2,      0.4,
     &        0.0,      0.2,     -0.4,      0.2,     -0.2/

      DATA NMAX(6)   /10/
      DATA TZERO(6)  /1970.0/
      DATA LABEL(6)  /'DGRF 1970                       '/
      DATA ((GG(6,J,K),K=1,11),J=1,11)  /
     &        0.0,   5737.0,  -2047.0,   -366.0,    167.0,     26.0,
     &      -12.0,    -70.0,      7.0,    -21.0,      1.0,
     &   -30220.0,  -2068.0,     25.0,    251.0,   -266.0,    139.0,
     &      100.0,    -27.0,    -15.0,     16.0,      1.0,
     &    -1781.0,   3000.0,   1611.0,   -196.0,     26.0,   -139.0,
     &       72.0,     -4.0,      6.0,      6.0,      3.0,
     &     1287.0,  -2091.0,   1278.0,    838.0,   -279.0,    -91.0,
     &      -37.0,      8.0,    -17.0,     -4.0,      4.0,
     &      952.0,    800.0,    461.0,   -395.0,    234.0,     83.0,
     &       -6.0,     23.0,      6.0,     -5.0,     -4.0,
     &     -216.0,    359.0,    262.0,    -42.0,   -160.0,    -56.0,
     &        1.0,    -23.0,     21.0,     10.0,      0.0,
     &       43.0,     64.0,     15.0,   -212.0,      2.0,      3.0,
     &     -112.0,    -11.0,     -6.0,     11.0,     -1.0,
     &       72.0,    -57.0,      1.0,     14.0,    -22.0,     -2.0,
     &       13.0,     -2.0,    -16.0,     -2.0,      3.0,
     &       14.0,      6.0,     -2.0,    -13.0,     -3.0,      5.0,
     &        0.0,     11.0,      3.0,      1.0,      1.0,
     &        8.0,     10.0,      2.0,    -12.0,     10.0,     -1.0,
     &        0.0,      3.0,      1.0,     -1.0,     -4.0,
     &       -3.0,     -3.0,      2.0,     -5.0,     -1.0,      6.0,
     &        4.0,      1.0,      0.0,      3.0,     -1.0/
      DATA ((GGT(6,J,K),K=1,11),J=1,11)  /
     &        0.0,    -12.4,     -4.0,      6.6,      4.8,      1.0,
     &       -0.2,     -1.4,     -0.2,      0.0,      0.0,
     &       24.0,     11.0,    -18.6,      2.2,      0.2,      1.8,
     &       -0.2,      0.2,     -0.2,      0.0,      0.0,
     &      -24.2,      2.0,      4.2,     -5.4,      2.6,     -2.6,
     &        0.6,     -0.2,     -0.4,      0.2,      0.0,
     &       -2.2,    -10.6,     -3.6,     -1.6,     -1.8,      1.6,
     &       -0.8,      0.4,     -0.4,      0.0,      0.0,
     &       -1.2,     -1.8,     -4.6,     -2.0,     -3.6,      1.0,
     &        0.4,     -0.2,      0.0,      0.0,      0.0,
     &       -0.4,     -0.6,      0.4,     -3.4,      0.2,      1.4,
     &        2.0,      0.0,     -0.6,      0.0,     -0.2,
     &        0.4,      0.4,      2.6,      2.8,     -0.2,      0.6,
     &        0.2,     -0.2,     -0.8,      0.0,      0.0,
     &       -0.2,      0.2,      0.0,      0.4,      1.6,      0.4,
     &       -0.2,     -0.6,     -0.2,     -0.2,      0.0,
     &        0.0,      0.0,      0.2,      0.2,     -1.0,     -0.2,
     &        0.0,     -0.2,     -0.4,      0.0,      0.0,
     &       -0.2,      0.0,      0.0,      0.0,      0.0,      0.0,
     &       -0.2,      0.2,      0.0,     -0.2,     -0.2,
     &        0.0,      0.0,      0.0,      0.0,     -0.2,     -0.2,
     &        0.0,      0.0,      0.0,      0.0,      0.0/

      DATA NMAX(7)   /10/
      DATA TZERO(7)  /1975.0/
      DATA LABEL(7)  /'DGRF 1975                       '/
      DATA ((GG(7,J,K),K=1,11),J=1,11)  /
     &        0.0,   5675.0,  -2067.0,   -333.0,    191.0,     31.0,
     &      -13.0,    -77.0,      6.0,    -21.0,      1.0,
     &   -30100.0,  -2013.0,    -68.0,    262.0,   -265.0,    148.0,
     &       99.0,    -26.0,    -16.0,     16.0,      1.0,
     &    -1902.0,   3010.0,   1632.0,   -223.0,     39.0,   -152.0,
     &       75.0,     -5.0,      4.0,      7.0,      3.0,
     &     1276.0,  -2144.0,   1260.0,    830.0,   -288.0,    -83.0,
     &      -41.0,     10.0,    -19.0,     -4.0,      4.0,
     &      946.0,    791.0,    438.0,   -405.0,    216.0,     88.0,
     &       -4.0,     22.0,      6.0,     -5.0,     -4.0,
     &     -218.0,    356.0,    264.0,    -59.0,   -159.0,    -49.0,
     &       11.0,    -23.0,     18.0,     10.0,     -1.0,
     &       45.0,     66.0,     28.0,   -198.0,      1.0,      6.0,
     &     -111.0,    -12.0,    -10.0,     11.0,     -1.0,
     &       71.0,    -56.0,      1.0,     16.0,    -14.0,      0.0,
     &       12.0,     -5.0,    -17.0,     -3.0,      3.0,
     &       14.0,      6.0,     -1.0,    -12.0,     -8.0,      4.0,
     &        0.0,     10.0,      1.0,      1.0,      1.0,
     &        7.0,     10.0,      2.0,    -12.0,     10.0,     -1.0,
     &       -1.0,      4.0,      1.0,     -2.0,     -5.0,
     &       -3.0,     -3.0,      2.0,     -5.0,     -2.0,      5.0,
     &        4.0,      1.0,      0.0,      3.0,     -1.0/
      DATA ((GGT(7,J,K),K=1,11),J=1,11)  /
     &        0.0,    -14.2,    -12.4,     -0.6,      4.2,      3.0,
     &       -0.4,     -1.0,      0.2,      0.0,      0.0,
     &       21.6,     11.4,    -26.4,      1.8,      1.6,      0.4,
     &       -1.2,     -0.2,     -0.4,      0.0,     -0.2,
     &      -19.0,      3.4,      6.2,     -5.8,      2.8,      0.2,
     &       -0.8,      0.0,      0.0,      0.4,      0.0,
     &        1.0,     -7.2,     -1.8,      0.6,     -1.8,      1.0,
     &       -0.4,      1.2,     -0.6,     -0.2,      0.4,
     &       -1.6,     -1.8,     -8.0,     -2.8,     -3.4,      0.8,
     &        0.4,     -0.8,      0.6,     -0.2,      0.0,
     &        0.0,      0.2,     -0.6,     -3.0,     -0.6,      0.2,
     &        1.2,      0.0,     -0.4,     -0.2,      0.2,
     &        0.6,      0.0,      2.8,      1.2,      0.6,      1.6,
     &        0.6,      0.4,     -0.6,     -0.2,      0.0,
     &        0.2,     -0.6,      0.2,      1.0,      0.4,      0.2,
     &       -0.2,      0.6,      0.4,     -0.6,      0.2,
     &        0.8,      0.0,      0.2,      0.2,      0.2,      0.0,
     &        0.6,     -0.8,     -0.4,      0.2,     -0.2,
     &       -0.4,      0.0,     -0.2,      0.0,     -0.2,     -0.4,
     &        0.0,      0.6,      0.2,     -0.6,     -0.2,
     &       -0.2,     -0.2,      0.0,      0.0,      0.0,      0.0,
     &       -0.2,      0.0,      0.4,      0.0,      0.2/

      DATA NMAX(8)   /10/
      DATA TZERO(8)  /1980.0/
      DATA LABEL(8)  /'DGRF 1980                       '/
      DATA ((GG(8,J,K),K=1,11),J=1,11)  /
     &        0.0,   5604.0,  -2129.0,   -336.0,    212.0,     46.0,
     &      -15.0,    -82.0,      7.0,    -21.0,      1.0,
     &   -29992.0,  -1956.0,   -200.0,    271.0,   -257.0,    150.0,
     &       93.0,    -27.0,    -18.0,     16.0,      0.0,
     &    -1997.0,   3027.0,   1663.0,   -252.0,     53.0,   -151.0,
     &       71.0,     -5.0,      4.0,      9.0,      3.0,
     &     1281.0,  -2180.0,   1251.0,    833.0,   -297.0,    -78.0,
     &      -43.0,     16.0,    -22.0,     -5.0,      6.0,
     &      938.0,    782.0,    398.0,   -419.0,    199.0,     92.0,
     &       -2.0,     18.0,      9.0,     -6.0,     -4.0,
     &     -218.0,    357.0,    261.0,    -74.0,   -162.0,    -48.0,
     &       17.0,    -23.0,     16.0,      9.0,      0.0,
     &       48.0,     66.0,     42.0,   -192.0,      4.0,     14.0,
     &     -108.0,    -10.0,    -13.0,     10.0,     -1.0,
     &       72.0,    -59.0,      2.0,     21.0,    -12.0,      1.0,
     &       11.0,     -2.0,    -15.0,     -6.0,      4.0,
     &       18.0,      6.0,      0.0,    -11.0,     -7.0,      4.0,
     &        3.0,      6.0,     -1.0,      2.0,      0.0,
     &        5.0,     10.0,      1.0,    -12.0,      9.0,     -3.0,
     &       -1.0,      7.0,      2.0,     -5.0,     -6.0,
     &       -4.0,     -4.0,      2.0,     -5.0,     -2.0,      5.0,
     &        3.0,      1.0,      2.0,      3.0,      0.0/
      DATA ((GGT(8,J,K),K=1,11),J=1,11)  /
     &        0.0,    -20.8,    -13.6,      5.2,      4.0,      0.2,
     &       -0.2,     -0.2,      0.2,      0.0,      0.0,
     &       23.8,     10.2,    -21.2,      2.6,      1.6,      0.0,
     &       -1.0,      0.0,     -0.2,     -0.2,      0.0,
     &      -15.0,      3.4,      4.8,     -9.0,      3.2,     -0.6,
     &       -0.4,      0.6,      0.2,      0.0,      0.0,
     &        3.0,     -5.6,     -0.8,     -0.8,      0.0,      0.6,
     &       -1.0,      0.8,     -0.2,     -0.2,      0.0,
     &       -0.4,     -0.4,     -7.4,     -1.0,     -5.8,      0.6,
     &        0.2,     -0.2,      0.4,      0.0,      0.0,
     &        0.8,     -0.4,     -1.6,     -3.8,     -0.4,      0.4,
     &        0.8,      0.0,     -0.4,      0.0,      0.0,
     &        1.0,     -0.2,      1.8,      1.4,      0.0,      0.4,
     &        1.2,      0.6,     -0.4,     -0.2,      0.0,
     &        0.4,     -0.6,      0.2,      0.6,      1.2,      0.6,
     &       -0.2,      0.4,      0.8,     -0.2,      0.0,
     &        0.6,      0.0,      0.0,      0.0,     -0.4,      0.0,
     &        0.2,     -0.4,     -0.6,      0.0,      0.0,
     &        0.0,      0.0,      0.0,      0.0,      0.0,      0.0,
     &        0.0,      0.0,     -0.2,      0.0,      0.0,
     &        0.0,      0.0,      0.2,      0.0,      0.0,      0.0,
     &        0.0,      0.0,      0.0,      0.0,      0.0/

      DATA NMAX(9)   /10/
      DATA TZERO(9)  /1985.0/
      DATA LABEL(9)  /'DGRF 1985                       '/
      DATA ((GG(9,J,K),K=1,11),J=1,11)  /
     &        0.0,   5500.0,  -2197.0,   -310.0,    232.0,     47.0,
     &      -16.0,    -83.0,      8.0,    -21.0,      1.0,
     &   -29873.0,  -1905.0,   -306.0,    284.0,   -249.0,    150.0,
     &       88.0,    -27.0,    -19.0,     15.0,      0.0,
     &    -2072.0,   3044.0,   1687.0,   -297.0,     69.0,   -154.0,
     &       69.0,     -2.0,      5.0,      9.0,      3.0,
     &     1296.0,  -2208.0,   1247.0,    829.0,   -297.0,    -75.0,
     &      -48.0,     20.0,    -23.0,     -6.0,      6.0,
     &      936.0,    780.0,    361.0,   -424.0,    170.0,     95.0,
     &       -1.0,     17.0,     11.0,     -6.0,     -4.0,
     &     -214.0,    355.0,    253.0,    -93.0,   -164.0,    -46.0,
     &       21.0,    -23.0,     14.0,      9.0,      0.0,
     &       53.0,     65.0,     51.0,   -185.0,      4.0,     16.0,
     &     -102.0,     -7.0,    -15.0,      9.0,     -1.0,
     &       74.0,    -62.0,      3.0,     24.0,     -6.0,      4.0,
     &       10.0,      0.0,    -11.0,     -7.0,      4.0,
     &       21.0,      6.0,      0.0,    -11.0,     -9.0,      4.0,
     &        4.0,      4.0,     -4.0,      2.0,      0.0,
     &        5.0,     10.0,      1.0,    -12.0,      9.0,     -3.0,
     &       -1.0,      7.0,      1.0,     -5.0,     -6.0,
     &       -4.0,     -4.0,      3.0,     -5.0,     -2.0,      5.0,
     &        3.0,      1.0,      2.0,      3.0,      0.0/
      DATA ((GGT(9,J,K),K=1,11),J=1,11)  /
     &        0.0,    -18.8,    -16.4,      5.2,      3.0,     -0.2,
     &        0.0,      0.6,      0.4,      0.2,      0.2,
     &       19.6,     11.4,    -13.4,      1.8,      1.8,      0.8,
     &       -1.2,      0.2,      0.0,      0.0,      0.2,
     &      -11.8,      3.0,     -0.2,    -11.0,      3.0,      0.2,
     &        0.0,      0.4,      0.2,      0.4,      0.0,
     &        3.6,     -6.2,      0.2,     -5.4,     -0.4,      1.2,
     &       -0.8,      0.2,      0.2,     -0.2,      0.0,
     &        0.6,      0.0,     -7.2,      0.2,     -5.8,      0.4,
     &        0.4,      0.0,      0.2,     -0.2,      0.0,
     &        0.0,     -0.4,     -1.6,     -3.2,     -0.2,      2.0,
     &        0.6,      0.0,     -0.4,      0.0,      0.0,
     &        1.6,      0.0,      1.6,      1.4,     -0.2,      0.4,
     &        1.2,      0.6,     -0.2,     -0.2,     -0.2,
     &        0.6,     -0.4,     -0.2,      0.4,      1.0,      0.2,
     &       -0.2,      0.0,      0.2,      0.0,     -0.2,
     &        0.4,     -0.2,     -0.2,      0.2,     -0.6,     -0.2,
     &        0.0,     -0.4,     -0.4,      0.0,     -0.2,
     &       -0.2,     -0.2,      0.0,      0.0,      0.0,     -0.2,
     &       -0.2,      0.0,      0.0,     -0.2,      0.0,
     &        0.2,      0.0,     -0.2,      0.0,      0.0,     -0.2,
     &        0.0,      0.0,      0.2,      0.0,      0.0/

      DATA NMAX(10)  /10/
      DATA TZERO(10) /1990.0/
      DATA LABEL(10) /'DGRF 1990                       '/
      DATA ((GG(10,J,K),K=1,11),J=1,11) /
     &        0.0,   5406.0,  -2279.0,   -284.0,    247.0,     46.0,
     &      -16.0,    -80.0,     10.0,    -20.0,      2.0,
     &   -29775.0,  -1848.0,   -373.0,    293.0,   -240.0,    154.0,
     &       82.0,    -26.0,    -19.0,     15.0,      1.0,
     &    -2131.0,   3059.0,   1686.0,   -352.0,     84.0,   -153.0,
     &       69.0,      0.0,      6.0,     11.0,      3.0,
     &     1314.0,  -2239.0,   1248.0,    802.0,   -299.0,    -69.0,
     &      -52.0,     21.0,    -22.0,     -7.0,      6.0,
     &      939.0,    780.0,    325.0,   -423.0,    141.0,     97.0,
     &        1.0,     17.0,     12.0,     -7.0,     -4.0,
     &     -214.0,    353.0,    245.0,   -109.0,   -165.0,    -36.0,
     &       24.0,    -23.0,     12.0,      9.0,      0.0,
     &       61.0,     65.0,     59.0,   -178.0,      3.0,     18.0,
     &      -96.0,     -4.0,    -16.0,      8.0,     -2.0,
     &       77.0,    -64.0,      2.0,     26.0,     -1.0,      5.0,
     &        9.0,      0.0,    -10.0,     -7.0,      3.0,
     &       23.0,      5.0,     -1.0,    -10.0,    -12.0,      3.0,
     &        4.0,      2.0,     -6.0,      2.0,     -1.0,
     &        4.0,      9.0,      1.0,    -12.0,      9.0,     -4.0,
     &       -2.0,      7.0,      1.0,     -6.0,     -6.0,
     &       -3.0,     -4.0,      2.0,     -5.0,     -2.0,      4.0,
     &        3.0,      1.0,      3.0,      3.0,      0.0/
      DATA ((GGT(10,J,K),K=1,11),J=1,11) /
     &        0.0,    -17.6,    -15.4,      4.2,      3.0,     -0.4,
     &        0.0,      0.6,      0.4,      0.2,      0.0,
     &       18.6,     11.8,    -10.4,      1.8,      1.6,      0.6,
     &       -1.0,      0.2,     -0.2,      0.0,      0.0,
     &      -13.2,      3.0,     -0.2,    -10.8,      2.8,      0.2,
     &       -0.4,      0.6,      0.2,      0.0,      0.0,
     &        3.0,     -5.8,      0.2,     -6.6,     -0.4,      1.0,
     &       -1.0,      0.2,      0.2,      0.0,      0.0,
     &        0.4,      0.4,     -6.8,      0.4,     -5.0,      0.4,
     &        0.6,     -0.2,      0.0,      0.0,      0.0,
     &        0.8,     -0.2,     -1.6,     -2.6,     -0.4,      2.0,
     &        0.8,      0.0,     -0.4,      0.0,      0.0,
     &        1.0,     -0.2,      1.2,      1.2,     -0.2,     -0.2,
     &        0.4,      0.2,      1.0,     -0.2,      0.0,
     &        0.2,     -0.6,     -0.2,      0.6,      1.0,      0.6,
     &        0.0,     -0.4,      0.0,     -0.2,      0.0,
     &        0.2,     -0.2,      0.0,      0.2,     -0.4,      0.2,
     &        0.2,     -0.4,     -0.2,     -0.2,      0.0,
     &        0.0,      0.0,      0.0,      0.0,      0.0,      0.0,
     &        0.0,      0.0,     -0.2,      0.0,      0.0,
     &        0.0,      0.0,      0.0,      0.0,      0.0,      0.0,
     &        0.0,      0.0,      0.0,      0.0,      0.0/


C IGRF -- Version 11 ----- April 30, 2010 ---  interpolated 1995-2000
      DATA NMAX(11)   /10/
      DATA TZERO(11)  /1995.0/
      DATA LABEL(11)  /'DGRF 1995                       '/
C     vv Coefficients at epoch vv
      DATA ((GG(11,J,K),K=1,11),J=1,11) /
     &        0.0,   5306.0,  -2366.0,   -262.0,    262.0,     46.0,
     &      -17.0,    -69.0,     11.0,    -20.0,      1.0,
     &   -29692.0,  -1784.0,   -413.0,    302.0,   -236.0,    165.0,
     &       72.0,    -25.0,    -21.0,     15.0,      0.0,
     &    -2200.0,   3070.0,   1681.0,   -427.0,     97.0,   -143.0,
     &       67.0,      4.0,      8.0,     12.0,      4.0,
     &     1335.0,  -2267.0,   1249.0,    759.0,   -306.0,    -55.0,
     &      -58.0,     24.0,    -23.0,     -6.0,      5.0,
     &      940.0,    780.0,    290.0,   -418.0,    122.0,    107.0,
     &        1.0,     17.0,     15.0,     -8.0,     -5.0,
     &     -214.0,    352.0,    235.0,   -118.0,   -166.0,    -17.0,
     &       36.0,    -24.0,     11.0,      8.0,     -1.0,
     &       68.0,     67.0,     68.0,   -170.0,     -1.0,     19.0,
     &      -93.0,     -6.0,    -16.0,      5.0,     -2.0,
     &       77.0,    -72.0,      1.0,     28.0,      5.0,      4.0,
     &        8.0,     -2.0,     -4.0,     -8.0,      1.0,
     &       25.0,      6.0,     -6.0,     -9.0,    -14.0,      9.0,
     &        6.0,     -5.0,     -7.0,      3.0,     -2.0,
     &        4.0,      9.0,      3.0,    -10.0,      8.0,     -8.0,
     &       -1.0,     10.0,     -2.0,     -8.0,     -7.0,
     &       -3.0,     -6.0,      2.0,     -4.0,     -1.0,      4.0,
     &        2.0,      2.0,      5.0,      1.0,      0.0/
C     vv Time derivative of coefficients vv
      DATA ((GGT(11,J,K),K=1,11),J=1,11) /
     &        0.0,    -24.0,    -23.1,      6.9,      2.1,     -0.4,
     &       -0.1,      0.9,      0.2,      0.1,      0.1,
     &       14.5,     11.2,     -9.0,     -1.7,      0.8,      1.4,
     &       -1.7,      0.2,     -0.1,     -0.3,      0.0,
     &      -13.5,     -0.3,     -2.0,    -12.8,      4.6,      2.0,
     &       -0.4,      0.4,      0.1,      0.1,      0.0,
     &        0.9,     -4.2,      0.6,     -8.9,      0.4,      3.1,
     &       -0.6,      0.0,      0.3,     -0.0,     -0.0,
     &       -1.5,      1.4,     -8.0,      3.0,     -2.1,     -0.1,
     &       -0.1,     -0.4,      0.1,     -0.1,     -0.2,
     &       -1.0,     -0.1,     -2.5,     -2.5,     -0.5,      0.8,
     &        1.6,     -0.3,     -0.4,      0.1,     -0.0,
     &        0.9,      0.2,      1.2,      1.8,     -1.0,     -0.4,
     &        0.5,      0.0,      0.2,     -0.2,     -0.2,
     &        0.4,     -0.4,     -0.2,      1.1,      0.8,      0.6,
     &       -0.1,      0.2,      0.4,     -0.0,     -0.2,
     &       -0.1,      0.1,     -0.6,      0.2,     -0.5,      0.0,
     &        0.2,     -0.6,      0.0,      0.4,     -0.0,
     &        0.2,      0.1,      0.0,      0.3,     -0.3,     -0.2,
     &       -0.1,     -0.1,     -0.5,     -0.0,     -0.1,
     &        0.1,      0.0,     -0.1,      0.2,      0.1,     -0.1,
     &       -0.2,      0.0,     -0.2,     -0.1,     -0.2/
C IGRF -- Version 11 ----- April 30, 2010 ---  interpolated 2000-2005
      DATA NMAX(12)   /10/
      DATA TZERO(12)  /2000.0/
      DATA LABEL(12)  /'DGRF 2000                       '/
C     vv Coefficients at epoch vv
      DATA ((GG(12,J,K),K=1,11),J=1,11) /
     &        0.0,   5186.1,  -2481.6,   -227.6,    272.6,     43.8,
     &      -17.4,    -64.6,     11.9,    -19.7,      1.7,
     &   -29619.4,  -1728.2,   -458.0,    293.4,   -231.9,    171.9,
     &       63.7,    -24.2,    -21.5,     13.4,      0.0,
     &    -2267.7,   3068.4,   1670.9,   -491.1,    119.8,   -133.1,
     &       65.1,      6.2,      8.5,     12.5,      4.0,
     &     1339.6,  -2288.0,   1252.1,    714.5,   -303.8,    -39.3,
     &      -61.2,     24.0,    -21.5,     -6.2,      4.9,
     &      932.3,    786.8,    250.0,   -403.0,    111.3,    106.3,
     &        0.7,     14.8,     15.5,     -8.4,     -5.9,
     &     -218.8,    351.4,    222.3,   -130.4,   -168.6,    -12.9,
     &       43.8,    -25.4,      8.9,      8.4,     -1.2,
     &       72.3,     68.2,     74.2,   -160.9,     -5.9,     16.9,
     &      -90.4,     -5.8,    -14.9,      3.8,     -2.9,
     &       79.0,    -74.0,      0.0,     33.3,      9.1,      6.9,
     &        7.3,     -1.2,     -2.1,     -8.2,      0.2,
     &       24.4,      6.6,     -9.2,     -7.9,    -16.6,      9.1,
     &        7.0,     -7.9,     -7.0,      4.8,     -2.2,
     &        5.0,      9.4,      3.0,     -8.4,      6.3,     -8.9,
     &       -1.5,      9.3,     -4.3,     -8.2,     -7.4,
     &       -2.6,     -6.0,      1.7,     -3.1,     -0.5,      3.7,
     &        1.0,      2.0,      4.2,      0.3,     -1.1/
C     vv Time derivative of coefficients vv
      DATA ((GGT(12,J,K),K=1,11),J=1,11) /
     &        0.0,    -21.6,    -22.6,      5.7,      1.9,     -0.2,
     &       -0.6,      0.7,     -0.1,     -0.1,      0.1,
     &       13.0,     11.8,    -11.5,     -4.7,      1.3,      1.7,
     &       -1.8,      0.3,      0.1,     -0.1,      0.0,
     &      -13.9,     -4.1,     -2.6,     -6.7,      5.1,      1.9,
     &       -0.3,      0.1,      0.3,      0.0,      0.1,
     &       -0.7,     -3.6,     -1.1,     -8.4,     -0.3,      3.9,
     &       -0.5,      0.3,      0.4,     -0.1,     -0.0,
     &       -2.4,      2.2,     -7.9,      4.6,     -2.3,     -0.5,
     &       -0.1,     -0.8,      0.1,      0.0,     -0.1,
     &       -1.6,      0.6,     -2.7,     -1.2,      0.1,     -0.1,
     &        1.4,     -0.2,     -0.3,     -0.1,      0.0,
     &        0.3,      0.3,      0.5,      1.9,     -1.7,     -0.5,
     &        0.8,      0.2,      0.4,     -0.2,     -0.1,
     &        0.2,     -0.1,     -0.3,      1.1,      0.6,      0.5,
     &       -0.4,      0.6,      0.4,      0.1,     -0.2,
     &        0.1,      0.2,     -0.5,      0.2,     -0.3,      0.2,
     &        0.5,     -0.7,      0.4,      0.2,     -0.0,
     &        0.1,      0.1,      0.1,      0.3,     -0.3,     -0.4,
     &        0.1,     -0.1,     -0.5,     -0.2,     -0.1,
     &        0.1,     -0.0,     -0.1,      0.1,      0.1,     -0.1,
     &       -0.1,      0.0,     -0.1,     -0.1,     -0.2/
C IGRF -- Version 12 ----- December, 2014 ---  interpolated 2005-2010
      DATA NMAX(13)   /10/
      DATA TZERO(13)  /2005.0/
      DATA LABEL(13)  /'DGRF 2005                       '/
C     vv Coefficients at epoch vv
      DATA ((GG(13,J,K),K=1,11),J=1,11) /
     &        0.00,   5077.99,  -2594.50,   -198.86,    282.07,
     &       42.72,    -20.33,    -61.14,     11.20,    -20.11,
     &        2.19,
     &   -29554.63,  -1669.05,   -515.43,    269.72,   -225.23,
     &      180.25,     54.75,    -22.57,    -20.88,     12.69,
     &        0.10,
     &    -2337.24,   3047.69,   1657.76,   -524.72,    145.15,
     &     -123.45,     63.63,      6.82,      9.83,     12.67,
     &        4.46,
     &     1336.30,  -2305.83,   1246.39,    672.51,   -305.36,
     &      -19.57,    -63.53,     25.35,    -19.71,     -6.72,
     &        4.76,
     &      920.55,    797.96,    210.65,   -379.86,    100.00,
     &      103.85,      0.24,     10.93,     16.22,     -8.16,
     &       -6.58,
     &     -227.00,    354.41,    208.95,   -136.54,   -168.05,
     &      -13.55,     50.94,    -26.32,      7.61,      8.10,
     &       -1.01,
     &       73.60,     69.56,     76.74,   -151.34,    -14.58,
     &       14.58,    -86.36,     -4.64,    -12.76,      2.92,
     &       -3.47,
     &       79.88,    -74.46,     -1.65,     38.73,     12.30,
     &        9.37,      5.42,      1.94,     -0.06,     -7.73,
     &       -0.86,
     &       24.80,      7.62,    -11.73,     -6.88,    -18.11,
     &       10.17,      9.36,    -11.25,     -4.87,      6.01,
     &       -2.31,
     &        5.58,      9.76,      3.58,     -6.94,      5.01,
     &      -10.76,     -1.25,      8.76,     -6.66,     -9.22,
     &       -7.93,
     &       -2.17,     -6.12,      1.42,     -2.35,     -0.15,
     &        3.06,      0.29,      2.06,      3.77,     -0.21,
     &       -2.09/
C     vv Time derivative of coefficients vv
      DATA ((GGT(13,J,K),K=1,11),J=1,11) /
     &        0.00,    -26.75,    -22.81,      7.69,      0.88,
     &        0.37,     -0.11,      0.67,     -0.07,     -0.09,
     &        0.11,
     &       11.61,     16.53,    -12.06,     -3.59,      2.84,
     &        1.75,     -2.11,      0.27,      0.17,     -0.24,
     &       -0.04,
     &      -11.76,     -4.27,      2.08,     -2.46,      3.86,
     &        1.08,     -0.42,     -0.06,      0.40,      0.02,
     &        0.05,
     &        0.71,     -4.14,     -2.86,     -7.76,     -0.87,
     &        3.91,     -0.55,     -0.08,      0.46,     -0.08,
     &       -0.06,
     &       -1.58,      2.20,     -8.81,      4.61,     -2.12,
     &       -0.56,      0.56,     -0.78,      0.10,      0.15,
     &       -0.13,
     &       -0.77,      0.58,     -1.74,     -0.90,      0.98,
     &        1.10,      0.89,     -0.26,     -0.13,     -0.03,
     &        0.01,
     &       -0.16,     -0.17,     -0.16,      1.99,     -1.65,
     &       -0.30,      1.65,      0.27,      0.40,     -0.16,
     &       -0.10,
     &        0.11,     -0.11,     -0.58,      1.30,      0.34,
     &        0.22,     -0.76,      0.60,      0.34,      0.33,
     &       -0.23,
     &       -0.08,      0.12,     -0.55,      0.26,     -0.25,
     &        0.29,      0.30,     -0.56,      0.27,      0.20,
     &        0.07,
     &       -0.02,     -0.06,     -0.03,      0.33,     -0.38,
     &       -0.32,      0.10,     -0.07,     -0.35,     -0.17,
     &       -0.08,
     &        0.05,     -0.02,     -0.11,      0.26,     -0.00,
     &       -0.12,     -0.12,      0.01,     -0.14,     -0.16,
     &       -0.14/
C IGRF -- Version 12 ----- December, 2014 ---  interpolated 2010-2015
      DATA NMAX(14)   /10/
      DATA TZERO(14)  /2010.0/
      DATA LABEL(14)  /'DGRF 2010                       '/
C     vv Coefficients at epoch vv
      DATA ((GG(14,J,K),K=1,11),J=1,11) /
     &        0.00,   4944.26,  -2708.54,   -160.40,    286.48,
     &       44.58,    -20.90,    -57.80,     10.84,    -20.54,
     &        2.73,
     &   -29496.57,  -1586.42,   -575.73,    251.75,   -211.03,
     &      189.01,     44.18,    -21.20,    -20.03,     11.51,
     &       -0.10,
     &    -2396.06,   3026.34,   1668.17,   -537.03,    164.46,
     &     -118.06,     61.54,      6.54,     11.83,     12.75,
     &        4.71,
     &     1339.85,  -2326.54,   1232.10,    633.73,   -309.72,
     &       -0.01,    -66.26,     24.96,    -17.41,     -7.14,
     &        4.44,
     &      912.66,    808.97,    166.58,   -356.83,     89.40,
     &      101.04,      3.02,      7.03,     16.71,     -7.42,
     &       -7.22,
     &     -230.87,    357.29,    200.26,   -141.05,   -163.17,
     &       -8.03,     55.40,    -27.61,      6.96,      7.97,
     &       -0.96,
     &       72.78,     68.69,     75.92,   -141.40,    -22.83,
     &       13.10,    -78.09,     -3.28,    -10.74,      2.14,
     &       -3.95,
     &       80.44,    -75.00,     -4.55,     45.24,     14.00,
     &       10.46,      1.64,      4.92,      1.64,     -6.08,
     &       -1.99,
     &       24.41,      8.21,    -14.50,     -5.59,    -19.34,
     &       11.61,     10.85,    -14.05,     -3.54,      7.01,
     &       -1.97,
     &        5.50,      9.45,      3.45,     -5.27,      3.13,
     &      -12.38,     -0.76,      8.43,     -8.42,    -10.08,
     &       -8.31,
     &       -1.94,     -6.24,      0.89,     -1.07,     -0.16,
     &        2.45,     -0.33,      2.13,      3.09,     -1.03,
     &       -2.80/
C     vv Time derivative of coefficients vv
      DATA ((GGT(14,J,K),K=1,11),J=1,11) /
     &        0.00,    -29.43,    -27.41,      9.02,     -0.64,
     &        0.54,      0.02,      0.74,     -0.15,     -0.21,
     &        0.09,
     &       10.91,     17.08,    -13.23,     -1.37,      4.47,
     &        1.60,     -2.20,      0.34,      0.35,     -0.14,
     &       -0.06,
     &       -9.81,     -2.69,      1.71,     -0.27,      3.29,
     &       -0.25,     -0.53,     -0.17,      0.29,     -0.19,
     &       -0.02,
     &        2.17,     -5.15,     -1.30,    -10.35,     -3.96,
     &        3.20,     -0.09,     -0.11,      0.56,      0.07,
     &       -0.01,
     &       -1.01,      0.95,     -9.24,      4.39,     -3.80,
     &       -0.17,      0.86,     -0.73,     -0.10,      0.10,
     &       -0.14,
     &       -0.35,      0.56,     -1.57,      0.03,      1.13,
     &        2.43,      1.44,      0.04,     -0.25,     -0.03,
     &        0.07,
     &       -0.56,     -0.20,     -0.64,      2.30,     -1.21,
     &        0.02,      1.44,      0.22,      0.33,     -0.23,
     &       -0.05,
     &        0.23,     -0.22,     -0.45,      1.31,      0.20,
     &       -0.21,     -0.89,      0.38,      0.09,      0.42,
     &       -0.16,
     &       -0.04,      0.12,     -0.48,      0.48,     -0.25,
     &        0.36,      0.17,     -0.37,      0.31,      0.28,
     &        0.15,
     &       -0.02,     -0.13,     -0.07,      0.39,     -0.49,
     &       -0.18,      0.13,      0.05,     -0.14,     -0.08,
     &       -0.08,
     &        0.01,     -0.01,     -0.16,      0.31,     -0.07,
     &       -0.13,     -0.07,     -0.01,     -0.14,     -0.15,
     &       -0.16/
C IGRF -- Version 12 ----- December, 2014 ---  extrapolated 2015-
      DATA NMAX(15)   /10/
      DATA TZERO(15)  /2015.0/
      DATA LABEL(15)  /'IGRF 2015                       '/
C     vv Coefficients at epoch vv
      DATA ((GG(15,J,K),K=1,11),J=1,11) /
     &        0.0,   4797.1,  -2845.6,   -115.3,    283.3,
     &       47.3,    -20.8,    -54.1,     10.1,    -21.6,
     &        3.2,
     &   -29442.0,  -1501.0,   -641.9,    244.9,   -188.7,
     &      197.0,     33.2,    -19.5,    -18.3,     10.8,
     &       -0.4,
     &    -2445.1,   3012.9,   1676.7,   -538.4,    180.9,
     &     -119.3,     58.9,      5.7,     13.3,     11.8,
     &        4.6,
     &     1350.7,  -2352.3,   1225.6,    582.0,   -329.5,
     &       16.0,    -66.7,     24.4,    -14.6,     -6.8,
     &        4.4,
     &      907.6,    813.7,    120.4,   -334.9,     70.4,
     &      100.2,      7.3,      3.4,     16.2,     -6.9,
     &       -7.9,
     &     -232.6,    360.1,    192.4,   -140.9,   -157.5,
     &        4.1,     62.6,    -27.4,      5.7,      7.8,
     &       -0.6,
     &       70.0,     67.7,     72.7,   -129.9,    -28.9,
     &       13.2,    -70.9,     -2.2,     -9.1,      1.0,
     &       -4.2,
     &       81.6,    -76.1,     -6.8,     51.8,     15.0,
     &        9.4,     -2.8,      6.8,      2.1,     -4.0,
     &       -2.8,
     &       24.2,      8.8,    -16.9,     -3.2,    -20.6,
     &       13.4,     11.7,    -15.9,     -2.0,      8.4,
     &       -1.2,
     &        5.4,      8.8,      3.1,     -3.3,      0.7,
     &      -13.3,     -0.1,      8.7,     -9.1,    -10.5,
     &       -8.7,
     &       -1.9,     -6.3,      0.1,      0.5,     -0.5,
     &        1.8,     -0.7,      2.1,      2.4,     -1.8,
     &       -3.6/
C     vv Time derivative of coefficients vv
      DATA ((GGT(15,J,K),K=1,11),J=1,11) /
     &        0.0,    -26.6,    -27.4,      8.2,     -1.3,
     &        0.6,      0.0,      0.8,     -0.3,      0.0,
     &        0.0,
     &       10.3,     18.1,    -14.1,     -0.4,      5.3,
     &        1.7,     -2.1,      0.4,      0.3,      0.0,
     &        0.0,
     &       -8.7,     -3.3,      2.1,      1.8,      2.9,
     &       -1.2,     -0.7,     -0.2,      0.1,      0.0,
     &        0.0,
     &        3.4,     -5.5,     -0.7,    -10.1,     -5.2,
     &        3.4,      0.2,     -0.3,      0.5,      0.0,
     &        0.0,
     &       -0.7,      0.2,     -9.1,      4.1,     -4.3,
     &        0.0,      0.9,     -0.6,     -0.2,      0.0,
     &        0.0,
     &       -0.2,      0.5,     -1.3,     -0.1,      1.4,
     &        3.9,      1.0,      0.1,     -0.3,      0.0,
     &        0.0,
     &       -0.3,     -0.1,     -0.7,      2.1,     -1.2,
     &        0.3,      1.6,     -0.2,      0.3,      0.0,
     &        0.0,
     &        0.3,     -0.2,     -0.5,      1.3,      0.1,
     &       -0.6,     -0.8,      0.2,      0.0,      0.0,
     &        0.0,
     &        0.2,      0.0,     -0.6,      0.5,     -0.2,
     &        0.4,      0.1,     -0.4,      0.3,      0.0,
     &        0.0,
     &        0.0,      0.0,      0.0,      0.0,      0.0,
     &        0.0,      0.0,      0.0,      0.0,      0.0,
     &        0.0,
     &        0.0,      0.0,      0.0,      0.0,      0.0,
     &        0.0,      0.0,      0.0,      0.0,      0.0,
     &        0.0/

      END

C***********************************************************************
C*  Institute:     MPE               *                *                *
C*                                   *                *   SUBROUTINE   *
C*  DDDDDD         AA       LLL      *       GRO      *     P4SHLL     *
C*  DDDDDDD      AA  AA     LLL      *     COMPTEL    *                *
C*  DD    DD    AA    AA    LLL      *                * First Version  *
C*  DD    DD    AA    AA    LLL      ******************                **
C*  DD    DD    AAAAAAAA    LLL      *                * Author:         *
C*  DD    DD    AAAAAAAA    LLL      *                * Georg           *
C*  DD    DD    AA    AA    LLL      *    COMPASS     * Weidenspointner *
C*  DD    DD    AA    AA    LLL      *                *                **
C*  DDDDDDD     AA    AA    LLLLLLLL *                * Date :         *
C*  DDDDDD      AA    AA    LLLLLLLL *                *  04-12-95      *
C*                                   *                *                *
C*  Data        Access      Layer    *                *                *
C***********************************************************************
C*                                                                     *
C*  Function: called from GEO_GL                                       *
C*            calculates local magnetic field and field-shell L-value  *
C*                                                                     *
C*  Note: code was provided by Kevin Bennett, ESTEC                    *
C*                                                                     *
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
C   Description: tbd
C***********************************************************************


C-----------------------------------------------------------------------
      subroutine shell(gclat, dlong, radius, outer, kp, fkp,
     &                 den, vel, dst, gmagmo, colat, elong, ut, amjd,
     &                 alpha, b, fl)
C
C     Procedure that calculates the magnetic field B and the parameter L,
C     taking into account external and internal fields.
C INPUT:
C     gclat : geographic latitude (in degrees)
C     dlong : geographic longitude (in degrees)
C     radius : geographic distance (in km)
C     outer : nr. of the external model (0...6) (0=no external field)
C     bltime : year for the calculation of B in the tables
C     kp : magnetic conditions for the Tsyganenko models
C     fkp : value of Kp
C     den : density of Solar wind (in particles/cc)
C     vel : velocity of Solar wind (in km/s)
C     Dst 
C     gmagmo : dipole moment (gauss/Re^3)
C     colat : geographic colatitude of boreal dipole pole
C     elong : geographic east longitude of boreal dipole pole
C     ut : universal time in degrees
C     amjd : modified Julian Day
C     alpha : pitch angle (in degrees)
C OUTPUT:
C     b : resultant field (gauss)
C     fl : McIlwain's L-parameter (Re)
C SUBROUTINES IN SHELL:
C     1 FLDINT
C     2 STEPSZ
C     3 INVR
C     4 INTRP
C     5 INTGRT
C     6 BTOT
C FUNCTIONS IN SHELL:
C     1 RMAGP      
C DEC 89: P.DOMANGE & J.LEMAIRE (IASB) : CREATION OF BLXTRA.
C SEP 90: P.DOMANGE & J.LEMAIRE (IASB) : DELIVERED TO ESTEC BY TREND
C FEB 91: L.FEDULLO & J.LEMAIRE (IASB) : DETERMINE VALUE OF LOCAL TIME (tlrad)
C                                        IN LINESA TO COMPUTE RMAGP.
C FEB 91: L.FEDULLO & J.LEMAIRE (IASB) : ADD EXTERNAL MAGNETIC FIELD OF
C                                        OLSON-PFITZER-77 IN OFIELD SUBROUTINE
C                                        (OUTER=5) 
C JUN 92: D. HEYNDERICKX (BIRA) :
C             GEOGRAPHIC COORDINATES OF BOREAL DIPOLE POLE ARE UPDATED
C               TO BLTIME AND ADDED TO INPUT PARAMETER LIST
C             AMJD ADDED TO INPUT PARAMETER LIST
C             UT replaces hour, min, sec
C MAR 93: D. HEYNDERICKX (BIRA) :
C             ADDED OLSON-PFITZER DYNAMIC EXTERNAL FIELD (OUTER=6)
C APR 93: D. HEYNDERICKX (BIRA) :
C             REPLACED INVAR BY FLDINT (PFITZER 1991)
C-----------------------------------------------------------------------

      IMPLICIT NONE

      REAL*8  dsol(3), t(3,3), tilt, strad, ut, amjd, den, vel, dst, fkp
      REAL*8  gclat, dlong, radius, gmagmo, colat, elong, b, fl
      REAL*8  alpha, radre
      INTEGER*4 kp, outer
      
      if (outer .gt. 0) then
C calculate position of sun in inertial coordinates
        CALL SOLA(amjd, dsol)
C calculate G.H.A. of equinox
        CALL GHA(amjd, strad)
        if ((outer .ge. 2) .and. (outer .le. 4)) then
C form transformation matrix from GEO to GSM coordinates
          CALL TRANS1(strad, dsol, t, tilt, colat, elong)
        else
C form transformation matrix from GEO to SM coordinates
          CALL TRANS2(strad, dsol, t, tilt, colat, elong)
        end if
      end if

C calculate field line positions and geomagnetic coordinates
      radre = radius / 6371.2D0
      CALL FLDINT(gclat, dlong, radre, ut, t, tilt, kp, fkp, den, vel,
     &            dst, outer, gmagmo, colat, elong, alpha, fl, b,
     &            0.5D-4)
      
      END

C///////////////////// INTEGRATION ALONG FIELD LINE ////////////////////
C
      SUBROUTINE FLDINT(XLat, XLong, RGeo, ut, t, tilt, kp, fkp, den,
     &                  vel, dst, outer, gmagmo, colat, elong, alpha,
     &                  EL, BLOCAL, Err)
C
C*****Modified by Francis December 1982
C*****Modified by G. Davidson 3/26/91
C
C>>>>>Purpose
C        Evaluate the magnetic L shell parameter at a specified point.
C>>>>>Method
C        Integrate along the magnetic field line which passes through the point. 
C        The integration is performed between the mirror point magnetic field 
C        values. The second adiabatic invariant is evaluated and Hilton's 
C        expansion of L in terms of BMAX and the second invariant is used 
C        to evaluate L (see subroutine HILTEL).
C
C>>>>>Input -- argument list
C        XLat   geocentric geographic latitude in degrees (+ is north)
C        XLong  geocentric geographic longitude east of greenwhich in degrees
C        RGeo   geocentric distance from the earth's center in units of
C               earth radii, RE = 6371.2 km
C        UT     universal time in degrees
C        Err    = 0.0005  scales the error limits for the integration the 
C               error in L is approximately L*err
C>>>>>Output
C        El     the L value determined as requested by flow control
C               *****note****
C               since this routine uses an actual magnetospheric magnetic
C               field, the field lines are not all closed. Thus L is defined 
C               only in the inner magnetosphere (in the region of closed drift
C               shells). An attempt to calculate L outside of this region 
C               will set El = 100.
C        BLocal the value of the magnetic field at the input position (in Gauss)
C
C>>>>>Subroutines required
C        SUBROUTINE STEPSZ
C        SUBROUTINE BMNEXT
C        SUBROUTINE hilton
C        SUBROUTINE INTRP
C        SUBROUTINE INTGRT
C        SUBROUTINE INVR
C
C>>>>>Variables
C        BIntl  a real array that saves the value of the magnetic field
C               at the input position
C        BVal   a real array that holds the instantaneous magnetic field
C               vector at each integration step
C        BStep  a 2 dimensioned real array that holds all of the magnetic 
C               field magnitudes calculated at all of the integration steps
C        BLast  a real array that saves the magnetic field vector from 
C               the previous integration step
C        BMax   the magnetic field at the particle mirror point
C        SinGL  the sine of the geographic colatitude
C        DStart the estimated step size necessary to complete the integration.  
C               if no estimate is yet possible it is set to 100.
C        Delta  scales the integration step size. it is proportional to the 
C               fourth root of the error limits. if it is positive, integration 
C               will be parallel to the field, if negative it is antiparallel
C        DStep  the current value of the integration step size in earth radii.
C               positive is for parallel to field, negative for antiparallel
C        IErFlg an error flag set by subroutine INTGRT. if non-zero, the 
C               integration has gone beyond the limits and must be terminated.
C        istp   step number for start of tracing. if istp=3, the starting
C               point is a local field maximum.
C        NStep  the current integration step number
C        DegRad pi / 180.
C        QCur, QLast real arrays containing the current and previous error
C               estimates. used by Gill's method integration routine to
C               control round off error
C        SErr   error control variable. the integration stops if the 
C               current position point is within distance SErr of BMax.
C        SMax   output of the interpolation subroutine intrp. it indicates 
C               the scalar distance along the field where B = BMax.
C        SavJ   a real array which saves the integration step values of 
C               the second adiabatic invariant
C        XDS    temorary value used for obtaining distance to completion
C               of integration
C        AdInJ  final value of the second adiabatic invariant
C        XLast  a real array holding the previous value of the position vector
C        XStep  a 3 dimensioned real array holding all of the position
C               vectors along the integration path
C        AdIntp interpolated value of the second adiabatic invariant
C        ZGeo   the z component of the position vector in centered
C               dipole coordinates
C
C     Version 10/25/77
C     For more information call or write K. A. Pfitzer at Mcdonnell
C     Douglas Astronautics Co. 5301 Bolsa Ave, Huntington Beach CA
C     phone (714) 896-3231.
C
C     ..This version has been modified by G. Davidson to accept input
C     for selected epochs. There is also a lot of cleaning up to make
C     the code more readable, for which I take full responsibility. The
C     code has been tested on a PC and on a VAX.  
C                                         ......G. D.  7/10/91
C
C     D. Heynderickx (BIRA) : May 1993:
C       Adapted to BLXTRA. Calculations are now in spherical coordinates 
C       instead of Cartesian coordinates. Further cleaning up of code.
C       Refinement of line tracing and calculation of step size. 
C
C-----------------------------------------------------------------------
C
      IMPLICIT NONE

C     ..Arguments
      REAL*8  XLat, XLong, RGeo, UT, El, BLocal, Err, alpha
      REAL*8  fkp, den, vel, dst, t(3,3), tilt, gmagmo, colat, elong
      REAL*8  DStep, Delta, XLast(3), XVal(3), XStep(100,3,4)
      REAL*8  BStep(100,4), BVect(100,4), SVal(100), QLast(3), QCur(3)
      REAL*8  BLast(3), BVal(3), SavJ(100), DStart
      INTEGER*4 outer, kp

C     ..Working variables
      REAL*8  lat, lon, XGeo(3), BIntl(3), SinGL, ZGeo, BMax, SErr, cs
      REAL*8  SMax, XDS, AdIntp, AdInJ, XmLat, Br, Bt, Bp, BMir
      INTEGER*4 i, j, istp, NStep, IErFlg

C     ..Constants
      REAL*8 DegRad

C     ..Functions
      REAL*8 Hilton, Rmagp

      integer*4 ndim, nfld
      real*8    flds(200), fldx(200,3), fldb(200,4), footp(2,2)
      common /fldlin/ ndim, nfld, flds, fldx, fldb, footp

      COMMON /field/ br, bt, bp

C=======================================================================
      ndim = 200
      DegRad = datan(1.0D0) / 45.0D0
      IErFlg = 0
      lat = (90.0D0-XLat) * DegRad
      lon = Xlong * DegRad
      SinGL = DSIN(lat)
      XGeo(1) = RGeo * SinGL * DCOS(lon)
      XGeo(2) = RGeo * SinGL * DSIN(lon)
      XGeo(3) = RGeo * DCOS(lat)
C Transform to dipole coordinates
      ZGeo = (XGeo(1)*DCOS(elong*DEGRAD)+XGeo(2)*DSIN(elong*DEGRAD)) *
     &       DSIN(colat*DEGRAD) + XGeo(3) * DCOS(COLAT*DEGRAD)
C Evaluate the magnetic latitude
      XMLat = 90.0D0 - DACOS(ZGeo/RGeo) / DEGRAD

C Evaluate the magnetic field at the starting point
      XVal(1) = RGeo
      XVal(2) = lat
      XVal(3) = lon
      CALL btot(XVal, t, tilt, kp, den, vel, dst, outer, BVal,
     &          BStep(2,1))
      BLocal = BStep(2,1)
C Save the initial position and magnetic field vectors
      DO I=1,3
        BIntl(I) = BVal(I)
        QCur(I) = 0.0D0
        XStep(2,I,1) = XVal(I)
        BVect(2,I+1) = BVal(I)
      END DO
      BVect(2,1) = BStep(2,1)
      br = bval(1)
      bt = bval(2)
      bp = bval(3)
C Exit the routine if position is over the polar cap or distance is too large
C or magnetic field is too weak
      IF (DABS(XMLat) .GT. 80.0D0) THEN
        IErFlg = 1
      ELSE IF (RGeo .GT. 8.0D0) THEN
        cs = dcos((ut+Xlong)*DEGRAD)
        if (RGeo .gt. rmagp(cs,fkp)) IErFlg = 2
      ELSE IF (BStep(2,1) .LT. 0.00004D0) THEN
        IErFlg = 3
      END IF
      IF (IErFlg .NE. 0) GO TO 330
C Set up the initial values for the variables
      NStep = 2
      SVal(2) = 0.0D0
      DStart = 100.0D0
      BMax = BStep(2,1) / DSIN(alpha*DEGRAD)**2
C Set up the error limits for the integration
      SErr = DSQRT(Err)
C Step size goes as error to the .25 power
      Delta = -2.5D0 * Err**0.25D0
      DStep = SErr
C Step once in the increasing field direction and set step parameters 
C to integrate in the decreasing field direction
      istp = 1
      IF (XMLat .GT. 0.0D0) GO TO 30
 20   Delta = -Delta
      DStep = -DStep
 30   XLast(1) = XVal(1) + DStep * BVal(1) / BStep(2,1)
      XLast(2) = XVal(2) + DStep / XVal(1) * BVal(2) / BStep(2,1)
      XLast(3) = XVal(3) + DStep / XVal(1) / DSIN(XVal(2)) * BVal(3) 
     &           / BStep(2,1)
      CALL btot(XLast, t, tilt, kp, den, vel, dst, outer, BLast,
     &          BStep(1,1))
C The magnetic field has a local maximum, exit.
      if (istp .eq. 3) then
        IErFlg = 7
        go to 330
      end if
C If the previous step was in the decreasing field direction try again
      IF (BStep(1,1) .LT. BStep(2,1)) THEN
        istp = istp + 1
        GO TO 20
      END IF
      SVal(1) = DStep
      DO I=1,3
        XStep(1,I,1) = XLast(I)
        BVect(1,I+1) = BLast(I)
      END DO
      BVect(1,1) = BStep(1,1)
C Begin the field line integration. The integration uses a variable stepsize 
C which is dependent on the curvature of the field line and on the distance
C each point is from earth center (a measure of the magnetic field strength).
C The initial integration is a line integral of the magnetic field unit vector.
C This integration loop also saves all of the variables which are later needed
C to evaluate the second integral invariant. 
 40   CALL STEPSZ(DStep, Delta, NStep, XVal, BStep, BLast, BVal, DStart)
      CALL INTGRT(DStep, NStep, IErFlg, XLast, XVal, XStep, BStep, SVal,
     &            QLast, QCur, BLast, BVal, t, tilt, outer, kp, fkp,
     &            den, vel, dst, ut, Xlong, BVect)
C If the integration is outside the region of validity or the maximum 
C number of steps have occurred the error exit is taken.
      IF (IErFlg .NE. 0) GO TO 330
C If field is still decreasing reloop
      IF (BStep(NStep,1) .LT. BStep(NStep-1,1)) GO TO 40
C Continue stepping along the field line as long as B < BMax and the 
C integration is more than a distance SErr from BMAX
C If BMax has been exceeded, exit to interpolation scheme
 50   IF (BStep(NStep,1) .GE. BMax) GO TO 70
      DStart = 100.0D0
      if (NStep .GT. 2) then
        CALL INTRP(BStep(NStep-2,1), SVal(NStep-2), BMax, SMax, 3)
C If SVal does not increase monotonically, ignore interpolation and reloop
        IF (DABS(SMax) .LE. DABS(SVal(NStep))) GO TO 40
        XDS = SMax - SVal(NStep)
C If within SErr of BMAX stop integration and get value of invariant
        IF (DABS(XDS) .LT. SErr) GO TO 100
        DStart = 0.9D0 * XDS
      end if
C RELOOP
      GO TO 40
C The function SQRT(1-B/BMax) does not exist for B > BMax if previous step
C is not within SErr of BMax interpolate to find a step size that will get 
C close to but not exceed BMax
 70   continue
c      if (NStep .eq. 3) then
c        El = hilton(gmagmo, BMax, 0.0D0)
c        return
c      end if
      NStep = NStep - 1
C Set up the step size and reset integration values to the previous step
      DStep = DStep / 2.0D0
      IF (NStep .GT. 2) THEN
        CALL INTRP(BStep(NStep-2,1), SVal(NStep-2), BMax, SMax, 3)
C If the step size < SErr, the previous step is close enough.
C Exit to invariant calculation
        IF (DABS(SMax-SVal(NStep)) .LT. SErr) GO TO 100
c        IF (DABS(SMax) .GT. DABS(SVal(NStep))) THEN
c          xds = 0.7D0 * (SMax-SVal(NStep))
c          if (DABS(xds) .GT. DABS(DStep)) DStep = xds
c        END IF
      END IF
      DO I=1,3
        XVal(I) = XLast(I)
        QCur(I) = QLast(I)
        BVal(I) = BLast(I)
      END DO
      CALL INTGRT(DStep, NStep, IErFlg, XLast, XVal, XStep, BStep, SVal,
     &            QLast, QCur, BLast, BVal, t, tilt, outer, kp, fkp,
     &            den, vel, dst, ut, Xlong, BVect)
      IF (IErFlg .NE. 0) GO TO 330
C If last step is still past BMax try the interpolation scheme again
 90   IF ((BStep(NStep,1) .GE. BMax) .AND.
     &    (DABS(SVal(NStep)-SVal(NStep-1)) .GT. 1.0D-10)) GO TO 70
      DStep = DStep / 2.0D0
C Interpolate to see if the interpolation step is close enough to BMax.
C If it is not, interpolate again and try to come closer.
      if (NStep .GT. 2) then
        CALL INTRP(BStep(NStep-2,1), SVal(NStep-2), BMax, SMax, 3)
C If we are close enough exit the integration loop
        IF (DABS(SMax-SVal(NStep)) .LT. SErr) GO TO 100
c        IF (DABS(SMax) .GT. DABS(SVal(NStep))) THEN
c          xds = 0.9D0 * (SMax-SVal(NStep))
c          if (DABS(xds) .GT. DABS(DStep)) DStep = xds
c        END IF
      end if
      CALL INTGRT(DStep, NStep, IErFlg, XLast, XVal, XStep, BStep, SVal,
     &            QLast, QCur, BLast, BVal, t, tilt, outer, kp, fkp,
     &            den, vel, dst, ut, Xlong, BVect)
      IF (IErFlg .NE. 0) GO TO 330
      GO TO 90
C The field maximum has now been reached. The stored values of the magnetic  
C field and the path length values can now be used to evaluate the second 
C invariant.
 100  IF (NStep .LT. 3) then
        IerFlg = 6
        go to 330
      end if
C Call the routine that determines the second invariant from the stored values.
      CALL INVR(BMax, NStep, BStep, SVal, SavJ)
C Interpolate to get the best fit.
      IF ((NStep .GT. 3) .AND.
     &    (DABS(SVal(NStep)-SVal(NStep-1)) .GT. 1.0D-10)) THEN
        CALL INTRP(BStep(NStep-2,1), SavJ(NStep-2), BMax, AdIntp, 3)
      ELSE
        AdIntp = SavJ(NStep)
      END IF
C Save the absolute value of the second invariant.
      AdInJ = DABS(AdIntp)
C Store the magnetic field vector, coordinate vector, and ds in COMMON
C BLOCK variables
      IF (NStep .GT. 100) THEN
        IErFlg = 4
        GO TO 330
      END IF
      nfld = NStep
      if ((DABS(SVal(NStep)-SVal(NStep-1)) .LE. 1.0D-10) .OR.
     &    (NStep .LE. 3)) then
        nfld = nfld - 1
      end if
      do i=2,nfld
        do j=1,3
          fldx(nfld+2-i,j) = XStep(i,j,1)
          fldb(nfld+2-i,j+1) = BVect(i,j+1)
        end do
        fldb(nfld+2-i,1) = BVect(i,1)
        flds(nfld+2-i) = SVal(i)
      end do
C Interpolate to find the location of the first mirror point.
      if ((DABS(SVal(NStep)-SVal(NStep-1)) .GT. 1.0D-10) .AND.
     &    (NStep .GT. 3)) then
 101    CALL INTGRT(DStep, NStep, IErFlg, XLast, XVal, XStep, BStep,
     &              SVal, QLast, QCur, BLast, BVal, t, tilt, outer, kp,
     &              fkp, den, vel, dst, ut, Xlong, BVect)
        IF (BStep(NStep,1) .LT. BMax) GO TO 101
        do i=1,3
          BVal(i) = BVect(NStep-3+i,1)
        end do
        do i=1,3
          CALL INTRP(BVal(1), XStep(NStep-2,i,1), BMax, fldx(1,i), 3)
        end do
        do i=1,4
          CALL INTRP(BVal(1), BVect(NStep-2,i), BMax, fldb(1,i), 3)
        end do
        CALL INTRP(BVal(1), SVal(NStep-2), BMax, flds(1), 3)
      else
        do j=1,3
          fldx(1,j) = XStep(NStep,j,1)
          fldb(1,j+1) = BVect(NStep,j+1)
        end do
        fldb(1,1) = BVect(NStep,1)
        flds(1) = SVal(NStep)
      end if
C Renormalize the magnetic field vector at the first mirror point.
      BMir = 0.0D0 
      do i=2,4
        BMir = BMir + fldb(1,i)**2
      end do
      BMir = dsqrt(BMir)
      do i=2,4
        fldb(1,i) = fldb(1,i) / BMir * BMax
      end do
      fldb(1,1) = BMax
C The integral has now been evaluated from the starting point through the 
C minimum value of B to BMax. If the pitch angle differs from 90 deg, the
C integral must be turned around and the section of the field line between 
C the initial location and BMax at the other end of the field line must now  
C also be determined.
C Check to see if BMax is sufficiently different to require this integration.
C If not, go directly to the L evaluation.
C      IF (DABS(BStep(2,1)-BMax)/BMax .LT. Err) GO TO 210
      IF ((alpha .eq. 90.0D0) .OR. (alpha .eq. 180.0D0)) GO TO 210
C      CALL INTRP(BStep(1,1), SVal(1), BMax, SMax, 2)
C      IF (DABS(SMax) .LT. SErr) GO TO 220
C We must integrate the rest of the line --- turn the starting points around 
C and reset the initial values.
      SavJ(1) = SavJ(3)
      BStep(1,1) = BStep(3,1)
      SVal(1) = SVal(3)
      Delta = -Delta
      DStep = SVal(2) - SVal(3)
      XVal(1) = RGeo
      XVal(2) = lat
      XVal(3) = lon
      DO I=1,3
        QCur(I) = 0.0D0
        BVal(I) = BIntl(I)
        XStep(1,I,1) = XStep(3,I,1)
        BVect(1,I+1) = BVect(3,I+1)
      END DO
      BVect(1,1) = BVect(3,1)
      NStep = 2
      GO TO 140
C Begin integrating the second part.
 130  CALL STEPSZ(DStep, Delta, NStep, XVal, BStep, BLast, BVal, DStart)
 140  CALL INTGRT(DStep, NStep, IErFlg, XLast, XVal, XStep, BStep, SVal,
     &            QLast, QCur, BLast, BVal, t, tilt, outer, kp, fkp,
     &            den, vel, dst, ut, Xlong, BVect)
      IF (IErFlg .NE. 0) GO TO 330
C Stop integration if BMax has been passed.
      IF (BStep(NStep,1) .GE. BMax) GO TO 150
      DStart = 100.0D0
      CALL INTRP(BStep(NStep-2,1), SVal(NStep-2), BMax, SMax, 3)
C Ignore interpolation if result is not monotonic.
      IF (DABS(SMax) .LE. DABS(SVal(NStep))) GO TO 130
      XDS = SMax - SVal(NStep)
C Stop integration if within SErr of BMax.
      IF (DABS(XDS) .LT. SErr) GO TO 200
      DStart = 0.9D0 * XDS
      GO TO 130
C BMax has been passed, begin interpolation scheme to find a point close to 
C BMax but less than it.
 150  NStep = NStep - 1
      DStep = DStep / 2.0D0
      IF (NStep .GE. 3) THEN
        CALL INTRP(BStep(NStep-2,1), SVal(NStep-2), BMax, SMax, 3)
        IF (DABS(SMax-SVal(NStep)) .LE. SErr) GO TO 200
c        IF (DABS(SMax) .GT. DABS(SVal(NStep))) THEN
c          XDS = 0.7D0 * (SMax-SVal(NStep))
c          if (DABS(xds) .GT. DABS(DStep)) DStep = xds
c        END IF
      END IF
      DO I=1,3
        XVal(I) = XLast(I)
        QCur(I) = QLast(I)
        BVal(I) = BLast(I)
      END DO
      CALL INTGRT(DStep, NStep, IErFlg, XLast, XVal, XStep, BStep, SVal,
     &            QLast, QCur, BLast, BVal, t, tilt, outer, kp, fkp,
     &            den, vel, dst, ut, Xlong, BVect)
      IF (IErFlg .NE. 0) GO TO 330
 170  IF ((BStep(NStep,1) .GE. BMax) .AND.
     &    (DABS(SVal(NStep)-SVal(NStep-1)) .GT. 1.0D-10)) GO TO 150
C If the point is less than BMax make sure it is close enough.
C If not, try to get closer.
      DStep = DStep / 2.0D0
      IF (NStep .GE. 3) THEN
        CALL INTRP(BStep(NStep-2,1), SVal(NStep-2), BMax, SMax, 3)
        IF (DABS(SMax-SVal(NStep)) .LT. SErr) GO TO 200
c        IF (DABS(SMax) .GT. DABS(SVal(NStep))) THEN
c          XDS = 0.9D0 * (SMax-SVal(NStep))
c          if (DABS(xds) .GT. DABS(DStep)) DStep = xds
c        END IF
      end if
      CALL INTGRT(DStep, NStep, IErFlg, XLast, XVal, XStep, BStep, SVal,
     &            QLast, QCur, BLast, BVal, t, tilt, outer, kp, fkp,
     &            den, vel, dst, ut, Xlong, BVect)
      IF (IErFlg .NE. 0) GO TO 330
      GO TO 170
C Integral is complete, use stored values to get invariant.
 200  IF (NStep .LT. 3) THEN
        IerFlg = 6
        go to 330
      END IF
      CALL INVR(BMax, NStep, BStep, SVal, SavJ)
      IF ((NStep .GE. 3) .AND.
     &    (DABS(SVal(NStep)-SVal(NStep-1)) .GT. 1.0D-10)) THEN
        CALL INTRP(BStep(NStep-2,1), SavJ(NStep-2), BMax, AdIntp, 3)
      ELSE
        AdIntp = SavJ(NStep)
      END IF
C Add in remaining contribution of second invariant.
 215  AdInJ = AdInJ + DABS(AdIntp)
C Store the magnetic field vector, coordinate vector, and ds in COMMON
C BLOCK variables
      IF (NStep+nfld .GT. 201) THEN
        IErflg = 4
        GO TO 330
      END IF
      do i=3,NStep
        do j=1,3
          fldx(nfld-2+i,j) = XStep(i,j,1)
          fldb(nfld-2+i,j+1) = BVect(i,j+1)
        end do
        fldb(nfld-2+i,1) = BVect(i,1)
        flds(nfld-2+i) = SVal(i)
      end do
      nfld = nfld + NStep - 1
C Interpolate to find the location of the second mirror point.
      if ((DABS(SVal(NStep)-SVal(NStep-1)) .GT. 1.0D-10) .AND.
     &    (NStep .GT. 3)) then
 102    CALL INTGRT(DStep, NStep, IErFlg, XLast, XVal, XStep, BStep,
     &              SVal, QLast, QCur, BLast, BVal, t, tilt, outer, kp,
     &              fkp, den, vel, dst, ut, Xlong, BVect)
        IF (BStep(NStep,1) .LT. BMax) GO TO 102
        do i=1,3
          BVal(i) = BVect(NStep-3+i,1)
        end do
        do i=1,3
          CALL INTRP(BVal(1), XStep(NStep-2,i,1), BMax, fldx(nfld,i), 3)
        end do
        do i=1,4
          CALL INTRP(BVal(1), BVect(NStep-2,i), BMax, fldb(nfld,i), 3)
        end do
        CALL INTRP(BVal(1), SVal(NStep-2), BMax, flds(nfld), 3)
      else
        nfld = nfld - 1
      end if
C Renormalize the magnetic field vector at the second mirror point.
      BMir = 0.0D0 
      do i=2,4
        BMir = BMir + fldb(nfld,i)**2
      end do
      BMir = dsqrt(BMir)
      do i=2,4
        fldb(nfld,i) = fldb(nfld,i) / BMir * BMax
      end do
      fldb(nfld,1) = BMax
C Call the L value routine.
 210  El = hilton(gmagmo, BMax, AdInJ)
      RETURN
C New BMax is very close to initial B. No additional points are required 
C to interpolate invariant value for second part of line.
C 220  CALL INTRP(BStep(2,1), SavJ(2), BMax, AdIntp, 1)
C      GO TO 215
C El cannot be calculated.
 330  El = -IErFlg
      RETURN

      END

C///////////////////////// STEP SIZE FOR INTEGRATION ///////////////////
C
      SUBROUTINE STEPSZ(DStep, Delta, NStep, XVal, BStep, BLast, BVal,
     &                  DStart)
C
C>>>>>Purpose
C        Determine the step size for the next integration step
C
C>>>>>Method
C        The step size of the Runge-Kutta integration is a function
C        of the error limits, the curvature of the field line, the
C        gradient in the magnetic field, and the estimated distance
C        to the end of the integration.
C
C>>>>>input -- COMMON block INTPAR
C        Delta  a parameter set up by the calling program to scale the
C               step size. It depends on the error limits of the integration.
C        BVal   a real array which contains the magnetic field vector at 
C               the current step
C        BLast  a real array which contains the magnetic field vector at 
C               the previous step
C        BStep  a 2 dimensioned real array
C               BStep(NStep,1) is the magnetic field magnitude at the
C               current step
C               BStep(NStep-1,1) is the magnetic field magnitude at the
C               previous step
C        DStart the estimated step size required to complete the integration
C
C>>>>>Input/Output -- COMMON block INTPAR
C        DStep  on entry to the routine ds contains the size of the last step.
C               The routine resets the value to the best step size for the
C               next integration step.
C
C>>>>>Calling subroutines
C        FLDINT
C
C>>>>>Temporary variables
C        CurvMn the minimum acceptable curvature.  This limits the step
C               size in the vicinity of the earth where the field
C               changes rapidly
C        Curv   the curvature of the field line
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

C     ..Arguments
      REAL*8  DStep, Delta, XVal(3), BStep(100,4), BLast(3), BVal(3)
      REAL*8  DStart
      INTEGER*4 NStep

C     ..Working variables
      REAL*8  CurvMn, Curv
      INTEGER*4 I
C=======================================================================
C
C Determine the minimum curvature.
      CurvMn = 5.0D0 / 3.0D0 / XVal(1)**1.5D0
C
C Determine the curvature of the field by using the rate of change of the unit
C field vector over the last step.
      Curv = 0.0D0
      DO I=1,3
        Curv = Curv + (BVal(I)/BStep(NStep,1)-
     &         BLast(I)/BStep(NStep-1,1))**2
      END DO
      Curv = DSQRT(Curv) / DABS(Dstep)
      Curv = DMAX1(Curv,CurvMn)
C Set up the new step size and limit the step size to < 2.8 earth radiito 
C prevent the integration from stepping out of the valid field region.
      DStep = Delta / Curv
      DStep = DSIGN(DMIN1(DABS(DStep),1.0D0), DStep)
      IF (NStep .LE. 3) DStep = DStep * (NStep*2-3) * 0.2D0
C If the distance to the end of the integration is smaller than the new step
C size, set the step size to the smaller value.
      IF (DABS(DStart) .LT. DABS(DStep)) DStep = DStart

      RETURN
      END

C///////////////////////// STEP-BY-STEP INTEGRATION ////////////////////
C
      SUBROUTINE INTGRT(DStep, NStep, IErFlg, XLast, XVal, XStep, BStep,
     &                  SVal, QLast, QCur, BLast, BVal, t, tilt, outer,
     &                  kp, fkp, den, vel, dst, ut, lon, bvect)
C
C>>>>>Purpose
C        This sub module performs a single Runge-Kutta integration
C        step and updates all of the variables in the integration loop.
C
C>>>>>Method
C        Perform a single fourth order integration step using Gill's
C        method of integration (ref. S. Gill Cambridge Philosophical
C        Society Proceedings vol. 47, 1951)
C
C>>>>>Input
C        DStep  the integration step size in units of earth radii. The
C               integration moves the space coordinate a distance DStep along
C               the magnetic field line. If DStep is positive, motion is in
C               the direction of the field. If DStep is negative motion is
C               anti-parallel to the field.
C        UT     universal time in degrees
C        lon    geographic longitude in degrees
C
C>>>>>Input/Output
C        NStep  the integration step number. It is incremented by
C               one at the end of this routine. (note NStep = 2 is the
C               beginning of the integration)
C        XVal   a real array giving the vector location of the
C               integration variable.
C               input - the initial position prior to the integration step
C               output- the final value after the integration step
C        BVal   a real array containing the vector magnetic field
C               in Gauss
C               input - the vector field befor the integration step
C               output- the vector field after the step
C        QCur   a real array containing an error control variable
C               used by Gills integration method
C               input - error from previous step
C               output- error after present step for input to subsequent steps
C
C>>>>>Output
C        SVal   a real array which saves each of the distances (since the
C               start of the integration) along the magnetic field line.
C               SVal(2) = 0
C               SVal(NStep+1) = SVal(NStep)+DStep etc.
C        XStep  a real 3 dimensioned array which saves the vector
C               position in earth radii for each of the integration
C               steps. XStep(NStep,1,1), XStep(NStep,2,1), XStep(NStep,3,1)
C               are vector cartesian position coordinates corresponding to 
C               position SVal(NStep) on the field line
C        BStep  a real 2 dimensioned array whcih saves the magnitude
C               of the magnetic field form each integration step.
C               BStep(NStep,1) is magnetic field value at distance s(n).
C               BStep(NStep-1,2), BStep(NStep-1,3), BStep(NStep-1,4) are
C               the intermediate values of the field used by Gills method to
C               get from BStep(NStep-1,1) to BStep(NStep,1).
C        XLast  a real array which saves the initial position values prior 
C               to starting the integration step
C        BLast  a real array which saves the vector magnetic field values 
C               prior to starting the integration step
C        QLast  a real array which saves the initial values of the error 
C               control variable
C        IErFlg an error control indicator which is used by the calling
C               program to control the program flow
C               IErFlg = 0 no error
C                        1 |geomagnetic latitude| > 80
C                        2 field line crosses the magnetopause
C                        3 integration is outside valid field limits (B<.00004)
C                        4 maximum step number (100) has been reached
C                        5 field line tracing too close to Earth (disabled)
C                        6 less than four points for field line integration
C
C>>>>>Constants
C        P29    1.0-SQRT(0.5)
C        OP7    1.0+SQRT(0.5)
C
C>>>>>Variables
C        P5DS   .5 * step size
C        P29DS  (1.0-SQRT(0.5)) * step size
C        OP7DS  (1.0+SQRT(0.5)) * step size
C        RR, SS real arrays used by Gills method to minimize computer
C               time and minimize roundoff error
C
C>>>>>Calling subroutines
C        SUBROUTINE FLDINT
C
C>>>>>Subroutines required
C        SUBROUTINE BTOT
C>>>>>Functions required
C        FUNCTION RMAGP
C-----------------------------------------------------------------------

      IMPLICIT NONE

      REAL*8 DStep, XLast(3), XVal(3), XStep(100,3,4), BStep(100,4) 
      REAL*8 BVect(100,4), SVal(100), QLast(3), QCur(3), BLast(3)
      REAL*8 BVal(3), SS(3), RR(3), P5DS, P29DS, OP7DS, DEGRAD
      REAL*8 t(3,3), tilt, den, vel, dst, fkp, ut, lon, cs
      INTEGER*4 I, Nstep, IerFlg, outer, kp

C     ..Functions
      REAL*8 Rmagp

C     ..Constants
      REAL*8  P29, OP7
      DATA  P29 / 0.2928932188D0 /, OP7 / 1.7071067812D0 /

C=======================================================================
C If NStep is too big, set error flag to 4.
      IF (NStep .GE. 100) THEN
        IErFlg = 4
        RETURN
      END IF
      DEGRAD = DATAN(1.0D0) / 45.0D0
C Save the initial values. These initial values may be needed if the integration
C step is unsuccessful (goes too far) and the step must be repeated.
      DO I=1,3
        XLast(I) = XVal(I)
        QLast(I) = QCur(I)
        BLast(I) = BVal(I)
      END DO
C Set up the constants needed by the integration loop.
      P5DS = 0.5D0 * DStep
      P29DS = P29 * DStep
      OP7DS = OP7 * DStep
C Begin Gill's method (Gill 1951) of fourth order integration.
      SS(1) = 1.0D0 
      SS(2) = 1.0D0 / XVal(1)
      SS(3) = 1.0D0 / XVal(1) / DSIN(XVal(2))
      DO I=1,3
        SS(I) = P5DS * SS(I) * BVal(I) / BStep(NStep,1)
        RR(I) = SS(I) - QCur(I)
        XVal(I) = XVal(I) + RR(I)
        QCur(I) = QCur(I) + 3.0D0 * RR(I) - SS(I)
        XStep(NStep,I,2) = XVal(I)
      END DO
      CALL btot(XVal, t, tilt, kp, den, vel, dst, outer, BVal,
     &          BStep(NStep,2))
      SS(1) = 1.0D0 
      SS(2) = 1.0D0 / XVal(1)
      SS(3) = 1.0D0 / XVal(1) / DSIN(XVal(2))
      DO I=1,3
        SS(I) = P29DS * SS(I) * BVal(I) / BStep(NStep,2)
        RR(I) = SS(I) - P29 * QCur(I)
        XVal(I) = XVal(I) + RR(I)
        QCur(I) = QCur(I) + 3.0D0 * RR(I) - SS(I)
        XStep(NStep,I,3) = XVal(I)
      END DO
      CALL btot(XVal, t, tilt, kp, den, vel, dst, outer, BVal, 
     &          BStep(NStep,3))
      SS(1) = 1.0D0 
      SS(2) = 1.0D0 / XVal(1)
      SS(3) = 1.0D0 / XVal(1) / DSIN(XVal(2))
      DO I=1,3
        SS(I) = OP7DS * SS(I) * BVal(I) / BStep(NStep,3)
        RR(I) = SS(I) - OP7 * QCur(I)
        XVal(I) = XVal(I) + RR(I)
        QCur(I) = QCur(I) + 3.0D0 * RR(I) - SS(I)
        XStep(NStep,I,4) = XVal(I)
      END DO
      CALL btot(XVal, t, tilt, kp, den, vel, dst, outer, BVal, 
     &          BStep(NStep,4))
      SS(1) = 1.0D0 
      SS(2) = 1.0D0 / XVal(1)
      SS(3) = 1.0D0 / XVal(1) / DSIN(XVal(2))
      DO I=1,3
        SS(I) = P5DS * SS(I) * BVal(I) / BStep(NStep,4)
        RR(I) = (SS(I)-QCur(I)) / 3.0D0
        XVal(I) = XVal(I) + RR(I)
        QCur(I) = QCur(I) + 3.0D0 * RR(I) - SS(I)
        XStep(NStep+1,I,1) = XVal(I)
      END DO
      NStep = NStep+1
C Save the current distance along the field line.
      SVal(NStep) = SVal(NStep-1) + DStep
C Obtain the current values of the magnetic field.
      CALL btot(XVal, t, tilt, kp, den, vel, dst, outer, BVal, 
     &          BStep(NStep,1))
      BVect(NStep,1) = BStep(NStep,1)
      DO I=1,3
        BVect(NStep,I+1) = BVal(I)
      END DO
C If outside integration limits set error flag to 2.
      IF (XVAL(1) .GT. 8.0D0) THEN
        cs = dcos((ut+lon)*DEGRAD)
        if (xval(1) .gt. rmagp(cs, fkp)) then
          IErFlg = 2
        end if
C Stay within valid region.
      ELSE IF (BStep(NStep,1) .LT. 0.00004D0) THEN
        IErFlg = 3
C If too close to Earth's surface set flag.
c      ELSE IF (Xval(1) .LT. 1.0D0) THEN
c        IErFlg = 5
      END IF

      RETURN
      END

C//////////////////////// SECOND ADIABATIC INVARIANT ///////////////////
C
      SUBROUTINE INVR(BMax, NStep, BStep, SVal, SavJ)
C
C>>>>>Purpose
C        To calculate the value of the second invariant
C
C>>>>>Method
C        Use the values stored in the S and BStep arrays to evaluate the
C        integral SQRT(1-BStep/BMax) along the field line. Use the same 
C        integration method (Gills method) used in integrating the field line
C
C>>>>>Input -- COMMON block INTPAR
C        NStep  the number of integration steps
C        BMax   the value of the maximum magnetic field (the point where 
C               the particle has its mirror point)
C        BStep  a real 2 dimensioned array containing all of the magnetic 
C               field magnitudes calculated in the field line integration
C        SVal   a real array containing all the distance values calculated 
C               in the field line integration SVal(NStep) is the distance
C               from the integration start to the point where the field
C               has value BStep(N,1)
C
C>>>>>Output -- COMMON block INTPAR
C        SavJ   the values of the second invariant integration at each 
C               integration step. SavJ(NStep) contains the best approximation 
C               to the value of the second invariant. the saving of the steps
C               permits the use of interpolation schemes to obtain a more 
C               accurate value of the invariant
C
C>>>>>Calling subroutines
C        SUBROUTINE FLDINT
C
C>>>>>Constants
C        CON(4) .5, 1.0-SQRT(.5), 1.0+SQRT(.5), .5
C-----------------------------------------------------------------------

      IMPLICIT NONE

C     ..Arguments
      REAL*8  BMax, BStep(100,4), SVal(100), SavJ(100) 
      INTEGER*4 NStep

C     ..Working variables
      REAL*8  Root, Temp
      INTEGER*4 I, K

C     ..Constants
      REAL*8  CON(4)
      DATA  CON / 0.5D0, 0.29289322D0, 1.70710678D0, 0.5D0 /
C=======================================================================
      SavJ(2) = 0.0D0
      DO K=2,NStep-1
        Temp = 0.0D0
        DO I=1,4
          IF (BStep(K,I) .LT. BMax) THEN
            Root = DSQRT(1.0D0-BStep(K,I)/BMax)
            Temp = Temp + CON(I) * Root
          END IF
        END DO
        SavJ(K+1) = SavJ(K) + Temp * (SVal(K+1)-SVal(K)) / 3.0D0
      END DO

      RETURN
      END

C/////////////////////////////// INTERPOLATION /////////////////////////
C
      SUBROUTINE INTRP(AA, CC, DD, EE, IfFlg)
C
C>>>>>Purpose
C        Interpolation routine
C
C>>>>>Method
C        Given a set of three x, y point pairs, intrp finds the solution
C        to the three linear equations expressing the logarithm of the
C        dependent variable y as a second order polynomial of the
C        independent variable XVal. (LOG Y = A*XVal**2 +B*XVal +C)
C        using the binomial formula, XVal can then be evaluated at a
C        specified value of Y1
C        XVal = (-B +- SQRT(B**2-4*A*(C-LOG(Y1))))/(2*A)
C
C>>>>>Input -- argument list
C        AA     a real array containing the three values of the dependent 
C               variable
C        CC     a real array containing the three coresponding values of 
C               the independent variable
C        IfFlg  a flow control variable
C               if IfFlg < 0 fit the polynomial to CC and B and find the
C               minimum value of the dependent variable
C               if IfFlg > 0 use the binomial formula to to find the value of
C               the independent variable when the dependent variable
C               has the value d. choose the root that is closest to cc(j)
C        DD     when IfFlg > 0, DD is used for input. It is the value of the
C               dependent variable where the solution to the dependent
C               variable is wanted
C
C>>>>>Output -- argument list
C        DD     when IfFlg < 0, DD outputs the value of the
C               dependent variable where the function is a minimum
C        EE     when IfFlg < 0, EE outputs the value of the
C               independent variable where the function is a minimum
C               when IfFlg > 0, EE outputs the value of the
C               independent variable where the function has the value d
C
C>>>>>Calling subroutines
C        SUBROUTINE FLDINT
C
C>>>>>Variables
C        X2, X3, Y1, Y2, Y3, D3 are used by the linear equation solution
C               to minimize computer time
C        ACoef, BCoef, CCoef  the three polynomial cooeficients
C        Dist   BCoef**2-4*ACoef*CCoef
C        SA, SB the two roots of the polynomial
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

C     ..Arguments
      REAL*8  AA(3), CC(3), DD, EE
      INTEGER*4 IfFlg

C     ..Working variables
      REAL*8  Y1, Y2, Y3, X2, X3, D3, ACoef, BCoef, CCoef, Dist
      REAL*8  SA, SB, XVal
C=======================================================================
C Set up the initial variables, move the origin of the independent variable 
C to CC(1).
      Y1 = DLOG(AA(1))
      Y2 = DLOG(AA(2))
      Y3 = DLOG(AA(3))
      X2 = CC(2) - CC(1)
      X3 = CC(3) - CC(1)
C Solve the linear equations.
      D3 = (X3-X2) * X2 * X3
      ACoef = (X3*(Y1-Y2)+X2*(Y3-Y1)) / D3
      BCoef = (X3**2*(Y2-Y1)-X2**2*(Y3-Y1)) / D3
C If IfFlg the flow control variable is < 0 branch to minimum evaluation routine
      IF (IfFlg .LT. 0) GO TO 100
      CCoef = Y1 - DLOG(DD)
      Dist = BCoef**2 - 4.0D0 * ACoef * CCoef
C If Dist is negative no solution exist, exchange dependent and independent 
C variable roles and try another solution.
      IF (Dist .LT. 0.0D0) GO TO 200
      Dist = DSQRT(Dist)
C Obtain the two roots.
      IF (ACoef .EQ. 0.0D0) THEN
        EE = CCoef / BCoef + CC(1)
      ELSE
        SA = (-BCoef+Dist) / (2.0D0*ACoef) + CC(1)
        SB = (-BCoef-Dist) / (2.0D0*ACoef) + CC(1)
        EE = SA
C Find the root closest to cc(j).
        IF (DABS(SB-CC(IfFlg)) .LT. DABS(SA-CC(IfFlg))) EE = SB
      END IF
      RETURN
C Find the values at the minimum.
  100 XVal = -BCoef / (2.0D0*ACoef)
      EE = XVal + CC(1)
      DD = DEXP(ACoef*XVal**2+BCoef*XVal+Y1)
      RETURN
C Alternate interpolation scheme placed here as a safeguard against a strange 
C field configuration causing an imaginary solution (exchange the rolesof  
C dependent and independent variables).
 200  Y1 = CC(1)
      Y2 = CC(2)
      Y3 = CC(3)
      X2 = AA(2) - AA(1)
      X3 = AA(3) - AA(1)
      D3 = (X3-X2) * X2 * X3
      ACoef = (X3*(Y1-Y2)+X2*(Y3-Y1)) / D3
      BCoef = (X3**2*(Y2-Y1)-X2**2*(Y3-Y1)) / D3
      EE = (ACoef*(DD-AA(1))+BCoef) * (DD-AA(1)) + Y1

      RETURN
      END

      REAL*8 FUNCTION HILTON(GMAGMO, B, I)
C-----------------------------------------------------------------------
C Calculation of the parameter L in function of I and B with the method of
C HILTON (cf. TREND TN1 eqs. 2.30 and 2.31; HILTON,H.H.,JGR,76,6952,1971).
C OCT 89 : P.DOMANGE & J.LEMAIRE (IASB): PROGRAM HILTON'S ALGORITHM TO 
C                                        CALCULATE McILWAIN'S L-PARAMETER.

      IMPLICIT NONE

C INPUT :
      real*8  gmagmo, b, i 
C LOCAL :
      real*8  eq230, g

      g = (i**3) * b / gmagmo
      eq230 = 1.0D0 + 1.350474D0 * g**(1.0D0/3.0D0) + 0.465380D0 * 
     &        g**(2.0D0/3.0D0) + 0.047546D0 * g
      hilton = (eq230*gmagmo/b)**(1.0D0/3.0D0)
     
      return
      end

C-----------------------------------------------------------------------
      subroutine btot(geo, t, tilt, kp, den, vel, dst, outer, b2, b)
C CALCULATE TOTAL MAGNETIC FIELD INTENSITY

      IMPLICIT NONE

C INPUT:
      REAL*8  geo(3), t(3,3), tilt, den, vel, dst
      INTEGER*4 kp, outer
C OUTPUT:
      REAL*8  b, b2(3)
C LOCAL:
      REAL*8  br, bt, bp, rre, theta, phi, dbr, dbt, dbp

      rre = geo(1)
      theta = geo(2)
      phi = geo(3)
      call bint(rre, theta, phi, br, bt, bp)
      if (outer .gt. 0) then
        call bext(rre, theta, phi, t, tilt, kp, den, vel, dst, outer,
     &            dbr, dbt, dbp)
        br = br + dbr
        bt = bt + dbt
        bp = bp + dbp
      end if

      b = DSQRT(br**2 + bt**2 + bp**2)
      b2(1) = br
      b2(2) = bt
      b2(3) = bp

      return
      end

C-----------------------------------------------------------------------
      REAL*8 FUNCTION RMAGP(CS, FKP)
C-----------------------------------------------------------------------

C 12/5/86       CEM   moved slightly inward
C 6/11/86       CEM   Average magnetopause location a la Fairfield JGR 76.28
C 5  SEP 1988   J.LEMAIRE (IASB) receives this routine from Carl McIlwain.
C 30 JUL 1990   J.LEMAIRE & JBY add Kp value in list of arguments.

C-----------------------------------------------------------------------

      IMPLICIT NONE

C Input
      REAL*8 cs, fkp
C Local
      REAL*8 rmp1, rmp2, rmp3, rmp4, rmp5, rmp6, rmp7
      REAL*8 gs, sn, aa, bb

      data rmp1 /.028D0/, rmp2 /.35D0/, rmp3 /-.58D0/, rmp4 /17.87D0/
      data rmp5 /-233.7D0/, rmp6 /1.057D0/, rmp7 /.0526D0/ 
                                      ! more consistent with T-U
      gs = -cs                        ! Noon is zero in orig. coord.

      sn = dsqrt(1.0D0-gs*gs)
      aa = 0.5D0 * (rmp3*sn+rmp4*gs) / (sn*sn+rmp1*sn*gs+rmp2*gs*gs)

      bb = aa * aa - rmp5 / (sn*sn+rmp1*sn*gs+rmp2*gs*gs)

      if (bb .lt. 0.0D0) then
        rmagp = 999.0D0
      else
        rmagp = (-aa+dsqrt(bb)) * (rmp6-rmp7*fkp/(1.0D0+0.1D0*fkp))
      end if

      return
      end

      subroutine bext(rre, theta, phi, t, tilt, kp, den, vel, 
     & dst, outer, dbr, dbt, dbp)

c
c Dummy routine provided to stop link errors ORW 20/11/95
c
c rew write(*,*) 'error dummy bext called'
c rew added declarations to avoid compiler warnings
      REAL*8  t(3,3), tilt, den, vel, dst
      INTEGER*4 kp, outer
      REAL*8  rre, theta, phi, dbr, dbt, dbp

      end

C***********************************************************************
C*  Institute:     MPE               *                *                *
C*                                   *                *   SUBROUTINE   *
C*  DDDDDD         AA       LLL      *       GRO      *     P4TRNS     *
C*  DDDDDDD      AA  AA     LLL      *     COMPTEL    *                *
C*  DD    DD    AA    AA    LLL      *                * First Version  *
C*  DD    DD    AA    AA    LLL      ******************                **
C*  DD    DD    AAAAAAAA    LLL      *                * Author:         *
C*  DD    DD    AAAAAAAA    LLL      *                * Georg           *
C*  DD    DD    AA    AA    LLL      *    COMPASS     * Weidenspointner *
C*  DD    DD    AA    AA    LLL      *                *                **
C*  DDDDDDD     AA    AA    LLLLLLLL *                * Date :         *
C*  DDDDDD      AA    AA    LLLLLLLL *                *  04-12-95      *
C*                                   *                *                *
C*  Data        Access      Layer    *                *                *
C***********************************************************************
C*                                                                     *
C*  Note: code was provided by Kevin Bennett, ESTEC                    *
C*                                                                     *
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
C   Description: tbd
C***********************************************************************


C-----------------------------------------------------------------------

      SUBROUTINE SOLA(DAY, DSOL)

C  ANALYTICAL SUN EPHEMERIS IN THE MEAN GEI SYSTEM OF DATE.
C  THE MOON POSITION ACCURACY IS BETTER THAN 0.1 DEG AND
C  THE SUN ACCURACY IS ABOUT 0.01 DEG.

C  REF: VANFLANDERN, PULKKINEN: LOW-PRECISION FORMULAE FOR PLANETARY
C       POSITIONS. ASTROPH. J. SUPPL. 41 PP. 391-411, NOV. 1979

C     DESCRIPTION OF PARAMETERS:
C  INPUT:
C   DAY  =  TIME IN MODIFIED JULIAN DAYS (FROM 1950)
C  OUTPUT:
C   DLUN(1)..(4)  =  X, Y, Z - POSITION OF THE MOON IN KM,
C     THE 4TH COMPONENT IS THE DISTANCE TO THE MOON
C   DSOL(1)..(4)  =  X, Y, Z - POSITION OF THE SUN IN KM,
C     THE 4TH COMPONENT IS THE DISTANCE TO THE SUN
C
C  JUN 92: D. HEYNDERICKX (BIRA) : DISCARDED CALCULATION OF POSITION OF MOON
C                                  OUTPUT IS DSOL(1)...(3), NORMALIZED

      IMPLICIT NONE

      REAL*8 DSOL(3), PI, ECL, CECL, SECL, D, DAY, X7, X8, AS

************************************************************************
*
*    CONSTANTS TAKEN FROM SUBROUTINE COOT
*    ************************************
*
      PI = 4.0D0 * DATAN(1.0D0)
C  ECL, CECL, SECL= INCLINATION OF THE ECLIPTIC AND ITS COS AND SIN .
C  REF: THE ASTRONOMICAL EPHEMERIS 1978, CALCULATED FOR NOV 1982.
      ECL = 0.234415117563D+02 * PI / 180.0D0
      CECL = DCOS(ECL)
      SECL = DSIN(ECL)
*
*
************************************************************************

C  CONVERT TO JULIAN DAY OF ANNO 2000:
      D = DAY - 18262.5D0
      X7 = DMOD(0.779072D0+0.273790931D-2*D, 1.0D0) * 2.0D0 * PI
      X8 = DMOD(0.993126D0+0.27377785D-2*D, 1.0D0) * 2.0D0 * PI
C  SUN POSITION
      AS = X7 + DSIN(X8) * (335.0D-4+698.0D-6*DCOS(X8))
      DSOL(1) = DCOS(AS)
      DSOL(2) = CECL * DSIN(AS)
      DSOL(3) = SECL * DSIN(AS)
      
      RETURN
      END


C-----------------------------------------------------------------------
      SUBROUTINE GHA(amjd, strad)

C CALCULATES GREENWICH HOUR ANGLE OF EQUINOX

      IMPLICIT NONE

C INPUT:
      REAL*8 amjd
C OUTPUT:
      REAL*8 strad
C LOCAL:
      REAL*8 amjdz, resjd, tu, sthr, sth, stdeg

      AMJDZ = DINT(AMJD)
      RESJD = AMJD - AMJDZ
      TU = (AMJDZ-18262.5D0) / 36525.0D0
      STHR = ((-1.722D-9*TU+2.586222D-5)*TU+2.400051336907D3) * TU +
     &       6.69737455833D0
      IF (STHR .LT. 0.0D0) THEN
        STH = (IDINT(-STHR/24.0D0)+1.0D0) * 24.0D0 + STHR
      ELSE
        STH = STHR - IDINT(STHR/24.0D0) * 24.0D0
      END IF
      STH = STH + RESJD * 2.406570721D1
      STDEG = STH * 15.0D0
      STRAD = STDEG * 1.7453292519943D-2
      RETURN
      
      END


C-----------------------------------------------------------------------
      SUBROUTINE CONVRT(I, GDLAT, ALT, GCLAT, RKM)

C  DOUBLE PRECISION 
C  CONVERTS SPACE POINT FROM GEODETIC TO GEOCENTRIC OR VICE VERSA
C  REFERENCE GEOID IS THAT ADOPTED BY IAU IN 1964
C  A=6378.16, B=6356.7746, F=1/298.25
C  I = 1  GEODETIC TO GEOCENTRIC
C  I = 2  GEOCENTRIC TO GEODETIC
C  GDLAT = GEODETIC LATITUDE IN DEGREES
C  ALT = ALTITUDE ABOVE GEOID IN KILOMETERS
C  GCLAT = GEOCENTRIC LATITUDE IN DEGREES
C  RKM = GEOCENTRIC DISTANCE IN KILOMETERS

      IMPLICIT NONE

      REAL*8 GDLAT, ALT, GCLAT, RKM, A, RAD, AB2, EP2, SINLAT, COSLAT
      REAL*8 SINTH, COSTH, RGEOID, X, Y, RER, A2, A4, A6, A8, SCL, CCL
      REAL*8 S2CL, C2CL, S4CL, C4CL, S6CL, S8CL, DLTCL
      INTEGER*4 I

      DATA A, AB2, EP2 /6378.16D0, 1.0067397D0, 0.0067397D0/

      RAD = 45.0D0 / DATAN(1.0D0)
      IF (I .EQ. 1) THEN
        SINLAT = DSIN(GDLAT/RAD)
        COSLAT = DSQRT(1.0D0-SINLAT**2)
        COSTH = SINLAT / DSQRT((AB2*COSLAT)**2+SINLAT**2)
        SINTH = DSQRT(1.0D0-COSTH**2)
        RGEOID = A / DSQRT(1.0D0+EP2*COSTH**2)
        X = RGEOID * SINTH + ALT * COSLAT
        Y = RGEOID * COSTH + ALT * SINLAT
        RKM = DSQRT(X*X+Y*Y)
        GCLAT = RAD * DATAN(Y/X)
      ELSE
        RER = RKM / A
C  SEE ASTRON. J. VOL.66 P.15, 1961, FOR FORMULAS BELOW.
        A2 = ((-1.4127348D-8/RER+0.94339131D-8)/RER+0.33523288D-2) / RER
        A4 = (((-1.2545063D-10/RER+0.11760996D-9)/RER+0.11238084D-4)/RER
     &       -0.2814244D-5) / RER
        A6 = ((54.939685D-9/RER-28.301730D-9)/RER+3.5435979D-9) / RER
        A8 = (((320.0D0/RER-252.0D0)/RER+64.0D0)/RER-5.0D0) / RER *
     &       0.98008304D-12
        SCL = DSIN(GCLAT/RAD)
        CCL = DSQRT(1.0D0-SCL*SCL)
        S2CL = 2.0D0 * SCL * CCL
        C2CL = 2.0D0 * CCL * CCL - 1.0D0
        S4CL = 2.0D0 * S2CL * C2CL
        C4CL = 2.0D0 * C2CL * C2CL - 1.0D0
        S8CL = 2.0D0 * S4CL * C4CL
        S6CL = S2CL * C4CL + C2CL * S4CL
        DLTCL = S2CL * A2 + S4CL * A4 + S6CL * A6 + S8CL * A8
        GDLAT = DLTCL * RAD + GCLAT
        ALT = RKM - A / DSQRT(1.0D0+EP2*SCL*SCL)
      END IF

      RETURN
      END


C-----------------------------------------------------------------------
      SUBROUTINE TRANS1(strad, dsol, t, tilt, colat, elong)

C CONVERTS GEI COORDINATES OF SUN TO GEO COORDINATES AND
C FORMS TRANSFORMATION MATRIX FROM GEO COORDINATES TO GSM COORDINATES

      IMPLICIT NONE

C INPUT:
      REAL*8 DSOL(3), STRAD, colat, elong
C OUTPUT:
      REAL*8 T(3,3), TILT
C LOCAL:
      REAL*8 sun(3), sst, cst, slt, clt, sln, cln
      REAL*8 aly, amy, any, amag, PCLAT, PELON, RADD, Dx, Dy, Dz, DGSMX

      RADD = 45.0D0 / DATAN(1.0D0)
      PCLAT = COLAT / RADD         ! Updated geocentric coordinates of
      PELON = ELONG / RADD         ! geomagnetic dipole pole
      SST = DSIN(STRAD)
      CST = DCOS(STRAD)
C CONVERT GEI COORDINATES OF SUN TO GEO COORDINATES
      SUN(1) = CST * DSOL(1) + SST * DSOL(2)
      SUN(2) = -SST * DSOL(1) + CST * DSOL(2)
      SUN(3) = DSOL(3)
C X-AXIS OF GSM COORDINATE SYSTEM COINCIDES WITH SUN DIRECTION
      T(1,1) = SUN(1)
      T(1,2) = SUN(2)
      T(1,3) = SUN(3)
C CALCULATE CARTESIAN GEO COORDINATES OF BOREAL DIPOLE POLE
      SLT = DSIN(PCLAT)
      CLT = DCOS(PCLAT)
      SLN = DSIN(PELON)
      CLN = DCOS(PELON)
      Dx = SLT * CLN
      Dy = SLT * SLN
      Dz = CLT
C CALCULATE GEO COORDINATES OF Y-AXIS OF GSM COORDINATE SYSTEM
      ALY = Dy * SUN(3) - Dz * SUN(2)
      AMY = Dz * SUN(1) - Dx * SUN(3)
      ANY = Dx * SUN(2) - Dy * SUN(1)
      AMAG = DSQRT(ALY*ALY+AMY*AMY+ANY*ANY)      ! NORMALIZATION
      T(2,1) = ALY / AMAG
      T(2,2) = AMY / AMAG
      T(2,3) = ANY / AMAG
C CALCULATE GEO COORDINATES OF Z-AXIS OF GSM COORDINATE SYSTEM
      T(3,1) = T(1,2) * T(2,3) - T(1,3) * T(2,2)
      T(3,2) = T(1,3) * T(2,1) - T(1,1) * T(2,3)
      T(3,3) = T(1,1) * T(2,2) - T(1,2) * T(2,1)
C DIPOLE TILT ANGLE IS GSM COLATITUDE OF DIPOLE AXIS
      TILT = (DACOS(T(3,1)*Dx+T(3,2)*Dy+T(3,3)*Dz)) * RADD
C INVERT SIGN OF TILT WHEN DIPOLE AXIS IS TILTED AWAY FROM THE SUN,
C I.E. WHEN GSM X COORDINATE OF DIPOLE AXIS IS NEGATIVE
      dgsmx = T(1,1) * Dx + T(1,2) * Dy + T(1,3) * Dz
      if (dgsmx .lt. 0.0D0) tilt = -tilt
      RETURN
      END


C-----------------------------------------------------------------------
      SUBROUTINE TRANS2(strad, dsol, t, tilt, colat, elong)

C CONVERTS GEI COORDINATES OF SUN TO GEO COORDINATES AND
C FORMS TRANSFORMATION MATRIX FROM GEO COORDINATES TO SM COORDINATES

      IMPLICIT NONE

C INPUT:
      REAL*8 DSOL(3), STRAD, colat, elong
C OUTPUT:
      REAL*8 T(3,3), TILT
C LOCAL:
      REAL*8 sun(3), sst, cst, slt, clt, sln, cln
      REAL*8 aly, amy, any, amag, PCLAT, PELON, RADD

      RADD = 45.0D0 / DATAN(1.0D0)
      PCLAT = COLAT / RADD         ! Updated geocentric coordinates of
      PELON = ELONG / RADD         ! geomagnetic dipole pole

      SST = DSIN(STRAD)
      CST = DCOS(STRAD)
C CONVERT GEI COORDINATES OF SUN TO GEO COORDINATES
      SUN(1) = CST * DSOL(1) + SST * DSOL(2)
      SUN(2) = -SST * DSOL(1) + CST * DSOL(2)
      SUN(3) = DSOL(3)
C CALCULATE CARTESIAN GEO COORDINATES OF BOREAL DIPOLE POLE
      SLT = DSIN(PCLAT)
      CLT = DCOS(PCLAT)
      SLN = DSIN(PELON)
      CLN = DCOS(PELON)
      T(3,1) = SLT * CLN
      T(3,2) = SLT * SLN
      T(3,3) = CLT
C CALCULATE GEO COORDINATES OF Y-AXIS OF SM COORDINATE SYSTEM
      ALY = T(3,2) * SUN(3) - T(3,3) * SUN(2)
      AMY = T(3,3) * SUN(1) - T(3,1) * SUN(3)
      ANY = T(3,1) * SUN(2) - T(3,2) * SUN(1)
      AMAG = DSQRT(ALY*ALY+AMY*AMY+ANY*ANY)      ! NORMALIZATION
      T(2,1) = ALY / AMAG
      T(2,2) = AMY / AMAG
      T(2,3) = ANY / AMAG
C CALCULATE GEO COORDINATES OF X-AXIS OF SM COORDINATE SYSTEM
      T(1,1) = T(2,2) * T(3,3) - T(2,3) * T(3,2)
      T(1,2) = T(2,3) * T(3,1) - T(2,1) * T(3,3)
      T(1,3) = T(2,1) * T(3,2) - T(2,2) * T(3,1)
C DIPOLE TILT ANGLE IS COMPLEMENT OF COLATITUDE OF SUN DIRECTION
      TILT = (DACOS(T(3,1)*SUN(1)+T(3,2)*SUN(2)+T(3,3)*SUN(3))) * RADD
      TILT = 90.0D0 - TILT
      RETURN
      END
