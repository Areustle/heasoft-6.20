C SHELLIG.FOR, Version 2.0, January 1992
C
C 11/1/91 SHELLG: lowest starting point for B0 search is 2  
C 1/27/92 Adopted to IGRF-91 coeffcients model
C 2/5/92  Reduce variable-names: INTER(P)SHC,EXTRA(P)SHC,INITI(ALI)ZE
C
C*********************************************************************
C  SUBROUTINES FINDB0, SHELLG, STOER, FELDG, FELDCOF, GETSHC,        *
C       INTERSHC, EXTRASHC, INITIZE                                  *
C*********************************************************************
C*********************************************************************
C
C
      SUBROUTINE FINDB0(STPS,BDEL,VALUE,BEQU,RR0)
C--------------------------------------------------------------------
C FINDS SMALLEST MAGNETIC FIELD STRENGTH ON FIELD LINE
C
C INPUT:   STPS   STEP SIZE FOR FIELD LINE TRACING
C       COMMON/FIDB0/
C          SP     DIPOLE ORIENTED COORDINATES FORM SHELLG; P(1,*),
C                 P(2,*), P(3,*) CLOSEST TO MAGNETIC EQUATOR 
C          BDEL   REQUIRED ACCURACY  = [ B(LAST) - BEQU ] / BEQU  
C                 B(LAST)  IS FIELD STRENGTH BEFORE BEQU
C
C OUTPUT:  VALUE  =.FALSE., IF BEQU IS NOT MINIMAL VALUE ON FIELD LINE
C          BEQU   MAGNETIC FIELD STRENGTH AT MAGNETIC EQUATOR
C          RR0    EQUATORIAL RADIUS NORMALIZED TO EARTH RADIUS
C          BDEL   FINAL ACHIEVED ACCURACY
C--------------------------------------------------------------------
      DIMENSION         P(8,4),SP(3)
      LOGICAL           VALUE
      COMMON/FIDB0/     SP
C
      STEP=STPS
      IRUN=0
7777  IRUN=IRUN+1
      IF(IRUN.GT.5) THEN
        VALUE=.FALSE.
        GOTO 8888
        ENDIF
C*********************FIRST THREE POINTS 
      P(1,2)=SP(1)
      P(2,2)=SP(2)
      P(3,2)=SP(3)
      STEP=-SIGN(STEP,P(3,2))
      CALL STOER(P(1,2),BQ2,R2)
      P(1,3)=P(1,2)+0.5*STEP*P(4,2)
      P(2,3)=P(2,2)+0.5*STEP*P(5,2)
      P(3,3)=P(3,2)+0.5*STEP
      CALL STOER(P(1,3),BQ3,R3)
      P(1,1)=P(1,2)-STEP*(2.*P(4,2)-P(4,3))
      P(2,1)=P(2,2)-STEP*(2.*P(5,2)-P(5,3))
      P(3,1)=P(3,2)-STEP
      CALL STOER(P(1,1),BQ1,R1)
      P(1,3)=P(1,2)+STEP*(20.*P(4,3)-3.*P(4,2)+P(4,1))/18.
      P(2,3)=P(2,2)+STEP*(20.*P(5,3)-3.*P(5,2)+P(5,1))/18.
      P(3,3)=P(3,2)+STEP
      CALL STOER(P(1,3),BQ3,R3)
C******************INVERT SENSE IF REQUIRED
      IF(BQ3.LE.BQ1) GOTO 2
        STEP=-STEP
        R3=R1
        BQ3=BQ1
        DO 1 I=1,5
                ZZ=P(I,1)
                P(I,1)=P(I,3)
1               P(I,3)=ZZ
C******************INITIALIZATION 
2       STEP12=STEP/12.
        VALUE=.TRUE.
        BMIN=1.E4
        BOLD=1.E4
C******************CORRECTOR (FIELD LINE TRACING)
        N=0
5555  P(1,3)=P(1,2)+STEP12*(5.*P(4,3)+8.*P(4,2)-P(4,1))
        N=N+1
      P(2,3)=P(2,2)+STEP12*(5.*P(5,3)+8.*P(5,2)-P(5,1))
C******************PREDICTOR (FIELD LINE TRACING)
      P(1,4)=P(1,3)+STEP12*(23.*P(4,3)-16.*P(4,2)+5.*P(4,1))
      P(2,4)=P(2,3)+STEP12*(23.*P(5,3)-16.*P(5,2)+5.*P(5,1))
      P(3,4)=P(3,3)+STEP
      CALL STOER(P(1,4),BQ3,R3)
        DO 1111 J=1,3
        DO 1111 I=1,8
1111    P(I,J)=P(I,J+1)
        B=SQRT(BQ3)
        IF(B.LT.BMIN) BMIN=B
        IF(B.LE.BOLD) THEN
                BOLD=B
                ROLD=1./R3
                SP(1)=P(1,4)
                SP(2)=P(2,4)
                SP(3)=P(3,4)
                GOTO 5555
                ENDIF
        IF(BOLD.NE.BMIN) THEN
                VALUE=.FALSE.
                ENDIF
        BDELTA=(B-BOLD)/BOLD
        IF(BDELTA.GT.BDEL) THEN
                STEP=STEP/10.
                GOTO 7777
                ENDIF
8888    RR0=ROLD
        BEQU=BOLD
        BDEL=BDELTA
        RETURN
        END
C
C
      SUBROUTINE SHELLG(GLAT,GLON,ALT,FL,ICODE,B0)
C--------------------------------------------------------------------
C CALCULATES L-VALUE FOR SPECIFIED GEODAETIC COORDINATES, ALTITUDE
C AND GEMAGNETIC FIELD MODEL.
C REF: G. KLUGE, EUROPEAN SPACE OPERATIONS CENTER, INTERNAL NOTE 
C      NO. 67, 1970.
C      G. KLUGE, COMPUTER PHYSICS COMMUNICATIONS 3, 31-35, 1972
C--------------------------------------------------------------------
C CHANGES (D. BILITZA, NOV 87):
C   - USING CORRECT DIPOL MOMENT I.E.,DIFFERENT COMMON/MODEL/
C   - USING IGRF EARTH MAGNETIC FIELD MODELS FROM 1945 TO 1990
C--------------------------------------------------------------------
C  INPUT:  ENTRY POINT SHELLG
C               GLAT  GEODETIC LATITUDE IN DEGREES (NORTH)
C               GLON  GEODETIC LONGITUDE IN DEGREES (EAST)
C               ALT   ALTITUDE IN KM ABOVE SEA LEVEL
C
C          ENTRY POINT SHELLC
C               V(3)  CARTESIAN COORDINATES IN EARTH RADII (6371.2 KM)
C                       X-AXIS POINTING TO EQUATOR AT 0 LONGITUDE
C                       Y-AXIS POINTING TO EQUATOR AT 90 LONG.
C                       Z-AXIS POINTING TO NORTH POLE
C
C          DIMO       DIPOL MOMENT IN GAUSS (NORMALIZED TO EARTH RADIUS) 
C
C          COMMON 
C               X(3)    NOT USED
C               H(144)  FIELD MODEL COEFFICIENTS ADJUSTED FOR SHELLG
C-----------------------------------------------------------------------
C  OUTPUT: FL           L-VALUE
C          ICODE        =1 NORMAL COMPLETION
C                       =2 UNPHYSICAL CONJUGATE POINT (FL MEANINGLESS)
C                       =3 SHELL PARAMETER GREATER THAN LIMIT UP TO
C                          WHICH ACCURATE CALCULATION IS REQUIRED;
C                          APPROXIMATION IS USED.
C          B0           MAGNETIC FIELD STRENGTH IN GAUSS
C-----------------------------------------------------------------------
      DIMENSION         V(3),U(3,3),P(8,100),SP(3)
      COMMON            X(3),H(144)
      COMMON/FIDB0/     SP
      COMMON/GENER/     UMR,ERA,AQUAD,BQUAD
      COMMON/DIPMO/     DIMO
C
C-- RMIN, RMAX ARE BOUNDARIES FOR IDENTIFICATION OF ICODE=2 AND 3
C-- STEP IS STEP SIZE FOR FIELD LINE TRACING
C-- STEQ IS STEP SIZE FOR INTEGRATION
C 
      DATA RMIN,RMAX    /0.05,1.01/
      DATA STEP,STEQ    /0.20,0.03/
c 26Jun2001 (MJT) This isn't doing much good up here since it's not 
c                 used until after the shellc entry point...
C*****ENTRY POINT  SHELLG  TO BE USED WITH GEODETIC CO-ORDINATES
      RLAT=GLAT*UMR
      CT=SIN(RLAT)                                             
      ST=COS(RLAT)                                            
      D=SQRT(AQUAD-(AQUAD-BQUAD)*CT*CT)
      X(1)=(ALT+AQUAD/D)*ST/ERA
      X(3)=(ALT+BQUAD/D)*CT/ERA
      RLON=GLON*UMR
      X(2)=X(1)*SIN(RLON)                                    
      X(1)=X(1)*COS(RLON)                                   
      GOTO9                                                
      ENTRY SHELLC(V,FL,ICODE,B0)                         
C*****ENTRY POINT  SHELLC  TO BE USED WITH CARTESIAN CO-ORDINATES
c     MJT 23July96 initializing iequ (g77 doesn't)
c     IEQU=0
c     MJT 26June2001 moved bequ initialization (see above)
c     BEQU=1.E10
      X(1)=V(1)                                                 
      X(2)=V(2)                                                
      X(3)=V(3)                                               
C*****CONVERT TO DIPOL-ORIENTED CO-ORDINATES                 
      DATA U/                 +0.3511737,-0.9148385,-0.1993679,
     A                        +0.9335804,+0.3583680,+0.0000000,
     B                        +0.0714471,-0.1861260,+0.9799247/
9     RQ=1./(X(1)*X(1)+X(2)*X(2)+X(3)*X(3))
C     REW 2002 Feb 21
C     here IEQU and BEQU are initialized for both SHELLG and SHELLC
      IEQU = 0
      BEQU=1.E10
      R3H=SQRT(RQ*SQRT(RQ))                                   
      P(1,2)=(X(1)*U(1,1)+X(2)*U(2,1)+X(3)*U(3,1))*R3H       
      P(2,2)=(X(1)*U(1,2)+X(2)*U(2,2)            )*R3H      
      P(3,2)=(X(1)*U(1,3)+X(2)*U(2,3)+X(3)*U(3,3))*RQ      
C*****FIRST THREE POINTS OF FIELD LINE                    
      STEP=-SIGN(STEP,P(3,2))                            
      CALL STOER(P(1,2),BQ2,R2)                         
      B0=SQRT(BQ2)                                     
      P(1,3)=P(1,2)+0.5*STEP*P(4,2)                   
      P(2,3)=P(2,2)+0.5*STEP*P(5,2)                  
      P(3,3)=P(3,2)+0.5*STEP                        
      CALL STOER(P(1,3),BQ3,R3)                    
      P(1,1)=P(1,2)-STEP*(2.*P(4,2)-P(4,3))       
      P(2,1)=P(2,2)-STEP*(2.*P(5,2)-P(5,3))                     
      P(3,1)=P(3,2)-STEP                                       
      CALL STOER(P(1,1),BQ1,R1)                               
      P(1,3)=P(1,2)+STEP*(20.*P(4,3)-3.*P(4,2)+P(4,1))/18.   
      P(2,3)=P(2,2)+STEP*(20.*P(5,3)-3.*P(5,2)+P(5,1))/18.  
      P(3,3)=P(3,2)+STEP                                   
      CALL STOER(P(1,3),BQ3,R3)                           
C*****INVERT SENSE IF REQUIRED                           
      IF(BQ3.LE.BQ1)GOTO2                               
      STEP=-STEP                                       
      R3=R1                                           
      BQ3=BQ1                                        
      DO 1 I=1,7                                    
      ZZ=P(I,1)                                    
      P(I,1)=P(I,3)                               
1     P(I,3)=ZZ                                  
C*****SEARCH FOR LOWEST MAGNETIC FIELD STRENGTH
2     IF(BQ1.LT.BEQU) THEN
        BEQU=BQ1
        IEQU=1
        ENDIF
      IF(BQ2.LT.BEQU) THEN
        BEQU=BQ2
        IEQU=2
        ENDIF
      IF(BQ3.LT.BEQU) THEN
        BEQU=BQ3
        IEQU=3
        ENDIF
C*****INITIALIZATION OF INTEGRATION LOOPS       
      STEP12=STEP/12.
      STEP2=STEP+STEP  
      STEQ=SIGN(STEQ,STEP)                                            
      FI=0.                                                          
      ICODE=1                                                       
      ORADIK=0.                                                    
      OTERM=0.                                                    
      STP=R2*STEQ                                                
      Z=P(3,2)+STP                                              
      STP=STP/0.75
      P(8,1)=STEP2*(P(1,1)*P(4,1)+P(2,1)*P(5,1))               
      P(8,2)=STEP2*(P(1,2)*P(4,2)+P(2,2)*P(5,2))              
C*****MAIN LOOP (FIELD LINE TRACING)                         
      DO 3 N=3,3333                                         
C*****CORRECTOR (FIELD LINE TRACING)                       
      P(1,N)=P(1,N-1)+STEP12*(5.*P(4,N)+8.*P(4,N-1)-P(4,N-2))          
      P(2,N)=P(2,N-1)+STEP12*(5.*P(5,N)+8.*P(5,N-1)-P(5,N-2))         
C*****PREPARE EXPANSION COEFFICIENTS FOR INTERPOLATION               
C*****OF SLOWLY VARYING QUANTITIES                                  
      P(8,N)=STEP2*(P(1,N)*P(4,N)+P(2,N)*P(5,N))                   
      C0=P(1,N-1)**2+P(2,N-1)**2                                  
      C1=P(8,N-1)                                                
      C2=(P(8,N)-P(8,N-2))*0.25                                 
      C3=(P(8,N)+P(8,N-2)-C1-C1)/6.0
      D0=P(6,N-1)                                                      
      D1=(P(6,N)-P(6,N-2))*0.5                                        
      D2=(P(6,N)+P(6,N-2)-D0-D0)*0.5                                 
      E0=P(7,N-1)
      E1=(P(7,N)-P(7,N-2))*0.5                                      
      E2=(P(7,N)+P(7,N-2)-E0-E0)*0.5                               
C*****INNER LOOP (FOR QUADRATURE)                                 
4     T=(Z-P(3,N-1))/STEP                                        
      IF(T.GT.1.)GOTO5                                          
      HLI=0.5*(((C3*T+C2)*T+C1)*T+C0)                          
      ZQ=Z*Z
      R=HLI+SQRT(HLI*HLI+ZQ)
      IF(R.LE.RMIN)GOTO30                                     
      RQ=R*R
      FF=SQRT(1.+3.*ZQ/RQ)                                             
      RADIK=B0-((D2*T+D1)*T+D0)*R*RQ*FF                               
      IF(R-RMAX)44,44,45                                             
45    ICODE=2                                                       
      RADIK=RADIK-12.*(R-RMAX)**2                                  
44    IF(RADIK+RADIK.LE.ORADIK) GOTO 10
      TERM=SQRT(RADIK)*FF*((E2*T+E1)*T+E0)/(RQ+ZQ)                
      FI=FI+STP*(OTERM+TERM)                                     
      ORADIK=RADIK                                              
      OTERM=TERM                                               
      STP=R*STEQ                                              
      Z=Z+STP                                                          
      GOTO4                                                           
C*****PREDICTOR (FIELD LINE TRACING)                                 
5     P(1,N+1)=P(1,N)+STEP12*(23.*P(4,N)-16.*P(4,N-1)+5.*P(4,N-2))  
      P(2,N+1)=P(2,N)+STEP12*(23.*P(5,N)-16.*P(5,N-1)+5.*P(5,N-2)) 
      P(3,N+1)=P(3,N)+STEP                                        
      CALL STOER(P(1,N+1),BQ3,R3)                                
C*****SEARCH FOR LOWEST MAGNETIC FIELD STRENGTH
      IF(BQ3.LT.BEQU) THEN
        IEQU=N+1
        BEQU=BQ3
        ENDIF
3     CONTINUE
10    IF(IEQU.lt.2) IEQU=2 
      SP(1)=P(1,IEQU-1)
      SP(2)=P(2,IEQU-1)
      SP(3)=P(3,IEQU-1)
      IF(ORADIK.LT.1E-15)GOTO11                                 
      FI=FI+STP/0.75*OTERM*ORADIK/(ORADIK-RADIK)              
C
C-- The minimal allowable value of FI was changed from 1E-15 to 1E-12,
C-- because 1E-38 is the minimal allowable arg. for ALOG in our envir.
C-- D. Bilitza, Nov 87.
C
11    FI=0.5*ABS(FI)/SQRT(B0)+1E-12                       
C*****COMPUTE L FROM B AND I.  SAME AS CARMEL IN INVAR.  
C
C-- Correct dipole moment is used here. D. Bilitza, Nov 87.
C
      DIMOB0=DIMO/B0
      XX=ALOG(FI*FI*FI/DIMOB0)
      IF(XX.GT.23.0) GOTO 776   
      IF(XX.GT.11.7) GOTO 775  
      IF(XX.GT.+3.0) GOTO 774    
      IF(XX.GT.-3.0) GOTO 773   
      IF(XX.GT.-22.) GOTO 772  
  771 GG=3.33338E-1*XX+3.0062102E-1                                     
      GOTO777                                                           
  772 GG=((((((((-8.1537735E-14*XX+8.3232531E-13)*XX+1.0066362E-9)*XX+  
     18.1048663E-8)*XX+3.2916354E-6)*XX+8.2711096E-5)*XX+1.3714667E-3)* 
     2XX+1.5017245E-2)*XX+4.3432642E-1)*XX+6.2337691E-1                
      GOTO777                                                         
  773 GG=((((((((2.6047023E-10*XX+2.3028767E-9)*XX-2.1997983E-8)*XX- 
     15.3977642E-7)*XX-3.3408822E-6)*XX+3.8379917E-5)*XX+1.1784234E-3)* 
     2XX+1.4492441E-2)*XX+4.3352788E-1)*XX+6.228644E-1                  
      GOTO777                                                          
  774 GG=((((((((6.3271665E-10*XX-3.958306E-8)*XX+9.9766148E-07)*XX-  
     11.2531932E-5)*XX+7.9451313E-5)*XX-3.2077032E-4)*XX+2.1680398E-3)* 
     2XX+1.2817956E-2)*XX+4.3510529E-1)*XX+6.222355E-1                 
      GOTO777                                                         
  775 GG=(((((2.8212095E-8*XX-3.8049276E-6)*XX+2.170224E-4)*XX-6.7310339
     1E-3)*XX+1.2038224E-1)*XX-1.8461796E-1)*XX+2.0007187E0             
      GOTO777                                                          
  776 GG=XX-3.0460681E0                                               
  777 FL=EXP(ALOG((1.+EXP(GG))*DIMOB0)/3.0)
      RETURN                                                            
C*****APPROXIMATION FOR HIGH VALUES OF L.                               
30    ICODE=3                                                           
      T=-P(3,N-1)/STEP                                                  
      FL=1./(ABS(((C3*T+C2)*T+C1)*T+C0)+1E-15)                          
      RETURN                                                            
      END                                                               
C
C
      SUBROUTINE STOER(P,BQ,R)                                          
C*******************************************************************
C* SUBROUTINE USED FOR FIELD LINE TRACING IN SHELLG                *
C* CALLS ENTRY POINT FELDI IN GEOMAGNETIC FIELD SUBROUTINE FELDG   *
C*******************************************************************
      DIMENSION         P(7),U(3,3)
      COMMON            XI(3),H(144)
C*****XM,YM,ZM  ARE GEOMAGNETIC CARTESIAN INVERSE CO-ORDINATES         
      ZM=P(3)                                                         
      FLI=P(1)*P(1)+P(2)*P(2)+1E-15
      R=0.5*(FLI+SQRT(FLI*FLI+(ZM+ZM)**2))
      RQ=R*R
      WR=SQRT(R)                                                     
      XM=P(1)*WR                                                    
      YM=P(2)*WR                                                   
C*****TRANSFORM TO GEOGRAPHIC CO-ORDINATE SYSTEM                  
      DATA U/                 +0.3511737,-0.9148385,-0.1993679,  
     A                        +0.9335804,+0.3583680,+0.0000000, 
     B                        +0.0714471,-0.1861260,+0.9799247/
      XI(1)=XM*U(1,1)+YM*U(1,2)+ZM*U(1,3)                     
      XI(2)=XM*U(2,1)+YM*U(2,2)+ZM*U(2,3)                    
      XI(3)=XM*U(3,1)          +ZM*U(3,3)                   
C*****COMPUTE DERIVATIVES                                  
      CALL FELDI
      Q=H(1)/RQ                                          
      DX=H(3)+H(3)+Q*XI(1)                              
      DY=H(4)+H(4)+Q*XI(2)                             
      DZ=H(2)+H(2)+Q*XI(3)                            
C*****TRANSFORM BACK TO GEOMAGNETIC CO-ORDINATE SYSTEM                 
      DXM=U(1,1)*DX+U(2,1)*DY+U(3,1)*DZ                               
      DYM=U(1,2)*DX+U(2,2)*DY                                        
      DZM=U(1,3)*DX+U(2,3)*DY+U(3,3)*DZ                             
      DR=(XM*DXM+YM*DYM+ZM*DZM)/R                                  
C*****FORM SLOWLY VARYING EXPRESSIONS                             
      P(4)=(WR*DXM-0.5*P(1)*DR)/(R*DZM)                          
      P(5)=(WR*DYM-0.5*P(2)*DR)/(R*DZM)                         
      DSQ=RQ*(DXM*DXM+DYM*DYM+DZM*DZM)
      BQ=DSQ*RQ*RQ
      P(6)=SQRT(DSQ/(RQ+3.*ZM*ZM))                                    
      P(7)=P(6)*(RQ+ZM*ZM)/(RQ*DZM)                                  
      RETURN                                                        
      END                                                          
C
C
      SUBROUTINE FELDG(GLAT,GLON,ALT,BNORTH,BEAST,BDOWN,BABS)          
C-------------------------------------------------------------------
C CALCULATES EARTH MAGNETIC FIELD FROM SPHERICAL HARMONICS MODEL
C REF: G. KLUGE, EUROPEAN SPACE OPERATIONS CENTRE, INTERNAL NOTE 61, 
C      1970.
C--------------------------------------------------------------------
C CHANGES (D. BILITZA, NOV 87):
C   - FIELD COEFFICIENTS IN BINARY DATA FILES INSTEAD OF BLOCK DATA
C   - CALCULATES DIPOL MOMENT
C 24Oct2000 (M.Tripicco) changed /MODEL/ to /GMODEL/
C--------------------------------------------------------------------
C  INPUT:  ENTRY POINT FELDG
C               GLAT  GEODETIC LATITUDE IN DEGREES (NORTH)
C               GLON  GEODETIC LONGITUDE IN DEGREES (EAST)
C               ALT   ALTITUDE IN KM ABOVE SEA LEVEL
C
C          ENTRY POINT FELDC
C               V(3)  CARTESIAN COORDINATES IN EARTH RADII (6371.2 KM)
C                       X-AXIS POINTING TO EQUATOR AT 0 LONGITUDE
C                       Y-AXIS POINTING TO EQUATOR AT 90 LONG.
C                       Z-AXIS POINTING TO NORTH POLE
C
C          COMMON BLANK AND ENTRY POINT FELDI ARE NEEDED WHEN USED
C            IN CONNECTION WITH L-CALCULATION PROGRAM SHELLG.
C       
C          COMMON /MODEL/ AND /GENER/
C               UMR     = ATAN(1.0)*4./180.   <DEGREE>*UMR=<RADIANT>
C               ERA     EARTH RADIUS FOR NORMALIZATION OF CARTESIAN 
C                       COORDINATES (6371.2 KM)
C               AQUAD, BQUAD   SQUARE OF MAJOR AND MINOR HALF AXIS FOR 
C                       EARTH ELLIPSOID AS RECOMMENDED BY INTERNATIONAL 
C                       ASTRONOMICAL UNION (6378.160, 6356.775 KM).
C               NMAX    MAXIMUM ORDER OF SPHERICAL HARMONICS
C               TIME    YEAR (DECIMAL: 1973.5) FOR WHICH MAGNETIC 
C                       FIELD IS TO BE CALCULATED
C               G(M)    NORMALIZED FIELD COEFFICIENTS (SEE FELDCOF)
C                       M=NMAX*(NMAX+2)
C------------------------------------------------------------------------
C  OUTPUT: BABS   MAGNETIC FIELD STRENGTH IN GAUSS
C          BNORTH, BEAST, BDOWN   COMPONENTS OF THE FIELD WITH RESPECT
C                 TO THE LOCAL GEODETIC COORDINATE SYSTEM, WITH AXIS
C                 POINTING IN THE TANGENTIAL PLANE TO THE NORTH, EAST
C                 AND DOWNWARD.   
C-----------------------------------------------------------------------
      DIMENSION         V(3),B(3)   
      character(12)      NAME
      COMMON            XI(3),H(144)
c     COMMON/MODEL/     NAME,NMAX,TIME,G(144)  
      COMMON/GMODEL/    NAME,NMAX,TIME,G(144)  
      COMMON/GENER/     UMR,ERA,AQUAD,BQUAD
C
C-- IS RECORDS ENTRY POINT
C
C*****ENTRY POINT  FELDG  TO BE USED WITH GEODETIC CO-ORDINATES         
      IS=1                                                             
      RLAT=GLAT*UMR
      CT=SIN(RLAT)                                                    
      ST=COS(RLAT)                                                   
      D=SQRT(AQUAD-(AQUAD-BQUAD)*CT*CT)                             
      RLON=GLON*UMR
      CP=COS(RLON)                                                 
      SP=SIN(RLON)                                                
       ZZZ=(ALT+BQUAD/D)*CT/ERA
       RHO=(ALT+AQUAD/D)*ST/ERA
       XXX=RHO*CP                                                
       YYY=RHO*SP                                               
      GOTO10                                                   
      ENTRY FELDC(V,BNORTH,BEAST,BDOWN,BABS)                  
C*****ENTRY POINT  FELDC  TO BE USED WITH CARTESIAN CO-ORDINATES       
      IS=2                                                            
      XXX=V(1)                                                       
      YYY=V(2)                                                      
      ZZZ=V(3)                                                     
10    RQ=1./(XXX*XXX+YYY*YYY+ZZZ*ZZZ) 
      XI(1)=XXX*RQ                                                
      XI(2)=YYY*RQ                                               
      XI(3)=ZZZ*RQ                                              
      GOTO20                                                   
      ENTRY FELDI                                             
C*****ENTRY POINT  FELDI  USED FOR L COMPUTATION             
      IS=3                                                  
20    IHMAX=NMAX*NMAX+1                                    
      LAST=IHMAX+NMAX+NMAX                                
      IMAX=NMAX+NMAX-1                                   
      DO 8 I=IHMAX,LAST                                 
8     H(I)=G(I)                                        
      DO 6 K=1,3,2                                    
      I=IMAX                                         
      IH=IHMAX                                      
1     IL=IH-I                                      
      F=2./FLOAT(I-K+2)                           
      X=XI(1)*F                                  
      Y=XI(2)*F                                 
      Z=XI(3)*(F+F)                            
      I=I-2                                   
      IF(I-1)5,4,2                           
2     DO 3 M=3,I,2                                                     
      H(IL+M+1)=G(IL+M+1)+Z*H(IH+M+1)+X*(H(IH+M+3)-H(IH+M-1))         
     A                               -Y*(H(IH+M+2)+H(IH+M-2))        
3     H(IL+M)=G(IL+M)+Z*H(IH+M)+X*(H(IH+M+2)-H(IH+M-2))             
     A                         +Y*(H(IH+M+3)+H(IH+M-1))            
4     H(IL+2)=G(IL+2)+Z*H(IH+2)+X*H(IH+4)-Y*(H(IH+3)+H(IH))       
      H(IL+1)=G(IL+1)+Z*H(IH+1)+Y*H(IH+4)+X*(H(IH+3)-H(IH))      
5     H(IL)=G(IL)+Z*H(IH)+2.*(X*H(IH+1)+Y*H(IH+2))              
      IH=IL                                                    
      IF(I.GE.K)GOTO1                                         
6     CONTINUE                                               
      IF(IS.EQ.3)RETURN                                     
      S=.5*H(1)+2.*(H(2)*XI(3)+H(3)*XI(1)+H(4)*XI(2))      
      T=(RQ+RQ)*SQRT(RQ)                                  
      BXXX=T*(H(3)-S*XXX)                                
      BYYY=T*(H(4)-S*YYY)                               
      BZZZ=T*(H(2)-S*ZZZ)                              
      IF(IS.EQ.2)GOTO7                                
      BABS=SQRT(BXXX*BXXX+BYYY*BYYY+BZZZ*BZZZ)
      BEAST=BYYY*CP-BXXX*SP                  
      BRHO=BYYY*SP+BXXX*CP                  
      BNORTH=BZZZ*ST-BRHO*CT               
      BDOWN=-BZZZ*CT-BRHO*ST              
      RETURN                             
7     B(1)=BXXX                         
      B(2)=BYYY                        
      B(3)=BZZZ                       
      RETURN                         
      END                           
C
C
        SUBROUTINE FELDCOF(YEAR,DIMO)
C------------------------------------------------------------------------
C  DETERMINES COEFFICIENTS AND DIPOL MOMENT FROM IGRF MODELS
C
C       INPUT:  YEAR    DECIMAL YEAR FOR WHICH GEOMAGNETIC FIELD IS TO
C                       BE CALCULATED
C       OUTPUT: DIMO    GEOMAGNETIC DIPOL MOMENT IN GAUSS (NORMALIZED 
C                       TO EARTH'S RADIUS) AT THE TIME (YEAR)
C  D. BILITZA, NSSDC, GSFC, CODE 633, GREENBELT, MD 20771, 
C       (301)286-9536   NOV 1987.
C 24Oct2000 (M.Tripicco) changed /MODEL/ to /GMODEL/
C-----------------------------------------------------------------------
        character(12)    FILMOD, FIL1, FIL2           
        DIMENSION       GH1(144),GHA(144),FILMOD(11),DTEMOD(11)
c       DIMENSION       GH2(120)
        DOUBLE PRECISION X,F0,F 
c       COMMON/MODEL/   FIL1,NMAX,TIME,GH1
        COMMON/GMODEL/  FIL1,NMAX,TIME,GH1
        COMMON/GENER/   UMR,ERAD,AQUAD,BQUAD
        DATA            FILMOD /'dgrf45.dat', 'dgrf50.dat',            
     1                  'dgrf55.dat', 'dgrf60.dat', 'dgrf65.dat',      
     2                  'dgrf70.dat', 'dgrf75.dat', 'dgrf80.dat',      
     3                  'dgrf85.dat', 'igrf90.dat','igrf90s.dat'/
        DATA            DTEMOD / 1945., 1950., 1955., 1960., 1965.,           
     1                  1970., 1975., 1980., 1985., 1990., 1995./      
        DATA GHA /-29667., -1785.,  5315., -2214.,  3070., -2374.,
     X              1693.,  -464.,  1333., -2282.,  -263.,  1246.,
     X               305.,   771.,  -414.,   945.,   788.,   266.,
     X               282.,  -228.,  -417.,   105.,   106.,  -305.,
     X              -205.,   353.,    47.,   232.,   159.,  -129.,
     X              -154.,  -166.,   -57.,   -25.,    98.,    67.,
     X                64.,   -16.,    72.,    77.,  -172.,    68.,
     X                 2.,   -58.,    17.,     8.,   -90.,    33.,
     X                83.,   -70.,   -75.,     4.,   -27.,    34.,
     X                 7.,    13.,    14.,     6.,    16.,    10.,
     X               -23.,     0.,    -5.,    22.,    -1.,    16.,
     X                -1.,   -20.,   -11.,     7.,   -18.,   -22.,
     X                 4.,    12.,     4.,     5.,    -3.,   -16.,
     X               -12.,    -5.,     4.,    10.,   -21.,     1.,
     X                15.,   -12.,    10.,     9.,    -6.,    -4.,
     X                -6.,    -1.,     9.,     7.,     9.,     2.,
     X                -7.,    -6.,     2.,    -4.,    -4.,     1.,
     X                 2.,     0.,    -5.,     3.,    -2.,     6.,
     X                 4.,    -4.,     3.,     0.,     1.,    -1.,
     X                 2.,     4.,     3.,     0.,0.,-6., 24*0./
c    X                 24*0./
c
c  numye is number of years represented by IGRF models
c
        NUMYE=10
C
C  IS=0 FOR SCHMIDT NORMALIZATION   IS=1 GAUSS NORMALIZATION
C  IU  IS INPUT UNIT NUMBER FOR IGRF COEFFICIENT SETS
C
        IU = 10
        IS = 0
C-- DETERMINE IGRF-YEARS FOR INPUT-YEAR
        TIME = YEAR
        IYEA = INT(YEAR/5.)*5
        L = (IYEA - 1945)/5 + 1
        IF(L.LT.1) L=1
        IF(L.GT.NUMYE) L=NUMYE         
        DTE1 = DTEMOD(L)   
        FIL1 = FILMOD(L)   
        DTE2 = DTEMOD(L+1) 
        FIL2 = FILMOD(L+1) 
C-- GET IGRF COEFFICIENTS FOR THE BOUNDARY YEARS
C       CALL GETSHC (IU, FIL1, NMAX1, ERAD, GH1, IER)  
C           IF (IER .NE. 0) STOP                           
C       CALL GETSHC (IU, FIL2, NMAX2, ERAD, GH2, IER)  
C           IF (IER .NE. 0) STOP                    
C-- DETERMINE IGRF COEFFICIENTS FOR YEAR
C       IF (L .LE. NUMYE-1) THEN                        
C         CALL INTERSHC (YEAR, DTE1, NMAX1, GH1, DTE2, 
C    1          NMAX2, GH2, NMAX, GHA)                        
C       ELSE               
C         CALL EXTRASHC (YEAR, DTE1, NMAX1, GH1, NMAX2,     
C    1          GH2, NMAX, GHA)                                    
C       ENDIF 
C-- DETERMINE MAGNETIC DIPOL MOMENT AND COEFFIECIENTS G
        NMAX = 10
        F0=0.D0
        DO 1234 J=1,3
           F = GHA(J) * 1.D-5
           F0 = F0 + F * F
1234    CONTINUE
        DIMO = DSQRT(F0)

        GH1(1) =  0.0
        I=2          
        F0=1.D-5                
        IF(IS.EQ.0) F0=-F0 
        SQRT2=SQRT(2.)      

      DO 9 N=1,NMAX           
        X = N
        F0 = F0 * X * X / (4.D0 * X - 2.D0)               
        IF(IS.EQ.0) F0 = F0 * (2.D0 * X - 1.D0) / X
        F = F0 * 0.5D0                                    
        IF(IS.EQ.0) F = F * SQRT2
        GH1(I) = GHA(I-1) * F0
        I = I+1                                         
      DO 9 M=1,N                                    
        F = F * (X + M) / (X - M + 1.D0)                 
        IF(IS.EQ.0) F = F * DSQRT((X - M + 1.D0) / (X + M))             
        GH1(I) = GHA(I-1) * F
        GH1(I+1) = GHA(I) * F
        I=I+2
9     CONTINUE                                          
        RETURN
        END
C
C
        SUBROUTINE INITMAGMOD
C----------------------------------------------------------------
C Initializes the parameters in COMMON/GENER/
C
C       UMR     = ATAN(1.0)*4./180.   <DEGREE>*UMR=<RADIANT>
C       ERA     EARTH RADIUS FOR NORMALIZATION OF CARTESIAN 
C                       COORDINATES (6371.2 KM) 
C       EREQU   MAJOR HALF AXIS FOR EARTH ELLIPSOID (6378.160 KM)
C       ERPOL   MINOR HALF AXIS FOR EARTH ELLIPSOID (6356.775 KM)
C       AQUAD   SQUARE OF MAJOR HALF AXIS FOR EARTH ELLIPSOID
C       BQUAD   SQUARE OF MINOR HALF AXIS FOR EARTH ELLIPSOID
C
C ERA, EREQU and ERPOL as recommended by the INTERNATIONAL 
C ASTRONOMICAL UNION .
C-----------------------------------------------------------------
        COMMON/GENER/   UMR,ERA,AQUAD,BQUAD
        COMMON/DIPMO/   DIMO
        ERA=6371.2
        EREQU=6378.16
        ERPOL=6356.775
        AQUAD=EREQU*EREQU
        BQUAD=ERPOL*ERPOL
        UMR=ATAN(1.0)*4./180.
        CALL FELDCOF(1996.0,DIMO)
        RETURN
        END

      SUBROUTINE EarthCoor(Lat, Lon, ZVel, Theta, Phi)
C
C     Lat is ACSSCLAT from ApID 16
C     Lon is ACSSCLON from ApID 16
C     ZVel is ACSSCVELZ from ApID 17
C     Output is BKGD_THETA and BKDG_PHI which are the longitude of
C        the N-going equatorial crossing for the current orbit and the
C        phase angle of the current orbit, respectively, in degrees.
C        The range of these values id (-180, 180].
C
C     Mike Stark 12-Sep-1996
C       added to starksubs.f on 09Oct96 by mjt (xtederive v1.9.1)
C
C       31Oct96 -- if ZVel is missing, determine N/S direction by 
C                  comparing current latitude with previous value
C
C       13Nov96 -- fixed problem with arg to ASIN() > 1.0d0
C
      IMPLICIT NONE
C
      REAL*4 Lat, Lon, ZVel, Theta, Phi
      REAL*4 Inclination, Pi, lastlat
      real*4 nulle
        parameter(nulle=-999999999.)

      real*8 tmp
C
      PARAMETER (Pi = 3.141592654)
      PARAMETER (Inclination = 0.4013)
C
      common /latitude/ lastlat
C
      if (zvel .eq. nulle) then
        if (lastlat .eq. nulle) then
          theta = nulle
          phi = nulle
          return
        endif
        if ( (lastlat - lat) .gt. 0 ) then
          zvel = -1.0
        else
          zvel =  1.0
        endif
      endif
C
C     13Nov added this to avoid F.P.E at 1.0+epsilon
C
      tmp=((Lat * (Pi/180.0)) / Inclination)
      if (tmp .gt. 1.0d0) then
        tmp=1.0d0
      endif
      if (tmp .lt. -1.0d0) then
        tmp=-1.0d0
      endif
c      Phi = ASIN((Lat/180.0) * (Pi / Inclination))
      Phi = ASIN(tmp)
      IF (ZVel .LT. 0.0) Phi = Pi - Phi
      IF (Phi .GT. Pi) Phi = Phi - 2.0 * Pi
      Theta = Lon - ATAN2(SIN(Phi) * COS(Inclination), COS(Phi)) /
     x              Pi * 180.0 + 23.617 * Phi / (2.0 * Pi)
      Theta = MOD(Theta, 360.0)
      IF (Theta .GT. 180.0) Theta = Theta - 360.0
      Phi = Phi / Pi * 180.0
      RETURN
      END
