**==dsclwt.spg  processed by SPAG 4.50J  at 16:32 on 17 Oct 1998
C DSCLWT.FOR
C    Version 1.01 4/25/88  by: Jesse Allen,    FITS creation

CH  Lorraine Breedon (2.0.0 24 Nov 1998) Remove all XPI calls \
CH                                       Remove COMMON blocks so
CH                                       parse COLwt,ICLwt
CH                                       via sub arg instead \
CH                                       Remove match0,MATch arrays \
CH                                       
C

C This routine sets up weights for colors for dsdlcurve

      SUBROUTINE DSCLWT(ICLwt,ratnam,sc_mode,sc_wt,
     &                  det_select,det_no,anti_coeff,anti_mode,
     &                  anti_wt,COLwt,Idn,ichat)


c ICLwt       I  i       Weight options for discovery scalars
c ratnam      I  ch*8    Array for all the types of RATE data available
c sc_mode     I  i       Discovery scalar mode for which to apply
c                        color weights
c sc_wt       I  r*4     Color weights for a given mode
c anti_coeff  I  l       Logical flag for anti-coincidence coefficient wting
c anti_mode   I  i       Discovery scalar mode for which to apply
c                        anti-coincidence coefficient weighting
c anti_wt     I  r*4     anti-coincidence coefficient weighting
c COLwt       O  r*4     weights array for colors and anti-coincidence
c                        coefficient (for a given discovery scalar mode)
c Idn         O  i       indicates the type of detector used
c ichat       O  i       chattiness flag for both terminal and output log


      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL clwt0 , clwt00 , clwt1 , clwt2 , clwt3 , clwt4 , 
     &     clwt5 , COLwt(9,6) , anti_wt, sc_wt(8)
      INTEGER i , ICLwt , Idn , anti_mode, sc_mode,
     &        j , jh , jm , ichat,det_no
      LOGICAL anti_coeff,det_select
      CHARACTER*(*) ratnam(ICLwt)
 
C*** End of declarations inserted by SPAG
C
C This routine sets up weights for colors for dsdlcurve and other similar
C programs.  Weights for 8 colors + antico coefficient are stored in
C COLWT(9,N) where N is the number of modes (presently 6).  A mode is
C essentially a selection of discovery scalers used for a stretch of
C time during the mission.  CLWT0 contains default values for COLWT the
C antico coefficient is defined to be the change in the weighted
C rate/sec for a change in the anti rate/mf of 1000.
C
C MOD 22-JUL-87 TO PRODUCE ARRAY OF MATCH
C	 MATCH CONTAINS INFORMATION TO TELL PROGRAM WHICH MODES CAN BE
C	 ADDED TOGETHER FOR THE DIFFERENT RATES
C MOD 31-JUL-87 TO RUN ON MICROVAX
C MOD 21-AUG-87 TO PRINT OUT NAMES FOR STANDARD RATES
C	CHANGE NUMBER OF MODES TO 6
C MOD 04-FEB-88 TO TELL USER WHAT MODE 6 IS
C MOD 05-FEB-88 TO DEFINE HARD AND SOFT RATE FOR MODE 6
C MOD 25-APR-88 TO RETURN IDN
C
     
      character(100) message
     
C
C
 
      DIMENSION clwt0(9,6) , clwt1(9,6) , clwt2(9,6) , 
     &          clwt00(9,6,6) , clwt3(9,6) , clwt4(9,6) , clwt5(9,6)
      EQUIVALENCE (clwt00(1,1,1),clwt0(1,1)) , 
     &             (clwt00(1,1,4),clwt3(1,1)) , 
     &             (clwt00(1,1,2),clwt1(1,1)) , 
     &             (clwt00(1,1,3),clwt2(1,1)) , 
     &             (clwt00(1,1,5),clwt4(1,1)) , 
     &             (clwt00(1,1,6),clwt5(1,1))
C
 
C MATCH: 1ST INDEX IS START AND STOP MODE
C	 2ND INDEX IS MODE FOR AVERAGE DAY
C	 3RD INDEX IS COLOR (TYPE OF RATE)
C COLWT0 WEIGHTS ARE SUCH THAT A SOURCE WITH A SPECTRUM OF GAMMA=1.65
C   ABS=1.8E20, AND A FLUX OF R15=1. PRODUCES 1 COUNT/SEC.
C CLWT0 HAS VALUES FOR OVERALL RATE
C CLWT1 HAS VALUES FOR "HARD" RATE
C CLWT2 HAS VALUES FOR "SOFT" RATE
C CLWT3 HAS VALUES FOR "R15"  RATE
C CLWT4 HAS VALUES FOR "WEIGHTED R15" RATE
C CLWT5 HAS VALUES FOR "MED M1" RATE
C
      DATA clwt0/.780 , .242 , 2*0. , .923 , 3*0. , .140 , .633 , .196 , 
     &     2*0. , .749 , .798 , 2*0. , .122 , .633 , .196 , 2*0. , 
     &     2*.749 , 2*.798 , .122 , 3*.633 , .196 , 2*.749 , 2*.798 , 
     &     .122 , 2*.802 , .279 , .181 , 2*.690 , 2*.735 , .102 , 
     &     2*.802 , .279 , .181 , 3*.690 , .735 , .102/
C COLORS ARE:
C MODE 1 (232-246)     H3 M1,M2,0,0  MED M1
C MODE 2 (246-248)     H3 M1,M2,0,0  MED M1,M2
C MODE 3 (248-305)     H3 M1,M2,0,0  MED 1ACD,1B,2A,2B
C MODE 4 (305-321)     H3 1AD,1B,1C,M2  MED 1ACD,1B,2A,2B
C MODE 5 (321-615)     H3 1A,1B,1CD,M2  MED 1ACD,1B,2A,2B
C MODE 6 (615-   )     H3 1A,1B,1CD,M2  MED 1A,1B,1CD,M2
C
      DATA clwt1/1.236 , 1.939 , 6*0. , .115 , 1.236 , 1.939 , 6*0. , 
     &     .115 , .944 , 1.329 , 3*0. , .897 , 0. , 1.293 , .099 , 
     &     3*.944 , 1.329 , 0. , .897 , 0. , 1.293 , .099 , 0. , 2.845 , 
     &     2.361 , 1.418 , 0. , 1.118 , 0. , 1.700 , .182 , 0. , 3.18 , 
     &     2.82 , 1.74 , 0. , 1.37 , 1.93 , 0. , .246/
      DATA clwt2/4*0. , 2.059 , 3*0. , .206 , 4*0. , 2.059 , 3*0. , 
     &     .206 , 4*0. , 1.455 , 0. , 4.534 , 0. , .175 , 4*0. , 1.455 , 
     &     0. , 4.534 , 0. , .175 , .311 , 3*0. , 1.190 , 0. , 4.188 , 
     &     0. , .131 , .45 , 3*0. , 2.12 , 3*0. , .150/
      DATA clwt3/2*1.311 , 6*0. , .103 , 2*1. , 3*0. , 1. , 2*0. , 
     &     .089 , 2*1. , 4*0. , 2*1. , .089 , 4*1. , 2*0. , 2*1. , 
     &     .089 , 4*1. , 2*0. , 2*1. , .089 , 4*1. , 2*0. , 2*1. , .089/
      DATA clwt4/1.171 , .364 , 6*0. , .087 , .952 , .295 , 3*0. , 
     &     1.201 , 2*0. , .074 , .952 , .295 , 4*0. , 2*1.201 , .074 , 
     &     3*.952 , .295 , 0. , 0. , 2*1.201 , .074 , 1.207 , 1.207 , 
     &     .420 , .272 , 0. , 0. , 2*1.333 , .050 , 2*1.207 , .420 , 
     &     .272 , 3*0. , 1.333 , .050/
      DATA clwt5/4*0. , 2.053 , 3*0. , .205 , 4*0. , 2.053 , 3*0. , 
     &     .205 , 4*0. , 2*2.053 , 2*0. , .205 , 4*0. , 2*2.053 , 2*0. , 
     &     .205 , 4*0. , 2*2.053 , 2*0. , .205 , 4*0. , 3*2.053 , 0. , 
     &     .205/



   

      IF (ICLwt .EQ. 0) THEN
          DO i = 1, 8
                 COLwt(i,sc_mode) = sc_wt(i)
          ENDDO
      ELSE

C set up standard weights
         DO i = 1 , 6
            DO j = 1 , 9
                COLwt(j,i) = clwt00(j,i,ICLwt)
            ENDDO
         ENDDO
      ENDIF

      IF (anti_coeff) COLwt(9,anti_mode) = anti_wt

C correct above if want data selection

      IF (det_no .EQ. 5) THEN

               DO i = 1 , 6
                 DO j = 5 , 8
                    COLwt(j,i) = 0.0
                 ENDDO
               ENDDO
      ELSEIF (det_no .EQ. 6) THEN
               DO i = 1 , 6
                 DO j = 1, 4
                    COLwt(j,i) = 0.0
                 ENDDO
               ENDDO
      ENDIF


      IF (ICLwt .NE. 0) THEN 
C Write weights
         WRITE (message,
     &'('' ....USING STANDARD WEIGHTS FOR '', a8, '' RATE'')') 
     & ratnam(ICLwt)
         CALL XWRITE(message,ichat)
         WRITE (message,
     &'('' The discovery scalar modes changed throught the mission.'')')
         CALL XWRITE(message,ichat)
         WRITE (message,
     &'('' The following table gives the standard weights for '')')
         CALL XWRITE(message,ichat)
         WRITE (message,
     &'('' the 6 discovery scalar scanning modes. Each mode '')')
         CALL XWRITE(message,ichat)
         WRITE (message,
     &'('' possessed 8 different X-ray colors & anti-coincidence '')')
         CALL XWRITE(message,ichat)
         WRITE (message,
     &'('' coefficient, thus a total of 9 weights are available. '')')
         CALL XWRITE(message,ichat)
         IF (.NOT. det_select) THEN
            WRITE (message,
     &'('' NOTE : weights take into account detector selection '')')
           CALL XWRITE(message,ichat)
         ENDIF     
 
         message = ' '
         CALL XWRITE(message,ichat)
         WRITE (message,
     &'('' Mode'',18X,''Color Wts.'',38X,''Acc Wt'')')
         CALL XWRITE(message,ichat)
 
         DO i = 1 , 6
             WRITE (message,'(I5,9F8.3)') i , (COLwt(j,i),j=1,9)
             CALL XWRITE(message,ichat)
         ENDDO

      ELSE
        WRITE (message,
     &'('' The discovery scalar modes changed throught the mission.'')')
         CALL XWRITE(message,ichat)
          WRITE (message,
     &'('' The following table gives the weights you entered for '')')
          CALL XWRITE(message,ichat)
          WRITE (message,
     &'('' your selected scanning mode. Each mode possessed 8  '')')
          CALL XWRITE(message,ichat)
          WRITE (message,
     &'('' different X-ray colors & anti-coincidence '')')
          CALL XWRITE(message,ichat)
          WRITE (message,
     &'('' coefficient, thus there are 9 weights available. '')')
          CALL XWRITE(message,ichat)
           IF (.NOT. det_select) THEN
              WRITE (message,
     &'('' NOTE : weights take into account detector selection '')')
              CALL XWRITE(message,ichat)
           ENDIF     

          message = ' '
          CALL XWRITE(message,ichat)
          WRITE (message,
     &'('' Mode'',18X,''Color Wts.'',38X,''Acc Wt'')')
          CALL XWRITE(message,ichat)
          WRITE (message,
     &'(I5,9F8.3)') sc_mode, (COLwt(j,sc_mode),j=1,9)
          CALL XWRITE(message,ichat)

      ENDIF
 
      message= ' '
      CALL XWRITE(message,ichat)

      
      IF (det_select) THEN
         
C Determine detector number
C Return either MED (5), HD3 (6), or COMBINED (7)
            jh = 0
            jm = 0
            DO i = 1 , 6
               DO j = 1 , 4
                  IF ( COLwt(j,i).NE.0. ) jh = 1
               ENDDO
               DO j = 5 , 8
                  IF ( COLwt(j,i).NE.0 ) jm = 1
               ENDDO
            ENDDO
            Idn = 6
            IF ( jm.NE.0 ) THEN
               IF ( jh.NE.0 ) THEN
                  Idn = 7
               ELSE
                  Idn = 5
               ENDIF
            ENDIF
            WRITE (message,
     &'('' Default Detector No. (5=MED,6=HD3,7=COMBINED): '',I3)')
     & Idn
            CALL XWRITE(message,ichat)
      ELSE
            WRITE (message,
     &'('' Selected Detector No. (5=MED,6=HD3,7=COMBINED): '',I3)')
     & det_no
           CALL XWRITE(message,ichat)


      ENDIF



 
      RETURN
      END
