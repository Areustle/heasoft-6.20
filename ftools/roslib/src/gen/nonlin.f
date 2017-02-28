        SUBROUTINE NONLIN (GAININ,PIIN,PIOUT)
C
C  NONLIN uses the results of ground calibration data collected with
C  the engineering model PSPC to address the apparent nonlinearity of the
C  proportional counter response as a function of detector gain.  These
C  data were collected long after the launch of ROSAT and after the 
C  PSPC had been used for extensive testing of XMM components.  There
C  is a fairly major spatial hole in the gain which affected these data.
C  Be that as it may, the relative means of the data collected at 183, 
C  277, 525, 932, 1486, 1740 and 2981 eV at gains of 149, 140, 107, and 
C  87 were used to produce an array which the following subroutine 
C  interpolates to derive a scale factor as a function of gain and input
C  channel.  The data were smoothed somewhat before implementation.
C
C  INPUT:   GAININ     Gain of the PSPC from the near-real-time analysis
C           PIIN       PI channel
C  OUTPUT:  PIOUT      Scaled PI channel
C
        IMPLICIT NONE
C
        INTEGER*4 IG, IP
        REAL*4 GAIN, GAININ, GAINAR(4), PIARR(7), PI, PIIN, 
     +      PIOUT, R1, R2, SC, SCALAR(4,7), X, XM, Y, YM
C
        DATA GAINAR /90., 110., 140., 150./
        DATA PIARR /20., 30., 57., 98., 151., 180., 313./
C
C  From analysis of Hasinger data.  Spectra were collected 
C  from a number of lines at different gains.  The spectra were
C  fit and their scaling compared to a nominal gain (149) 
C  calculated.
C
        DATA SCALAR /1.000, 1.000, 1.000, 1.000,
     +               1.030, 1.019, 1.002, 1.000,
     +               1.035, 1.021, 1.006, 1.000,
     +               1.012, 1.009, 1.006, 1.000,
     +               1.000, 1.000, 1.000, 1.000,
     +               0.991, 0.995, 0.998, 1.000,
     +               0.986, 0.989, 0.995, 1.000/
C
        SC = 0.0
C
C  If the gain is greater than 150 default the scaling to 1.0
C
        IF(GAININ .GE. GAINAR(4)) THEN
            PIOUT = PIIN
        ELSEIF(PIIN .LE. PIARR(1)) THEN
C
C  If the PI channel is less than 20 don't scale it 
C
            PIOUT = PIIN
        ELSE
C
C  If the gain is less than 90, reset it to 90
C
            GAIN = GAININ
            IF(GAIN .LE. 90) GAIN = 90
C
C  If the PI channel is greater than 313, reset it to that
C  for the calculations
C
            PI = PIIN
            IF(PI .GE. 313.0) PI = 313.0
C
C  Set the gain counter
C
            IF(GAIN .LE. GAINAR(2)) THEN
                IG = 2
            ELSEIF(GAIN .LE. GAINAR(3)) THEN
                IG = 3
            ELSE
                IG = 4
            ENDIF
C
C  Set the channel counter
C
            IF(PI .LE. PIARR(2)) THEN
                IP = 2
            ELSEIF(PI .LE. PIARR(3)) THEN
                IP = 3
            ELSEIF(PI .LE. PIARR(4)) THEN
                IP = 4
            ELSEIF(PI .LE. PIARR(5)) THEN
                IP = 5
            ELSEIF(PI .LE. PIARR(6)) THEN
                IP = 6
            ELSE
                IP = 7
            ENDIF
C
C  Calculate the weightings (the distances to the corners)
C
            X = (PIIN - PIARR(IP-1))/(PIARR(IP) - PIARR(IP-1))
            XM = 1.0 - X
            Y = (GAIN - GAINAR(IG-1))/(GAINAR(IG) - GAINAR(IG-1))
            YM = 1.0 - Y
C
C  Reset the PI channel if necessary
C
            IF(PIIN .GE. 313.0) PI = PIIN
C
C  Check to see if the input point lies on a grid point
C
            IF(((X .NE. 0.) .OR. (Y .NE. 0)) .AND. 
     +              ((XM .NE. 0.) .OR. (Y .NE. 0)) .AND.
     +              ((X .NE. 0.) .OR. (YM .NE. 0)) .AND.
     +              ((XM .NE. 0.) .OR. (YM .NE. 0))) THEN
C
C  If not, calculate the scaling
C
                R1 = XM*SCALAR(IG-1,IP-1) + X*SCALAR(IG-1,IP)
                R2 = XM*SCALAR(IG,IP-1) + X*SCALAR(IG,IP)
                SC = YM*R1 + Y*R2
C
C  If so, set the scale factor to the grid point
C
            ELSEIF((X .EQ. 0.) .AND. (Y .EQ. 0)) THEN
                SC = SCALAR(IG-1,IP-1)
            ELSEIF((XM .EQ. 0.) .AND. (Y .EQ. 0)) THEN
                SC = SCALAR(IG-1,IP)
            ELSEIF((X .EQ. 0.) .AND. (YM .EQ. 0)) THEN
                SC = SCALAR(IG,IP-1)
            ELSEIF((XM .EQ. 0.) .AND. (YM .EQ. 0)) THEN
                SC = SCALAR(IG,IP)
            ENDIF
C
C  Scale the channel
C
            PIOUT = SC*PI
        ENDIF
C
        RETURN
        END

