*+
      SUBROUTINE CDJCAL (NDP, DJM, IYMDF, J)
      IMPLICIT NONE
*     - - - - - -
*      CD J C A L
*     - - - - - -
*
* --- RY, Nicked routine from Xanadu for CALLIB --
* --- WARNING, VALUES >= 5 for NDP appear to give an integer
* overflow on the VAX, and incorrect values on the SUNS
*
*  Modified Julian Date to Gregorian Calendar, expressed
*  in a form convenient for formatting messages (namely
*  rounded to a specified precision, and with the fields
*  stored in a single array)
*
*  Given:
*     NDP      i      number of decimal places of days in fraction
*     DJM      d      modified Julian Date (JD-2400000.5)
*
*  Returned:
*     IYMDF    i(4)   year, month, day, fraction in Gregorian
*                     calendar
*     J        i      status:  nonzero = out of range
*
*  Any date after 4701BC March 1 is accepted.
*
*  The algorithm is derived from that of Hatcher 1984
*  (QJRAS 25, 53-55).
*
*  P.T.Wallace   Starlink   January 1989

        character(5) version
        parameter (version = '1.0.0')
*       1.0.0;Rehana Yusaf  Nicked version from Xanadu for CALLIB 
*-
      DOUBLE PRECISION DJM
      INTEGER IYMDF(4),J,NDP
      INTEGER NFD
      DOUBLE PRECISION FD,DF,F,D
      INTEGER JD,N4,ND10



*  Validate
      IF (DJM.LE.-2395520D0.OR.DJM.GE.1D9) THEN
         J=1
      ELSE

*     Denominator of fraction
         NFD=10**MAX(NDP,0)
         FD=DBLE(NFD)

*     Round date and express in units of fraction
         DF=NINT(DJM*FD)

*     Separate day and fraction
         F=MOD(DF,FD)
         IF (F.LT.0D0) F=F+FD
         D=(DF-F)/FD

*     Express day in Gregorian calendar
         JD=NINT(D)+2400001

         N4=4*(JD+((2*((4*JD-17918)/146097)*3)/4+1)/2-37)
         ND10=10*(MOD(N4-237,1461)/4)+5

         IYMDF(1)=N4/1461-4712
         IYMDF(2)=MOD(ND10/306+2,12)+1
         IYMDF(3)=MOD(ND10,306)/10+1
         IYMDF(4)=NINT(F)

         J=0

      END IF
      return
      END
