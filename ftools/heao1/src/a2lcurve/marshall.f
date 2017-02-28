C----------------------------------------------------------------------------
C Calls parameters for FXRATE task from the parameter file
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 0.0  13 Feb 1998  First draft version
C          0.9  25 Feb 1998  Reordered time arrays
C (Lorraine Breedon) 19 Oct 1998 Handles 1 lightcurve instead of up to 4

      subroutine marshall(hdet, hds, xday, fctim, bkgd, ber)

      implicit none

      integer*2 hdet, hds(4)

      integer j

      real bkgd, ber, fctim, xday
      real time1(4,4), time2(4,4), time3(4,4), time4(4,4)

      character(80) message

      data time1 / 4.60,   6.075,  9.98,  7.155,
     +             2.66,  10.775, 16.33,  4.415,
     +             2.15,   1.655,  2.57,  2.02,
     +             1.25,   2.275,  3.29,  1.68 /
      data time2 / 4.73,   6.07,   9.98,  7.13, 
     +             2.71,  10.76,  16.33,  4.41,
     +             2.17,   1.63,   2.57,  1.97,
     +             1.23,   2.27,   3.29,  1.64 /
      data time3 / 4.687,  5.968,  9.722, 7.024,
     +             2.655, 10.643, 16.079, 4.305,
     +             2.193,  1.551,  2.469, 1.906,
     +             1.202,  2.167,  3.207, 1.570 /
      data time4 / 4.99,   6.042,  9.877, 6.744, 
     +             2.873, 10.812, 16.43,  4.139,
     +             2.265,  2.182,  2.431, 1.833,
     +             1.281,  1.182,  3.137, 1.517 /


c      do 20 i = 1, 4
       bkgd = 0.0
       ber = 0.0
c         if (hdet.eq. 0) go to 20
       if (hdet.gt.0) then
         do 10 j = 1, 4
            if (hds(j) .eq. 0) go to 10
            IF (XDAY .LT. 337.5D0) THEN
               bkgd = bkgd+ time1(hdet,hds(j))
            ELSE IF ((XDAY .GE. 337.5D0) .AND. (XDAY .LT. 447.5D0)) THEN
               bkgd = bkgd + time2(hdet,hds(j))
            ELSE IF ((XDAY .GE. 447.5D0) .AND. (XDAY .LT. 541.5D0)) THEN
               bkgd = bkgd + time3(hdet,hds(j))
            ELSE 
               bkgd = bkgd + time4(hdet,hds(j))
            ENDIF
 10      continue
         if ((xday .lt. 337.5D0) .and. (hdet .eq. 3)) then
            if ((hds(1) .eq. 1) .and. (hds(2) .eq. 2) .and.
     +          (hds(3) .eq. 3) .and. (hds(4) .eq. 4)) then
               bkgd = 32.29
            else 
               message = 'WARNING: HED-2 background rates prior to ' //
     +           'day 337.5 are unavailable, '
               call xwrite(message, 1)
               message = 'using estimated background rates from days' //
     +           ' 390 to 410 for earlier times'
               call xwrite(message,1)
            endif
         endif
         bkgd = bkgd * fctim
         ber = bkgd * 0.03 
c 20   continue
       else
         message = 'WARNING: the detector value is set to zero!!'
         call xwrite(message, 1)
       endif


      return

      end

      

