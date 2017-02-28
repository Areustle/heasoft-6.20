C***************************************************************
C
C subroutine:  xrtywin
C              read window file
C
C written by:
C      Emily A. Greene
C      HEASARC/GSFC/NASA  Hughes STX
C      3/10/95
C
C modification history:
C
C notes:
C      see xronos.inc for the meaning and current size of all parameter
C      arrays
C
C      this routine is to replace xrtywin.f calls
C
C      cser holds the name of the series both 123 and ABC were used 
C           perviously.  If you don't like my choice, change the data
C           statement
C
C calling sequence:
C      call xrtywin(nser, twia, twio, pwi,
C                        pwia, pwio, fwia, fwio, ewia, ewio, nwi, nwito,
C                        status)
C
C variables:
C
C      nser - integer - input - number of time series
C      twia - double array - output - time window start times
C      twio - double array - output - time window stop times
C      pwi - double array - output - phase window epoch and period
C      pwia - real array - output - phase window start
C      pwio - real array - output - phase window stop
C      fwia - real array - output - flux window start
C      fwio - real array - output - flux window stop
C      ewia - real array - output - exposure window start
C      ewio - real array - output - exposure window stop
C      nwi - integer array - output - number of each type of windows
C      nwito - integer - output - total number of all windows = iflags(5)
C      status - integer - output - status of operation
C
C***************************************************************
      SUBROUTINE xrtywin(nser, twia, twio, pwi, 
     &     pwia, pwio, fwia, fwio, ewia, ewio, nwi, 
     &     nwito, status)

      implicit none
      include '../include/xronos.inc'

      integer  nwito, i, j, k, iv, je, ise, idum, iwia(5), iwio(5)
      double precision swia, swio
      character(10) cint(maxbintype)
      include '../include/xronos_init.inc'



      data cint/'Orig. Bins', 'New Bins  ', 'Intervals'/

      swia=0
      swio=0
      do idum=1,5
         iwia(idum)=0
         iwio(idum)=0
      enddo

c calculate no. of ints. and expos. windows for a given nser
      i = 0
      je = 0
      DO ise = 1, nser
         idum = ise*3 - 3
         i = i + nwi(idum+3) + nwi(idum+4) + nwi(idum+5) 
         je = je + nwi(idum+15) + nwi(idum+16) + nwi(idum+17)
      ENDDO
c type window summary
      IF (nwito.GT.0) then
         WRITE (context, 1000)
     &        nwi(1), nwi(2), i, je
         call xwrite (context, 15)
 1000    FORMAT ( ' ', I4, ' Time, ', I4, ' Phase, ', I4,
     &        ' Intensity, ', I4, ' Exposure Windows')
      else
         WRITE (context, 1003)
 1003    FORMAT ( ' No Windows ')
      endif
      call xwrite (' ', 15)

c type windows one by one if chattiness is high enough
      IF (nwi(1).GT.0) THEN
         WRITE (context, 1001)
         call xwrite (context, 15)
 1001    FORMAT (' ', 'Time Window:',
     &        '           Start                              Stop')
         WRITE (context, 1101)
         call xwrite (context, 15)
 1101    FORMAT (' ', '                 day           h  m  s  ms',
     &        '          day           h  m  s  ms')
c  1                12345.67890123456   20 11 30 456   12345.67890123456
         DO i = 1, nwi(1)
CEAG
C this subroutine is in mathsubr.  It looks OK, except that is doesn't
C return a status and uses *8 for double.  I don't see a reason to touch it.
CEAG
            CALL xrdhms(twia(i), swia, iwia, 1)
            CALL xrdhms(twio(i), swio, iwio, 1)
            WRITE (context, 1002) i, twia(i),
     &           (iwia(k), k=2, 5), twio(i), (iwio(k), k=2, 5)
            call xwrite (context, 15)
 1002       FORMAT (' ', I4, 3X, 2(3X,F17.11,I5,I3,I3,I4))
         ENDDO
      ENDIF

c type phase windows
      IF (nwi(2).GT.0) THEN
         CALL xrdhms(pwi(1), swia, iwia, 1)
         swia = pwi(2)*86400.D0
         WRITE (context, 2001) 
         call xwrite (context, 15)
 2001    FORMAT ( ' ', 'Phase Window:',
     &        '          Epoch                              Period')
         WRITE (context, 2101) 
         call xwrite (context, 15)
 2101    FORMAT (' ', '                 day           h  m  s  ms',
     &        '         days           s    ')
         write(context,2201) pwi(1),
     &        (iwia(k), k=2, 5), pwi(2), swia
         call xwrite(context,15)
 2201    FORMAT ( ' ', 10X, F17.11,
     &        I5, I3, I3, I4, 3X, G16.11, 1X, G16.11)
         context=' '
         call xwrite(context,15)
         write(context,2301) 
         call xwrite(context,15)
 2301    FORMAT ( ' ', 46X,
     &        '   Start Phase     Stop Phase')
         DO i = 1, nwi(2)
            WRITE (context, 2002) i, pwia(i),
     &           pwio(i)
            call xwrite (context, 15)
 2002       FORMAT (' ', I4, 36X, 2F15.4)
         ENDDO
      ENDIF
c type intensity windows
c loop for series
      DO ise = 1, nser
c loop for original bin, new bin, interval
         DO j = 1, 3
            iv = 2 + 3*(ise-1) + j
            IF (nwi(iv).GT.0) THEN
               WRITE (context, 3001) cint(j),
     &              cser(ise)
               call xwrite (context, 15)
 3001          FORMAT ( ' ', 'Ints. Window in ', A, ' of Series ', A,
     &              ':', 8X, '  Min (c/s)      Max (c/s) ')
               DO i = 1, nwi(iv)
                  WRITE (context, 3002) i,
     &                 fwia(iv-2, i), fwio(iv-2, i)
                  call xwrite (context, 15)
 3002             FORMAT (' ', I4, 41X, 2G15.5)
               ENDDO
            ENDIF
 32      ENDDO
 31   ENDDO
c  type exposure windows
c      IF (je.GT.0) THEN    !Rev.1
c  !Rev.1 skip lines also if d/f exposure windows are used
      IF (je.GT.0.OR.je.lt.0) call xwrite (' ', 15)

c loop for series
      DO ise = 1, nser
c loop for original bin, new bin, interval
         DO j = 1, 3
            iv = 14 + 3*(ise-1) + j
            IF (nwi(iv).GT.0) THEN
 4001          FORMAT (' ', 'Exps. Window of ', A, ' of Series ', A,
     &              ':       Min =', F7.3, '   Max =', F7.3)
               DO i = 1, nwi(iv)
                  WRITE (context, 4001)
     &                 cint(j), cser(ise), ewia(iv-14), ewio(iv-14)
c     &                 cint(j), ise, ewia(iv-14), ewio(iv-14)  
                  call xwrite (context, 15)
               ENDDO
c !Rev.1 start  
c              write message if no exposure windows are used
            ELSEIF (nwi(iv).EQ.0) THEN
c Rev.2 start
 4002          FORMAT (' ', 'No Exposure Window in ', A, ' of Series '
     &              , A)
c     &                 , I2)
               WRITE (context, 4002)
     &              cint(j), cser(ise) 
               call xwrite (context, 15)
c              write message if default exposure windows and 'win' are used
            ELSEIF (nwi(iv).LT.0) THEN
 4003          FORMAT (' ', 'Default Exposure Window in ', A, 
     &              ' of Series ', A)
c     &                  ' of Series',I2)
               WRITE (context, 4003)
     &              cint(j), cser(ise)
               call xwrite (context, 15)
            ENDIF
 42      ENDDO
 41   ENDDO
      call xwrite (' ', 15)
      RETURN
      END
c
c
