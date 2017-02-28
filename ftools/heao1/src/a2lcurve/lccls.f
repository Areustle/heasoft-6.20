C This subroutine finalizes the contents and updates keywords for the 
C HEAO-1 A2 light curves.
C
C Author: Jesse Allen
C History:
C  Version 1.0  11 Jul 1996  Add TIMEDEL to the TSTOP for proper stop time
C          1.1. 12 Aug 1996  Write data characteristics to an ASCII file 
C                            for constructing a database
C          1.2  26 Jan 1998  Calculates and writes the appropriate total 
C                            area of the detector
C          1.3  28 Jan 1998  Uses SLA routines for time conversions
C Author: Lorraine Breedon
C          1.4  22 Oct 1998  Handles just 1 lightcurve instead of up to 4

c      subroutine closelc(fitsunit, row, detnum, detindx, hd, status)
      subroutine closelc(fitsunit, row, detnum, hd, status)
      implicit none

C Common block declarations

c      common /TASK/ taskname
      
c      character(40) taskname

C Local variables

      logical anyf, qleft, qright

      integer*2 detnum, hd(4), hhh, hsign, hm

c      integer detindx
      integer fitsunit, row, bytlen, hdutype, status
      integer year, month, day, i, j, iside
      integer slastat, hmsf(4)

      real timedel, rate, error, totalrate, totalerr, second
      real geoarea, area(2,4)

      double precision tstart, tstop, mjd, mjdref, fracday

      character(1) sign
      character(8) starttime, stoptime, startdate, stopdate
      character(70) ctemp
      character(80) message

      data mjdref/43143.0D0/
      DATA AREA/390.7,363.0,418.,425.9,418.,425.9,418.,402.4/

C Calculate the total byte size of the light curve

      call ftmkyj(fitsunit, 'NAXIS2', row, 'Number of rows in table',
     +     status)
      call ftgkyj(fitsunit, 'NAXIS1', bytlen, ctemp, status)
      bytlen = bytlen * row

C Get the time of the start and stop of the light curve

      call ftgkye(fitsunit, 'TIMEDEL', timedel, ctemp, status)
      call ftgcvd(fitsunit, 1, 1, 1, 1, 0.0d0, tstart, anyf, status)
      mjd = (tstart/86400.0D0) + mjdref
      call sla_DJCL(mjd, year, month, day, fracday, slastat)
      if (slastat .ne. 0) then
         message = 'Error converting start time into calendar date'
         call xaerror(message,1)
         write(message,'(''SLA status = '', i3)') slastat
         call xaerror(message,1)
      endif
      call sla_DD2TF(2, fracday, sign, hmsf)
      second = hmsf(3) + (hmsf(4) / 100.0)
      write(startdate,1000) day, month, (year - 1900)
      write(starttime,2000) hmsf(1), hmsf(2), NINT(second)
      call ftgcvd(fitsunit, 1, row, 1, 1, 0.0d0, tstop, anyf, status)
      tstop = tstop + timedel
      mjd = (tstop/86400.0D0) + mjdref
      call sla_DJCL(mjd, year, month, day, fracday, slastat)
      if (slastat .ne. 0) then
        message = 'Error converting stop time into calendar date'
         call xaerror(message,1)
         write(message,'(''SLA status = '', i3)') slastat
         call xaerror(message,1)
       endif
      call sla_DD2TF(2, fracday, sign, hmsf)
      second = hmsf(3) + (hmsf(4) / 100.0)
      write(stopdate,1000) day, month, (year - 1900)
      write(stoptime,2000) hmsf(1), hmsf(2), NINT(second)

C Calculate the average count rate and count rate error

      totalrate = 0.0
      totalerr = 0.0
      do 10 i = 1, row
         call ftgcve(fitsunit, 2, i, 1, 1, 0, rate, anyf, status)
         totalrate = totalrate + rate
 10   continue
      if (row .gt. 1) then
         totalrate = totalrate / REAL(row)
      endif
      do 20 i = 1, row
         call ftgcve(fitsunit, 2, i, 1, 1, 0, rate, anyf, status)
         error = (rate - totalrate) * (rate - totalrate)         
         totalerr = totalerr + error
 20   continue
      totalerr = totalerr / (row * (row - 1))
      totalerr = sqrt(totalerr)

C Calculate the total geometric area of the detector

      qleft = .false.
      qright = .false.
      geoarea = 0.0
      do 30 j = 1, 4
c         HHH = ABS(HD(detindx,j))
         HHH = ABS(HD(j))
         IF (HHH .EQ. 0) GO TO 30
         HSIGN = 1
c         IF (HHH .NE. HD(detindx,j)) HSIGN = -1
          IF (HHH .NE. HD(j)) HSIGN = -1
         HM = MOD(INT(HHH),2)
         IF ((HM .EQ. 0) .AND. QRIGHT) GO TO 30
         IF ((HM .EQ. 1) .AND. QLEFT)  GO TO 30
         IF (HM .EQ. 0) QRIGHT = .TRUE.
         IF (HM .EQ. 1) QLEFT = .TRUE.
         ISIDE = 1
         IF (HM .EQ. 0) ISIDE = 2
         geoarea = geoarea + area(iside, detnum)
 30   continue

C Write the modified keyword values into the FITS file

      call ftmkyg(fitsunit,'TSTART', tstart, 3,
     +        'Seconds since MJDREF at obervation start', status)
      call ftmkyg(fitsunit,'TSTOP', tstop, 3,
     +        'Seconds since MJDREF at obervation stop', status)
      call ftmkyg(fitsunit,'TELAPSE', (tstop - tstart), 3,
     +     'Time interval (Start - Stop) in seconds', status)
      call ftmkyg(fitsunit,'ONTIME', ((DBLE(row)+1.0d0)*5.12D0), 3,
     +     'Time (s) on source for this extension', status)
      call ftmkys(fitsunit,'DATE-OBS', startdate,
     +        'Date of observation start (dd/mm/yy)', status)
      call ftmkys(fitsunit,'TIME-OBS', starttime,
     +        'Time of observation start (hh:mm:ss)', status)
      call ftmkys(fitsunit,'DATE-END', stopdate,
     +        'Daste of observation stop (dd/mm/yy)', status)
      call ftmkys(fitsunit,'TIME-END', stoptime,
     +        'Time of observation stop (hh:mm:ss)', status)
      call ftgkye(fitsunit, 'SRCCNT', rate, ctemp, status)
      call ftmkyf(fitsunit, 'SRCCNT', totalrate, 3, ctemp, status)
      call ftgkye(fitsunit, 'SRCCNTE', error, ctemp, status)
      call ftmkyf(fitsunit, 'SRCCNTE', totalerr, 3, ctemp, status)
      call ftmkyf(fitsunit, 'GEOAREA', geoarea, 2, 
     +     'Total geometric area of the detector', status)

      call ftddef(fitsunit, bytlen, status)

C Done!  Now move to the primary header and modify keywords there too

      call ftmahd(fitsunit, 1, hdutype, status)

      call ftmkys(fitsunit, 'DATE-OBS', startdate,
     +     'Date of observation start (dd/mm/yy)', status)
      call ftmkys(fitsunit, 'TIME-OBS', starttime,
     +     'Time of observation start (hh:mm:ss)', status)
      call ftmkys(fitsunit, 'DATE-END', stopdate,
     +     'Date of observation stop (dd/mm/yy)', status)
      call ftmkys(fitsunit, 'TIME-END', stoptime,
     +     'Time of observation stop (hh:mm:ss)', status)
      call ftgkye(fitsunit, 'SRCCNT', rate, ctemp, status)
      call ftmkyf(fitsunit, 'SRCCNT', totalrate, 3, ctemp, status)
      call ftgkye(fitsunit, 'SRCCNTE', error, ctemp, status)
      call ftmkyf(fitsunit, 'SRCCNTE', totalerr, 3, ctemp, status)
      call ftclos(fitsunit, status)

 1000 format(I2.2, '/', I2.2, '/', I2.2)
 2000 format(I2.2, ':', I2.2, ':', I2.2)

      return

      end

