CH  Lorraine Breedon  (1.0.0 05 Apr 1999) Original working version

C  This is a subroutine to finalise contents of OSO-8 B/C detector rates 
C  data for a selected X-ray source
 
      SUBROUTINE OSOCL_RATES_BC(Fitsunit,Nbins,Stim,Stimms,Etim,Etimms,
     &                      area_tot,rates_out, Status)
 
      IMPLICIT NONE
 
      INTEGER*4 Fitsunit , row , bytlen , hdutype ,  Status
      INTEGER*4 i, year , month , day ,Stim,Etim
      INTEGER*4  Stimms, Etimms
      INTEGER*4 slastat , hmsf(4) , Nbins
 
      REAL second, dayfrac_st,dayfrac_en,bbnrm,area_tot
      logical anyf
  
      DOUBLE PRECISION mjd ,mjdref,fracday,tstart,tstop,telapse
 
      character(1) sign
      character(8) starttime , stoptime , startdate , stopdate
      character(70) ctemp
      character(80)  errm
      CHARACTER*(*) rates_out
      real rate, error, totalrate, totalerr,totalbbnrm
 
      DATA mjdref/42412.0d0/
 
      
      
      CALL FTMKYJ(Fitsunit,'NAXIS2',Nbins,'Number of rows in table',
     &            Status)
      row = Nbins
      CALL FTGKYJ(Fitsunit,'NAXIS1',bytlen,ctemp,Status)
      bytlen = bytlen*row
 
C Stims is in millisecs 
      dayfrac_st = Stimms/(1000.0 * 86400.0)
      
      mjd = DBLE(Stim) + DBLE(dayfrac_st) + mjdref
 
      CALL SLA_DJCL(mjd,year,month,day,fracday,slastat)
      IF ( slastat.NE.0 ) THEN
 
         WRITE (errm,
     &    '(''Error start time to calendar'','' date, SLA Status='',i3)'
     &    ) slastat
         CALL XAERROR(errm,5)
 
      ENDIF
      CALL SLA_DD2TF(2,fracday,sign,hmsf)
      second = REAL(hmsf(3)) + (hmsf(4)/100.0)
      WRITE (startdate,99001) day , month , (year-1900)
      WRITE (starttime,99002) hmsf(1) , hmsf(2) , NINT(second)

C Etims is in millisecs 
      dayfrac_en = Etimms/(1000.0 * 86400.0)
      
      mjd = DBLE(Etim) + DBLE(dayfrac_en) + mjdref
  
      CALL SLA_DJCL(mjd,year,month,day,fracday,slastat)
      IF ( slastat.NE.0 ) THEN
 
         WRITE (errm,
     &     '(''Error stop time to calendar'',''date, SLA Status='', i3)'
     &     ) slastat
         CALL XAERROR(errm,5)
 
      ENDIF
      
      CALL SLA_DD2TF(2,fracday,sign,hmsf)
      second = REAL(hmsf(3)) + (hmsf(4)/100.0)
      WRITE (stopdate,99001) day , month , (year-1900)
      WRITE (stoptime,99002) hmsf(1) , hmsf(2) , NINT(second)

C Calculate the average count rate and count rate error

      totalrate = 0.0
      totalerr = 0.0
      do 10 i = 1, row
         call ftgcve(Fitsunit, 2, i, 1, 1, 0, rate, anyf, Status)
         totalrate = totalrate + rate
 10   continue
      if (row .gt. 1) then
         totalrate = totalrate / REAL(row)
      endif
      do 20 i = 1, row
         call ftgcve(Fitsunit, 2, i, 1, 1, 0, rate, anyf, Status)
         error = (rate - totalrate) * (rate - totalrate)         
         totalerr = totalerr + error
 20   continue
      totalerr = totalerr / (row * (row - 1))
      totalerr = sqrt(totalerr)

C Calculate the total geometric area of the detector for the observation

      totalbbnrm = 0.0
      do 30 i = 1, row
         call ftgcve(Fitsunit, 8, i, 1, 1, 0, bbnrm, anyf, Status)
         totalbbnrm = totalbbnrm + bbnrm


 30   continue
      if (row .gt. 1) then
         totalbbnrm = totalbbnrm / REAL(row)
         area_tot = area_tot /REAL(row)

      endif

C perform time updates in 1st extension
    
      CALL FTMKYS(Fitsunit,'DATE-OBS',startdate,
     &            'Date of observation start (dd/mm/yy)',Status)
      CALL FTMKYS(Fitsunit,'TIME-OBS',starttime,
     &            'Time of observation start (hh:mm:ss)',Status)
      CALL FTMKYS(Fitsunit,'DATE-END',stopdate,
     &            'Daste of observation stop (dd/mm/yy)',Status)
      CALL FTMKYS(Fitsunit,'TIME-END',stoptime,
     &            'Time of observation stop (hh:mm:ss)',Status)
  
 
C Also modify values for the TSTART, TSTOP, TELAPSE keywords

c      tstart = dble((Stimms/1000.0) + (real(Stim) * 86400.0))
      tstart = dble(Stimms)/1000.0d0 + (DBLE(Stim) * 86400.0d0)
c      tstop = dble((Etimms/1000.0) + (real(Etim) * 86400.0))
      tstop = dble(Etimms)/1000.0d0 + (DBLE(Etim) * 86400.0d0)
      telapse = tstop - tstart
      
      Status=0
      call ftmkyg(Fitsunit,'TSTART', tstart, 3,
     +        '&', Status)
      call ftmkyg(Fitsunit,'TSTOP', tstop, 3,
     +        '&',Status)
      call ftmkyg(Fitsunit,'TELAPSE', telapse, 3,
     +     '&', Status)

C Also modify srce count keywords etc
       

      call ftgkye(Fitsunit, 'SRCCNT', rate, ctemp, Status)
      call ftmkyf(Fitsunit, 'SRCCNT', totalrate, 3, ctemp, Status)
      call ftgkye(Fitsunit, 'SRCCNTE', error, ctemp, Status)
      call ftmkyf(Fitsunit, 'SRCCNTE', totalerr, 3, ctemp, Status)


      CALL FTDDEF(Fitsunit,bytlen,Status)
C now move to primary header and modify keywords there too
 
      CALL FTMAHD(Fitsunit,1,hdutype,Status)
 
      CALL FTMKYS(Fitsunit,'DATE-OBS',startdate,
     &            'Date of observation start (dd/mm/yy)',Status)
      CALL FTMKYS(Fitsunit,'TIME-OBS',starttime,
     &            'Time of observation start (hh:mm:ss)',Status)
      CALL FTMKYS(Fitsunit,'DATE-END',stopdate,
     &            'Date of observation stop (dd/mm/yy)',Status)
      CALL FTMKYS(Fitsunit,'TIME-END',stoptime,
     &            'Time of observation stop (hh:mm:ss)',Status)
      call ftgkye(Fitsunit, 'SRCCNT', rate, ctemp, Status)
      call ftmkyf(Fitsunit, 'SRCCNT', totalrate, 3, ctemp, Status)
      call ftgkye(Fitsunit, 'SRCCNTE', error, ctemp, Status)
      call ftmkyf(Fitsunit, 'SRCCNTE', totalerr, 3, ctemp, Status)

     


      IF ( Status.NE.0 ) THEN
         WRITE (errm,
     & '( '' Problem updating keywords to output lc : '',a80)')
     & rates_out
         CALL XAERROR(errm,1)
         return
      ENDIF

      CALL FTCLOS(Fitsunit,Status)
      CALL XFRLUN(Fitsunit,Status)
      IF ( Status.NE.0 ) THEN
         errm = ' Problem releasing unit number'
         CALL XAERROR(errm,1)
         return
      ENDIF
 
      RETURN
 
99001 FORMAT (I2.2,'/',I2.2,'/',I2.2)
99002 FORMAT (I2.2,':',I2.2,':',I2.2)
 
      END
