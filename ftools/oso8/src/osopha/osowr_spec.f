CH  Lorraine Breedon (1.0.0 14 Mar 1999) Original working version

C  This is a subroutine to write OSO-8 B/C detector pha spectrum
C for a slected X-ray source
 
      SUBROUTINE OSOWR_SPEC(Fitsunit,Nbins,dt,Stim,Stimms,Etim,
     &                      Etimms,Rate,Error,Status)
 
      IMPLICIT NONE
 
      INTEGER*4 Fitsunit , row , bytlen , hdutype ,  Status
      INTEGER*4 i, year , month , day ,Stim,Etim
      INTEGER*4  Stimms, Etimms
      INTEGER*4 slastat , hmsf(4) , Nbins,chan
 
      REAL second, Rate(63), Error(63),dayfrac_st,dayfrac_en,
     &     dt
  
      DOUBLE PRECISION mjd ,mjdref,fracday,tstart,tstop,telapse
 
      character(1) sign
      character(8) starttime , stoptime , startdate , stopdate
      character(70) ctemp
      character(80)  errm
 
      DATA mjdref/42412.0d0/
 
      CALL FTMKYJ(Fitsunit,'NAXIS2',Nbins,'Number of rows in table',
     &            Status)
      row = Nbins
      CALL FTGKYJ(Fitsunit,'NAXIS1',bytlen,ctemp,Status)
      bytlen = bytlen*row
 
      IF ( Nbins.NE. 63 ) THEN
         WRITE (errm,*) 'WARNING: Expected 63 bins, found ' , Nbins
         CALL XAERROR(errm,5)
 
      ENDIF
C Stims is in millisecs 
      dayfrac_st = FLOAT(Stimms)/(1000.0 * 86400.0)
      
      mjd = DBLE(Stim) + DBLE(dayfrac_st) + mjdref
 
      CALL SLA_DJCL(mjd,year,month,day,fracday,slastat)
      IF ( slastat.NE.0 ) THEN
 
         WRITE (errm,
     &    '(''Error start time to calendar'','' date, SLA status='',i3)'
     &    ) slastat
         CALL XAERROR(errm,5)
 
      ENDIF
      CALL SLA_DD2TF(2,fracday,sign,hmsf)
      second = REAL(hmsf(3)) + REAL(hmsf(4))/100.0
      WRITE (startdate,99001) day , month , (year-1900)
      WRITE (starttime,99002) hmsf(1) , hmsf(2) , NINT(second)

C Etims is in millisecs 
      dayfrac_en = FLOAT(Etimms)/(1000.0 * 86400.0)
      
      mjd = DBLE(Etim) + DBLE(dayfrac_en) + mjdref
  
      CALL SLA_DJCL(mjd,year,month,day,fracday,slastat)
      IF ( slastat.NE.0 ) THEN
 
         WRITE (errm,
     &     '(''Error stop time to calendar'',''date, SLA status='', i3)'
     &     ) slastat
         CALL XAERROR(errm,5)
 
      ENDIF
      
      CALL SLA_DD2TF(2,fracday,sign,hmsf)
      second = REAL(hmsf(3)) + REAL(hmsf(4))/100.0
      WRITE (stopdate,99001) day , month , (year-1900)
      WRITE (stoptime,99002) hmsf(1) , hmsf(2) , NINT(second)
      
      CALL FTMKYS(Fitsunit,'DATE-OBS',startdate,
     &            'Date of observation start (dd/mm/yy)',Status)
      CALL FTMKYS(Fitsunit,'TIME-OBS',starttime,
     &            'Time of observation start (hh:mm:ss)',Status)
      CALL FTMKYS(Fitsunit,'DATE-END',stopdate,
     &            'Daste of observation stop (dd/mm/yy)',Status)
      CALL FTMKYS(Fitsunit,'TIME-END',stoptime,
     &            'Time of observation stop (hh:mm:ss)',Status)
  
      DO i = 1 , Nbins
             chan=i
         CALL FTPCLJ(Fitsunit,1,i,1,1,chan,Status)
         CALL FTPCLE(Fitsunit,2,i,1,1,Rate(i),Status)
         CALL FTPCLE(Fitsunit,3,i,1,1,Error(i),Status)
      ENDDO
 
C Also modify values for the TSTART, TSTOP, TELAPSE keywords

c      tstart = dble((Stimms/1000.0) + (real(Stim) * 86400.0))
c      tstop = dble((Etimms/1000.0) + (real(Etim) * 86400.0))
      tstart = dble(Stimms)/1000.0d0 + (DBLE(Stim) * 86400.0d0)
      tstop = dble(Etimms)/1000.0d0 + (DBLE(Etim) * 86400.0d0)
      telapse = tstop - tstart
      
      
      Status=0
      call ftmkyg(Fitsunit,'TSTART', tstart, 3,
     +        '&', Status)
      call ftmkyg(Fitsunit,'TSTOP', tstop, 3,
     +        '&',Status)
      call ftmkyg(Fitsunit,'TELAPSE', telapse, 3,
     +     '&', Status)
      
      CALL FTPKYF(Fitsunit,'DEADC',dt,6,
     &            'Deadtime correction factor',Status)


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
     


      IF ( Status.NE.0 ) THEN
         errm = ' Problem closing output spectral file'
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
