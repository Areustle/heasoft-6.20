**==writescan.spg  processed by SPAG 4.50J  at 16:32 on 17 Oct 1998

C
C Author: Jesse Allen
C History:
C  Version 0.0   5 Apr 1998
CH  Lorraine Breedon (1.0.0 02 Dec 1998) Free the logical unit no\
Ch                                       Set row = Nbins \

C  This is a subroutine to write HEAO-1 A-2  DSDISK scan data into FITS format
 
      SUBROUTINE WRITESCAN(Fitsunit,Xmin,Nbins,Stim,Etim,Rate,Error,
     &                     Angle,Status)


c Fitsunit     I  i       logical unit no for output FITS scan file
c Xmin         I  r*4     Scan angle from which to start scan 
c Nbins        I  i       No of bins in output scan file
c Stim         I  r*4     time of 1st raw data record accessed 
c Etim         I  r*4     time of final raw data record accessed
c Rate         I  r*4     source count rate for each bin during scan
c Error        I  r*4     source count rate error for each bin
c Angle        I  r*4     scan angle when source is in FOV
c Status       O  i       Output error status flag
 
      IMPLICIT NONE
 
    
 
      INTEGER Fitsunit , row , bytlen , hdutype ,  Status
      INTEGER i , year , month , day 
      INTEGER slastat , hmsf(4) , Nbins
 
      REAL second , Stim , Etim , Rate(100) , Error(100) , 
     &     Angle
      REAL Xmin
 
      DOUBLE PRECISION mjd , mjdref , fracday
 
      character(1) sign
      character(8) starttime , stoptime , startdate , stopdate
      character(70) ctemp
      character(80)  errm
 
      DATA mjdref/43143.0D0/
 
 
      CALL FTMKYJ(Fitsunit,'NAXIS2',Nbins,'Number of rows in table',
     &            Status)
      row = Nbins
      CALL FTGKYJ(Fitsunit,'NAXIS1',bytlen,ctemp,Status)
      bytlen = bytlen*row
 
      IF ( Nbins.NE.100 ) THEN
         WRITE (errm,*) 'WARNING: Expected 100 bins, found ' , Nbins
         CALL XERROR(errm,5)
 
      ENDIF
 
      mjd = DBLE(Stim) + mjdref
      CALL SLA_DJCL(mjd,year,month,day,fracday,slastat)
      IF ( slastat.NE.0 ) THEN
 
         WRITE (errm,
     &    '(''Error start time to calendar'','' date, SLA status='',i3)'
     &    ) slastat
         CALL XERROR(errm,5)
 
      ENDIF
      CALL SLA_DD2TF(2,fracday,sign,hmsf)
      second = REAL(hmsf(3)) + (hmsf(4)/100.0)
      WRITE (startdate,99001) day , month , (year-1900)
      WRITE (starttime,99002) hmsf(1) , hmsf(2) , NINT(second)
      mjd = DBLE(Etim) + mjdref
      CALL SLA_DJCL(mjd,year,month,day,fracday,slastat)
      IF ( slastat.NE.0 ) THEN
 
         WRITE (errm,
     &     '(''Error stop time to calendar'',''date, SLA status='', i3)'
     &     ) slastat
         CALL XERROR(errm,5)
 
      ENDIF
      CALL SLA_DD2TF(2,fracday,sign,hmsf)
      second = REAL(hmsf(3)) + (hmsf(4)/100.0)
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
      CALL FTMKYF(Fitsunit,'SCANANG',Angle,3,
     &            'Scan angle position of the source',Status)
 
      DO i = 1 , Nbins
         CALL FTPCLE(Fitsunit,1,i,1,1,Xmin+((i-1)*0.25),Status)
         CALL FTPCLE(Fitsunit,2,i,1,1,Rate(i),Status)
         CALL FTPCLE(Fitsunit,3,i,1,1,Error(i),Status)
      ENDDO
 
      CALL FTDDEF(Fitsunit,bytlen,Status)
 
      CALL FTMAHD(Fitsunit,1,hdutype,Status)
 
      CALL FTMKYS(Fitsunit,'DATE-OBS',startdate,
     &            'Date of observation start (dd/mm/yy)',Status)
      CALL FTMKYS(Fitsunit,'TIME-OBS',starttime,
     &            'Time of observation start (hh:mm:ss)',Status)
      CALL FTMKYS(Fitsunit,'DATE-END',stopdate,
     &            'Date of observation stop (dd/mm/yy)',Status)
      CALL FTMKYS(Fitsunit,'TIME-END',stoptime,
     &            'Time of observation stop (hh:mm:ss)',Status)
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
