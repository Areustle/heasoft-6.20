**==xpiopenlog.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
      SUBROUTINE XPIOPENLOG
 
*
* OPens the default log file
*
 
      INTEGER lstr , nbuf
      REAL*4 rbuf(10)
      character(1000) str
 
      CALL TRLOG('FLOGFILE',8,str,lstr)
      IF ( lstr.GT.0 ) THEN
C         print *,'trying log file ',str(:lstr)
         rbuf(1) = 0
         CALL LOGGER(3,rbuf,nbuf,str,lstr)
C         print *,'no logfile found'
      ENDIF
 
      RETURN
 
      ENTRY XPICLOSELOG
 
C      print *,'Lets close that logfile'
      str = 'keep'
      CALL LOGGER(4,rbuf,nbuf,str,lstr)
 
      RETURN
      END
 
 
 
 
