**==uerget.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
      SUBROUTINE UERGET(Istat,Msg)
 
      INTEGER*4 Istat
      CHARACTER*(*) Msg
 
      IF ( Istat.NE.0 ) THEN
         IF ( Istat.EQ.-1 ) THEN
            Msg = 'End of file on CL string'
         ELSEIF ( Istat.EQ.1 ) THEN
            Msg = 'CL parameter not found'
         ELSEIF ( Istat.EQ.2 ) THEN
            Msg = 'CL parameter has bad data type'
         ELSEIF ( Istat.EQ.3 ) THEN
            Msg = 'CL parameter is undefined'
         ELSE
            Msg = 'XPI error'
         ENDIF
 
      ENDIF
      RETURN
      END
