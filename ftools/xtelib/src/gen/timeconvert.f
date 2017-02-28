
      subroutine timeconvert(string,conversion,status)
 
c TIming routine to DECode a keyword string for Time UNits.
c This function is case insensitive.  It first looks for a string of either
c 'SEC' or 'DAY'.  Failing that, it takes the first nonblank character
c and assumes days for 'D' and seconds for 'S'.  Obviously, this method is not
c foolproof, so no guarantees are made.

c   I  string  (c) Character string with physical time units, ignored if blank
c  I/O flag    (i) Code for input time units, updated if string is not blank:
c                  = 1 seconds
c                  = 2 days
c                  = 3 milliseconds
c                  = 4 microseconds
c                  = 5 hours
c                  = 6 years
c                  = -1 information missing or not decoded.
c  I/O conversion   (d) time value converted, in place
c   O  status    (i) error status

c Author:  EAL  NASA/Goddard, HSTX  August, 1994

c Subroutines called: ftupch

      character*(*) string
      character(80) errm
      character(32) cbuf
      integer flag,status,outlen,fcstln,i
      double precision conversion

      conversion=1.0d0
      flag=0

      if(string.eq.' ')then
        conversion= 1.0d0
        return
      endif
      
      IF(status.ne.0) return

c Search the length of the string for any meaningful sequence.
c We have to do this in case the TUNIT keyword says something
c like 'MET DAY', etc.

      IF(string.ne.' ') THEN

        flag = -1
        cbuf = ' '
        cbuf = string
        CALL ftupch(cbuf)
        outlen=fcstln(cbuf)
         
c Copy string into a work variable, and convert to upper case.

        do 10 i=1,outlen
          if(flag.eq.-1)then
            if(cbuf(i:i+3).eq.'SEC') flag = 1
            if(cbuf(i:i+3).eq.'DAY') flag = 2
            if(cbuf(i:i+2).eq.'MS') flag = 3
            if(cbuf(i:i+3).eq.'MMS') flag = 4
            if(cbuf(i:i+2).eq.'US') flag = 4

c And don't fail to catch these cases.

            if(cbuf(i:i+2).eq.'MS' ) flag = 3
            if(cbuf(i:i+4).eq.'MSEC' ) flag = 3
            if(cbuf(i:i+5).eq.'MILLI' ) flag = 3
            if(cbuf(i:i+3).eq.'MMS' ) flag = 4
            if(cbuf(i:i+5).eq.'MICRO' ) flag = 4

c If no such string is found, pick off the first character.

            if(flag.eq.-1) then
              if(cbuf(1:1).eq.'S') flag = 1
              if(cbuf(1:1).eq.'D') flag = 2
c      >>> Not supported as of April 4, 1994. <<<
              if(cbuf(1:1).eq.'H') flag = 5
              if(cbuf(1:1).eq.'Y') flag = 6
            endif
          endif
10      continue
       
        IF(flag.lt.0) THEN
          errm = 'Failed to interpret timeunit value: '
          CALL fcecho(errm)
          call fcecho(cbuf)
          errm = 'Assuming values are in seconds.'
          call fcecho(errm)
          conversion=1.0d0
          RETURN
        ENDIF

      ENDIF

c Now convert to seconds.

c      Converting to seconds.

      IF    (flag.eq.1) THEN
        RETURN
      ELSEIF(flag.eq.2) THEN
        conversion = conversion * 86400.d0
      ELSEIF(flag.eq.3) THEN
        conversion = conversion / 1.d3
      ELSEIF(flag.eq.4) THEN
        conversion = conversion / 1.d6
      ELSEIF(flag.eq.5) THEN
        conversion = conversion * 3600.d0
      ELSEIF(flag.eq.6) THEN
        conversion = conversion * 3.15576d7
      ELSE
        status = 1
        errm = 'xrdectun: illegal unit conversion flag'
      ENDIF

      IF(status.ne.0) CALL fcecho(errm)

      return
      end
        
