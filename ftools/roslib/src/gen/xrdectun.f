      subroutine xrdectun(string,flag,mode,dtime,ierr)

c DECode a keyword string for Time UNits.

c This routine operates in several modes.

c If string is non-blank, it first tries to decode the string. 
c (Case insensitive)
c It sets the value of flag as follows:
c                  = 1 seconds
c                  = 2 days
c                  = 3 milliseconds
c                  = 4 microseconds
c                  = 5 hours
c                  = 6 years
c                  = -1 information missing or not decoded.

c IF string is blank, the routine accepts the value of flag that is passed to
c it.

c In either case the routine will try to convert the value of
c dtime from the units specified by flag to the units specified by mode.

c At the moment, mode = 1 and mode = 2 (i.e., conversion into
c either seconds or days) are the only two cases implemented.
c If mode < 0 only the interpretation is performed; dtime is left unchanged.
c If mode = 0 or > 2 an error is returned.

c We have attempted to make the string interpretation as forgiving as possible.
c The first thing it does is to convert the input string to upper case.
c Strings are interpreted as follows:

c First, if the substring 'SEC' appears anywhere in the string, flag = 1
c Second, if the substring 'DAY' appears anywhere in the string flag = 2
c    This is to cover cases like TIMEUNIT = 'MET DAY'.

c The routine then checks the following cases:

c 'MS' or 'MSEC' => millisconds  (overwriting flag if 'SEC' got noticed before)
c 'MMS' or 'US' => microsconds
c ... + several other cases; see below.

c   I  string  (c) Character string with physical time units, ignored if blank
c  I/O flag    (i) Code for input time units, updated if string is not blank:
c   I  mode    (i) units to convert to (1 for seconds, 2 for days, 
c                  others not supported); < 0 to skip conversion - decode
c                  string only
c  I/O dtime   (d) time value converted, in place
c   O  ierr    (i) error status

c Author:  EAL  NASA/Goddard, HSTX  August, 1994

      character*(*) string
      character(80) errm
      character(32) cbuf
      integer flag,mode,ierr
      double precision dtime

      IF(ierr.ne.0) return

c Search the length of the string for any meaningful sequence.
c We have to do this in case the TUNIT keyword says something
c like 'MET DAY', etc.

      IF(string.ne.' ') THEN

         flag = -1

c Copy string into a work variable, and convert to upper case.

         cbuf = ' '
         cbuf = string
         CALL upc(cbuf)
         CALL rmvlbk(cbuf)

         if(index(cbuf,'SEC').gt.0) flag = 1
         if(index(cbuf,'DAY').gt.0) flag = 2
         if(index(cbuf,'MS' ).gt.0) flag = 3
         if(index(cbuf,'MMS').gt.0) flag = 4
         if(index(cbuf,'US').gt.0) flag = 4

c And don't fail to catch these cases.

         if(cbuf(:2).eq.'MS' ) flag = 3
         if(cbuf(:4).eq.'MSEC' ) flag = 3
         if(cbuf(:5).eq.'MILLI' ) flag = 3
         if(cbuf(:3).eq.'MMS' ) flag = 4
         if(cbuf(:5).eq.'MICRO' ) flag = 4

c If no such string is found, pick off the first character.

         if(flag.eq.-1) then
            if(cbuf(1:1).eq.'S') flag = 1
            if(cbuf(1:1).eq.'D') flag = 2
c >>> Not supported as of April 4, 1994. <<<
            if(cbuf(1:1).eq.'H') flag = 5
            if(cbuf(1:1).eq.'Y') flag = 6
         endif

         IF(flag.lt.0) THEN
            errm = 'Failed to interpret time units: ' // cbuf
            CALL xaerror(errm,1)
            ierr = -1
            RETURN
         ENDIF

      ENDIF

c Now convert to specified units.

      IF(mode.lt.0) THEN
         RETURN
      ELSEIF(mode.eq.2) THEN

c Converting to days.

         IF    (flag.eq.1) THEN
            dtime = dtime / 86400.d0
         ELSEIF(flag.eq.2) THEN
            RETURN
         ELSEIF(flag.eq.3) THEN
            dtime = dtime / 86400.d3
         ELSEIF(flag.eq.4) THEN
            dtime = dtime / 86400.d6
         ELSEIF(flag.eq.5) THEN
            dtime = dtime / 24.d0
         ELSEIF(flag.eq.6) THEN
            dtime = dtime * 365.25
         ELSE
            ierr = 1
            errm = 'xrdectun: illegal unit conversion flag'
         ENDIF

      ELSEIF(mode.eq.1) THEN

c Converting to seconds.

         IF    (flag.eq.1) THEN
            RETURN
         ELSEIF(flag.eq.2) THEN
            dtime = dtime * 86400.d0
         ELSEIF(flag.eq.3) THEN
            dtime = dtime / 1.d3
         ELSEIF(flag.eq.4) THEN
            dtime = dtime / 1.d6
         ELSEIF(flag.eq.5) THEN
            dtime = dtime * 3600.d0
         ELSEIF(flag.eq.6) THEN
            dtime = dtime * 3.15576d7
         ELSE
            ierr = 1
            errm = 'xrdectun: illegal unit conversion flag'
         ENDIF

      ELSE
         ierr = 1
         errm = 'xrdectun: illegal unit conversion mode'
      ENDIF

      IF(ierr.ne.0) CALL xaerror(errm,1)

      return
      end
