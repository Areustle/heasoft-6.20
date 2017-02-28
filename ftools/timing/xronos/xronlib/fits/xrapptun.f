      subroutine xrapptun(flag,mode,dtime,ierr)

c XRonos routine to APPly Time UNit conversion.

c  I  flag  (i) Units to convert from
c               = 1 seconds
c               = 2 days
c               = 3 milliseconds
c               = 4 microseconds
c               = 5 hours
c               = 6 years
c               = -1 information missing or not decoded.
c  I  mode  (i)  units to convert to (1 for seconds, 2 for days, 
c                others not supported)
c I/O dtime (d)  time value converted, in place
c  O  ierr  (i) error status

c Author:  EAL  NASA/Goddard, HSTX  April, 1994

      include '../include/io.inc'
      integer flag,mode,ierr
      double precision dtime
      parameter (subname = 'xrapptun:')

      IF(ierr.ne.0) return

      IF(mode.eq.2) THEN

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
            errm = 'xrapptun: illegal unit conversion flag'
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
            errm = 'xrapptun: illegal unit conversion flag'
         ENDIF

      ELSE
         ierr = 1
         errm = 'xrapptun: illegal unit conversion mode'
      ENDIF

      errm = subname//' '//errm
      IF(ierr.ne.0) CALL xaerror(errm, 5) 

      return
      end
