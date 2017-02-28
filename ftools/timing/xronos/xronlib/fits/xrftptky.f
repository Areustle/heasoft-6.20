      subroutine xrftptky(luo,tstart,tstop,timeunit,tzero,ftstat)

c TIming routine to Put Timing KeyWords into a xronos-readable FiTs file header.

c  I  luo      (i)  Lu of output file
c  I  tstart   (d)  Start time value
c  I  tstop    (d)  Stop time value
c  I  timeunit (c)  Units for header timing data
c  I  tzero    (d)  Zero-point offset for TIME column

c Author:  eal   February 1994, NASA/Goddard Space Flight Center

      include '../include/io.inc'
      character*(*) timeunit
      character(80) comm
      integer ftstat,luo,ival
      double precision tstart,tstop,tzero,dval
      parameter (subname = 'xrftptky:')

c Write each keyword directly to the header, or delete it and write a
c new one if it already exists.

      ival = int(tstart)
      dval = tstart - dble(ival)
      comm = 'Start time for this extension'
      CALL xrftpdky(luo,'TSTARTI','TSTARTF','TSTART',ival,dval,comm
     &             ,ftstat)

      ival = int(tstop)
      dval = tstop - dble(ival)
      comm = 'Stop time for this extension'
      CALL xrftpdky(luo,'TSTOPI','TSTOPF','TSTOP',ival,dval,comm
     &             ,ftstat)
      if(ftstat.ne.0) goto 999

      comm = 'Units for header timing keywords'
      ftstat = 0
      CALL ftdkey(luo,'TIMEUNIT',ftstat)
      ftstat = 0
      CALL ftpkys(luo,'TIMEUNIT',timeunit,comm,ftstat)

      ival = int(tzero)
      dval = tzero - dble(ival)
      comm = 'Zero-point offset for TIME column'
      CALL xrftpdky(luo,'TIMEZERI','TIMEZERF','TIMEZERO',ival,dval,comm
     &             ,ftstat)

999   continue
      errm = 'xrftptky: writing output header timing keywords'
      errm = subname//' '//errm
      if(ftstat.ne.0) CALL xaerror(errm,1)

      return
      end
