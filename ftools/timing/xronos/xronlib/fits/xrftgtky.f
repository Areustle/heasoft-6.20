      subroutine xrftgtky(lui,iopt,nrows,itype,ivect,htunit,ctunit
     &                   ,dtzero,dtoffset,dtint,dtsta,dtsto,npts
     &                   ,dfoffset,refflag,ierr)
      implicit none

c XRonos FiTs routine to Get Timing KeYwords.
c The returned parameters contain all information necessary to reconstruct 
c the full TIME for any row in the file (but not inside packets).

c This routine does many things.

c 1) Looks for the TIMESYS keyword.  If it exists and is equal to 
c    JD, MJD or TJD, the routine sets refflag = 2, 1 or 3, respectively.
c    Otherwise the routine looks for the MJDREF keyword (or the
c    MJDREFI MJDREFF pair).  If successful, reffalg = 0, otherwise 
c    refflag = -1 (meaning that the JD value for times in the file
c    cannot be found).

c 2) Looks for the TIMEUNIT keyword and tries to find what its value is.
c    If it is not there the routine will look for a TIME column and see
c    if its units can be decoded, issuing a warning if successful.
c    (See subroutine xrdectun).  If the keyword is found but cannot be
c    interpreted, a fatal error is issued from xrdectun.
c    Finally, if now unit keyword is found, but refflag > 0, the routine
c    assumes time are written in days.

c 3) Looks for a TIME column, and if present, looks for its TUNITnnn
c    keyword.  If not successful, the routine assumes that times in the
c    TIME column are written in the same units as those in the header.

c 4) If neither TIMEUNIT nor TUNITxxx for TIME column can be found,
c    the routine issues a warning.  If the calling routine knows
c    what the units are ahead of time, the warning can be supressed
c    by choosing a chattiness less than 5 when you initialize xwrite.

c 5) Looks for a TIMEZERO keyword, or its TIMEZERI TIMEZERF double.
c    If successful, the value is stored in dtzero, converted into days,
c    and returned.  All TIME column values should be offset by dtzero.

c 6) Looks for the integration time by first searching for the TIMEDEL
c    keyword and then the DELTIME keyword.  If successful, the value
c    is converted to seconds and returned in dtint.  If unsuccessful, and
c    if the file contains binned data (itype = 2), a warning is reported.
c    the warning can also be supressed
c    by choosing a chattiness less than 5 when you initialize xwrite.

c 7) Sets an internal offset value dtoffset that can be used to convert
c    all times to TJD.  If refflag < 0 this is not possible, and the
c    routine returns dtoffset = 0.d0.  If refflag < 0 and a TIME column
c    is present, and if the first value in the TIME column is
c    negative, dtoffset is set so that TIME column values start from
c    zero when dtoffset is added to them (and dtzero added don as well, 
c    of course).

c 8) Gets start and stop times.  It first looks for TSTART and TSTOP
c    keywords (or their double representations).  If successful, it
c    converts their values into days and adds on dtoffset.  If unsuccessful
c    and if a TIME column is present, it fetches the first and last
c    entries in the column, converts them into days, and adds on
c    dtzero and dtoffset (see 5 and 7 above).  The start and stop
c    are returned in dtsta and dtsto, respectively.

c 9) Looks at iopt(2) and iopt(3) for user-supplied first and last rows
c    to read from the file.  If either one is not zero, the TIME value
c    for repective rows is used to replace dtsta or dtsto, as appropriate.
c    The input value of npts is adjusted to the numberof rows that will
c    actually be read from the file.

c 10) Finally, for the case of packet data, the routine looks for the
c     TIMEPIXR keyword.  Its value should be between 0 and 1, and
c     is stored in dfoffset (not to be confused with dtoffset).  The
c     keyword indicates where in the packet bin the time is measured:
c     0 for the beginning, 1 for end and 0.5 for half way through.  Xronos
c     assumes halfway through by default, so this routine subtracts
c     from 0.5 the dfoffset before returning it. The return value is 
c     positive for any dfoffset less than 0.5 and negative if dfoffset
c     is greater than 0.5.
c
c  I  lui      (i)  Lu of input FITS file
c  I  iopt     (i)  Array of option flags
c  I  nrows    (i)  number of rows in the extension
c  I  itype    (i)  File type ( = 1 for EVENTS, 2 for RATE, 3 for PACKET)
c                             = 4 for GTIs
c  I  ivect    (i)  vector of selected column numbers
c I/O htunit   (i)  Flag for Time UNITs in extension Header
c                 (Taken as input if itype = 4)
c  O  ctunit   (i)  Flag for Time UNITs in TIME Column
c  O  dtzero   (d)  Offset from TIMEZERO keyword (days)
c I/O dtoffset (d)  Offset from MJDREF keyword (days)
c  O  dtint    (d)  Integration time (seconds)
c  O  dtsta    (d)  Start time (days)
c  O  dtsto    (d)  Stop time (days)
c  O  npts     (i)  Actual number of rows that will be used
c  O  dfoffset (d)  offset to have time in the center of each bin (days)
c  O  refflag  (i)  Flag that tells how xronos reconstructed the time
c  O  ierr     (i)  error status
c
c [h,c]tunit = 1 for seconds, = 2 for days.
c
c Author: eal  HSTX/GSFC HEASARC  March, 1994

      LOGICAL anynul
      character(16) cval,cdum
      PARAMETER (cdum = ' ')
      character(80) comm,errm
      INTEGER lui,refflag,htunit,ctunit,ierr,col1,col2,idum,itype
     &   ,iopt(*),nrows,npts,kystat,ivect(*),frow
      DOUBLE PRECISION mjdref,dtzero,dtint,dtoffset,dtsta,dtsto,
     $     dfoffset,ddum,nulvd
      data nulvd /-1.2d34/

      if(ierr.ne.0) return
      cval = ' '

c -------------------------------
c Time system and reference time.
c -------------------------------
      if(itype.ne.4) then

c Initialize htunit
         htunit=0

c Time column to be searched.

         col1 = ivect(1)
         col2 = ivect(1)

c Get TIMESYS / MJDREF.

         mjdref = 0.d0
         kystat = 0
         CALL ftgkys(lui,'TIMESYS ',cval,comm,kystat)

         if(cval     .eq.'MJD ') then
            refflag = 1
         elseif(cval     .eq.'JD  ') then
            refflag = 2
         elseif(cval     .eq.'TJD ') then
            refflag = 3
         else

c Ignore any other values of TIMESYS, and look for the MJDREF.

            kystat = 0
            CALL xrftgdky(lui,'MJDREFI','MJDREFF','MJDREF',mjdref
     &                   ,kystat)
            if(kystat.eq.0) then
               refflag = 0
            else
               refflag = -1
            endif
         endif
      else
         col1 = ivect(7)
         col2 = ivect(8)
      endif

c ---------------------------------------------------------------------
c Time units: establish what units are used in the file any way we can.
c If units can not be deciphered, exit with fatal error status
c ---------------------------------------------------------------------

c Units in header.
      
      ddum=1

      kystat = 0
      CALL ftgkys(lui,'TIMEUNIT',cval,comm,kystat)
      IF(kystat.eq.0) CALL xrdectun(cval,htunit,-1,ddum,ierr)

c Units on TIME column, if present.

      kystat = 0
      if(col1.gt.0) then
         CALL ftgkns(lui,'TUNIT',col1,1,cval,idum,kystat)
         CALL xrdectun(cval,ctunit,-1,ddum,ierr)

c If header time units were not found, use column units, if available.

         if(htunit.le.0) then
            if(ctunit.gt.0) then
               htunit = ctunit
               errm = ' Units not found for header timing keywords'
     &              // ' -- Assuming TIME column value.'
               CALL xwarn(errm,25)
            else

c No time unit keywords found at all.
c Last attempt is if refflag > 0, times are in days.

               if(refflag.gt.0) then
                  htunit = 2
                  ctunit = 2
                  errm = ' Assuming times are listed in days'
                  CALL xwarn(errm,25)
               else
c FATAL ERROR: No TIMEUNIT keyword and no TUNITnnn for TIME column.
                  ierr = -1048
               endif
            endif
         else

c If column time units were not found, use header units.

            if(ctunit.le.0) then
               ctunit = htunit
               errm = ' Units not found for TIME column'
     &              // ' -- Assuming TIMEUNIT keyword value.'
               CALL xwarn(errm,15)
            endif
         endif
      else
c FATAL ERROR: No TIMEUNIT keyword and no TIME column.
         if(htunit.lt.0) ierr = -1048
      endif
      IF(ierr.LT.0) THEN 
         errm = ' Could not decode time units.'
         CALL xwrite(errm,5)
         RETURN
      ENDIF
c -----------
c Get dtzero.
c -----------

      kystat = 0
      CALL xrftgdky(lui,'TIMEZERI','TIMEZERF','TIMEZERO',dtzero,kystat)
      IF(kystat.eq.0) THEN
         CALL xrdectun(cdum,htunit,2,dtzero,ierr)
      ELSE
         dtzero = 0.d0
      ENDIF
      IF(itype.lt.4) THEN

c ---------------------
c Get integration time.
c ---------------------

c First try.

         kystat = 0
         CALL    ftgkyd(lui,'TIMEDEL ',dtint   ,comm,kystat)
         if(kystat.ne.0) then 

c Second try.

            kystat = 0
            CALL ftgkyd(lui,'DELTIME ',dtint   ,comm,kystat)
         endif
         if(kystat.eq.0) then

c Success:  convert dtint to seconds.

            CALL xrdectun(cdum,htunit,1,dtint,ierr)
         else

c Find out if this is an events file.  If not, and there is no TIMEDEL
c column, and if TIMEDEL was not found, report error (calling routine
c decides what to do with it).

            errm = ' Warning: TIMEDEL (or DELTIME) keyword not found.'
            CALL xwrite(errm,5)

            if(itype.eq.1) then
               dtint = -1.d0
            elseif(ivect(5).gt.0) then
               dtint = 0.d0
               errm = ' Warning: Setting Integration time to 0. '//
     &                 'This may result in division by 0...'
               CALL xwrite(errm,5)
            else
C no TIMEDEL column or keyword and not EVENT LIST
               dtint=-2.d0
               errm = ' Could not decode integration time.'
               CALL xwrite(errm,5)
            endif
         endif

c -------------------
c Set interal offset.
c -------------------

         if(refflag.eq.1) then
c Convert from MJD to TJD.
            dtoffset = -   40000.d0
         elseif(refflag.eq.2) then
c Convert from  JD to TJD.
            dtoffset = - 2440000.5d0
         elseif((refflag.eq.0).and.(mjdref.gt.40000.d0)) then
            dtoffset = mjdref - 40000.d0
         elseif(refflag.lt.0) then
c Unsupported time system.  Take the times as they appear in the
c file unless they are negative.  If a negative time value is
c found, set dtoffset to offset times to start from zero.  This
c must assume that all files in the series have the same time 
c representation, thus this part only gets executed (dtoffset only 
c gets set) on the first file in the series.
            if(dtoffset.eq.0.d0) then
               ddum = 0.d0
               if(ivect(1).ne.0) then
                  anynul = .false.
                  CALL ftgcvd(lui,ivect(1),1,1,1,nulvd,ddum,anynul,ierr)
                  CALL xrdectun(cdum,ctunit,2,ddum,ierr)
                  ddum = ddum + dtzero
               else
                  if(dtzero.lt.0.d0) ddum = dtzero
               endif
               if(ddum.lt.0.d0) then
                  dtoffset = - ddum
               endif
            endif
         endif

      ENDIF
c
c TEST to avoid to add the big offset automatically
      If((iopt(11).EQ.1).and.(refflag.eq.0).and.(mjdref.gt.40000.d0))
     &  dtoffset=0.d0
c      write(*,*)'dtoffset, itype',dtoffset, itype
c ------------------------
c Get start and stop time.
c ------------------------

      kystat = 0
      CALL xrftgdky(lui,'TSTARTI','TSTARTF','TSTART',dtsta,kystat)
      CALL xrftgdky(lui,'TSTOPI' ,'TSTOPF' ,'TSTOP' ,dtsto,kystat)

      if((kystat.eq.0).and.(dtsta.lt.dtsto)) then
         CALL xrdectun(cdum,htunit,2,dtsta,ierr)
         CALL xrdectun(cdum,htunit,2,dtsto,ierr)
         dtsta = dtsta + dtoffset
         dtsto = dtsto + dtoffset
      else

c Second attempt: Search the TIME column for first and last values.
c No offset for this case.

         if(col1.gt.0) then
            anynul = .false.
            CALL ftgcvd(lui,col1,1    ,1,1,nulvd,dtsta,anynul,ierr)
            CALL ftgcvd(lui,col2,nrows,1,1,nulvd,dtsto,anynul,ierr)
            CALL xrdectun(cdum,ctunit,2,dtsta,ierr)
            CALL xrdectun(cdum,ctunit,2,dtsto,ierr)
            dtsta = dtsta + dtzero + dtoffset 
            dtsto = dtsto + dtzero + dtoffset 
c            IF (itype.ne.1) THEN   
            IF (dtint.ne.-1) THEN   
              dtsta = dtsta - dtint/86400.D0/2.D0
              dtsto = dtsto + dtint/86400.D0/2.D0
            ENDIF            
            IF(anynul.or.(ierr.ne.0)) GOTO 10
         else
            GOTO 10
         endif
      endif
      goto 20

10    continue
c xronos FATAL ERROR:

            ierr = -1046
            errm =' TSTART and TSTOP keywords missing from FITS header.'
            CALL xwrite(errm,5)

20    continue

c Apply options for first and last rows to read from rate tables.

      npts = nrows
      IF(itype.ne.4) THEN
         if(iopt(2).gt.0) then
            if(col1.le.0) then
               dtsta = dtsta + dtint*dble(iopt(2) - 1)/86400.d0
               npts = npts - (iopt(2) - 1)
            else
               frow  = iopt(2)
               anynul = .false.
               CALL ftgcvd(lui,col1,frow ,1,1,nulvd,dtsta,anynul,ierr)
               CALL xrdectun(cdum,ctunit,2,dtsta,ierr)
               dtsta = dtsta + dtzero + dtoffset 
               IF (dtint.ne.-1) dtsta = dtsta - dtint/86400.D0/2.D0
c               If (itype.ne.1) 
               npts = npts - (iopt(2) - 1)
            endif
         endif

         if(iopt(3).gt.0) then
            if(col1.le.0) then
               dtsto = dtsta + dtint*dble(iopt(3) - max(1,iopt(2)))
     &            /86400.d0
               npts = npts - (nrows - iopt(3))
            else
               frow  = iopt(3)
               anynul = .false.
               CALL ftgcvd(lui,col1,frow ,1,1,nulvd,dtsto,anynul,ierr)
               CALL xrdectun(cdum,ctunit,2,dtsto,ierr)
               dtsto = dtsto + dtzero + dtoffset 
               IF (dtint.ne.-1) dtsto = dtsto - dtint/86400.D0/2.D0
c               If (itype.ne.1) dtsto = dtsto + dtint/86400.D0/2.D0
               npts = npts - (nrows - iopt(3))
            endif
         endif
      ENDIF

c ---------------------------------------------------------------------
c Look for keyword to set dfoffset (for time centering on packet data).
c ---------------------------------------------------------------------

      kystat = 0
      CALL ftgkyd(lui,'TIMEPIXR',dfoffset,comm,kystat)
      IF(kystat.eq.0) then
c         dfoffset = dfoffset - 0.5d0
         dfoffset = 0.5d0 - dfoffset
      ELSE
         dfoffset = 0.d0
      ENDIF
      return
      end
c
