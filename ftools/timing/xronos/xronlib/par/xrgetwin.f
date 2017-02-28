C***************************************************************
C
C subroutine:  xrgetwin
C              get window parameter and read window file
C
C written by:
C      Emily A. Greene
C      HEASARC/GSFC/NASA  Hughes STX
C      3/1/95
C
C modification history:
C
C notes:
C      see xronos.inc for the meaning and current size of all parameter
C      arrays
C
C      this routine is to replace xrgetwin.f calls
C
C calling sequence:
C      call xrgetwin (nser, cpf, twia, twio, pwi, pwia, pwio,
C          fwia, fwio, ewia, ewio, nwi, nwito, status)
C
C variables:
C
C      nser - integer - input - number of time series (iflags(10))
C      cpf - string array - input - array of string parameters
C      twia - double array - output - time window start times
C      twio - double array - output - time window stop times
C      pwi - double array - output - phase window epoch and period
C      pwia - real array - output - phase window start
C      pwio - real array - output - phase window stop
C      fwia - real array - output - flux window start
C      fwio - real array - output - flux window stop
C      ewia - real array - output - exposure window start
C      ewio - real array - output - exposure window stop
C      nwi - integer array - output - number of each type of windows
C      nwito - integer - output - total number of all windows = iflags(5)
C      dtsta, dtsto - in/out - start and stop times of infiles (days)
C      status - integer - output - status of operation
C
C***************************************************************
      subroutine xrgetwin (nser, cpf, twia, twio, pwi, pwia,
     &     pwio, fwia, fwio, ewia, ewio, nwi, nwito, 
     $     dtsta,dtsto,status)

      include '../include/io.inc'
      include '../include/xronos.inc'
      
      integer itim(5),jtim(5)
      double precision dssta,dssto
      integer  nwito, lenact
      character(160) dpath
      character(160) window
      character(160) tmpwindow
      include '../include/xronos_init.inc'
      parameter (subname = 'xrgetwin:')


      if (status .ne. 0) return
c
      call uclgst ('window', window, status)
      tmpwindow=window
      call upc(tmpwindow)
C if INDEF use default windows or none specified, use default values
      if ((tmpwindow .eq. 'INDEF').or.(window .eq. '-')
     $     .or.(tmpwindow.eq.'DEFAULT').or.
     $     (window .eq. ' ').or.(tmpwindow.eq.'NONE')) then
C Use default windows only for "Fourier type" programs
         if((cpf(5).ne.'ac'.and.cpf(5).ne.'cc'.and.cpf(5).ne.'ts'.and.
     $        cpf(5).ne.'ps').or.(window.eq.' ')
     $        .or.(tmpwindow.eq.'NONE')) then
            window = 'NONE'
         else
            call uclgst('dpath',dpath,status)
            if (dpath .eq. 'XRDEFAULTS') then
              call getenv('XRDEFAULTS', dpath)
              if(index(dpath, '/') .ne. 0) then
                dpath = dpath(1:lenact(dpath))//'/'
              elseif(index(dpath, '[') .eq. 0) then
                dpath = ' '
              endif
            endif
            window= dpath(1:lenact(dpath))//'default_win.wi'
            call uclpst ('window', window, status)
         endif
      endif
      if (status .ne. 0) then
         context = ' Error with WINDOW or DPATH parameter '
         errm = subname//' '//context
         call xaerror (errm, 5)
         goto 999
      endif

      context='Using window file: '//window
      call xwrite(context,15)

      if (window.ne.'NONE') then
c read window file
         CALL xrrdwin(nser,window, twia, twio, pwi, pwia,
     &        pwio, fwia, fwio, ewia, ewio, nwi, nwito, status)
         
         if (status .ne. 0) then
            context = ' Error reading window file: '//window
            errm = subname//' '//context
            call xaerror (errm, 5)
            goto 999
         endif

      endif
         
c     
c type windows
      CALL xrtywin(nser, twia, twio, pwi, pwia, 
     &     pwio, fwia, fwio, ewia, ewio, nwi, nwito, status)

      if (status .ne. 0) then
         context = 'Error reporting window information.'
         errm = subname//' '//context
         call xaerror (errm, 5)
         goto 999
      endif

C Get expected start and stop time from window parameters

      do idum=1,5
         itim(idum)=0
         jtim(idum)=0
      enddo

c
c  Determine expected start and stop time
c
      IF (nwi(1).NE.0) THEN
c if 1st time wind. starts later
         IF (dtsta.LT.twia(1)) dtsta = twia(1)
c if last time wind. stops earlier
         IF (dtsto.GT.twio(nwi(1))) dtsto = twio(nwi(1))
c       fatal error condition for non-overlapping time windows
         IF (dtsto.LT.twia(1) .OR. dtsta.GT.twio(nwi(1))) THEN
            status = -1033
            errm = subname//' '//
     &            ' Time windows and data do not overlap '
            call xaerror (errm, 5)
            goto 999
         ENDIF
      ENDIF
c  condition to force start to be at the start of first window
      IF (ipf(1).EQ.1 .AND. nwi(1).NE.0) THEN
         dtsta = twia(1)
         WRITE (context, 1000)
         call xwrite (context, 5)
 1000    FORMAT ( ' **** Warning :',
     &        ' Start time forced to the start of first time window !')
      ENDIF
c  ... but no time window specified!
      IF (ipf(1).EQ.1 .AND. nwi(1).EQ.0) then
         WRITE (context, 1001)
         call xwrite (context, 5)
 1001 FORMAT ( ' **** Warning :',
     &     ' No time window specified: cannot force start time !')
         endif
c
c  Write expected start and stop times
c
      CALL xrdhms(dtsta, dssta, itim, 1)
      CALL xrdhms(dtsto, dssto, jtim, 1)
      WRITE (context, 1002) dtsta, (itim(k), k=2, 5)
      call xwrite (' ', 10)
      call xwrite (context, 10)
 1002 FORMAT ( ' Expected Start ... ', F17.11, '  (days)     ', 
     &     I3,':', I2, ':', I2, ':', I3, '  (h:m:s:ms) ')
      write(context,1102)  dtsto, (jtim(k), k=2, 5)
      call xwrite(context,10)
 1102 FORMAT (' Expected Stop .... ', F17.11, '  (days)     ',
     &     I3, ':', I2, ':', I2, ':', I3, '  (h:m:s:ms) ')
      call xwrite (' ', 10)


c
 999  RETURN
      END
c
c
