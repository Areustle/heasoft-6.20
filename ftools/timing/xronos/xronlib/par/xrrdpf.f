C***************************************************************
C
C subroutine:  xrrdpf.f
C              get parameters that were "global" in XRONOS 4
C              check to see that they are reasonable
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
C      many of the ipf and iflags values should be booleans also see
C      notes in xronos.inc
c
C      this routine is to replace xrrdpf calls
C
C calling sequence:
C      call xrrdpf (cpf, ipf, rpf, dpf, iflags, rflags, status)
C
C variables:
C
C      cpf - string array - output - string parameters
C      ipf - integer array - output - integer parameters
C      rpf - real array    - output - real parameters
C      dpf - double array  - output - double parameters
C      iflags - integer array - output - integer flags
C      rflags - real array - output - real flags
C      ichat - integer - max of term. and log chattiness
C      progname - character - name of program (for ommitting irrelev. pars.)
C      status - integer - output - status of operation
C
C***************************************************************
      subroutine xrrdpf (cpf, ipf, rpf, dpf, iflags, rflags, ichat,
     $     progname, status)
      implicit none
C all arguments are defined in xronos.inc
      include '../include/io.inc'
      include '../include/xronos.inc'

      integer tchat, lchat, gapfill, errorbars, normalization
      logical clobber, forcestart, exposure, simultaneous
      logical fast, flatexpo
      double precision spwinbefore, spwinafter, rescale, offset
      integer nbuf,lbuf
      character(40) outfileroot
      character(160) logname
      include '../include/xronos_init.inc'
      real logunit(1)
      parameter (subname = 'xrrdpf:')


      if (status .ne. 0) return

C get the terminal chattiness parameter
      call uclgsi ('tchat', tchat, status)
      if (status .ne. 0) then
         context = ' Error getting TCHAT parameter'
         errm = subname//' '//context
         call xaerror (errm, 5)
         goto 999
      endif

C get the logfile chattiness parameter
      call uclgsi ('lchat', lchat, status)
      if (status .ne. 0) then
         context = ' Error getting LCHAT parameter'
         errm = subname//' '//context
         call xaerror (errm, 5)
         goto 999
      endif

C set up chattiness of program
      call xchaty(tchat, lchat)

C make ichat flag to pass for optimization of some routines
C since we can save a great deal of time by branching around
C *in loop* write/xwrite pairs.
      ichat=max(tchat,lchat)

      if(lchat.ne.0) then
C Open log file
C get log file name
         call uclgst ('logname', logname, status)
         if (status .ne. 0) then
            context = ' Error getting LOGNAME parameter'
            errm = subname//' '//context
            call xaerror (errm, 5)
            goto 999
         endif
         if ((logname .eq. 'INDEF').or.(logname .eq. ' ') .or. 
     $        (logname .eq. '-').or.(logname.eq.'default')) then
            logname='xronos.log'
         endif
         logname='+'//logname
         lbuf=160
         call logger(3,logunit,nbuf,logname,lbuf)
         if(nbuf.lt.0) then
            errm = subname//' '//'Error opening log file'
            call xaerror(errm, 5)
            goto 999
         endif
      endif

C and identify the task
C        LEB add date and time (at some point)
      call xwrite (' ', 5)
      call xwrite (taskname, 5)
      call xwrite (' ', 5)


C get the outfile status
C the references in the code to cpf(1) should be removed, and faopen and
C ffinit used to directly check the clobber parameter
      call uclgsb ('clobber', clobber, status)
      if (clobber) then
C create new output file
         cpf(1) = 'NEW'
      else
         cpf(1) = 'UNKNOWN'
      endif
c
      if(progname(1:2) .ne. 'ld'.and.progname(1:2) .ne. 'ls') then
C get output file root name
         call uclgst ('outfileroot', outfileroot, status)
         if (status .ne. 0) then
            context = ' Error getting OUTFILEROOT parameter'
            errm = subname//' '//context
            call xaerror (errm, 5)
            goto 999
         endif
         cpf(3) = outfileroot
      endif

C get the number of points to use to fill in gaps
      If(progname(1:2) .ne. 'ld') then
         call uclgsi ('gapfill', gapfill, status)
         if (status .ne. 0) then
            context = ' Error getting GAPFILL parameter'
            errm = subname//' '//context
            call xaerror (errm, 5)
            goto 999
         endif
         if (gapfill .lt. 0) then
             write (context, '(a11,i4,a32)') ' GAPFILL = ', gapfill,
     &        ' must be greater than 0 newbins '
             errm = subname//' '//context
             call xaerror (errm, 5)
             goto 999
         endif
         ipf(6) = gapfill
         iflags(17) = gapfill
C get whether to force the start at the beginning of the 1st window
         call uclgsb ('forcestart', forcestart, status)
         if (status .ne. 0) then
             context = ' Error getting FORCESTART parameter'
             errm = subname//' '//context
             call xaerror (errm, 5)
             goto 999
         endif
         if (forcestart) then
            ipf(1) = 1
         else
            ipf(1) = 0
         endif

C get number of points to determine how to do error calculation
         call uclgsi ('errorbars', errorbars, status)
         if (status .ne. 0) then
            context = ' Error getting ERRORBARS parameter'
            errm = subname//' '//context
            call xaerror (errm, 5)
            goto 999
         endif
         if (errorbars .lt. 2) then
            write (context, '(a21,i5)') 
     &             ' ERRORBARS too small ', errorbars
            errm = subname//' '//context
            call xaerror (errm, 5)
            context =
     &        ' Minimum number of points for sigma from scatter is 2'
            errm = subname//' '//context
            call xaerror (errm, 5)
            goto 999
         endif
         ipf(2) = errorbars
         iflags(6) = errorbars

C get whether to do an exposure profile or not
         call uclgsb ('exposure', exposure, status)
         if (status .ne. 0) then
            context = ' Error getting EXPOSURE parameter'
            errm = subname//' '//context
            call xaerror (errm, 5)
            goto 999
         endif
         if (exposure) then
            ipf(3) = 1
         else
            ipf(3) = 0
         endif

C get how to do normalization
         call uclgsi ('normalization', normalization, status)
         if (status .ne. 0) then
            context = ' Error getting NORMALIZATION parameter'
            errm = subname//' '//context
            call xaerror (errm, 5)
            goto 999
          endif
         if ((normalization .lt. -2) .or. (normalization .gt. 3)) then
            write (context, '(a17,i4,a25)') ' NORMALIZATION = ',
     &        normalization, ' must be between -2 and 3'
            errm = subname//' '//context
            call xaerror (errm, 5)
            goto 999
         endif
         ipf(4) = normalization
         iflags(7) = normalization

C get whether to force strict simultanaous
         call uclgsb ('simultaneous', simultaneous, status)
         if (status .ne. 0) then
             context = ' Error getting SIMULTANEOUS parameter'
             errm = subname//' '//context
             call xaerror (errm, 5)
             goto 999
         endif
         if (simultaneous) then
            ipf(5) = 1
         else
            ipf(5) = 0
         endif
         iflags(16) = ipf(5)

C get number of seconds in special window before
         call uclgsd ('spwinbefore', spwinbefore, status)
         if (status .ne. 0) then
            context = ' Error getting SPWINBEFORE parameter'
            errm = subname//' '//context
            call xaerror (errm, 5)
            goto 999
         endif
         if (spwinbefore .lt. 0) then
             write (context, '(a15,f8.1,a32)') ' SPWINBEFORE = ',
     &        spwinbefore, ' must be greater than 0. seconds'
             errm = subname//' '//context
             call xaerror (errm, 5)
             goto 999
         endif
         rpf(1) = spwinbefore

C get number of seconds in special window after
         call uclgsd ('spwinafter', spwinafter, status)
         if (status .ne. 0) then
             context = ' Error getting SPWINAFTER parameter'
             errm = subname//' '//context
             call xaerror (errm, 5)
             goto 999
         endif
         if (spwinafter .lt. 0) then
             write (context, '(a15,f8.1,a32)') ' SPWINAFTER = ',
     &        spwinafter, ' must be greater than 0. seconds'
             errm = subname//' '//context
             call xaerror (errm, 5)
             goto 999
         endif
         rpf(2) = spwinafter

C get rescaling factor on output
         call uclgsd ('rescale', rescale, status)
         if (status .ne. 0) then
             context = ' Error getting RESCALE parameter'
             errm = subname//' '//context
             call xaerror (errm, 5)
             goto 999
         endif
         rpf(3) = rescale
         rflags(2) = rescale

C get offset value for final output
         call uclgsd ('offset', offset, status)
         if (status .ne. 0) then
            context = ' Error getting OFFSET parameter'
            errm = subname//' '//context
            call xaerror (errm, 5)
            goto 999
         endif
         rpf(4) = offset
         rflags(3) = offset
      
C get offset value for final output
         call uclgsb ('fast', fast, status)
         if (status .ne. 0) then
            context = ' Error getting FAST parameter'
            errm = subname//' '//context
            call xaerror (errm, 5)
            goto 999
         endif
         iflags(15) = 0
         if(fast) iflags(15)=1
       endif
c
c get the flag for efold and efsearch to apply or/not the exposure
c calculation for fast pulsar with event file
c
      if(progname(1:2) .eq. 'es'.or.progname(1:2) .eq. 'ef') then
         call uclgsb ('flatexpo', flatexpo, status)
         if (status .ne. 0) then
             context = ' Error getting FLATEXO parameter'
             errm = subname//' '//context
             call xaerror (errm, 5)
             goto 999
         endif
         iflags(19)=0
         if(flatexpo)iflags(19)= 1
c         write(*,*)'iflags(19)',iflags(19)
      endif
 
 999  return
      end

