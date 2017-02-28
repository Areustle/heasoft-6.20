C***************************************************************
C
C subroutine:  xrgetlc.f
C              get lc1,lc2,... specific binning parameters
C
C written by:
C      Lawrence E Brown
C      HEASARC/GSFC/NASA  Hughes STX
C      3/23/95
C
C modification history:
C
C notes:
C      see xronos.inc for the meaning and current size of all parameter
C      arrays
C
C
C calling sequence:
C      call xrgetlc(dtnb, nbint, tunits, status)
C
C variables:
C
C     dtnb     I  newbin duration (secs)
C     nbint    I  no. of newbins/intv
C     tunits   O  time axis units {0=s from start of current intv.,1=s,2=h,
c                 3=d,4=s from start of 1st interval} (=iflags(2))
C
C***************************************************************
      SUBROUTINE xrgetlc(dtnb, nbint, tunits, status)
      implicit none
      include '../include/io.inc'
      include '../include/xronos.inc'
      integer tunits,tunits_def
      double precision rv
      include '../include/xronos_init.inc'
      parameter (subname = 'xrgetlc:')


      if(status.ne.0) return
c
c Ask for time units (if program is lc1,lc2,lc3) in output file
c
c     set d/f according to interval duration
c     intv. duration in secs
      rv = dble(nbint)*dtnb
c     secs  from start
      tunits = 0
c     secs  from start of hour
      IF (rv.GT.10.) tunits = 1
c     hours from start of day
      IF (rv.GT.3600.) tunits = 2
c     days
      IF (rv.GT.86400.) tunits = 3
C     Following line disabled due to XPI bug!!!

      tunits_def = tunits
c     tunits is now a hidden parameter
c      write(context,'('' Default time units are:'',i10)')
c     $     ,tunits_def
c      call xwrite(context,5)
c      call xwrite(' Type INDEF to accept the default value',10)

      call uclgsi('tunits',tunits,status)
      if(status.eq.3) then
         status=0
         tunits=tunits_def
         call uclpsi('tunits',tunits,status)
      endif

      if(status.NE.0) then
         context= 'Couldn''t get tunits parameter'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif      
 
C     XPI/IRAF already take care of this. It won't let you enter
C     a value outside of range
c      IF (tunits.LT.0 .OR. tunits.GT.4) then
c         status = 1002
c         write(context,'(I9,
c     $        ''time units must be in range (0-4)'')')
c     $        tunits
c         call xaerror(context, 5)
c         call xaerror('Must be 0,1,2,3, or 4', 5)
c         GOTO 999
c      ENDIF         
      
      WRITE (context,'('' tunits = '',i10)') tunits
      call xwrite(context,15)
 999  return
      end

      
      
