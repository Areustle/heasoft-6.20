C***************************************************************
C
C subroutine:  xrgetname
C     get parameters for progname, program type, and number of time
C     series involved in this task (needed for decision making)
C
C written by:
C      Lawrence E. Brown
C      HEASARC/GSFC/NASA  Hughes STX
C      3/30/95
C
C modification history:
C
C notes:
C     We may eventually want to try to make this get the task name 
C     some other way.
C
C calling sequence:
C      call xrgetname(nser,ipow2,iavgreb,status)
C
C variables:
C     nser       O  How many time series does this task need?
C     ipow2      O   1 if nbint must be a power of 2 in fast mode,0 otherwise
c     iavgreb    O  -1 for programs in which intvs. cannot be averaged 
C                   and results cannot be rebinned; (e.g. lc1) ; =-2 for
C                   programs in which results cannot be rebinned (e.g. ops)
C                   (=initial value of nintfm)
C     nbdf       O  default newbins/interval (if TIME -recommended value - 512)
C                   default phasebins/period (if FOLD -recommended value - 8)
C
C***************************************************************
      subroutine xrgetname(nser,ipow2,iavgreb,nbdf,status)
      implicit none
      include '../include/io.inc'
      integer iavgreb
      include '../include/xronos.inc'
      include '../include/xronos_init.inc'
      parameter (subname = 'xrgetname:')

      
      if(status.ne.0) return
      
      call uclgsi('nser',nser,status)
      if(status.ne.0) then
         context='Couldn''t get NSER parameter'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif


      call uclgsi('ipow2',ipow2,status)
      if(status.ne.0) then
         context='Couldn''t get IPOW2 parameter'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif


      call uclgsi('iavgreb',iavgreb,status)
      if(status.ne.0) then
         context='Couldn''t get IAVGREB parameter'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif

      call uclgsi('nbdf',nbdf,status)
      if(status.ne.0) then
         context='Couldn''t get NBDF parameter'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif




 999  return
      end





