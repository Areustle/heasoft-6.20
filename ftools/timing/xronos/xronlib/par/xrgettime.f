C***************************************************************
C
C subroutine:  xrgettime.f
C              get time specific binning parameters
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
C      call xrgettime( dtint, dtsta, dtsto, 
C     &     nbdf, ipow2, isev, dtnb, nbint, 
C     &     status)
C
C variables:
C
c     I   dtint = longest bin duration in input file(s) (in secs)
c                 (=-1. for arrival time files)
c     I   dtsta, dtsto = start and stop times of infiles (days)
c     I   nbdf = default for no. of newbins/intv
c     I   ipow2 = 1 if nbint must be a power of 2, 0 otherwise
c     I   isev = if nonzero, be strict about minimum bin size
c     O   dtnb = newbin duration (secs)
c     O   nbint = no. of newbins/intv
C**********************************************************************


C
c

      SUBROUTINE xrgettime( dtint, dtsta, dtsto, 
     &     nbdf, ipow2, isev, dtnb, nbint, 
     &     status)
      implicit none
      include '../include/io.inc'
      include '../include/xronos.inc'
      integer irebflag,iv
      double precision dtnb_def
      logical dnear
      external dnear
      include '../include/xronos_init.inc'
      parameter (subname = 'xrgettime:')

c
c  Ask for newbin duration
c
c     for arr. time files or low severity (=0) analysis
c

      if(status.ne.0) return
c       define default
c default to original binning
      dtnb = dtint
c for 1 intv of nbdf newbin
      dv = (dtsto-dtsta)*86400.D0/(dble(nbdf)-.6D0)
      IF (dtint.LE.0.0 .OR. isev.EQ.0) THEN
c set d/f dtnb for nbdf newbin/intv or less
         IF (dv.GT.dtint) dtnb = dv
c                               !note that stat. above works also if dtint<0
c
c for high severity
      ELSE
         IF (dv.GT.dtint) THEN
            iv = int(dv/dtint)
            IF (iv.LT.dv/dtint) iv = iv + 1
            dtnb = dble(iv)*dtint
         ENDIF
      ENDIF
c
c       adjust the default no. of newbins if necessary
c no. of new bins expected for dtnb
      iv = int((dtsto-dtsta)*86400.D0/dtnb)
      IF (iv.LT.(dtsto-dtsta)*86400.D0/dtnb) iv = iv + 1
c to have nbdf at most as d/f
      IF (nbdf.GT.iv) nbdf = iv
c       for analysis requiring a power of 2
c nbdf to the next power of 2
      IF (ipow2.EQ.1) CALL xrpowtwo(nbdf, 1)


c
c  Write message for high severity
      IF (dtint.GT.0.0D0 .AND. isev.GT.0) THEN
         WRITE (context, 1103)
         call xwrite (context, 10)
 1103    FORMAT (' **** Warning: Newbin Time must be',
     &        ' an integer multiple of Minimum Newbin Time')
      ENDIF

c
c  write bin duration
c
      IF (dtint.GT.0.D0) THEN
c        iv=nint((dtsto-dtsta)*86400.d0/dtint)
         iv = int((dtsto-dtsta)*86400.D0/dtint)
         IF (iv.LT.(dtsto-dtsta)*86400.D0/dtint) iv = iv + 1
c
         WRITE (context, 1003) dtint
         call xwrite (context, 5)
 1003    FORMAT (' Minimum Newbin Time', G17.8, '  (s)')
         write(context, 1005) iv
         call xwrite(context,5)
 1005    format (' for Maximum Newbin No..', I17)
      ELSE
         WRITE (context, 1004)
         call xwrite (' ', 10)
         call xwrite (context, 10)
 1004    FORMAT (' Arrival Time Input File(s) ')
         call xwrite (' ', 10)
      ENDIF


      dtnb_def = dtnb

c
c
c  Write message for default newbin duration
      WRITE (context, 1102) dtnb, nbdf
      call xwrite (' ', 5)
      call xwrite(context,5)

 1102 FORMAT (' Default Newbin Time is:', G16.8,
     &     '(s) (to have 1 Intv. of ', I7, ' Newbins)')

      call xwrite(' Type INDEF to accept the default value',10)
      call xwrite (' ', 5)

      
      call uclgsd('dtnb',dtnb,status) 
C if INDEF, use default
      if(status.eq.3) then
         status=0
         dtnb=dtnb_def
         call uclpsd('dtnb',dtnb,status)
      endif
      if(status.NE.0) then
         context= 'Trouble with DTNB parameter'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif      
c
      IF ((dtnb.GT.0.D0 .AND. dtnb.LT.dtint) .AND. (.NOT.
     &     DNEAR(dtnb,dtint,1.d-8) )) status = 1002
      IF (dtnb.LE.0.D0 .AND. nint(dtnb).EQ.0) status = 1002
      IF (dtnb.LT.0.D0) THEN
         IF (dtint.GT.0.D0) THEN
            dtnb = -nint(dtnb)*dtint
         ELSE
            status=1041
            goto 999
         ENDIF
      ENDIF
c Rev. 3 stop
      IF (dtnb.GE.(dtsto-dtsta)*86400.D0) then
         status  = 1023
         context='Newbin duration is longer than total duration.'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif
      if(status.ne.0) then
         write(context,'(g17.8,
     $        ''is not a legal value for Newbin Time'')')
     $        dtnb
         errm = subname//' '//context
         call xaerror(errm, 5)
         GOTO 999
      ENDIF         

      WRITE (context, '('' dtnb (s) = '',g17.10)') dtnb
      call xwrite (context, 15)
c
c Reset newbin duration for high severity if necessary
      IF (dtint.GT.0.0D0 .AND. isev.GT.0) THEN
         IF (dtnb.GT.dtint) THEN
            iv = nint(dtnb/dtint)
            IF (iv.NE.dtnb/dtint) THEN
c Rev.2 stop
               WRITE (context, 1203) dble(iv)*dtint
               call xwrite (' ', 5)
               call xwrite (context, 5)
 1203          FORMAT ( ' **** Warning: Newbin',
     &              ' Time reset to ', G15.8, '  s')
               call xwrite (' ', 5)
            ENDIF
            dtnb = dble(iv)*dtint
         ENDIF
      ENDIF
c
c  Get no. of newbins per intv.
c

c set original default
      nbint = nbdf
c no. of new bins expected for dtnb
      iv = int((dtsto-dtsta)*86400.D0/dtnb)
      IF (iv.LT.(dtsto-dtsta)*86400.D0/dtnb) iv = iv + 1
c to have nbdf at most as d/f
c the defaults for powspec is set to 8192 
c to get long fft chnage parameter nbdf
c in the paramter file
      IF (nbint.GT.iv) nbint = iv
c
c the following statment allow to get the longer fft possible
c is comment out to avoid unreasonable long fft 
c      nbint=iv
c
c       for analysis requiring a power of 2
c approximate nbint to the next power of 2
      IF (ipow2.EQ.1) CALL xrpowtwo(nbint, 1)



 999  return
      end


