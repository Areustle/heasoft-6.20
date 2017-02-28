C***************************************************************
C
C subroutine:  xrgetnbin.f
C              get binning parameters
C
C written by:
C      Emily A. Greene
C      HEASARC/GSFC/NASA  Hughes STX
C      3/13/95
C
C modification history:
C     Lawrence Brown did some more 3/23/95-
C
C notes:
C      see xronos.inc for the meaning and current size of all parameter
C      arrays
C
C      many of the ipf and iflags values should be booleans also see
C      notes in xronos.inc
c
C      this routine is to replace xrgetnbint calls
C      also should be used in folding tasks since xrgetnbinf contained
C      some duplicate code
C      timing or folding specific tasks should first call:
C      xrgetfold or xrgettime respectively
C
C calling sequence:
C      call xrgetnbin( 
C
C variables:
C
c     I   dtint = longest bin duration in input file(s) (in secs)
c                 (=-1. for arrival time files)
c     I/O   dtsta, dtsto = start and stop times of infiles (days)
c     I   twia,twio = time window starts, stops (days)
c     I   nwi = no. of time windows (also for other window types)
c     I   ipf = parameter file flags
c     I   ipow2 = 1 if nbint must be a power of 2, 0 otherwise
c     I   dtnb = newbin duration (secs)
c     I/O nbint = no. of newbins/intv
c     I/O nintfm = no. of intvs/frame (=-1 in input for programs in which
c                  intvs. cannot be averaged and results cannot be rebinned;
c                  e.g. lc1 ; =-2 for programs in which results cannot be
c                  rebinned e.g. ops)
c     O   iflags = integer flags for output file (see below)
c     O   rflags = real flags for output file (see below)
C***************************************************************
c
      SUBROUTINE xrgetnbin( dtint, dtsta, dtsto, twia, twio, 
     $     nwi, ipf, ipow2,  dtnb, nbint, nintfm, progtype,
     &     iflags, rflags, status)

      implicit none
      include '../include/io.inc'
      include '../include/xronos.inc'
      integer k,iv,noint,idum,nbint_def,nintfm_def,ifmt,irebflag
      include '../include/xronos_init.inc'
      parameter (subname = 'xrgetnbin:')
      

      if(status.ne.0) return

c     
c     Determine if it is possible to rebin data
c     
c     meaning possible
         irebflag = 1
c     meaning impossible
         IF (nintfm.LT.0) irebflag = 0



C
c  write newbin duration and expected no. of new bins and intvs.
         
      iv = int((dtsto-dtsta)*86400.D0/dtnb)
      IF (iv.LT.(dtsto-dtsta)*86400.D0/dtnb) iv = iv + 1
      WRITE (context, 1304) dtnb
      call xwrite (' ', 10)
      call xwrite (context, 10)
 1304 FORMAT (' Newbin Time ...... ', G17.8, '  (s)')
      write(context, 1314)  iv
      call xwrite (context, 10)
 1314 format (' Maximum Newbin No. ', I17)
      call xwrite (' ', 5)

      
      nbint_def=nbint
      write(context,'('' Default Newbins per Interval are:'',i12)')
     $     nbint_def
      call xwrite(context,5)
      iv = (iv/(nbint+1)+1)
      if(iv.eq.1)then
         write(context,1315) iv, nbint
      else
         write(context,1316) iv, nbint
      endif
 1315 format (' (giving ', I7, ' Interval of ', I12, ' Newbins)')
 1316 format (' (giving ', I7, ' Intervals of ', I12, ' Newbins each)')
      call xwrite(context,5)
      call xwrite(' Type INDEF to accept the default value',10)
      call xwrite (' ', 5)
      
      call uclgsi('nbint',nbint, status)
      if(status.eq.3) then
         status=0
         nbint=nbint_def
         call uclpsi('nbint',nbint,status)
      endif
      
      
      if(status.NE.0) then
         context= 'Couldn''t get NBINT parameter'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif      
      
      
      IF (nbint.EQ.0 .OR. nbint.EQ.1 .OR. nbint.LT.-31) THEN
         status = 1002
         write(context,'(I9,
     $        ''is not a legal value for Newbins/Interval'')') nbint
         errm = subname//' '//context
         call xaerror(errm, 5)
         GOTO 999
      ENDIF
      IF (nbint.LT.0) nbint = 2**(-nbint)
      if (progtype(1:4).ne.'FOLD')  then
         IF (ipow2.EQ.1 ) THEN
            iv = nbint
c     iv set =0 if not a power of 2
            CALL xrpowtwo(iv, 0)
            IF (iv.EQ.0) THEN
c     set nbint to the next pow of 2
               CALL xrpowtwo(nbint, 1)
               WRITE (context, 1305) nbint
               call xwrite (context, 5)
 1305          FORMAT ( ' **** Warning: No. of Newbins/Intv. reset to '
     &              , I8)
            ENDIF
         ENDIF
         WRITE (context, '('' nbint = '',i10)')  nbint
         call xwrite (context, 15)
      endif
      
C     calculate expected number of intervals
      iv = int((dtsto-dtsta)*86400.D0/dtnb)
      IF (iv.LT.(dtsto-dtsta)*86400.D0/dtnb) iv = iv + 1
      noint = iv/nbint
      IF (noint.LT.float(iv)/float(nbint)) noint = noint + 1
      
      if(noint.eq.1)then
         write(context,1315) noint, nbint
      else
         write(context,1316) noint, nbint
      endif
      call xwrite(context, 15)

C     get intervals/frame if necessary
         WRITE (context, 1005) noint, nbint, dtnb
         call xwrite (context, 10)
 1005    FORMAT (' Maximum of ', I7, ' Intvs. with ', I12,
     &        ' Newbins of ', G17.6, ' (s) ')
      
      IF (nintfm.NE.-1) THEN
         
         nintfm_def = noint 
         write(context,'(''Default intervals per frame are:'',i10)')
     $        nintfm_def
         call xwrite(context,5)
         call xwrite('Type INDEF to accept the default value',10)
         call uclgsi('nintfm',nintfm,status)
         if(status.eq.3) then
            status=0
            nintfm=nintfm_def
            call uclpsi('nintfm',nintfm,status)
         endif
         
         if(status.ne.0) then
            context='Couldn''t get NINTFM parameter'
            errm = subname//' '//context
            call xaerror(errm, 5)
            goto 999
         endif
         
         IF (nintfm.LE.0) then
            status=1002
            write(context,'(I9,
     $       ''is not a legal value for Number of Intervals/Frame'')')
     $           nintfm
            errm = subname//' '//context
            call xaerror(errm, 5)
            GOTO 999
         ENDIF         
         
         
         write (context, '('' nintfm = '',i10)') nintfm
         call xwrite (context, 15)
      ENDIF


c     
c     Write set up values
c     
      IF (nintfm.NE.-1) THEN
         WRITE (context, 1006) nintfm
         call xwrite (context, 5)
 1006    FORMAT ( ' Results from up to ', I7,
     &        ' Intvs. will be averaged in a Frame')
      endif


c
c  Get result rebinning constant if necessary (>1 linear rebin, <1 log rebin)
c
      IF (irebflag.EQ.1) THEN
         rebin = 0.
         call uclgsr('rebin',rflags(1),status)
         if(status.NE.0) then
            context= 'Trouble with REBIN parameter'
            errm = subname//' '//context
            call xaerror(errm, 5)
            goto 999
         endif      

         IF (rebin.LT.-2.) then
            status = 1002
            write(context,'(g9.2,
     $           ''is not a legal value for Rebin'')')
     $           rflags(1)
            errm = subname//' '//context
            call xaerror(errm, 5)
            GOTO 999
         ENDIF         
         
         WRITE (context, '('' rebin = '',g14.8)') rflags(1)
         call xwrite (context, 15)

         IF (rflags(1).GT.1.) THEN
            WRITE (context, 1007) rflags(1)
            call xwrite (context, 10)
 1007       FORMAT (' Results will be rebinned',
     &           ' by a factor of ', F7.2)
         ENDIF
         IF (rflags(1).LT.-1.) THEN
            WRITE (context, 1008)  - 1.*rflags(1)
            call xwrite (context, 10)
 1008       FORMAT (' Results will be rebinned geometrically ',
     &           ' with a series of step ', F7.2)
         ENDIF
      ENDIF

c     
c     Write various warnings
c                                               !(note that iflags(16)=ipf(5))
c forced simultaneousness
      IF (iflags(16).EQ.1 .AND. iflags(10).GT.1) THEN
         WRITE (context, 1800) iflags(10)
         call xwrite (context, 5)
 1800    FORMAT ( ' **** Warning: Simultaneousness of the', I2,
     &        ' series ', 'will be forced ')
      ENDIF
c (note that rflags(2)=rpf(3))
c for rescaling factor for results
      IF (rflags(2).NE.1.) THEN
         WRITE (context, 1801) rflags(2)
         call xwrite (context, 5)
 1801    FORMAT ( ' **** Warning: All results and errors',
     &        ' will be multiplied by', G14.7)
      ENDIF
c (note that rflags(3)=rpf(4))
c for additive constant for results
      IF (rflags(3).NE.0.) THEN
         WRITE (context, 1802) rflags(3)
         call xwrite (context, 5)
 1802    FORMAT ( ' **** Warning: All results will be',
     &        ' added with ', G14.7)
      ENDIF




c maximum no. of frames expected
      iflags(12) = (noint-1)/abs(nintfm)+ 1
c
 999  RETURN
      END
c
c
