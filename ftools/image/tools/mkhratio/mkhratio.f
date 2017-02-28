      subroutine mkhratio 
      implicit none

      include 'dynmem.inc'
c 
c Generates hardness ratio image
c
      character(1024) afile, bfile, outfile
      INTEGER*4 ichat, lchat, tchat
      character(80) log_file, istring, program, errm
      INTEGER*4 lenact, status, parse, frstat
      LOGICAL clobber

      integer p_amap, p_bmap, p_outmap
      integer szx, szy

      integer sclmode, nbins, cut(2)
      real sclfact
      real*8 amin, amax, bmin, bmax, datamin, datamax

c Dummy variables for log file.

      DATA istring,parse /' ',0/

c
c Initialize variable
c Set up FITSIO and XPI, and get parameters.
      status=0
      program='mkhratio'
      CALL mkhinit (afile, bfile, outfile, sclmode, sclfact,
     &              nbins, cut, tchat, lchat, clobber, status)
c
      IF (status.NE.0) GOTO 900
c open the log file if necessary tchat>=lchat
c reset the internal chatness to lchat
c
      CALL xchaty(tchat,lchat)
      ichat=lchat
      CALL XWRITE(program, ichat) 
      log_file='+'//program(1:lenact(program))//'.log'
      IF (lchat.GE.tchat) THEN
         CALL SETLOG (istring, parse, log_file, ' ')
      ENDIF
c
c Look up image size
c
      call mkhrdsz(afile, bfile, szx, szy, status)
      if ( status.ne.0 ) goto 900
      write(istring,*) ' Image size: ', szx, ' x ', szy
      call rmvxbk(istring(2:))
      call xwrite(istring, 10)
c
c Allocate image maps
c
      call mkhalloc(1, szx, szy, p_amap, status)
      call mkhalloc(1, szx, szy, p_bmap, status)
      call mkhralloc(1, szx, szy, p_outmap, status)
      if ( status.ne.0 ) goto 800
c
c Read in image maps
c
      call mkhread(afile, memi(p_amap), szx, szy, amin, amax, status)
      call mkhread(bfile, memi(p_bmap), szx, szy, bmin, bmax, status)
      if ( status.ne.0 ) goto 800
c
c Create hardness ratio image
c
      call mkhcalc(memi(p_amap), memi(p_bmap), memr(p_outmap), szx, szy, 
     &             sclmode, sclfact, nbins, cut, datamin, datamax, 
     &             status)
c
c Write hardness ratio image
c
      call mkhwrite(afile, outfile, memr(p_outmap), szx, szy, 
     &              datamin, datamax, clobber, status)
c
c  Free buffers
c
 800  CONTINUE
      frstat = 0
      call mkhalloc(0, szx, szy, p_amap, frstat)
      call mkhalloc(0, szx, szy, p_bmap, frstat)
      call mkhralloc(0, szx, szy, p_outmap, frstat)

 900  CONTINUE
      if ( status.gt.0 ) then
c        call ftgerr(status, errm)
c        call xwrite(errm, 5)
         call ftrprt('STDERR', status)
      endif
      END
