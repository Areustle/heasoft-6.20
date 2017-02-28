      subroutine mkcolor 
      implicit none

      include 'dynmem.inc'
c 
c Generates hardness ratio or true color image
c
      character(1024) rfile, gfile, bfile, outfile
      INTEGER*4 ichat, lchat, tchat
      character(80) log_file, istring, program, errm
      INTEGER*4 lenact, status, parse, frstat
      LOGICAL clobber

      integer p_rmap, p_gmap, p_bmap, p_outmap, p_hist
      integer szx, szy, szhist
      real*8 rmin, rmax, gmin, gmax, bmin, bmax
      real*8 datamin, datamax

      integer i, base, mxinten, szlvl
      integer p_rlvl, p_glvl, p_blvl
      integer cutmode
      real cut(6)

c Dummy variables for log file.

      DATA istring,parse /' ',0/

c
c Initialize variable
c Set up FITSIO and XPI, and get parameters.
      status=0
      program='mkcolor'
      CALL mkcinit (rfile, gfile, bfile, outfile, cutmode,
     &              cut, base, tchat,lchat,clobber,status)
      IF (status.NE.0) GOTO 900
      mxinten = base-1

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
      call mkcrdsz(rfile, gfile, bfile, szx, szy, status)
      if ( status.ne.0 ) goto 900
      write(istring,*) ' Image size: ', szx, ' x ', szy
      call rmvxbk(istring(2:))
      call xwrite(istring, 10)
c
c Allocate image maps
c
      call mkcalloc(1, szx, szy, p_rmap, status)
      call mkcalloc(1, szx, szy, p_gmap, status)
      call mkcalloc(1, szx, szy, p_bmap, status)
      call mkcalloc(1, szx, szy, p_outmap, status)
      if ( status.ne.0 ) goto 800
c
c Read in image maps
c
      call mkcread(rfile, memi(p_rmap), szx, szy, rmin, rmax, status)
      call mkcread(gfile, memi(p_gmap), szx, szy, gmin, gmax, status)
      call mkcread(bfile, memi(p_bmap), szx, szy, bmin, bmax, status)
      if ( status.ne.0 ) goto 800

c     cut(1) = MIN(rmin,gmin,bmin)
c     cut(2) = MAX(rmax,gmax,bmax)
c     cut(3) = cut(1)
c     cut(4) = cut(2)
c     cut(5) = cut(1)
c     cut(6) = cut(2)

      if ( cut(1).lt.0. ) cut(1) = rmin
      if ( cut(2).lt.0. ) cut(2) = rmax
      if ( cut(3).lt.0. ) cut(3) = gmin
      if ( cut(4).lt.0. ) cut(4) = gmax
      if ( cut(5).lt.0. ) cut(5) = bmin
      if ( cut(6).lt.0. ) cut(6) = bmax
      if ( cutmode.eq.0 ) then
c
c Allocate histogram buffer and level arrays
c
         szhist = nint(MAX(rmax,gmax,bmax))
         call mkcalloc(1, szhist, 1, p_hist, status)
         szlvl = mxinten+1
         call mkcralloc(1, szlvl, 1, p_rlvl, status)
         call mkcralloc(1, szlvl, 1, p_glvl, status)
         call mkcralloc(1, szlvl, 1, p_blvl, status)
c
c Calculate levels 
c
         call mkclvl(memi(p_rmap),szx,szy,memi(p_hist),szhist,
     &               memr(p_rlvl),mxinten,status)
         call mkclvl(memi(p_gmap),szx,szy,memi(p_hist),szhist,
     &               memr(p_glvl),mxinten,status)
         call mkclvl(memi(p_bmap),szx,szy,memi(p_hist),szhist,
     &               memr(p_blvl),mxinten,status)
c        print*,' R:', memr(p_rlvl), memr(p_rlvl+1), memr(p_rlvl+2),
c    &                 memr(p_rlvl+3)
c        print*,' G:', memr(p_glvl), memr(p_glvl+1), memr(p_glvl+2),
c    &                 memr(p_glvl+3)
c        print*,' B:', memr(p_blvl), memr(p_blvl+1), memr(p_blvl+2),
c    &                 memr(p_blvl+3)
c
c Free histogram buffer
c
         call mkcalloc(0, szhist, 1, p_hist, status)

      endif
c
c Create true color image
c
      call mkccalc(memi(p_rmap), memi(p_gmap), memi(p_bmap), 
     &             memi(p_outmap), szx, szy, cutmode, cut, 
     &             memr(p_rlvl), memr(p_glvl), memr(p_blvl),
     &             mxinten, base, datamin, datamax, status)
c
c Write true color image
c
      call mkcwrite(rfile, outfile, memi(p_outmap), szx, szy, datamin,
     &              datamax, base, clobber, status)
c
c  Free buffers
c
 800  CONTINUE
      frstat = 0
      call mkcalloc(0, szx, szy, p_rmap, frstat)
      call mkcalloc(0, szx, szy, p_gmap, frstat)
      call mkcalloc(0, szx, szy, p_bmap, frstat)
      call mkcalloc(0, szx, szy, p_outmap, frstat)
c
c Deallocate level arrays from histogram calculation
c
      if ( cutmode.eq.0 ) then
         call mkcralloc(0, szlvl, 1, p_rlvl, status)
         call mkcralloc(0, szlvl, 1, p_glvl, status)
         call mkcralloc(0, szlvl, 1, p_blvl, status)
      endif

 900  CONTINUE
      if ( status.gt.0 ) then
c        call ftgerr(status, errm)
c        call xwrite(errm, 5)
         call ftrprt('STDERR', status)
      endif
      END
