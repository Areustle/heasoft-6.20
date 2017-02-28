      SUBROUTINE TIMESTAMP(Cmdid, Status)

      implicit none
c
c  Write timestamp on the image
c
c  I  Cmdid   (i)  Command id
c  O  Status  (i)  Error flag (0=OK)
c
      integer Cmdid, Status
c
c  Local variables
c
      character(128) usertext, timetext
      INTEGER*4 userlen, timelen
      REAL*4 xpix, ypix, xchsiz, ychsiz
      LOGICAL isdisplay

      INTEGER*4 argc, color, lwidth
      REAL*4 csize
      character(10) font

      color = 1
      csize = 0.6
      lwidth = 1
      font = ' '
c
      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARI(Cmdid,'COLOR',color,Status)
      CALL GPARR(Cmdid,'CSIZE',csize,Status)
      CALL GPARI(Cmdid,'LWIDTH',lwidth,Status)
      CALL GPARS(Cmdid,'FONT',font,Status)
      if ( status.ne.0 ) return
      
      if (.not.isdisplay() ) then
         call XWARN ('No display',5)
         status = -1
         return
      endif
c
c plot timestamp
c
      call PGSAVE
      call text_pgstate(color, csize, lwidth, font)
      call PGQINF ('USER', usertext, userlen)
      call PGQINF ('NOW', timetext, timelen)
      call PGQCS (0, xchsiz, ychsiz)
      call ndc2wor (1., ychsiz/4., xpix, ypix)
      call PGPTXT (xpix, ypix, 0., 1.0, 
     &             usertext(1:userlen)//' '//timetext(1:timelen))
      call PGUNSA
c
c  Journal command
c
      call jrncmd(Cmdid, status)

      status = 0
      RETURN
      END
