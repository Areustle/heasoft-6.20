      SUBROUTINE EXCESS(Cmdid, Map, Szx, Szy, Mapid, Status)
      IMPLICIT NONE
c
c Find excesses
c
c  I  cmdid   (i)  Command id
c  I  map     (i)  Image map
c  I  szx/y   (i)  Size of map
c  I  mapid   (s)  Map id string
c  O  status  (i)  Error flag (0 = OK)
c
      INTEGER*4 Cmdid, Szx, Szy, Status
      real*4 Map(Szx,Szy)
      character*(*) Mapid

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/startup.inc'
      INCLUDE '../include/io.inc'
      INCLUDE 'backgd.inc'
      INCLUDE 'excess.inc'
c
c  Local variables
c
      CHARACTER*(MAX_FILELEN) outfile, infile
      INTEGER*4 argc, boxsize
      REAL*4 user_threshold_scal
      LOGICAL bright, pex, isloaded, isdisplay
      character(1) maptype

      INTEGER*4 itel, iboxx, di
      REAL*4 disss
      REAL*8 zmx, zmy, xcen, ycen, xoff, yoff, datamin, datamax

      outfile = ' '
      infile = ' '
      
      user_threshold_scal = 1.
      boxsize = 0
      bright = .FALSE.
      pex = .FALSE.

      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARS(Cmdid,'INFILE',infile,Status)
      CALL GPARS(Cmdid,'OUTFILE',outfile,Status)
      CALL GPARL(Cmdid,'BRIGHT',bright,Status)
      CALL GPARL(Cmdid,'PLOT_EXCESSES',pex,Status)
      CALL GPARR(Cmdid,'THR_SCALING',user_threshold_scal,Status)
      CALL GPARI(Cmdid,'SOURCE_BOX_SIZE',boxsize,Status)
      if ( status.ne.0 ) return
c
      call qustrip(infile)
      call qustrip(outfile)

      if ( .not.isloaded(mapid) ) then
         call XWRITE(' Image not loaded', 10)
         Status = -1
         return
      endif
      call gheads(mapid, 'MAPTYPE', maptype, 0, status)
      if ( maptype.ne.'I' ) then
         call xwarn('Non-integer map, rounding in excess calculation',5)
      endif
      call gheadd(mapid, 'DATAMIN', datamin, 0, status)
      call gheadd(mapid, 'DATAMAX', datamax, 0, status)
      if ( datamin.lt.0 .or. datamax.lt.0 ) then
         call xwarn('Negative values in map',5)
      endif

      if ( infile.ne.' ' ) then
         call excess_in(infile, status)
         return
      endif

      if ( .not.isdisplay() .and. pex ) then
         call XWRITE(' No display: PLOT_EXCESSES unavailable', 10)
         pex = .FALSE.
      endif

      if ( NBOxes.eq.0 ) then
         call XWARN(' Background not calculated', 10)
         call XWRITE(' Run BACKGROUND command', 10)
c        Don't report as error (just warning)
         return
      endif
c
c  Load background map
c
      call getbgmap(Mapid, BGMap, BGSz, status)
      if ( status.ne.0 ) return
c
c  Source box size
c
      call get_refram(mapid, di, di, zmx, zmy, xcen, ycen, status)
      call get_itel(mapid, itel)

      if ( itel.gt.0 ) then
         if ( ZTElescop(itel).eq.'EXOSAT' .and.
     &        ZINstrume(itel)(1:3).eq.'CMA' ) then
            call get_optax(itel, xcen, ycen, xoff, yoff)
            disss = SQRT((xcen-xoff)**2 + (ycen-yoff)**2)
            iboxx = 9.03E-5*disss*disss - .01603*disss + 6.958 - 1.5
            IF ( iboxx.GT.20 ) iboxx = 20
            IF ( iboxx.LT.8 ) iboxx = 8
         else
            call gmdbi(itel, 'DETBOX', iboxx, 0, status)
         endif
      else
         iboxx = 8
      endif
c
c user has set the box size  (moved up from the end by Nick 6/15/93)
c                            (otherwise scal not set right?)
c
      IF ( boxsize.NE.0 ) iboxx = boxsize
c
c Prior version added one for some unknown reason
c
c     BOXsiz = iboxx/zmx + 1
      BOXsiz = nint(dble(iboxx)/zmx)
c
c always have a box bigger than 1 pixel
c
      IF ( BOXsiz.LE.1 ) BOXsiz = 2
      iboxx = NINT(dble(BOXsiz)*zmx)
      WRITE (ZWRite,99001) iboxx , BOXsiz
      CALL XWRITE(ZWRite,10)
c
c run excess search algorithm
c
      call excess_search(Map, Szx, Szy, Mapid, pex, bright, 
     &                   user_threshold_scal, Status)
      if ( Status.ne.0 ) return

      if ( outfile.ne.' ' ) then
         call excess_out(mapid, outfile, status)
      endif

      RETURN
99001 FORMAT (' Source box size (orig pix):',i5,' (image pix):',i5)
      END
