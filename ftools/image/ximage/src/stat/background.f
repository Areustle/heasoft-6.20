      subroutine background(Cmdid, Map, Szx, Szy, Mapid, Status)
      implicit none
c
c  Calculates background, writes background file
c
c  I  Map      (i)  Image map
c  I  Szx/y    (i)  Size of map
c  O  Status   (i)  Error flag (0 = OK)
c
      integer*4 Cmdid, Szx, Szy, Status
      real*4 Map(Szx,Szy)
      character*(*) Mapid

      include '../include/maxvals.inc'
      include 'backgd.inc'
c
c  Local variables
c
      INTEGER*4 argc, ipd, ibbace, in1, in2, in, LENACT
      REAL*4 flatval
      LOGICAL plot_pd , optimize, isloaded, isdisplay, reset
      CHARACTER*(MAX_FILELEN) filepd, infile, outfile
      character(20) flatstr
      character(1) maptype
      REAL*8 datamin, datamax, dd

      logical readonly, global
      real*4 imgpix, detpix, arcmin, detpixsec, arcminsec

      reset = .FALSE.
      plot_pd = .FALSE.
      optimize = .FALSE.
      DRAw_boxes = .FALSE.
      filepd = ' '
      infile = ' '
      outfile = ' '
      ibbace = 0
      ipd = 8
      flatstr = ' '
      SIGmult = DEFsigm
      BARylim = DEFbarl
      BXHprob = DEFhprob
c
      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARL(Cmdid,'DRAW_REJ_BACK_BOXES',DRAw_boxes,Status)
      CALL GPARI(Cmdid,'BOX_SIZE',ibbace,Status)
      CALL GPARL(Cmdid,'OPTIMIZE',optimize,Status)
      CALL GPARS(Cmdid,'INFILE',infile,Status)
      CALL GPARS(Cmdid,'OUTFILE',outfile,Status)
      CALL GPARS(Cmdid,'FLAT_VALUE',flatstr,Status)
      CALL GPARR(Cmdid,'SIGMULT',SIGmult,Status)
      CALL GPARR(Cmdid,'BARYLIM',BARylim,Status)
      CALL GPARR(Cmdid,'BXHPROB',BXHprob,Status)
      CALL GPARL(Cmdid,'RESET',reset,Status)
      CALL GPARI(Cmdid,'NPD',ipd,Status)
      CALL GPARS(Cmdid,'FILE_PD',filepd,Status)
      CALL GPARL(Cmdid,'PLOT_PD',plot_pd,Status)
      if ( status.ne.0 ) return

      call qustrip(infile)
      call qustrip(outfile)
      call qustrip(filepd)

      if ( .not.isloaded(mapid) ) then
         call XWRITE(' Image not loaded', 5)
         status = -1
         return
      endif
      call gheads(mapid, 'MAPTYPE', maptype, 0, status)
      if ( maptype.ne.'I' ) then
         call xwarn('Non-integer map, rounding in bg calculation', 5)
      endif
      call gheadd(mapid, 'DATAMIN', datamin, 0, status)
      call gheadd(mapid, 'DATAMAX', datamax, 0, status)
      if ( datamin.lt.0 .or. datamax.lt.0 ) then
         call xwarn('Negative values in map',5)
      endif

      if ( reset ) goto 500

      if ( infile.ne.' ' ) then
         call backgd_in(infile, status)
         call prback(mapid, status)
         return
      endif

      if ( .not.isdisplay() .and. DRAw_boxes ) then
         call XWRITE(' No display: DRAW_REJ_BACK_BOXES unavailable', 10)
         DRAw_boxes = .FALSE.
      endif
c
      if ( filepd.ne.' ' ) then
         CALL DIRPOS(filepd,in1,in2)
         IF (in2.EQ.0 ) THEN
            in = INDEX(filepd,'.')
         ELSE
            in = INDEX(filepd(in2:),'.')
         ENDIF
         IF ( in.EQ.0 ) then
            filepd = filepd(:LENACT(filepd))//'.qdp'
         ENDIF
      else
         if ( plot_pd ) filepd = 'pd.qdp'
      endif

      call getbgmap(Mapid, BGMap, BGSz, status)
      if ( status.ne.0 ) goto 500

      if ( flatstr.eq.' ' ) then
         call do_back(Map, Szx, Szy, Mapid, optimize, ibbace, status)
      else
         call strnum(flatstr, 4, dd, status)
         flatval = dd
         call flatback(mapid, flatval, ibbace, status)
      endif
      if ( status.ne.0 ) goto 500

      if ( filepd.ne.' ' ) then
         CALL WRBACK(mapid, filepd, ipd, plot_pd, status)
      endif

      if ( outfile.ne.' ' ) then
         call backgd_out(mapid, outfile, status)
      endif

      STAtid = Mapid
c
c  Export background values to Tcl
c
      readonly = .FALSE.
      global = .TRUE.
      call backvals(mapid, imgpix, detpix, arcmin, detpixsec, arcminsec, 
     &              status)
      call tclvars('background(mapid)',mapid,readonly,global,status)
      call tclvarr('background(imgpix)',imgpix,readonly,global,status)
      call tclvarr('background(detpix)',detpix,readonly,global,status)
      call tclvarr('background(arcmin)',arcmin,readonly,global,status)
      call tclvarr('background(detpixsec)',detpixsec,readonly,global,
     &             status)
      call tclvarr('background(arcminsec)',arcminsec,readonly,global,
     &             status)

      status = 0
      RETURN
c
c  Zero out background and return
c
  500 continue
      STAtid = ' '
      NBOxes = 0
      call tclrun('catch {unset ::background}', status)
      status = 0
      RETURN
      END
