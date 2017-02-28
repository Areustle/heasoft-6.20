      SUBROUTINE SEARCH(Cmdid, Map, Szx, Szy, Mapid, Status)
      IMPLICIT NONE
c
c  Search for sources
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
      CHARACTER*(MAX_FILELEN) filedet, fitsdet, infile, outfile
      INTEGER*4 color, lwidth, lstyle
      REAL*4 csize, snr_thr, prob_lim
      character(10) font
      LOGICAL nolabel, detlab, isloaded, isdisplay
      character(1) maptype
      integer argc, equinox
      real*8 datamin, datamax

      color = 16
      call get_color(color)
      lwidth = 1
      lstyle = 1
      csize = 0.7
      font = ' '
      
      snr_thr = 0.0
      prob_lim = 0.0
      nolabel = .FALSE.
      filedet = ' '
      fitsdet = ' '
      infile = ' '
      outfile = ' '
c
      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARR(Cmdid,'SNR_THRESHOLD',snr_thr,Status)
      CALL GPARR(Cmdid,'PROB_LIMIT',prob_lim,Status)
      CALL GPARL(Cmdid,'NOLABEL',nolabel,Status)
      CALL GPARS(Cmdid,'FILEDET',filedet,Status)
      CALL GPARS(Cmdid,'FITSDET',fitsdet,Status)
      CALL GPARS(Cmdid,'INFILE',infile,Status)
      CALL GPARS(Cmdid,'OUTFILE',outfile,Status)
      CALL GPARI(Cmdid,'COLOR',color,Status)
      CALL GPARI(Cmdid,'LWIDTH',lwidth,Status)
      CALL GPARI(Cmdid,'LSTYLE',lstyle,Status)
      CALL GPARR(Cmdid,'CSIZE',csize,Status)
      CALL GPARS(Cmdid,'FONT',font,Status)
      if ( status.ne.0 ) return

c
      call qustrip(infile)
      call qustrip(outfile)
      call qustrip(filedet)
      call qustrip(fitsdet)
      call xtend(fitsdet, 'fits')

      if ( .not.isloaded(mapid) ) then
         call XWRITE(' Image not loaded', 10)
         Status = -1
         return
      endif
      call gheads(mapid, 'MAPTYPE', maptype, 0, status)
      if ( maptype.ne.'I' ) then
         call xwarn('Non-integer map, rounding in search calculation',5)
      endif
      call gheadd(mapid, 'DATAMIN', datamin, 0, status)
      call gheadd(mapid, 'DATAMAX', datamax, 0, status)
      if ( datamin.lt.0 .or. datamax.lt.0 ) then
         call xwarn('Negative values in map',5)
      endif

c  Get Ximage equinox
      call tclresi("set default(equinox)", equinox, status)

      if ( infile.ne.' ' ) then
         call search_in(infile, status)
         return
      endif

      if ( isdisplay() ) then
         call PGSAVE
         call text_pgstate(color, csize, lwidth, font)
         call line_pgstate(color, lwidth, lstyle)
         call pgbbuf
      endif

      if ( NBOxes.eq.0 ) then
         call XWARN(' Background not calculated', 10)
         call XWRITE(' Run BACKGROUND command', 10)
c        Don't report as error (just warning)
         return
      endif

      if ( NUMexs.eq.0 ) then
         call XWARN(' No excesses calculated', 10)
         call XWRITE(' Run EXCESS command', 10)
c        Don't report as error (just warning)
         return
      endif
c
c  Load background map
c
      call getbgmap(Mapid, BGMap, BGSz, status)
      if ( status.ne.0 ) return
c
c  Run detection algorithm
c
      detlab = .NOT.nolabel
      call detect_search(Map, Szx, Szy, Mapid, snr_thr, prob_lim,
     &                   detlab, Status)
      IF ( Status.NE.0 ) RETURN
c
      call detect_out(Mapid, equinox, filedet, fitsdet, status)

      if ( outfile.ne.' ' ) then
         call search_out(mapid, outfile, status)
      endif

      if ( isdisplay() ) then
         call PGUNSA
         call pgebuf
      endif
c
      RETURN
      END
