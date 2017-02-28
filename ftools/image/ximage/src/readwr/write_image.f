      subroutine write_image(Cmdid, Mapid, Status)
      implicit none
c
c  Write an image in fits or ascii or exosat(i*2) format
c
c  I  cmdid    (i)  Command id
c  I  mapid    (s)  Map id string
c  O  status   (i)  Error flag (0=OK)
c
      INTEGER*4 Cmdid, Status
      CHARACTER*(*) Mapid

      INCLUDE '../include/dynmem.inc'
      INCLUDE '../include/io.inc'
      INCLUDE '../include/maxvals.inc'
c
c Local variables
c
      INTEGER*4 LENACT
      INTEGER argc
      CHARACTER*(MAX_FILELEN) filename, template
      LOGICAL exosat, fits, ascii, exp_map, disp, there, nonull, nowcs
      LOGICAL isdisplay, isloaded
      CHARACTER*(MAX_IDSTR) wrmapid, wcsid
      character(1) maptype
      INTEGER*4 p_i2buf, mapptr, szx, szy
      INTEGER*4 sigdig
      character(3) ext
c
c set value start value for qualifiers
c
      wrmapid = Mapid
      sigdig = 0
      exosat = .FALSE.
      fits = .FALSE.
      ascii = .FALSE.
      exp_map = .FALSE.
      disp = .FALSE.
      filename = 'output'
      template = ' '
      nonull = .FALSE.
      nowcs = .FALSE.

      Status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARS(Cmdid,'MAPID',wrmapid,Status)
      CALL GPARL(Cmdid,'EXOSAT',exosat,Status)
      CALL GPARL(Cmdid,'FITS',fits,Status)
      CALL GPARL(Cmdid,'ASCII',ascii,Status)
      CALL GPARL(Cmdid,'EXPOSURE_MAP',exp_map,Status)
      CALL GPARL(Cmdid,'DISPLAY_MAP',disp,Status)
      CALL GPARS(Cmdid,'FILE',filename,Status)
      CALL GPARS(Cmdid,'TEMPLATE',template,Status)
      CALL GPARI(Cmdid,'SIGDIG',sigdig,Status)
      CALL GPARL(Cmdid,'NONULL',nonull,Status)
      CALL GPARL(Cmdid,'NOWCS',nowcs,Status)
      if ( status.ne.0 ) return

      IF ( .NOT.exosat .AND. .NOT.fits .AND. .NOT.ascii ) THEN
         fits = .true.  
         CALL XWRITE(' Writing a FITS format image ',10)
      ENDIF
c
      if ( fits )    ext = 'img'
      if ( ascii )   ext = 'qdp'
      if ( exosat )  ext = 'exo'

      call qustrip(filename)
      call xtend(filename,ext)
c
c write filename output on the screen
      INQUIRE(file=filename,exist=there) 
      IF(there) THEN
        ZWRite =' File : '//filename(:LENACT(filename))
     &            //' already exists'
        call xwrite(ZWRite,5)
        ZWRite =' Remove '//filename(:LENACT(filename))
     &            //' or use a different filename'
        call xwrite(ZWRite,5)
        return
      ENDIF 
c
      call lkupfile(template, 'hdr', 'header template', status)
      if ( status.ne.0 ) return

      if ( exp_map ) then

         call gheads(Mapid, 'EXMAPID', wrmapid, 0, status)
         if ( wrmapid.eq.' ' ) then
            call xwrite(' No exposure map to write', 5)
            status = -1
         endif
         if ( status.ne.0 ) return

      elseif ( disp ) then

         if ( .not.isdisplay() ) then
            call xwrite(' No display', 10)
            status = -1
            return
         endif
         wrmapid = 'DIS'

      endif
      if ( .not.isloaded(wrmapid) ) then
         call xwrite(' Map not loaded', 10)
         status = -1
         return
      endif
      call gheads(wrmapid, 'MAPTYPE', maptype, 0, status)
      call gheadi(wrmapid, 'MAPPTR', mapptr, 0, status)
      call gheadi(wrmapid, 'SZX', szx, 0, status)
      call gheadi(wrmapid, 'SZY', szy, 0, status)
c
c  Blank wcsid, if nowcs specified
c
      if ( nowcs ) then
         call gheads(wrmapid, 'WCSID', wcsid, 0, status)
         call gheads(wrmapid, 'WCSID', ' ', 1, status)
      endif

      IF (fits) THEN
    
         call wr_fits(filename,template,memr(mapptr),szx,szy,
     &                wrmapid,maptype,nonull,status)

      ELSEIF ( exosat ) THEN

         call i2alloc(1,szx,1,p_i2buf,status)
         if ( status.eq.0 ) then
            call wr_exo(filename,memr(mapptr),szx,szy,wrmapid,
     &                  maptype,mems(p_i2buf),status)
            call i2alloc(0,szx,1,p_i2buf,status)
         else
            call XWRITE(' Failed allocation for exosat image', 5)
         endif

      ELSEIF ( ascii )THEN

         call wr_asc(filename,template,memr(mapptr),szx,szy,wrmapid,
     &               maptype,sigdig,nonull,status)

      ENDIF
c
c  Restore wcsid
c
      if ( nowcs ) call gheads(wrmapid, 'WCSID', wcsid, 1, status)

      RETURN
      END
