      SUBROUTINE PG_START (Ixmin, Ixmax, Iymin, Iymax,
     &                     Trf, Overlay, Keepvp, Vport, Refresh, Status)
      IMPLICIT NONE
c
c  I  ixmin   (i) Minimum window bound in x
c  I  ixmax   (i) Maximum window bound in x
c  I  iymin   (i) Minimum window bound in y
c  I  iymax   (i) Maximum window bound in y
c  O  trf     (r) Transformation matrix
c  I  overlay (l) Whether to continue on current page
c  I  keepvp  (l) Whether to keep existing viewport
c  I  vport   (r) Viewport for image
c  I  refresh (l) Whether to buffer for refreshing
c  O  status  (i) Error flag
c
      INTEGER*4   Ixmin, Ixmax, Iymin, Iymax, Status
      REAL*4 Trf(*), Vport(*)
      LOGICAL Overlay, Keepvp, Refresh

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/colordef.inc'
      INCLUDE '../include/io.inc'

      INTEGER PGOPEN, LENACT
c
c  Local variables
c
      CHARACTER CTMP*24
      character(100) ds
      INTEGER slen, di, opstat
      character(8) devkey(1)
      character(20) opntype, reqtype
      character*(MAX_FILELEN) opnfile, reqfile
      integer iot, iof, irt, irf

      logical tkexists, pagedev, readonly, global
      integer optnum
      parameter (optnum=3)
      character(8) opttyp(optnum)
      character*(MAX_FILELEN) device
      data opttyp/'XTK','XWINDOW','XSERVE'/

      Status = 0
c
c  Get plot device
c
      call tclress('set default(device)', device, MAX_FILELEN, status)
c
c  Open device
c
      if ( .not.Overlay ) then

C Come from for error correction
100      CONTINUE
         IF (device.eq.' ' .or. device(:lenact(device)).EQ.'?') THEN
            CALL PGLDEV
            CTMP = ' Input device:'
            device = ' '
            CALL XCREAD(CTMP,device,status)
            IF ( status.ne.0 ) RETURN
c
c          Record device setting as default
c
            readonly = .FALSE.
            global = .TRUE.
            call tclvars('default(device)', device, readonly, global,
     &                   status)
            GOTO 100
         END IF
c
c  Find file and type of open and requested devices
c
         opntype = ' '
         opnfile = ' '
         reqtype = ' '
         reqfile = ' '
c  Initialize to avoid warning
         irt = 0
         irf = 0
c  --
         call pgqinf('TYPE', opntype, iot)
         call pgqinf('FILE', opnfile, iof)

c        PGPLOT returns null string when there is no file,
c        while comparison expects blank string
         call rmvnul(opnfile)

         di = index(device, '/')
         if ( di.gt.0 ) then
            reqtype = device(di+1:LENACT(device))
            call upc(reqtype)
            irt = LENACT(reqtype)
            if ( di.gt.1 ) then
               irf = di-1
               reqfile = device(1:irf)
            else
               reqfile = ' '
               irf = 1
c              Is 1 not 0 because that is what pgqinf returns
c                for opnfile
            endif
         endif
c
c  Set /PS and /GIF to avoid paging. PS adds to existing file and
c  GIF appends _2, _3, etc.  User does not expect this behavior.
c
         pagedev = .TRUE.
         if ( reqtype(irt-1:irt).eq.'PS' .and. reqfile.eq.' ' ) then
            reqfile = 'pgplot.ps'
            irf = LENACT(reqfile)
            pagedev = .FALSE.
         elseif ( reqtype(1:irt).eq.'GIF' .and. reqfile.eq.' ' ) then
            reqfile = 'pgplot.gif'
            irf = LENACT(reqfile)
            pagedev = .FALSE.
         elseif ( reqtype(1:irt).eq.'XTK' ) then
c
c  If xtk device not up, start it
c
            tkexists = .FALSE.
            call tclresi('pgtk::tkexists', tkexists, status)
            if ( status.ne.0 ) tkexists = .FALSE.
            if ( .not.tkexists ) then
               call tclrun(
     &            'pgtk::setup $xtkwid $xtkhgt',status)
               if ( status.ne.0 ) return
            endif
            call tclrun('tk::wm deiconify .ximage', status)
c
c  If Device is /xtk, translate to $mainpg/xtk
c
            if ( reqfile.eq.' ' ) then
               call tclress('subst $mainpg/xtk', device,
     &                      MAX_FILELEN, status)
               di = index(device, '/')
               irf = di-1
               reqfile = device(1:irf)
            endif
         endif

         call pgqinf('STATE', ds, slen)
         if ( ds(1:slen).eq.'OPEN' ) then
            call pgqid(opstat)
c
c  If widget-name of open device is same as requested device,
c  advance page, otherwise open a new device
c
            devkey(1) = opntype(1:iot)
            call matchkey(reqtype(1:irt), devkey, 1, di, status)
c
            if ( status.eq.0 .and. pagedev .and.
     &           opnfile(1:iof).eq.reqfile(1:irf) ) then
c
c  If requested type is same as open type, use buffering to improve
c  plotting performance.  This requires the use of PGERAS rather
c  than PGPAGE so PGBBUF/PGEBUF may be used.
c
               call matchkey(reqtype(1:irt), opttyp, optnum, di, status)
               if ( status.eq.0 ) then
                  if ( Refresh ) call pgbbuf
                  call pgeras
               else
                  call pgpage
                  if ( Refresh ) call pgbbuf
                  status = 0
               endif
            else
               status = 0
               if ( .not.pagedev ) call pgclos
               opstat = pgopen(device)
               if ( Refresh ) call pgbbuf
            endif
         else
            opstat = pgopen(device)
            if ( Refresh ) call pgbbuf
         endif
c
         if ( opstat.gt.0 ) then
c
c    Set up to confirm PGPAGE each time if XIMAGE_PGASK environmental
c    variable is true
c
            call getenv('XIMAGE_PGASK',ds)
            call UPC(ds)
            if ( ds.eq.'TRUE' .or. ds.eq.'1' .or. ds.eq.'YES') then
               call PGASK(.TRUE.)
            else
               call PGASK(.FALSE.)
            endif
         else
            write(ZWRite,'(2a)') 'Unable to open device: ',
     &                        device(:LENACT(device))
            call XERROR(ZWRite,5)
            status = -1
            return
         endif
      endif
c
c  Set the viewport unless plotting on top of something that
c  needs to keep the viewport as is
c
      if ( .not.Keepvp ) then
         call PGVPORT(Vport(1),Vport(2),Vport(3),Vport(4))
c
c  Set frame min and max
c
         call PGWNAD(real(Ixmin)-0.5,real(Ixmax)+0.5,
     &               real(Iymin)-0.5,real(Iymax)+0.5)
      endif
c
c  Calculate transformation matrix
c
      Trf(1)=0.
      Trf(2)=1.
      Trf(3)=0.
      Trf(4)=0.
      Trf(5)=0.
      Trf(6)=1.
c
c  Overwrite default colors (0-15), if set
c
      call set_defcols(status)
c
c  Set default colour map 
c
      call copy_coltab(CTAB_DEFAULT)

      return 
      end 
