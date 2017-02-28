      SUBROUTINE VPLABEL(Cmdid,Vpset,Status)
      implicit none
c
c  Write labels on the image in relation to viewport
c
c  I  Cmdid  (i)  Command id
c  I  Vpset  (r)  Viewport to plot scale in relation to
c  O  Status (i)  Error flag (0=OK)
c
      INCLUDE '../include/io.inc'

      INTEGER Cmdid, Status
      REAL*4 Vpset(4)
c
c  Local variables
c
      character(128) text
      REAL*4 csize, vpsave(4)
      INTEGER*4 color , lwidth
      INTEGER*4 argc, i
      LOGICAL isdisplay

      character(10) just, font
      character(1) side
      REAL*4 position, margin, rjust
      LOGICAL bottom, toplog, left, right

      INTEGER njust
      PARAMETER (njust=3)
      character(6) justopts(njust)
      REAL*4 justvals(njust)
      LOGICAL curvp, lfrt
c
      DATA justopts /'LEFT', 'CENTER', 'RIGHT'/
      DATA justvals /0.0, 0.5, 1.0/
c
      color = 1
      csize = 1.0
      lwidth = 1
      font = ' '
      text = ' '
c
      curvp = .false.
      toplog = .false.
      bottom = .false.
      left = .false.
      right = .false.

      side = 'T'
      position = -999.0
      margin = 1.0
      just = 'CENTER'

      status = 0

      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARS(Cmdid,'TEXT',text,Status)
      CALL GPARL(Cmdid,'CURVP',curvp,Status)
      CALL GPARI(Cmdid,'COLOR',color,Status)
      CALL GPARR(Cmdid,'CSIZE',csize,Status)
      CALL GPARI(Cmdid,'LWIDTH',lwidth,Status)
      CALL GPARS(Cmdid,'FONT',font,Status)
      CALL GPARS(Cmdid,'JUST',just,Status)
      CALL GPARL(Cmdid,'TOP',toplog,Status)
      CALL GPARL(Cmdid,'BOTTOM',bottom,Status)
      CALL GPARL(Cmdid,'LEFT',left,Status)
      CALL GPARL(Cmdid,'RIGHT',right,Status)
      CALL GPARR(Cmdid,'POSITION',position,Status)
      CALL GPARR(Cmdid,'MARGIN',margin,Status)
      if ( status.ne.0 ) return
      
      if (.not.isdisplay()) then
         call XWARN ('No display',5)
         status = -1
         return
      endif
      if ( text.eq.' ' ) then
         call xwrite(' No label text entered', 10)
         status = -1
         return
      endif
c
c Check if display/left, /right in use
c
      call tclresl('info exists pgtk::lfrt', lfrt, status)
      if ( lfrt ) curvp = .TRUE.
c
c Save and set viewport
c
      if ( .not.curvp .and. Vpset(1).ge.0.0 ) then
         call pgqvp(0,vpsave(1),vpsave(2),vpsave(3),vpsave(4))
         call pgsvp(Vpset(1),Vpset(2),Vpset(3),Vpset(4))
      endif
c
c Evaluate qualifiers
c
c  Text state
c
      call PGSAVE
      call text_pgstate(color, csize, lwidth, font)
c
c  Viewport position
c
      if ( toplog ) then
         side = 'T'
      elseif ( bottom ) then
         side = 'B'
      elseif ( left ) then
         side = 'L'
      elseif ( right ) then
         side = 'R'
      endif
c
c  Justification
c
      call matchopts (just, justopts, njust, i, status)
      if ( status.eq.0 ) then
         rjust = justvals(i)
      else
         call XWRITE (' Invalid justification: Defaulting to LEFT...'
     &                , 10)
         rjust = 0.0
         status = 0
      endif
c
c  If no POSITION set, base it on justification
c
      if ( position.le.-999.0 ) position = rjust
c
c  Adjust margin
c
      if ( side.eq.'B' .or. side.eq.'R' ) margin = margin + 0.7
c
c  plot label
c
      CALL PGMTXT(side,margin,position,rjust,text)
c
c  Restore
c
      if ( .not.curvp .and. Vpset(1).ge.0.0 ) then
         call pgsvp(vpsave(1),vpsave(2),vpsave(3),vpsave(4))
      endif
      call PGUNSA
c
c  Journal command
c
      call jrncmd(Cmdid, status)
      
      status = 0
      RETURN
      END
