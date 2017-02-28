      SUBROUTINE COLORS(Cmdid, Vpset, Status)
      IMPLICIT NONE
c
c  Plot colors with color indexes
c
c  I  Cmdid        (i)  Command id
c  I  Vpset        (r)  Viewport to plot scale in relation to
c  O  Status       (i)  Error flag (0=OK)
c
      INTEGER*4 Cmdid, Status
      REAL*4 Vpset(4)

      INCLUDE '../include/colordef.inc'
      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
c
c  Local variables
c
      INTEGER*4 nxsub , nysub
      INTEGER*4 i , j, step , LENACT
      INTEGER*4 loopnum
      INTEGER*4 mm , pp , nc , minci, maxci, ici, numcols
      REAL*4 ci(MAX_NUMLEVS)
      LOGICAL*4 isdisplay

      REAL*4 inten
      REAL*4 xtick , ytick 
      REAL*4 min , max , xlef , xrig
      REAL*4 xpos , ypos
      REAL*4 sxvp1 , sxvp2 , syvp1 , syvp2
      REAL*4 xvp1 , xvp2 , yvp1 , yvp2
      REAL*4 xw1 , xw2 , yw1 , yw2
      REAL*4 xch, ych, xpch, ypch
 
      character(16) xopt , yopt
      character(30) nlab
 
      INTEGER argc, ndivs, color, lwidth
      REAL*4 size, margin, thick, txtspace
      LOGICAL plot, clear, reset
      character(10) font
      integer*4 setcol, swapcol, copycol, irgb, nullin
      character(30) nmstr, rgbstr
      real*4 rtmp, gtmp, btmp
      real*8 dd

      plot = .FALSE.
      clear = .FALSE.
      reset = .FALSE.
      setcol = -1
      swapcol = -1
      copycol = -1
      rgbstr = ' ' 
      nmstr = ' '
      nullin = -1

c  Initialize to avoid warning
      rtmp = 0.
      gtmp = 0.
      btmp = 0.
c  --
      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARL(Cmdid,'PLOT',plot,status)
      CALL GPARL(Cmdid,'CLEAR',clear,status)
      CALL GPARI(Cmdid,'SETCOLOR',setcol,status)
      CALL GPARI(Cmdid,'COPYCOLOR',copycol,status)
      CALL GPARI(Cmdid,'SWAPCOLOR',swapcol,status)
      CALL GPARS(Cmdid,'NAMECOLOR',nmstr,status)
      CALL GPARS(Cmdid,'RGBCOLOR',rgbstr,status)
      CALL GPARL(Cmdid,'RESET',reset,status)
      CALL GPARI(Cmdid,'NULL',nullin,status)
      if ( status.ne.0 ) return
c
c  Color manipulation
c
      if ( reset ) then
         do i = 0, 15
            PGRvals(i+1) = -1.0
            PGGvals(i+1) = -1.0
            PGBvals(i+1) = -1.0
         enddo
         call xwrite(' PGPLOT standard colors will be restored in'//
     &               ' next image opened', 10)
         return
      endif

      if ( setcol.ge.0 ) then
         if (.not.(copycol.ge.0 .or. swapcol.ge.0 .or. nmstr.ne.' '
     &             .or. rgbstr.ne.' ') ) then
            call xwrite(' SETCOLOR requires COPY, SWAP, NAME, or '//
     &                  'RGBCOLOR', 10)
            return
         elseif ( setcol.gt.15 ) then
            call xwrite(' Only color indices 0-15 may be set', 10)
            return
         endif
c  Swap
         if ( swapcol.ge.0 ) then
            if ( swapcol.gt.15 ) then
               call xwrite(' Only color indices 0-15 may be swapped',10)
               return
            endif
            if ( .not.isdisplay() ) then
               call xwrite(' SWAPCOLOR requires open device', 10)
               return
            endif
            call pgqcr(setcol, PGRvals(swapcol+1), PGGvals(swapcol+1), 
     &                         PGBvals(swapcol+1))
            call pgqcr(swapcol, PGRvals(setcol+1), PGGvals(setcol+1), 
     &                          PGBvals(setcol+1))
c  Copy
         elseif ( copycol.gt.0 ) then
            if ( .not.isdisplay() ) then
               call xwrite(' COPYCOLOR requires open device', 10)
               return
            endif
            call pgqcr(copycol, PGRvals(setcol+1), PGGvals(setcol+1), 
     &                          PGBvals(setcol+1))
c  R,G,B format
         elseif ( rgbstr.ne.' ' ) then
            status = 0
            irgb = index(rgbstr,',')
            if ( irgb.gt.0 ) then
               call strnum(rgbstr(1:irgb-1),4,dd,status)
               rtmp = dd
               rgbstr = rgbstr(irgb+1:LENACT(rgbstr))
               irgb = index(rgbstr,',')
               if ( irgb.gt.0 .and. status.eq.0 ) then
                  call strnum(rgbstr(1:irgb-1),4,dd,status)
                  gtmp = dd
                  rgbstr = rgbstr(irgb+1:LENACT(rgbstr))
                  call strnum(rgbstr,4,dd,status)
                  btmp = dd
               else 
                 status = -1
               endif
               if ( status.eq.0 ) then
                  PGRvals(setcol+1) = rtmp
                  PGGvals(setcol+1) = gtmp
                  PGBvals(setcol+1) = btmp
               else
                  status = -1
               endif
            else
               status = -1
            endif
            if ( status.ne.0 ) then
               call xwrite(' Failed to parse R,G,B format', 10)
               return
            endif
c  Color name
         elseif ( nmstr.ne.' ' ) then
            call upc(nmstr)
            if ( nmstr.eq.'BLACK' ) then
               PGRvals(setcol+1) = 0.
               PGGvals(setcol+1) = 0.
               PGBvals(setcol+1) = 0.
            elseif ( nmstr.eq.'WHITE' ) then
               PGRvals(setcol+1) = 1.
               PGGvals(setcol+1) = 1.
               PGBvals(setcol+1) = 1.
            else
               if ( isdisplay() ) then
                  call pgscrn(setcol, nmstr, status)
                  call pgqcr(setcol, PGRvals(setcol+1), 
     &                       PGGvals(setcol+1), PGBvals(setcol+1))
                  if ( status.ne.0 ) then
                     call xwrite(' Cannot find rgb file', 10)
                     call xwrite(' Only black and white supported', 10)
                     return
                  endif
               else
                  call xwrite(' PGPLOT color name facility requires'//
     &                        ' open display', 10)
                  call xwrite(' Only black and white supported', 10)
                  return
               endif
            endif
         endif
         if ( isdisplay() ) then
            call set_defcols(status)
            call pgupdt
         endif
         return
      elseif ( nullin.ge.0 ) then
c
c  Set color index to plot null pixels with
c
         NULlcol = nullin
         write(ZWRite,'(a,i4)') ' Null pixel color index: ', NULlcol
         call xwrite(ZWRite, 10)
         return
      else
         if (.not.( plot.or.clear )) then
            call xwrite(' Static colors (Name or RGB values):', 10)
            do i = 0, 15
               if ( PGRvals(i+1).lt.0. ) then
                  write(ZWRite,99001) i, PGDefnam(i+1)
               else
                  write(ZWRite,99002) i, PGRvals(i+1), PGGvals(i+1), 
     &                                   PGBvals(i+1)
               endif
               call xwrite(ZWRite, 10)
            enddo
            return
         endif
      endif
c
c  Color legend
c
      if ( plot.and.clear ) then
         call xwrite(' Use either PLOT or CLEAR', 10)
         return
      endif
c
      if ( .not.isdisplay() ) then
         call XWARN ('No display',5)
         return
      endif

      size = -1.0
      font = ' '
      color = 1
      lwidth = 1
      margin = 1.
      thick = 0.7
      txtspace = 0.5
      ndivs = 4
c
c get color index values
c
      call PGQCIR(minci,maxci)
c
c  Process qualifiers
c
c    Text attributes
c
      call PGSAVE
      call text_pgstate(color, size, lwidth, font)

c inquire viewport and world coordinates of the image plot
 
      CALL PGQVP(0,sxvp1,sxvp2,syvp1,syvp2)
      if ( Vpset(1).lt.0.0 ) then
         CALL PGQVP(0,xvp1,xvp2,yvp1,yvp2)
      else
         xvp1 = Vpset(1)
         xvp2 = Vpset(2)
         yvp1 = Vpset(3)
         yvp2 = Vpset(4)
      endif
      CALL PGQWIN(xw1,xw2,yw1,yw2)
c
c definitions for the scale plot
c
      min = float(minci) - 0.5
      max = float(maxci) + 0.5

      if ( clear ) then
         thick = 4.7 + thick
         CALL PGSVP(xvp2+margin*xch,xvp2+thick*xch,yvp1,yvp2)
         CALL PGSWIN(0.,1.,min,max)
         CALL PGSCI(0)
         CALL PGRECT(0.,1.,min,max)
         goto 700
      endif
      
c     if ( maxci-minci.ne.No_of_levels ) then
c        loopnum = 1
c        minci = 0
c        maxci = 15
c        margin = margin + 2.5
c        ndivs = 5
c     else
         loopnum = 2
c     endif

      do j = 1, loopnum

      numcols = maxci - minci + 1
      do i = 1, numcols
         ci(i) = i + minci - 1
      enddo
c
c Setup viewport/window spacing for the colors plot
c
      call PGQCS(0, xch, ych)
      CALL PGSVP(xvp2+margin*xch,xvp2+(margin+thick)*xch,yvp1,yvp2)
      CALL PGSWIN(0.,1.,min,max)
c
c plot colour scale
c
      xrig = min
 
      DO 300 i = minci, maxci
         xlef = xrig
         xrig = float(i) + 0.5
         CALL PGSCI(i)
         CALL PGRECT(0.,1.,xlef,xrig)
 300  CONTINUE
 
c plot frame
 
      xopt = 'ABC'
      xtick = 1.
      nxsub = 1
 
      yopt = 'ABCT'
      step = (max-min-1.)/ndivs
      ytick = (max - min)*10.
      nysub = 1
      CALL PGSCI(color)
      CALL PGBOX(xopt,xtick,nxsub,yopt,ytick,nysub)
 
c write numeric labels
 
      call PGQCS(4, xpch, ypch)
 
      ypos = min
      xpos = 1.0 + (0.7 + txtspace)*xpch

      ici = 1

      DO 400 i = 1 , ndivs + 1
         inten = ci(ici)
         CALL DEC_TO_EXP(inten,mm,pp,2)
         CALL PGNUMB(mm,pp,0,nlab,nc)
         ypos = inten
         CALL PGPTXT(xpos,ypos,90.,0.5,nlab(1:nc))
         ici = ici + step
 400  CONTINUE

      minci = 0
      maxci = 15
      min = float(minci) - 0.5
      max = float(maxci) + 0.5
      margin = margin + 2.5
      ndivs = 5

      enddo
c
c  Restore properties
c
 700  CONTINUE
      CALL PGSVP(sxvp1,sxvp2,syvp1,syvp2)
      CALL PGSWIN(xw1,xw2,yw1,yw2)
      call PGUNSA
 
      RETURN
99001 format(1x,i2,' = ',a)
99002 format(1x,i2,' = ',f5.3,',',f5.3,',',f5.3)
      END
