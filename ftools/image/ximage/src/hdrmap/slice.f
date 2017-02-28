      subroutine slice(Cmdid, Map, Szx, Szy, Mapid, Status)
      implicit none
c
c  Slice an image or plot over an image a file
c
c  I  cmdid   (i)  Command id
c  I  map     (r)  Display map
c  I  szx/y   (i)  Size of image map
c  I  mapid   (s)  Map id string
c  O  status  (i)  Error flag (0=OK)
c
      integer Cmdid, Szx, Szy, Status
      real*4 Map(Szx,Szy)
      character*(*) Mapid

      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/io.inc'
      INCLUDE '../include/dynmem.inc'
c
c  Local variables
c
      LOGICAL isdisplay
c
c variables associated with qualifiers or input parameters
c
      logical x_slice, y_slice, log, previous_frame, nooverlay, plot
      logical spectrogram, mean, connect
      integer*4 color, lwidth, lstyle, bordcol, symbol
      character(20) startstr, endstr, minstr, maxstr
      character*(MAX_FILELEN) infile, outfile
      real*4 startpix, endpix
      real*8 dd
      logical plotsel

      INTEGER*4 argc, slen
c
c variable associated with routine operation
c
      real*8 xcen, ycen, zmx, zmy
      integer di
      REAL*4 xvp1 , xvp2 , yvp1 , yvp2
      REAL*4 xw1 , xw2 , yw1 , yw2

      integer*4 sdim
      integer*4 p_sum, p_sumtot, p_xy, p_npix
      integer*4 itotal, igood, p_errup, p_errdn
      real*4 average

      logical cursor, there
      integer*4 i, n, LENACT
      real*4 pnts(4)
      character*(MAX_FILELEN) template
      integer*4 i1, i2, oflen
      real*4 svsummin, svsummax
      real*4 summin, summax
      real*4 xpix, ypix, ximg, yimg
      integer*4 istart, iend
      character(1) plotdir, rangedir
      character*(MAX_FILELEN+10) cmd
      real*4 errspec
      character(10) sdir
      integer*4 ncols, nrows

      DATA svsummin/-99.0/, svsummax/-99.0/

      SAVE svsummin, svsummax
c
c initialize variable
c
      infile = ' '
      outfile = ' '
      minstr = ' '
      maxstr = ' '
      spectrogram = .FALSE.
      mean = .FALSE.
      connect = .FALSE.
      plot = .FALSE.
      x_slice = .FALSE.
      y_slice = .FALSE.
      startstr = ' '
      endstr = ' '
      log = .FALSE.
      previous_frame = .FALSE.
      bordcol = 16
      call get_color(bordcol)
      color = bordcol
      lwidth = 2
      lstyle = 1
      symbol = 5
      nooverlay = .FALSE.
      errspec = 0.001
      ncols = 0
      nrows = 0
      plotsel = .TRUE.
      itotal = 0
      average = 0.

      status = 0
      call numcarg(cmdid,argc,status)
      if ( argc.ne.0 ) call wrongargs(cmdid, status)

      CALL GPARL(Cmdid,'XSLICE',x_slice,status)
      CALL GPARL(Cmdid,'YSLICE',y_slice,status)
      CALL GPARS(Cmdid,'START_PIXEL',startstr,status)
      CALL GPARS(Cmdid,'END_PIXEL',endstr,status)
      CALL GPARL(Cmdid,'LOG',log,status)
      CALL GPARL(Cmdid,'PREVIOUS_FRAME',previous_frame,status)
      CALL GPARS(Cmdid,'INFILE',infile,status)
      CALL GPARS(Cmdid,'OUTFILE',outfile,status)
      CALL GPARS(Cmdid,'MINFRAME',minstr,status)
      CALL GPARS(Cmdid,'MAXFRAME',maxstr,status)
      CALL GPARL(Cmdid,'NO_OVERLAY',nooverlay,status)
      CALL GPARL(Cmdid,'PLOT',plot,status)
      CALL GPARI(Cmdid,'COLOR',color,status)
      CALL GPARI(Cmdid,'LWIDTH',lwidth,status)
      CALL GPARI(Cmdid,'LSTYLE',lstyle,status)
      CALL GPARI(Cmdid,'SYMBOL',symbol,status)
      CALL GPARL(Cmdid,'SPECTROGRAM',spectrogram,status)
      CALL GPARL(Cmdid,'MEAN',mean,status)
      CALL GPARL(Cmdid,'CONNECT',connect,status)
      CALL GPARR(Cmdid,'ERROR',errspec,status)
      if ( status.ne.0 ) return
c
      if ( .not.isdisplay() ) then
         call XWARN('No display.',10)
         return
      endif
c
c  Qualifier error checking
c
c    If start pixel or end pixels are not specified then use cursor
c
      if ( startstr.eq.' ' .and. endstr.eq.' ' ) then
         cursor = .true.
      elseif ( startstr.ne.' ' .and. endstr.ne.' ' ) then
         cursor = .false.
      else
         call xwrite(' Both START_PIXEL and END_PIXEL must be specified'
     &               , 10)
         return
      endif

      if ( x_slice .and. y_slice ) then
         call xwrite(' Either X_SLICE or Y_SLICE may be specified', 10)
         return
      elseif ( .not.x_slice .and. .not.y_slice ) then
         x_slice = .TRUE.
      endif

      if ( spectrogram ) then
         if ( y_slice ) then
            call xwrite(' SPECTROGRAM only valid with X_SLICE', 10)
            return
         endif
      else
         if ( mean .or. connect ) then
            call xwrite(' MEAN and CONNECT used only'//
     &                  ' with SPECTROGRAM', 10)
            return
         endif
      endif

      if ( outfile.ne.' ' .and. infile.ne.' ' ) then
         call xwrite(' INFILE and OUTFILE cannot be used'//
     &               ' simultaneously', 10)
         return
      endif
      if ( outfile.ne.' ' ) then
         call qustrip(outfile)
         call xtend(outfile,'cut')
      elseif ( infile.ne.' ' ) then
         call qustrip(infile)
         inquire(file=infile, exist=there)
         if ( .not.there ) call xtend(infile,'cut')
      else
         if ( plot ) outfile = 'slice.cut'
      endif
c
c  Get info from header and viewport
c
      call get_refram(mapid, di, di, zmx, zmy, xcen, ycen, status)
c
c Open display and gets the point and work out all the variables
c to pass down for the calculating the slice
c
      CALL pgqvp(0,xvp1,xvp2,yvp1,yvp2)
      CALL pgqwin(xw1,xw2,yw1,yw2)
c     CALL pgwnad(x_min,x_max,y_min,y_max)

      CALL PGSAVE
      call line_pgstate(bordcol, 2, 1)

      if ( infile.ne.' ' ) then
         spectrogram = .FALSE.
         call txinfo(infile, ncols, nrows, status)
         if ( status.ne.0 ) goto 500
         call txrdskey(infile, 'SLICEDIR', 0, sdir, status)
         if ( status.eq.0 ) then
            call upc(sdir)
            if ( sdir(1:1).eq.'X' ) then
               call xwrite(' Input file sliced in X direction', 10)
               x_slice = .TRUE.
               y_slice = .FALSE.
            elseif ( sdir(1:1).eq.'Y' ) then
               call xwrite(' Input file sliced in Y direction', 10)
               x_slice = .FALSE.
               y_slice = .TRUE.
            else
               call xwrite(' Input file sliced in unknown direction', 
     &                     10)
            endif
         endif
         status = 0
         call txrdkey(infile, 'SLICEMIN', 0, startpix, status)
         call txrdkey(infile, 'SLICEMAX', 0, endpix, status)
         if ( status.ne.0 ) plotsel = .false.
         status = 0
      elseif ( cursor ) then
         if ( x_slice ) then
            call xwrite(' Select Y range', 10)
            call tclreslr("select yrange", pnts, n, 4, status)
            if ( status.ne.0 ) goto 500
            startpix = min(pnts(2),pnts(4))
            endpix = max(pnts(2),pnts(4))
         else
            call xwrite(' Select X range', 10)
            call tclreslr('select xrange', pnts, n, 4, status)
            if ( status.ne.0 ) goto 500
            startpix = min(pnts(1),pnts(3))
            endpix = max(pnts(1),pnts(3))
         endif
      else
         call strnum(startstr, 4, dd, status)
         startpix = dd 
         call strnum(endstr, 4, dd, status)
         endpix = dd 
      endif
c
c plot on the display the section in x or y choosen
c
      if ( plotsel ) then
         CALL PGSLS(4)
         CALL PGSLW(4)
         IF ( x_slice ) THEN
            call pgmove(xw1, startpix)
            call pgdraw(xw2, startpix)
            call pgmove(xw1, endpix)
            call pgdraw(xw2, endpix)
         ELSEIF ( y_slice ) THEN
            call pgmove(startpix, yw1)
            call pgdraw(startpix, yw2)
            call pgmove(endpix, yw1)
            call pgdraw(endpix, yw2)
         ENDIF
      endif
c
c  Allocate arrays
c
      if ( infile.ne.' ' ) then
         sdim = nrows
      elseif ( x_slice ) then
         sdim = Szx
      elseif ( y_slice ) then
         sdim = Szy
      else
         call xwrite(' Failure to figure out necessary allocation', 5)
         goto 500
      endif

      call ralloc(1, sdim, 1, p_sum, status)
      call ralloc(1, sdim, 1, p_sumtot, status)
      call workalloc(1, sdim, 1, p_npix, status)
      call ralloc(1, sdim, 1, p_xy, status)
      if ( status.ne.0 ) then
         call xwrite(' Memory allocation failure in SLICE', 10)
         goto 500 
      endif

      if ( infile.ne.' ' ) then
c
c  Read input file (only care about first two columns)
c
         call txrdcol(infile, 1, sdim, memr(p_xy), igood, status)
         call txrdcol(infile, 2, sdim, memr(p_sum), igood, status)
         if ( status.ne.0 ) then 
            call xwrite(' Failed to read necessary columns', 10)
            goto 400
         endif
         summin = 1e30
         summax = -1e30
         do i = 1, igood
            if ( memr(p_sum+i-1).gt.summax ) summax = memr(p_sum+i-1)
            if ( memr(p_sum+i-1).lt.summin ) summin = memr(p_sum+i-1)
         enddo
      else
c
c  Figure out selected range in array coordinates
c
         if ( x_slice ) then
            xpix = xcen
            call calimgpix(Szx, Szy, zmx, zmy, xcen, ycen, xpix,
     &                     Startpix, ximg, yimg, 2)
            istart = MAX(1,nint(yimg))
            call calimgpix(Szx, Szy, zmx, zmy, xcen, ycen, xpix,
     &                     Endpix, ximg, yimg, 2)
            iend = MIN(Szy,nint(yimg))
         else
            ypix = ycen
            call calimgpix(Szx, Szy, zmx, zmy, xcen, ycen, Startpix,
     &                     ypix, ximg, yimg, 2)
            istart = MAX(1,nint(ximg))
            call calimgpix(Szx, Szy, zmx, zmy, xcen, ycen, Endpix,
     &                     ypix, ximg, yimg, 2)
            iend = MIN(Szx,nint(ximg))
         endif
         write(ZWRite, *) ' Index start/end : ', istart, iend
         call xwrite(ZWRite, 15)
c
c  Do calculation
c
         call slicework(Map, Szx, Szy, Mapid, x_slice, istart, iend,
     &                  log, sdim, memr(p_sum), memr(p_sumtot),
     &                  memi(p_npix), memr(p_xy), summin, summax,
     &                  status)
         if ( status.ne.0 ) goto 400
c
c  Spectrogram
c
         if ( spectrogram ) then
c
c       Calculate average
c
            if ( mean ) then
               do i = 1, sdim
                  if ( memr(p_sum+i-1).ne.0 ) then
                     itotal = itotal + 1
                     average = average + memr(p_sum+i-1)
                  endif
               enddo
               if ( itotal.ne.0 ) then
                  average = average/float(itotal)
               else
                  call xwrite(' No data available for spectrogram', 10)
                  goto 400
               endif
            endif
c
c       Determine sums for spectrogram
c
            igood = 0
            do i = 1, sdim
               if ( memr(p_sum+i-1).ne.0 ) then
                  igood = igood + 1
                  if ( .not.mean ) then
                     memr(p_sum+igood-1) = memr(p_sum+i-1)
                  elseif ( average.ne.0 ) then
                     memr(p_sum+igood-1) = (memr(p_sum+i-1)-average)/
     &                                     average
                  else
                     call xwrite(' Average value is 0', 5)
                     goto 400
                  endif
                  memr(p_xy+igood-1) = memr(p_xy+i-1)
                  memr(p_sumtot+igood-1) = memr(p_sumtot+i-1)
                  memi(p_npix+igood-1) = memi(p_npix+i-1)
               endif
            enddo
c
c  Calculate error arrays
c
            call ralloc(1, sdim, 1, p_errup, status)
            call ralloc(1, sdim, 1, p_errdn, status)
            if ( igood.gt.0 ) then
               summin = memr(p_sum)
               summax = memr(p_sum)
               write(ZWRite,*) ' Error (Val+/-Val*Error): ', errspec
               call xwrite(ZWRite,15)
               do i = 1, igood
                  memr(p_errup+i-1) = memr(p_sum+i-1)
     &                                + abs(memr(p_sum+i-1))*errspec
                  memr(p_errdn+i-1) = memr(p_sum+i-1)
     &                                - abs(memr(p_sum+i-1))*errspec
                  if ( memr(p_errup+i-1).gt.summax ) 
     &                   summax = memr(p_errup+i-1)
                  if ( memr(p_errdn+i-1).lt.summin ) 
     &                   summin = memr(p_errdn+i-1)
               enddo
            else
               call xwrite(' Nothing to plot (igood=0)', 5)
               status = -1
               goto 400
            endif
         else
            igood = sdim
         endif
      endif
c
c previous frame. If a slice has been already choosen, previos frame
c qualifiers will plot the second slice with the min and max from
c the previous frame)
c
      if ( previous_frame ) then
         if ( svsummin.lt.0. .and. svsummax.lt.0. ) then
            call XWRITE(' No previous slice, using'//
     &                  ' current min/max', 10)
         else
            summin = svsummin
            summax = svsummax
         endif
      endif
      if ( minstr.ne.' ' ) then
         call strnum(minstr, 4, dd, status)
         summin = dd
      endif
      if ( maxstr.ne.' ' ) then
         call strnum(maxstr, 4, dd, status)
         summax = dd
      endif
      svsummin = summin
      svsummax = summax
c
c plot the value sum(i) versus xy(i) either overlay with the display
c or in a different frame.
c
      write(ZWRite, *) ' Plot min/max (Cnts/img pixel): ', summin,
     &                                                     summax
      call xwrite(ZWRite, 10)
      call line_pgstate(bordcol, 2, 1)

      IF ( x_slice ) THEN
         IF ( nooverlay ) THEN
            CALL pgsvp(xvp1,xvp2,yvp2+.05,1.)
            CALL pgswin(xw1,xw2,summin,summax)
            CALL PGBOX('BCNT',0.,0,'BCNT',0.,0)
         ELSE
            CALL pgswin(xw1,xw2,summin,summax)
         ENDIF
         call line_pgstate(color, lwidth, lstyle)
         if ( spectrogram ) then
            if ( connect ) then
               call pgline(igood, memr(p_xy), memr(p_sum))
            else
               call pgpoint(igood, memr(p_xy), memr(p_sum), symbol)
               call pgerry(igood, memr(p_xy), memr(p_errup), 
     &                            memr(p_errdn), 0.)
            endif
         else
            call pgline(igood,memr(p_xy),memr(p_sum))
         endif
      ELSE
         IF ( nooverlay ) THEN
            CALL pgsvp(xvp1,xvp2,yvp2+.05,1.)
            CALL pgswin(yw1,yw2,summin,summax)
            CALL pgbox('BCNT',0.,0,'BCT',0.,0)
            call line_pgstate(color, lwidth, lstyle)
            CALL PGLINE(igood,memr(p_xy),memr(p_sum))
         ELSE
            CALL pgswin(summin,summax,yw1,yw2)
            call line_pgstate(color, lwidth, lstyle)
            CALL PGLINE(igood,memr(p_sum),memr(p_xy))
         ENDIF
      ENDIF
c
c  Write to file
c
      if ( outfile.ne.' ' ) then
         call dirpos(outfile, i1, i2)
         oflen = LENACT(outfile)
         if ( i2.lt.oflen ) then
            write(ZWRite, *) ' Writing slice into file: ',
     &                       outfile(i2+1:oflen)
            call xwrite(ZWRite, 10)
         else
            call xwrite(' Attempting to write slice into file', 10)
         endif
         call xwrite(' Full path:', 15)
         call xwrite(outfile, 15)

         template = 'shortid'
         call lkupfile(template, 'hdr', 'header template', status)
         if ( status.ne.0 ) return

         call txinit(status)
         call txwrcom(outfile, 'color off 3 4', status)
         call txwrcom(outfile, '!', status)
         call txwrcom(outfile, '! Slice cut file', status)
         call txwrcom(outfile, '!', status)
         call txwrhdr(outfile, mapid, template, status)
         call txwrcom(outfile, '!', status)
            
         if ( x_slice ) then
            plotdir = 'X'
            rangedir = 'Y'
         else
            plotdir = 'Y'
            rangedir = 'X'
         endif
         write(ZWRite, *) '! Slice selected from ', startpix, ' to ',
     &                     endpix, ' in ', rangedir,' (detector pixels)'
         call RMVXBK(ZWRite)
         call txwrcom(outfile, ZWRite, status)
         write(ZWRite, *) '! Slice selected from ', istart, ' to ',
     &                     iend, ' in ', rangedir,' (image pixels)'
         call RMVXBK(ZWRite)
         call txwrcom(outfile, ZWRite, status)
         call txwrcom(outfile, '! Column 1: '//plotdir//' pixels', 
     &                          status)
         if ( mean ) then
            call txwrcom(outfile, '! Column 2: (Cnts/pix-avg)/avg', 
     &                                                       status)
         else
            call txwrcom(outfile, '! Column 2: Count/npixels', status)
         endif
         call txwrcom(outfile, '! Column 3: Counts', status)
         call txwrcom(outfile, '! Column 4: Npixels', status)
         call txwrcom(outfile, '!', status)
         call txwrcol(outfile, memr(p_xy), igood, status)
         call txwrcol(outfile, memr(p_sum), igood, status)
         call txwrcol(outfile, memr(p_sumtot), igood, status)
         call txwricol(outfile, memi(p_npix), igood, status)
         call txwrskey(outfile, 'SLICEDIR', 0, plotdir, 
     &                 'Slice direction', status)
         call txwrkey(outfile, 'SLICEMIN', 0, startpix, 
     &                 'Slice range minimum in det. coords', status)
         call txwrkey(outfile, 'SLICEMAX', 0, endpix,
     &                 'Slice range maximum in det. coords', status)
         call txwrfile(outfile, status)
      endif

      if ( plot ) then
         if ( infile.ne.' ' ) then
            cmd = 'qdp '//infile(1:LENACT(infile))
         else
            cmd = 'qdp '//outfile(1:LENACT(outfile))
         endif
         call xwrite(cmd, 15)
         call spawn(cmd, LENACT(cmd), status)
      endif
c
c  Free arrays
c
  400 continue
      call ralloc(0, sdim, 1, p_sum, status)
      call ralloc(0, sdim, 1, p_sumtot, status)
      call workalloc(0, sdim, 1, p_npix, status)
      call ralloc(0, sdim, 1, p_xy, status)
      if ( spectrogram ) then
         call ralloc(0, sdim, 1, p_errup, status)
         call ralloc(0, sdim, 1, p_errdn, status)
      endif
      status = 0
c
c back to the image vp and world coordinates.
c
  500 continue
      CALL pgsvp(xvp1,xvp2,yvp1,yvp2)
      CALL pgswin(xw1,xw2,yw1,yw2)
      CALL PGUNSA
c
c  Journal command
c
      if ( cursor ) then
         dd = startpix
         call xdstr(dd, -1, startstr, slen)
         call spars(Cmdid,'START_PIXEL',startstr,status)
         dd = endpix
         call xdstr(dd, -1, endstr, slen)
         call spars(Cmdid,'END_PIXEL',endstr,status)
      endif
      call jrncmd(Cmdid, status)
      status = 0

      RETURN
      END
