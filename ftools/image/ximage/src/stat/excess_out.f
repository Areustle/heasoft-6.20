      subroutine excess_out(mapid, outfile, status)
      implicit none
c
c  Writes excess finding results to file
c
c  I  mapid    (s)  map id string
c I/O outfile  (s)  output file
c  O  status   (i)  error flag
c
      character*(*) mapid, outfile
      integer status

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/dynmem.inc'
      include 'excess.inc'
c
c  Local variables
c
      integer*4 p_exnum, p_detx, p_dety, p_bxhf, frstat
      integer*4 i, szx, szy, LENACT
      real*8 xcen, ycen, zmx, zmy
      character*(MAX_FILELEN) template

      p_exnum = -1
      p_detx = -1
      p_dety = -1
      p_bxhf = -1

      template = 'shortid'
      call lkupfile(template, 'hdr', 'header template', status)
      if ( status.ne.0 ) return

      call xtend(outfile, 'xs')
      write(ZWRite, *) ' Writing excess file: ',
     &                   outfile(:LENACT(outfile))
      call XWRITE(ZWRite, 10)

      call workalloc(1, NUMexs, 1, p_exnum, status)
      if ( status.ne.0 ) goto 100
      call ralloc(1, NUMexs, 1, p_detx, status)
      if ( status.ne.0 ) goto 100
      call ralloc(1, NUMexs, 1, p_dety, status)
      if ( status.ne.0 ) goto 100
      call ralloc(1, NUMexs, 1, p_bxhf, status)
      if ( status.ne.0 ) goto 100

      call get_refram(mapid, szx, szy, zmx, zmy, xcen, ycen, status)
      do i = 1, NUMexs
         memi(p_exnum+i-1) = i
c        call calimgpix(szx, szy, zmx, zmy, xcen, ycen,
c    &                  memr(p_detx+i-1), memr(p_dety+i-1), 
c    &                  ASOUx(i), ASOUy(i), 1)
         call calimgpix(szx, szy, zmx, zmy, xcen, ycen,
     &                  memr(p_detx+i-1), memr(p_dety+i-1), 
     &                  ASOUx(i)-0.5, ASOUy(i)-0.5, 1)
         memr(p_bxhf+i-1) = BXN(i)*zmx/2.
      enddo

      call txinit(status)
      call txwrcom(outfile, '!', status)
      call txwrcom(outfile, '! Non-contiguous Excesses', status)
      call txwrcom(outfile, '!', status)
      call txwrhdr(outfile, mapid, template, status)
      call txwrcom(outfile, '!', status)
      call txwrcom(outfile, '! Column 1: Excess number', status)
      call txwrcom(outfile, '! Column 2: X image coord', status)
      call txwrcom(outfile, '! Column 3: Y image coord', status)
      call txwrcom(outfile, '! Column 4: X det coord', status)
      call txwrcom(outfile, '! Column 5: Y det coord', status)
      call txwrcom(outfile, '! Column 6: Box size', status)
      call txwrcom(outfile, '! Column 7: Box half-size (det coord)', 
     &                                          status)
      call txwrcom(outfile, '! Column 8: Box value', status)
      call txwrcom(outfile, '!', status)
      call txwricol(outfile, memi(p_exnum), NUMexs, status)
      call txwrcol(outfile, ASOUx, NUMexs, status)
      call txwrcol(outfile, ASOUy, NUMexs, status)
      call txwrcol(outfile, memr(p_detx), NUMexs, status)
      call txwrcol(outfile, memr(p_dety), NUMexs, status)
      call txwrcol(outfile, BXN, NUMexs, status)
      call txwrcol(outfile, memr(p_bxhf), NUMexs, status)
      call txwricol(outfile, INTm, NUMexs, status)
      call txwrfile(outfile, status)

 100  continue
      call workalloc(0, NUMexs, 1, p_exnum, frstat)
      call ralloc(0, NUMexs, 1, p_detx, frstat)
      call ralloc(0, NUMexs, 1, p_dety, frstat)
      call ralloc(0, NUMexs, 1, p_bxhf, frstat)


      if ( status.ne.0 ) then
         call XWRITE(' Failed to  write excess output', 10)
      endif

      return
      end
