      subroutine backgd_out(mapid, outfile, status)
      implicit none
c
c  Write contents of backgd include to file
c
c  I  mapid    (s)  Map id string
c I/O outfile  (i)  Output file location
c  O  status   (i)  Error flag (0 = OK)
c
      character*(*) mapid, outfile
      integer status

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/dynmem.inc'
      include 'backgd.inc'
c
c  Local variables
c
      character*(MAX_FILELEN) template
      integer i, p_bgnum, frstat, LENACT
 
      status = 0
      call workalloc(1, NBOxes, 1, p_bgnum, status)
      if ( status.ne.0 ) return
      do i = 1, NBOxes
         memi(p_bgnum+i-1) = i
      enddo

      template = 'shortid'
      call lkupfile(template, 'hdr', 'header template', status)
      if ( status.ne.0 ) return

      call xtend(outfile, 'bg')
      write(ZWRite, *) ' Writing background file: ', 
     &                 outfile(:LENACT(outfile))
      call XWRITE(ZWRite, 10)

      call txinit(status)
      call txwrcom(outfile, '!', status)
      call txwrcom(outfile, '!  Background output file ', status)
      call txwrcom(outfile, '!', status)
      call txwrhdr(outfile, mapid, template, status)
      call txwrcom(outfile, '!', status)
      call txwrikey(outfile, 'NBOXES', 0, NBOxes, 
     &             'Total no of bg boxes', status)
      call txwrikey(outfile, 'NBX', 0, NBX, 
     &             'No of bg boxes along x', status)
      call txwrikey(outfile, 'NBY', 0, NBY, 
     &             'No of bg boxes along y', status)
      call txwrikey(outfile, 'IBBAC', 0, IBBac, 
     &             'Size of bg boxes (image pix)', status)
      call txwrkey(outfile, 'BACK', 0, BACk, 
     &             'Background (1st estimate)', status)
      call txwrkey(outfile, 'BNEW', 0, BNEw, 
     &             'Background (cnts/image pix)', status)
      call txwrcom(outfile, '!', status)
      call txwrcom(outfile, '! Column 1: Box number', status)
      call txwrcom(outfile, '! Column 2: BB_FLAGS (box flags)', 
     &              status)
      call txwrcom(outfile, '!           0 = good', status)
      call txwrcom(outfile, '!           1 = barycenter too far '//
     &                       'from center', status)
      call txwrcom(outfile, '!           2 = non-poissonian '//
     &                       'statistics', status)
      call txwrcom(outfile, '!           3 = back value too far '//
     &                       'from average', status)
      call txwrcom(outfile, '!           4 = box halves differ '//
     &                       'too much in dist.', status)
      call txwrcom(outfile, '! Column 3: BB (box values)', status)
      call txwrcom(outfile, '! Column 4: BB_SIG (box error)', status)
      call txwrcom(outfile, '! Column 5: WW (box weights)', status)
      call txwrcom(outfile, '! Column 6: BB_NPIX (no of pixels in box)'
     &                       , status)
      call txwrcom(outfile, '!', status)
     
      call txwricol(outfile, memi(p_bgnum), NBOxes, status)
      call txwricol(outfile, BB_flags, NBOxes, status)
      call txwrcol(outfile, BB, NBOxes, status)
      call txwrcol(outfile, BB_sig, NBOxes, status)
      call txwrcol(outfile, WW, NBOxes, status)
      call txwricol(outfile, BB_npix, NBOxes, status)
      call txwrfile(outfile, status)

      if ( status.ne.0 ) then
         call XWRITE(' Failed to write background file', 10)
      endif

      frstat = 0
      call workalloc(0, NBOxes, 1, p_bgnum, frstat)

      RETURN
      END
