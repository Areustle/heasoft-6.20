      subroutine search_out(mapid, outfile, status)
      implicit none
c
c  Writes source searching results to file
c
c  I  mapid     (s)  Map id string
c  I  outfile   (s)  output file
c  O  status    (i)  error flag
c
      character*(*) mapid, outfile
      integer status

      include '../include/maxvals.inc'
      include '../include/io.inc'
      include '../include/dynmem.inc'
      include 'detect.inc'
c
c  Local variables
c
      integer i, p_srcnum, frstat, LENACT
      character*(MAX_FILELEN) template

      template = 'shortid'
      call lkupfile(template, 'hdr', 'header template', status)
      if ( status.ne.0 ) return

      call xtend(outfile, 'sr')
      write(ZWRite,*) ' Writing source search output: ',
     &                 outfile(:LENACT(outfile))

      call workalloc(1, NUMdet, 1, p_srcnum, status)
      if ( status.ne.0 ) goto 100

      do i = 1, NUMdet
         memi(p_srcnum+i-1) = i
      enddo

      call txinit(status)
      call txwrcom(outfile, '!', status)
      call txwrcom(outfile, '! Source search results', status)
      call txwrcom(outfile, '!', status)
      call txwrhdr(outfile, mapid, template, status)
      call txwrcom(outfile, '!', status)
      call txwrcom(outfile, '! Column 1: Source number', status)
      call txwrcom(outfile, '! Column 2: DTSOX (x det coord)', 
     &                                                     status)
      call txwrcom(outfile, '! Column 3: DTSOY (y det coord)', 
     &                                                     status)
      call txwrcom(outfile, '! Column 4: PSFCO (PSF correction)', 
     &                                                     status)
      call txwrcom(outfile, '! Column 5: VCO (vignetting correction)', 
     &                                                     status)
      call txwrcom(outfile, '! Column 6: BASo (local background)', 
     &                                                     status)
      call txwrcom(outfile, '! Column 7: SINt (signal)', status)
      call txwrcom(outfile, '! Column 8: ERR (error)', status)
      call txwrcom(outfile, '! Column 9: PROb (probability)', status)
      call txwrcom(outfile, '! Column 10: BXHa (box halfsize)', status)
      call txwrcom(outfile, '! Column 11: GOODFLAG (0 = not good, 1 = '
     &                       //'good)', status)
      call txwrcom(outfile, '! Column 12: Detect box (x img coord)', 
     &                                                     status)
      call txwrcom(outfile, '! Column 13: Detect box (y img coord)', 
     &                                                     status)
      call txwrcom(outfile, '!', status)
      call txwricol(outfile, memi(p_srcnum), NUMdet, status)
      call txwrcol(outfile, DTSox, NUMdet, status)
      call txwrcol(outfile, DTSoy, NUMdet, status)
      call txwrcol(outfile, PSFco, NUMdet, status)
      call txwrcol(outfile, VCO, NUMdet, status)
      call txwrcol(outfile, BASo, NUMdet, status)
      call txwrcol(outfile, SINt, NUMdet, status)
      call txwrcol(outfile, ERR, NUMdet, status)
      call txwrcol(outfile, PROb, NUMdet, status)
      call txwrcol(outfile, BXHa, NUMdet, status)
      call txwricol(outfile, GOOdflag, NUMdet, status)
      call txwrcol(outfile, IMSox, NUMdet, status)
      call txwrcol(outfile, IMSoy, NUMdet, status)
      call txwrfile(outfile, status)

      call workalloc(0, NUMdet, 1, p_srcnum, frstat)

 100  continue

      if ( status.ne.0 ) then
         call XWRITE(' Failed to write source search output', 10)
      endif

      return
      end
