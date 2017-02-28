c
      subroutine xrfwrshe(lui,itype,dtint,dtsta,dtsto,dtzero
     &                   ,dtoffset,dtp,keywords,tmax,refflag,ivect
     &                   ,npts,extns,gtizero,gtista,gtisto,ierr)

c >>> May want an announcement about energy options.<<<
 
c WRite Xronos Fits HEader information -- Short format.
 
c   I  lui      (i) = lu of input FITS file.
c   I  itype    (i) = 1 for events, 2 for bins, 3 for packets
c   I  dtint    (d) = bin integration time 
c                     (if <0 indicates an arrival time file)
c   I  dtsta    (d) = start time of xronos FITS file (days)
c   I  dtsto    (d) = stop    "   "    "    "    "     "
c   I  dtzero   (d) = zero-point time value "    "     "
c   I  dtp      (d) = time increment for packet data
c   I  keywords (c*20) = descriptive keyword values
c   I  tmax     (i) = number of times in each packet
c   I  refflag  (i) = flag for how times were reconstructed from the file
c   I  ivect    (i) = Listing of column names and numbers
c   I  npts     (i) = total number of rows to be read from each extension
c   I  extns    (i) = Array locating FITS extensions for RATE TABLE,
c                     GTIs and EXPOSURE.  (More in the future)
c   I  gtizero  (d) = TIMEZERO value in GTI extension
c   I  gtista   (d) = First GTI start.
c   I  gtisto   (d) = Last GTUI stop.
c   I  ierr     (i) = flag indicating non-fatal errors to be announced.

c Subroutines called: ftgkys, ftgkyl, ftgkyd, timofday, lenact

c Author: eal          HEASARC/GSFC, September, 1993
c Revised:                           November, 1993 to display information
c                                    from GTI and EXPOSURE extensions.
c                                    February, 1994 to receive dummy keyword
c                                    values and to work on vax.

      integer cmax
      parameter (cmax = 100)
      character(16) telescop, instrume, object, comm, detname, clockcor
     &   , filter, timesys, extname, ra_src, dec_src
      character(20) ttitle, fld, keywords(40)
      character(40) card1, card2, card3, card4, card5, card6
      character(160) card
      logical vignapp, deadapp, backapp, clockapp,dnear
      integer lt,ivect(*), npts(*), kystat, lui, extns(*), refflag
     &   , xtend, ftstat, ierr, tmax, itype, LENACT, ichat
      double precision dtint, dtsta, dtsto, dtzero, dtoffset, dtp
     &   , vignet, deadc, backv, gtista, gtisto, gtizero, tol
      parameter(tol = 1.d-12)

      external dnear
      data deadc, vignet, backv /3*1.d0/
c      ichat = lt - 2
      ichat=10

      extname  = keywords(17)(1:16)
      telescop = keywords(19)(1:16)
      detname  = keywords(21)(1:16)
      instrume = keywords(20)(1:16)
      object   = keywords(18)(1:16)
      timesys  = keywords(24)(1:16)
      filter   = keywords(22)(1:16)
      clockcor = keywords(23)(1:16)
      ra_src   = keywords(9) (1:16)
      dec_src  = keywords(13)(1:16)
      if(clockcor.eq.' ') clockcor = 'No'

c Formats:

101   format (1x,a20,a18)
102   format (1x,a20,a18)

c Read necesary keywords.

c     First make sure this is the rate table header.
      ftstat = 0
      CALL ftmahd(lui,extns(1),xtend,ftstat)

      clockapp = .FALSE.
      kystat = 0 
      CALL ftgkyl(lui,'CLOCKAPP',clockapp,comm,kystat)
      if(clockapp) clockcor = 'Yes'
      vignapp = .FALSE.
      kystat = 0 
      CALL ftgkyl(lui,'VIGNAPP ',vignapp ,comm,kystat) 
      deadapp = .FALSE.
      kystat = 0 
      CALL ftgkyl(lui,'DEADAPP ',deadapp ,comm,kystat) 
      backapp = .FALSE.
      kystat = 0 
      CALL ftgkyl(lui,'BACKAPP ',backapp ,comm,kystat) 
      kystat = 0 

c Banner line:

      CALL xwrite(' ',ichat)

      write(card1,'('' Selected FITS extensions:'')')
      write(card2,'(i2, '' - RATE TABLE;  '')') extns(1) - 1
      if(extns(3).gt.0) then
         write(card3,'(i2, '' - EXPOSURE''  )') extns(3) - 1
      elseif(extns(2).gt.0) then 
         write(card3,'(i2, '' - GTIs''  )') extns(2) - 1
      else
         card3 = ' '
      endif

      card = card1(:lenact(card1)) // card2(:lenact(card2)) 
     &    // card3(:lenact(card3))
      CALL xwrite(card,ichat)

      CALL xwrite(' ',ichat)

c Line 1: OBJECT and Start Time.

      ttitle = 'Source ............'
      write(fld, '(a)') object
      write (card1,101) ttitle, fld

      if(refflag.gt.0) then
         ttitle = 'TJD Start Time ....'
      else
         ttitle = 'Start Time (d) ....'
      endif
c     Get character calendar representation for the time of day.
      CALL timofday(dtsta,0, comm)
      write(fld,'(i5,x,a12)') int(dtsta), comm
      write(card2,102) ttitle, fld

      card = card1 // card2
      CALL xwrite(card,ichat)

c Line 2: FITS extension name and number, and Stop Time.

      ttitle = 'FITS Extension ....'
      write (fld, '(i2,'' - `'',a10,''`'')') extns(1)-1, extname
      write (card1,101) ttitle, fld

      if(refflag.gt.0) then
         ttitle = 'TJD Stop Time .....'
      else
         ttitle = 'Stop Time (d) .....'
      endif
c     Get character calendar representation for the time of day.
      CALL timofday(dtsto,0, comm)
      write(fld,'(i5,x,a12)') int(dtsto), comm
      write(card2,102) ttitle, fld

      card = card1 // card2
      CALL xwrite(card,ichat)

c Line 3: Number of FITS rows and integration time.
 
      ttitle = 'No. of Rows .......'
      write(fld,'(I12)') npts(1)
      write (card1,101) ttitle, fld

      ttitle = 'Bin Time (s) ......'
      if(dtint.gt.0) then
         write(fld,'(g12.4)') dtint
      else
         fld = 'none'
      endif
      write(card2,102) ttitle, fld

      card = card1 // card2
      CALL xwrite(card,ichat)

c Line 4: RA and TIMESYS.

      ttitle = 'Right Ascension ...'
      write (fld, '(a16)') ra_src
      write(card1,101) ttitle, fld

      ttitle = 'Internal time sys..'
      if(refflag.lt.0) then
         fld = 'Literal'
      else
         fld = 'Converted to TJD'
      endif
      write(card2,102) ttitle, fld

      card = card1 // card2
      CALL xwrite(card,ichat)

c Line 5: DEC and TELESCOP/INSTRUME

      ttitle = 'Declination .......'
      write (fld, '(a16)') dec_src
      write(card1,101) ttitle, fld

      ttitle = 'Experiment ........'
      write (fld, '( a8, x, a9)') telescop, instrume
      write(card2,102) ttitle, fld 

      card = card1 // card2
      CALL xwrite(card,ichat)

c Line 6: DETNAME and/or FILTER, if present.

      if(detname.ne.' ') then
         ttitle = 'Detector ..........'
         write(fld,'(a16)') detname
         write(card1,101) ttitle, fld
      endif

      if(filter.ne.' ') then
         ttitle = 'Filter ............'
         write(fld,'(a16)') filter
         if(detname.ne.' ') then
            write(card2,102) ttitle, fld
         else
            write(card1,101) ttitle, fld
            card2 = ' '
         endif
      else
         card1 = ' '
         card2 = ' '
      endif

      card = card1 // card2
      CALL xwrite(card,ichat)

c Line 7: Corrections

      if(vignapp) then
         card1 = 'Yes'
         call ftgkyd(lui,'VIGNET  ',vignet  ,comm,kystat) 
         kystat=0
      else
         card1 = 'No'
      endif
      if(deadapp) then 
         card2 = 'Yes'
         call ftgkyd(lui,'DEADC',deadc,comm,kystat) 
         kystat=0
      else 
         card2 = 'No' 
      endif 
      if(backapp) then 
         card3 = 'Yes' 
         call ftgkyd(lui,'BACKV   ',backv   ,comm,kystat) 
         kystat=0
      else 
         card3 = 'No' 
      endif 

      write(card,1106) card1, card2, card3, clockcor
1106  format(1x,    'Corrections applied: Vignetting - ',a3,
     &              '; Deadtime - ',a3, '; Bkgd - ',a3,'; Clock - ',a3)
      CALL xwrite(card,ichat)

      if(vignapp.or.deadapp.or.backapp) then
         write(card,1107) vignet, deadc ,backv
1107     format(1x, '            values: ', 3(g14.9,3x) )
         CALL xwrite(card,ichat)
      endif


c Line 8: Selected columns

      card2 = ' '
      card3 = ' '
      card4 = ' '
      card5 = ' '
      card6 = ' '
      card = ' '
      write (card1,'('' Selected Columns: '')') 
      if(ivect(1).gt.0) write(card2,'(i3,''- Time;''      )') ivect(1)
      if(ivect(2).gt.0) write(card3,'(i3,''- Y-axis;''    )') ivect(2)
      if(ivect(3).gt.0) write(card4,'(i3,''- Y-error;''   )') ivect(3)
      if(ivect(4).gt.0) write(card5,'(i3,''- Dead Time;'' )') ivect(4)
      if(ivect(5).gt.0) write(card6,'(i3,''- Delta Time;'')') ivect(5)
      if(ivect(6).gt.0) write(card6, 
     &                     '(i3,''- Fractional exposure;'')') ivect(6)
      if(ivect(9).gt.0) write(card3,'(i3,''- E-Channel;'' )') ivect(9)
      CALL xwrite(' ',ichat)
c
      card = card1(:lenact(card1)) // card2(:lenact(card2)) 
     &    // card3(:lenact(card3)) // card4(:lenact(card4)) 
     &    // card5(:lenact(card5)) // card6(:lenact(card6))
      CALL xwrite(card,ichat)

c Line 9: blank

      CALL xwrite(' ',ichat)

c Line 10: Type of file

      card = ' '
      if(itype.eq.1) then
         write(card,'('' File contains arrival-time data.'')')
      elseif(itype.eq.2) then
         if(dtp.lt.dtint) then
            write(card,'('' File contains Packet data:    dt = '',g12.4
     &                 ,'' s;'', i4, '' times per packet.'')') dtp, tmax
         else
            write(card,'('' File contains binned data.'')')
         endif
      endif
      CALL xwrite(card,ichat)

      write(card,'('' Print Fits Header'')') 
      call xwrite(card, 20)
      call xrfwrlhe(lui)

c Line 11: blank

      CALL xwrite(' ',ichat)

c End of RATE TABLE header printout.  The following prints information
c relating to other headers to be used by xronos.  Some variables
c from the rate table are overwritten.

c Move to the new extension header.
c GTI will be ignored if EXPOSURE is present.

      if(extns(3).gt.0) then
         CALL ftmahd(lui,extns(3),xtend,ftstat)
      elseif(extns(2).gt.0) then
         if(ierr.eq.101) goto 910
         if(ierr.eq.102) goto 910
         CALL ftmahd(lui,extns(2),xtend,ftstat)
      else
         return
      endif

c Currently hard-wired for GTIs.

      kystat = 0
      extname = ' '
      CALL ftgkys(lui,'EXTNAME',extname,comm,kystat)

c Line 1: Number and name of extension, and Start Time.

      ttitle = 'FITS Extension ....'
      write (fld, '(i2,'' - `'',a10,''`'')') extns(2)-1, extname
      write(card1,101) ttitle, fld

      ttitle = 'First GTI Start ...'
c     Get character calendar representation for the time of day.
      CALL timofday(gtista,0, comm)
      write(fld,'(i5,x,a12)') int(gtista), comm
      write(card2,102) ttitle, fld

      card = card1 // card2
      CALL xwrite(card,ichat)

c Line 2: FITS # of rows and Stop Time.

      ttitle = 'No. of Rows .......'
      write(fld,'(I12)') npts(2)
      write(card1,101) ttitle, fld

      ttitle = 'Last GTI Stop .....'
c     Get character calendar representation for the time of day.
      CALL timofday(gtisto,0, comm)
      write(fld,'(i5,x,a12)') int(gtisto), comm
      write(card2,102) ttitle, fld

      card = card1 // card2
      CALL xwrite(card,ichat)

      write(card,'('' Selected Columns: '', i4,'' - GTI Start; ''
     &          , i4,'' - GTI Stop   ''  )') ivect(7), ivect(8)

      CALL xwrite(card,ichat)

      write(card,'('' Print Fits Header'')') 
      call xwrite(card, 20)
      call xrfwrlhe(lui)
      
c Normal terminus.
      CALL xwrite(' ',ichat)
      CALL xwrite(' ',ichat)
      return

910   continue
      Write(card,'(''Error reading GTI extension!'')')
      CALL xwarn(card,ichat)
      Write(card,'(''GTI extension will not be used.'')')
      CALL xwarn(card,ichat)

      CALL xwrite(' ',ichat)
      CALL xwrite(' ',ichat)

      return
      end
