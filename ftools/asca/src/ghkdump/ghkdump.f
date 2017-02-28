C  FTOOLs info $Header: /headas/headas/ftools/asca/src/ghkdump/ghkdump.f,v 3.14 2013/05/21 19:08:06 irby Exp $
C
C***********************************************************************
C     Astro-D Quick Analysis FTOOL:
C     ghkdump
C
C     DESCRIPTION:
C     Read Astro-D GIS HK file and dump GIS HK parameters
C
C     AUTHOUR/DATE:
C     Ken Ebisawa 1993/03/17
C
C     MODIFICATION HISTORY:
C     Ver 1.0 1993/03/17 for FRFread ver. 2.952
C     Ver 1.1 1993/03/19 for FRFread ver. 2.961
C     Ver 2.0 1993/05/18 for FRFread ver. 2.980
C     *Can read plural files from a file list @file_list.
C     *Write Start and End time, Number of events.
C     *Read modal parameters from files modal, modal2
C
C     Ver 2.1 1993/05/20 for FRFread ver. 2.980
C     *Modal parameters are not read from files, but hard-wired to the
C     Progarm. (See the block data sentence in the end of the file).
C
C     Ver 3.0 1993/11/02 for FRFread ver 3.000
C     Significant change for FRFread version 3.000.
C     In FRFRead 3.000, a single HK file is made from a FRF.
C     Keywords are changed significantly.
C
C     Ver 4.0 1994/09/14 for FRFread ver 3.025
C     * Modification corredponding to the change of
C     several HK parameter names in FRFread.
C     This change of HK paramater names is requirement from the
C     new MKFILTER program which reads HK files instead of a FRF file.
C     * ascatime --> UT routines are changed.
C     * New hidden parameter 'modal_check'.  If 'no' (default), modal
C      parameter change is not checked.
C
C     ver 5.0 95/03/21 at ISAS
C     * Error on HP was reported (find HP) and corrected.
C     * Error was found when 'average=yes' and corrected.
C
C     by Jeff Guerber, GSFC664/HSTX, 1997/2/24
C     * In ghkdp2, exact=.false. so ftgcno calls will be case-insensitive.
C
C     Jeff Guerber, GSFC/RSTX, 1998-07-03
C     * Increase size of date/time strings, write them in new fits format
C      ("yyyy-mm-ddThh:mm:ss").  Don't truncate year.
C
C     ver 5.1 2000/03/13 Ken Ebisawa
C     BITRATE, ORBITBEG, ORBITEND are not read from the primary header.
C     New FRFREAD (up from some recent version) will not write these
C     keywords. The routine 'puphead' has been modified.
C
C     NOTES:
C
C     USAGE:
C
C     ARGUMENTS:
C     none
C
C     PRIMARY LOCAL VARIABLES:
C
C     CALLED ROUTINES:
C     subroutine g2ishk - gets parameters from parameter files
C     subroutine ghkdp2 - read GIS HK file and dump parameters
C***********************************************************************
      subroutine ghkdup
C     implicit none
      character(160) infile, outfil
      character(80) context
      double precision intvl
      logical averge, modal_check
C     Added ERROR REPORT JKB
      character(40) taskname
      common /task/ taskname

      taskname = 'ghkdump4.0'

c     Print the version number
C     context = 'GHKDMP ver 1.1 (1993/03/19)'
C     context = 'GHKDMP ver 2.0 (1993/05/18)'
C      context = 'GHKDMP ver 2.1 (1993/05/20)'
C      context = 'GHKDMP ver 3.0 (1993/11/02)'
C      context = 'GHKDMP ver 4.0 (1994/09/14)'
      context = 'GHKDMP ver 5.1 (2000/3/13)'

      call fcecho(context)
c$$$      context =
c$$$     $     'This version is consistent with FRFread ver 3.025'
c$$$      call fcecho(context)

c     get parameters from parameter file
      call g2ishk(infile, outfil, intvl, averge, modal_check)

c     read the data and plot
      call ghkdp2(infile, outfil, intvl, averge, modal_check)
      end

C***********************************************************************
C     SUBROUTINE:
C     g2ishk
C
C     DESCRIPTION:
C     Get parametrs from parameter file
C
C     AUTHOUR/DATE:
C     Ken Ebisawa 1993/03/17
C
C     MODIFICATION HISTORY:
C
C     NOTES:
C
C     USAGE:
C     call g2ishk(infile, outfil, intvl, averge, modal_check)
C
C     ARGUMENTS:
C     infile(out) - input GIS HK file name
C     outfil(out) - output ASCII file name
C     intvl (out) - interval of dump (seconds)
c     averge(out) - dump average or sum
c     modal_check(out) - check modal parameter change (yes) or not (no)
C
C     PRIMARY LOCAL VARIABLES:
C     context - error message
C     status - error number
C
C     CALLED ROUTINES:
C     subroutine uclgst - get string parameters
C     subroutine uclgsd - get double precission parameters
C     subroutine uclgsb - get boolean parameters
C     subroutine fcecho - echo message to terminal
C     subroutine fcerrm - echo error message to terminal
C***********************************************************************
      subroutine g2ishk(infile, outfil, intvl, averge, modal_check)

C      implicit none
      character*(*) infile, outfil
      double precision intvl
      logical averge, modal_check

      character(80) context
      integer status

C     initialize variables
      status = 0

C     get the name of the input FITS file
      call uclgst('infile', infile, status)
      if(status.ne.0) then
         context = 'could not get the infile parameter'
         call fcerr(context)
         go to 999
      endif

C     get the name of the output ASCII file
      call uclgst('outfile', outfil, status)
      if(status.ne.0) then
         context = 'could not get the outfile parameter'
         call fcerr(context)
         go to 999
      endif

C     get the interval
      call uclgsd('interval', intvl, status)
      if(status.ne.0) then
         context = 'could not get the interval parameter'
         call fcerr(context)
         go to 999
      endif

C     Average or Sum ?
      call uclgsb('average', averge, status)
      if(status.ne.0) then
         context = 'could not understand Average or Sum'
         call fcerr(context)
         go to 999
      endif

C     modal parameter change check ?
      call uclgsb('modal_check', modal_check, status)
      if(status.ne.0) then
         context = 'could not get modal_check parameter'
         call fcerr(context)
         go to 999
      endif

 999  continue
      if(status. ne. 0) call fcerrm(status)

      return
      end


C***********************************************************************
C     SUBROITINE:
C     ghkdp2
C
C     DESCRIPTION:
C     Read GIS HK file and dump HK parameters
C
C     AUTHOUR/DATE:
C     Ken Ebisawa 1993/03/17
C
C     MODIFICATION HISTORY:
C     Ken Ebisawa 1993/03/19 - not to check TELESCOP keyword
C     Ken Ebisawa 1993/05/14 - Can read plural files from @file_list
C     Write Tstart and Tend
C     Read Modal Parameters from files modal and modal2
C     Jeff Guerber, GSFC664/HSTX, 1997/2/24 - Changed exact to .false. so
C        ftgcno call will be case-insensitive.
C
C     NOTES:
C
C     USAGE:
C     ghkdp2(infile, outfil, intvl, averge, modal_check)
C
C     ARGUMENTS:
C     infile(in) - input GIS HK file name
C     outfil(in) - output ASCII file
C     intvl (in) - interval of the dump
C     averge(in) - dump average (yes) or sum(no)
C     modal_check(in) - check modal prameter change (yes) or not (no)
C
C     PRIMARY LOCAL VARIABLES:
C
C     CALLED ROUTINES:
C     FTOOL library:
C     subroutine fcecho - echo message to terminal
C     subroutine fcerrm - echo error message to terminal
C     FITSIO library:
C     subroutine ftopen - open the fits file
C     subroutine ftclos - close the fits file
C     subroutine ftghpr - get primary header keywords
C     subroutine ftgkys - get header keywords
C     surroutine ftmrhd - move the extension
C     subroutine ftghbn - get extension header keywords
C     subroutine ftgcno - get column information
C     subroutine ftgcv[s,j,d] - get values in the extension
C
C     subroutine puphed - pick up header keywords
C     subroutine getint - output initial HK parameters, get scale down
C     subroutine gethk - get necessary HK parameter values
C     subroutine clrstr - clear string
C     subroutine fisct2 - ASCA time --> UT, Thank you Emily !
C     subroutine fid2my - ASCA time --> UT, Thank you Emily !
C***********************************************************************
      subroutine ghkdp2(infile, outfil, intvl, averge, modal_check)
      implicit none
      character*(*) infile, outfil
      double precision intvl
      logical averge, modal_check

      integer maxcl
      parameter (maxcl = 999)
      character(80) context, comment
      character(150) head1, head2, head3, head4, head5, head6, head7
      character(150) outraw
      character(8) telscp, contnt, instrm, bitrat, item, datmod
      integer iunit, ounit, block, ftstat
      logical simple, extend, inopen, outopn, exact, anyf
      integer i, bitpix, naxis, naxes(maxcl), pcount, gcount,
     $     htype, nrows, tfields, varidat, colnum(maxcl), value,
     $     LD, L0, L1, L2, H0, H1, H2, Cin,  Cout, Tout, RBM,
     $     rbmflg, cpuopr(2), L1SD, LDSD, M2SD, XRAYSD, ODDSD,
     $     events(8), X1d(32), Y1d(32)
      character(70) ttype(maxcl), tform(maxcl), tunit(maxcl), extname
      character(3) off_on(0:1)
      data off_on /'OFF', 'ON '/
      character(4) errnrm(0:1)
      data errnrm /'ERR ', 'NORM'/
      double precision time, timebuf
      real l_mon, h_mon
      real h_com, temp, r_temp
      integer year, month, day, hour, minute
      real second

C     To parse the infile
      integer ifile, ninfle
      character(80) vinfle(999)
C     negflg is used in fcgcls (I do not the mening though...)
      logical negflg

C     Initialize variables
      iunit = 15
      ounit = 16
      block = 0
      ftstat = 0
      inopen = .false.
      outopn = .false.
      exact = .false.


C     Parse the input file
      call fcgcls(infile, vinfle, ninfle, negflg)
C     vinfile is the array of the infile names, and ninfile is the
C     number of infiles.

C     Repeat the procedure for the number of infiles
      do 1000 ifile = 1, ninfle

C     Open the fits file
         call ftopen(iunit, vinfle(ifile), 0, block, ftstat)
         if (ftstat.ne.0) then
            context = 'Unable to open infile'
            call fcerr(context)
            go to 999
         end if
         inopen = .true.

C     Open the ASCII file
         if (outfil. ne. 'STDOUT') then
            outopn = .true.
            open(unit = ounit, file = outfil, status = 'new', err = 110)
            go to 120
 110        context = 'error: output file already existes ?'
            call fcerr(context)
            go to 999
 120        continue
         endif

c     Obtain significant primary header keywords
c     (ftgprh is an old fitsio routine)
c     call ftgprh(iunit, simple, bitpix, naxis, naxes,
         call ftghpr(iunit, maxcl, simple, bitpix, naxis, naxes,
     $        pcount, gcount ,extend, ftstat)
         if (ftstat.ne.0) then
            context = 'Error in the primary header'
            call fcerr(context)
            go to 999
         elseif (.not.simple) then
            context = 'File is not in the FITS format'
            call fcerr(context)
            go to 999
         elseif(.not.extend) then
            context = 'File does not have the extenstion'
            call fcerr(context)
            go to 999
         end if


c     Is this an ASCA  GIS HK file ?
         call ftgkys(iunit, 'TELESCOP', telscp, comment, ftstat)
         if (ftstat.ne.0) goto 999
         call ftgkys(iunit, 'CONTENT ', contnt, comment, ftstat)
         if (ftstat.ne.0) goto 999
         call ftgkys(iunit, 'INSTRUME', instrm, comment, ftstat)
         if (ftstat.ne.0) goto 999

         if(  (telscp.eq.'ASCA'.or.telscp.eq.'ASUKA').and.
     $        (instrm.eq.'GIS2    '.or.instrm.eq.'GIS3    ').
     $        and.contnt.eq.'HK      ') then
            continue
         else
            context = 'This is not a ASCA GIS HK file'
            call fcerr(context)
            go to 999
         endif

c     Set headers of the output file
c     clear the buffer
         call clrstr(head1)
         call clrstr(head2)
         call clrstr(head3)
         call clrstr(head4)

c     The first header
         head1 = 'ASCA GIS HK:'//vinfle(ifile)


c     Pick up header keywords from the primary header and make output
c     the second and third raws)
         call puphed(iunit, instrm, bitrat, datmod, head2, head3,
     $        head4, ftstat)

         if(ftstat.ne.0) go to 999

c     Print header rows at the top of the output file
         call fprint(ounit, outopn, head1)
         call fprint(ounit, outopn, head2)
         call fprint(ounit, outopn, head3)
         call fprint(ounit, outopn, head4)

c     Move to the first extenstion
         call ftmrhd(iunit, 1, htype, ftstat)
         if (ftstat.ne.0) then
            write(context, '(a34, i3)')
     $           'Error moving to the extension number', 1
            call fcerr(context)
            go to 999
         endif

c     Obtain the header keywords of the extension (ftgbnh is an old fitsio
c     subroutine)
         if (htype.eq.2) then
c     call ftgbnh(iunit, nrows, tfields, ttype,
            call ftghbn(iunit, maxcl, nrows, tfields, ttype,
     $           tform, tunit, extname, varidat, ftstat)
         else

c     Error if the type is not the binary extension
            context = 'Extension type not supported'
            call fcerr(context)
            go to 999
         endif

c     Get column information for the column 1, 2, 3.
         do 10 i = 1, tfields
            call ftgcno(iunit, exact, ttype(i), colnum(i), ftstat)
 10      continue

c     Get and ouput initial values of the modal parameters
c$$$  call clrstr(head1)
c$$$  call clrstr(head2)
c$$$  call clrstr(head3)
c$$$  call clrstr(head4)
c$$$  call clrstr(head5)
c$$$  call clrstr(head6)
c$$$  call clrstr(head7)

         if (modal_check) then
            call clrstr(head1)
            call fprint(ounit, outopn, head1)

            write(head1,*)
     $           '*** History of the modal parameter change ***'
            call fprint(ounit, outopn, head1)

C     Since Frfread 3.000, there will be a single HK file from a
C     single FRF. Therefore no longer modal pameter headers (head1-7) are
C     not written in the beginning.

         endif

c     Took GETINT out of the above if block, added modal_check as
c     an argument --- the intent is that, when modal_check is .false.,
c     we still want to initialize reading the HK file, not just print
c     out the vaious messages about modal parameter changes - KM, 2007/05/23

         call getint(iunit, ounit, outopn, instrm, datmod, nrows,
     $           modal_check, head1, head2, head3, head4,
     $           head5, head6, head7, L1SD, LDSD, M2SD, XRAYSD, ODDSD,
     $           ftstat)

c$$$  call fprint(ounit, outopn, head1)
c$$$  call fprint(ounit, outopn, head2)
c$$$  call fprint(ounit, outopn, head3)
c$$$  call fprint(ounit, outopn, head4)
c$$$  call fprint(ounit, outopn, head5)
c$$$  call fprint(ounit, outopn, head6)
c$$$  call fprint(ounit, outopn, head7)

c     write Average or Sum
         call clrstr(head1)
         call fprint(ounit, outopn, head1)
         if(averge) then
            head1 = ' *** Monitor Counts (per second), '//
     $           'Temperature, HV monitor, RBM flag, CPU monitor ***'
         else
            write(head1, '(a, f6.1, a, a)')
     $           ' *** Monitor Counts (per ', intvl,
     $           ' sec), Temperature, HV monitor, RBM flag, ',
     $           'CPU monitor ***'
         endif
         call fprint(ounit, outopn, head1)

c     write the first row (title) of the dump
         call clrstr(head1)
         head1 =  '     TIME[UT]           ASCA TIME'//
     $        '   LD   L0   L1   L2   H0   H1   H2 C_IN C_OUT'//
     $        ' T_OUT RBM   TMP  RTMP LMON HMON HCUM RFG'//
     $        ' CPU2/3'
         call fprint(ounit, outopn, head1)

c     Initialization of the parameters before the roop
         timebuf = 0.0
         LD = 0
         L0 = 0
         L1 = 0
         L2 = 0
         H0 = 0
         H1 = 0
         H2 = 0
         Cin = 0
         Cout = 0
         Tout = 0
         RBM = 0
         do 12 i = 1, 8
            events(i) = 0
 12      continue
         do 13 i = 1, 32
            X1d(i) = 0
            Y1d(i) = 0
 13      continue
         h_com = 0.0
         h_mon = 0.
         l_mon = 0.
         temp = 0.0
         r_temp = 0.0
         rbmflg = 0
         cpuopr(1) = 0
         cpuopr(2) = 0

c     Do roop from the row 1 to the end (nrows)
         do 500 i = 1, nrows
            call ftgcvs(iunit, colnum(1), i, 1, 1, ' ',
     $           item, anyf, ftstat)
            call ftgcvj(iunit, colnum(2), i, 1, 1, 0,
     $           value, anyf, ftstat)
            call ftgcvd(iunit, colnum(3), i, 1, 1, 0.0d0,
     $           time, anyf, ftstat)

c     get necessary HK parameter values
            call gethk(item, value, LD, L0, L1, L2,
     $           H0, H1, H2, Cin, Cout, Tout, RBM, temp, R_temp, L_mon,
     $           h_mon, h_com, rbmflg, cpuopr, events, x1d, y1d)

c     output data for every intvl time
            if (time.ge.timebuf+intvl) then
c     average monitor count values
               if(averge) then
                  LD = LD / intvl
                  L0 = L0 / intvl
                  L1 = L1 / intvl
                  L2 = L2 / intvl
                  H0 = H0 / intvl
                  H1 = H1 / intvl
                  H2 = H2 / intvl
                  Cin = Cin /intvl
                  Cout = Cout /intvl
                  Tout = Tout /intvl
                  RBM = RBM /intvl
               endif

c     Correction of the L1, LDhit scale down
               if(bitrat.eq.'MEDIUM  ') then
                  if(L1SD.eq.2.or.L1SD.eq.3) then
                     L1 = L1 * 8
                  endif
                  if(LDSD.eq.2.or.LDSD.eq.3) then
                     LD = LD * 8
                  endif
               elseif(bitrat.eq.'LOW     ') then
                  if(L1SD.eq.1) then
                     L1 = L1 * 4
                  elseif(L1SD.eq.2) then
                     L1 = L1 * 8
                  elseif(L1SD.eq.3) then
                     L1 = L1 * 32
                  endif
                  if(LDSD.eq.1) then
                     LD = LD * 4
                  elseif(LDSD.eq.2) then
                     LD = LD * 8
                  elseif(LDSD.eq.3) then
                     LD = LD * 32
                  endif
               endif

c     calculate UT from ASCA time (origin = 1993/01/01)
C     call fisct2(time, day, hour, minute, second)
C     call fid2my(1, 1, 1993, day, month, year)
               call asca2ut(time, day, month, year,
     $              hour, minute, second)

c     Print line
               if(rbm.gt.99999) then
                  write(outraw,'(i4.4, a1, i2.2, a1, i2.2, a1,
     $                 i2.2, a1, i2.2, a1, i2.2, 1x, f13.3, 10(i5),
CHP the last comma was missing in the next line (95/03/21)
     $                 1P,e6.0e1,0P,
     $                 2(f6.1), 2(i5), f5.1,
     $                 1x, a, 1x, a, a1, a)')
     $                 year, '-',month, '-',day, 'T',
     $                 hour, ':',minute, ':',int(second),
     $                 time, LD, L0, L1, L2, H0, H1, H2,
     $                 Cin, Cout, Tout, real(RBM),
     $                 temp, r_temp, int(L_mon), int(h_mon),
     $                 h_com,
     $                 off_on(rbmflg), errnrm(cpuopr(1)),'/',
     $                 errnrm(cpuopr(2))
               else
                  write(outraw,'(i4.4, a1, i2.2, a1, i2.2, a1,
     $                 i2.2, a1, i2.2, a1, i2.2, 1x, f13.3, 10(i5), i6,
     $                 2(f6.1), 2(i5), f5.1,
     $                 1x, a, 1x, a, a1, a)')
     $                 year, '-',month, '-',day, 'T',
     $                 hour, ':',minute, ':',int(second),
     $                 time, LD, L0, L1, L2, H0, H1, H2, Cin, Cout,
     $                 Tout,RBM,
     $                 temp, r_temp, int(L_mon), int(h_mon), h_com,
     $                 off_on(rbmflg), errnrm(cpuopr(1)),'/',
     $                 errnrm(cpuopr(2))
               endif
               call fprint(ounit, outopn, outraw)
c     new time value
               timebuf = time
c     clear the buffer
               LD = 0
               L0 = 0
               L1 = 0
               L2 = 0
               H0 = 0
               H1 = 0
               H2 = 0
               Cin = 0
               Cout = 0
               Tout = 0
               RBM = 0
            endif
 500     continue

c     print event monitor counts
         call clrstr(outraw)
         call fprint(ounit, outopn, outraw)
         write(outraw,  '(a)') ' *** Total Number of PH Events ***'
         call fprint(ounit, outopn, outraw)

c     Correction for MASK2, X-ray events
         if(M2SD.ge.1.and.M2SD.le.8) then
            events(7) = events(7) * 2**M2SD
         endif
         if(XRAYSD.ge.1.or.XRAYSD.le.8) then
            events(8) = events(8) * 2**XRAYSD
         endif

c     Correction for 1D distribution
         if(ODDSD.ge.1.and.ODDSD.le.8) then
            do 100 i = 1, 32
               x1d(i) = x1d(i) * 2**ODDSD
               y1d(i) = y1d(i) * 2**ODDSD
 100        continue
         endif

         call clrstr(outraw)
         write(outraw, '(8a)')
     $        '  AN_FF', ' AN_AL0', '   EDGE', '  MASK1', ' CALOUT',
     $        ' DSCOUT', '  MASK2', '  X-RAY'
         call fprint(ounit, outopn, outraw)

         call clrstr(outraw)
         write(outraw, '(8i7)') (events(i), i = 1, 8)
         call fprint(ounit, outopn, outraw)

         call clrstr(outraw)
         call fprint(ounit, outopn, outraw)

c     print X-1-D distribution
         write(outraw,  '(a)')
     $        ' *** Total Number of 1-D Distribution ***'
         call fprint(ounit, outopn, outraw)
         call clrstr(outraw)
         write(outraw, '(16a)')
     $        '    X-0', '    X-1', '    X-2', '    X-3', '    X-4',
     $        '    X-5', '    X-6', '    X-7', '    X-8', '    X-9',
     $        '   X-10', '   X-11', '   X-12', '   X-13', '   X-14',
     $        '   X-15'
         call fprint(ounit, outopn, outraw)

         call clrstr(outraw)
         write(outraw, '(16i7)') (x1d(i), i = 1, 16)
         call fprint(ounit, outopn, outraw)

         write(outraw, '(16a)')
     $        '   X-16', '   X-17', '   X-18', '   X-19', '   X-20',
     $        '   X-21', '   X-22', '   X-23', '   X-24', '   X-25',
     $        '   X-26', '   X-27', '   X-28', '   X-29', '   X-30',
     $        '   X-31'
         call fprint(ounit, outopn, outraw)

         call clrstr(outraw)
         write(outraw, '(16i7)') (x1d(i), i = 17, 32)
         call fprint(ounit, outopn, outraw)

         call clrstr(outraw)
c     print Y-1-D distribution
         write(outraw, '(16a)')
     $        '    Y-0', '    Y-1', '    Y-2', '    Y-3', '    Y-4',
     $        '    Y-5', '    Y-6', '    Y-7', '    Y-8', '    Y-9',
     $        '   Y-10', '   Y-11', '   Y-12', '   Y-13', '   Y-14',
     $        '   Y-15'
         call fprint(ounit, outopn, outraw)

         call clrstr(outraw)
         write(outraw, '(16i7)') (Y1d(i), i = 1, 16)
         call fprint(ounit, outopn, outraw)

         write(outraw, '(16a)')
     $        '   Y-16', '   Y-17', '   Y-18', '   Y-19', '   Y-20',
     $        '   Y-21', '   Y-22', '   Y-23', '   Y-24', '   Y-25',
     $        '   Y-26', '   Y-27', '   Y-28', '   Y-29', '   Y-30',
     $        '   Y-31'
         call fprint(ounit, outopn, outraw)
         write(outraw, '(16i7)') (Y1d(i), i = 17, 32)
         call fprint(ounit, outopn, outraw)

 999     continue
         if(ftstat.ne.0) call fcerrm(ftstat)
         if (inopen) then
            ftstat = 0
            call ftclos(iunit, ftstat)
         endif

 1000 continue
      return
      end

C***********************************************************************
C     SUBROITINE:
C     puphed
C
C     DESCRIPTION:
C     Pick up necessary GIS HK header keywords and make header rows for the
C     output file
C
C     AUTHOUR/DATE:
C     Ken Ebisawa 1993/03/17
C
C     MODIFICATION HISTORY:
C     Ken Ebisawa 1993/03/19 for frfread2.961
C     Ken Ebisawa 1993/05/18 for frfread2.980
C     Ken Ebisawa 2000/03/13 BITRATE,ORBITBEG,ORBITEND are not read.
C
C     NOTES:
C     It is assumed that the GIS HK files is already opened.
C
C     USAGE:
C     puphed(iunit, instrm, bitrat, datmod, head2, head3, ftstat)
C
C     ARGUMENTS:
C     iunit(in): unit of the HK file
C     instrm(in): Instrument name
C     bitrat(out): Bitrate
C     datmod(out): data mode
C     head2(out): The first output header row
C     head3(out): The first output header row
C     ftstat(out):     Error status
C
C     PRIMARY LOCAL VARIABLES:
C
C     CALLED ROUTINES:
C     subroutine ftgky[s,e] - pick up header keywords
C     subroutine fcerrm - output error message
C***********************************************************************

      subroutine puphed(iunit, instrm, bitrat, datmod, head2, head3,
     $     head4, ftstat)

C     implicit none
      integer iunit
      character(8) instrm, bitrat
      character*(*)  head2, head3, head4
      integer ftstat

      character(80) comment
      character(8) origin
      character(20) object
      character(18) piname, tlfile
      character(14) author
      real ra, dec
      character(68) date, datobs, timobs, datend, timend
      character(8) datmod

      call ftgkys(iunit, 'ORIGIN', origin, comment, ftstat)
      if (ftstat.ne.0) goto 999

      call ftgkys(iunit, 'OBJECT', object, comment, ftstat)
      if (ftstat.ne.0) goto 999

      call ftgkys(iunit, 'OBSERVER', piname, comment, ftstat)
      if (ftstat.ne.0) then
         ftstat = 0
         call ftgkys(iunit, 'SEQPI', piname, comment, ftstat)
         if (ftstat.ne.0) goto 999
      endif

      call ftgkye(iunit, 'RA_NOM', ra, comment, ftstat)
      if (ftstat.ne.0) then
         call fcecho('WARNING:RA_NOM not found in the primary.')
         ftstat = 0
      endif

      call ftgkye(iunit, 'DEC_NOM', dec, comment, ftstat)
      if (ftstat.ne.0) then
         call fcecho('WARNING:DEC_NOM not found in the primary.')
         ftstat = 0
      endif

      call ftgkys(iunit, 'DATE', date, comment, ftstat)
      if (ftstat.ne.0) goto 999

      call ftgkys(iunit, 'CREATOR', author, comment, ftstat)
      if (ftstat.ne.0) then
         ftstat = 0
         call ftgkys(iunit, 'AUTHOR', author, comment, ftstat)
         if (ftstat.ne.0) go to 999
      endif

      call ftgkys(iunit, 'TLM_FILE', tlfile, comment, ftstat)
      if (ftstat.ne.0) go to 999

      call ftgkys(iunit, 'DATE-OBS', datobs, comment, ftstat)
      if (ftstat.ne.0) go to 999

      call ftgkys(iunit, 'TIME-OBS', timobs, comment, ftstat)
      if (ftstat.ne.0) go to 999

      call ftgkys(iunit, 'DATE-END', datend, comment, ftstat)
      if (ftstat.ne.0) go to 999

      call ftgkys(iunit, 'TIME-END', timend, comment, ftstat)
      if (ftstat.ne.0) go to 999

C      call ftgkyj(iunit, 'NEVENTS', events, comment, ftstat)
C      if (ftstat.ne.0) go to 999

C      call ftgkys(iunit, 'DATAMODE', datmod, comment, ftstat)
C      if (ftstat.ne.0) go to 999

c     Write head2
c     head2='ORIGIN:'//origin//' PI:'//piname//
c     $     ' INSTM:'//instrm(1:4)//' MODE:'//datmod//'  BITR:'//bitrat(1:7)
c     $     //'  CREATION:'//date// ' '//tlfile//' '//author
      head2='ORIGIN:'//origin//' PI:'//piname//
     $     ' INSTM:'//instrm(1:4)//' TARGET:'//object


c     Write head3
c$$$      write(head3, '(a,1x,f6.2,a1,f6.2,
c$$$     $     1x, a, a, 1x, a, a, a, 1x, a,
c$$$     $     1x, a, f10.2, a, a, i8)')
c$$$     $     'POINTING:', ra, ',', dec,
c$$$     $     'START:', datobs, timobs, ' END:', datend, timend,
c$$$     $     'EXPOSURE:', expsre, ' sec', ' NEVENTS:', events

      write(head3, '(a,1x,f6.2,a1,f6.2,1x,
     $     a, a, 1x, a, a, a, 1x, a,
     $     1x, a, i8)')
     $     'POINTING:', ra, ',', dec,
     $     'START:', datobs(1:10), timobs(1:8), ' END:',
     $     datend(1:10), timend(1:8)

c     Write head4
      head4 = 'CREATION:'//date(1:19)//' FRF:'//tlfile//
     $     ' AUTHOR:'//author

 999  continue
      if(ftstat.ne.0) call fcerrm(ftstat)
      return
      end


C***********************************************************************
C     SUBROITINE:
C     getint
C
C     DESCRIPTION:
C     Output initial GIS HK parameters
C
C     AUTHOUR/DATE:
C     Ken Ebisawa 1993/03/17
C
C     MODIFICATION HISTORY:
C     Completely rewritten.
C     Read modal paramters from modal and modal2
C     Ken Ebisawa 1993/05/18 for FRFread2.980
C     Koji Mukai 2007/05/23 (added modal_change as argument)
C
C     NOTES:
C
C     USAGE:
C     getint(iunit, ounit, outopn, instrm, datmode, nrows, modal_change
C     head1, head2, head3, head4,
C     head5, head6, head7, L1SD, LDHTSD, M2SD, XRAYSD, ODDSD, ftstat)
C
C     ARGUMENTS:
C     iunit  - unit on which GIS HK file is open
C     ounit  - unit of the output file
C     outopn - output file (yes) or terminal (no)
C     instrm - instrument name
C     datmod - data mode
C     nrows  - number of rows in the HK file
C     modal_change - if true, print out message about modal changes
C     head1 - (output) headers with initial modal parameters
C     head2 - (output) headers with initial modal parameters
C     head3 - (output) headers with initial modal parameters
C     head4 - (output) headers with initial modal parameters
C     head5 - (output) headers with initial modal parameters
C     head6 - (output) headers with initial modal parameters
C     head7 - (output) headers with initial modal parameters
C     L1SD  - (output) L1 scale down (0, 1, 2, 3; 999 if error)
C     LDHTSD- (output) LDhit scale down (0, 1, 2, 3; 999 if error)
C     M2SD  - (output) M2 scale down (0,1,2,3,4,5,6,7,8)
C     XRAYSD- (output) X-ray  scale down (0,1,2,3,4,5,6,7,8)
C     ODDSD - (output) 1DD scale down (0,1,2,3,4,5,6,7,8)
C     ftstat - status of fits i/o
C
C     PRIMARY LOCAL VARIABLES:
C
C     CALLED ROUTINES:
C
C***********************************************************************
      subroutine getint(iunit, ounit, outopn, instrm, datmod, nrows,
     $     modal_change, head1, head2, head3,
     $     head4, head5, head6, head7, L1SD, LDHTSD,
     $     M2SD, XRAYSD, ODDSD, ftstat)

C     implicit none
      integer iunit, ounit, nrows,
     $     L1SD, LDHTSD, M2SD, XRAYSD, ODDSD, ftstat
      logical outopn, modal_change
      character(8) instrm, datmod
      character*(*)  head1, head2, head3, head4, head5, head6, head7

C     To print characters.
      character(80) context

C     To use fitsio reading HK items from a HK file.
      logical anyf
      integer value
      double precision time

C     Used for the do loops
      integer i, j

C     HK item, GIS
      character(8) item

C     Modal is the array of modal HK prameters which are in the fits HK file
c     and to be processed for the current GIS mode and sensor.
C     This is made from the character array 'HKitem' according to the GIS mode
c     and sensor selection.
c     Modptr is the array of the pointer to the items written in the header of
c     the file. Nmodal is the number of the arrays in modal.
C     Modset is a flag to see whether modal parameters have appeared(1) in the
C     file or not (0). If modal parameters do not appear, default values read
C     from the integer array 'default' are adoped.
      character(8) modal(160)
      integer     modptr(160), modset(160), nmodal

C     Modval is values of the modal parameters read from the HK file and
C     written in the header.
C     Numpar is the number of these parameters (150 for GHKDUMP ver 2.1).
      integer modval(160),  numpar

C     Common block for HK parameter names, and default values when the
C     parameters are not found.
      common/modHK/HKitem, default
      character(8) HKitem(4, 271)
      integer default(2,150)

C     Used for display purpose.

      character(3) off_on(0:2)
      character(4) runstp(0:2)
C     character(3) condis(0:2)
      character(4) posdet(0:2)
      character(3) orand(0:2)
      character(3) out_in(0:2)

      data off_on /'OFF', 'ON ', '***'/
      data runstp /'STOP', 'RUN ', '****'/
C     data condis /'CON', 'DIS', '***'/
      data posdet /'POW2', 'FLF ', '****'/
      data orand  /'OR ', 'AND', '***'/
      data out_in /'OUT', 'IN ', '***'/

      character(9) gismod

C*************************************************************************
C     The old external block data "ghkblk" was moved here to make
C     gfortran happy and to ensure it gets initialized.
C
C     Block Data to specify GIS HK modal parameters and default values of
C     these parameters.
C     Ken Ebisawa 1993/05/20
C     In the ver 2.0 of GHKDUMP, these are read from external files modal and
C     modal2.

C     Modified for FRFread 3.000. Nov 2, 93.
C     G2/3PHBIT, XPBIT, YPBIT, RTBIT, SPBIT appear only for that
C     single GIS.
C     Consequently, HK parameter number 29-33 are not used any longer.

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 1, 13) /
     $  'GIS2_POW',  'GIS2',  '  ALL',  '  1',
     $  'GIS3_POW',  'GIS3',  '  ALL',  '  1',
     $  'GIS_BYP ',  'BOTH',  '  ALL',  '  2',
     $  'GIS_AN  ',  'BOTH',  '  ALL',  '  3',
     $  'HVL2_POW',  'GIS2',  '  ALL',  '  4',
     $  'HVH2_POW',  'GIS2',  '  ALL',  '  5',
     $  'HVL3_POW',  'GIS3',  '  ALL',  '  4',
     $  'HVH3_POW',  'GIS3',  '  ALL',  '  5',
     $  'GHV_RED ',  'BOTH',  '  ALL',  '  6',
     $  'HVL2_LVL',  'GIS2',  '  ALL',  '  7',
     $  'HVH2_LVL',  'GIS2',  '  ALL',  '  8',
     $  'HVL3_LVL',  'GIS3',  '  ALL',  '  7',
     $  'HVH3_LVL',  'GIS3',  '  ALL',  '  8' /

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 14, 18) /
     $  'RBM_POW ',  'BOTH',  '  ALL',  '  9',
     $  'G_RBM_A ',  'BOTH',  '  ALL',  ' 10',
     $  'G_RBM_FL',  'BOTH',  '  ALL',  ' 11',
     $  'G_RBM_MF',  'BOTH',  '  ALL',  ' 12',
     $  'RBM_LDL ',  'BOTH',  '  ALL',  ' 13' /

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 19, 23) /
     $  'MEM_CHK ',  'BOTH',  '  ALL',  ' 14',
     $  'MPC     ',  'BOTH',  '  ALL',  ' 15',
     $  'PCAL    ',  'BOTH',  '  ALL',  ' 16',
     $  'G2_FN_GR',  'GIS2',  '  ALL',  ' 17',
     $  'G3_FN_GR',  'GIS3',  '  ALL',  ' 17'/

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 24, 33) /
     $  'G2_RT_LO',  'GIS2',  '  ALL',  ' 18',
     $  'G3_RT_LO',  'GIS3',  '  ALL',  ' 18',
     $  'G2_RT_HI',  'GIS2',  '  ALL',  ' 19',
     $  'G3_RT_HI',  'GIS3',  '  ALL',  ' 19',
     $  'G2_LE_DS',  'GIS2',  '  ALL',  ' 20',
     $  'G3_LE_DS',  'GIS3',  '  ALL',  ' 20',
     $  'G2_SD_L1',  'GIS2',  '  ALL',  ' 21',
     $  'G2_SD_LD',  'GIS2',  '  ALL',  ' 22',
     $  'G3_SD_L1',  'GIS3',  '  ALL',  ' 21',
     $  'G3_SD_LD',  'GIS3',  '  ALL',  ' 22' /

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 34, 44) /
     $  'MPC_BIT ',  'BOTH',  '  ALL',  ' 23',
     $  'G2_PHBIT',  'GIS2',  'NOMEM',  ' 24',
     $  'G3_PHBIT',  'GIS3',  'NOMEM',  ' 24',
     $  'G2_XPBIT',  'GIS2',  'NOMEM',  ' 25',
     $  'G3_XPBIT',  'GIS3',  'NOMEM',  ' 25',
     $  'G2_YPBIT',  'GIS2',  'NOMEM',  ' 26',
     $  'G3_YPBIT',  'GIS3',  'NOMEM',  ' 26',
     $  'G2_RTBIT',  'GIS2',  'NOMEM',  ' 27',
     $  'G3_RTBIT',  'GIS3',  'NOMEM',  ' 27',
     $  'G2_SPBIT',  'GIS2',  'NOMEM',  ' 28',
     $  'G3_SPBIT',  'GIS3',  'NOMEM',  ' 28' /

        data  ( ( HKitem(i, j), i = 1, 4), j = 45, 49) /
     $  'C2_RN_ST',  'BOTH',  '  ALL',  ' 34',
     $  'C3_RN_ST',  'BOTH',  '  ALL',  ' 35',
     $  'CPU_GIS2',  'BOTH',  '  ALL',  ' 36',
     $  'CPU_GIS3',  'BOTH',  '  ALL',  ' 37',
     $  'CPU_DISC',  'BOTH',  '  ALL',  ' 38' /

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 50, 63) /
     $  'G2_RS_CD',  'GIS2',  'NOMEM',  ' 39',
     $  'G3_RS_CD',  'GIS3',  'NOMEM',  ' 39',
     $  'G2_A_SEL',  'GIS2',  'NOMEM',  ' 40',
     $  'G3_A_SEL',  'GIS3',  'NOMEM',  ' 40',
     $  'G2_A_TUN',  'GIS2',  'NOMEM',  ' 41',
     $  'G3_A_TUN',  'GIS3',  'NOMEM',  ' 41',
     $  'G2_P_DET',  'GIS2',  'NOMEM',  ' 42',
     $  'G3_P_DET',  'GIS3',  'NOMEM',  ' 42',
     $  'G2_RA_DS',  'GIS2',  'NOMEM',  ' 43',
     $  'G3_RA_DS',  'GIS3',  'NOMEM',  ' 43',
     $  'G2_SP_DS',  'GIS2',  'NOMEM',  ' 44',
     $  'G3_SP_DS',  'GIS3',  'NOMEM',  ' 44',
     $  'G2_PHTUN',  'GIS2',  'NOMEM',  ' 45',
     $  'G3_PHTUN',  'GIS3',  'NOMEM',  ' 45' /

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 64, 71) /
     $  'G2_MASK1',  'GIS2',  'NOMEM',  ' 46',
     $  'G3_MASK1',  'GIS3',  'NOMEM',  ' 46',
     $  'G2_M1_LG',  'GIS2',  'NOMEM',  ' 47',
     $  'G3_M1_LG',  'GIS3',  'NOMEM',  ' 47',
     $  'G2_MASK2',  'GIS2',  'NOMEM',  ' 48',
     $  'G3_MASK2',  'GIS3',  'NOMEM',  ' 48',
     $  'G2_M2_RG',  'GIS2',  'NOMEM',  ' 49',
     $  'G3_M2_RG',  'GIS3',  'NOMEM',  ' 49' /

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 72, 87) /
     $  'G2P_ANFF',  'GIS2',  'NOMEM',  ' 50',
     $  'G3P_ANFF',  'GIS3',  'NOMEM',  ' 50',
     $  'G2PANAL0',  'GIS2',  'NOMEM',  ' 51',
     $  'G3PANAL0',  'GIS3',  'NOMEM',  ' 51',
     $  'G2PEDGEV',  'GIS2',  'NOMEM',  ' 52',
     $  'G3PEDGEV',  'GIS3',  'NOMEM',  ' 52',
     $  'G2P_M1EV',  'GIS2',  'NOMEM',  ' 53',
     $  'G3P_M1EV',  'GIS3',  'NOMEM',  ' 53',
     $  'G2PCLCOU',  'GIS2',  'NOMEM',  ' 54',
     $  'G3PCLCOU',  'GIS3',  'NOMEM',  ' 54',
     $  'G2PDSCOU',  'GIS2',  'NOMEM',  ' 55',
     $  'G3PDSCOU',  'GIS3',  'NOMEM',  ' 55',
     $  'G2P_M2EV',  'GIS2',  'NOMEM',  ' 56',
     $  'G3P_M2EV',  'GIS3',  'NOMEM',  ' 56',
     $  'G2P_XREV',  'GIS2',  'NOMEM',  ' 57',
     $  'G3P_XREV',  'GIS3',  'NOMEM',  ' 57'/

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 88, 103) /
     $  'G2_S_X0 ',  'GIS2',  'NOMEM',  ' 58',
     $  'G2_S_X1 ',  'GIS2',  'NOMEM',  ' 59',
     $  'G2_S_X2 ',  'GIS2',  'NOMEM',  ' 60',
     $  'G2_S_X3 ',  'GIS2',  'NOMEM',  ' 61',
     $  'G2_S_X4 ',  'GIS2',  'NOMEM',  ' 62',
     $  'G2_S_X5 ',  'GIS2',  'NOMEM',  ' 63',
     $  'G2_S_X6 ',  'GIS2',  'NOMEM',  ' 64',
     $  'G2_S_X7 ',  'GIS2',  'NOMEM',  ' 65',
     $  'G2_S_X8 ',  'GIS2',  'NOMEM',  ' 66',
     $  'G2_S_X9 ',  'GIS2',  'NOMEM',  ' 67',
     $  'G2_S_X10',  'GIS2',  'NOMEM',  ' 68',
     $  'G2_S_X11',  'GIS2',  'NOMEM',  ' 69',
     $  'G2_S_X12',  'GIS2',  'NOMEM',  ' 70',
     $  'G2_S_X13',  'GIS2',  'NOMEM',  ' 71',
     $  'G2_S_X14',  'GIS2',  'NOMEM',  ' 72',
     $  'G2_S_X15',  'GIS2',  'NOMEM',  ' 73'/

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 104, 119) /
     $  'G2_S_Y0 ',  'GIS2',  'NOMEM',  ' 74',
     $  'G2_S_Y1 ',  'GIS2',  'NOMEM',  ' 75',
     $  'G2_S_Y2 ',  'GIS2',  'NOMEM',  ' 76',
     $  'G2_S_Y3 ',  'GIS2',  'NOMEM',  ' 77',
     $  'G2_S_Y4 ',  'GIS2',  'NOMEM',  ' 78',
     $  'G2_S_Y5 ',  'GIS2',  'NOMEM',  ' 79',
     $  'G2_S_Y6 ',  'GIS2',  'NOMEM',  ' 80',
     $  'G2_S_Y7 ',  'GIS2',  'NOMEM',  ' 81',
     $  'G2_S_Y8 ',  'GIS2',  'NOMEM',  ' 82',
     $  'G2_S_Y9 ',  'GIS2',  'NOMEM',  ' 83',
     $  'G2_S_Y10',  'GIS2',  'NOMEM',  ' 84',
     $  'G2_S_Y11',  'GIS2',  'NOMEM',  ' 85',
     $  'G2_S_Y12',  'GIS2',  'NOMEM',  ' 86',
     $  'G2_S_Y13',  'GIS2',  'NOMEM',  ' 87',
     $  'G2_S_Y14',  'GIS2',  'NOMEM',  ' 88',
     $  'G2_S_Y15',  'GIS2',  'NOMEM',  ' 89' /

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 120, 135) /
     $  'G3_S_X0 ',  'GIS3',  'NOMEM',  ' 58',
     $  'G3_S_X1 ',  'GIS3',  'NOMEM',  ' 59',
     $  'G3_S_X2 ',  'GIS3',  'NOMEM',  ' 60',
     $  'G3_S_X3 ',  'GIS3',  'NOMEM',  ' 61',
     $  'G3_S_X4 ',  'GIS3',  'NOMEM',  ' 62',
     $  'G3_S_X5 ',  'GIS3',  'NOMEM',  ' 63',
     $  'G3_S_X6 ',  'GIS3',  'NOMEM',  ' 64',
     $  'G3_S_X7 ',  'GIS3',  'NOMEM',  ' 65',
     $  'G3_S_X8 ',  'GIS3',  'NOMEM',  ' 66',
     $  'G3_S_X9 ',  'GIS3',  'NOMEM',  ' 67',
     $  'G3_S_X10',  'GIS3',  'NOMEM',  ' 68',
     $  'G3_S_X11',  'GIS3',  'NOMEM',  ' 69',
     $  'G3_S_X12',  'GIS3',  'NOMEM',  ' 70',
     $  'G3_S_X13',  'GIS3',  'NOMEM',  ' 71',
     $  'G3_S_X14',  'GIS3',  'NOMEM',  ' 72',
     $  'G3_S_X15',  'GIS3',  'NOMEM',  ' 73'/

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 136, 151) /
     $  'G3_S_Y0 ',  'GIS3',  'NOMEM',  ' 74',
     $  'G3_S_Y1 ',  'GIS3',  'NOMEM',  ' 75',
     $  'G3_S_Y2 ',  'GIS3',  'NOMEM',  ' 76',
     $  'G3_S_Y3 ',  'GIS3',  'NOMEM',  ' 77',
     $  'G3_S_Y4 ',  'GIS3',  'NOMEM',  ' 78',
     $  'G3_S_Y5 ',  'GIS3',  'NOMEM',  ' 79',
     $  'G3_S_Y6 ',  'GIS3',  'NOMEM',  ' 80',
     $  'G3_S_Y7 ',  'GIS3',  'NOMEM',  ' 81',
     $  'G3_S_Y8 ',  'GIS3',  'NOMEM',  ' 82',
     $  'G3_S_Y9 ',  'GIS3',  'NOMEM',  ' 83',
     $  'G3_S_Y10',  'GIS3',  'NOMEM',  ' 84',
     $  'G3_S_Y11',  'GIS3',  'NOMEM',  ' 85',
     $  'G3_S_Y12',  'GIS3',  'NOMEM',  ' 86',
     $  'G3_S_Y13',  'GIS3',  'NOMEM',  ' 87',
     $  'G3_S_Y14',  'GIS3',  'NOMEM',  ' 88',
     $  'G3_S_Y15',  'GIS3',  'NOMEM',  ' 89'/

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 152, 167) /
     $  'G2_M_X0 ',  'GIS2',  'NOMEM',  ' 90',
     $  'G2_M_X1 ',  'GIS2',  'NOMEM',  ' 91',
     $  'G2_M_X2 ',  'GIS2',  'NOMEM',  ' 92',
     $  'G2_M_X3 ',  'GIS2',  'NOMEM',  ' 93',
     $  'G2_M_X4 ',  'GIS2',  'NOMEM',  ' 94',
     $  'G2_M_X5 ',  'GIS2',  'NOMEM',  ' 95',
     $  'G2_M_X6 ',  'GIS2',  'NOMEM',  ' 96',
     $  'G2_M_X7 ',  'GIS2',  'NOMEM',  ' 97',
     $  'G2_M_X8 ',  'GIS2',  'NOMEM',  ' 98',
     $  'G2_M_X9 ',  'GIS2',  'NOMEM',  ' 99',
     $  'G2_M_X10',  'GIS2',  'NOMEM',  '100',
     $  'G2_M_X11',  'GIS2',  'NOMEM',  '101',
     $  'G2_M_X12',  'GIS2',  'NOMEM',  '102',
     $  'G2_M_X13',  'GIS2',  'NOMEM',  '103',
     $  'G2_M_X14',  'GIS2',  'NOMEM',  '104',
     $  'G2_M_X15',  'GIS2',  'NOMEM',  '105'/

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 168, 183) /
     $  'G2_M_Y0 ',  'GIS2',  'NOMEM',  '106',
     $  'G2_M_Y1 ',  'GIS2',  'NOMEM',  '107',
     $  'G2_M_Y2 ',  'GIS2',  'NOMEM',  '108',
     $  'G2_M_Y3 ',  'GIS2',  'NOMEM',  '109',
     $  'G2_M_Y4 ',  'GIS2',  'NOMEM',  '110',
     $  'G2_M_Y5 ',  'GIS2',  'NOMEM',  '111',
     $  'G2_M_Y6 ',  'GIS2',  'NOMEM',  '112',
     $  'G2_M_Y7 ',  'GIS2',  'NOMEM',  '113',
     $  'G2_M_Y8 ',  'GIS2',  'NOMEM',  '114',
     $  'G2_M_Y9 ',  'GIS2',  'NOMEM',  '115',
     $  'G2_M_Y10',  'GIS2',  'NOMEM',  '116',
     $  'G2_M_Y11',  'GIS2',  'NOMEM',  '117',
     $  'G2_M_Y12',  'GIS2',  'NOMEM',  '118',
     $  'G2_M_Y13',  'GIS2',  'NOMEM',  '119',
     $  'G2_M_Y14',  'GIS2',  'NOMEM',  '120',
     $  'G2_M_Y15',  'GIS2',  'NOMEM',  '121'/

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 184, 199) /
     $  'G3_M_X0 ',  'GIS3',  'NOMEM',  ' 90',
     $  'G3_M_X1 ',  'GIS3',  'NOMEM',  ' 91',
     $  'G3_M_X2 ',  'GIS3',  'NOMEM',  ' 92',
     $  'G3_M_X3 ',  'GIS3',  'NOMEM',  ' 93',
     $  'G3_M_X4 ',  'GIS3',  'NOMEM',  ' 94',
     $  'G3_M_X5 ',  'GIS3',  'NOMEM',  ' 95',
     $  'G3_M_X6 ',  'GIS3',  'NOMEM',  ' 96',
     $  'G3_M_X7 ',  'GIS3',  'NOMEM',  ' 97',
     $  'G3_M_X8 ',  'GIS3',  'NOMEM',  ' 98',
     $  'G3_M_X9 ',  'GIS3',  'NOMEM',  ' 99',
     $  'G3_M_X10',  'GIS3',  'NOMEM',  '100',
     $  'G3_M_X11',  'GIS3',  'NOMEM',  '101',
     $  'G3_M_X12',  'GIS3',  'NOMEM',  '102',
     $  'G3_M_X13',  'GIS3',  'NOMEM',  '103',
     $  'G3_M_X14',  'GIS3',  'NOMEM',  '104',
     $  'G3_M_X15',  'GIS3',  'NOMEM',  '105'/

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 200, 215) /
     $  'G3_M_Y0 ',  'GIS3',  'NOMEM',  '106',
     $  'G3_M_Y1 ',  'GIS3',  'NOMEM',  '107',
     $  'G3_M_Y2 ',  'GIS3',  'NOMEM',  '108',
     $  'G3_M_Y3 ',  'GIS3',  'NOMEM',  '109',
     $  'G3_M_Y4 ',  'GIS3',  'NOMEM',  '110',
     $  'G3_M_Y5 ',  'GIS3',  'NOMEM',  '111',
     $  'G3_M_Y6 ',  'GIS3',  'NOMEM',  '112',
     $  'G3_M_Y7 ',  'GIS3',  'NOMEM',  '113',
     $  'G3_M_Y8 ',  'GIS3',  'NOMEM',  '114',
     $  'G3_M_Y9 ',  'GIS3',  'NOMEM',  '115',
     $  'G3_M_Y10',  'GIS3',  'NOMEM',  '116',
     $  'G3_M_Y11',  'GIS3',  'NOMEM',  '117',
     $  'G3_M_Y12',  'GIS3',  'NOMEM',  '118',
     $  'G3_M_Y13',  'GIS3',  'NOMEM',  '119',
     $  'G3_M_Y14',  'GIS3',  'NOMEM',  '120',
     $  'G3_M_Y15',  'GIS3',  'NOMEM',  '121'/

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 216, 223) /
     $  'G2_CNX_P',  'GIS2',  'NOMEM',  '122',
     $  'G3_CNX_P',  'GIS3',  'NOMEM',  '122',
     $  'G2_CNY_P',  'GIS2',  'NOMEM',  '123',
     $  'G3_CNY_P',  'GIS3',  'NOMEM',  '123',
     $  'G2_CNX_F',  'GIS2',  'NOMEM',  '124',
     $  'G3_CNX_F',  'GIS3',  'NOMEM',  '124',
     $  'G2_CNY_F',  'GIS2',  'NOMEM',  '125',
     $  'G3_CNY_F',  'GIS3',  'NOMEM',  '125' /

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 224, 235) /
     $  'G2_SLDAP',  'GIS2',  'NOMEM',  '126',
     $  'G3_SLDAP',  'GIS3',  'NOMEM',  '126',
     $  'G2_SUDAP',  'GIS2',  'NOMEM',  '127',
     $  'G3_SUDAP',  'GIS3',  'NOMEM',  '127',
     $  'G2_SD_BP',  'GIS2',  'NOMEM',  '128',
     $  'G3_SD_BP',  'GIS3',  'NOMEM',  '128',
     $  'G2_SD_CP',  'GIS2',  'NOMEM',  '129',
     $  'G3_SD_CP',  'GIS3',  'NOMEM',  '129',
     $  'G2_SDH2P',  'GIS2',  'NOMEM',  '130',
     $  'G3_SDH2P',  'GIS3',  'NOMEM',  '130',
     $  'G2_SDL8P',  'GIS2',  'NOMEM',  '131',
     $  'G3_SDL8P',  'GIS3',  'NOMEM',  '131'/

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 236, 247) /
     $  'G2_SLDAF',  'GIS2',  'NOMEM',  '132',
     $  'G3_SLDAF',  'GIS3',  'NOMEM',  '132',
     $  'G2_SUDAF',  'GIS2',  'NOMEM',  '133',
     $  'G3_SUDAF',  'GIS3',  'NOMEM',  '133',
     $  'G2_SD_BF',  'GIS2',  'NOMEM',  '134',
     $  'G3_SD_BF',  'GIS3',  'NOMEM',  '134',
     $  'G2_SD_CF',  'GIS2',  'NOMEM',  '135',
     $  'G3_SD_CF',  'GIS3',  'NOMEM',  '135',
     $  'G2_SDH2F',  'GIS2',  'NOMEM',  '136',
     $  'G3_SDH2F',  'GIS3',  'NOMEM',  '136',
     $  'G2_SDL8F',  'GIS2',  'NOMEM',  '137',
     $  'G3_SDL8F',  'GIS3',  'NOMEM',  '137' /

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 248, 259) /
     $  'G2_RD_P ',  'GIS2',  'NOMEM',  '138',
     $  'G3_RD_P ',  'GIS3',  'NOMEM',  '138',
     $  'G2_RD_F ',  'GIS2',  'NOMEM',  '139',
     $  'G3_RD_F ',  'GIS3',  'NOMEM',  '139',
     $  'G2_M_XL ',  'GIS2',  'NOMEM',  '140',
     $  'G3_M_XL ',  'GIS3',  'NOMEM',  '140',
     $  'G2_M_XU ',  'GIS2',  'NOMEM',  '141',
     $  'G3_M_XU ',  'GIS3',  'NOMEM',  '141',
     $  'G2_M_YL ',  'GIS2',  'NOMEM',  '142',
     $  'G3_M_YL ',  'GIS3',  'NOMEM',  '142',
     $  'G2_M_YU ',  'GIS2',  'NOMEM',  '143',
     $  'G3_M_YU ',  'GIS3',  'NOMEM',  '143'/

        data  ( ( HKitem(i, j), i = 1, 4 ), j = 260, 271) /
     $  'G2_M2ESD',  'GIS2',  'NOMEM',  '144',
     $  'G3_M2ESD',  'GIS3',  'NOMEM',  '144',
     $  'G2_XRESD',  'GIS2',  'NOMEM',  '145',
     $  'G3_XRESD',  'GIS3',  'NOMEM',  '145',
     $  'G2_1DDSD',  'GIS2',  'NOMEM',  '146',
     $  'G3_1DDSD',  'GIS3',  'NOMEM',  '146',
     $  'C2_PGVER',  'BOTH',  'NOMEM',  '147',
     $  'C3_PGVER',  'BOTH',  'NOMEM',  '148',
     $  'G2_TBL21',  'GIS2',  'NOMEM',  '149',
     $  'G3_TBL21',  'GIS3',  'NOMEM',  '149',
     $  'G2_TBL22',  'GIS2',  'NOMEM',  '150',
     $  'G3_TBL22',  'GIS3',  'NOMEM',  '150'  /

        data ( ( default(i,j), i = 1, 2 ), j = 1, 10)/
     $     1,   2,
     $     2,   2,
     $     3,   2,
     $     4,   2,
     $     5,   2,
     $     6,   2,
     $     7, 999,
     $     8, 999,
     $     9,   2,
     $    10,   2/

        data ( ( default(i,j), i = 1, 2 ), j = 11, 20)/
     $    11, 999,
     $    12,   2,
     $    13, 999,
     $    14,   2,
     $    15,   2,
     $    16,   2,
     $    17, 999,
     $    18, 999,
     $    19, 999,
     $    20, 999/

        data ( ( default(i,j), i = 1, 2 ), j = 21, 28)/
     $    21, 999,
     $    22, 999,
     $    23, 999,
     $    24,   5,
     $    25,   5,
     $    26,   5,
     $    27, 999,
     $    28, 999/

        data ( ( default(i,j), i = 1, 2 ), j = 34, 40)/
     $    34,   2,
     $    35,   2,
     $    36, 999,
     $    37, 999,
     $    38,   2,
     $    39,  17,
     $    40,   2/

        data ( ( default(i,j), i = 1, 2 ), j = 41, 50)/
     $    41,   2,
     $    42,   2,
     $    43,   2,
     $    44,   2,
     $    45,   2,
     $    46,   2,
     $    47,   2,
     $    48,   2,
     $    49,   2,
     $    50, 999/

        data ( ( default(i,j), i = 1, 2 ), j = 51, 60)/
     $    51, 999,
     $    52, 999,
     $    53, 999,
     $    54, 999,
     $    55, 999,
     $    56, 999,
     $    57, 999,
     $    58, 999,
     $    59, 999,
     $    60, 999/

        data ( ( default(i,j), i = 1, 2 ), j = 61, 70)/
     $    61, 999,
     $    62, 999,
     $    63, 999,
     $    64, 999,
     $    65, 999,
     $    66, 999,
     $    67, 999,
     $    68, 999,
     $    69, 999,
     $    70, 999/

        data ( ( default(i,j), i = 1, 2 ), j = 71, 80)/
     $    71, 999,
     $    72, 999,
     $    73, 999,
     $    74, 999,
     $    75, 999,
     $    76, 999,
     $    77, 999,
     $    78, 999,
     $    79, 999,
     $    80, 999/

        data ( ( default(i,j), i = 1, 2 ), j = 81, 90)/
     $    81, 999,
     $    82, 999,
     $    83, 999,
     $    84, 999,
     $    85, 999,
     $    86, 999,
     $    87, 999,
     $    88, 999,
     $    89, 999,
     $    90, 999/

        data ( ( default(i,j), i = 1, 2 ), j = 91, 100)/
     $    91, 999,
     $    92, 999,
     $    93, 999,
     $    94, 999,
     $    95, 999,
     $    96, 999,
     $    97, 999,
     $    98, 999,
     $    99, 999,
     $   100, 999/

        data ( ( default(i,j), i = 1, 2 ), j = 101, 110)/
     $   101, 999,
     $   102, 999,
     $   103, 999,
     $   104, 999,
     $   105, 999,
     $   106, 999,
     $   107, 999,
     $   108, 999,
     $   109, 999,
     $   110, 999/

        data ( ( default(i,j), i = 1, 2 ), j = 111, 120)/
     $   111, 999,
     $   112, 999,
     $   113, 999,
     $   114, 999,
     $   115, 999,
     $   116, 999,
     $   117, 999,
     $   118, 999,
     $   119, 999,
     $   120, 999/

        data ( ( default(i,j), i = 1, 2 ), j = 121, 130)/
     $   121, 999,
     $   122,9999,
     $   123,9999,
     $   124,9999,
     $   125,9999,
     $   126,9999,
     $   127,9999,
     $   128,9999,
     $   129,9999,
     $   130,9999/

        data ( ( default(i,j), i = 1, 2 ), j = 131, 140)/
     $   131,9999,
     $   132,9999,
     $   133,9999,
     $   134,9999,
     $   135,9999,
     $   136,9999,
     $   137,9999,
     $   138,9999,
     $   139,9999,
     $   140,9999/

        data ( ( default(i,j), i = 1, 2 ), j = 141, 150)/
     $   141,9999,
     $   142,9999,
     $   143,9999,
     $   144,  17,
     $   145,  17,
     $   146,  17,
     $   147, 999,
     $   148, 999,
     $   149, 999,
     $   150, 999/

C     End of variable declaration

C     Create modal parameters array modal(i) and pointer array modptr(i).

      nmodal = 0
      do 80 i = 1, 271
         if(
     $        ( (HKitem(2, i).eq.'BOTH').or.
     $        (HKitem(2, i).eq.instrm(1:4)) )
     $        .and.
     $        (  HKitem(3,i).eq.'  ALL'.or.
     $        ( (HKitem(3,i).eq.'NOMEM').and.(datmod(1:3).ne.'MEM') ) )
     $        ) then
            nmodal = nmodal + 1
            modal(nmodal) = HKitem(1,i)
            read(HKitem(4,i), '(i3)') modptr(nmodal)
C     write(*,*) i, nmodal, modal(nmodal), modptr(nmodal)
         endif
 80   continue

C     Nmodal is the number of the modal parameters for the current GIS
C     mode and sensor (150 for PH/PCAL/MPC mode and 28 for Mem_check mode).

C     Numpar is the number of modal HK items to be written in the header.
C     Currently (for GHKDUMP ver 2.1), it is 150.
      numpar = 150

C     Set default values.
      do 90 i = 1, numpar
         modval(i) = default(2, i)
C     write(*,*) i, modval(i)
 90   continue

C     Initialization
      do 30 i = 1, nmodal
C     Since FRFread3.000, PHBIT, XPBIT, YPBIT, RTBIT, and SPBIT appear
C     only for that one of the two GISs.  Therefore HK numbers 29 to 33
C     are not used any longer.
         if(i.ge.29.and.i.le.33) then
            modset(i) = 1
         else
            modset(i) = 0
         endif
 30   continue

C     Read the data from the HK file
      do 100 i = 1, nrows
         call ftgcvs(iunit, 1, i, 1, 1, ' ',
     $        item, anyf, ftstat)
         if(ftstat.ne.0) go to 999
         call ftgcvj(iunit, 2, i, 1, 1, 0,
     $        value, anyf, ftstat)
         if(ftstat.ne.0) go to 999
         call ftgcvd(iunit, 3, i, 1, 1, 0.0d0,
     $        time, anyf, ftstat)
         if(ftstat.ne.0) go to 999

c$$$C If ENDINIT2 appears, all the modal parameters have appeared.
c$$$  if(item.eq.'ENDINIT2') go to 111

C     Get the modal parameter values
         do 200 j = 1, nmodal
            if(item.eq.modal(j)) then
               if(modset(j).eq.1 .and. modal_change ) then
                  call prtchg(ounit, outopn, item, time, value)
               end if
               modset(j) = 1
               modval(modptr(j)) = value
            endif
 200     continue
 100  continue

C     Jump to here if ENDINIT2 appeared
 111  continue

C     Warning if modal parameters did not appear.
      do 300 i = 1, nmodal
         if(modset(i).eq.0) then
            write(context, '(a,a,a)')
     $           'Warning: ', modal(i), ' did not appear.'
            call fprint(ounit, outopn, context)
         endif
 300  continue

c     Output all the GIS status (modal) parameters

c     Set Mode
      if (modval(14).eq.1) then
         gismod = 'MEM_CHECK'
      elseif(modval(14).eq.0.and.modval(15).eq.1) then
         gismod = 'MODE:MPC'
      elseif(modval(14).eq.0.and.modval(15).eq.0.and.
     $        modval(16).eq.0) then
         gismod = 'MODE: PH '
      elseif(modval(14).eq.0.and.modval(15).eq.0.and.
     $        modval(16).eq.1) then
         gismod = 'MODE:PCAL'
      else
         gismod = 'MODE:????'
      endif

c     write header1
      write(head1, '(5(a,a,1x), 2(a,i1,1x),
     $     a,a,1x,a,1x, a, i2, 1x,
     $     2(a, i3, 1x), a, i3)')
     $     'POWER:', off_on(modval(1)),
     $     'BYPASS:', off_on(modval(2)),
     $     'APOW:', off_on(modval(3)),'HVH:', off_on(modval(5)),
     $     'HVL:', off_on(modval(4)),
     $     'H_LVL:', modval(8),'L_LVL:', modval(7),
     $     'HV_RED:', off_on(modval(6)), gismod, 'GAIN:', modval(17),
     $     'E_LD:', modval(20), 'RT_LD:', modval(18), 'RT_UD:',
     $     modval(19)

c     write header2 (cpu, anode, pos-det, r_disri, s_discr,ph..)
      write(head2,
     $     '(a, a, 1x, a, a, 1x,
     $     a, i1, 1x, a,  i1, 1x
     $     a, a, 1x, a, a, 1x,
     $     a, a, 1x, a, a, 1x,
     $     a, a, 1x, a, a, 1x,
     $     a, a, 1x, a, i3)')
     $     'C2:', runstp(modval(34)), 'C3:', runstp(modval(35)),
     $     'G2:C',modval(36)+2, 'G3:C',3-modval(37),
     $     'DSCN:',off_on(modval(38)), 'AN_USE:', off_on(modval(40)),
     $     'AN_TUNE:', off_on(modval(41)),
     $     'P_DET:', posdet(modval(42)),
     $     'R_DSCR:', off_on(modval(43)),
     $     'S_DSCR:', off_on(modval(44)),
     $     'PH_TUNE:', off_on(modval(45)), 'MPC_PH_BIT:',
     $     2**(2+2*modval(23))

c     write header3(CPU info)
      write(head3, '(a, i4, a, i4, 1x,
     $     2(a, i3, a, i3, 1x),
     $     2(a, i3, a, i3, 1x),
     $     a, 8i1, 1x
     $     a, i1, 1x a, i1)')
     $     'PH_MODE_CH(G2/3)::PH:', 2**(8+2*modval(24)),
     $     '/',2**(8+2*modval(25)),
     $     'XP:', 2**(2+2*modval(26)), '/', 2**(2+2*modval(27)),
     $     'YP:', 2**(2+2*modval(28)), '/', 2**(2+2*modval(29)),
     $     'RT:', 32*modval(30)+modval(30)/3*160, '/',
     $     32*modval(31)+modval(31)/3*160,
     $     'SP:', 256*modval(32), '/', 256*modval(33),
     $     'PH_EVNT:', (modval(i), i = 50, 57),
     $     'C2_PGM_VER.', modval(147), 'C3_PGM_VER.', modval(148)

c     write header4(X-use, Y-use, Mask1)
      write(head4,
     $     '(a, 16i1, 1x, a, 16i1, 1x, a, a, 1x a, a, 1x a,
     $     16i1, 1x a, 16i1)')
     $     'X_use:',(modval(i),i=58, 73),
     $     'Y_use:',(modval(i),i=74, 89),
     $     'MASK1:', off_on(modval(46)),
     $     'LOGIC:', orand(modval(47)),
     $     'MASK1X:',(modval(i),i=90, 105),
     $     'MASK1Y:',(modval(i),i=106,121)

c     write head5 (mask2, radius discriminator)
      write(head5, '(a, a, 1x, a, a, 1x,
     $     a, i3, 1x  a, i3, 1x, a, i3, 1x,
     $     a, i3, 1x  a, i3, 1x, a, i3, 1x,
     $     a, i3, 1x  a, i3, 1x, a, i3, 1x, a, i3)')
     $     'MASK2:', off_on(modval(48)),
     $     'M2RGN:', out_in(modval(49)),
     $     'M2XL:', modval(140), 'M2XU:',
     $     modval(141), 'M2YL:', modval(142),
     $     'M2YU:', modval(143), 'C_X_F:',
     $     modval(124),  'C_Y_F:', modval(125),
     $     'C_X_P:', modval(122), 'C_Y_P:',
     $     modval(123), 'RD_F:', modval(139),
     $     'RD_P:', modval(138)

c     write head6 (spread discri)
      write(head6,
     $     '(a, i3, 1x, a, i3, 1x,
     $     a, i3, 1x, a, i3, 1x,
     $     a, i4,
     $     a, i3, 1x, a, i3, 1x,
     $     a, i3, 1x, a, i3, 1x,
     $     a, i4)')
     $     'S_DSCR::A_L_F:', modval(132), 'A_U_F:', modval(133),
     $     'B_F:', modval(134), 'C_F:', modval(135),
     $     'PH_F:',  256*modval(136)+modval(137),
     $     ':A_L_P:', modval(126), 'A_U_P:', modval(127),
     $     'B_P:', modval(128), 'C_P:', modval(129),
     $     'PH_P:',  256*modval(130)+modval(131)

c     write head7 (RBM, Scale Down)
      write(head7, '(3(a, a, 1x),
     $     2(a, i1, 1x),
     $     3(a, i1, 1x), 2(a, i3, 1x), a, i3)')
     $     'RBM::POW:', off_on(modval(9)),
     $     'AUTO:', off_on(modval(10)),
     $     'MANFL:', off_on(modval(12)),
     $     'DSCR:', modval(13), 'FL_LVL:',  modval(11),
     $     'S_DOWN::LD ', modval(22), 'L1:', modval(21),
     $     'RT:1/', 2**modval(39), '1DD:1/', 2**modval(146),
     $     'M2:1/', 2**modval(144), 'XRAY:1/', 2**modval(145)

c     Return  Scale down parameters
      ODDSD = modval(146)
      M2SD  = modval(144)
      XRAYSD= modval(145)
      L1SD  = modval(21)
      LDHTSD= modval(22)

 999  continue
      if(ftstat.ne.0) call fcerrm(ftstat)
      return
      end

C***********************************************************************
C     SUBROUTINE:
C     gethk
C
C     DESCRIPTION:
C     get HK parameters values
C
C     AUTHOUR/DATE:
C     Ken Ebisawa 1993/03/17
C
C     MODIFICATION HISTORY:
C
C     NOTES:
C
C     USAGE:
C     call gethk(item, value, LD, L0, L1, L2, H0, H1, H2, Cin,
C     Cout, RBM, temp, R_temp, L_mon, h_mon, h_com, rbmflg, cpuopr,
c     events, x1d, y1d)
C
C     ARGUMENTS:
C     item - (in) HK item name
C     value - (in) HK value
C     LD - (in/out) LD hit value (accumulation)
C     L0 - (in/out) L0 value (accumulation)
C     L1 - (in/out) L1 value (accumulation)
C     L2 - (in/out) L2 value (accumulation)
C     H0 - (in/out) H0 value (accumulation)
C     H1 - (in/out) H1 value (accumulation)
C     H2 - (in/out) H2 value (accumulation)
C     Cin- (in/out) Cin value (accumulation)
C     Cout - (in/out) Cout value (accumulation)
C     RBM - (in/out) RBM value (accumulation)
C     temp - (out) temperature
C     R_temp - (out) RBM temprature
C     L_mon - (out) HV_L monitor
C     H_mon - (out) HV_H monitor
C     H_com - (out) HV current monitor
C     rbmflg - (out) RBM flag
C     cpuopr - (out) CPU operation normal/error
C     events - (out) PH events monitor
C     x1d    - (out) X-1D distribution
C     y1d    - (out) Y-1D distribution
C
C     PRIMARY LOCAL VARIABLES:
C     subroutine c_cnv  - convert HV-H current monitor value to physical value
C     subroutine hm_cnv  - convert HV-H monitor telemetry --> physical value
C     subroutine lm_cnv  - convert HV-L monitor telemetry --> physical value
C     subroutine t_cnv  - convert temperature telemetry --> physical value
C
C     CALLED ROUTINES:
C
C***********************************************************************
      subroutine gethk(item, value, LD, L0, L1, L2, H0, H1, H2,
     $     Cin, Cout, Tout, RBM, temp, R_temp, L_mon, h_mon, h_com,
     $     rbmflg, cpuopr, events, x1d, y1d)
      implicit none
      character*(*) item
      integer value, LD, L0, L1, L2, H0, H1, H2, Cin, Cout, Tout, RBM
      real temp, R_temp, H_com, l_mon, h_mon
      integer rbmflg, cpuopr(2),
     $     events(8), x1d(32), y1d(32)

      integer i

      integer LDbuf, L0buf, L1buf, L2buf, H0buf, H1buf, H2buf,
     $     Cinbuf, Cobuf, RBMbuf, evtbuf(8), x1dbuf(32), y1dbuf(32), 
     $     ldflag, l0flag, l1flag, l2flag, h0flag, h1flag, h2flag,
     $     cinflg, coflag, rbmset, evtflg(8), x1dflg(32), y1dflg(32)

      data LDbuf/0/, L0buf/0/, L1buf/0/, L2buf/0/, H0buf/0/,
     $     H1buf/0/, H2buf/0/, Cinbuf/0/, Cobuf/0/, RBMbuf/0/,
     $     evtbuf/0,0,0,0,0,0,0,0/,
     $     x1dbuf/
     $     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     $     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/,
     $     y1dbuf/
     $     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     $     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/,
     $     ldflag/0/, l0flag/0/, l1flag/0/, l2flag/0/,
     $     h0flag/0/, h1flag/0/, h2flag/0/,
     $     cinflg/0/,coflag/0/,rbmset/0/,
     $     evtflg/0,0,0,0,0,0,0,0/,
     $     x1dflg/
     $     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     $     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/,
     $     y1dflg/
     $     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     $     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/

      character(6) evname(8)
      data evname/
     $     'E_ANFF', 'EANAL0', 'EEDGEV', 'E_M1EV',
     $     'ECLCOU', 'EDSCOU', 'E_M2EV', 'E_XREV'/
      character(3) x1name(32)
      data x1name/
     $     'X0 ', 'X1 ', 'X2 ', 'X3 ', 'X4 ', 'X5 ', 'X6 ', 'X7 ',
     $     'X8 ', 'X9 ', 'X10', 'X11', 'X12', 'X13', 'X14', 'X15',
     $     'X16', 'X17', 'X18', 'X19', 'X20', 'X21', 'X22', 'X23',
     $     'X24', 'X25', 'X26', 'X27', 'X28', 'X29', 'X30', 'X31'/
      character(3) y1name(32)
      data y1name/
     $     'Y0 ', 'Y1 ', 'Y2 ', 'Y3 ', 'Y4 ', 'Y5 ', 'Y6 ', 'Y7 ',
     $     'Y8 ', 'Y9 ', 'Y10', 'Y11', 'Y12', 'Y13', 'Y14', 'Y15',
     $     'Y16', 'Y17', 'Y18', 'Y19', 'Y20', 'Y21', 'Y22', 'Y23',
     $     'Y24', 'Y25', 'Y26', 'Y27', 'Y28', 'Y29', 'Y30', 'Y31'/

c     LD
C      if (item(6:7).eq.'LD') then
C     Change in version 4.0 (94/09/02) (made backward compatible)
      if (item(6:7).eq.'LD'.or.item(4:8).eq.'LDHIT') then
c     When the item is found initailly, buffer=value
         if(ldflag.eq.0) then
            ldbuf = value
            ldflag = 1
         endif
         LD = LD + value - LDbuf
         if(value.lt.LDbuf) LD=LD+256
         LDbuf=value
      endif

c     L0
C      if (item(6:7).eq.'L0') then
C     Change in version 4.0 (94/09/02) (made backward compatible)
      if (item(6:7).eq.'L0'.or.item(4:5).eq.'L0') then
c     When the item is found initailly, buffer=value
         if(l0flag.eq.0) then
            l0buf = value
            l0flag = 1
         endif
         L0 = L0 + value - L0buf
         if(value.lt.L0buf) L0=L0+256
         L0buf=value
      endif

c     L1
C      if (item(6:7).eq.'L1') then
C     Change in version 4.0 (94/09/02) (made backward compatible)
      if (item(6:7).eq.'L1'.or.item(4:5).eq.'L1') then
c     When the item is found initailly, buffer=value
         if(l1flag.eq.0) then
            l1buf = value
            l1flag = 1
         endif
         L1 = L1 + value - L1buf
         if(value.lt.L1buf) L1=L1+256
         L1buf=value
      endif

c     L2
C      if (item(6:7).eq.'L2') then
C     Change in version 4.0 (94/09/02) (made backward compatible)
      if (item(6:7).eq.'L2'.or.item(4:5).eq.'L2') then
c     When the item is found initailly, buffer=value
         if(l2flag.eq.0) then
            l2buf = value
            l2flag = 1
         endif
         L2 = L2 + value - L2buf
         if(value.lt.L2buf) L2=L2+256
         L2buf=value
      endif

c     H0
C      if (item(6:7).eq.'H0') then
C     Change in version 4.0 (94/09/02) (made backward compatible)
      if (item(6:7).eq.'H0'.or.item(3:6).eq.'_H0 ') then
c     When the item is found initailly, buffer=value
         if(h0flag.eq.0) then
            h0buf = value
            h0flag = 1
         endif
         H0 = H0 + value - H0buf
         if(value.lt.H0buf) H0=H0+256
         H0buf=value
      endif

c     H1
C      if (item(6:7).eq.'H1') then
C     Change in version 4.0 (94/09/02) (made backward compatible)
      if (item(6:7).eq.'H1'.or.item(3:6).eq.'_H1 ') then
c     When the item is found initailly, buffer=value
         if(h1flag.eq.0) then
            h1buf = value
            h1flag = 1
         endif
         H1 = H1 + value - H1buf
         if(value.lt.H1buf) H1=H1+256
         H1buf=value
      endif

c     H2
C      if (item(6:7).eq.'H2') then
C     Change in version 4.0 (94/09/02) (made backward compatible)
      if (item(6:7).eq.'H2'.or.item(3:6).eq.'_H2 ') then
c     When the item is found initailly, buffer=value
         if(h2flag.eq.0) then
            h2buf = value
            h2flag = 1
         endif
         H2 = H2 + value - H2buf
         if(value.lt.H2buf) H2=H2+256
         H2buf=value
      endif

c     Telemetry out
      if (item(1:1).eq.'G'.and.item(3:7).eq.'_TELM') then
         Tout =  Tout + value
      endif

c     CPU-in
C      if (item(4:7).eq.'C_IN') then
C     Change in version 4.0 (94/09/02) (made backward compatible)
      if (item(4:7).eq.'C_IN'.or.item(4:8).eq.'CPU_I') then
c     When the item is found initailly, buffer=value
         if(cinflg.eq.0) then
            cinbuf = value
            cinflg = 1
         endif
         Cin = Cin + value - Cinbuf
         if(value.lt.Cinbuf) Cin=Cin+256
         Cinbuf=value
      endif

c     CPU-out
C      if (item(4:8).eq.'C_OUT') then
C     Change in version 4.0 (94/09/02) (made backward compatible)
      if (item(4:8).eq.'C_OUT'.or.item(4:8).eq.'CPU_O') then
c     When the item is found initailly, buffer=value
         if(coflag.eq.0) then
            cobuf = value
            coflag = 1
         endif
         Cout = Cout + value - Cobuf
         if(value.lt.Cobuf) Cout=Cout+256
         Cobuf=value
      endif

c     RBM
C      if (item.eq.'RBM_MON ') then
C     Change in version 4.0 (94/09/02) (made backward compatible)
      if (item.eq.'RBM_MON '.or.item.eq.'RBM_CONT') then
c     When the item is found initailly, buffer=value
         if(rbmset.eq.0) then
            rbmbuf = value
            rbmset = 1
         endif
         RBM = RBM + value - RBMbuf
         if(value.lt.RBMbuf) RBM=RBM+65536
         RBMbuf=value
      endif

c     HV-H current monitor
      if (item.eq.'HVH2_CM '.or.item.eq.'HVH3_CM ') then
         h_com=value
c     Convert h_com value
         call C_cnv(h_com)
      endif

c     HV-H monitor
      if (item.eq.'HVH2_MON'.or.item.eq.'HVH3_MON') then
         h_mon=value
c     Convert h_mon value
         call hm_cnv(h_mon)
      endif

c     HV-L monitor
      if (item.eq.'HVL2_MON'.or.item.eq.'HVL3_MON') then
         l_mon=value
c     Convert l_mon value
         call lm_cnv(l_mon)
      endif

c     temperature
      if (item.eq.'G2_TEMP'.or.item.eq.'G3_TEMP') then
         temp=value
c     Convert temp value
         if(item.eq.'G2_TEMP') then
            call t_cnv(temp, 2)
         elseif(item.eq.'G3_TEMP') then
            call t_cnv(temp, 3)
         endif
      endif

c     RBM temperature
      if (item.eq.'RBM_TEMP') then
         r_temp=value
c     Convert rbm temp value
         call t_cnv(r_temp, 1)
      endif

c     RBM flag
      if (item.eq.'G_RBM_F '.or.item.eq.'GIS_RBMF') then
         rbmflg = value
      endif

c     CPU OPR/ERR
      if (item(1:6).eq.'C2_OPR') then
         cpuopr(1) = value
      endif
      if (item(1:6).eq.'C3_OPR') then
         cpuopr(2) = value
      endif

c     EVENTS
      do 100 i = 1, 8
         if (item(3:8).eq.evname(i)) then
c     When the item is found initailly, buffer=value
            if(evtflg(i).eq.0) then
               evtbuf(i) = value
               evtflg(i) = 1
            endif
            events(i) = events(i) + value - evtbuf(i)
            if(value.lt.evtbuf(i)) events(i) = events(i) + 256
            evtbuf(i) = value
         endif
 100  continue

c     X 1-D distribution
      do 200 i = 1, 32
         if (item(1:3).eq.'GIS'.and.item(6:8).eq.x1name(i)) then
c     when the item was found first time, x1dbuf = value
            if(x1dflg(i).eq.0)  then
               x1dbuf(i) = value
               x1dflg(i) = 1
            endif
            x1d(i) = x1d(i) + value - x1dbuf(i)
            if(value.lt.x1dbuf(i)) x1d(i) = x1d(i) + 256
            x1dbuf(i) = value
         endif
 200  continue

c     Y 1-D distribution
      do 300 i = 1, 32
         if (item(1:3).eq.'GIS'.and.item(6:8).eq.y1name(i)) then
c     when the item was found initially, y1dbuf = value
            if(y1dflg(i).eq.0)  then
               y1dbuf(i) = value
               y1dflg(i) = 1
            endif
            y1d(i) = y1d(i) + value - y1dbuf(i)
            if(value.lt.y1dbuf(i)) y1d(i) = y1d(i) + 256
            y1dbuf(i) = value
         endif
 300  continue

      return
      end

C***********************************************************************
C     SUBROITINE:
C     c_conv, hm_conv, lm_cnv, t_cnv
C
C     DESCRIPTION:
C     Convert Current Monitor, HV-H monitor, HV-L monitor, temperature to the
C     physical values
C
C     The conversion parameters are taken from titania/raimei/b1/fjt
c     crt/nidstd.f (a program created by FACOM Company for the Quick Look system)
C
C     AUTHOUR/DATE:
C     Ken Ebisawa 03/17/93
C
C     MODIFICATION HISTORY:
C
C     NOTES:
C
C     USAGE:
C     call c_conv(c_mon) or call t_cnv(temp, sensor)
C
C     ARGUMENTS:
C     c_mon: current monitor value input and output
C     temp:  temperature input and output
C     sensor: 1=RBM, 2=GIS2, 3=GIS3
C
C     PRIMARY LOCAL VARIABLES:
C
C     CALLED ROUTINES:
C
C***********************************************************************

      subroutine C_cnv(h_com)
      real h_com

      h_com = 100./255.*h_com

      return
      end

      subroutine hm_cnv(h_mon)
      real h_mon

      h_mon = 8000./255*h_mon

      return
      end

      subroutine lm_cnv(l_mon)
      real l_mon

      l_mon = 1400./255*l_mon

      return
      end

      subroutine t_cnv(temp, sensor)
      real temp
      integer sensor
c     sensor 1 = RBM, 2 = GIS2, 3 = GIS3

c     This is the old formula
c     temp = -50. + 130./255*temp

      if(sensor.eq.1) then
         temp = -45.90 + 0.5459*temp
      elseif(sensor.eq.2.or.sensor.eq.3) then
         temp = -47.50 + 0.5459*temp
      endif
      return
      end

C***********************************************************************
C     SUBROITINE:
C     clrstr
C
C     DESCRIPTION:
C     clear character string
C
C     AUTHOUR/DATE:
C     Ken Ebisawa 02/16/93
C
C     MODIFICATION HISTORY:
C
C     NOTES:
C
C     USAGE:
C     call clrstr(string)
C
C     ARGUMENTS:
C     string: character string
C
C     PRIMARY LOCAL VARIABLES:
C
C     CALLED ROUTINES:
C
C***********************************************************************
      subroutine clrstr(string)
      character*(*) string
      integer i

      do 100 i = 1, len(string)
         string(i:i) = ' '
 100  continue
      return
      end

C***********************************************************************
C     SUBROITINE:
C     prgchg
C
C     DESCRIPTION:
C     Print warning when a modal parameter change in a single file
C
C     AUTHOUR/DATE:
C     Ken Ebisawa 03/17/93
C
C     MODIFICATION HISTORY:
C
C     NOTES:
C
C     USAGE:
C     call prtchg(ounit, outopn, item, time)
C
C     ARGUMENTS:
C     ounit(in): output file unit
C     outopn(in): output to file (yes) or the terminal(no)
C     item (in): HK parameter name
C     time (in): Time when the parameter change
C
C     PRIMARY LOCAL VARIABLES:
C
C     CALLED ROUTINES:
C
C***********************************************************************
      subroutine prtchg(ounit, outopn, item, time, value)
      integer ounit, value
      logical outopn
      character item*8
      double precision time

      integer year, day, hour, minute, month
      real second
      character(80) context

c     Convert ASCA time to UT
      call asca2ut(time, day, month, year, hour, minute, second)
C      call fisct2(time, day, hour, minute, second)
C      call fid2my(1, 1, 1993, day, month, year)

      write(context,'(a, a, i7, a, f15.3, 1x,
     $     a1, i4, a1, i2.2, a1, i2.2, a1,
     $     i2.2, a1, i2.2, a1, i2.2, a1)')
     $     item, '=', value, ' at ', time,
     $     '(', year, '-', month, '-', day, 'T',
     $     hour, ':', minute, ':', int(second), ')'
      call fprint(ounit, outopn, context)
      end
