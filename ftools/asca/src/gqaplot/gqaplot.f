C  FTOOLs info $Header: /headas/headas/ftools/asca/src/gqaplot/gqaplot.f,v 3.13 2013/05/21 19:08:07 irby Exp $
C
CCCCCCCCCCC
C     gqaplot ---- Make a quick analysis plot from a GIS science file
C     Ken Ebisawa NASA/GSFC 1993/04/28
C     Kent Blackburn 1993/04/28 - Removed NON ANSI "implicit none"
C
C     ver 1.1: Modification by Ken Ebisawa 1993/05/13
C     Detector coordinates column names DX, DY --> RAWX, RAWY
C     Read correct TSCALE, TZERO for the PH mode RISE TIME
C     File names can be read from a file specified with @file_name
C
C     ver 1.2: Modification by Ken Ebisawa 1993/05/21
C     Check range of X, Y, pha, and rise time.
C
C     ver 1.3: Modification by Ken Ebisawa 1993/08/04
C     Made consistent with FRFread 2.994
C
C     ver 1.4: Modification by Ken Ebisawa 1993/08/06
C     Read more parameters (image X, Y name etc) from the parameter file.
C     Add more anotations.
C
C     ver 1.5: Modification by Ken Ebisawa 1993/10/25
C     Made compatible with FRFread 3.000.
C     Add more anotations.
C
C     ver 1.6: Modification by Ken Ebisawa 1993/12/14
C     Capablility to plot the energy in the log scale for spectrum and
C     rise time diagrams.
C
C     ver 1.7: Modification by Ken Ebisawa 1994/04/19
C     Do not use the TSCALE and TZERO keyword in the event extension
C
C     ver 1.8: Modification for the ASCATIME --> UT conversion
c     by Ken Ebisawa 1994/04/21
C
C     ver 1.9: Modification for the MPC mode
c     by Ken Ebisawa 1994/04/27
C
C     In qplot, changed sensecase argument in ftgcno calls to .false.
C     by Jeff Guerber GSFC664/HSTX, 1997/2/24.
C
C     ver 1.10: Y2K/new-format date string fixes ("yyyy-mm-dd")
C     by Jeff Guerber RSTX/GSFC-664, 1998-07-03
C
CCCCCCCCC
      subroutine gqaplt

C      implicit none

      character(80) infile, outfil
      character(12)  xname, yname, ph_name, rt_name
      logical contour, gray, dotmap, imagelog, phalog, englog
      character(8) device

C     Error report
      character(40) taskname
      common /task/ taskname
      taskname = 'gqaplot ver 1.10'

C     get parameters from the parameter file
      call qgpar(infile, outfil, xname, yname,
     $     ph_name, rt_name, contour, gray, dotmap,
     $     imagelog, phalog, englog, device)

C     do the real job
      call qplot(infile, outfil, xname, yname,
     $     ph_name, rt_name, contour, gray, dotmap,
     $     imagelog, phalog, englog, device)
      end

CCCCCCCCCC
C     qgpar ---- get parameters from the parameter file
C     Ken Ebisawa NASA/GSFC 1993/04/28
C
C     Modification by Ken Ebisawa (93/08/04)
C     reads xname, yname, ph_name, rt_name, phalog etc.
CCCCCCCCC
      subroutine qgpar(infile, outfil, xname, yname,
     $     ph_name, rt_name, contour, gray, dotmap,
     $     imagelog, phalog, englog, device)

C      implicit none

      character(80) infile, outfil
      character(12)  xname, yname, ph_name, rt_name
      logical phalog, englog, contour, gray, dotmap, imagelog
      character(8) device

      character(80) contxt
      integer status

      status = 0

C     Get the input fits file name
      call uclgst('infile', infile, status)
      if(status.ne.0) then
         contxt = 'could not get the input file name'
         call fcerr(contxt)
         go to 999
      endif

C     Get the output file name
      call uclgst('outfile', outfil, status)
      if(status.ne.0) then
         contxt = 'could not get the output file name'
         call fcerr(contxt)
         go to 999
      endif

C     Get the X column name
      call uclgst('xname', xname, status)
      if(status.ne.0) then
         contxt = 'could not get the X-column name'
         call fcerr(contxt)
         go to 999
      endif

C     Get the Y column name
      call uclgst('yname', yname, status)
      if(status.ne.0) then
         contxt = 'could not get the Y-column name'
         call fcerr(contxt)
         go to 999
      endif

C     Get the PH column name
      call uclgst('phname', ph_name, status)
      if(status.ne.0) then
         contxt = 'could not get the output file name'
         call fcerr(contxt)
         go to 999
      endif

C     Get the pha log or linear flag
      call uclgsb('phalog', phalog, status)
      if(status.ne.0) then
         contxt = 'could not get the pha log/linear flag'
         call fcerr(contxt)
         go to 999
      endif

C     Get the energy log or linear flag
      call uclgsb('englog', englog, status)
      if(status.ne.0) then
         contxt = 'could not get the energy log/linear flag'
         call fcerr(contxt)
         go to 999
      endif

C     Get the RT column name
      call uclgst('rtname', rt_name, status)
      if(status.ne.0) then
         contxt = 'could not get the output file name'
         call fcerr(contxt)
         go to 999
      endif

C     Get the contour flag
      call uclgsb('contour', contour, status)
      if(status.ne.0) then
         contxt = 'could not get the contour flag'
         call fcerr(contxt)
         go to 999
      endif

C     Get the image log or linear flag
      call uclgsb('imagelog', imagelog, status)
      if(status.ne.0) then
         contxt = 'could not get the image log/linear flag'
         call fcerr(contxt)
         go to 999
      endif

C     Get the gray flag
      call uclgsb('gray', gray, status)
      if(status.ne.0) then
         contxt = 'could not get the gray flag'
         call fcerr(contxt)
         go to 999
      endif

C     Get the dotmap flag
      call uclgsb('dotmap', dotmap, status)
      if(status.ne.0) then
         contxt = 'could not get the dotmap flag'
         call fcerr(contxt)
         go to 999
      endif

C     Get the device name
      call uclgst('device', device, status)
      if(status.ne.0) then
         contxt = 'could not get the device name'
         call fcerr(contxt)
         go to 999
      endif

 999  continue
      if(status.ne.0) call fcerrm(status)
      end

Cccccccccc
C     qplot ---- Open the file, read data, plot figures
C     Ken Ebisawa NASA/GSFC 1993/04/28
C
C     ver 1.1: Modification by Ken Ebisawa NASA/GSFC 1993/05/13
C     Detector coordinates column names DX, DY --> RAWX, RAWY
C     Read correct TSCALE, TZERO for the PH mode RISE TIME
C     ver 1.2: Modification by Ken Ebisawa NASA/GSFC 1993/05/21
C     Check range of X, Y, PHA, Rise time
C
C     ver 1.3: Modification by Ken Ebisawa NASA/GSFC 1993/08/04
C     Check range of use new parameters xname, yname, ph_name, rt_name, phalog
C
C     ver 1.5: Compatible with the FRFread 3.00. 93/10/25
Ccccccccc
      subroutine qplot(infile, outfil, xname, yname,
     $     ph_name, rt_name, contour, gray, dotmap,
     $     imagelog, phalog, englog, device)

C     implicit none

      character(80) infile, outfil
C     xname, yname, ph_name, rt_name are names of the columns for
C     X, Y, PH, Rise Time
      character(12)  xname, yname, ph_name, rt_name
C     Plot the PH with log or linear
      logical contour, gray, dotmap, imagelog, phalog, englog
C     Plot device
      character(8) device

C     To parse the infile name
      integer ifile, ninfle
      character(80) vinfle(999)
C     negflg is used in fcgcls (I do not the meaning though ...)
      logical negflg

C     Variables concerning file i/o
      integer iunit, ounit, block, ftstat
      logical inopen, outopn

C     Max number of columns
      integer maxcl
      parameter (maxcl = 20)

C     To get significant primary keywords
      integer bitpix, naxis, naxes(maxcl), pcount, gcount
      logical simple, extend

C     Contxt is the character array to print message to the terminal or
C     the output file.  Commnt is the comment for the fits keywords.
      character(80) contxt, commnt

Cccccccccccccc
C     Variables for primary keywords
C

C     For the keywors, 'TELESCOP', 'INSTRUME', and 'DATAMODE'
      character(10) telscp, instrm, datmod

C     Nominal RA, DEC, and EQUINOX, EXPOSURE
      real ra_nom, decnom, eqinox, exposure

      character(8) origin, bitrat
      character(20) object, piname

C     obdate, obtime:observation start/end date/time
C     c_date: creation date
      character(68) obdate, obtime, c_date

C     orbit0:initial orbit, orbit1:final orbit, tlmfle:telemetry file name,
C     authour:authour of the file (FRFREAD version number)
      character(8) orbit0, orbit1
      character(20) author, tlmfle

C     tstart, tend: start and end time of the file
      double precision tstart, tend

C     HVH-level, HVL-level, GAIN value, lower energy discriminator
C     (from the primary header)
      integer hvhlvl, hvllvl, gain, le_ds

C     Position determination method, CPU selection
      character(8) posdet, cpusel

C     Rise time lower/upper discriminator level, Rise time bit condense
      integer rt_ld, rt_ud, rt_bcd

C     R_DSCR, SP_DSCR
      character(8) r_dscr, sp_dscr
c
Cccccccccccccc

C     nkwd is the number of keyword in the primary header
C     keyadd is neceaary  in the ftool routine ftghsp (not used in this
C     program)
      integer nkwd, keyadd

C     htype:header type of the first extension, nrow:number of rows in
C     the first extenstion.
C     nevent is the number of events (equal to nrows in PH and PCAL mode)
      integer htype, nrows, nevent

C     Number of anomaly events
      integer illevt

C     Channel sizes for X-bin, Y-bin, PHA-bin, spread bin, and number
C     of bits for the rise time
C     integer xbsize, ybsize, phabin,  sbsize, risbit
      integer xbsize, ybsize, phabin,  sbsize, risbin,timebin

C     x_cnum, y_cnum, ph_name, r_cnum, t_cnum: column numbers for
C     X, Y, PH, Rise Time, Time
C     pha_cnum is the column number for PHA
      integer x_cnum, y_cnum, ph_cnum, r_cnum, t_cnum, pha_cnum

C     Variable to get the data from each row in the first extension
      logical anyf
      double precision time
      integer xvalue, yvalue, phval, phaval, ristim

C     Arrays for PH, Image, PH_Rise time to draw diagram
      integer phampc(256)
      real    image(0:255, 0:255), pha(0:1023), phactr(0:1023),
     $     ph_rt(0:1023, 0:255)

Ccc
C     Parameters for light curves

C     binlen: length of one bin for the light curve
      real binlen

C     num: number of light curve bins
      integer num

C     l_chst, h_chst: count history for low and high energy bands
C     l_ctr, h_ctr: count history for the central region
      real l_chst(256), h_chst(256), l_ctr(256), h_ctr(256)

C     tbstrt is the start time of each light curve bin
      double precision tbstrt
Ccc

C     To control do loop
      integer i, j

C     End of the variable declaration. Start the real job.

C     Parse the infile
      call fcgcls(infile, vinfle, ninfle, negflg)
C     vinfile is the array of the infile names, and ninfile is the
C     number of infiles.

C     Repeat the procedure for the number of infiles
      do 1000 ifile = 1, ninfle

C     Open the fits file
         inopen = .false.
         iunit = 15
         block = 0
         ftstat = 0
         call  ftopen(iunit, vinfle(ifile), 0, block, ftstat)
         if (ftstat.ne.0) then
            contxt = 'Unable to open infile'
            call fcerr(contxt)
            go to 999
         end if
         inopen = .true.

C     Open the ASCII file
         outopn = .false.
         ounit = 16
         if (outfil. ne. 'STDOUT') then
            outopn = .true.
            open(unit = ounit, file = outfil, status = 'new', err = 110)
            go to 120
 110        contxt = 'error: output file already existes ?'
            call fcerr(contxt)
            go to 999
 120        continue
         endif

C     Print the version number
C     contxt = '** GQAPLOT ver 1.0 (1993/04/28) **'
C     contxt = '** GQAPLOT ver 1.1 (1993/05/13) **'
C     contxt = '** GQAPLOT ver 1.2 (1993/05/21) **'
C     contxt = '** GQAPLOT ver 1.4 (1993/08/06) **'
C     contxt = '** GQAPLOT ver 1.5 (1993/10/26) **'
C     contxt = '** GQAPLOT ver 1.6 (1993/12/14) **'
C     contxt = '** GQAPLOT ver 1.7 (1994/04/19) **'
C     contxt = '** GQAPLOT ver 1.8 (1994/04/22) **'
C        contxt = '** GQAPLOT ver 1.9 (1994/04/27) **'
         contxt = '** GQAPLOT ver 1.10 (1998-07-03) **'
         call fprint(ounit, outopn, contxt)

C     Print the fits file name
         write(contxt, '(a)') 'GIS Science File Name:'
         call fprint(ounit, outopn, contxt)
         write(contxt, '(a)') vinfle(ifile)(1:80)
         call fprint(ounit, outopn, contxt)

C     Obtain significant primary header keywords
         call ftghpr(iunit, maxcl, simple, bitpix, naxis, naxes,
     $        pcount, gcount, extend, ftstat)
         if (ftstat.ne.0) then
            contxt = 'Error in the primary header'
            call fcerr(contxt)
            go to 999
         elseif (.not.simple) then
            contxt = 'File is not in the FITS format'
            call fcerr(contxt)
            go to 999
         elseif(.not.extend) then
            contxt = 'File does not have the extenstion'
            call fcerr(contxt)
            go to 999
         end if

C     Check the file (only GIS PH, PCAL, MPC data are supported)
         call ftgkys(iunit, 'TELESCOP', telscp, commnt, ftstat)
         if (ftstat.ne.0) then
            contxt = 'no TELESCOP keyword in the primary header'
            call fcerr(contxt)
            go to 999
         endif
         call ftgkys(iunit, 'INSTRUME', instrm, commnt, ftstat)
         if (ftstat.ne.0) then
            contxt = 'no INSTRUME keyword in the primary header'
            call fcerr(contxt)
            go to 999
         endif
         call ftgkys(iunit, 'DATAMODE', datmod, commnt, ftstat)
         if (ftstat.ne.0) then
            contxt = 'no DATAMODE keyword in the primary header'
            call fcerr(contxt)
            go to 999
         endif
         if(telscp.ne.'ASUKA'.and.telscp.ne.'ASCA') then
            contxt = 'This is not the ASCA fits file.'
            call fcerr(contxt)
            go to 999
         elseif(instrm.ne.'GIS2'.and.instrm.ne.'GIS3') then
            contxt='This is not GIS science file. '
     $           //'Only GIS data are supported.'
            call fcerr(contxt)
            write(contxt, '(a,a)') 'instrument =', instrm
            call fcerr(contxt)
            go to 999
         elseif(datmod.ne.'PH'.and.datmod.ne.'PCAL'.
     $           and.datmod.ne.'MPC') then
            contxt=
     $           'Mode is not one of the PH, PCAL and  MPC modes.'
            call fcerr(contxt)
            write(contxt, '(a,a)') 'Data mode = ', datmod
            call fcerr(contxt)
            go to 999
         endif
C     Now the file is GIS PH, PCAL or MPC  mode science file.

C     Get the origin, object and PI
         call ftgkys(iunit, 'ORIGIN', origin, commnt, ftstat)
         if (ftstat.ne.0) go to 999
         call ftgkys(iunit, 'OBJECT', object, commnt, ftstat)
         if (ftstat.ne.0) go to 999
         call ftgkys(iunit, 'OBSERVER', piname, commnt, ftstat)
         if (ftstat.ne.0) then
            ftstat = 0
            call ftgkys(iunit, 'SEQPI', piname, commnt, ftstat)
            if (ftstat.ne.0) go to 999
         endif
         write(contxt, '(a, a8, 1x, a, a20, 1x, a, a20)') 'Origin:',
     $        origin, 'Object:', object, 'PI:', piname
         call fprint(ounit, outopn, contxt)

C     Print the instrument, data mode, exposure  and bit rate
         call ftgkys(iunit, 'BIT_RATE', bitrat, commnt, ftstat)
         if (ftstat.ne.0) go to 999
         call ftgkye(iunit, 'ONTIME', exposure, commnt, ftstat)
         if (ftstat.ne.0) then
            ftstat = 0
            call ftgkye(iunit, 'EXPOSURE', exposure, commnt, ftstat)
            if (ftstat.ne.0) go to 999
         endif
         write(contxt, '(a, a4, 1x, a, a4, 1x, a, a, a, 1p, E8.2, a)')
     $        'Instrument:', instrm,
     $        'Mode:', datmod, 'Bit_Rate:', bitrat, 'EXPOSURE:',
     $        exposure, 'sec'
         call fprint(ounit, outopn, contxt)

C     Read the start time
         call ftgkyd(iunit, 'TSTART', tstart, commnt, ftstat)
         if (ftstat.ne.0) go to 999
         call ftgkys(iunit, 'DATE-OBS', obdate, commnt, ftstat)
         if (ftstat.ne.0) go to 999
         call ftgkys(iunit, 'TIME-OBS', obtime, commnt, ftstat)
         if (ftstat.ne.0) go to 999
         write(contxt, '(a, f13.3, a1, 1x, a10, 1x, a8)' )
     $        'Start Time (ASCA time, UT)',
     $        tstart, ',', obdate(1:10), obtime(1:8)
         call fprint(ounit, outopn, contxt)

C     Read the end time
         call ftgkyd(iunit, 'TSTOP', tend, commnt, ftstat)
         if (ftstat.ne.0) go to 999
         call ftgkys(iunit, 'DATE-END', obdate, commnt, ftstat)
         if (ftstat.ne.0) go to 999
         call ftgkys(iunit, 'TIME-END', obtime, commnt, ftstat)
         if (ftstat.ne.0) go to 999
         write(contxt, '(a, f13.3, a1, 1x, a10, 1x, a8)' )
     $        'End   Time (ASCA time, UT)',
     $        tend, ',', obdate(1:10), obtime(1:8)
         call fprint(ounit, outopn, contxt)

C     Print orbit number and telemetry file name
         call ftgkys(iunit, 'ORBITBEG',   orbit0, commnt, ftstat)
         if(ftstat.ne.0) then
            ftstat = 0
            call ftgkys(iunit, 'ORBIT0',   orbit0, commnt, ftstat)
            if (ftstat.ne.0) go to 999
         endif
         call ftgkys(iunit, 'ORBITEND',   orbit1, commnt, ftstat)
         if(ftstat.ne.0) then
            ftstat = 0
            call ftgkys(iunit, 'ORBIT1',   orbit1, commnt, ftstat)
            if (ftstat.ne.0) go to 999
         endif
         call ftgkys(iunit, 'TLM_FILE', tlmfle, commnt, ftstat)
         if (ftstat.ne.0) go to 999
         write(contxt, '(a, a, a, a, a, a)') 'Initial Orbit:', orbit0,
     $        ' Final Orbit:', orbit1, ' Telemetry File:', tlmfle
         call fprint(ounit, outopn, contxt)

C     Print authour name and creation date
         call ftgkys(iunit, 'CREATOR', author, commnt, ftstat)
         if(ftstat.ne.0) then
            ftstat = 0
            call ftgkys(iunit, 'AUTHOR', author, commnt, ftstat)
            if (ftstat.ne.0) go to 999
         endif
         call ftgkys(iunit, 'DATE',   c_date, commnt, ftstat)
         if (ftstat.ne.0) go to 999
         write(contxt, '(a, a, a, a)') 'Creator of the File:', author,
     $        ' Creation date:', c_date(1:20)
         call fprint(ounit, outopn, contxt)

c     Obtain important mode dependent parameters.

         if (datmod.eq.'MPC') then
            ph_name = 'PHA'
            xbsize = 0
            ybsize = 0
            risbin = 0
            sbsize = 0
         else
c     Read the number of X-bins
            call ftgkyj(iunit, 'RAWXBINS', xbsize, commnt, ftstat)
            if (ftstat.ne.0) go to 999

c     Read the number of Y-bins
            call ftgkyj(iunit, 'RAWYBINS', ybsize, commnt, ftstat)
            if (ftstat.ne.0) go to 999
c     Read the number of RISE-TIME bins
            call ftgkyj(iunit, 'RISEBINS', risbin, commnt, ftstat)
            if (ftstat.ne.0) go to 999

c     Read the number of Spread bins
            call ftgkyj(iunit, 'SP_BINS',  sbsize, commnt, ftstat)
            if (ftstat.ne.0) go to 999
         endif

c     Read the number of PHA-bins
         call ftgkyj(iunit, 'PHA_BINS', phabin, commnt, ftstat)
         if (ftstat.ne.0) go to 999

c     Read the number of Timing bins
         call ftgkyj(iunit, 'TIMEBINS', timebin, commnt, ftstat)
         if (ftstat.ne.0) go to 999

c     Now X, Y, PH, Rise_Time  binsize are set.

c     Print X, Y, PH, Rise_Time bin size
         write(contxt, '(a, i3, a, i3, a, i3, a, i3)')
     $        'X-bin size:', xbsize, ' Y-bin size:', ybsize,
     $        ' Rise_time bin size:', risbin,
     $        ' Spread bin size:', sbsize
         call fprint(ounit, outopn, contxt)
         write(contxt, '(a, i4, 1x, a, i4)') 'PHA bin size:', phabin,
     $        'TIME bin size:', timebin
         call fprint(ounit, outopn, contxt)

C     Modal parameters

         write(contxt, '(80x)')
         call fprint(ounit, outopn, contxt)
         contxt = '** GIS Modal Parameters **'
         call fprint(ounit, outopn, contxt)
C     nkwd is the number of keywors in the header
         call ftghsp(iunit, nkwd, keyadd, ftstat)
         if (ftstat.ne.0) go to 999
         j = nkwd
         do 50 i = 1, nkwd
            call ftgrec(iunit, i, contxt, ftstat)
            if(contxt(1:5).eq.'POWER') j = i
            if(i.ge.j) call fprint(ounit, outopn, contxt)
 50      continue

C     Get the HV-H
         call ftgkyj(iunit, 'HVH_LVL', hvhlvl, commnt, ftstat)
         if (ftstat.ne.0) go to 999

C     Get the HV-L
         call ftgkyj(iunit, 'HVL_LVL', hvllvl, commnt, ftstat)
         if (ftstat.ne.0) go to 999

C     Get the gain
         call ftgkyj(iunit, 'GAIN', gain,  commnt, ftstat)
         if (ftstat.ne.0) go to 999

C     Get the lower energy discriminator
         call ftgkyj(iunit, 'LE_DS', le_ds,  commnt, ftstat)
         if (ftstat.ne.0) go to 999

C     Get the CPU
C     In frfread 2.980, sometimes CPU_SEL is missing.
C     frfread 2.980 (93/05/13).
         call ftgkys(iunit, 'CPU_SEL', cpusel, commnt, ftstat)
         if (ftstat.ne.0) go to 999

C     Get Position determination method
         call ftgkys(iunit, 'POS_DET', posdet, commnt, ftstat)
         if (ftstat.ne.0) go to 999

C     Get RT_LD
         call ftgkyj(iunit, 'RT_LD', rt_ld, commnt, ftstat)
         if (ftstat.ne.0) go to 999

C     Get RT_UD
         call ftgkyj(iunit, 'RT_UD', rt_ud, commnt, ftstat)
         if (ftstat.ne.0) go to 999

C     Get RT_B_CD
         call ftgkyj(iunit, 'RT_B_CD', rt_bcd, commnt, ftstat)
         if (ftstat.ne.0) go to 999

C     Get R_DSCR
         call ftgkys(iunit, 'R_DSCR', r_dscr, commnt, ftstat)
         if (ftstat.ne.0) go to 999

C     GET S_DSCT
         call ftgkys(iunit, 'S_DSCR', sp_dscr, commnt, ftstat)
         if (ftstat.ne.0) go to 999

C     Jump to the fist extension
         call ftmrhd(iunit, 1, htype, ftstat)
         if (ftstat.ne.0) then
            write(contxt, '(a34, i3)')
     $           'Error moving to the extension number', 1
            call fcerr(contxt)
            go to 999
         endif
C     error if the extention is not the binary extension
         if (htype.ne.2) then
            contxt = 'Extension type not supported'
            call fcerr(contxt)
            go to 999
         endif

C     Again get the PHA_BINS and TIMEBINS.  This is not actually
C     needed, but there were such cases in Frfread 3.018 MPC mode
C     that the PHA_BINS and TIMEBINS in the primary were wrong,
C     but still correct in the 1-st extension.

c     Read the number of PHA-bins
         call ftgkyj(iunit, 'PHA_BINS', phabin, commnt, ftstat)
         if (ftstat.ne.0) go to 999

c     Read the number of Timing bins
         call ftgkyj(iunit, 'TIMEBINS', timebin, commnt, ftstat)
         if (ftstat.ne.0) go to 999

C     Get the nominal RA and DEC
         call ftgkye(iunit, 'RA_PNT', ra_nom, commnt, ftstat)
         if (ftstat.ne.0) then
            ftstat = 0
            call ftgkye(iunit, 'RA_NOM', ra_nom, commnt, ftstat)
            if (ftstat.ne.0) go to 999
         endif
         call ftgkye(iunit, 'DEC_PNT', decnom, commnt, ftstat)
         if (ftstat.ne.0) then
            ftstat = 0
            call ftgkye(iunit, 'DEC_NOM', decnom, commnt, ftstat)
            if (ftstat.ne.0) go to 999
         endif
         call ftgkye(iunit, 'EQUINOX', eqinox, commnt, ftstat)
         if (ftstat.ne.0) then
            eqinox = 0
            ftstat = 0
         endif

         write(contxt, '(a, f7.2, 1x, 1a, f7.2, a, f5.0, a)')
     $        'Nominal RA, DEC:', ra_nom, ',', decnom,
     $        ' (EQUINOX=', eqinox, ')'
         call fprint(ounit, outopn, ' ')
         call fprint(ounit, outopn, contxt)

C     Now the file is GIS science file, PH, PCAL or MPC mode data.
         write(contxt, '(80x)')
         call fprint(ounit, outopn, contxt)

c     Now the file is fine.  Start the real job.

c     Read the number of rows
         call ftgkyj(iunit, 'NAXIS2', nrows, commnt, ftstat)
         if (ftstat.ne.0) go to 999
         if(datmod.eq.'PH'.or.datmod.eq.'PCAL') then
            nevent = nrows
            write(contxt, '(a, i12)') 'Number of total events     :',
     $           nevent
         elseif(datmod.eq.'MPC') then
c     In MPC mode, number of events is not nrow
            write(contxt, '(a, i12)') 'Number of rows:', nrows
         endif
         call fprint(ounit, outopn, contxt)

c     If number of the events is 0, then END
         if(nrows.eq.0) then
            write(contxt, *) 'Number of events = 0. Not print figure.'
            call fcerr(contxt)
            go to 999
         endif

c     illevt is the number of ill events in PH and PCAL mode
         illevt = 0

c     Initialize number of events for MPC mode
         if(datmod.eq.'MPC') nevent = 0

c     Determine the binlen (bin-width for the light curve)
c     binlen shoule be 2**n, where 256*2**n > tend - tstart > 256**2**(n-1)
         binlen = REAL( (tend - tstart) /256.0D0 )
         binlen = log10(binlen)/log10(2.0)
         binlen = 2.0**(INT(binlen) + 1)

c     Find the column number of the DX, DY, rise_ time and time
c     (different for defferent modes)
C     column names have change to RAWX, RAWY from DX, DY. 199/05/13
         if(datmod.eq.'PH'.or.datmod.eq.'PCAL') then
            call ftgcno(iunit, .false., xname, x_cnum, ftstat)
            if (ftstat.ne.0) then
               write(contxt,*) 'Could not get ', xname ,'.'
               call fcecho(contxt)
               go to 999
            endif
            call ftgcno(iunit, .false., yname, y_cnum, ftstat)
            if (ftstat.ne.0) then
               write(contxt,*) 'Could not get ', yname ,'.'
               call fcecho(contxt)
            endif
            call ftgcno(iunit, .false., 'PHA', pha_cnum, ftstat)
            if (ftstat.ne.0) then
               write(contxt,*) 'Could not get PHA.'
               call fcecho(contxt)
               go to 999
            endif
            call ftgcno(iunit, .false., ph_name, ph_cnum, ftstat)
            if (ftstat.ne.0) then
               write(contxt,*) 'Could not get ', ph_name ,'.'
               call fcecho(contxt)
               go to 999
            endif
            call ftgcno(iunit, .false., rt_name, r_cnum, ftstat)
            if (ftstat.ne.0) then
               write(contxt,*) 'Could not get ', rt_name ,'.'
               call fcecho(contxt)
               go to 999
            endif
         endif
         call ftgcno(iunit, .false., 'TIME', t_cnum, ftstat)
         if (ftstat.ne.0) go to 999
         if (datmod.eq.'PH') then
C     From frfread2.980, correct TZERO and TSCALE are set for RISE_TIME
C     Reset the TSCALE for the RAWX, RAWY and PHA to be one

C     From frfread 3**, TZERO and TSCALE are removed for RISE_TIME.
C     So no longer TSCALE and TZERO are used.   94/04/19
C     Instead, RT_B_CD (rt_bcd) and RT_LD (rt_ld) are used.
C     RISE_TIME (physical) = RT_LD + RISE_TIME (tlmout) *2^RT_B_CD

C     Additionally, pha_cnum (PHA column number) is correctly read using
C     using ftgcno.  Previously, pha_cnum=3 was used incorrectly (this was
C     so in old versions of FRFread). 94/04/19

            call fttscl(iunit, x_cnum, 1.0D0, 0.0D0, ftstat)
            if (ftstat.ne.0) go to 999
            call fttscl(iunit, y_cnum, 1.0D0, 0.0D0, ftstat)
            if (ftstat.ne.0) go to 999
            call fttscl(iunit, pha_cnum, 1.0D0, 0.0D0, ftstat)
            if (ftstat.ne.0) go to 999
            call fttscl(iunit, r_cnum, 1.0D0, 0.0D0, ftstat)
            if (ftstat.ne.0) go to 999
         endif

c     Initialize image, pha, light curve, pha-rise time arrays
         do 5 i = 1, 256
            l_chst(i) = 0
            h_chst(i) = 0
            l_ctr(i) = 0
            h_ctr(i) = 0
            phampc(i) = 0
            do 3 j = 1, 256
c     image( (i-1)*256 + j-1 ) = 0.0
               image(i - 1, j - 1) = 0.0
 3          continue
 5       continue
         do 1 i = 1, 1024
            pha(i - 1) = 0.0
            phactr(i - 1) = 0.0
            do 7 j = 1, 256
c     ph_rt( (i-1)*256 + j-1 ) = 0.0
               ph_rt(i - 1, j - 1 ) = 0.0
 7          continue
 1       continue

c     num is the number of bins for light curve
c     tbstrt = start time of each light curve bin
         num = 1
         tbstrt = tstart

c     Read the data to the end
         do 10 i =1, nrows

            if(datmod.eq.'PH'.or.datmod.eq.'PCAL') then
c     PH or PCAL mode
c     Get X-value
               call ftgcvj(iunit, x_cnum, i, 1, 1, 0, xvalue, anyf,
     $              ftstat)
               if (ftstat.ne.0) go to 999
c     Get Y-value
               call ftgcvj(iunit, y_cnum, i, 1, 1, 0, yvalue, anyf,
     $              ftstat)
               if (ftstat.ne.0) go to 999
c     Events  with X, Y > 241 are 'ill' events (calculation error)
               if(xvalue.lt.241*xbsize/256.and.yvalue.lt.
     $              241*ybsize/256)  then

c     Make image data
                  if(xvalue.ge.0.and.xvalue.le.xbsize.and.
     $                 yvalue.ge.0.and.yvalue.le.ybsize) then
                     image(xvalue, yvalue) =
     $                    image(xvalue, yvalue) + 1.0
                  else
C     X, Y values out of range
                     write(contxt,*)
     $                    'Warning: X or Y values out of range'
                     call fcecho(contxt)
                     write(contxt,*)
     $                    'X =', xvalue, 'Y =', yvalue
                     call fcecho(contxt)
                  endif

c     Get PHA value
                  call ftgcvj(iunit, pha_cnum, i, 1, 1, 0, phaval, anyf,
     $                 ftstat)
                  if (ftstat.ne.0) go to 999

c     Get PHA or PI value
                  call ftgcvj(iunit, ph_cnum, i, 1, 1, 0, phval, anyf,
     $                 ftstat)
                  if (ftstat.ne.0) go to 999

                  if(phval.ge.0.and.phval.lt.phabin) then
c     Make PHA data
                     pha(phval) = pha(phval) + 1
c     Make PHA data for the centeral events (r < 15 arcmin)
c     (assuming 1 arcmin ~ 5 channels)
                     if(  ( (xvalue*256/xbsize - 120)**2
     $                    + (yvalue*256/ybsize - 120)**2 )
     $                    .lt. 75**2 ) then
                        phactr(phval) = phactr(phval) + 1
                     endif

c     Get Rise time value
                     call ftgcvj(iunit, r_cnum, i, 1, 1, 0, ristim,
     $                    anyf, ftstat)

                     if(rt_name.eq.'RISE_TIME') then
C     Reproduce the physical RISE_TIME using
C     RISE_TIME(physical) = RT_LD + RT_TIME(tlmout)*2^RT_B_CD
C     94/04/19
                        ristim = rt_ld + ristim * 2**rt_bcd
                     endif

                     if (ftstat.ne.0) go to 999
                     if(ristim.ge.0.and.ristim.le.255) then
c     Make PHA-Rise time matrix.
c     Always PHA, not PI, is used for the X-axis regardless of the
C     PHA. PI selection for spectra.  94/04/19
                        ph_rt(phaval, ristim) =
     $                       ph_rt(phaval, ristim) + 1.0
                     else
                        write(contxt, *)
     $                       'Warning: Rise Time value out of range'
                        call fcecho(contxt)
                        write(contxt, *) 'Rise Time =', ristim
                        call fcecho(contxt)
                     endif
                  else
C     PHA out of range
                     write(contxt, *)
     $                    'Warning: PHA/PI value out of range'
                     call fcecho(contxt)
                     write(contxt, *) 'PHA/PI =', phval
                     call fcecho(contxt)
                  endif

c     Get the time of events
                  call ftgcvd(iunit, t_cnum, i, 1, 1, 0.D0, time, anyf,
     $                 ftstat)
                  if (ftstat.ne.0) go to 999

c     Create the light curve array
                  if(time.gt.(tbstrt+DBLE(binlen))) then
c     Proceed to the next time bin if the time is larger than the end
c     of the current light curve time bin
                     num = num + 1
                     tbstrt = tbstrt + DBLE(binlen)
                  endif

                  if(phval.lt.phabin/2) then
c     Lower energy band
                     l_chst(num) = l_chst(num) + 1.0
                     if(  ( (xvalue*256/xbsize - 120)**2
     $                    + (yvalue*256/ybsize - 120)**2 )
     $                    .lt. 75**2 ) then
c     .. and Centeral region
                        l_ctr(num) = l_ctr(num) + 1.0
                     endif
                  else
c     higher energy band
                     h_chst(num) = h_chst(num) + 1.0
                     if(  ( (xvalue*256/xbsize - 120)**2
     $                    + (yvalue*256/ybsize - 120)**2 )
     $                    .lt. 75**2 ) then
c     .. and Centeral region
                        h_ctr(num) = h_ctr(num) + 1.0
                     endif
                  endif

               else
c     This is an anomaly event
                  illevt = illevt + 1
               endif

            elseif (datmod.eq.'MPC') then

c     MPC mode
c     Get the time of events
               call ftgcvd(iunit, t_cnum, i, 1, 1, 0.D0, time, anyf,
     $              ftstat)
               if (ftstat.ne.0) go to 999
               if(time.gt.(tbstrt+DBLE(binlen))) then
c     If time is greater than the end of the current bin, create
c     the next time bin
                  num = num + 1
                  tbstrt = tbstrt + DBLE(binlen)
               endif

c     Obtain PHA data, and make PHA array and light curve array
               call ftgcvj(iunit, 4, i, 1, phabin, 0,
     $              phampc, anyf, ftstat)
               if (ftstat.ne.0) go to 999
               do 100 j = 1, phabin
                  pha(j - 1) = pha(j - 1) + phampc(j)
                  nevent = nevent + phampc(j)
c     Lower energy band of the light curve array
                  if(j.le.phabin/2) then
                     l_chst(num) = l_chst(num) + phampc(j)
c     higher energy band of the light curve array
                  else
                     h_chst(num) = h_chst(num) + phampc(j)
                  endif
 100           continue

            endif

 10      continue

c     Print number of anomaly events (PH and PCAL mode) or
c     total number of events (MPC mode).
         if(datmod.eq.'PH'.or.datmod.eq.'PCAL') then
            write(contxt, '(a, i12)') 'Number of anomaly events :',
     $           illevt
            call fprint(ounit, outopn, contxt)
         elseif(datmod.eq.'MPC') then
            write(contxt, '(a, i12)') 'Number of events:', nevent
            call fprint(ounit, outopn, contxt)
         endif

c     Start pgplot
         call pgbegin(0, device, 2, 2)

c     Draw image
         call imgplt(256, xbsize, ybsize, image, vinfle(ifile),
     $        instrm(1:4), datmod, nevent, illevt, cpusel,
     $        posdet(1:4),xname, yname,
     $        contour, gray, dotmap, imagelog, exposure,
     $        r_dscr, sp_dscr, origin, object, piname, bitrat,
     $        ra_nom, decnom, eqinox, author, c_date, tlmfle,
     $        phabin, risbin, sbsize, timebin)

c     Draw pha spectrum
         call phaplt(phabin, pha, phactr, hvhlvl, hvllvl, gain, le_ds,
     $        ph_name, phalog, englog)

c     Draw the light curve
         call pltlcv(phabin, tstart, tend, binlen,
     $        num, l_chst, h_chst, l_ctr, h_ctr)

c     Plot the pha-rise time diagram (gray map)
         call phartg(1024, phabin, risbin, rt_bcd, REAL(rt_ld),
C     $        REAL(rt_ud),  ph_rt, ph_name, rt_name)
     $        REAL(rt_ud),  ph_rt, 'PHA', rt_name)
C     Draw always PHA in the horizontal axis, even PI is chosen for the
C     spectral figure. (94/04/19)

c     End PGPLOT
         call pgend

 999     continue
         if(ftstat.ne.0) call fcerrm(ftstat)
         if (inopen) then
            ftstat = 0
            call ftclos(iunit, ftstat)
         endif
 1000 continue
      return
      end

cccccccccc
c     imgplt ---- draw the contour map and gray map of the image
c     Ken Ebisawa NASA/GSFC 1993/04/28
c     PGPLOT library subroutines are called.
c
c     modified Aug.5 93 Ken Ebisaa
ccccccccc
      subroutine imgplt(xdim, xbsize, ybsize, image, infile, instrm,
     $     datmod, events, illevt, cpusel, posdet, xname, yname,
     $     contour, gray, dotmap, imagelog, exposure, r_dscr, sp_dscr,
     $     origin, object, piname, bit_rate,
     $     ra_nom, decnom, equinox, author, c_date, tlmfile,
     $     phabin, risbin, sbsize, timebin)

C     implicit none

c     All the arguments are 'input'.

c     xdim   -- X-dimension of the actual array size
c     xbsize -- X-dimension of the image
c     ybsize -- Y-dimension of the image
c     image(xbsize, ybsize) -- image data
c     events -- total number of events
c     illevt -- total number of anomaly events
c     phabin, risbin, sbsize -- PHA, RISE time, Spread bin size
c     timebin -- number of timing bins
      integer xdim, events, illevt
      integer xbsize, ybsize, phabin, risbin, sbsize, timebin
c     real image(xbsize, ybsize)
      real image(xdim, ybsize)

c     infile  -- fits file name
c     instrm  -- instrument name
c     datmod  -- data mode
c     cpusel -- cpu selection, posdet -- position determination method
c     xname, yname -- column name of x and y
c     r_dscr, sp_dscr -- radial discri, spread discri on/off
c     origin -- origin of the file
c     object -- target, authour -- program to create the file,
c     c_date -- creation of the file, tlmfile -- original frf
      character*(*) infile, instrm, posdet, datmod, cpusel,
     $     xname, yname, r_dscr, sp_dscr, origin, object, piname,
     $     bit_rate, author, c_date, tlmfile

c     plot the coutour, gray, dotmap (true) or not (false);
c     plot image in the log scale (true) or linear (false)
      logical contour, gray, dotmap, imagelog

c     exposure time
      real exposure, ra_nom, decnom, equinox

      integer NC
      real im_min, im_max, c_int, C(16), TR(6), fg, bg
      integer i, j

c     for annotation
      character(30)  text

c     font size
      real fsize

c     Define the new page
      call pgpage
      call pgvport(0.204, 0.796, 0.1, 0.9)
      if (datmod.ne.'MPC') then
         call pgwindow(0.0, REAL(xbsize), 0.0, REAL(ybsize))
      else
         call pgwindow(0.0, 256.0, 0.0, 256.0)
      endif

c     large font size
      fsize = 1.2
      call pgsch(fsize)

      if(datmod.ne.'MPC') then
c     find min and max of image
         im_min = 1e30
         im_max = 1e-30

         do i = 1, xbsize
            do j = 1, ybsize
               if (image(i, j).gt.im_max) im_max = image(i, j)
               if (image(i, j).lt.im_min) im_min = image(i, j)
            end do
         end do

         bg = im_min
         fg = im_max

c     Now image_min and image_max are the min and max of
c     the image(i, j)

c     number of contours
         NC = 8

         if(imagelog) then
c     Determine the Contour level (log NC levels)
            if(im_max.gt.0.0) then
               im_max = log10(im_max)
            else
               im_max = 1e-5
            endif
            if(im_min.gt.0.0) then
               im_min = log10(im_min)
            else
               im_min = 1e-5
            endif
            c_int = (im_max - im_min)/(NC - 1)
            do  25 i = 1, NC
               C(i) =10.0**(im_min + (i-1)*c_int)
 25         continue
         else
c     Determine the Contour level (linear NC levels)
            c_int = (im_max - im_min)/ (NC - 1)
            do 30 i = 1, NC
               C(i) = im_min + (i-1)*c_int
 30         continue
         endif
c     parameters needed in the pgcont task
         TR(1) = -1.0
         TR(2) = 1.0
         TR(3) = 0.0
         TR(4) = -1.0
         TR(5) = 0.0
         TR(6) = 1.0

         if (gray) then
c     Plot the gray map
            call pggray(image, xdim, ybsize, 1, xbsize, 1, ybsize,
     $           fg, bg, TR)
         endif

         if(contour) then
c     Plot the contour map
            call pgcont(image, xdim, ybsize, 1, xbsize, 1, ybsize,
     $           C, NC, TR)
         endif

         if(dotmap) then
C     Plot the dotmap
            do j = 1, ybsize
               do i = 1, xbsize
                  if(image(i,j).gt.0.0) then
c     do k = 1, INT(image(i,j))
                     call pgpoint(1, REAL(i-1), REAL(j-1), -1)
c     end do
                  endif
               end do
            end do
         endif

      endif
c     ...... of the if(datmod.ne.'MPC')

c     Draw the box
c     (due to such a bug in PGPLOT that box is erased when gray map is
c     drawn, the box is drawn in the end)
      call pgbox('bcnst', 0.0, 0, 'bcnst', 0.0, 0)
      call pglabel(xname, yname, ' ')
      text = instrm//' image'
      call pgmtxt('t', 0.40, 0.5, 0.5, text)
c     Add anotations (at the top of the image)
      text = 'MODE='//datmod
      call pgmtxt('t', -1.5, 0.02, 0.0, text)
      text = 'CPU ='//cpusel
      call pgmtxt('t', -2.7, 0.02, 0.0, text)
      text = posdet//' method'
      call pgmtxt('t', -3.9, 0.02, 0.0, text)
      write(text, '(a, i6)') 'Total events   =', events
      call pgmtxt('t', -1.5, 0.52, 0.0, text)
      write(text, '(a, i6)') 'Anomaly events=', illevt
      call pgmtxt('t', -2.5, 0.52, 0.0, text)

c     Add anotations (at the bottome of the image)
      write(text, '(a, 1p, e8.2, a)') 'Exposure=', exposure, ' sec'
      call pgmtxt('b', -0.5, 0.02, 0.0, text)
      write(text, '(a, a)') 'SP_DSCR:', sp_dscr
      call pgmtxt('b', -0.5, 0.50, 0.0, text)
      write(text, '(a, a)') 'R_DSCR:', r_dscr
      call pgmtxt('b', -0.5, 0.77, 0.0, text)

c     Add anotations (righthand side of the image)
      write(text, '(a,a)') 'Object:', object
      call pgmtxt('t', -1.5, 1.01, 0.0, text)
      write(text, '(a,a)') 'PI:', piname
      call pgmtxt('t', -2.7, 1.01, 0.0, text)
      write(text, '(a,a)') 'BIT RATE:', bit_rate
      call pgmtxt('t', -3.9, 1.01, 0.0, text)
      write(text, '(a, f5.1)') 'RA(deg) :', ra_nom
      call pgmtxt('t', -5.1, 1.01, 0.0, text)
      write(text, '(a, f5.1)')  'DEC(deg):', decnom
      call pgmtxt('t', -6.3, 1.01, 0.0, text)
      write(text, '(a, f5.0)')  'EQUINOX:', equinox
      call pgmtxt('t', -7.5, 1.01, 0.0, text)
      write(text, '(a, i3)')  'RAWXBINS:', xbsize
      call pgmtxt('t', -8.7, 1.01, 0.0, text)
      write(text, '(a, i3)')  'RAWYBINS:', ybsize
      call pgmtxt('t', -9.9, 1.01, 0.0, text)
      write(text, '(a, i4)')  'PHA_BINS:', phabin
      call pgmtxt('t', -11.1, 1.01, 0.0, text)
      write(text, '(a, i3)')  'RISEBINS:', risbin
      call pgmtxt('t', -12.3, 1.01, 0.0, text)
      write(text, '(a, i3)')  'SP_BINS:', sbsize
      call pgmtxt('t', -13.5, 1.01, 0.0, text)
      write(text, '(a, i4)')  'TIMEBIN:', timebin
      call pgmtxt('t', -14.7, 1.01, 0.0, text)

      write(text, '(a,a)') 'ORIGIN:', origin
      call pgmtxt('b', -5.3, 1.01, 0.0, text)
      write(text, '(a,a)') 'CREATOR:', author
      call pgmtxt('b', -4.1, 1.01, 0.0, text)
      write(text, '(a,a)') 'CREATION:', c_date(1:10)
      call pgmtxt('b', -2.9, 1.01, 0.0, text)
      write(text, '(a)') 'TLM_FILE:'
      call pgmtxt('b', -1.7, 1.01, 0.0, text)
      call pgmtxt('b', -0.5, 1.01, 0.0, tlmfile)

c     write the title of the page
      call pgsch(2.)
      call pgmtxt('t', 1.2,  1.3, 0.5, infile)
      call pgsch(fsize)
      end

cccccccccc
c     phaplt ---- plot the pha spectrum
c     Ken Ebisawa NASA/GSFC 1993/04/28
c     PGPLOT library subroutines are called.
ccccccccc
      subroutine phaplt(phabin, pha, phactr, hvhlvl, hvllvl,
     $     gain, le_ds, ph_name, phalog, englog)

C      implicit none

c     phabin -- pha bin number,  pha(phabin) -- pha data
      integer phabin
      real pha(phabin), phactr(phabin)

c     HVH level, HVL level, GAIN level, Lower energy discriminator
c     (for annotation)
      integer hvhlvl, hvllvl, gain, le_ds

c     Name of the PH column (PHA or PI)
      character(3) ph_name

c     Plot log scale or not
      logical phalog, englog

c     X-coordinate for the plot
      real X(1024)

c     Diagram min and max values
      real xmin, xmax, ymin, ymax

c     for annotation
      character(25) text
      character(8) xset, yset
      real X1, X2, X3, Y1, Y2, Y3, Y4

      integer i

c     obtain max of the counts
      ymax=0.0
      do 10 i = 1, phabin
         if(ymax.lt.pha(i)) ymax = pha(i)
 10   continue

c     Draw the box
      call pgpage
      call pgvstand

      if(ymax.eq.0.0) ymax = 1.0
      if(phalog) then
         ymin = log10(0.9)
         ymax = 1.05 * log10(ymax)
         yset = 'bclnst'
      else
         ymin = 0.0
         ymax = 1.05 * ymax
         yset = 'bcnst'
      endif
      if(englog) then
         xmin = log10(0.3)
         xmax = log10(13.0)
         xset = 'bclnst'
      else
         xmin = 0.0
         xmax = REAL(phabin)
         xset = 'bcnst'
      endif
      call pgwindow(xmin, xmax, ymin, ymax)
      call pgbox(xset, 0.0, 0, yset, 0.0, 0)

c     Print labels
      if(englog) then
         text = ph_name//' (keV)'
         call pglabel(text, 'counts/bin', ' ')
      else
         call pglabel(ph_name, 'counts/bin', ' ')
      endif
      call pgmtxt('t', 0.40, 0.5, 0.5, 'Pulse Height Spectrum')

c     Print annotation
      X1 = 0.65 * (xmax - xmin) + xmin
      X2 = 0.75 * (xmax - xmin) + xmin
      X3 = 0.80 * (xmax - xmin) + xmin
      Y1 = 0.91 * (ymax - ymin) + ymin
      Y2 = 0.90 * (ymax - ymin) + ymin
      Y3 = 0.86 * (ymax - ymin) + ymin
      Y4 = 0.85 * (ymax - ymin) + ymin

      call pgmove(X1, Y1)
      call pgdraw(X2, Y1)
      call pgtext(X3, Y2, 'total')
      call pgsls(4)
      call pgsci(2)
      call pgmove(X1, Y3)
      call pgdraw(X2, Y3)
      call pgsci(1)
      call pgtext(X3 , Y4, 'r < 15 arcmin')
      call pgsls(1)

      write(text, '(a, i1)') 'HV-H level: ', hvhlvl
      call pgmtext('t', -5.5, 0.66, 0.0, text)

      write(text, '(a, i1)') 'HV-L level: ', hvllvl
      call pgmtext('t', -6.7, 0.66, 0.0, text)

      write(text, '(a, i2)') 'Gain level: ', gain
      call pgmtext('t', -7.9, 0.66, 0.0, text)

      write(text, '(a, i1)') 'Lower discri level: ', le_ds
      call pgmtext('t', -9.1, 0.66, 0.0, text)

      do 100 i = 1, phabin
         X(i) = REAL(i - 1)
         if(englog) then
            X(i) = 5.9/(500.*phabin/1024)*X(i)
            if(X(i).gt.0.0) then
               X(i) = log10(X(i))
            else
               X(i) = -999.
            endif
         endif
         if(phalog) then
            if(pha(i).gt.0.0) then
               pha(i) = log10(pha(i))
            else
               pha(i) = -999.
            endif
            if(phactr(i).gt.0.0) then
               phactr(i) = log10(phactr(i))
            else
               phactr(i) = -999.
            endif
         endif
 100  continue

c     Draw the PHA
      if(phabin.gt.1) then
         call pgbin(phabin, X, pha, .false.)
      else
c     There seems to be a bug in pgbin such that it does not draw lines when
c     bin-number is 1.
         call pgmove(0.0, pha(1))
         call pgdraw(1.0, pha(1))
      endif

c     Draw the PHA for the center region with a dotted line
      call pgsls(4)
      call pgsci(2)
      call pgbin(phabin, X, phactr, .false.)
      call pgsls(1)
      call pgsci(1)
      end

cccccccccc
c     pltlcv ---- plot the light curve
c     Ken Ebisawa NASA/GSFC 1993/04/28
c     PGPLOT library subroutines are called.
ccccccccc
      subroutine pltlcv(phabin, tstart, tend, binlen, num,
     $     l_chst, h_chst, l_ctr, h_ctr)

C      implicit none

c     phabin: number of pha channels
      integer phabin

c     tstart, tend:start and end of the data, binlen: length of a bin
      double precision tstart, tend
      real binlen

c     num:number of points, l_chst(num):count history of the low band
c     h_chst(num):count history of the high band
c     l_ctr(num), h_ctr(num): count history in the central region
c     low  band is PHA(0) - PHA(phabin/2-1)
c     high band is PHA(phabin/2) - PHA(phabin-1)
      integer num
      real l_chst(num), h_chst(num), l_ctr(num), h_ctr(num)

c     time is the array of the start time of the bins
      real time(256)

c     date and UT variables
      integer year, month, day, hour, minute, itstrt, itend, it
      real second
      character(10) date
      character(8)  ut

c     i is used for a do loop, intvl is the interval between the UT labels
      integer i, intvl

c     cbinlen is the chracter bin width
      character(4) cblen

c     max is the max of the l_chst or h_chst
      real max

c     xlabel = x-label (e.g. 'seconds from 7710900.3 = 1993/03/30 12:23:60')
      character(70)  xlabel

c     clabel = y-label for the range of channels
c     (e.g. 'ch 512-1024')
      character(11) clabel

c     for annotation
      character(25) text

      do i = 1, num
         time(i) =  (i-1) * binlen
      end do

      if(binlen.gt.1.0) then
         write(cblen, '(i4)') INT(binlen)
      else
         write(cblen, '(f4.2)') binlen
      endif

c     Start a new page
      call pgpage

cccccccccccc
c     lower viewport
c
c     obtain max of l_chst
      max = 0.0
      do 300 i = 1, num
         if(max.lt.l_chst(i)) max = l_chst(i)
 300  continue
      max = max * 1.1
      if(max.eq.0.0) max = 1.0

c     Draw the lower box
      call pgvport(0.10, 0.92, 0.12, 0.5)
      call pgwindow(0.0, REAL(tend - tstart), 0.0, max)
      call pgbox('bcnst', 0.0, 0, 'bcnst', 0.0, 0)

c     Print X-label

C      call fisct3(tstart, day, hour, minute, second)
C      call fid3my(1, 1, 1993, day, month, year)
C     New routine for ASCA time --> UT conversion 94/04/21
      call asca2ut(tstart, day, month, year, hour, minute, second)

      write(xlabel,
     $     '(a, f11.1, 1x,a,
     $     i4, a1, i2.2, a1, i2.2, 1x,
     $     i2.2, a1, i2.2, a1, f4.1, a)')
     $     'Seconds from ',  tstart, '(ASCA time), ',
     $     year, '-', month, '-', day,
     $     hour, ':', minute, ':', second, '(UT)'

      text = 'counts /'//cblen//' sec'
      call pglabel(xlabel, text, ' ')

c     write the channel label along the left edge
      write(clabel,'(a,i3)') 'ch 0 - ', phabin/2 - 1
      call pgmtxt('L', 3.2, 0.5, 0.5, clabel)

c     Plot light curve of the lower energy band
      call pgbin(num, time, l_chst, .false.)

c     Plot light curve of the central region
      call pgsls(4)
      call pgsci(2)
      call pgbin(num, time, l_ctr, .false.)
      call pgsci(1)
      call pgsls(1)
ccccccccccc


ccccccccccc
c     upper viewport
c
c     obtain max of h_chst
      max = 0.0
      do 400 i = 1, num
         if(max.lt.h_chst(i)) max = h_chst(i)
 400  continue

c     Draw the lower and vertical borders of the box
      call pgvport(0.10, 0.92, 0.5, 0.88)
      max = max * 1.1
      if(max.eq.0.0) max = 1.0
      call pgwindow(0.0, REAL(tend - tstart), 0.0, max)
      call pgbox('bst', 0.0, 0, 'bcnst', 0.0, 0)
      text = 'counts /'//cblen//' sec'
      call pglabel(' ', text, ' ')

c     Draw the upper border of the box
      call pgbox('c', 0.0, 0, ' ', 0.0, 0)

c     Write the channel label along the left edge
      write(clabel,'(a2,i3,a1,i4)') 'ch ', phabin/2, '-', phabin-1
      call pgmtxt('L', 3.2, 0.5, 0.5, clabel)

c     Print annotation (explanation of the solid and broken lines)
      call pgmove(0.65*REAL(tend - tstart), 0.89*max)
      call pgdraw(0.75*REAL(tend - tstart), 0.89*max)
      call pgtext(0.8*REAL(tend - tstart) , 0.87 *max,
     $     'total')
      call pgsls(4)
      call pgsci(2)
      call pgmove(0.65*REAL(tend - tstart), 0.81*max)
      call pgdraw(0.75*REAL(tend - tstart), 0.81*max)
      call pgsci(1)
      call pgtext(0.8*REAL(tend - tstart) , 0.80*max,
     $     'r < 15 arcmin')
      call pgsls(1)

c     Plot light curve of the upper energy band
      call pgbin(num, time, h_chst, .false.)

c     Plot light curve of the central region
      call pgsls(4)
      call pgsci(2)
      call pgbin(num, time, h_ctr, .false.)
      call pgsci(1)
      call pgsls(1)
ccccccccccc

c     Determine the number and interval of UT labels
      itstrt = INT(tstart)
      itend = INT(tend)
      call dtmnUT(itstrt, itend, intvl)

c     Now itstrt is the time for of the first UT label (integer) and
c     intvl is the interval of the UT labels (integer)

c     Draw minour ticks (4 minour ticks within one majour tick interval)
      i = (tstart - itstrt) / (REAL(intvl) / 5.0)
 30   continue
      call pgmove(REAL(DBLE(itstrt) + i * (DBLE(intvl) / 5.0D0)
     $     - tstart), max)
      call pgdraw(REAL(DBLE(itstrt) + i * (DBLE(intvl) / 5.0D0)
     $     - tstart), 0.98*max)
      i = i + 1
      if ( (DBLE(itstrt) + i * (DBLE(intvl) / 5.0D0) ).le. tend)
     $     go to 30

c     Add UT labels and draw majour ticks
      i = 0
 10   continue
      i = i + 1
      it = itstrt + intvl*(i - 1)
      if (it.gt.itend) go to 100

C      call fisct3(1.0d0*it, day, hour, minute, second)
C      call fid3my(1, 1, 1993, day, month, year)
C     New routine for ASCA time --> UT conversion 94/04/21
      call asca2ut(1.0d0*it, day, month, year, hour, minute, second)

      write(date, '(i4.4, a1, i2.2, a1, i2.2)')
     $     year, '-', month, '-', day
      if(intvl.lt.60) then
         write(ut,'(i2.2, a1, i2.2, a1, i2.2)')
     $        hour, ':', minute, ':', INT(second)
      else
         write(ut,'(i2.2, a1, i2.2)')
     $        hour, ':', minute
      endif
      call addUT(0.0, REAL(tend - tstart),
     $     max, 0., REAL(it - tstart), date, ut)
      go to 10
 100  continue
      end

cccccccccc
c     dtmnUT ---- determine interval of the UT labels
c     at the top of the graph
c     Ken Ebisawa NASA/GSFC 1993/04/28
ccccccccc
      subroutine dtmnUT(itstrt, itend, intvl)

C     implicit none

c     input -- itstrt, itend:start and end time
c     output -- itstrt: time of the first label
c     intvl : interval of the labels
      integer itstrt, itend, intvl

      intvl = (itend - itstrt) / 5

      if(intvl.lt.1) then
         intvl = 1
      elseif(intvl.lt.2) then
         intvl = 2
      elseif(intvl.lt.5) then
         intvl = 5
      elseif(intvl.lt.10) then
         intvl = 10
      elseif(intvl.lt.15) then
         intvl = 15
      elseif(intvl.lt.30) then
         intvl = 30
      elseif(intvl.lt.60) then
         intvl = 60
      elseif(intvl.lt.120) then
         intvl = 120
      elseif(intvl.lt.300) then
         intvl = 300
      elseif(intvl.lt.600) then
         intvl = 600
      elseif(intvl.lt.900) then
         intvl = 900
      elseif(intvl.lt.1800) then
         intvl = 1800
      elseif(intvl.lt.3600) then
         intvl = 3600
      elseif(intvl.lt.9000) then
         intvl = 9000
      elseif(intvl.lt.18000) then
         intvl = 18000
      else
         intvl = (intvl/18000)*18000
      endif
      itstrt = ( itstrt/intvl + 1) * intvl
      end

cccccccccc
c     addUT ---- write UT labels and draw tick marks
c                at the top of the graph
c     Ken Ebisawa NASA/GSFC 1993/04/28
c     PGPLOT library subroutines are called.
ccccccccc
      subroutine addUT(tstart, tend, max, min, time, date, UT)

C      implicit none

c     tstart, tend: start and end time of the graph.
      real tstart, tend
c     max, min: max and min value of the Y-coordinate
      real max, min
c     time: the time to which UT label is written
      real time
c     date: year, month, date (e.g. 1993-04-07)
      character(10) date
c     ut: UT hour, minute, second  (e.g. 07:05:30)
      character(8) ut

      real xpts(2), ypts(2)

      xpts(1) = time
      xpts(2) = time
      ypts(1) = max
      ypts(2) = 0.97*(max - min)
      call pgline(2, xpts, ypts)
      call pgmtxt('T', 1.3,
     $     (time - tstart)/(tend - tstart),
     $     0.5, date)
      call pgmtxt('T', 0.3,
     $     (time - tstart)/(tend - tstart),
     $     0.5, ut)
      end

cccccccccc
c     phartg ---- plot the pha_rise_time diagram
c     Ken Ebisawa NASA/GSFC 1993/04/28
c     PGPLOT library subroutines are called.
ccccccccc
      subroutine phartg(xsize, phabin, risbin, rt_bcd, rt_ld, rt_ud,
     $     ph_rt, ph_name, rt_name)

C     implicit none

      common /task/ taskname
      character(40) taskname

c     xsize:  size of the 1-st dimension of the matrix
c     phabin: number of pha channels, risbin: bin number for rise_time
c     rt_bcd: rise time bit condense
      integer xsize, phabin, risbin, rt_bcd

c     rt_ld, rt_ud: rise time lower and upper discriminator values
      real rt_ld, rt_ud

c     ph_rt: matrix of rise_tive vs pha value
c     real ph_rt(phabin, 256)
      real ph_rt(xsize, 256)

c     Column names of PHA and RISE_TIME
      character(3) ph_name
      character*(*) rt_name

c     pha(1024) is the pha number, rti_low, rti_upper
c     are the RTI lower/upper boundaries.
      real pha(256), rti_low(256), rti_upper(256)

      integer i, j
      real ld1(2), ld2(2), ud1(2), ud2(2)

c     for annotations
      character(25) text

c     necessary for the pggray task
      real fg, bg, TR(6)

c     Define the view port
      call pgpage
      call pgvstand
      call pgwindow(0.0, REAL(phabin), 0.0, 256.)

      bg = 0.0
c     Obtain the foreground value (maximum in the matrix)
      fg = 0.0
      do i =1, phabin
         do j = 1, 256
            if(ph_rt(i, j).gt.fg) fg = ph_rt(i, j)
         end do
      end do
c     write(*,*) 'fg=', fg

c     parameters needed in the pgcont task
      TR(1) = -1.0
      TR(2) = 1.0
      TR(3) = 0.0
      TR(4) = -1.0
      TR(5) = 0.0
      TR(6) = 1.0

c     Draw the box
      call pgbox('bcnst', 0.0, 0, 'bcnst', 0.0, 0)
      text = ph_name//' (ch)'
      call pglabel(text, rt_name,
     $     'Pulse Height vs Rise Time')

c     Print annotations
      write(text, '(a, i3)') 'Rise time UD = ', INT(rt_ud)
      call pgmtext('b', -4.7, 0.65, 0.0, text)
      write(text, '(a, i3)') 'Rise time LD = ', INT(rt_ld)
      call pgmtext('b', -3.5, 0.65, 0.0, text)
      write(text, '(a, i3,a)') 'Rise time bin = ', risbin,
     $     ' ch'
      call pgmtext('b', -2.3, 0.65, 0.0, text)
      write(text, '(a, i1, a, i1, a)') 'Bit condense = ', rt_bcd,
     $     ' (1/', 2**rt_bcd,')'
      call pgmtext('b', -1.1, 0.65, 0.0, text)

      if (rt_name.eq.'RISE_TIME') then
c     set lower discriminator
         ld1(1) = 0.0
         ld1(2) = REAL(phabin)
         ld2(1) = rt_ld
         ld2(2) = rt_ld

c     set upper discriminator
         ud1(1) = 0.0
         ud1(2) = REAL(phabin)
         ud2(1) = rt_ud
         ud2(2) = rt_ud

c     Draw lower and upper RT discriminator
         call pgline(2, ld1, ld2)
         call pgline(2, ud1, ud2)

      elseif(rt_name.eq.'RTI') then
c     Draw the RTI range
         call rti_range(phabin, rti_low, rti_upper)
         do i = 1, 256
            if(phabin.eq.256) then
               pha(i) = REAL(i-1)
            elseif(phabin.eq.1024) then
               pha(i) = 4.0*REAL(i-1)
            endif
         end do
         call pgline(256, pha, rti_low)
         call pgline(256, pha, rti_upper)
      endif

c     Plot points
      do j = 1, 256
         do i = 1, phabin
            if(ph_rt(i, j).ne.0.0) then
c     do k = 1, INT(ph_rt(i,j))
               call pgpoint(1, REAL(i-1), REAL(j-1), -1)
c     end do
            endif
         end do
      end do
c     write version number
      call pgmtext('b', 3.0, 0.8, 0.0, taskname)
c     write user name bottom of the plot
      call pgiden

      end

cccccccccc
c     rti_range ---- return the valid RTI lower and upper values
c     which depend on PHA
c     Ken Ebisawa NASA/GSFC 1994/04/19
ccccccccc
        subroutine rti_range(phabin, rti_low, rti_upper)
        integer phabin
        real rti_low(phabin), rti_upper(phabin)

        integer i

        do i = 1, 16
           rti_low(i)   = 160.0
        end do
        do i = 1, 9
           rti_upper(i) = 210.0
        end do
        do i = 10, 10
           rti_upper(i) = 209.0
        end do
        do i = 11, 11
           rti_upper(i) = 208.0
        end do
        do i = 12, 12
           rti_upper(i) = 206.0
        end do
        do i = 13, 14
           rti_upper(i) = 205.0
        end do
        do i = 15, 15
           rti_upper(i) = 204.0
        end do
        do i = 16, 16
           rti_upper(i) = 203.0
        end do

        do i = 17, 18
           rti_low(i)   = 161.0
        end do
        do i = 19, 21
           rti_low(i)   = 162.0
        end do
        do i = 17, 17
           rti_upper(i) = 202.0
        end do
        do i = 18, 18
           rti_upper(i) = 201.0
        end do
        do i = 19, 21
           rti_upper(i) = 200.0
        end do

        do i = 22, 27
           rti_low(i)   = 163.0
        end do
        do i = 28, 34
           rti_low(i)   = 164.0
        end do
        do i = 22, 22
           rti_upper(i) = 199.0
        end do
        do i = 23, 25
           rti_upper(i) = 198.0
        end do
        do i = 26, 27
           rti_upper(i) = 197.0
        end do
        do i = 28, 30
           rti_upper(i) = 196.0
        end do
        do i = 31, 34
           rti_upper(i) = 195.0
        end do

        do i = 35, 44
           rti_low(i)   = 165.0
        end do
        do i = 45, 52
           rti_low(i)   = 166.0
        end do
        do i = 53, 68
           rti_low(i)   = 167.0
        end do
        do i = 69, 80
           rti_low(i)   = 168.0
        end do
        do i = 81, 114
           rti_low(i)   = 169.0
        end do
        do i = 35, 38
           rti_upper(i)   = 194.0
        end do
        do i = 39, 44
           rti_upper(i)   = 193.0
        end do
        do i = 45, 52
           rti_upper(i)   = 192.0
        end do
        do i = 53, 61
           rti_upper(i)   = 191.0
        end do
        do i = 62, 74
           rti_upper(i)   = 190.0
        end do
        do i = 75, 94
           rti_upper(i)   = 189.0
        end do
        do i = 95, 114
           rti_upper(i)   = 188.0
        end do

        do i =115, 140
           rti_low(i)   = 170.0
           rti_upper(i) = 187.0
        end do
        do i =141, 155
           rti_low(i)   = 171.0
        end do
        do i = 115, 150
           rti_upper(i) = 187.0
        end do
        do i = 156, 200
           rti_low(i)   = 172.0
        end do
        do i = 151, 200
           rti_upper(i) = 186.0
        end do
        do i = 201, 256
           rti_low(i)   = 173.0
           rti_upper(i) = 185.0
        end do

        end
