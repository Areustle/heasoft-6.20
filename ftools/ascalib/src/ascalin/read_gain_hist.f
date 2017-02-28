C******************************************************************************
C     SUBROUTINE:
C
C     read_gain_hist
C
C     DESCRIPTION:
C
C     Opens and reads in gain history records for the given observation file.
C
C     AUTHOR/DATE:
C
C     Eric Gotthelf,      Jan 1993
C     ASTRO-D GOF, GSFC
C
C     MODIFICATION HISTORY:
C
C       Feb 1997. Added code to correct for radial dependent long-term
C                 secular gain drift (see Gain History File, V4+, for
C                 details). E.Gotthelf/K.Ebisawa/T.Ishisaki.
C       Dec 1998. Agree with asca_defs.inc's dimension of gmapc_coeff:
C                 there are only 4 actual radial gain coefficients, not 5.
C                 Jeff Guerber, RSTX/GSFC
C       Dec 1998. gmapc_dim is really supposed to be the order (max 4) of the
C                 radial polynomial, so there are gmapc_dim+1 coefficients.
C                 (Also fixed asca_defs.inc and gis_pi.f to match.)
C                 Jeff Guerber, RSTX/GSFC
C
C     NOTES:
C
C     USAGE:
C
C     call read_gain_hist(gain_name, status)
C
C     ARGUMENTS:
C
C     data_name - input FITS file and extension number
C     status    - FITSIO status
C
C     PRIMARY LOCAL VARIABLES:
C
C     dmode  - datamode string
C     detect - detector string
C     maxcl  - maximum number of FITS table columns
C     contxt - error discription string and temp string
C     mode   - common block containing various det mode and data file info
C
C     fitsio variables - hdtype, nrows, filenm, xtensn, rwmode, block, rowlen
C     vardat, fcstln, crpix1, crpix2, tfield, exact
C     tbcol, ttype, tform, tunit, extnam, comment,  keywd
C
C     CALLED ROUTINES:
C
C     subroutine fcecho - echo message to terminal
C     function fcstln   - returns index of last non-blank character
C     subroutine ftxxxx - FITSIO calls
C
C******************************************************************************

        subroutine read_gain_hist(gain_name, iunit, gain_time, calxo,
     &       calyo, status)

        implicit none

        character*(*) gain_name, gain_time
        integer iunit, calxo, calyo, status

        include 'asca_defs.inc'

        integer xsize, ival, i, stat

        integer maxcl
        parameter (maxcl = 512)

        integer inst, row, hdtype, nrecords, xtensn, rwmode
        integer block, rowlen, vardat, fcstln, tfield, tbcol(maxcl)
        integer tstart_col, tstop_col, temp_col, iron_col
        integer rise_col, hvl_col, hvh_col, ghversion, ip, jp, gdim

        real tscale, tzero
        real x, y, tx, ty, sx, sy, dx, dy, corr
        double precision ghstart, ghstop
        double precision start, stop

        character(80) filenm, ttype(maxcl), tform(maxcl), tunit(maxcl)
        character(80) extnam, contxt, comment, telescope
        character(15) keywd, dmode, detect, pos_det, det_str
        character(12) tstartcol(2), tstopcol(2), hvhcol(2), hvlcol(2)
        character(12) tempcol(2), ironcol(2), risecol(2), cntscol (2)
        character(12) xoffcol(2), xsclcol(2), yoffcol(2), ysclcol(2)
        character(8) gmapc_key(2,5)

        logical anyf, exact, flgval

        include 'asca_common.inc'

        data tstartcol /'CAL_START', 'CAL_START'/
        data tstopcol  /'CAL_STOP', 'CAL_STOP'/
        data hvlcol    /'HV_LOW_S2', 'HV_LOW_S3'/
        data hvhcol    /'HV_HIGH_S2', 'HV_HIGH_S3'/
        data tempcol   /'TEMP_S2', 'TEMP_S3'/
        data ironcol   /'FE55_PEAK_S2', 'FE55_PEAK_S3'/
        data risecol   /'RT_PEAK_S2', 'RT_PEAK_S3'/
        data cntscol   /'COUNTS_S2', 'COUNTS_S3'/
        data xoffcol   /'XOFF_S2', 'XOFF_S3'/
        data xsclcol   /'XSCL_S2', 'XSCL_S3'/
        data yoffcol   /'YOFF_S2', 'YOFF_S3'/
        data ysclcol   /'YSCL_S2', 'YSCL_S3'/

        data gmapc_key(1,1) /'GMAPC0S2'/
        data gmapc_key(1,2) /'GMAPC1S2'/
        data gmapc_key(1,3) /'GMAPC2S2'/
        data gmapc_key(1,4) /'GMAPC3S2'/
        data gmapc_key(1,5) /'GMAPC4S2'/

        data gmapc_key(2,1) /'GMAPC0S3'/
        data gmapc_key(2,2) /'GMAPC1S3'/
        data gmapc_key(2,3) /'GMAPC2S3'/
        data gmapc_key(2,4) /'GMAPC3S3'/
        data gmapc_key(2,5) /'GMAPC4S3'/

        tzero  = 0.0
        tscale = 1.0

        stat = 0
        exact = .false.
        rwmode = 0
	start =0d0
	stop =0d0

c     compute cal_xo and cal_yo for gain normalization:

        x=float(calxo) + 0.5
        y=float(calyo) + 0.5
        tx=tr(1)+tr(2)*x+tr(3)*y
        ty=tr(4)+tr(5)*x+tr(6)*y
        cal_xo=max(min(int((tx)/det_x_scale+
     &       det_x_center),gis_size),1)
        cal_yo=max(min(int((ty)/det_y_scale+
     &       det_y_center),gis_size),1)

C     check for no gain file case:

        if (gain_name .eq. 'NONE' .or. gain_name .eq. 'none') then
           contxt = 'WARNING: No GAIN HISTORY file specified'
           goto 1000
        end if

C     get the input file name and extension

        call fcpars (gain_name, filenm, xtensn, status)
        call ftopen (iunit, filenm, rwmode, block, status)

        contxt = 'Unable to open GAIN HISTORY file: '//filenm

        if (status .ne. 0) goto 999

C     check for reasonable extension number

        if (xtensn .eq. -99) xtensn = 1
        contxt = 'Primary array not supported'
        if (xtensn .le. 0) goto 998

C     move to the correct extension

        call ftmrhd (iunit, xtensn, hdtype, status)
        contxt = 'Error moving to requested extension'
        if (status .ne. 0) goto 998

C     get header depending on extension type

        if (hdtype .eq. 1) then
           call ftghtb (iunit, maxcl, rowlen, nrecords, tfield, ttype,
     &          tbcol, tform, tunit, extnam, status)
        else if (hdtype .eq. 2) then
           call ftghbn (iunit, maxcl, nrecords, tfield, ttype, tform,
     &          tunit, extnam, vardat, status)
        else
           contxt = 'File extension type not supported'
           goto 998
        endif

C     get the detector name from the primary header

        keywd = 'TELESCOP'
        call ftgkys (iunit, keywd, telescope, contxt, status)
        contxt = 'Cannot determine telescope : '//telescope
        if (status .ne. 0) goto 998
C     if (index(telescope, 'ASCA') .eq. 0) goto 998

C     get the detector name from the primary header

        keywd = 'INSTRUME'
        call ftgkys (iunit, keywd, detect, contxt, status)
        contxt = 'Unknown detector (not GIS) : '//detect
        if (status .ne. 0) goto 998
C ============================================================
C     if (index(detect, 'GIS') .eq. 0) goto 998
C ============================================================

        call ftgkys(iunit, 'DATE', gain_time, contxt, status)
        if (status .eq. 202) then
           status = 0
           gain_time = 'NOT AVAILABLE'
        end if

        call ftgkyj(iunit, 'VERSION', ghversion, contxt, status)
        if (status .eq. 202) then
           ghversion = 0
           status = 0
        else if (status .ne. 0) then
           contxt = 'Error getting version number'
           goto 998
        end if

C     get start and stop time for this gain history file

        call ftgkyd(iunit, 'TSTART', ghstart, comment, status)
        if (status .ne. 0) then
           contxt = 'Cannot get TSTART'
           call fcecho (contxt)
           goto 998
        endif

        call ftgkyd(iunit, 'TSTOP', ghstop, comment, status)
        if (status .ne. 0) then
           contxt = 'Cannot get TSTOP'
           call fcecho (contxt)
           goto 998
        endif

C     check if event file and gain history times are in range

        if (ghstart-60.0D0.gt.tstart.or.ghstop+60.0D0.lt.tstop) then

Cebi  the following is slightly modified by Ken Ebisawa 93/07/01 (revised EVG)
           if (ghstart .gt. tstart) then
              contxt =
     & 'WARNING: Event start time preceeds gain history file'
           else
              contxt =
     & 'WARNING: Event end time not covered by gain history file'
           endif

           stat = 0
           call ftclos(iunit, stat)
           go to 1000

        end if

        if (detector .eq. GIS2) then
           inst = 1
        else
           inst = 2
        end if

C     COMPUTE CAL_XO, CAL_YO, USE ALGORITHM CORRESPONDING TO GH FILE VERSION:

        if (ghversion .le. 1) then
           tstartcol(1) = 'START TIME'
           tstartcol(2) = 'START TIME'
           tstopcol(1) =  'END TIME'
           tstopcol(2) =  'END TIME'
           hvhcol(1) = 'S2 HVH LEVEL'
           hvhcol(2) = 'S3 HVH LEVEL'
           hvlcol(1) = 'S2 HVL LEVEL'
           hvlcol(2) = 'S3 HVL LEVEL'
           tempcol(1) = 'S2 TEMP'
           tempcol(2) = 'S3 TEMP'
           ironcol(1) = 'S2 CAL PH'
           ironcol(2) = 'S3 CAL PH'
           risecol(1) = 'S2 CAL RT'
           risecol(2) = 'S3 CAL RT'
        end if

        if (ghversion .eq. 1) then
           x=float(calxo)+0.5
           y=float(calyo)+0.5
           tx=tr(1)+tr(2)*x+tr(3)*y
           ty=tr(4)+tr(5)*x+tr(6)*y
           sx=(tx/det_x_scale)+det_x_center
           sy=(ty/det_y_scale)+det_y_center
           ip=max(min(int(sx)+1,gis_size),1)
           jp=max(min(int(sy)+1,gis_size),1)
           dx=deltax(ip,jp)
           dy=deltay(ip,jp)
           cal_xo=max(min(int((tx+dx)/det_x_scale+
     &          det_x_center)+1,gis_size),1)
           cal_yo=max(min(int((ty+dy)/det_y_scale+
     &          det_y_center)+1,gis_size),1)
           corr = (472.9188/500.0)/(511.4022/500.0) / 0.98488
           cal_ratio = cal_ratio*corr
           cal_norm = cal_norm/corr
        end if

C     Read the radial long-term secular gain correction coefficients.
C     gmapc_dim is the order of the polynomial (up to 4), not the dimension,
C     so there are gmapc_dim+1 coefficients in gmapc_coeff(), up to 5.

        if (ghversion .eq. 4) then
           call ftgkyj(iunit, 'GMAPCDIM', gmapc_dim, contxt, status)
           if (status .ne. 0) then
              contxt = 'GMAPCDIM gain history keyword not found'
              goto 998
           end if
           if (gmapc_dim .gt. 4) then
              contxt='Gain hist file GMAPCDIM dimension not supported.'
              goto 998
           end if
           do i = 1, gmapc_dim+1
              call ftgkye(iunit, gmapc_key(inst,i), gmapc_coeff(i),
     &             contxt, status)
           end do
        else
           gmapc_dim = 0
           gmapc_coeff(1) = 0.0
           gmapc_coeff(2) = 0.0
           gmapc_coeff(3) = 0.0
           gmapc_coeff(4) = 0.0
           gmapc_coeff(5) = 0.0
           call fcerr(
     &     'WARNING: Old gain history file without radial coefficients')
        endif

C     check that the TIMESTART, TIMESTOP columns exist

        call ftgcno (iunit, exact, tstartcol(inst), tstart_col, status)
        call ftgcno (iunit, exact, tstopcol(inst), tstop_col, status)
        if (ghversion.le.1) contxt=
     &       'TSTART/TSTOP column do not exist'
        if (ghversion.gt.1) contxt=
     &       'CAL_START/CAL_STOP column do not exist'
        if (status .ne. 0) goto 998

C     check that the Fe55 and RISETIME PEAK columns exist

        call ftgcno (iunit, exact, tempcol(inst), temp_col, status)
        if (ghversion.le.1) contxt='Sn TEMP column do not exist'
        if (ghversion.gt.1) contxt='TEMP_Sn column do not exist'
        if (status .ne. 0) goto 998

C     check that the Fe55 and RISETIME PEAK columns exist

        call ftgcno (iunit, exact, ironcol(inst), iron_col, status)
        call ftgcno (iunit, exact, risecol(inst), rise_col, status)
        if (ghversion.le.1) contxt='Sn CAL PH/RT column do not exist'
        if (ghversion.gt.1) contxt=
     &       'FE55/RT_PEAK_Sn column do not exist'
        if (status .ne. 0) goto 998

C     check that the HVL and HVH columns exist

        call ftgcno (iunit, exact, hvlcol(inst), hvl_col, status)
        call ftgcno (iunit, exact, hvhcol(inst), hvh_col, status)
        if (ghversion.le.1) contxt=
     &       'Sn HVL/HVH LEVEL column do not exist'
        if (ghversion.gt.1) contxt=
     &       'HV_LOW/HIGH_Sn column do not exist'
        if (status .ne. 0) goto 998

C     loop through file and get values

        row = 1

        call ftgcfd (iunit, tstart_col, row, 1, 1, start,
     &       flgval, anyf, status)

        do while (start .lt. tstart-60.0d0 .and. status .eq. 0 .and.
     &       row .le. nrecords)

           call ftgcfd (iunit, tstart_col, row, 1, 1, start,
     &          flgval, anyf, status)

           row = row + 1

        end do

        i = 1
        row = max(row - 2, 1)

        do while (stop .lt. tstop+60.0d0 .and. status .eq. 0 .and.
     &       row .le. nrecords)

           call ftgcfd (iunit, tstart_col, row, 1, 1, gh_start(i),
     &          flgval, anyf, status)

           call ftgcfd (iunit, tstop_col, row, 1, 1, gh_stop(i),
     &          flgval, anyf, status)
           stop = gh_stop(i)

           call ftgcfj (iunit, hvl_col, row, 1, 1, gh_hvl(i),
     &          flgval, anyf, status)

           call ftgcfj (iunit, hvh_col, row, 1, 1, gh_hvh(i),
     &          flgval, anyf, status)

           call ftgcfe (iunit, hvh_col, row, 1, 1, gh_temp(i),
     &          flgval, anyf, status)

           call ftgcfe (iunit, iron_col, row, 1, 1, gh_iron(i),
     &          flgval, anyf, status)

           call ftgcfe (iunit, rise_col, row, 1, 1, gh_rise(i),
     &          flgval, anyf, status)

           i  = i + 1
           row = row + 1

        end do

        if (status .eq. 0)  then
           gh_records = i - 1
        else
           gh_records = max(i - 2, 0)
        end if

        return

 998    call fcerr (contxt)
        stat = 0
        call ftclos (iunit, stat)

        return

 999    call fcerr (contxt)

        return

 1000   continue

        det_pi_size = 1

        gh_records = 2
        gh_start(1) = tstart-100.0d0
        gh_start(2) = tstart
        gh_stop(1)  = tstart-50.0d0
        gh_stop(2)  = tstop
        gh_iron(1) = 0.0
        gh_iron(2) = 0.0
        gh_rise(1) = 128.0
        gh_rise(2) = 128.0
        gh_temp(1) = 20.0
        gh_temp(2) = 20.0
        gh_hvl(1) = 3
        gh_hvl(2) = 3
        gh_hvh(1) = 3
        gh_hvh(2) = 3

        gain_name = 'No temporal gain corrections applied.'
        gain_time = 'NOT AVAILABLE'
        call fcerr (contxt)
        call fcerr
     &       ('WARNING: No temporal gain corrections applied.')
        call fcerr ('WARNING: PI column not filled')

        return

        end
