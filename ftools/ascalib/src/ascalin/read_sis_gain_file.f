C******************************************************************************
C     SUBROUTINE:
C
C     read_sis_gain_file
C
C     DESCRIPTION:
C
C     opens and gets gain history records for the given observation file
C
C     AUTHOR/DATE:
C
C     Eric Gotthelf,      Jan 1993
C     ASTRO-D GOF, GSFC
C
C     MODIFICATION HISTORY:
C
C     Koji Mukai,         Feb 1996
C                         New version to be shared by ascalin and sispi
C     Koji Mukai,         Oct 1996
C                         Changed trantype from character(15) to integer
C                         Changed declaration of flgval to logical (6)
C     EVG and KM,         Feb 1998
C                         Changed tstart+60.0 to tstop+60.0 in read coeff loop.
C     Jeff Guerber,       August 1998
C                         Check DATE kwd first when looking for gain_time.
C                         Write dates in new format (yyyy-mm-dd).
C     Jeff Guerber,       Aug 1998.  Close file on successful return.
C       Bryan Irby,       Mar 2001.  Fix y2k bug in 'gain_time' string.
C     Koji Mukai,         Mar 2001.  Added the capability to read a new
C                         type of SIS gain history extensions.
C     Koji Mukai          Mar 2005.  Added the Type 3 capability.
C
C     NOTES:
C
C     USAGE:
C
C     call read_sis_gain_file(gain_name, iunit, gain_time, gain_norm,
C    &                        off_norm, launch, status )
C
C     ARGUMENTS:
C
C     gain_name - SIS gain history file name
C     iunit     - Fortran logical unit number to read the above with
C     gain_time - Return variable with file creation date information
C     gain_norm - If positive, force the energy channel gain to this value
C     off_norm  - Optionally the energy channel offset can be specified
C     launch    - Should normally be .false.
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

        subroutine read_sis_gain_file(gain_name, iunit, gain_time,
     &       gain_norm, off_norm, launch, status)

        implicit none

        character*(*) gain_name, gain_time
        integer iunit, status, yy
        real gain_norm, off_norm
        logical launch

        include 'asca_defs.inc'

        integer maxcl
        parameter (maxcl = 512)

        integer inst, row, hdtype, nrecords, xtensn, rwmode
        integer block, rowlen, vardat, fcstln, tfield, tbcol(maxcl)
        integer tstart_col, tstop_col, zero_col, one_col, ghversion
        integer two_col, three_col, xsize, ival, stat, i, j, k
        integer trantype, n_coefs, labor

        real tscale, tzero, delta_t, fract_t, frac1, frac2
        real file_gain, o1, ol, g1, gl, coeff(6, 0:3)
        double precision ghstart, ghstop
        double precision start, stop, mid_t

        real a0(8), a1(8), a2(8), a3(8)

        character(256) filenm, contxt
        character(80) ttype(maxcl), tform(maxcl), tunit(maxcl)
        character(80) extnam, comment, telescope, cdbname
        character(15) keywd, dmode, detect, pos_det, det_str
        character(12) tstartcol(2), tstopcol(2), c_zero_col(2)
        character(12) c_one_col(2), c_two_col(2), c_three_col(2)


c       flgval must be logical (8) as it is used in FTGCFE calls
c       for a0 etc., which have eight elements.   KM, 1996 Oct 11/2005 March
        logical anyf, exact, flgval(8), extrapolate

        include 'asca_common.inc'

        data tstartcol   / 'TIME', 'TIME' /
        data c_zero_col  / 'S0C0', 'S1C0' /
        data c_one_col   / 'S0C1', 'S1C1' /
        data c_two_col   / 'S0C2', 'S1C2' /
        data c_three_col / 'S0C3', 'S1C3' /

        tzero  = 0.0
        tscale = 1.0

        stat = 0
        exact = .false.
        extrapolate = .false.
        rwmode = 0
        inst = 0

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

C Changed in 2001 March by KM: If extension was specified, go there
C and see if it makes sense (Type I or Type II for the correct clocking
C mode).  If extension was not specified, try to find the right Type II
C extension using the EXTNAME keyword (and if that fials, try to find
C a Type I extension).

C     check for reasonable extension number

        if (xtensn .eq. -99) then
          if( ccd_mode .eq. 1 ) then
            extnam = 'PH2PI2M1'
          else if( ccd_mode .eq. 2 ) then
            extnam = 'PH2PI2M2'
          else if( ccd_mode .eq. 4 ) then
            extnam = 'PH2PI2M4'
          else
            contxt = 'Unable to decifer CCD MODE'
            goto 998
          end if

          hdtype = 2
          call FTMNHD( iunit, hdtype, extnam, 0, status )
          if( status .ne. 0 ) then
            status = 0
            extnam = 'PH2PI_T1'
            call FTMNHD( iunit, hdtype, extnam, 0, status )
            contxt = 'Error moving to PH2PI extension'
            if( status .ne. 0 ) goto 998
            n_ph2pi = 0
          else
            n_ph2pi = ccd_mode
          end if
          call ftghbn( iunit, maxcl, nrecords, tfield, ttype, tform,
     &          tunit, extnam, vardat, status )

        else
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
          if( extnam .eq. 'PH2PI_T1' ) then
            n_ph2pi = 0
          else if( extnam .eq. 'PH2PI2M1' ) then
            contxt =
     &             'Clocking mode mismatch between data and calibration'
            if( ccd_mode .ne. 1 ) goto 998
            n_ph2pi = 1
          else if( extnam .eq. 'PH2PI2M2' ) then
            contxt =
     &             'Clocking mode mismatch between data and calibration'
            if( ccd_mode .ne. 2 ) goto 998
            n_ph2pi = 2
          else if( extnam .eq. 'PH2PI2M4' ) then
            contxt =
     &             'Clocking mode mismatch between data and calibration'
            if( ccd_mode .ne. 4 ) goto 998
            n_ph2pi = 4
          else
            contxt = 'Unknown extension type for CTI calibration'
            goto 998
          end if
        end if
              

C     get the detector name from the primary header

c        keywd = 'TELESCOP'
c        call ftgkys (iunit, keywd, telescope, contxt, status)
c        contxt = 'Cannot determine telescope : '//telescope
c        if (status .ne. 0) goto 998

C     if (index(telescope, 'ASCA') .eq. 0) goto 998

C     get the detector name from the primary header

c        keywd = 'INSTRUME'
c        call ftgkys (iunit, keywd, detect, contxt, status)
c        contxt = 'Unknown detector (not SIS) : '//detect
c        if (status .ne. 0) goto 998

C     if (index(detect, 'SIS') .eq. 0) goto 998

C Version number/gain_time algorithm is *messy*.
C       Since none of the files seem to have the DATE keyword (so don't
C       bother), we'll rely on FILENAME keyword (which exists in all
C       officially released versions? Maybe not in processing).
C       we'll use 'XFORMTYP' (1 for the first official version,
C       2 for the later ones) and 'VERSION' (exists in most but not all).
C       It might fail for very old or pre-release gain history files.

         call FTGKYJ( iunit, 'XFORMTYP', ghversion, contxt, status )
C         call ftgkys (iunit, 'DATE', gain_time, contxt, status)
C         if (status .eq. 202) then
C             status = 0
         call ftgkys (iunit, 'FILENAME', cdbname, contxt, status)
         if( status .eq. 202 ) then
           status = 0
           if( ghversion .ge. 2 ) then
C            Now try the VERSION string
             call FTGKYJ( iunit, 'VERSION', labor, contxt, status )
             if( status .eq. 0 ) then
               if( labor .eq. 1 ) then
                 gain_time = '1995-07-14 (from Version keyword)'
               else if( labor .eq. 2 ) then
                 gain_time = '1996-02-29 (from Version keyword)'
               else if( labor .eq. 3 ) then
                 gain_time = '1997-03-11 (from Version keyword)'
               else if( labor .eq. 4 ) then
                 gain_time = '2001-02-13 (from Version keyword)'
               else if( labor .eq. 5 ) then
                 gain_time = '2001-04-01 (from Version keyword)'
               else if( labor .eq. 6 ) then
                 gain_time = '2003-02-13 (from Version keyword)'
               else
                 gain_time = 'Unavailable (Version keyword value?)'
               end if
             else if( status .eq. 202 ) then
               gain_time = '1996-02-29 (estimated from file format)'
               status = 0
             else
               contxt = 'Error getting version number'
               goto 998
             end if
           else
             call FTGKYJ (iunit, 'RAN_SEED', ghversion,
     &                 contxt, status)
             if (status .eq. 0) then
               ghversion = 1
               gain_time = '1994-07-28 - first official version'
             else if (status .eq. 202) then
               ghversion = 0
               gain_time = '1994-06-09 - preliminary version'
               status = 0
             else
               contxt = 'Error getting version number'
               goto 998
             end if
           end if
         else if( status .ne. 0 ) then
           contxt = 'Error getting version number'
           goto 998
         else
           yy = 10*(ichar(cdbname(14:14))-48) +
     &                     (ichar(cdbname(15:15))-48)
           if( cdbname( 1: 9 ) .eq. 'sisph2pi_'
     &                 .and. cdbname( 16: 20 ) .eq. '.fits' ) then
             if(yy .lt. 70) then
               gain_time = '20' // cdbname(14:15) // '-'
     &                     // cdbname(12:13) // '-' // cdbname(10:11)
     &                     // '  (from CALDB name)'
             else
               gain_time = '19' // cdbname(14:15) // '-'
     &                     // cdbname(12:13) // '-' // cdbname(10:11)
     &                     // '  (from CALDB name)'
             end if
           else
             gain_time = 'CALDB name with date : ' // cdbname
           end if
         end if
C           elseif (status .ne. 0) then
C             gain_time = 'NOT AVAILABLE'
C             contxt = 'Error getting SIS gain file date'
C             goto 998
C           endif

        if( n_ph2pi .eq. 0 ) then
          n_coefs = 6
        else if( n_ph2pi .eq. 1 .or. n_ph2pi .eq. 2
     &                                  .or. n_ph2pi .eq. 4 ) then
          n_coefs = 7
          if( ghversion .eq. 4 ) then
            n_coefs = 8
          end if

* Also read the pristine gain values from the header
          status = 0
          if( detector .eq. SIS0 )then
            call FTGKYE( iunit, 'S0C0GAIN',
     &                              gain_chips( 0 ), comment, status )
            call FTGKYE( iunit, 'S0C1GAIN',
     &                              gain_chips( 1 ), comment, status )
            call FTGKYE( iunit, 'S0C2GAIN',
     &                              gain_chips( 2 ), comment, status )
            call FTGKYE( iunit, 'S0C3GAIN',
     &                              gain_chips( 3 ), comment, status )
          else
            call FTGKYE( iunit, 'S1C0GAIN',
     &                              gain_chips( 0 ), comment, status )
            call FTGKYE( iunit, 'S1C1GAIN',
     &                              gain_chips( 1 ), comment, status )
            call FTGKYE( iunit, 'S1C2GAIN',
     &                              gain_chips( 2 ), comment, status )
            call FTGKYE( iunit, 'S1C3GAIN',
     &                              gain_chips( 3 ), comment, status )
          end if
          if( status .ne. 0 ) then
            contxt = 'Error getting original gain of chips'
            goto 998
          end if

        else
           contxt = 'Unrecognized calibration file format'
           status = 1
           goto 998
        end if

c     call ftgkys(iunit, 'DATE', gain_time, contxt, status)
c     if (status .eq. 202) then
c     status = 0
c     gain_time = 'NOT AVAILABLE'
c     end if

*       Potentially need to be able to distinguish future
*       versions of the cal file

        if( .not. (ghversion .le. 4 .and. ghversion .ge. 0) ) then
           contxt = 'Error getting version number'
           goto 998
        end if

        call FTGKYJ (iunit, 'XFORMTYP', trantype, comment, status )
        if( status .ne. 0 ) then
           write( contxt, 201 ) trantype
 201       format( 'SIS PI transform type not known :', i5 )
C           Trantype was character(15), causing core dumps on SGI
C           contxt = 'SIS PI transform type not known'//trantype
           goto 998
        end if
        call FTGKYE (iunit, 'NOM_GAIN', file_gain, comment, status )
        if (status .ne. 0) then
           contxt = 'Failed to read nominal SIS gain'
           goto 998
        end if

c     Choose gain, convert from eV to keV for internal consistency.

        if (gain_norm .lt. 0.0 ) then
           sis_gain = file_gain / 1000.0
        else
           sis_gain = gain_norm / 1000.0
        end if

*       Lines below added by KM - copy off_norm to common block and
*       Set up bright to linear and linear to bright transformation
        sis_off = off_norm
        if( datamode .eq. BRIGHT_mode ) then
          sis_b2l( 1 ) = 1024.0
          sis_b2l( 2 ) = 1536.0
          sis_l2b( 1 ) = 1023
          sis_l2b( 2 ) = 2046
        else if( datamode .eq. BRIGHT2_mode ) then
          sis_b2l( 1 ) = 4096.0
          sis_b2l( 2 ) = 4096.0
          sis_l2b( 1 ) = 4096
          sis_l2b( 2 ) = 4096
        end if

C     get start and stop time for this gain history file

c        call ftgkyd(iunit, 'TSTART', ghstart, comment, status)
c        if (status .ne. 0) then
c           contxt = 'Cannot get TSTART'
c           call fcecho (contxt)
c           goto 998
c        endif

c        call ftgkyd(iunit, 'TSTOP', ghstop, comment, status)
c        if (status .ne. 0) then
c           contxt = 'Cannot get TSTOP'
c           call fcecho (contxt)
c           goto 998
c        endif
c

C     SET UP COLUMN NAMES DEPENDING ON GAIN FILE VERSION

        if (ghversion .gt. 1) then
           tstartcol(1) = 'TIME'
           tstartcol(2) = 'TIME'
           c_zero_col(1) = 'S0C0'
           c_zero_col(2) = 'S1C0'
           c_one_col(1) = 'S0C1'
           c_one_col(2) = 'S1C1'
           c_two_col(1) = 'S0C2'
           c_two_col(2) = 'S1C2'
           c_three_col(1) = 'S0C3'
           c_three_col(2) = 'S1C3'
        else
           tstartcol(1) = 'TIME'
           tstartcol(2) = 'TIME'
           c_zero_col(1) = 'S0C0'
           c_zero_col(2) = 'S1C0'
           c_one_col(1) = 'S0C1'
           c_one_col(2) = 'S1C1'
           c_two_col(1) = 'S0C2'
           c_two_col(2) = 'S1C2'
           c_three_col(1) = 'S0C3'
           c_three_col(2) = 'S1C3'
        end if

        inst = 0
        if (detector .eq. SIS0) then
           inst = 1
        else
           inst = 2
        end if

C     check that the TIMESTART column exist

        call ftgcno (iunit, exact, tstartcol(inst), tstart_col, status)
        contxt= 'TIME column do not exist'
        if (status .ne. 0) goto 998

C     get start and stop time for this gain history file

        call ftgcfd (iunit, tstart_col, 1, 1, 1, ghstart, flgval,
     &       anyf, status)
        if (status .ne. 0) then
           contxt = 'Cannot get TSTART'
           call fcecho (contxt)
           goto 998
        endif

        call ftgcfd (iunit, tstart_col, nrecords, 1, 1, ghstop, flgval,
     &       anyf, status)
        if (status .ne. 0) then
           contxt = 'Cannot get TSTOP'
           call fcecho (contxt)
           goto 998
        endif

C     check that the chip coeff columns exist

        call ftgcno (iunit, exact, c_zero_col(inst), zero_col, status)
        contxt='SnC0 column do not exist'
        if (status .ne. 0) goto 998

        call ftgcno (iunit, exact, c_one_col(inst), one_col, status)
        contxt='SnC1 column do not exist'
        if (status .ne. 0) goto 998

        call ftgcno (iunit, exact, c_two_col(inst), two_col, status)
        contxt='SnC2 column do not exist'
        if (status .ne. 0) goto 998

        call ftgcno (iunit, exact, c_three_col(inst), three_col, status)
        contxt='SnC3 column do not exist'
        if (status .ne. 0) goto 998

C     check if event file and gain history times are in range

        if (ghstart-60.0D0.gt.tstart.or.ghstop+60.0D0.lt.tstop
     &       .and. .not. launch) then

           if (ghstart .gt. tstart) then
              contxt =
     &  'WARNING: Event start time preceeds gain history file'

           else
              contxt =
     &  'WARNING: Event end time not covered by gain history file'
              call fcecho(contxt)
c     Time is after the last entry -> warn and extrapolate last data point
              contxt = 'WARNING: extrapolating gain/CTI trend'
              call fcecho(contxt)
              extrapolate = .TRUE.

           endif

           if (.not. extrapolate) then
              stat = 0
              call fcecho(contxt)
              call ftclos(iunit, stat)
              go to 1000
           end if

        end if

C     loop through file and get values

        i = 1
        row = 1
        call ftgcfd (iunit, tstart_col, row, 1, 1, start,
     &       flgval, anyf, status)

        do while (start .lt. tstop+60.0d0 .and. status .eq. 0 .and.
     &       row .le. nrecords)

           call ftgcfd (iunit, tstart_col, row, 1, 1, gh_start(i),
     &          flgval, anyf, status)

           call ftgcfe (iunit, zero_col,   row, 1, n_coefs, a0,
     &          flgval, anyf, status)

           call ftgcfe (iunit, one_col,    row, 1, n_coefs, a1,
     &          flgval, anyf, status)

           call ftgcfe (iunit, two_col,    row, 1, n_coefs, a2,
     &          flgval, anyf, status)

           call ftgcfe (iunit, three_col,  row, 1, n_coefs, a3,
     &          flgval, anyf, status)

           start = gh_start(i)
           if( n_coefs .eq. 7 ) then
*            Special for Type 2 formula, which has 7 paramters;
*            which corresponds to Type 3 with p3=1.0
             do j=1, 2
               gh_c0(i,j) = a0(j)
               gh_c1(i,j) = a1(j)
               gh_c2(i,j) = a2(j)
               gh_c3(i,j) = a3(j)
             end do
             gh_c0( i, 3 ) = 1.0
             gh_c1( i, 3 ) = 1.0
             gh_c2( i, 3 ) = 1.0
             gh_c3( i, 3 ) = 1.0
             do j=3, 7
               gh_c0(i,j+1) = a0(j)
               gh_c1(i,j+1) = a1(j)
               gh_c2(i,j+1) = a2(j)
               gh_c3(i,j+1) = a3(j)
             end do
           else
             do j=1, n_coefs
               gh_c0(i,j) = a0(j)
               gh_c1(i,j) = a1(j)
               gh_c2(i,j) = a2(j)
               gh_c3(i,j) = a3(j)
             end do
           end if
           i  = i + 1
           row = row + 1

        end do

        if (extrapolate) then
           if (i .gt. 2) then
              ghstop = tstop + 32.0d0
              delta_t = gh_start(i-1) - gh_start(i-2)
              fract_t = ghstop - gh_start(i-1)
              do j = 1, n_coefs
                 gh_c0(i,j) = gh_c0(i-1,j) +
     &                fract_t*(gh_c0(i-1,j) - gh_c0(i-2,j))/delta_t
                 gh_c1(i,j) = gh_c1(i-1,j) +
     &                fract_t*(gh_c1(i-1,j) - gh_c1(i-2,j))/delta_t
                 gh_c2(i,j) = gh_c2(i-1,j) +
     &                fract_t*(gh_c2(i-1,j) - gh_c2(i-2,j))/delta_t
                 gh_c3(i,j) = gh_c3(i-1,j) +
     &                fract_t*(gh_c3(i-1,j) - gh_c3(i-2,j))/delta_t
              end do
              gh_start(i) = ghstop
              i = i + 1
           else
              stat = 0
              contxt =
     & 'WARNING: Cannot extrapolate, PI channel not filled.'
              call fcecho(contxt)
              call ftclos(iunit, stat)
              go to 1000
           end if
        end if

        if (status .eq. 0)  then
           if (launch) then
              gh_records = 1
           else
              gh_records = i - 1
           end if
        else
           gh_records = max(i - 2, 0)
           contxt = 'Error reading gain history file column data.'
           go to 998
        end if

        call ftclos (iunit, stat)
        return

 998    call fcerr (contxt)
        stat = 0
        call ftclos (iunit, stat)
        n_ph2pi = -1

        return

 999    call fcerr (contxt)
        n_ph2pi = -1

        return

 1000   continue

        det_pi_size = 1

        gh_records = 2
        gh_start(1) = tstart-100.0d0
        gh_start(2) = tstart

        do i=1,6
           do j=1,2
              gh_c0(i,j) = 0.0
              gh_c1(i,j) = 0.0
              gh_c2(i,j) = 0.0
              gh_c3(i,j) = 0.0
           end do
        end do

        if (inst .eq. 1) then
           gh_c0(1,1) = 3.5608187
           gh_c0(2,1) = 3.5608187
           gh_c0(1,3) = -9.4684774e-6
           gh_c0(2,3) = -9.4684774e-6
           gh_c1(1,1) = 3.5608187
           gh_c1(2,1) = 3.5608187
           gh_c1(1,3) = -9.4684774e-6
           gh_c1(2,3) = -9.4684774e-6
           gh_c2(1,1) = 3.5608187
           gh_c2(2,1) = 3.5608187
           gh_c2(1,3) = -9.4684774e-6
           gh_c2(2,3) = -9.4684774e-6
           gh_c3(1,1) = 3.5608187
           gh_c3(2,1) = 3.5608187
           gh_c3(1,3) = -9.4684774e-6
           gh_c3(2,3) = -9.4684774e-6
        else
           gh_c0(1,1) = 3.5608187
           gh_c0(2,1) = 3.5608187
           gh_c0(1,3) = -9.4684774e-6
           gh_c0(2,3) = -9.4684774e-6
           gh_c1(1,1) = 3.5608187
           gh_c1(2,1) = 3.5608187
           gh_c1(1,3) = -9.4684774e-6
           gh_c1(2,3) = -9.4684774e-6
           gh_c2(1,1) = 3.5608187
           gh_c2(2,1) = 3.5608187
           gh_c2(1,3) = -9.4684774e-6
           gh_c2(2,3) = -9.4684774e-6
           gh_c3(1,1) = 3.5608187
           gh_c3(2,1) = 3.5608187
           gh_c3(1,3) = -9.4684774e-6
           gh_c3(2,3) = -9.4684774e-6
        end if

        gain_name = 'No temporal gain corrections applied.'
        gain_time = 'NOT AVAILABLE'
        call fcerr (contxt)
        call fcerr
     &       ('WARNING: No temporal gain corrections applied.')
        call fcerr ('WARNING: PI column not filled')
        n_ph2pi = -1

        return

        end
