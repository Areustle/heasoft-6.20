C----------------------------------------------------------------------------
C Calls parameters for FXRATE task from the parameter file
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 0.0  13 Feb 1998  First draft version
C          0.9  16 Feb 1998  Error checking, full set of values returned 
C          0.91 19 Apr 1998  Gets rootname, equinoxes, and accepts HMS DMS
C                            strings for celestial coordinates 
C               25 May 1998  Replaced input date and time formats with 
C          (Lorraine Breedon)    dd/mm/yr and hh:mm:ss
C          19 Oct 1998  Generates just 1 lightcurve (rather than up to 4)
C          (Lorraine Breedon)  
C          26 Oct 1998  Remove common blocks
      subroutine getpar(infile, rootname, rastr, decstr, 
     +           equinox, beginday, imsecs, endday, imsece, hdet, hds, 
     +           backmode, bkgd, berr, 
     +           anglim,mcfilter, mcilwainl, lchat, 
     +           tchat, clobber, irafsts)

      implicit none



 
C Local variable declarations

      logical mcfilter, marshallflag, clobber

      integer*2 hdet, hds(4)
      integer i, j, det_tmp, scalar_tmp, lchat, tchat, irafsts
      integer backmode
      integer beginyr, beginday, endyr, endday, imsecs, imsece

      real beginsec, endsec, mcilwainl
      real bkgd, berr, anglim(2)

      double precision equinox

      character*(*) rastr, decstr, infile, rootname
      character(80) message
      integer iy,imon,id,errstat,ih,im,is
      character(8) stdate, sttime, endate, entime

      integer mtab(12),ndays,lenact
 
      data mtab/31,28,31,30,31,30,31,31,30,31,30,31/



C       initialize variables
        iy = 0
        imon = 0
        id = 0
        ih = 0
        im = 0
        is = 0
        beginyr=0
        beginday=0
        endyr=0
        endday=0
        imsecs=0
        imsece=0
        errstat=0

        

      irafsts = 0

C Get the name of the source, its celestial coordinates, and the equinox of
C the given coordinates.  FK4 (Besselian) coordinate system is assumed.
C However, the difference between FK4 and FK5 is significantly less than 
C the resolution of the A2 instrument.

      call uclgsd('equinox', equinox, irafsts)
      if (irafsts .ne. 0) then
         message = 'Error reading EQUINOX'
         go to 999
      endif 

      call uclgst('ra', rastr, irafsts)
      if (irafsts .ne. 0) then
         message = 'Error reading RA'
         go to 999
      endif 

      call uclgst('dec', decstr, irafsts)
      if (irafsts .ne. 0) then
         message = 'Error reading DEC' 
         go to 999
      endif 

      call uclgst('infile', infile, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get INFILE parameter'
         go to 999
      endif

C Get the "root name": an identifying string to add to the filename to
C distinguish between sources.  For example "cirx1" would be added to the
C light curve names as "cirx1_rate1.lc".

      call uclgst('rootname', rootname, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get ROOTNAME parameter'
         go to 999
      endif

C Get the beginning and end times for the light curve creation.  If the
C user enters a full year (e.g. 1978 instead of 78), accept the entry and
C correct it.  Accept ending year entries up to 1980 (e.g. although
C 1980 day 1 is after the end of the mission, accept this as an input),
C but do not permit a start year after the end of the mission.

 20   call uclgst('strtdate', stdate, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get STRTDATE parameter'
         go to 999
      endif


C       Parse the date into day month and year
        call ftc2ii(stdate(1:2),id,errstat)
        call ftc2ii(stdate(4:5),imon,errstat)
        call ftc2ii(stdate(7:8),iy,errstat)
        if (errstat .ne. 0) goto 999
        if (lenact(stdate) .eq. 0) then
            message= ' Illegal value! Re-enter the start date'
            call xwrite(message,1)
            goto 20
        endif

c       check on format of input string
        if ((stdate(3:3) .ne. '/') .or. (stdate(6:6) .ne. '/')) then
           
            message = ' Delimiter should be "/" ..but its OK'
            call xwrite(message,1)

        endif 


c validate year
      beginyr=iy
      if (beginyr .gt. 99) beginyr = beginyr - 1900
      if ((beginyr .lt. 77) .or. (beginyr .gt. 79)) then
         message =' Illegal value!  Mission spanned from 1977 to 1979'
         call xwrite(message,1)
         message = ' Please enter a valid starting year (1977 - 1979)'
         call xwrite(message,1)
         go to 20
      endif
c validate month

        if (imon.lt.1 .or. imon.gt.12) then

           message = 'Illegal value ! Please enter a valid month'
           call xwrite(message,1)
           go to 20
        endif


c Validate day
        if (id.lt.1.or.id.gt.mtab(imon)) then
                message = 'Illegal value! Please enter a valid day'
                call xwrite(message,1)
                message = 'for this month '
                call xwrite(message,1)
                go to 20

        endif


  
C now calculate number of days from input date value
        ndays=0
        do 10 i=1,imon

             if (i .gt. 1) then
                ndays=ndays+mtab(i-1)
             endif
 10     continue
        beginday=ndays+id

        i=0     

 
    
       if ((beginday .lt. 224) .and. (beginyr .eq. 77)) then
         message = 'Mission began on day 224 of 1977 '
         call xwrite(message,1) 
         message = ' setting start day to 224'
         call xwrite(message,1)
         beginday = 224
         beginsec = 0.0
         go to 50
       else if ((beginday .gt. 9) .and. (beginyr .ge. 79)) then
         message = 'Mission ended on day 9 of 1979'
         call xwrite(message,1)

         write(message,'(''which is later than '', i3, ''of 19'', i2)')
     +       beginday, beginyr
         call xwrite(message,1)
         message = ' Please enter a valid starting year and day '
         call xwrite(message,1)
         go to 20
      endif
      beginday = beginday + (365 * (beginyr - 77))
  


 40   call uclgst('strtime', sttime, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get STRTIME parameter'
         go to 999
      endif

            
C       Parse the time into hours, minutes, and seconds
        call ftc2ii(sttime(1:2),ih,errstat)
        call ftc2ii(sttime(4:5),im,errstat)
        call ftc2ii(sttime(7:8),is,errstat)
        if (errstat .ne. 0) goto 999

        if (lenact(sttime) .eq. 0) then
            message= ' Illegal value! Re-enter the start time'
            call xwrite(message,1)
            goto 40
        endif



c       check on format of input string
        if ((sttime(3:3) .ne. ':') .or. (sttime(6:6) .ne. ':')) then
            message = ' Delimiter should be ":" ..but its OK'
            call xwrite(message,1)

        endif 


C  check for valid inputs

        if (ih .gt. 23) then
            message = ' WARNING: Invalid number of hours..re-enter'
         call xwrite(message,1)

            goto 40
        elseif (im .gt. 60) then
            message = ' WARNING: Invalid number of minutes..re-enter'
         call xwrite(message,1)

            goto 40
        elseif (is .gt. 60) then
           message = ' WARNING: Invalid number of seconds..re-enter'
         call xwrite(message,1)

           goto 40
        endif

       beginsec=(ih*360)+(im*60)+is


       if ((beginsec .lt. 0.0) .or. (beginsec .gt. 86400.0)) then
         message = 'Illegal value!  0 <= total seconds in day <= 86400 '
         call xwrite(message,1)
         message = ' Please enter a valid time of the day '
         call xwrite(message,1)
         go to 40
      endif


 50   call uclgst('enddate', endate, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get ENDDATE parameter'
         go to 999
      endif

C       Parse the date into day month and year
        call ftc2ii(endate(1:2),id,errstat)
        call ftc2ii(endate(4:5),imon,errstat)
        call ftc2ii(endate(7:8),iy,errstat)
        if (errstat .ne. 0) goto 999

       if (lenact(endate) .eq. 0) then
            message= ' Illegal value! Re-enter the end date'
            call xwrite(message,1)
            goto 50
        endif


c       check on format of input string
        if ((endate(3:3) .ne. '/') .or. (endate(6:6) .ne. '/')) then
            message = ' Delimiter should be  "/" ..but its OK'
            call xwrite(message,1)

        endif 


c validate year
      endyr=iy

       if (endyr .gt. 99) endyr = endyr - 1900
      if ((endyr .lt. 77) .or. (endyr .gt. 80)) then
         message = 'Illegal value!  Mission spanned from 1977 to 1979'
         call xwrite(message,1)
         message = ' Please enter a valid ending year (1977 - 1980)'
         call xwrite(message,1)
         go to 50
      endif

c validate month

        if (imon.lt.1 .or. imon.gt.12) then
           message = 'Illegal value ! Please enter a valid month'
           call xwrite(message,1)
           go to 50
        endif


c Validate day
        if (id.lt.1.or.id.gt.mtab(imon)) then
                message = 'Illegal value! '
                call xwrite(message,1)
                message = 'Please enter a valid day for this month '
                call xwrite(message,1)
                go to 50

        endif




C now calculate number of days from input date value
       
       ndays=0
        do 60 i=1,imon
            if (i .gt. 1) then
                ndays=ndays+mtab(i-1)
 
             endif
 60     continue
        endday=ndays+id

          i=0     

 

       if ((endday .lt. 224) .and. (endyr .eq. 1977)) then
         message = 'Mission began on day 224 of 1977'
         call xwrite(message,1)
         write(message,'(''which is later than '', i3, ''of 19'', i2)') endday,
     +        endyr
         call xwrite(message,1)
         message = ' Please enter a valid end year and day '
         call xwrite(message,1)
         go to 50
      endif
      endday = endday + (365 * (endyr - 77))
      if (endday .gt. 739) endday = 739

  

 70   call uclgst('endtime', entime, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get ENDTIME parameter'
         go to 999
      endif




C       Parse the time into hours, minutes, and seconds
        call ftc2ii(entime(1:2),ih,errstat)
        call ftc2ii(entime(4:5),im,errstat)
        call ftc2ii(entime(7:8),is,errstat)
        if (errstat .ne. 0) goto 999

        if (lenact(entime) .eq. 0) then
            message= ' Illegal value! Re-enter the end time'
            call xwrite(message,1)
            goto 70
        endif


c       check on format of input string
        if ((entime(3:3) .ne. ':') .or. (entime(6:6) .ne. ':')) then
            message = ' Delimiter should be  ":" ..but its OK'
         call xwrite(message,1)

        endif 


C  check for valid inputs

        if (ih .gt. 23) then
            message = ' WARNING: Invalid number of hours..re-enter'
         call xwrite(message,1)

            goto 70
        elseif (im .gt. 60) then
            message = ' WARNING: Invalid number of minutes..re-enter'
         call xwrite(message,1)

            goto 70
        elseif (is .gt. 60) then
           message = ' WARNING: Invalid number of seconds..re-enter'
         call xwrite(message,1)

           goto 70
        endif


       endsec=(ih*360)+(im*60)+is

      if ((endsec .lt. 0.0) .or. (endsec .gt. 86400.0)) then
         message = 'Illegal value!  0 <= total seconds in day <= 86400 '
         call xwrite(message,1)
         message = ' Please enter a valid second of the day '
         call xwrite(message,1)
         go to 70
      endif

      imsecs = INT(beginsec * 1000.0)
      imsece = INT(endsec * 1000.0)
c 80   call uclgsi('numlc', numlc, irafsts)
c      if (irafsts .ne. 0) then
c         message = 'Unable to get NUMLC parameter'
c         go to 999
c      endif
c      if ((numlc .lt. 1) .or. (numlc .gt. 1)) then
c         message ='Illegal value!  You may generate just 1 light curve'
c         call xwrite(message,1) 
c         go to 80
c      endif

C Get the discovery scalar settings and scan angle limits 
      marshallflag = .true.
         hdet = 0
         anglim(1) = 0.0
         anglim(2) = 360.0
         do 90 j = 1, 4
            hds(j) = 0
 90      end do
 
      if (index(infile(:lenact(infile)),'_hedfiles') .ne. 0) then
 105      call uclgsi('detector', det_tmp, irafsts)
          if (irafsts .ne. 0) then
             message = 'Unable to get DETECTOR parameter'
             go to 999
          else if ((det_tmp .lt. 2) .or. (det_tmp .gt. 4)) then
             call baddetvalue(1)
             go to 105
          endif
          hdet= det_tmp
      else
          hdet=1
      endif

 110  call uclgsi('scalar1_1', scalar_tmp, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get SCALAR1_1 parameter'
         go to 999
      else if ((scalar_tmp .lt. -8) .or. (scalar_tmp .gt. 8)) then
         call badscalar(1)
         go to 110
      else if (scalar_tmp .eq. 0) then
         message = ' You have requested no data for this light curve'
         call xwrite(message,1)
         message = ' Please enter a non-zero scalar value [+/- 1-8]'
         call xwrite(message,1)
         go to 110
      else if ((scalar_tmp .lt. -4) .or. (scalar_tmp .gt. 4)) then
         marshallflag = .false.
      endif
      hds(1) = scalar_tmp

 120  call uclgsi('scalar1_2', scalar_tmp, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get SCALAR1_2 parameter'
         go to 999
      else if ((scalar_tmp .lt. -8) .or. (scalar_tmp .gt. 8)) then
         call badscalar(2)
         go to 120
      else if ((scalar_tmp .lt. -4) .or. (scalar_tmp .gt. 4)) then
         marshallflag = .false.
      endif
      hds(2) = scalar_tmp
      if (scalar_tmp .eq. 0) then
         hds(3) = 0
         hds(4) = 0
         go to 150
      endif

 130  call uclgsi('scalar1_3', scalar_tmp, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get SCALAR1_3 parameter'
         go to 999
      else if ((scalar_tmp .lt. -8) .or. (scalar_tmp .gt. 8)) then
         call badscalar(3)
         go to 130
      else if ((scalar_tmp .lt. -4) .or. (scalar_tmp .gt. 4)) then
         marshallflag = .false.
      endif
      hds(3) = scalar_tmp
      if (scalar_tmp .eq. 0) then
         hds(4) = 0
         go to 150
      endif

 140  call uclgsi('scalar1_4', scalar_tmp, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get SCALAR1_4 parameter'
         go to 999
      else if ((scalar_tmp .lt. -8) .or. (scalar_tmp .gt. 8)) then
         call badscalar(4)
         go to 140
      else if ((scalar_tmp .lt. -4) .or. (scalar_tmp .gt. 4)) then
         marshallflag = .false.
      endif
      hds(4) = scalar_tmp

 150  call uclgsr('anglim1_1', anglim(1), irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get ANGLIM1_1 parameter'
         go to 999
      else if ((anglim(1) .lt. 0.0).or.(anglim(1) .gt. 360.0)) then
         message = 'Illegal starting scan angle limit, using 0 degrees'
         call xwrite(message,1)
         anglim(1) = 0.0
      endif
 160  call uclgsr('anglim1_2', anglim(2), irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get ANGLIM1_2 parameter'
         go to 999
      else if ((anglim(2) .lt. 0.0).or.(anglim(2) .gt. 360.0)) then
         message = 'Illegal stopping scan angle limit, using 360 '//
     +     'degrees'
         call xwrite(message,1)
         anglim(2) = 360.0
      else if (anglim(2) .eq. 0.0) then
         anglim(2) = 360.0
      endif

 500  call uclgsb('mcfilter', mcfilter, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get MCFILTER parameter'
         go to 999
      endif
      if (mcfilter .eqv. .false.) then
         mcilwainl = 0.0
         go to 600
      endif

 510  call uclgsr('mcilwainl', mcilwainl, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get MCILWAINL parameter'
         go to 999
      endif

 600  call uclgsi('backmode', backmode, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get BACKGROUND MODE parameter'
         go to 999
      else if ((backmode .lt. 0) .or. (backmode .gt. 2)) then
         message = 'Illegal value !'
         call xwrite(message,1)
         message = 'Type 0 for scanned background'
         call xwrite(message,1)
        message = 'Type 1 for Marshall background'
         call xwrite(message,1)
        message = 'Type 2 for user-input background'
         call xwrite(message,1)

         go to 600
      else if (.not.marshallflag .and. (backmode .eq. 1)) then
         message = ' The Marshall background rates are ' //
     +     'available for discovery scalars 1 - 4.'
         call xwrite(message,1)
         message = ' You have already selected scalars outside this ' //
     +     'range.  Please choose background mode '
         call xwrite(message,1)
         message = ' 0 for scanned background or 2 to input values.'
         call xwrite(message,1)
         go to 600
      endif
      if (backmode .eq. 0) then
         message = 'Using scanned background calculated from the source'
         call xwrite(message,1)
         go to 700
      else if (backmode .eq. 1) then
         message = 'Using background values from Marshall table'
         call xwrite(message,1)
         go to 700
      endif

      call uclgsr('backrate1', bkgd, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get BACKGROUND RATE parameter'
         go to 999
      endif
      call uclgsr('backerr1', berr, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get BACKGROUND RATE parameter'
         go to 999
      else if (berr .le. 0.0) then
         message = ' Error provided is less than 0, using 3% of the' //
     +     ' background'
         call xwrite(message,1)
         berr = 0.03 * bkgd
      endif
         
 700  call uclgsi('lchat', lchat, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get LCHAT parameter'
         go to 999
      endif


      call uclgsi('tchat', tchat, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get TCHAT parameter'
         go to 999
      endif


      call uclgsb('clobber', clobber, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get CLOBBER parameter'
         go to 999
      endif


 999  if (irafsts .ne. 0) call xaerror(message,1)

      return

      end



C----------------------------------------------------------------------------
C Writes the appropriate error message to the output device when a user 
C specifies an illegal detector number
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 1.0   27 Mar 1996

      subroutine baddetvalue(i)

      integer i
      character(80) message

      if (i .eq. 1) then
         message = 'Illegal value!  Please select one of the'
         call xwrite(message,1)
         message = 'following detectors for the first light curve:'
      else if (i .eq. 2) then 
        message = 'Illegal value!  Please select one of the'
         call xwrite(message,1)
         message = 'following detectors for the second light curve:'
      else if (i .eq. 3) then 
        message = 'Illegal value!  Please select one of the'
         call xwrite(message,1)
         message = 'following detectors for the third light curve:'

      else 
        message = 'Illegal value!  Please select one of the'
         call xwrite(message,1)
         message = 'following detectors for the fourth light curve:'


      endif
      call xwrite(message,1)
      message = '1   Medium Energy Detector (MED)'
      call xwrite(message,1)
      message = '2   High Energy Detector 1 (HED-1)'
      call xwrite(message,1)
      message = '3   High Energy Detector 2 (HED-2)'
      call xwrite(message,1)
      message = '4   High Energy Detector 3 (HED-3)'
      call xwrite(message,1)

      return

      end

C----------------------------------------------------------------------------
C Writes the appropriate error message to the output device when a user 
C specifies an illegal scalar number
C
C Author: Jesse S. Allen (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 1.0   27 Mar 1996

      subroutine badscalar(i)

      integer i
      character(80) message

      if (i .eq. 1) then
        message = 'Illegal value!  Please select one of the'
         call xwrite(message,1)
         message = 'following values for the first discovery scalar:'

      else if (i .eq. 2) then 
        message = 'Illegal value!  Please select one of the'
         call xwrite(message,1)
         message = 'following values for the second discovery scalar:'

      else if (i .eq. 3) then 
        message = 'Illegal value!  Please select one of the'
         call xwrite(message,1)
         message = 'following values for the third discovery scalar:'

      else 

        message = 'Illegal value!  Please select one of the'
         call xwrite(message,1)
         message = 'following values for the fourth discovery scalar:'

      endif
      call xwrite(message,1)
      message = '0   No scalar (sets subsequent scalars to 0 also)'
      call xwrite(message,1)
      message = ' 1   Left 1  '
      call xwrite(message,1)
      message = ' 2   Right 1 '
      call xwrite(message,1)
      message = ' 3   Left 2  '
      call xwrite(message,1)
      message = ' 4   Right 2  '
      call xwrite(message,1)
      message = ' Discovery scalars 5 - 8 are time dependent'
      call xwrite(message,1)
      message = 'combinations of spectral windows'
      call xwrite(message,1)
      message = ' 5   Spectral window combination 1 (left)'
      call xwrite(message,1)
      message = ' 6   Spectral window combination 2 (right)'
      call xwrite(message,1)
      message = ' 7   Spectral window combination 3 (left)'
      call xwrite(message,1)
      message = ' 8   Spectral window combination 4 (right)'
      call xwrite(message,1)
      message = ' Discovery scalars are added unless they are '
      call xwrite(message,1)
      message = 'preceeded by a - sign'
      call xwrite(message,1)

      return

      end
