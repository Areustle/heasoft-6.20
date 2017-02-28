C ********************************************************************
C SUBROUTINE:
C     rd_quatrns
C
C DESCRIPTION:
C     obtain the average value of the quaternions from the XTEFILT file
C     between the input start and stop times
C
C AUTHOR:
C     James Lochner 5/95
C
C MODIFICATION HISTORY:
C  Oct 30, 1995 - converted jitter criterion from std dev in the quaternions
C                to degrees.
C  Feb. 7, 1996 - added check for null value when reading ACSESTQ(1-4) so
C                that XTE FILTER file may be used.
C  Feb 12, 1996 - Use jitter criterion in seconds of arc.  Also bug fix:
C                change computation of sdquat to DOUBLE PRECISION
C  Sep 25, 1997 - Bug fixes: expand format for time allow output of
C                times greater than 1.0E08; increment the loop through
C                the quat file when the quat value is INDEF.
C NOTES:
C 
C USEAGE:
C CALL rd_quartns(ngti, tstart_in, tstop_in, mjdref_in, xtefilt, quat, ierr)
C
C ARGUMENTS:
C     ngti         - number of good time intervals/dimension of tstart_in      
C     tstart_in    - desired start time
c     tstop_in     - desired stop time
c     mjdref_in    - mjd reference value for start and stop times
c     xtefilt      - name of xtefilter file for the observation
c     jitter       - nominal s/c jitter
c     quat         - output array of average quaternions
C     ierr         - error flag
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED SUBROUTINES:
C
C ******************************************************************

      SUBROUTINE rd_quatrns(ngti, tstart_in, tstop_in, mjdref_in,
     &  xtefilt, coor, jitter, sccoor, chat, ierr)
      
      IMPLICIT NONE
      CHARACTER*(*) xtefilt
      INTEGER ngti
      DOUBLE PRECISION tstart_in(ngti), tstop_in(ngti), mjdref_in
      REAL coor(3), jitter, sccoor(3)
      INTEGER ierr, chat
      INTEGER qsize
      PARAMETER(qsize=500000)

      LOGICAL anynull, abort
      character(8) acsqname(4)
      character(80) context, errstr, comment
      character(120) msg
      character(160) ephfiles(5) 
      INTEGER i, iunit, frow, felem, block, ftstatus, htype, nrows
      INTEGER acscol(4), timecol, ntimes, ephnofiles
      REAL nlvale, sc_jitter
      REAL att(3,3)
      REAL quat(4)
      DOUBLE PRECISION acsestq1(qsize), acsestq2(qsize)
      DOUBLE PRECISION acsestq3(qsize), acsestq4(qsize)
      DOUBLE PRECISION y, z, sdy, sdz, sumy, sumz, sqry, sqrz
      DOUBLE PRECISION sdoffy, sdoffz
      DOUBLE PRECISION tstart, tstop, mjdref, time(qsize)
      DOUBLE PRECISION nlvald
      INTEGER igti
      DOUBLE PRECISION arc_rad
      arc_rad = 3600.0 * 180.0 / 3.14159
      
      ftstatus = 0
      frow = 1
      felem = 1
      
C     Open the xte filter file
      CALL ftgiou(iunit,ftstatus)
      CALL fcgcls(xtefilt,ephfiles,ephnofiles,abort)
      IF (ephnofiles .gt. 1) THEN
        CALL fcecho('More than one Filter/Attitude file !')
        CALL fcecho('Using ony the first one !')
      ENDIF      
      CALL ftopen(iunit,ephfiles(1),0,block,ftstatus)
      IF (ftstatus .NE. 0) THEN
        context = 'unable to open attitude file'
        CALL fcerr(context)
        GOTO 999
      ENDIF

      CALL ftmrhd(iunit,1,htype,ftstatus)
      IF (ftstatus .NE. 0) THEN
        context = 'error moving to extension '
        CALL fcerr(context)
        GOTO 999
      ENDIF

C     Get the TSTART, TSTOP and MJDREF times in the filter file.
      CALL startnstop(iunit,xtefilt,tstart,tstop,mjdref,ierr)
      
      IF (chat .GE. 10) THEN
        CALL fcecho('Attitude file start and stop time:')
        WRITE(msg,'(f16.6, 2X, F16.6)') tstart, tstop
        CALL fcecho(msg)
        call fcecho('PHA file start and stop time(s): ')
        DO igti = 1, ngti
          WRITE(msg,'(f16.6, 2X, F16.6)')
     &      tstart_in(igti), tstop_in(igti)
          CALL fcecho(msg)
        ENDDO
      ENDIF

C     Compare filter start/stop times with input start/stop times
C     If they don't overlap, exit with fatal error
C     for now, assume tstart_in and tstart are in the same units.
C     If not, then you'd have to get them in the same units by
C     comparing the timeunit keywords from both pha and filt file.
        
      IF ((tstop_in(ngti).LT. tstart) .OR.
     &    (tstart_in(1) .GT. tstop)) THEN
           ierr = 2
           msg = 'ERROR: Times in XTE attitude file do not overlap'//
     &          ' integration times in PHA file.'
           GOTO 999
      ELSE IF ((tstart_in(1).LT. tstart) .AND.
     &        (tstop_in(ngti) .GT. tstop)) THEN
        ierr = 1
        msg = 'WARNING: Times in attitude file cover only a'//
     &          ' portion of times in pha file.'
      ELSE IF (tstart_in(1).LT. tstart) THEN
        ierr = 1
        WRITE(msg, '((A), F16.6, (A), F16.6, (A))') 
     &    'WARNING: PHA start time: ', tstart_in(ngti),
     &    ' preceeds XTE Filter start time: ', tstart,
     &    '. Using only overlap.'
      ELSE IF (tstop_in(ngti) .GT. tstop) THEN
        ierr = 1
        WRITE(msg, '((A), F16.6, (A), F16.6, (A))') 
     &    'WARNING: PHA stop time: ', tstop_in(ngti),
     &    ' exceeds XTE Filter stop time: ', tstop, 
     &    '. Using only overlap.'
      ELSE
        tstart = tstart_in(1)
        tstop = tstop_in(ngti)
      ENDIF

      IF (ierr .NE. 0) THEN
        ierr = 0
        CALL fcecho(msg)
      ENDIF

c     Get the number of rows from keyword NAXIS2
      CALL ftgkyj(iunit,'NAXIS2',nrows,comment,ftstatus)
      IF (ftstatus .NE. 0) THEN
        errstr = 'error reading nrows keyword from FITS file'
        CALL fcerr(errstr)
        GOTO 999
      ENDIF
      IF (nrows .gt. qsize) THEN
        ierr = 2
        msg = 'HXTARF array storage not large enough for quaternions'
        GOTO 999
      END IF
               
C     Determine the column numbers for the TIME and ASCESTQ values
        acsqname(1) = 'ACSESTQ1'
        acsqname(2) = 'ACSESTQ2'
        acsqname(3) = 'ACSESTQ3'
        acsqname(4) = 'ACSESTQ4'
        CALL ftgcno(iunit,.false.,'TIME',timecol,ftstatus)
        IF (ftstatus .NE. 0) THEN
           errstr = 'error getting time column from attitude file'
           CALL fcerr(errstr)
           GOTO 999
        ENDIF
        DO i = 1,4
           CALL ftgcno(iunit,.false.,acsqname(i),acscol(i),ftstatus)
           IF (ftstatus .NE. 0) THEN
              errstr = 'error getting quaternion columns from file'
              CALL fcerr(errstr)
              GOTO 999
           ENDIF
        ENDDO
        
C     read TIME column, and ACSESTQ(1,2,3,4) columns in filter file
C     (i.e. Use Estimated Quaternions)
        CALL ftgcvd(iunit,timecol,frow,felem,nrows,nlvald,time,
     +       anynull,ftstatus)
        IF (ftstatus .NE. 0) THEN
           errstr = 'error reading the time column from attitude file'
           CALL fcerr(errstr)
           GOTO 999
        ENDIF
        nlvale = -99.0
        nlvald = -99.0
        CALL ftgcvd(iunit,acscol(1),frow,felem,nrows,nlvald,
     &       acsestq1,anynull,ftstatus)
        IF (ftstatus .NE. 0) THEN
           errstr = 'error reading the quaternions from attitude file'
           CALL fcerr(errstr)
           GOTO 999
         ENDIF
        CALL ftgcvd(iunit,acscol(2),frow,felem,nrows,nlvald,
     &       acsestq2,anynull,ftstatus)
        CALL ftgcvd(iunit,acscol(3),frow,felem,nrows,nlvald,
     &       acsestq3,anynull,ftstatus)
        CALL ftgcvd(iunit,acscol(4),frow,felem,nrows,nlvald,
     &    acsestq4,anynull,ftstatus)

C       Close the filter/quaternion file
        CALL ftclos(iunit,ftstatus)
        CALL ftfiou(iunit,ftstatus)


        IF (chat .GE. 20) THEN
           WRITE(msg,'(a,f16.6,a,f16.6)')
     &          'About to go through times in quat file between ',
     &          tstart,' and ',tstop
           CALL fcecho(msg)
        ENDIF
      
C
        ntimes = 0
        y = 0.0D0
        z = 0.0D0
        sumy = 0.0D0
        sumz = 0.0D0
        sqry = 0.0D0
        sqrz = 0.0D0
        i=1
        igti = 1
        DO WHILE (i .LE. nrows .AND. igti .LE. ngti)
          IF (time(i) .LT. tstart_in(igti)) THEN
C           Advance to the next quaternion            
             i = i + 1
          ELSE IF (time(i) .GT. tstop_in(igti)) THEN
C           Advance to the next GTI if there is one            
             igti = igti + 1
          ELSE IF (acsestq1(i) .NE. nlvale) THEN
C           Time is within range of the current GTI and not an INDEF.
            quat(1) = REAL(acsestq1(i))
            quat(2) = REAL(acsestq2(i))
            quat(3) = REAL(acsestq3(i))
            quat(4) = REAL(acsestq4(i))
C           Compute the source position in the spacecraft coord.
            CALL xteamat(quat,att)
            CALL xtetoirf(att,coor,sccoor,1)
C           Update the statistics            
            y = DBLE(sccoor(2))
            z = DBLE(sccoor(3))
            sumy = sumy + y
            sumz = sumz + z
            sqry = sqry + y * y
            sqrz = sqrz + z * z
C           Update the tally, and advance to the next quaternion            
            ntimes = ntimes + 1
            i = i + 1
         ELSE
C          Time within range, but quaternion is INDEF. 
C           So just proceed to the next quaternion.
            i = i + 1
         ENDIF                  ! (time)
        ENDDO ! (WHILE (i))

        IF (chat .GE. 10) THEN
          WRITE(msg,'(a,i12)') 'number of accepted quaternions = ',
     &                         ntimes
          CALL fcecho(msg)
        ENDIF

C Compute the average and standard deviation of the spacecraft coords
        sdy = 0.0D0
        sdz = 0.0D0
        IF (ntimes .GT. 0) THEN
          y = sumy / DBLE(ntimes)
          z = sumz / DBLE(ntimes)
        ENDIF
        IF (ntimes .GT. 1.0) THEN
          sdy = DSQRT((sqry-sumy*sumy/DBLE(ntimes))/DBLE(ntimes-1))
          sdz = DSQRT((sqrz-sumz*sumz/DBLE(ntimes))/DBLE(ntimes-1))
        ENDIF

C Return these coordinates in sccoor        
        sccoor(2) = REAL(y)
        sccoor(3) = REAL(z)
        sccoor(1) = REAL(1.0D0 - y*y - z*z)
        
C Convert the y and z jitters into arcseconds, and just take the
C maximum (as y and z jitters are probably NOT independent).
C Remember that for the offset angle (in radian) along each axis,
C        tan(off) =  [y,z], so sd[y,z] = sec^2(offset)* sdoff[y,z]
C                                      = 1+tan^2(offset) * sdoff[y,z]
C                                      = 1+[y,z]**2        
        sdoffy = sdy / (1.0D0 + y*y)
        sdoffz = sdz / (1.0D0 + z*z)
        sc_jitter = REAL(MAX(sdoffy, sdoffz)) * arc_rad

        IF (chat .GE. 10) THEN
          CALL fcecho('Source offset in spacecraft coordinates '//
     &      '(relative to spacecraft X-axis):')
          WRITE(msg,'(A, F7.1, A, F7.1, A)')
     &      'Y=', ATAN(y) * arc_rad, '+/-', sdoffy * arc_rad, ' arcsec'
          CALL fcecho(msg)
          WRITE(msg, '(A, F7.1, A, F7.1, A)')
     &      'Z=', ATAN(z) * arc_rad, '+/-', sdoffz * arc_rad, ' arcsec.'
          CALL fcecho(msg)
        ENDIF
                
C Report if std. dev are outside of accepted jitter        
        IF (sc_jitter  .GT. jitter) THEN
           WRITE(msg,'(a,f7.1,a,f5.1,a)')
     &     'WARNING - pointing jitter of ',sc_jitter,
     &     ' arcsec is larger than nominal value of ',jitter,
     &     ' arcsec.'
           CALL fcecho(msg)
         ENDIF

999     IF (ierr .NE. 0) CALL fcecho(msg)
        IF (ftstatus .NE. 0) THEN
           CALL fcerrm(ftstatus)
           ierr = 2
        ENDIF        
        IF (ierr .eq. 1) ierr = 0

        RETURN
      END
