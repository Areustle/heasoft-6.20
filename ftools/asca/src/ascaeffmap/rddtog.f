
      SUBROUTINE rddtog(filenm, nchan, nungr, phabuf, ierr, itype, 
     &                  qsysdt)

      CHARACTER*(*) filenm
      INTEGER nchan, nungr
      REAL phabuf(nungr, 3)
      INTEGER ierr, itype
      LOGICAL qsysdt

c kaa 4/9/93
c	XSPEC subroutine called by RDDTRT to read from a spectral data file 
c	in OGIP FITS format. Broken out from RDDTRT.
c 11/16/93 Added ability to read files which have some channels missing
c          from the beginning or end. Only works if TLMIN1 and TLMAX1 keywords
c          exist in the BINTABLE extension.
c
c
c	filenm		c*(*)    i: The name of the SF PHA file
c       nchan		i        i: The number of data points (grouped)
c       nungr           i        i: The number of data points (ungrouped)
c       phabuf		r        r: The data, the first row of which contains
c                                   the data, the second contains the variance,
c                                   and the third the systematics values.
c       ierr            i        r: Error flag (0= no error)
c       itype           i        r: The type of data,
c					0 = counts,
c					1 = counts/sec,
c                                       2 = counts/sec/area
c       qsysdt          l        r: there was a systematics packages.

      INTEGER MAXPHS
      PARAMETER (MAXPHS=4096)

      REAL syserr
      INTEGER index, ftstat, ibin
      INTEGER unit, ndata, nbin, j
      INTEGER llchan, hlchan, ldchan, hdchan, mchan
      character(1) cgroup(MAXPHS)
      CHARACTER comment*80, context*80, wrtstr*255
      LOGICAL qpoiss, qanyf


      context = 'RDDTOG : FITSIO error'

      ftstat = 0
c      CALL getlun(unit)
      call ftgiou (unit, ftstat)
      CALL ftopen(unit, filenm, 0, index, ftstat)
      IF (ftstat .NE. 0) THEN
         ierr = ftstat
c         CALL frelun(unit)
         ftstat = 0
         call ftfiou (unit, ftstat)
         RETURN
      ENDIF

c  go to the BINTABLE extension which includes the spectral data.
c  identify the spectrum either by the EXTNAME='SPECTRUM' or 
c  HDUCLAS1='SPECTRUM'

      CALL fndext(unit, 'SPECTRUM', ' ', context, ftstat)

c  Find the number of stored data points (may not be = nchan)

      CALL ftgkyj(unit, 'NAXIS2', ndata, comment, ftstat)
      IF (ftstat .NE. 0) THEN
         context = 'RDDTOG: Failed to read NAXIS2'
         GOTO 100
      ENDIF

c  Find out whether this is `pha' or `pha per sec' data.

      itype = -1
      CALL ftgcno(unit, .FALSE., 'COUNTS', index, ftstat)
      IF ( (ftstat .EQ. 0) .AND. (index .NE. 0) ) THEN
         itype = 0
      ELSE
         ftstat = 0
         CALL ftgcno(unit, .FALSE., 'RATE', index, ftstat)
         IF ( (ftstat .EQ. 0) .AND. (index .NE. 0) ) itype = 1
      ENDIF
      ftstat = 0

      IF (itype .EQ. -1) THEN
         context = 'RDDTOG: neither COUNTS nor RATE columns found'
         GOTO 100
      ENDIF

c  Check whether the TLMIN1 and TLMAX1 keywords are set. If so then
c  read the start and end channels in the data.

      CALL ftgkyj(unit, 'TLMIN1', llchan, comment, ftstat)
      IF (ftstat .NE. 0) THEN
         ftstat = 0
         llchan = -1
      ENDIF
      CALL ftgkyj(unit, 'TLMAX1', hlchan, comment, ftstat)
      IF (ftstat .NE. 0) THEN
         ftstat = 0
         hlchan = -1
      ENDIF

      IF (llchan .NE. -1 .AND. hdchan .NE. -1) THEN
         CALL ftgcvj(unit, 1, 1, 1, 1, 0, ldchan, qanyf, ftstat)
         CALL ftgcvj(unit, 1, ndata, 1, 1, 0, hdchan, qanyf, ftstat)
         IF (ftstat .NE. 0) THEN
            context = 'RDDTOG: Failed to read CHANNEL data'
            GOTO 100
         ENDIF
      ENDIF

c  If counts data then extract into phabuf

      IF (itype .EQ. 0) THEN

         CALL ftgcve(unit, index, 1, 1, ndata, 0., phabuf, qanyf,
     &               ftstat)
         IF (ftstat .NE. 0) THEN
            context = 'RDDTOG: Failed to read COUNTS data'
            GOTO 100
         ENDIF

c  check for the Poisson error keyword

         qpoiss = .FALSE.
         CALL ftgkyl(unit, 'POISSERR', qpoiss, comment, ftstat)
         IF (ftstat .NE. 0) THEN
            context = 'RDDTOG: Failed to read POISSERR keyword'
            GOTO 100
         ENDIF

c  if the Poisson error keyword is set then errors are square roots
c  otherwise load them from the stat_err column

         IF (qpoiss) THEN

            DO ibin = 1, ndata
               phabuf(ibin, 2) = max(sqrt(phabuf(ibin,1)), 1.)
            ENDDO

         ELSE

            CALL ftgcno(unit, .FALSE., 'STAT_ERR', index, ftstat)
            IF (ftstat .NE. 0) THEN
               context = 'RDDTOG: Failed to find STAT_ERR column'
               GOTO 100
            ENDIF
            CALL ftgcve(unit, index, 1, 1, ndata, 0., phabuf(1,2),
     &                  qanyf, ftstat)
            IF (ftstat .NE. 0) THEN
               context = 'RDDTOG: Failed to read STAT_ERR data'
               GOTO 100
            ENDIF

         ENDIF

c  otherwise read the `pha per sec' data

      ELSEIF (itype .EQ. 1) THEN

         CALL ftgcno(unit, .FALSE., 'RATE', index, ftstat)
         IF (ftstat .NE. 0) THEN
            context = 'RDDTOG: Failed to find RATE column'
            GOTO 100
         ENDIF
         CALL ftgcve(unit, index, 1, 1, ndata, 0., phabuf,
     &               qanyf, ftstat)
         IF (ftstat .NE. 0) THEN
            context = 'RDDTOG: Failed to read RATE data'
            GOTO 100
         ENDIF

c  and the (mandatory) statistical errors

         CALL ftgcno(unit, .FALSE., 'STAT_ERR', index, ftstat)
         IF (ftstat .NE. 0) THEN
            context = 'RDDTOG: Failed to find STAT_ERR column'
            GOTO 100
         ENDIF
         CALL ftgcve(unit, index, 1, 1, ndata, 0., phabuf(1,2),
     &               qanyf, ftstat)
         IF (ftstat .NE. 0) THEN
            context = 'RDDTOG: Failed to read STAT_ERR data'
            GOTO 100
         ENDIF

      ENDIF

c  check for systematic errors

      CALL ftgkye(unit, 'SYS_ERR', syserr, comment, ftstat)
      IF (ftstat .EQ. 0) THEN
         IF (syserr .EQ. 0.) THEN
            qsysdt = .FALSE.
         ELSE
            DO ibin = 1, ndata
               phabuf(ibin, 3) = syserr
            ENDDO
            qsysdt = .TRUE.
         ENDIF
      ELSE
         ftstat = 0
         CALL ftgcno(unit, .FALSE., 'SYS_ERR', index, ftstat)
         IF ( (ftstat .NE. 0) .AND. (ftstat .NE. 219) ) THEN
            context = 'RDDTOG: Failed to find SYS_ERR column'
            GOTO 100
         ENDIF
         IF ( index .GT. 0 ) THEN
            CALL ftgcve(unit, index, 1, 1, ndata, 0., phabuf(1,3),
     &                  qanyf, ftstat)
            IF (ftstat .NE. 0) THEN
               context = 'RDDTOG: Failed to read SYS_ERR data'
               GOTO 100
            ENDIF
         ENDIF
         qsysdt = .TRUE.
      ENDIF

c  convert errors to variance

      DO ibin = 1, ndata
         phabuf(ibin,2) = phabuf(ibin,2)**2
      ENDDO

c  if systematic have been added then add these to the statistical
c  errors.

      IF (qsysdt) THEN

         DO ibin = 1, ndata
            phabuf(ibin, 2) = phabuf(ibin, 2) +
     &            (phabuf(ibin, 1)*phabuf(ibin, 3))**2
         ENDDO
      ENDIF

c Now need to catch the case of the actual channel range in the data
c not being the same as the legal channel range.

      IF ( llchan .GE. 0 .AND. llchan .LT. ldchan .AND.
     &     hlchan .GE. 0 .AND. hlchan .LT. hdchan ) THEN

c Shift the data up to leave the extra low channels - it doesn't matter
c what they are set to since they are just about to be ignored.

         mchan = ldchan - llchan
         DO j = 1, 3
            DO ibin = ndata, 1, -1
               phabuf(ibin+mchan,j) = phabuf(ibin,j)
            ENDDO
         ENDDO

         nchan = nchan + mchan + (hdchan-hlchan)

      ENDIF

c Now bin up data if the grouping has been set - OGIP 92a format
c always stores unbinned data. First extract grouping and quality
c info and set up a grouping character array

      CALL rdgrpc (unit, 1, ndata, MAXPHS, cgroup, nbin, ftstat)
      IF (ftstat .NE. 0) THEN
         context = 'RDDTOG: Failure in RDGRPC'
         GOTO 100
      ENDIF

c Now we can bin the data and variances.

      CALL grpdat(phabuf(1,1), ndata, cgroup, ndata)
      CALL grpdat(phabuf(1,2), ndata, cgroup, ndata)

c Close the FITS file

      ftstat = 0
      CALL ftclos(unit, ftstat)
c      CALL frelun(unit)
      ftstat = 0
      call ftfiou (unit, ftstat)

      RETURN

c Check for any FITSIO errors

  100 CONTINUE
      IF (ftstat .NE. 0) THEN
         call fcecho (context)
         WRITE(wrtstr,'(''   FITSIO = '', i4)') ftstat
         call fcecho (wrtstr)
      ENDIF

      ftstat = 0
      CALL ftclos(unit, ftstat)
      ftstat = 0
      call ftfiou (unit, ftstat)

      RETURN

      END
