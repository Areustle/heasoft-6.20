
      SUBROUTINE rdgrpc (iunit, ispec, nchan, maxgcd, gcard, nbin, 
     &                   ierrsf)

      INTEGER maxgcd, nchan, nbin, iunit, ierrsf, ispec
      character(1) gcard(maxgcd)

c Gets the grouping and quality information from a standard FITS file 
c and returns in the grouping card.

c	iunit		i	i: I/O unit
c       ispec           i       i: the row required (type-II files only)
c	nchan		i	i: ungrouped channels
c	nbin		i	r: channels after grouping
c	maxgcd		i	i: max size of grouping card
c	gcard		c	r: grouping card
c	ierrsf		i	r: error flag
c				   0  : success
c                                 -1  : too many channels for the grouping card
c				   >0: FITSIO error

      INTEGER MAXPHS
      PARAMETER(MAXPHS=4096)

      INTEGER gvalue(MAXPHS), qvalue(MAXPHS)

      INTEGER igcol, iqcol, i, value, llchan, ldchan, hlchan, hdchan
      INTEGER ndata, ftype, index, ijunk
      CHARACTER comment*45, contxt*72
      character(256) wrtstr
      LOGICAL qanyf, qqual, qgroup

c  Check whether this is a type-I single spectrum file or a type-II
c  multiple spectrum file.

      ftype = 1
      CALL ftgcno(iunit, .FALSE., 'SPEC_NUM', index, ierrsf)
      IF ( (ierrsf .EQ. 0) .AND. (index .NE. 0) ) ftype = 2
      ierrsf = 0

c  First find out whether GROUPING and/or QUALITY keywords exist
c  If they do then they ought to be zero - if they aren't then
c  complain. If keywords do not exist then appropriate logicals
c  are set to true so that data columns are searched for.

      ierrsf = 0
      qgroup = .TRUE.
      CALL ftgkyj(iunit, 'GROUPING', value, comment, ierrsf)
      IF (ierrsf .EQ. 0) THEN
         qgroup = .FALSE.
         IF (value .NE. 0) THEN
            WRITE(wrtstr, '(a,1x,i6)')
     &       ' WARNING in RDGRPC : Unexpected value of GROUPING :',
     &       value
            CALL xwrite(wrtstr, 5)
            CALL xwrite(
     &       ' Assuming that data is ungrouped', 5)
            value = 1
         ENDIF
         DO i = 1, nchan
            gvalue(i) = value
         ENDDO
      ENDIF
      ierrsf = 0

      qqual = .TRUE.
      CALL ftgkyj(iunit, 'QUALITY', value, comment, ierrsf)
      IF (ierrsf .EQ. 0) THEN
         qqual = .FALSE.
         IF (value .NE. 0) THEN
            WRITE(wrtstr, '(a,1x,i4)')
     &       ' WARNING in RDGRPC : Unexpected value of QUALITY :',
     &       value
            CALL xwrite(wrtstr, 5)
            CALL xwrite(
     &       ' Assuming that it is zero', 5)
            value = 0
         ENDIF
         DO i = 1, nchan
            qvalue(i) = value
         ENDDO
      ENDIF
      ierrsf = 0

c  If required finding the columns for the grouping and quality data
c  If they are required but don't exist then complain again

      IF (qgroup) THEN
         ierrsf = 0
         CALL ftgcno(iunit, .FALSE., 'GROUPING', igcol, ierrsf)
         IF (ierrsf .NE. 0 .OR. igcol .EQ. 0) THEN
            CALL xwrite(
     &       ' WARNING in RDGRPC : No grouping information found', 5)
            CALL xwrite(' All data assumed ungrouped', 5)
            igcol = 0
            ierrsf = 0
            DO i = 1, nchan
               gvalue(i) = 1
            ENDDO
         ENDIF
      ENDIF
      IF (qqual) THEN
         ierrsf = 0
         CALL ftgcno(iunit, .FALSE., 'QUALITY', iqcol, ierrsf)
         IF (ierrsf .NE. 0 .OR. iqcol .EQ. 0) THEN
            CALL xwrite(
     &       ' WARNING in RDGRPC : No quality information found', 5)
            CALL xwrite(' All data assumed good', 5)
            iqcol = 0
            ierrsf = 0
            DO i = 1, nchan
               qvalue(i) = 0
            ENDDO
         ENDIF
      ENDIF

c  Check if there are too many channels for the internal array. 

      IF (nchan .GT. maxgcd) THEN
         WRITE(wrtstr, '(a,1x,i6)')
     &         ' Error in RDGRPC : number of channels :', nchan
         CALL xwrite(wrtstr, 5)
         WRITE(wrtstr, '(a,1x,i6)')
     &         ' exceeds the size of the grouping character string :',
     &         maxgcd
         CALL xwrite(wrtstr, 5)
         ierrsf = -1
         RETURN
      ENDIF

c  Check whether the TLMIN1 and TLMAX1 keywords are set. If so then
c  read the start and end channels in the data.

      CALL ftgkyj(iunit, 'TLMIN1', llchan, comment, ierrsf)
      IF (ierrsf .NE. 0) THEN
         ierrsf = 0
         llchan = -1
      ENDIF
      CALL ftgkyj(iunit, 'TLMAX1', hlchan, comment, ierrsf)
      IF (ierrsf .NE. 0) THEN
         ierrsf = 0
         hlchan = -1
      ENDIF


      IF ( ftype .EQ. 1 ) THEN

         CALL ftgkyj(iunit, 'NAXIS2', ndata, comment, ierrsf)
         contxt = ' RDGRPC: Failed to read NAXIS2'
         IF (ierrsf .NE. 0) GOTO 100

         IF (llchan .NE. -1 .AND. hlchan .NE. -1) THEN
            CALL ftgcvj(iunit, 1, 1, 1, 1, 0, ldchan, qanyf, ierrsf)
            CALL ftgcvj(iunit, 1, ndata, 1, 1, 0, hdchan, qanyf, ierrsf)
            contxt = 'RDGRPC: Failed to read CHANNEL data'
            IF (ierrsf .NE. 0) GOTO 100
         ENDIF

      ELSEIF ( ftype .EQ. 2 ) THEN

         CALL ftgcno(iunit, .FALSE., 'CHANNEL', index, ierrsf)
         IF (ierrsf .NE. 0) THEN
            contxt = 'RDGRPC: Failed to find CHANNEL column'
            GOTO 100
         ENDIF
         CALL ftgtdm(iunit, index, 1, ijunk, ndata, ierrsf)
         IF (ierrsf .NE. 0) THEN
            contxt = 'RDGRPC: Failed to find size of CHANNEL column'
            GOTO 100
         ENDIF

         IF (llchan .NE. -1 .AND. hlchan .NE. -1) THEN
            CALL ftgcvj(iunit, index, ispec, 1, 1, 0, ldchan, qanyf, 
     &                  ierrsf)
            CALL ftgcvj(iunit, index, ispec, ndata, 1, 0, hdchan, 
     &                  qanyf, ierrsf)
            contxt = 'RDGRPC: Failed to read CHANNEL data'
            IF (ierrsf .NE. 0) GOTO 100
         ENDIF

      ENDIF

c  If required read the quality data

      IF (qqual .AND. iqcol .GT. 0) THEN
         IF ( ftype .EQ. 1 ) THEN
            CALL ftgcvj(iunit, iqcol, 1, 1, ndata, 0., qvalue, 
     &                  qanyf, ierrsf)
         ELSEIF ( ftype .EQ. 2 ) THEN
            CALL ftgcvj(iunit, iqcol, ispec, 1, ndata, 0., qvalue, 
     &                  qanyf, ierrsf)
         ENDIF
         contxt = 'RDGRPC: Failed to read QUALITY column'
         IF (ierrsf .NE. 0) GOTO 100
      ELSE
         DO i = 1, ndata
            qvalue(i) = 0
         ENDDO
      ENDIF

c  If required read the grouping data

      IF (qgroup .AND. igcol .GT. 0 ) THEN
         IF ( ftype .EQ. 1 ) THEN
            CALL ftgcvj(iunit, igcol, 1, 1, ndata, 0., gvalue, 
     &                  qanyf, ierrsf)
         ELSEIF ( ftype .EQ. 2 ) THEN
            CALL ftgcvj(iunit, igcol, ispec, 1, ndata, 0., gvalue, 
     &                  qanyf, ierrsf)
         ENDIF
         contxt = 'RDGRPC: Failed to read GROUPING column'
         IF (ierrsf .NE. 0) GOTO 100
      ELSE
         DO i = 1, ndata
            gvalue(i) = 1
         ENDDO
      ENDIF

c  If the actual channel range is not the same as the legal one
c  then shift the array while setting quality=1 for the channels
c  that do not exist.

      IF ( llchan .GE. 0 .AND. hlchan .GE. 0 .AND.
     &    (llchan .NE. ldchan .OR. hlchan .NE. hdchan) ) THEN

         DO i = ndata, 1, -1
            qvalue(i+ldchan-llchan) = qvalue(i)
            gvalue(i+ldchan-llchan) = gvalue(i)
         ENDDO
         DO i = 1, ldchan-llchan
            qvalue(i) = 1
         ENDDO
         DO i = ndata+ldchan-llchan+1, hlchan
            qvalue(i) = 1
         ENDDO

      ENDIF

c  Loop round the channels setting the grouping card

      nbin = 0
      DO i = 1, nchan

c  Test that the quality for this channel is sensible.

         IF (qvalue(i) .LT. -1 .OR. qvalue(i) .GT. 5) THEN
            WRITE(wrtstr, '(a,1x,i4)')
     &        ' WARNING in RDGRPC : unrecognized quality : ',
     &        qvalue(i)
            CALL xwrite(wrtstr, 5)
            CALL xwrite(' assuming good', 5)
            qvalue(i) = 0
         ENDIF


c  Test that the grouping for this channel is sensible.

         IF (qvalue(i) .EQ. 0 .AND. 
     &       gvalue(i) .NE. 1 .AND. gvalue(i) .NE. -1) THEN
            WRITE(wrtstr, '(a,i4,a,i4)')
     &        ' WARNING in RDGRPC : channel ', i,
     &        ' unrecognized grouping : ', gvalue(i)
            CALL xwrite(wrtstr, 5)
            CALL xwrite(' must be 1 or -1 - assuming 1', 5)
            gvalue(i) = 1
         ENDIF

c  Set the grouping card

         IF (qvalue(i) .EQ. 0) THEN
            IF (gvalue(i) .eq. 1) THEN
               gcard(i) = '+'
               nbin = nbin + 1
            ELSEIF (gvalue(i) .eq. -1) THEN
               gcard(i) = '-'
            ENDIF
         ELSEIF (qvalue(i) .EQ. 1) THEN
            gcard(i) = ' '
         ELSE
            gcard(i) = '*'
            nbin = nbin + 1
         ENDIF

      ENDDO

100   CONTINUE
      IF ( ierrsf .NE. 0 ) THEN
         CALL xwrite(contxt, 10)
         WRITE(contxt, '(a,i4)') 'FITSIO error = ', ierrsf
         CALL xwrite(contxt, 10)
      ENDIF

      RETURN

      END

