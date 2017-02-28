      SUBROUTINE chkrmf(nfiles, inrmfs, infact, iunit, 
     &                  nphbns, Maxne,Maxgrp, Maxelt, 
     &                  telescop, instrume, detnam, filter, chntyp, 
     &                  flchan, thresh, hduvers, hduclas3, contxt, 
     &                  extnm,extnm_comm,
     &                  status)

      INTEGER nfiles
      REAL    thresh, infact(nfiles)
      INTEGER nphbns, Maxne, Maxgrp, Maxelt, status, flchan
      INTEGER iunit
      CHARACTER*(*) inrmfs(nfiles)
      CHARACTER*(*) contxt, telescop, instrume, detnam, filter, chntyp
      CHARACTER*(*) hduvers, hduclas3
      CHARACTER*(*) extnm, extnm_comm

c  Subroutine to return the number of PHA bins and do some checks to
c  make sure that the RMFs that the user wants to add can actually be
c  added with some hope of a sensible result.

c  Arguments :
c     nfiles         i        i: Number of RMFs
c     inrmfs         c        i: RMF names
c     infact         r        i: RMF weighting factors
c     iunit          i        i: RMF I/O unit
c     nphbns         i        r: Number of PHA bins
c     Maxne          i        r: Size of energy array
c     Maxgrp         i        r: Size of group array
c     Maxelt         i        r: Size of Matrix array
c     telescop       c        r: Telescope name
c     instrume       c        r: Instrument name
c     detnam         c        r: Detector name
c     filter         c        r: Filter name
c     chntyp         c        r: Channel type (PI or PHA)
c     flchan         i        r: First channel in RESPONSE
c     contxt         c        r: Description of error
c     status         i        r: Status  0 = OK, !0 = !OK

c ----------------- Authors/modifications ------------------------
c Banashree M Seifert (12 March, 1997)
c          . correction for hduclas3
c             (otherwise it was blank hduclas3
c
c Alex M.   (Feb. 11, 1999)
c          . Added new variables (parameters) Maxne, Maxgrp, and 
c            Maxelt and the call to RMFSZ that returns these 
c            parameters.
c AM        (Apr. 9, 1999)
c          . disabled the checking if Maxgrp and Maxelt are the same 
c            for all input files.
c Ziqin Pan (Jan. 13, 2003)
c          . add extnm and extnm_comm to get first input file 
c            extname and extname comment
c ------------------------------------------------------------------
      REAL    rvalue

      INTEGER rmfsiz, nfound, htype, ifile, index, chan0
      INTEGER next(10)
      INTEGER ivalue, ivalue1, ivalue2, ivalue3, j

      character(511) comment, outstr
      character(20) instr(2), cvalue
      character(20) outhdu(9,10), extnam(10), outver(9,10)

      LOGICAL qanyf

      status = 0
      thresh = 1.e30
      ivalue = 0
      index  = 0
      chan0  = 0

c Loop round the input files
     
      DO ifile = 1, nfiles

c Open the RMF
         rmfsiz=2880
         CALL ftopen(iunit, inrmfs(ifile), 0, rmfsiz, status)
         WRITE(contxt,'(''CHKRMF : Failed to open RMF # '',i4)') ifile
         IF ( status .NE. 0 ) RETURN

c Find the extension with the energy bounds

         instr(1) = 'RESPONSE'
         instr(2) = 'EBOUNDS'
         CALL fndhdu(5, iunit, 2, instr, 10, nfound, next, outhdu,
     &               outver, extnam, status)
         WRITE(contxt,'(a,i4)')
     & 'CHKRMF : Failed to find EBOUNDS extension in RMF # ', ifile
         IF ( nfound .EQ. 0 ) status = 1
         IF ( status .NE. 0 ) RETURN

c Go to the extension

         CALL ftmrhd(iunit, next(1), htype, status)
         WRITE(contxt,'(a,i4)')
     & 'CHKRMF : Failed to go to EBOUNDS extension in RMF # ', ifile
         IF ( status .NE. 0 ) RETURN

c Find which column contains the CHANNELs and read the first entry

         CALL ftgcno(iunit, .FALSE., 'CHANNEL', index, status)
         contxt = 'CHKRMF : Failed to find CHANNEL column'
         IF ( status .NE. 0 ) RETURN
         CALL ftgcvj(iunit, index, 1, 1, 1, 0, ivalue, qanyf, status)
         contxt = 'CHKRMF : Failed to read CHANNEL column'
         IF ( status .NE. 0 ) RETURN

c If the first file then set the CHAN0 variable otherwise check that 
c it is the same as previous files.

         IF ( ifile .EQ. 1 ) THEN
            chan0 = ivalue
            call ftgkys(iunit, 'EXTNAME', extnm, extnm_comm, status)
            IF (status .NE. 0) THEN
                extnm = ' '
                extnm_comm = ' '
                status = 0
            ENDIF

         ELSE
            IF ( ivalue .NE. chan0 ) THEN
               contxt = 
     &      'Incompatible RMFs : Channel zero points differ in EBOUNDS!'
               status = 1
	       return
            ENDIF
         ENDIF

c Close the file and reopen because we don't know which order the extensions
c are

         CALL ftclos(iunit, status)
         WRITE(contxt,'(''CHKRMF : Failed to close RMF # '',i4)') ifile
         IF ( status .NE. 0 ) RETURN
         CALL ftopen(iunit, inrmfs(ifile), 0, rmfsiz, status)
         WRITE(contxt,'(''CHKRMF : Failed to open RMF # '',i4)') ifile
         IF ( status .NE. 0 ) RETURN

c Find the extension with the redistribution matrix.

         instr(1) = 'RESPONSE'
         instr(2) = 'RSP_MATRIX'
         do j=1,10
             next(j)= 0
         enddo
         CALL fndhdu(5, iunit, 2, instr, 10, nfound, next, outhdu,
     &               outver, extnam, status)

         WRITE(contxt,'(a,i4)')
     & 'CHKRMF : Failed to find matrix extension in RMF # ', ifile
         IF ( nfound .EQ. 0 ) status = 1
         IF ( status .NE. 0 ) RETURN

c Go to the extension

         CALL ftmrhd(iunit, next(1), htype, status)
         WRITE(contxt,'(a,i4)')
     & 'CHKRMF : Failed to go to matrix extension in RMF # ', ifile
         IF ( status .NE. 0 ) RETURN

c Get the HDUVERS keyword - if this is not present then check for the
c HDUVERS2 keyword which may be used in older files.

         CALL ftgkys(iunit, 'HDUVERS', cvalue, comment, status)
         IF ( status .NE. 0 ) THEN
            status = 0
            CALL ftgkys(iunit, 'HDUVERS2', cvalue, comment, status)
            IF ( status .NE. 0 ) THEN
               status = 0
               CALL ftgkys(iunit, 'HDUVERS1', cvalue, comment, status)
            ENDIF
            contxt =
     &       'CHKRMF : Failed to read HDUVERS or HDUVERSn keywords'
            IF ( status .NE. 0 ) RETURN
         ENDIF

c If the first file then set the HDUCLAS3 amd HDUVERS variables 
c otherwise check that they are the same as previous files.

         IF ( ifile .EQ. 1 ) THEN
c this correction is done by BMS (April, 1997)
ccc            hduclas3 = outhdu(3,next(1))(1:len(hduclas3))
            hduclas3 = outhdu(3,1)(1:len(hduclas3))
            hduvers = cvalue(1:len(hduvers))
         ELSE
ccc            IF ( hduclas3 .NE. outhdu(3,next(1))(1:len(hduclas3)) ) THEN
            IF ( hduclas3 .NE. outhdu(3,1)(1:len(hduclas3)) ) THEN
               contxt = 
     &          'Incompatible RMFs : HDUCLAS3 keywords differ !'
               status = 1
               RETURN
            ENDIF
            IF ( hduvers .NE. cvalue(1:len(hduvers)) ) THEN
               CALL fcecho(
     &            'Warning : not all RMFS have the same HDUVERS')
            ENDIF
         ENDIF

c Get the DETCHANS keyword
         CALL ftgkyj(iunit, 'DETCHANS', ivalue, comment, status)      
         contxt = 'CHKRMF : Failed to find the DETCHANS keyword'
         IF ( status .NE. 0 ) RETURN

c if this is the first file then save it in NPHBNS otherwise check for
c consistency

         IF ( ifile .EQ. 1 ) THEN
            nphbns = ivalue
         ELSE
            IF ( ivalue .NE. nphbns ) THEN
               contxt = 
     &          'Incompatible RMFs : number of PHA bins differ !'
               status = 1
               RETURN
            ENDIF
         ENDIF

c and the Maxne, Maxgrp and Maxelt

         ivalue1 = 0
         ivalue2 = 0
         ivalue3 = 0
         CALL rmfsz(iunit, 5, ivalue1,ivalue2,ivalue3,status)
         contxt = 'CHKRMF : Failed to get RMF array sizes'
         IF ( status .NE. 0 ) RETURN

c if this is the first file then save it in NENERG otherwise check for
c consistency

         IF ( ifile .EQ. 1 ) THEN
            Maxne  = ivalue1
            Maxgrp = ivalue2
            Maxelt = ivalue3
         ELSE

            IF ( ivalue1 .NE. Maxne ) THEN
               contxt = 
     &          'Incompatible RMFs : energy array sizes differ !'
               status = 1
               RETURN
            ENDIF
            Maxgrp = MAX(Maxgrp, ivalue2)
            Maxelt = MAX(Maxelt, ivalue3)

         ENDIF

c Write a string giving the RMF name and weighting factor

         WRITE(outstr(1:14),'('' RMF # '',i4,'' : '')') ifile
         outstr(15:35) = inrmfs(ifile)(1:21)
         WRITE(outstr(36:52),'(5x,1pg12.5)') infact(ifile)
         CALL fcecho(outstr(1:52))


c Read the TELESCOP, INSTRUME, DETNAM, FILTER, and CHANTYPE keywords and 
c put together a description string for output. If this is the first file 
c then save these c keyword values

         outstr(1:14) = ' '

         CALL ftgkys(iunit, 'TELESCOP', cvalue, comment, status)
         contxt = 'CHKRMF : Failed to read the TELESCOP keyword'
         IF ( status .NE. 0 .AND. status .NE. 202 ) RETURN
         IF ( status .EQ. 202 ) THEN
            status = 0
            cvalue = 'NONE'
         ENDIF
         outstr(15:25) = cvalue(1:10)//' '
         IF ( ifile .EQ. 1 ) telescop = cvalue(1:MIN(len(cvalue),
     &                                               len(telescop)))

         CALL ftgkys(iunit, 'INSTRUME', cvalue, comment, status)
         contxt = 'CHKRMF : Failed to read the INSTRUME keyword'
         IF ( status .NE. 0 .AND. status .NE. 202 ) RETURN
         IF ( status .EQ. 202 ) THEN
            status = 0
            cvalue = 'NONE'
         ENDIF
         outstr(26:36) = cvalue(1:10)//' '
         IF ( ifile .EQ. 1 ) instrume = cvalue(1:MIN(len(cvalue),
     &                                               len(instrume)))

         CALL ftgkys(iunit, 'DETNAM', cvalue, comment, status)
         contxt = 'CHKRMF : Failed to read the DETNAM keyword'
         IF ( status .NE. 0 .AND. status .NE. 202 ) RETURN
         IF ( status .EQ. 202 ) THEN
            status = 0
            cvalue = 'NONE'
         ENDIF
         outstr(37:47) = cvalue(1:10)//' '
         IF ( ifile .EQ. 1 ) detnam = cvalue(1:MIN(len(cvalue),
     &                                             len(detnam)))

         CALL ftgkys(iunit, 'FILTER', cvalue, comment, status)
         contxt = 'CHKRMF : Failed to read the FILTER keyword'
         IF ( status .NE. 0 .AND. status .NE. 202 ) RETURN
         IF ( status .EQ. 202 ) THEN
            status = 0
            cvalue = 'NONE'
         ENDIF
         outstr(48:58) = cvalue(1:10)//' '
         IF ( ifile .EQ. 1 ) filter = cvalue(1:MIN(len(cvalue),
     &                                             len(filter)))

         CALL ftgkys(iunit, 'CHANTYPE', cvalue, comment, status)
         contxt = 'CHKRMF : Failed to read the CHANTYPE keyword'
         IF ( status .NE. 0 .AND. status .NE. 202 ) RETURN
         IF ( status .EQ. 202 ) THEN
            status = 0
            cvalue = 'NONE'
         ENDIF
         outstr(59:69) = cvalue(1:10)//' '
         IF ( ifile .EQ. 1 ) chntyp = cvalue(1:MIN(len(cvalue),
     &                                             len(chntyp)))

         CALL fcecho(outstr(1:69))

c Read the threshold keyword and store the smallest of those read

         CALL ftgkye(iunit, 'LO_THRES', rvalue, comment, status)
         contxt = 'CHKRMF : Failed to read the LO_THRES keyword'
         IF ( status .NE. 0 .AND. status .NE. 202 ) RETURN
         IF ( status .EQ. 202 ) THEN
            status = 0
            rvalue = 0.
         ENDIF
         thresh = MIN(thresh, rvalue)

c Check for the presence of a TLMIN4 keyword - if it doesn't exist
c then assume that the channels start at 1.

         CALL ftgkyj(iunit, 'TLMIN4', ivalue, comment, status)
         IF ( status .NE. 0 ) THEN
            status = 0
            ivalue = 1
         ENDIF

c If the first file then set the FLCHAN variable otherwise check that 
c it is the same as previous files.

         IF ( ifile .EQ. 1 ) THEN
            flchan = ivalue
         ELSE
            IF ( ivalue .NE. flchan ) THEN
               contxt = 
     &  'Incompatible RMFs : Channel zero points differ in RESPONSE!'
               status = 1
	       return
            ENDIF
         ENDIF

         CALL ftclos(iunit, status)
         WRITE(contxt,'(''CHKRMF : Failed to close RMF # '',i4)') ifile
         IF ( status .NE. 0 ) RETURN

      ENDDO

      RETURN
      END


