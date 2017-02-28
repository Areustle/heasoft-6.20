
      SUBROUTINE gtenrg1(rsu, rsfil, sensor, npha, lower, hiher,
     &                  middl, eff, status)

      INTEGER rsu, sensor, npha, status
      REAL lower(npha), hiher(npha), middl(npha), eff(npha)
      CHARACTER*(*) rsfil

c Routine to read the RMF file specified and return the energy ranges
c to be used. Also accumulates the total efficiency for each channel.

c Arguments :
c     rsu     i      i: I/O unit for RMF file
c     rsfil   c      i: Name of RMF file
c     npha    i      r: Actual size of spectrum arrays
c     lower   r      r: Lower energies
c     hiher   r      r: Upper energies
c     middl   r      r: Middle energies
c     eff     r      r: The efficiency array
c     status  i      r: Status  -   0 = OK

      REAL    rbuf, elow, ehigh, rsum, weight
      INTEGER rssize, i, j, k, rngrup, nchan, nenerg
      INTEGER egyloc, egyhic, rspind(6)
      INTEGER ipha, ielt
      CHARACTER contxt*72, comment*80
      character(8) rspnam(6)
      LOGICAL anyf

      INTEGER lenact
      EXTERNAL lenact

      DATA rspnam /'ENERG_LO', 'ENERG_HI', 'N_GRP', 'N_CHAN', 
     &             'F_CHAN', 'MATRIX' /

      status = 0

      CALL ftopen(rsu,rsfil,0,rssize,status)
      contxt = 'Error opening response file'
      IF ( status .NE. 0 ) GOTO 999


c need to find the extension containing the energy bounds used in the RMF.

      CALL fndext(rsu, 'RESPONSE', 'EBOUNDS', contxt, status)
      IF ( status .NE. 0 ) GOTO 999

      CALL ftgcno(rsu, .false., 'E_MIN', egyloc, status)
      contxt = 'Failed to read E_MIN keyword'
      IF ( status .NE. 0 ) GOTO 999
      CALL ftgcno(rsu, .false., 'E_MAX', egyhic, status)
      contxt = 'Failed to read E_MAX keyword'
      IF ( status .NE. 0 ) GOTO 999

      anyf = .FALSE.

      DO i = 1, npha

         CALL ftgcve(rsu,egyloc,i,1,1,0,lower(i),anyf,status)
         WRITE(contxt,'(a,i5)') 'Failed to read lower energy ', i
         IF ( status .NE. 0 ) GOTO 999
         CALL ftgcve(rsu,egyhic,i,1,1,0,hiher(i),anyf,status)
         WRITE(contxt,'(a,i5)') 'Failed to read upper energy ', i
         IF ( status .NE. 0 ) GOTO 999

         middl(i)=(lower(i)+hiher(i))/2.

      ENDDO

c If we are doing the SIS then accumulate the efficiency array

      IF ( sensor .LT. 2 ) THEN

c need to find the extension containing the matrix.

         CALL fndext(rsu, 'RESPONSE', 'RSP_MATRIX', contxt, status)
         contxt = 'Failed to find RSP_MATRIX extension'
         IF ( status .NE. 0 ) GOTO 999

c Get the number of energy bins

         CALL ftgkyj(rsu, 'NAXIS2', nenerg, comment, status)
         contxt = 'Failed to read NAXIS2 keyword in RSP_MATRIX'
         IF ( status .NE. 0 ) GOTO 999

c Get the columns with the information we need.

         DO i = 1, 6
            CALL ftgcno(rsu, .FALSE., rspnam(i), rspind(i), status)
            contxt = rspnam(i)(:lenact(rspnam(i)))//' column is missing'
            IF ( (status .NE. 0) .OR. (rspind(i) .EQ. 0) ) GOTO 999
         ENDDO

c initialize the efficiencies

         DO i = 1, npha
            eff(i) = 0.
         ENDDO

         ipha = 1
         weight = 0.

c loop over the energies accumulating the total efficiency

         DO i = 1, nenerg

c get the energy range for this energy bin

            CALL ftgcve(rsu, rspind(1), i, 1, 1, 0., elow, anyf, 
     &                  status)
            contxt = 'failed to read ENERG_LO'
            IF (status .NE. 0) GOTO 999
            CALL ftgcve(rsu, rspind(2), i, 1, 1, 0., ehigh, anyf, 
     &                  status)
            contxt = 'failed to read ENERG_HI'
            IF (status .NE. 0) GOTO 999

c get the number of response groups for this energy range

            CALL ftgcvj(rsu, rspind(3), i, 1, 1, 0, rngrup, anyf, 
     &                  status)
            contxt = 'failed to read number of response groups'
            IF (status .NE. 0) GOTO 999

c loop round the response groups and accumulate the total efficiency

            ielt = 0
            rsum = 0.
            DO j = 1, rngrup

c get the number of channels in this group

               CALL ftgcvj(rsu, rspind(4), i, j, 1, 0, nchan, anyf, 
     &                     status)
               contxt = 'failed to read N_CHAN value'
               IF (status .NE. 0) GOTO 999

c Now loop round the response elements accumulating the efficiency in the
c appropriate array elements

               DO k = 1, nchan

                  ielt = ielt + 1

                  CALL ftgcve(rsu, rspind(6), i, ielt, 1, 0., rbuf, 
     &                        anyf, status)
                  contxt = 'failed to read response value'
                  IF (status .NE. 0) GOTO 999

                  rsum = rsum + rbuf

               ENDDO

            ENDDO

c assign this efficiency to the correct pha bin assuming that the entire
c efficiency goes into the bin whose energy matches that of the input response
c energy (ie the matrix is effectively diagonalized).

            DO WHILE(hiher(ipha) .LT. elow .AND. ipha .LT. npha)
               ipha = ipha + 1
            ENDDO

            IF ( elow .LE. lower(ipha) .AND. 
     &           ehigh .GE. lower(ipha) )THEN
               IF ( ehigh .LE. hiher(ipha) ) THEN
                  eff(ipha) = eff(ipha) + 
     &                rsum * (ehigh-lower(ipha))/(ehigh-elow)
                  weight = weight + (ehigh-lower(ipha))/(ehigh-elow)
               ELSE
                  eff(ipha) = eff(ipha) + 
     &             rsum * (hiher(ipha)-lower(ipha))/(ehigh-elow)
                  weight = weight + (hiher(ipha)-lower(ipha))
     &                              /(ehigh-elow)
                  eff(ipha) = eff(ipha) / weight
                  weight = 0.
                  ipha = ipha + 1
                  eff(ipha) = eff(ipha) + 
     &             rsum * (ehigh-lower(ipha))/(ehigh-elow)
                  weight = weight + (ehigh-lower(ipha))/(ehigh-elow)
               ENDIF
            ELSEIF ( elow .GT. lower(ipha) .AND.
     &               elow .LE. hiher(ipha) ) THEN
               IF ( ehigh .LE. hiher(ipha) ) THEN
                  eff(ipha) = eff(ipha) + rsum
                  weight = weight + 1.
               ELSE
                  eff(ipha) = eff(ipha) + 
     &             rsum * (hiher(ipha)-elow)/(ehigh-elow)
                  weight = weight + (hiher(ipha)-elow)/(ehigh-elow)
                  eff(ipha) = eff(ipha) / weight
                  weight = 0.
                  ipha = ipha + 1
                  eff(ipha) = eff(ipha) + 
     &             rsum * (ehigh-lower(ipha))/(ehigh-elow)
                  weight = weight + (ehigh-lower(ipha))/(ehigh-elow)
               ENDIF
            ENDIF

         ENDDO

      ENDIF

* we don't need the resp file anymore so

      CALL ftclos(rsu, status)
      contxt = 'Error closing response file'
      IF ( status .NE. 0 ) GOTO 999

      RETURN

 999  CONTINUE
      CALL fcerr(contxt)
      CALL fcerrm(status)

      RETURN
      END
