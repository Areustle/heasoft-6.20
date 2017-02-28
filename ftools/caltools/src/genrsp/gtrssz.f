
      SUBROUTINE gtrssz(inrfil, max_elements, n_energies, n_channels,
     &                  max_tot_groups, ierr)

      IMPLICIT NONE

      INTEGER max_elements, n_energies, n_channels, max_tot_groups
      INTEGER Ierr

      CHARACTER*(*) inrfil

c Read the input RMF file to find out the number of energies, channels
c etc so that we can grab the appropriate amount of memory.

c Arguments :
c     inrfil          c*(*)       i: input RMF filename
c     max_elements    i           r: number of non-zero response elements
c     n_energies      i           r: number of energy bins
c     n_channels      i           r: number of channels
c     max_tot_groups  i           r: number of response groups
c     ierr            i           r: error flag   0 == OK

      INTEGER ilun, block, i, j, ngrcol, nchcol, ngroup, nchan

      CHARACTER(72) comment, contxt

      LOGICAL qanyf

      INTEGER lenact
      EXTERNAL lenact

c Open the input RMF file

      CALL getlun(ilun)
      CALL ftopen(ilun, inrfil, 0, block, ierr)
      contxt = 'Failed to open '//inrfil(:lenact(inrfil))
      IF ( ierr .NE. 0 ) GOTO 999

c Go to the response matrix extension

      CALL xfndxt(ilun, 'RESPONSE', 'RSP_MATRIX', comment, ierr)
      contxt = 'GTRSSZ: Unable to find RSP_MATRIX extension'
      IF (ierr .NE. 0) GOTO 999

c To get the number of channels read the DETCHANS keyword

      CALL ftgkyj(ilun, 'DETCHANS', n_channels, comment, ierr)
      contxt = 'GTRSSZ: Cannot read DETCHANS from RSP_MATRIX extension'
      IF (ierr .NE. 0) GOTO 999

c To get the number of energies read the NAXIS2 keyword

      CALL ftgkyj(ilun, 'NAXIS2', n_energies, comment, ierr)
      contxt = 'GTRSSZ: Cannot read NAXIS2 from RSP_MATRIX extension'
      IF (ierr .NE. 0) GOTO 999

c Find the columns for N_GRP and N_CHAN

      CALL ftgcno(ilun, .FALSE., 'N_GRP', ngrcol, ierr)
      contxt = 'GTRSSZ: Cannot find N_GRP column'
      IF (ierr .NE. 0) GOTO 999

      CALL ftgcno(ilun, .FALSE., 'N_CHAN', nchcol, ierr)
      contxt = 'GTRSSZ: Cannot find N_CHAN column'
      IF (ierr .NE. 0) GOTO 999

c Loop round the energies to find the total number of groups and response
c matrix elements required.

      max_elements = 0
      max_tot_groups = 0

      DO i = 1, n_energies

c Read the number of groups

         CALL ftgcvj(ilun, ngrcol, i, 1, 1, 0, ngroup, qanyf, ierr)
         contxt = 'GTRSSZ: Cannot read N_GRP data'
         IF (ierr .NE. 0) GOTO 999

         max_tot_groups = max_tot_groups + ngroup

c and the number of channels in each group

         DO j = 1, ngroup

            CALL ftgcvj(ilun, nchcol, i, j, 1, 0, nchan, qanyf, ierr)
            contxt = 'GTRSSZ: Cannot read N_CHAN data'
            IF (ierr .NE. 0) GOTO 999

            max_elements = max_elements + nchan

         ENDDO

      ENDDO

      CALL ftclos(ilun, ierr)
      contxt = 'GTRSSZ: Failed to close file'
      IF (ierr .NE. 0) GOTO 999
      CALL frelun(ilun)

 
 999  CONTINUE
      IF ( Ierr.NE.0 ) THEN
         CALL xwrite(contxt,10)
         WRITE(contxt,'(a,i4)') ' error = ', ierr
         CALL xwrite(contxt,10)
      ENDIF

      RETURN
      END
