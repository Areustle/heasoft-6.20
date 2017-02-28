
      SUBROUTINE inprmf(inrfil, n_energies, resp_energies, n_channels, 
     &                  ch_bounds, rsp_min, ngroup, max_tot_groups,
     &                  max_elements, ichanb, ichane, resp_matrix,
     &                  num_elements, num_tot_groups, ierr)

      IMPLICIT NONE

      INTEGER N_energies , N_channels
      INTEGER Max_elements , Max_tot_groups
 
      REAL Resp_energies(0:N_energies) , Resp_matrix(Max_elements)
      REAL Ch_bounds(N_channels,2)
      REAL Rsp_min
 
      INTEGER Ngroup(N_energies)
      INTEGER Ichanb(Max_tot_groups) , Ichane(Max_tot_groups)
      INTEGER Num_elements , Num_tot_groups , Ierr

      CHARACTER inrfil*(*)

c This routine actually reads in the response matrix.
c Arguments :
c     Inrfil           c     i: The name of the response matrix
c     N_energies       i     i: Number of response energy bins
c     Resp_energies    r     i: Response energy ranges
c     N_channels       i     i: Number of channels
c     Ch_bounds        r     i: Channel energy boundaries
c     Rsp_min          r     i: The minimum response value that will be stored
c     Ngroup           i     r: The number of response groups for each energy
c     Max_tot_groups   i     i: The maximum number of total response groups
c     Ichanb           i     r: Start channel for each response group
c     Ichane           i     r: End channel for each response group
c     Max_elements     i     i: Maximum number of response elements
c     Resp_matrix      r     r: Response matrix elements
c     Num_elements     i     r: Number of response matrix elements calculated
c     Num_tot_groups   i     r: Total number of response groups calculated
c     Ierr             i     r: Error status    0 = OK

      INTEGER ilun, block, i, j, col(6), nchan, nentr

      CHARACTER(72) comment, contxt

      LOGICAL qanyf

      INTEGER lenact
      EXTERNAL lenact

c Open the input response matrix

      CALL getlun(ilun)
      CALL ftopen(ilun, inrfil, 0, block, ierr)
      contxt = 'Failed to open '//inrfil(:lenact(inrfil))
      IF ( ierr .NE. 0 ) GOTO 999

c Go to the EBOUNDS extension

      CALL xfndxt(ilun, 'RESPONSE', 'EBOUNDS', comment, ierr)
      contxt = 'INPRMF: Unable to find EBOUNDS extension'
      IF (ierr .NE. 0) GOTO 999

c Get the columns for E_MIN and E_MAX

      CALL ftgcno(ilun, .FALSE., 'E_MIN', col(1), ierr)
      contxt = 'INPRMF: Cannot find E_MIN column'
      IF (ierr .NE. 0) GOTO 999

      CALL ftgcno(ilun, .FALSE., 'E_MAX', col(2), ierr)
      contxt = 'INPRMF: Cannot find E_MAX column'
      IF (ierr .NE. 0) GOTO 999

c Read the energy bounds

      DO i = 1, N_channels

         CALL FTGCVE(ilun, col(1), i, 1, 1, 0., ch_bounds(i,1), qanyf,
     &               ierr)
         contxt = 'INPRMF: Cannot read E_MIN data'
         IF (ierr .NE. 0) GOTO 999

         CALL FTGCVE(ilun, col(2), i, 1, 1, 0., ch_bounds(i,2), qanyf,
     &               ierr)
         contxt = 'INPRMF: Cannot read E_MAX data'
         IF (ierr .NE. 0) GOTO 999

      ENDDO

c Now go to RSP_MATRIX extension - close and reopen the file in case the
c extensions are out of order

      CALL FTCLOS(ilun, ierr)
      contxt = 'INPRMF: Failed to close file'
      IF (ierr .NE. 0) GOTO 999
      CALL ftopen(ilun, inrfil, 0, block, ierr)
      contxt = 'Failed to reopen '//inrfil(:lenact(inrfil))
      IF ( ierr .NE. 0 ) GOTO 999

      CALL xfndxt(ilun, 'RESPONSE', 'RSP_MATRIX', comment, ierr)
      contxt = 'INPRMF: Unable to find RSP_MATRIX extension'
      IF (ierr .NE. 0) GOTO 999

c Get the LO_THRES keyword

      CALL ftgkye(ilun, 'LO_THRES', rsp_min, comment, ierr)
      contxt = 'INPRMF: Cannot read LO_THRES'
      IF (ierr .NE. 0) GOTO 999

c Get the columns for the input vectors

      CALL ftgcno(ilun, .FALSE., 'ENERG_LO', col(1), ierr)
      contxt = 'INPRMF: Cannot find ENERG_LO column'
      IF (ierr .NE. 0) GOTO 999

      CALL ftgcno(ilun, .FALSE., 'ENERG_HI', col(2), ierr)
      contxt = 'INPRMF: Cannot find ENERG_HI column'
      IF (ierr .NE. 0) GOTO 999

      CALL ftgcno(ilun, .FALSE., 'N_GRP', col(3), ierr)
      contxt = 'INPRMF: Cannot find N_GRP column'
      IF (ierr .NE. 0) GOTO 999

      CALL ftgcno(ilun, .FALSE., 'F_CHAN', col(4), ierr)
      contxt = 'INPRMF: Cannot find F_CHAN column'
      IF (ierr .NE. 0) GOTO 999

      CALL ftgcno(ilun, .FALSE., 'N_CHAN', col(5), ierr)
      contxt = 'INPRMF: Cannot find N_CHAN column'
      IF (ierr .NE. 0) GOTO 999

      CALL ftgcno(ilun, .FALSE., 'MATRIX', col(6), ierr)
      contxt = 'INPRMF: Cannot find MATRIX column'
      IF (ierr .NE. 0) GOTO 999

c Get the first response energy

      CALL ftgcve(ilun, col(1), 1, 1, 1, 0., resp_energies(0), qanyf, 
     &            ierr)
      contxt = 'INPRMF: Cannot read ENERG_LO data'
      IF (ierr .NE. 0) GOTO 999

c Loop round energies loading the response matrix

      Num_elements = 0
      Num_tot_groups = 0

      DO i = 1, N_energies

c Read the response energy

         CALL ftgcve(ilun, col(2), i, 1, 1, 0., resp_energies(i), 
     &               qanyf, ierr)
         contxt = 'INPRMF: Cannot read ENERG_HI data'
         IF (ierr .NE. 0) GOTO 999

c Read the number of groups

         CALL ftgcvj(ilun, col(3), i, 1, 1, 0, Ngroup(i), qanyf, ierr)
         contxt = 'INPRMF: Cannot read N_GRP data'
         IF (ierr .NE. 0) GOTO 999

c Loop round the groups for this energy - nentr accumulates how many
c response entries there are for this energy

         nentr = 0
         DO j = 1, Ngroup(i)

c Read the start channel and number of channels for this group

            Num_tot_groups = Num_tot_groups + 1

            CALL ftgcvj(ilun, col(4), i, j, 1, 0, 
     &                  ichanb(Num_tot_groups), qanyf, ierr)
            contxt = 'INPRMF: Cannot read F_CHAN data'
            IF (ierr .NE. 0) GOTO 999

            CALL ftgcvj(ilun, col(5), i, j, 1, 0, nchan, qanyf, ierr)
            contxt = 'INPRMF: Cannot read N_CHAN data'
            IF (ierr .NE. 0) GOTO 999

            nentr = nentr + nchan
            ichane(Num_tot_groups) = ichanb(Num_tot_groups) + nchan-1

         ENDDO

c Read the matrix data

         CALL ftgcve(ilun, col(6), i, 1, nentr, 0.,
     &               resp_matrix(Num_elements+1), qanyf, ierr)
         contxt = 'INPRMF: Cannot read MATRIX data'
         IF (ierr .NE. 0) GOTO 999

         Num_elements = Num_elements + nentr

      ENDDO

c Close the response matrix file

      CALL ftclos(ilun, ierr)
      contxt = 'INPRMF: Failed to close file'
      IF (ierr .NE. 0) GOTO 999
      CALL frelun(ilun)

      WRITE(contxt, '(a,i7,a)') '...', Num_tot_groups, 
     &        ' groups and '
      CALL xwrite(contxt, 10)
      WRITE(contxt, '(a,i7,a,a)') '...', Num_elements, 
     &        ' response values from ', inrfil(:LENACT(inrfil))
      CALL xwrite(contxt, 10)

 999  CONTINUE
      IF ( Ierr.NE.0 ) THEN
         CALL xwrite(contxt,10)
         WRITE(contxt,'(a,i4)') ' error = ', ierr
         CALL xwrite(contxt,10)
      ENDIF

      RETURN
      END
