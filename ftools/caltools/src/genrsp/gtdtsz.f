
      SUBROUTINE gtdtsz(efffil, detfil, filfil, resol_file, resp_file, 
     &                  chan_file, n_a_energies, n_d_energies, 
     &                  n_f_energies, n_r_energies, n_e_energies, 
     &                  n_c_energies, n_lines, Ierr)

      IMPLICIT NONE

      INTEGER n_a_energies, n_d_energies, n_f_energies, n_r_energies
      INTEGER n_e_energies, n_c_energies, n_lines, Ierr

      CHARACTER*(*) efffil, detfil, filfil, resol_file, resp_file
      CHARACTER*(*) chan_file

c Read the input data files to find out the number of energies in each
c file so that we can grab the appropriate amount of memory.

c Arguments :
c     efffil        c*(*)       i: filename with effective areas
c     detfil        c*(*)       i: filename with detector efficiencies
c     filfil        c*(*)       i: filename with filter transmissions
c     resol_file    c*(*)       i: filename with resolutions (or none)
c     resp_file     c*(*)       i: filename with response energy definition (or none)
c     chan_file     c*(*)       i: filename with channel definition (or none)
c     n_a_energies  i           r: number of entries in efffil
c     n_d_energies  i           r: number of entries in detfil
c     n_f_energies  i           r: number of entries in filfil
c     n_r_energies  i           r: number of entries in resol_file
c     n_e_energies  i           r: number of entries in resp_file
c     n_c_energies  i           r: number of entries in chan_file
c     n_lines       i           r: number of lines (1 if only photopeak)
c     ierr          i           r: error flag   0 == OK


      REAL junk(2)

      INTEGER lun, j
 
      character(72) contxt
 
      INTEGER LENACT
      EXTERNAL LENACT

      N_a_energies = 0
      N_d_energies = 0 
      N_f_energies = 0
      N_r_energies = 0
      N_e_energies = 0
      N_c_energies = 0
      
c  read effective area file to find out how many energies are available.

      IF ( efffil(1:4) .NE. 'none' ) THEN
         IF ( LENACT(efffil) .EQ. 0 ) THEN
            CALL xwrite('No effective area filename', 10)
         ENDIF
         CALL GETLUN(lun)
         OPEN (UNIT=lun,FILE=efffil,STATUS='old',IOSTAT=Ierr)
         contxt = 'Failed to open effective area file '
     &            //efffil(:LENACT(efffil))
         IF ( Ierr.NE.0 ) GOTO 999

         contxt = 'Failed to read '//efffil(:LENACT(efffil))
         READ (lun,*,IOSTAT=Ierr) (junk(j),j=1,2)
         IF ( Ierr .NE. 0 ) GOTO 999

         DO WHILE (Ierr .EQ. 0)
            N_a_energies = N_a_energies + 1
            READ (lun,*,IOSTAT=Ierr) (junk(j),j=1,2)
         ENDDO
         CLOSE (lun)
         CALL FRELUN(lun)
         Ierr = 0
      ENDIF
 
c  Read in detector efficiencies

      IF ( detfil(1:4) .NE. 'none' ) THEN 
         IF ( LENACT(detfil) .EQ. 0 ) THEN
            CALL xwrite('No detector efficiency filename', 10)
         ENDIF
         CALL GETLUN(lun)
         OPEN (UNIT=lun,FILE=detfil,STATUS='old',IOSTAT=Ierr)
         contxt = 'Failed to open detector efficiency file '
     &               //detfil(:LENACT(detfil))
         IF ( Ierr.NE.0 ) GOTO 999

         contxt = 'Failed to read '//detfil(:LENACT(detfil))
         READ (lun,*,IOSTAT=Ierr) (junk(j),j=1,2)
         IF ( Ierr .NE. 0 ) GOTO 999

         DO WHILE (Ierr .EQ. 0)
            N_d_energies = N_d_energies + 1
            READ (lun,*,IOSTAT=Ierr) (junk(j),j=1,2)
         ENDDO
         CLOSE (lun)
         CALL FRELUN(lun)
         Ierr = 0
      ENDIF
 
c Read in filter transmissions
 
      IF ( filfil(1:4) .NE. 'none' ) THEN
         IF ( LENACT(filfil) .EQ. 0 ) THEN
            CALL xwrite('No filter transmission filename', 10)
         ENDIF
         CALL GETLUN(lun)
         OPEN (UNIT=lun,FILE=filfil,STATUS='old',IOSTAT=Ierr)
         contxt = 'Failed to open filter transmission file '
     &                //filfil(:LENACT(filfil))
         IF ( Ierr.NE.0 ) GOTO 999

         contxt = 'Failed to read '//filfil(:LENACT(filfil))
         READ (lun,*,IOSTAT=Ierr) (junk(j),j=1,2)
         IF ( Ierr .NE. 0 ) GOTO 999

         DO WHILE (Ierr .EQ. 0)
            N_f_energies = N_f_energies + 1
            READ (lun,*,IOSTAT=Ierr) (junk(j),j=1,2)
         ENDDO
         CLOSE (lun)
         CALL FRELUN(lun)
         Ierr = 0
      ENDIF

c Read in resolutions. If there is no file then set n_lines to 1.
 
      IF ( resol_file(1:4) .EQ. 'none' ) THEN
         N_lines = 1
      ELSE
         IF ( LENACT(resol_file) .EQ. 0 ) THEN
            CALL xwrite('No resolution filename', 10)
         ENDIF
         CALL GETLUN(lun)
         OPEN (UNIT=lun,FILE=resol_file,STATUS='old',IOSTAT=Ierr)
         contxt = 'Failed to open resolution file '
     &                //resol_file(:LENACT(resol_file))
         IF ( Ierr.NE.0 ) GOTO 999

c Read the number of lines and number of response energies from the file

         contxt = 'Failed to read '//resol_file(:LENACT(resol_file))
         READ (lun,*,IOSTAT=Ierr) N_lines, N_r_energies
         IF ( Ierr .NE. 0 ) GOTO 999

         CLOSE (lun)
         CALL FRELUN(lun)
         Ierr = 0
      ENDIF

c Get size of response energy file
 
      IF ( resp_file(1:4) .NE. 'none' ) THEN
         IF ( LENACT(resp_file) .EQ. 0 ) THEN
            CALL xwrite('No response energy relation filename', 10)
         ENDIF
         CALL GETLUN(lun)
         OPEN (UNIT=lun,FILE=resp_file,STATUS='old',IOSTAT=Ierr)
         contxt = 'Failed to open response energy relation file '
     &                //resp_file(:LENACT(resp_file))
         IF ( Ierr.NE.0 ) GOTO 999

         contxt = 'Failed to read '//resp_file(:LENACT(resp_file))
         READ (lun,*,IOSTAT=Ierr) (junk(j),j=1,2)
         IF ( Ierr .NE. 0 ) GOTO 999

         DO WHILE (Ierr .EQ. 0)
            N_e_energies = N_e_energies + 1
            READ (lun,*,IOSTAT=Ierr) (junk(j),j=1,2)
         ENDDO
         CLOSE (lun)
         CALL FRELUN(lun)
         Ierr = 0
      ENDIF

c Get size of channel definition file
 
      IF ( chan_file(1:4) .NE. 'none' ) THEN
         IF ( LENACT(chan_file) .EQ. 0 ) THEN
            CALL xwrite('No channel filename', 10)
         ENDIF
         CALL GETLUN(lun)
         OPEN (UNIT=lun,FILE=chan_file,STATUS='old',IOSTAT=Ierr)
         contxt = 'Failed to open channel file '
     &                //chan_file(:LENACT(chan_file))
         IF ( Ierr.NE.0 ) GOTO 999

         contxt = 'Failed to read '//chan_file(:LENACT(chan_file))
         READ (lun,*,IOSTAT=Ierr) (junk(j),j=1,2)
         IF ( Ierr .NE. 0 ) GOTO 999

         DO WHILE (Ierr .EQ. 0)
            N_c_energies = N_c_energies + 1
            READ (lun,*,IOSTAT=Ierr) (junk(j),j=1,2)
         ENDDO
         CLOSE (lun)
         CALL FRELUN(lun)
         Ierr = 0
      ENDIF

 999  CONTINUE
      IF ( Ierr.NE.0 ) THEN
         CALL xwrite(contxt,10)
         WRITE(contxt,'(a,i4)') ' error = ', ierr
         CALL xwrite(contxt,10)
      ENDIF

      RETURN
      END
