**==rddata.spg  processed by SPAG 4.50J  at 17:34 on 20 Jan 1996
 
      SUBROUTINE RDINPD(N_a_energies, Efffil, Eff_area, N_d_energies, 
     &                  Detfil, Det_eff, N_f_energies, Filfil, 
     &                  Filt_eff, N_r_energies, N_lines, Resol_file, 
     &                  LineEn, LineDt, N_e_energies, Resp_file, 
     &                  Resp_data, N_c_energies, Chan_file, Chan_data, 
     &                  Ierr)

      IMPLICIT NONE

      INTEGER N_a_energies, N_d_energies, N_f_energies, N_r_energies
      INTEGER N_e_energies, N_c_energies, N_lines
 
      REAL Eff_area(2,N_a_energies), Det_eff(2,N_d_energies)
      REAL Filt_eff(2,N_f_energies), LineEn(N_r_energies)
      REAL LineDt(3, N_lines, N_r_energies)
      REAL Resp_data(2,N_e_energies), Chan_data(2,N_c_energies)

      CHARACTER*(*) efffil, detfil, filfil, Resol_file, Resp_file
      CHARACTER*(*) Chan_file

c Routine to read in the input data
c     N_a_energies       i          i: Number of effective area points
c     Efffil             c*(*)      i: Filename for effective areas
c     Eff_area           r          r: Energies and effective areas
c     N_d_energies       i          i: Number of detector efficiency points
c     Detfil             c*(*)      i: Filename for detector efficiencies
c     Det_eff            r          r: Energies and detector efficiencies
c     N_f_energies       i          i: Number of filter transmission points
c     Filfil             c*(*)      i: Filename for filter transmissions
c     Filt_eff           r          r: Energies and filter transmissions
c     N_r_energies       i          i: Number of resolution points
c     N_lines            i          i: Number of lines
c     Resol_file         c*(*)      i: Filename for resolutions
c     LineEn             r          r: Energies for tabulated response data
c     LineDt             r          r: centroids, resolutions, norms
c     N_e_energies       i          i: Number of response energy bins
c     Resp_file          c*(*)      i: Filename for response energy definition
c     Resp_data          r          r: Response energy definitions
c     N_c_energies       i          i: Number of response energy bins
c     Chan_file          c*(*)      i: Filename for channel definition
c     Chan_data          r          r: Channel definitions
c     Ierr               i          r: Error flag     0 == OK

      INTEGER Ierr
 
      INTEGER lun, i, j
 
      character(72) contxt
 
      INTEGER LENACT
      EXTERNAL LENACT
 
c  read in effective areas
 
      IF ( N_a_energies .GT. 0 ) THEN 
         CALL GETLUN(lun)
         OPEN (UNIT=lun,FILE=efffil,STATUS='old',IOSTAT=Ierr)
         contxt = 'Failed to open '//efffil(:LENACT(efffil))
         IF ( Ierr.NE.0 ) GOTO 999

         contxt = 'Failed to read '//efffil(:LENACT(efffil))
         DO i = 1 , N_a_energies
            READ (lun,*,IOSTAT=Ierr) (Eff_area(j,i),j=1,2)
            IF ( Ierr.NE.0 ) GOTO 999
         ENDDO
         CLOSE (lun)
         CALL FRELUN(lun)
         WRITE(contxt, '(a,i7,a)') '...', N_a_energies, 
     &           ' entries read from'
         contxt = contxt(:LENACT(contxt)) // ' ' // 
     &            efffil(:LENACT(efffil))
         CALL xwrite(contxt, 10)
      ENDIF
 
c  Read in detector efficiencies

      IF ( N_d_energies .GT. 0 ) THEN 
         CALL GETLUN(lun)
         OPEN (UNIT=lun,FILE=detfil,STATUS='old',IOSTAT=Ierr)
         contxt = 'Failed to open '//detfil(:LENACT(detfil))
         IF ( Ierr.NE.0 ) GOTO 999
 
         contxt = 'Failed to read '//detfil(:LENACT(detfil))
         DO i = 1 , N_d_energies
            READ (lun,*,IOSTAT=Ierr) (Det_eff(j,i),j=1,2)
            IF ( Ierr.NE.0 ) GOTO 999
         ENDDO
         CLOSE (lun)
         CALL FRELUN(lun)
         WRITE(contxt, '(a,i7,a)') '...', N_d_energies, 
     &           ' entries read from'
         contxt = contxt(:LENACT(contxt)) // ' ' //
     &            detfil(:LENACT(detfil))
         CALL xwrite(contxt, 10)
      ENDIF
 
c Read in filter transmissions
 
      IF ( N_f_energies .GT. 0 ) THEN
         CALL GETLUN(lun)
         OPEN (UNIT=lun,FILE=filfil,STATUS='old',IOSTAT=Ierr)
         contxt = 'Failed to open '//filfil(:LENACT(filfil))
         IF ( Ierr.NE.0 ) GOTO 999
 
         contxt = 'Failed to read '//filfil(:LENACT(filfil))
         DO i = 1 , N_f_energies
            READ (lun,*,IOSTAT=Ierr) (Filt_eff(j,i),j=1,2)
            IF ( Ierr.NE.0 ) GOTO 999
         ENDDO
         CLOSE (lun)
         CALL FRELUN(lun)
         WRITE(contxt, '(a,i7,a)') '...', N_f_energies, 
     &           ' entries read from'
         contxt = contxt(:LENACT(contxt)) // ' ' //
     &            filfil(:LENACT(filfil))
         CALL xwrite(contxt, 10)
      ENDIF

c Read in resolutions. 
 
      IF ( N_r_energies .GT. 0 ) THEN
         CALL GETLUN(lun)
         OPEN (UNIT=lun,FILE=Resol_file,STATUS='old',IOSTAT=Ierr)
         contxt = 'Failed to open '//Resol_file(:LENACT(Resol_file))
         IF ( Ierr.NE.0 ) GOTO 999
 
c Jump over first line

         READ (lun,*,IOSTAT=Ierr)
         contxt = 'Failed to read '//Resol_file(:LENACT(Resol_file))
         IF ( Ierr.NE.0 ) GOTO 999

c If there is only the photopeak then don't read a normalization

         IF ( N_lines .EQ. 1 ) THEN
            DO i = 1 , N_r_energies
               READ (lun,*,IOSTAT=Ierr) LineEn(i), LineDt(1,1,i), 
     &                                  LineDt(2,1,i)
               IF ( Ierr.NE.0 ) GOTO 999
               LineDt(3,1,i) = 1.0
            ENDDO
         ELSE
            DO i = 1 , N_r_energies
               READ (lun,*,IOSTAT=Ierr) LineEn(i), (LineDt(1,j,i), 
     &             LineDt(2,j,i), LineDt(3,j,i), j=1,N_lines)
               IF ( Ierr.NE.0 ) GOTO 999
            ENDDO
         ENDIF
         CLOSE (lun)
         CALL FRELUN(lun)
         WRITE(contxt, '(a,i7,a)') '...', N_r_energies, 
     &           ' entries read from'
         contxt = contxt(:LENACT(contxt)) // ' ' //
     &            Resol_file(:LENACT(Resol_file))
         CALL xwrite(contxt, 10)
      ENDIF

c Read in response energy bin definition
 
      IF ( N_e_energies .GT. 0 ) THEN
         CALL GETLUN(lun)
         OPEN (UNIT=lun,FILE=Resp_file,STATUS='old',IOSTAT=Ierr)
         contxt = 'Failed to open '//Resp_file(:LENACT(Resp_file))
         IF ( Ierr.NE.0 ) GOTO 999
 
         contxt = 'Failed to read '//Resp_file(:LENACT(Resp_file))
         DO i = 1 , N_e_energies
            READ (lun,*,IOSTAT=Ierr) (Resp_data(j,i),j=1,2)
            IF ( Ierr.NE.0 ) GOTO 999
         ENDDO
         CLOSE (lun)
         CALL FRELUN(lun)
         WRITE(contxt, '(a,i7,a)') '...', N_e_energies, 
     &           ' entries read from'
         contxt = contxt(:LENACT(contxt)) // ' ' //
     &            Resp_file(:LENACT(Resp_file))
         CALL xwrite(contxt, 10)
      ENDIF

c Read in channel definition
 
      IF ( N_c_energies .GT. 0 ) THEN
         CALL GETLUN(lun)
         OPEN (UNIT=lun,FILE=Chan_file,STATUS='old',IOSTAT=Ierr)
         contxt = 'Failed to open '//Chan_file(:LENACT(Chan_file))
         IF ( Ierr.NE.0 ) GOTO 999
 
         contxt = 'Failed to read '//Chan_file(:LENACT(Chan_file))
         DO i = 1 , N_c_energies
            READ (lun,*,IOSTAT=Ierr) (Chan_data(j,i),j=1,2)
            IF ( Ierr.NE.0 ) GOTO 999
         ENDDO
         CLOSE (lun)
         CALL FRELUN(lun)
         WRITE(contxt, '(a,i7,a)') '...', N_c_energies, 
     &           ' entries read from'
         contxt = contxt(:LENACT(contxt)) // ' ' //
     &            Chan_file(:LENACT(Chan_file))
         CALL xwrite(contxt, 10)
      ENDIF

 999  CONTINUE
      IF ( Ierr.NE.0 ) THEN
         CALL xwrite(contxt,10)
         WRITE(contxt,'(a,i4)') ' error = ', ierr
         CALL xwrite(contxt,10)
      ENDIF
 
      RETURN
      END
 



