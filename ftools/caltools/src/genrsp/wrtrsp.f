 
      SUBROUTINE WRTRSP(Max_elements, N_energies, N_channels,
     &                  Max_tot_groups, Matrix, Energies, Ngroup, 
     &                  Ichanb, Ichane, Ch_bounds, Rsp_file, Tlscpe,
     &                  Instrm, Rsp_min, Clobber, Status)

      IMPLICIT NONE

      INTEGER Max_elements, Max_tot_groups, N_channels, N_energies
 
      REAL Matrix(Max_elements)
      REAL Energies(0:N_energies)
      REAL Ch_bounds(N_channels,2)
      REAL Rsp_min
 
      INTEGER Ngroup(N_energies)
      INTEGER Ichanb(Max_tot_groups)
      INTEGER Ichane(Max_tot_groups)
      INTEGER Status
 
      CHARACTER*(*) Rsp_file, Tlscpe, Instrm

      LOGICAL Clobber
 
c  routine to write a FITS format response file. response matrix is passed as
c  XSPEC compressed format
c  Arguments :
c      Max_elements     i     i: Maximum number of response elements
c      N_energies       i     i: Number of response energy bins
c      N_channels       i     i: Number of channels
c      Max_tot_groups   i     i: The maximum number of total response groups
c      Matrix           r     i: Response matrix elements
c      Energies         r     i: Response energy ranges
c      Ngroup           i     i: The number of response groups for each energy
c      Ichanb           i     i: Start channel for each response group
c      Ichane           i     i: End channel for each response group
c      Ch_bounds        r     i: Channel energy boundaries
c      Rsp_file         c*(*) i: Name of output file
c      Tlscpe           c*(*) i: Telescope name
c      Instrm           c*(*) i: Instrument name
c      Clobber          l     i: If true then overwrite an existing file
c      Ierr             i     r: Status   0 == OK

c Local variables
 
      INTEGER ounit , i , nfields , ngrp
      INTEGER nmat , igrp , iresp , j , sum
      CHARACTER(72) contxt
      character(20) ttype(6) , tunits(6)
      character(5) tform(6)

      INTEGER lenact
      EXTERNAL lenact
 
c ------------------------------------------------------------
c Open the output FITS file.
c ------------------------------------------------------------

      CALL GETLUN(ounit)
 100  CONTINUE
      CALL FTINIT(ounit, Rsp_file, 1, Status)
      IF ( Status.EQ.105 .AND. Clobber ) THEN
         Status = 0
         contxt = 'Failed to delete pre-existing file'
         CALL xdelfil(Rsp_file, Status)
         IF ( Status .NE. 0 ) GOTO 999
         GOTO 100
      ELSEIF ( Status.EQ.105 .AND. .NOT.Clobber ) THEN
         contxt = ' Output file already exists'
         GOTO 999
      ELSEIF ( Status .NE. 0 ) THEN
         contxt = ' Failed to open FITS file'
         GOTO 999
      ENDIF

c ------------------------------------------------------------
c Write the primary header
c ------------------------------------------------------------

      contxt = 'Failed to define primary header' 
      CALL FTPDEF(ounit,8,0,0,0,1,Status)
      IF ( Status .NE. 0 ) GOTO 999
 
c Write the basic primary array keywords
 
      contxt = 'Failed to write array keywords' 
      CALL FTPHPR(ounit,.TRUE.,8,0,0,0,1,.TRUE.,Status)
      IF ( Status .NE. 0 ) GOTO 999
 
c Write out the additional keywords about the creation of the
c FITS file.
 
      contxt = 'Failed to write CONTENT keyword' 
      CALL FTPKYS(ounit,'CONTENT','RESPONSE','spectral response matrix',
     &            Status)
      IF ( Status .NE. 0 ) GOTO 999
      contxt = 'Failed to write ORIGIN keyword' 
      CALL FTPKYS(ounit,'ORIGIN','NASA/GSFC','origin of FITS file',
     &            Status)
      IF ( Status .NE. 0 ) GOTO 999
 
c Write the TELESCOP and INSTRUME keywords.
 
      contxt = 'Failed to write TELESCOP keyword' 
      CALL FTPKYS(ounit,'TELESCOP',Tlscpe,'Telescope (mission) name',
     &            Status)
      IF ( Status .NE. 0 ) GOTO 999
      contxt = 'Failed to write INSTRUME keyword' 
      CALL FTPKYS(ounit,'INSTRUME',Instrm,'Instrument name',Status)
      IF ( Status .NE. 0 ) GOTO 999
 
c ------------------------------------------------------------
c Write the energy bounds extensions
c ------------------------------------------------------------

      CALL wtebd3(ounit, 10, 0, ' ', 0, ' ', '1.3.0', Tlscpe, Instrm,
     &            ' ', ' ', 1., 'PHA', 1, N_channels, Ch_bounds(1,1),
     &            Ch_bounds(1,2), status)
      contxt = 'Failure while writing the EBOUNDS extension'
      IF ( Status .NE. 0 ) GOTO 999
 
c ------------------------------------------------------------
c Write the response matrix extensions
c ------------------------------------------------------------
 
c Create the extension
 
      contxt = 'Failed to create RSP_MATRIX extension' 
      CALL FTCRHD(ounit,Status)
      IF ( Status .NE. 0 ) GOTO 999

c Calculate the maximum number of groups and significant channels at any energy
 
      ngrp = 0
      nmat = 0
      igrp = 0
      DO i = 1 , N_energies
         sum = 0
         DO j = 1 , Ngroup(i)
            sum = sum + Ichane(j+igrp) - Ichanb(j+igrp) + 1
         ENDDO
         ngrp = MAX(ngrp,Ngroup(i))
         nmat = MAX(nmat,sum)
         igrp = igrp + Ngroup(i)
      ENDDO
 
c Set up the columns
 
      nfields = 6
      ttype(1) = 'ENERG_LO'
      tform(1) = 'E'
      tunits(1) = 'keV'
      ttype(2) = 'ENERG_HI'
      tform(2) = 'E'
      tunits(2) = 'keV'
      ttype(3) = 'N_GRP'
      tform(3) = 'J'
      tunits(3) = ' '
      ttype(4) = 'F_CHAN'
      IF ( ngrp .LT. 10 ) THEN
         WRITE (tform(4),'(i1,a)') ngrp , 'J'
      ELSEIF ( ngrp .LT. 100 ) THEN
         WRITE (tform(4),'(i2,a)') ngrp , 'J'
      ELSEIF ( ngrp .LT. 1000 ) THEN
         WRITE (tform(4),'(i3,a)') ngrp , 'J'
      ELSEIF ( ngrp .LT. 10000 ) THEN
         WRITE (tform(4),'(i4,a)') ngrp , 'J'
      ENDIF
      tunits(4) = ' '
      ttype(5) = 'N_CHAN'
      tform(5) = tform(4)
      tunits(5) = ' '
      ttype(6) = 'MATRIX'
      IF ( nmat .LT. 10 ) THEN
         WRITE (tform(6),'(i1,a)') nmat , 'E'
      ELSEIF ( nmat .LT. 100 ) THEN
         WRITE (tform(6),'(i2,a)') nmat , 'E'
      ELSEIF ( nmat .LT. 1000 ) THEN
         WRITE (tform(6),'(i3,a)') nmat , 'E'
      ELSEIF ( nmat .LT. 10000 ) THEN
         WRITE (tform(6),'(i4,a)') nmat , 'E'
      ENDIF
      tunits(6) = ' '
 
c Write the required header keywords
 
      contxt = 
     & 'Failed to write header keywords for the RSP_MATRIX extension' 
      CALL FTPBNH(ounit,N_energies,nfields,ttype,tform,tunits,
     &            'SPECRESP MATRIX',0,Status)
      IF ( Status .NE. 0 ) GOTO 999
 
 
c Write the CLAS and VERS keywords

      contxt = 'Failed to write HDUCLASS keyword' 
      CALL FTPKYS(ounit,'HDUCLASS ','OGIP',
     &            'format conforms to OGIP standard',Status)
      IF ( Status .NE. 0 ) GOTO 999
      contxt = 'Failed to write HDUCLAS1 keyword' 
      CALL FTPKYS(ounit,'HDUCLAS1 ','RESPONSE',
     &            'dataset relates to spectral response',Status)
      IF ( Status .NE. 0 ) GOTO 999
      contxt = 'Failed to write HDUVERS1 keyword' 
      CALL FTPKYS(ounit,'HDUVERS1 ','1.0.0',
     &            'version of family of formats',Status)
      IF ( Status .NE. 0 ) GOTO 999
      contxt = 'Failed to write HDUCLAS2 keyword' 
      CALL FTPKYS(ounit,'HDUCLAS2 ','RSP_MATRIX',
     &            'datasets is a spectral response matrix',Status)
      IF ( Status .NE. 0 ) GOTO 999
      contxt = 'Failed to write HDUVERS2 keyword' 
      CALL FTPKYS(ounit,'HDUVERS2 ','1.2.0',
     &     'Version of format (OGIP memo CAL/GEN/92-002a)',Status)
      IF ( Status .NE. 0 ) GOTO 999
      contxt = 'Failed to write HDUCLAS3 keyword' 
      CALL FTPKYS(ounit,'HDUCLAS3 ','FULL','includes all efficiencies',
     &            Status)
      IF ( Status .NE. 0 ) GOTO 999
 
c The other required keywords
 
      contxt = 'Failed to write TELESCOP keyword' 
      CALL FTPKYS(ounit,'TELESCOP',Tlscpe,'mission/satellite',Status)
      IF ( Status .NE. 0 ) GOTO 999
      contxt = 'Failed to write INSTRUME keyword' 
      CALL FTPKYS(ounit,'INSTRUME',Instrm,'mission/satellite',Status)
      IF ( Status .NE. 0 ) GOTO 999
      contxt = 'Failed to write DETNAM keyword' 
      CALL FTPKYS(ounit,'DETNAM',' ','detector in use',Status)
      IF ( Status .NE. 0 ) GOTO 999
      contxt = 'Failed to write FILTER keyword' 
      CALL FTPKYS(ounit,'FILTER',Instrm,'filter in use',Status)
      IF ( Status .NE. 0 ) GOTO 999
 
      contxt = 'Failed to write DETCHANS keyword' 
      CALL FTPKYJ(ounit,'DETCHANS',N_channels,
     &            'total number of detector channels',Status)
      IF ( Status .NE. 0 ) GOTO 999
      contxt = 'Failed to write CHANTYPE keyword' 
      CALL FTPKYS(ounit,'CHANTYPE','PHA',
     &            'detector channel type (PHA or PI)',Status)
      IF ( Status .NE. 0 ) GOTO 999

c Write both LO_THRESH and LO_THRES. The latter is better FITS but both are included
c for consistency with older s/w.

      contxt = 'Failed to write LO_THRESH keyword' 
      CALL FTPKYF(ounit,'LO_THRESH',Rsp_min,6,
     &            'lower threshold for stored matrix',Status)
      IF ( Status .NE. 0 ) GOTO 999
      contxt = 'Failed to write LO_THRES keyword' 
      CALL FTPKYF(ounit,'LO_THRES',Rsp_min,6,
     &            'lower threshold for stored matrix',Status)
      IF ( Status .NE. 0 ) GOTO 999

      contxt = 'Failed to write RMFVERSN keyword' 
      CALL FTPKYS(ounit,'RMFVERSN','1992a',
     &            'deprecated classification of format',Status)
      IF ( Status .NE. 0 ) GOTO 999

c The TLMIN and TLMAX for the channel column.

      contxt = 'Failed to write TLMIN4 keyword' 
      CALL FTPKYJ(ounit,'TLMIN4',1, 'Start channel used',Status)
      IF ( Status .NE. 0 ) GOTO 999
      contxt = 'Failed to write TLMAX4 keyword' 
      CALL FTPKYJ(ounit,'TLMAX4',N_channels, 'Last channel used',Status)
      IF ( Status .NE. 0 ) GOTO 999
 
c Define the extension structure
 
      contxt = 'Failed to define RSP_MATRIX extension structure'
      CALL FTBDEF(ounit,nfields,tform,0,N_energies,Status)
      IF ( Status .NE. 0 ) GOTO 999
 
c and write out the data
 
      igrp = 1
      iresp = 1
      DO i = 1 , N_energies
 
c the energy range
 
         CALL FTPCLE(ounit,1,i,1,1,Energies(i-1),Status)
         CALL FTPCLE(ounit,2,i,1,1,Energies(i),Status)
         WRITE(contxt, '(a,i6)') 
     &     'Failed to write energy information for range ', i
         IF ( Status .NE. 0 ) GOTO 999
 
c the grouping info

         nmat = 0 
         CALL FTPCLJ(ounit,3,i,1,1,Ngroup(i),Status)
         DO j = 1, Ngroup(i) 
            nmat = nmat + Ichane(igrp) - Ichanb(igrp) + 1
            CALL FTPCLJ(ounit,4,i,j,1,Ichanb(igrp),Status)
            CALL FTPCLJ(ounit,5,i,j,1,
     &                  (Ichane(igrp)-Ichanb(igrp)+1),Status)
            igrp = igrp + 1
         ENDDO
         WRITE(contxt, '(a,i6)') 
     &     'Failed to write grouping information for range ', i
         IF ( Status .NE. 0 ) GOTO 999
 
c the matrix
 
         CALL FTPCLE(ounit,6,i,1,nmat,Matrix(iresp),Status)
         WRITE(contxt, *) 
     &     'Failed to write ', nmat, ' matrix elements for range ', i
         IF ( Status .NE. 0 ) GOTO 999
 
c update the pointers
 
         iresp = iresp + nmat
 
      ENDDO

      contxt = 'Failed to close the FITS file' 
      CALL FTCLOS(ounit,Status)
      IF ( Status .NE. 0 ) GOTO 999
      CALL FRELUN(ounit)
 
 999  CONTINUE
      IF ( Status .NE. 0 ) THEN
         CALL xwrite(contxt, 10)
         WRITE(contxt, '(a,i4)') ' error = ', Status
         CALL xwrite(contxt, 10)
      ENDIF
 
      RETURN
      END
 
