c Program to read an RMF file and compress it by throwing out all response
c elements below a value input by the user. 

c Parameters
c     infile       input response matrix file
c     outfile      output response matrix file
c     threshold    value below which to throw out response elements.
c     clobber      overwrite the output file
c
c  2004-oct-19   update LO_THRES on output MFC
c
c

      SUBROUTINE cmprmf
      IMPLICIT NONE

      REAL threshold

      CHARACTER(255) infile, outfile
      CHARACTER(72) contxt

      LOGICAL qclobber

      character(40) taskname
      COMMON /task/ taskname

      taskname = 'cmprmf v1.03'
      contxt = taskname(1:13)//'   21 Jan 2015'
      CALL fcecho(contxt)

c Get the parameters 

      CALL gcmprmf(infile, outfile, threshold, qclobber)

c Do the actual task

      CALL dcmprmf(infile, outfile, threshold, qclobber)

      RETURN
      END



c **************************************************************************

      SUBROUTINE gcmprmf(infile, outfile, threshold, qclobber)

      REAL threshold

      CHARACTER infile*(*), outfile*(*)

      LOGICAL qclobber

c Gets the parameters
c    infile        input response matrix file
c    outfile       output response matrix file
c    threshold     value below which to throw out response elements.
c    clobber       overwrite the output file

      INTEGER status

      CHARACTER(72) contxt

      status = 0

c Get the input response matrix filename

      CALL uclgst('infile', infile, status)
      contxt = 'Failed to get infile parameter'
      IF ( status .NE. 0 ) GOTO 999

c Get the output response matrix filename

      CALL uclgst('outfile', outfile, status)
      contxt = 'Failed to get outfile parameter'
      IF ( status .NE. 0 ) GOTO 999

c Get the threshold

      CALL uclgsr('threshold', threshold, status)
      contxt = 'Failed to get threshold parameter'
      IF ( status .NE. 0 ) GOTO 999

c Get the clobber parameter

      CALL uclgsb('clobber', qclobber, status)
      contxt = 'Failed to get clobber parameter'
      IF ( status .NE. 0 ) GOTO 999

 999  CONTINUE

      IF ( status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(status)
         STOP
      ENDIF
 
      RETURN
      END

c **************************************************************************

      SUBROUTINE dcmprmf(infile, outfile, threshold, qclobber)

      REAL threshold

      CHARACTER infile*(*), outfile*(*)

      LOGICAL qclobber

c This routine does the work. It starts by copying the primary HDU and CHDU
c with the EBOUNDS from the input to the output files. Then it loops through
c the MATRIX array, one energy at a time, reading the response, compressing 
c it, and writing it out.

c Dynamic memory allocation stuff

      INTEGER          MEMI(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      EQUIVALENCE (MEMI,MEMR,MEMD)
      COMMON /MEM/ MEMD

c Pointers to dynamic arrays

      INTEGER irspelt, iichanb, iichann, iorder
      INTEGER orspelt, oichanb, oichann, oorder

c Local variables

      REAL energy(2)       

      INTEGER nenerg, nchan, ie, ngrp, igrp, i, nelt
      INTEGER block, status, iunit, ounit, ogrp, naxis1
      INTEGER datacode, repeat, width
      INTEGER ntotgrp, ntotelt
      INTEGER colnum(7)

      CHARACTER(255) contxt
      CHARACTER(80) comment
      CHARACTER(8) colnam(7)
      CHARACTER(6) tstring

      LOGICAL qorder, qany

      INTEGER lenact
      EXTERNAL lenact

      DATA colnam /'ENERG_LO', 'ENERG_HI', 'N_GRP', 'F_CHAN', 'N_CHAN',
     &             'MATRIX', 'ORDER' /

      status = 0

c Open the input and output files.

      CALL getlun(iunit)
      CALL ftopen(iunit, infile, 0, block, status)
      contxt = 'Failed to open '//infile(:lenact(infile))
      IF ( status .NE. 0 ) GOTO 999

      IF ( qclobber ) CALL xdelfil(outfile, status)
      contxt = 'Failed to delete old '//outfile(:lenact(outfile))
      IF ( status .NE. 0 ) GOTO 999

      CALL getlun(ounit)
      CALL ftinit(ounit, outfile, block, status)
      contxt = 'Failed to open '//outfile(:lenact(outfile))
      IF ( status .NE. 0 ) GOTO 999

c Copy the primary HDU.

      CALL ftcopy(iunit, ounit, 0, status)
      contxt = 'Failed to copy primary header'
      IF ( status .NE. 0 ) GOTO 999
      CALL ftcrhd(ounit, status)
      contxt = 'Failed to create second HDU'
      IF ( status .NE. 0 ) GOTO 999

c Copy the EBOUNDS extension.

      CALL ftmnhd(iunit, 2, 'EBOUNDS', 0, status)
      contxt = 'Failed to find EBOUNDS extension in input file'
      IF ( status .NE. 0 ) GOTO 999
      CALL ftcopy(iunit, ounit, 0, status)
      contxt = 'Failed to copy EBOUNDS extension'
      IF ( status .NE. 0 ) GOTO 999
      CALL ftcrhd(ounit, status)
      contxt = 'Failed to create third HDU'
      IF ( status .NE. 0 ) GOTO 999

c Go to the SPECRESP MATRIX extension.

      CALL ftmnhd(iunit, 2, 'SPECRESP MATRIX', 0, status)
      IF ( status .NE. 0 ) THEN
         status = 0
         CALL ftmnhd(iunit, 2, 'MATRIX', 0, status)
         contxt = 'Failed to find MATRIX extension in input file'
         IF ( status .NE. 0 ) GOTO 999
      ENDIF

c Copy the keywords but not the data

      CALL ftcphd(iunit, ounit, status)
      contxt = 'Failed to copy SPECRESP header'
      IF ( status .NE. 0 ) GOTO 999

c Get the number of energies and number of channels.

      CALL ftgkyj(iunit, 'NAXIS2', nenerg, comment, status)
      contxt = 'Failed to read NAXIS2 from SPECRESP extension'
      IF ( status .NE. 0 ) GOTO 999

      CALL ftgkyj(iunit, 'DETCHANS', nchan, comment, status)
      contxt = 'Failed to read DETCHANS from SPECRESP extension'
      IF ( status .NE. 0 ) GOTO 999

c Find the column numbers for the quantities in the SPECRESP extension.
c The ORDER column is optional so don't worry about it if it isn't there.

      DO i = 1, 6
         CALL ftgcno(iunit,.FALSE.,colnam(i),colnum(i),status)
         contxt = 'Failed to find column for '//colnam(i)
         IF ( status .NE. 0 ) GOTO 999
      ENDDO
      CALL ftgcno(iunit, .FALSE., colnam(7), colnum(7), status)
      IF ( status .NE. 0 ) THEN
         qorder = .FALSE.
         status = 0
      ELSE
         qorder = .TRUE.
      ENDIF

c Grab enough memory to handle one energy at a time. Just set the maximum
c number of response groups as the number of channels - overkill but shouldn't
c matter.

      irspelt = 0
      orspelt = 0
      iichanb = 0
      oichanb = 0
      iichann = 0
      oichann = 0
      iorder = 0
      oorder = 0

      CALL udmget(nchan, 6, irspelt, status)
      contxt = 'Failed to get memory for input response elements'
      IF ( status .NE. 0 ) GOTO 999
      CALL udmget(nchan, 6, orspelt, status)
      contxt = 'Failed to get memory for output response elements'
      IF ( status .NE. 0 ) GOTO 999
      CALL udmget(nchan, 4, iichanb, status)
      contxt = 'Failed to get memory for input start channels'
      IF ( status .NE. 0 ) GOTO 999
      CALL udmget(nchan, 4, oichanb, status)
      contxt = 'Failed to get memory for output start channels'
      IF ( status .NE. 0 ) GOTO 999
      CALL udmget(nchan, 6, iichann, status)
      contxt = 'Failed to get memory for input number of channels'
      IF ( status .NE. 0 ) GOTO 999
      CALL udmget(nchan, 6, oichann, status)
      contxt = 'Failed to get memory for output number of channels'
      IF ( status .NE. 0 ) GOTO 999
      IF ( qorder ) THEN
         CALL udmget(nchan, 6, iorder, status)
         contxt = 'Failed to get memory for input order array'
         IF ( status .NE. 0 ) GOTO 999
         CALL udmget(nchan, 6, oorder, status)
         contxt = 'Failed to get memory for output order array'
         IF ( status .NE. 0 ) GOTO 999
      ENDIF

c Check whether F_CHAN, N_CHAN, and MATRIX are already variable length
c arrays. If not, update the output NAXIS1 by the appropriate amount and
c set the output TFORM keywords for variable length arrays.

      CALL ftgkyj(iunit, 'NAXIS1', naxis1, comment, status)
      contxt = 'Failed to read NAXIS1 keyword from SPECRESP extension'
      IF ( status .NE. 0 ) GOTO 999

      CALL ftgtcl(iunit, colnum(4), datacode, repeat, width, status)
      contxt = 'Failed to get info about F_CHAN column'
      IF ( status .NE. 0 ) GOTO 999
      IF ( datacode .GT. 0 ) naxis1 = naxis1 - repeat*(datacode/10) + 8
      WRITE(tstring, '(a5,i1)') 'TFORM', colnum(4)
      CALL ftukys(ounit, tstring, 'PI', 'variable length array', status)
      contxt = 'Failed to update TFORM keyword for F_CHAN column'
      IF ( status .NE. 0 ) GOTO 999

      CALL ftgtcl(iunit, colnum(5), datacode, repeat, width, status)
      contxt = 'Failed to get info about N_CHAN column'
      IF ( status .NE. 0 ) GOTO 999
      IF ( datacode .GT. 0 ) naxis1 = naxis1 - repeat*(datacode/10) + 8
      WRITE(tstring, '(a5,i1)') 'TFORM', colnum(5)
      CALL ftukys(ounit, tstring, 'PI', 'variable length array', status)
      contxt = 'Failed to update TFORM keyword for N_CHAN column'
      IF ( status .NE. 0 ) GOTO 999

      CALL ftgtcl(iunit, colnum(6), datacode, repeat, width, status)
      contxt = 'Failed to get info about MAXTRIX column'
      IF ( status .NE. 0 ) GOTO 999
      IF ( datacode .GT. 0 ) naxis1 = naxis1 - repeat*(datacode/10) + 8
      WRITE(tstring, '(a5,i1)') 'TFORM', colnum(6)
      CALL ftukys(ounit, tstring, 'PE', 'variable length array', status)
      contxt = 'Failed to update TFORM keyword for MATRIX column'
      IF ( status .NE. 0 ) GOTO 999

c Reset NAXIS1 to take into account variable length arrays

      CALL ftukyj(ounit, 'NAXIS1', naxis1, 'width of table in bytes', 
     &            status)
      contxt = 'Failed to update NAXIS1 keyword in SPECRESP extension'
      IF ( status .NE. 0 ) GOTO 999

c Need to keep track of the total number of response groups and elements

      ntotgrp = 0
      ntotelt = 0

c Loop round energies

      DO ie = 1, nenerg

c Read the response information for this energy

         CALL ftgcve(iunit, colnum(1), ie, 1, 1, 0., energy(1), qany, 
     &               status)
         WRITE(contxt, '(a,i8)') 'Failed to read ENERG_LO for energy ', 
     &                           ie
         IF ( status .NE. 0 ) GOTO 999

         CALL ftgcve(iunit, colnum(2), ie, 1, 1, 0., energy(2), qany, 
     &               status)
         WRITE(contxt, '(a,i8)') 'Failed to read ENERG_HI for energy ', 
     &                           ie
         IF ( status .NE. 0 ) GOTO 999

         CALL ftgcvj(iunit, colnum(3), ie, 1, 1, 0, ngrp, qany, status)
         WRITE(contxt, '(a,i8)') 'Failed to read N_GRP for energy ', 
     &                           ie
         IF ( status .NE. 0 ) GOTO 999

         CALL ftgcvj(iunit, colnum(4), ie, 1, ngrp, 0, MEMI(iichanb), 
     &               qany, status)
         WRITE(contxt, '(a,i8)') 'Failed to read F_CHAN for energy ', 
     &                           ie
         IF ( status .NE. 0 ) GOTO 999

         CALL ftgcvj(iunit, colnum(5), ie, 1, ngrp, 0, MEMI(iichann), 
     &               qany, status)
         WRITE(contxt, '(a,i8)') 'Failed to read F_CHAN for energy ', 
     &                           ie
         IF ( status .NE. 0 ) GOTO 999

         IF ( qorder ) THEN
            CALL ftgcvj(iunit, colnum(7), ie, 1, ngrp, 0, MEMI(iorder), 
     &                  qany, status)
            WRITE(contxt, '(a,i8)') 'Failed to read ORDER for energy ', 
     &                              ie
            IF ( status .NE. 0 ) GOTO 999
         ENDIF

c Calculate the number of response elements to read and get them

         nelt = 0
         DO igrp = 1, ngrp
            nelt = nelt + MEMI(iichann+igrp-1)
         ENDDO

         CALL ftgcve(iunit, colnum(6), ie, 1, nelt, 0., MEMR(irspelt), 
     &               qany, status)
         WRITE(contxt, '(a,i8)') 'Failed to read MATRIX for energy ', 
     &                           ie
         IF ( status .NE. 0 ) GOTO 999

c Loop round elements checking whether they exceed threshold. Remove unwanted
c elements and update the groups and channel boundaries.

         CALL cmprow(threshold, ngrp, MEMI(iichanb), MEMI(iichann), 
     &               MEMR(irspelt), qorder, MEMI(iorder), ogrp, 
     &               MEMI(oichanb), MEMI(oichann), MEMR(orspelt), 
     &               MEMI(oorder))

         nelt = 0
         DO igrp = 1, ogrp
            nelt = nelt + MEMI(oichann+igrp-1)
         ENDDO

c Add to the running totals for response groups and elements

         ntotgrp = ntotgrp + ogrp
         ntotelt = ntotelt + nelt

c Write the data for this energy to the output file

         CALL ftpcle(ounit, colnum(1), ie, 1, 1, energy(1), status)
         WRITE(contxt, '(a,i8)') 'Failed to write ENERG_LO for energy ', 
     &                           ie
         IF ( status .NE. 0 ) GOTO 999

         CALL ftpcle(ounit, colnum(2), ie, 1, 1, energy(2), status)
         WRITE(contxt, '(a,i8)') 'Failed to write ENERG_HI for energy ', 
     &                           ie
         IF ( status .NE. 0 ) GOTO 999

         CALL ftpclj(ounit, colnum(3), ie, 1, 1, ogrp, status)
         WRITE(contxt, '(a,i8)') 'Failed to write N_GRP for energy ', 
     &                           ie
         IF ( status .NE. 0 ) GOTO 999

         CALL ftpclj(ounit, colnum(4), ie, 1, ogrp, MEMI(oichanb), 
     &               status)
         WRITE(contxt, '(a,i8)') 'Failed to write F_CHAN for energy ', 
     &                           ie
         IF ( status .NE. 0 ) GOTO 999

         CALL ftpclj(ounit, colnum(5), ie, 1, ogrp, MEMI(oichann), 
     &               status)
         WRITE(contxt, '(a,i8)') 'Failed to write N_CHAN for energy ', 
     &                           ie
         IF ( status .NE. 0 ) GOTO 999

         CALL ftpcle(ounit, colnum(6), ie, 1, nelt, MEMR(orspelt), 
     &               status)
         WRITE(contxt, '(a,i8)') 'Failed to write MATRIX for energy ', 
     &                           ie
         IF ( status .NE. 0 ) GOTO 999

         IF ( qorder ) THEN
            CALL ftpclj(ounit, colnum(7), ie, 1, ogrp, MEMI(oorder), 
     &                  status)
            WRITE(contxt, '(a,i8)') 'Failed to write ORDER for energy ', 
     &                              ie
            IF ( status .NE. 0 ) GOTO 999
         ENDIF

c End loop over energies

      ENDDO
      
c Update LO_THRES keyword
      
      CALL FTMKYE(ounit,'LO_THRES',threshold,5,'modified by cmprmf', 
     &       status)
      contxt='Failed to write LO_THRES keyword'
      IF ( status .NE. 0) GOTO 999

c Update the NUMELT and NUMGRP keywords

      CALL FTUKYJ(ounit,'NUMELT',ntotelt,
     &     'total number of response elements', status)
      contxt='Failed to write NUMELT keyword'
      IF ( status .NE. 0) GOTO 999

      CALL FTUKYJ(ounit,'NUMGRP',ntotgrp,
     &     'total number of response groups', status)
      contxt='Failed to write NUMGRP keyword'
      IF ( status .NE. 0) GOTO 999

c Copy any extra columns in the current extension

      CALL cpexco(iunit, ounit, status)
      contxt = 'Failed to copy any extra columns in the SPECRESP'
      IF ( status .NE. 0 ) GOTO 999

c Copy any extra extensions from the input to the output file

      CALL cpextr(iunit, ounit, status)
      contxt = 'Failed to copy any extra extensions'
      IF ( status .NE. 0 ) GOTO 999

c Close the input and output files

      CALL ftclos(iunit, status)
      contxt = 'Failed to close input file'
      IF ( status .NE. 0 ) GOTO 999

      CALL ftclos(ounit, status)
      contxt = 'Failed to close output file'
      IF ( status .NE. 0 ) GOTO 999


 999  CONTINUE

      IF ( status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(status)
         STOP
      ENDIF
 
      RETURN
      END

c ************************************************************************

      SUBROUTINE cmprow(threshold, ngrp, iichanb, iichann, irspelt, 
     &                  qorder, iorder, ogrp, oichanb, oichann, 
     &                  orspelt, oorder)

      REAL irspelt(*), orspelt(*)
      REAL threshold

      INTEGER iichanb(*), iichann(*), oichanb(*), oichann(*)
      INTEGER iorder(*), oorder(*)
      INTEGER ngrp, ogrp

      LOGICAL qorder
      
c Compresses the current row using the threshold.

      INTEGER iresp, oresp, igrp, ichan

      LOGICAL qingrp

      qingrp = .FALSE.
      ogrp = 0
      iresp = 0
      oresp = 0
      DO igrp = 1, ngrp

         DO ichan = iichanb(igrp), iichanb(igrp)+iichann(igrp)-1

            iresp = iresp + 1
            IF ( irspelt(iresp) .GE. threshold ) THEN

               IF ( .NOT.qingrp ) THEN
                  ogrp = ogrp + 1
                  oichanb(ogrp) = ichan
                  IF ( qorder ) oorder(ogrp) = iorder(igrp)
                  qingrp = .TRUE.
               ENDIF
               oresp = oresp + 1
               orspelt(oresp) = irspelt(iresp)

            ELSE

               IF ( qingrp ) THEN
                  oichann(ogrp) = ichan - oichanb(ogrp)
                  qingrp = .FALSE.
               ENDIF

            ENDIF

         ENDDO

         IF ( qingrp ) THEN
            oichann(ogrp) = iichanb(igrp)+iichann(igrp)-1
     &                     -oichanb(ogrp) + 1
            qingrp = .FALSE.
         ENDIF

      ENDDO

      RETURN
      END


c ************************************************************************

      SUBROUTINE cpextr(iunit, ounit, status)

      INTEGER iunit, ounit, status

c Copy any extra extensions other than the EBOUNDS or SPECRESP MATRIX from
c the input file to the output file.

      INTEGER iext, hdutyp

      CHARACTER(20) extnam
      CHARACTER(80) comment
      CHARACTER(72) contxt

c Loop round extensions

      iext = 1

      DO WHILE ( status .EQ. 0 )

         iext = iext + 1
         CALL ftmahd(iunit, iext, hdutyp, status)

         IF ( status .EQ. 0 ) THEN

            CALL ftgkys(iunit, 'EXTNAME', extnam, comment, status)

c If it isn't one of the standard extensions then copy it over.

            IF ( status .NE. 0 .OR. (extnam .NE. 'EBOUNDS' .AND.
     &           extnam .NE. 'SPECRESP MATRIX' .AND.
     &           extnam .NE. 'MATRIX') ) THEN

               status = 0
               CALL ftcopy(iunit, ounit, 0, status)
               contxt = 'Failed to copy extra extension'
               IF ( status .NE. 0 ) GOTO 999

            ENDIF

            status = 0

         ENDIF

      ENDDO

      status = 0

 999  CONTINUE

      IF ( status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(status)
         STOP
      ENDIF
 
      RETURN
      END


c ************************************************************************

      SUBROUTINE cpexco(iunit, ounit, status)

      INTEGER iunit, ounit, status

c Copy any extra columns in the SPECRESP extension other than the standard
c ones for the response matrix.

      INTEGER icol, ncol

      CHARACTER(20) colnam
      CHARACTER(80) comment
      CHARACTER(72) contxt
      CHARACTER(8) ttype

c Get the number of columns

      CALL ftgkyj(iunit, 'TFIELDS', ncol, comment, status)
      contxt = 'Failed to read TFIELDS from SPECRESP extension'
      IF ( status .NE. 0 ) GOTO 999

c If there are only 6 columns then we are done.

      IF ( ncol .EQ. 6 ) RETURN

c Otherwise loop through the columns finding the extra ones.

      DO icol = 1, ncol

         ttype = 'TTYPE'
         IF ( icol .LE. 9 ) THEN
            WRITE(ttype(6:6), '(i1)') icol
         ELSEIF ( icol .LE. 99 ) THEN
            WRITE(ttype(6:7), '(i2)') icol
         ELSE
            WRITE(ttype(6:8), '(i3)') icol
         ENDIF

         CALL ftgkys(iunit, ttype, colnam, comment, status)
         contxt = 'Failed to read '//ttype//' from SPECRESP extension'
         IF ( status .NE. 0 ) GOTO 999

         IF ( colnam .NE. 'ENERG_LO' .AND. colnam .NE. 'ENERG_HI' .AND.
     &        colnam .NE. 'N_GRP' .AND. colnam .NE. 'F_CHAN' .AND.
     &        colnam .NE. 'N_CHAN' .AND. colnam .NE. 'MATRIX' ) THEN
            CALL ftcpcl(iunit, ounit, icol, icol, .FALSE., status)
            WRITE(contxt, '(a,i4)') 'Failed to copy column ', icol
            IF ( status .NE. 0 ) GOTO 999
         ENDIF

      ENDDO

 999  CONTINUE

      IF ( status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(status)
         STOP
      ENDIF
 
      RETURN
      END
