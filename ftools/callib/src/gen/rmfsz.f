*+RMFSZ
      SUBROUTINE RMFSZ(Iunit,Chatter,Nenerg,Numgrp,Numelt,Ierr)

      IMPLICIT NONE

      INTEGER Iunit, Chatter, Nenerg, Numgrp, Numelt, Ierr

c --- DESCRIPTION -----------------------------------------------------
c
c Gets the RMF arrays sizes for a file conforming to the HDUVERS='1.*.*' 
c family.
c Currently the OGIP formats supported are
c HDUVERS2 = '1.0.0'
c HDUVERS2 = '1.1.0'
c HDUVERS2 = '1.2.0'
c HUDVERS  = '1.3.0'
c see OGIP/92-002a
c
c Assumes that the FITS file is open and positioned at the RMF extension.
c
c Passed parameters
c  IUNIT         i   : FORTRAN unit number of open RMF file
c  CHATTER       i   : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c  NENERG          o : Number of energy ranges
c  NUMGRP          o : Total number of response groups
c  NUMELT          o : Total number of response elements
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------------
c      kaa   (1.0.0: 98 Dec 30) Original version
c --------------------------------------------------------------------
      character(7) VERSION
      PARAMETER (VERSION='1.0.0')
*-
c Internals
      character(6) SUBNAME
      PARAMETER (SUBNAME='rmfsz')

      INTEGER colnum(2)
      INTEGER status, ie, ig, ngrp, nchan, inull, i
      CHARACTER(30) comm
      CHARACTER(80) message
      character(8) colnam(2)
      LOGICAL anyflg

      DATA colnam /'N_GRP', 'N_CHAN'/

c Initialise
      Ierr = 0
      status = 0
 
c User info, if requested
      message = 'using '//SUBNAME//' '//VERSION
      CALL WTINFO(Chatter,15,1,message)
 
c Get the number of energies from the NAXIS2 keyword

      status = 0
      CALL FTGKYJ(Iunit,'NAXIS2',Nenerg,comm,status)
      IF ( status.NE.0 ) THEN
         CALL WTFERR(SUBNAME,VERSION,status,'reading NAXIS2 keyword')
         Ierr = 4
         GOTO 200
      ENDIF

c Now get the number of groups and elements. First check for the presence
c of the NUMGRP and NUMELT keywords. If they are there then we are done

      CALL FTGKYJ(Iunit,'NUMGRP',Numgrp,comm,status)
      CALL FTGKYJ(Iunit,'NUMELT',Numelt,comm,status)
      IF ( status .EQ. 0 ) GOTO 200

c If the keywords are not there then we actually have to read the N_GRP
c and N_CHAN columns to calculate the total number of groups and matrix
c elements. First find the columns.

      status = 0
      DO i = 1, 2
         CALL FTGCNO(Iunit,.FALSE.,colnam(i),colnum(i),status)
         IF ( status.NE.0 ) THEN
            message = 'Cannot find column for '//colnam(i)
            CALL WTFERR(SUBNAME,VERSION,status,message)
            Ierr = 4
            GOTO 200
         ENDIF
      ENDDO

c Now loop round the rows (energies)

      Numgrp = 0
      Numelt = 0
      inull = 0

      DO ie = 1, Nenerg

         CALL FTGCVJ(Iunit,colnum(1),ie,1,1,inull,ngrp,anyflg,status)
         IF ( status.NE.0 ) THEN
            CALL WTFERR(SUBNAME,VERSION,status,
     &                  'Problem reading N_GRP column')
            Ierr = 1
            GOTO 200
         ENDIF

         DO ig = 1, ngrp

            CALL FTGCVJ(Iunit,colnum(2),ie,ig,1,inull,nchan,anyflg,
     &                  status)
            IF ( status.NE.0 ) THEN
               CALL WTFERR(SUBNAME,VERSION,status,
     &                     'Problem reading N_CHAN column')
               Ierr = 1
               GOTO 200
            ENDIF

            Numelt = Numelt + nchan

         ENDDO

         Numgrp = Numgrp + ngrp

      ENDDO

 200  IF ( Ierr.NE.0 ) THEN
         CALL WTERRM(SUBNAME,VERSION,' Fatal error - aborting')
      ELSE
         CALL WTINFO(Chatter,20,1,
     &               'successfully found sizes for RSP_MATRIX data')
      ENDIF
 
 
      RETURN
      END
c ----------------------------------------------------------------------
c     END OF RMFSZ
c ----------------------------------------------------------------------
