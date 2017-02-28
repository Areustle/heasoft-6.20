C     gtcalf_interface.f
C
C     An easily callable C interface to be used to call gtcalf.f from the
C     callib library.

      subroutine gtcaface( chatter, detnum, status )

      implicit none
      integer       chatter, detnum, status
      character(3)   detnam
      character(128) filenam(1), online(1)
      integer       extno(1), nret, nfound

      character(128) passfile
      common/pfile/passfile

C     Set the detector name
      if ( detnum .EQ. 0 ) then
         detnam = 'PWA'
      else
         detnam = 'PWB'
      endif

C     Call our "real" FORTRAN routine
      call gtcalf( chatter, 'XTE', 'HEXTE', detnam, '-', 'HKCONV', 
     c '31/12/95', '00:00:00', 
     c '31/12/99', '00:00:00',
     c '-', 1, filenam, extno, online, nret, nfound, status)

      if ( (status .NE. 0) .OR. (nfound .GT. 1) ) then
         call fcecho('Unable to get coefficients file from CALDB')
         goto 999
      else
C     Copy the name of the input file to the common block
         passfile = filenam(1)
      endif

 999  continue

      return

      end
