
      SUBROUTINE gtenrg(rsu, rsfil, nenerg, lower, hiher,
     &                  middl, status)

      INTEGER rsu, nenerg, status
      REAL lower(nenerg), hiher(nenerg), middl(nenerg)
      CHARACTER*(*) rsfil

c Routine to read the RMF file specified and return the energy ranges
c to be used when calculating the ARF.

c Arguments :
c     rsu     i      i: I/O unit for RMF file
c     rsfil   c      i: Name of RMF file
c     nenerg  i      r: Actual size of energy array
c     lower   r      r: Lower energies
c     hiher   r      r: Upper energies
c     middl   r      r: Middle energies
c     status  i      r: Status  -   0 = OK

      INTEGER rstype, rssize, i, iext
      INTEGER egyloc, egyhic
      CHARACTER rstnam*64, comment*80, contxt*72
      LOGICAL anyf, found

      status = 0

      CALL ftopen(rsu,rsfil,0,rssize,status)
      contxt = 'Error opening response file'
      IF ( status .NE. 0 ) GOTO 999

c need to find the extension containing the energies used in the RMF.

      iext = 1
      found = .FALSE.

      DO WHILE (.NOT. found)

         CALL ftmahd(rsu,iext,rstype,status)
         contxt = 'Failed to find extension with RMF energies'
         IF ( status .NE. 0 ) GOTO 999

         CALL ftgkys(rsu,'EXTNAME',rstnam,comment,status)
         IF (rstnam .EQ. 'SPECRESP MATRIX') found = .TRUE.
         IF (rstnam .EQ. 'MATRIX') found = .TRUE.
         status = 0
         CALL ftgkys(rsu,'HDUCLAS1',rstnam,comment,status)
         IF ( status.EQ.0 .AND. rstnam.EQ.'RESPONSE' ) THEN
            CALL ftgkys(rsu,'HDUCLAS2',rstnam,comment,status)
            IF ( rstnam .EQ. 'RSP_MATRIX' ) found = .TRUE.
         ENDIF
         status = 0
         iext = iext + 1

      ENDDO

      CALL ftgcno(rsu, .false., 'ENERG_LO', egyloc, status)
      contxt = 'Failed to read ENERG_LO keyword'
      IF ( status .NE. 0 ) GOTO 999
      CALL ftgcno(rsu, .false., 'ENERG_HI', egyhic, status)
      contxt = 'Failed to read ENERG_HI keyword'
      IF ( status .NE. 0 ) GOTO 999

      anyf = .FALSE.

      DO i = 1, nenerg

         CALL ftgcve(rsu,egyloc,i,1,1,0,lower(i),anyf,status)
         WRITE(contxt,'(a,i5)') 'Failed to read lower energy ', i
         IF ( status .NE. 0 ) GOTO 999
         CALL ftgcve(rsu,egyhic,i,1,1,0,hiher(i),anyf,status)
         WRITE(contxt,'(a,i5)') 'Failed to read upper energy ', i
         IF ( status .NE. 0 ) GOTO 999

         middl(i)=(lower(i)+hiher(i))/2.

*if middle of energy bin is outside the range useable by xrtea (i.e., less
*than zero or greater than 12 eV), we'll consider this bin to have an
*effective area of zero (rather than an effective area given by xrtea),
*we'll notify the user that this has occurred, and we'll write a comment
*line to the ARF file. We'll set middl for this bin = -999 so that we can
*test below, when we are actually calculating the effective area, whether
*or not to use xrtea or just use zero.

         IF ((middl(i).le.0.0).or.(middl(i).gt.12.0)) THEN
            call fcecho(
     &  'Warning: an RMF energy is out of range. Effective area will')
            call fcecho(
     &  '         set to for this energy channel.')
            middl(i)=-999
         ENDIF

      ENDDO


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


