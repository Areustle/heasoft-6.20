
      SUBROUTINE cphdeb(iunit, inrmfl, ounit, nhist, hist, 
     &                  contxt, status)

      IMPLICIT none
      CHARACTER*(*) inrmfl, contxt, hist(*)
      INTEGER iunit, ounit, nhist, status

c Copy the header and energy bound extensions from the first RMF file
c into the output file.

*  Modifications
*       Peter D Wilson 02/20/98
*             . Preserve history keywords of RSP extension of
*               first RMF file
*

c Arguments :
c     iunit        i      i: I/O unit for input RMF file
c     inrmfl       c      i: Name of input RMF file
c     ounit        i      i: I/O unit for output RMF file (already opened)
c     nhist        i      i: Number of history keywords
c     hist         c      i: History keywords
c     contxt       c      r: error message
c     status       i      r: Status   0 = OK, !0 = !OK

      INTEGER rmfsiz, nfound, htype, i, j
      INTEGER next(10)

      character(20) instr(2)
      character(20) outhdu(9,10), extnam(10), outver(9,10)

      status = 0

c Open the input RMF file

      rmfsiz=2880 
      CALL ftopen(iunit, inrmfl, 0, rmfsiz, status)
      contxt = 'CPHDEB : Failed to open first RMF'
      IF ( status .NE. 0 ) RETURN

c Copy the header from the input RMF to the output RMF

      CALL ftcopy(iunit, ounit, nhist, status)
      contxt = 'CPHDEB : Failed to copy header'
      IF ( status .NE. 0 ) RETURN

c Add the history keywords

      DO i = 1, nhist
         CALL ftphis(ounit, hist(i), status)
         contxt = 'CPHDEB : Failed to write history keyword'
         IF ( status .NE. 0 ) RETURN
      ENDDO

c Append a new empty HDU to the output RMF

      CALL ftcrhd(ounit, status)
      contxt = 'CPHDEB : Failed to append new HDU to the output RMF'
      IF ( status .NE. 0 ) RETURN

c Find the EBOUNDS extension in the input RMF and go to it

      instr(1) = 'RESPONSE'
      instr(2) = 'EBOUNDS'
      CALL fndhdu(5, iunit, 2, instr, 10, nfound, next, outhdu,
     &            outver, extnam, status)
      contxt = 'CPHDEB : Failed to find EBOUNDS extension in first RMF'
      IF ( nfound .EQ. 0 ) status = 1
      IF ( status .NE. 0 ) RETURN

      CALL ftmrhd(iunit, next(1), htype, status)
      contxt = 'CPHDEB : Failed to go to EBOUNDS extension in first RMF'
      IF ( status .NE. 0 ) RETURN

c Copy the EBOUNDS extension from the input RMF to the output RMF

      CALL ftcopy(iunit, ounit, 0, status)
      contxt = 'CPHDEB : Failed to copy EBOUNDS extension'
      IF ( status .NE. 0 ) RETURN

c Close then reopen input RMF to reset HDU position... PDW 02/20/98

      CALL ftclos(iunit, status)
      contxt = 'CPHDEB : Failed to close EBOUNDS extension'

      IF ( status .NE. 0 ) RETURN
      CALL ftopen(iunit, inrmfl, 0, rmfsiz, status)
      contxt = 'CPHDEB : Failed to reopen first RMF'
      IF ( status .NE. 0 ) RETURN
      
c Find the extension with the redistribution matrix.

      instr(1) = 'RESPONSE'
      instr(2) = 'RSP_MATRIX'
      do j=1,10
         next(j)= 0
      enddo
      CALL fndhdu(5, iunit, 2, instr, 10, nfound, next, outhdu,
     &     outver, extnam, status)
      
      contxt = 'CPHDEB : Failed to find RSP extension in first RMF'
      IF ( nfound .EQ. 0 ) status = 1
      IF ( status .NE. 0 ) RETURN
      
c Go to the extension and read in history keywords
      
      CALL ftmrhd(iunit, next(1), htype, status)
      contxt = 'CPHDEB : Failed to go to matrix extension in first RMF'
      IF ( status .NE. 0 ) RETURN
      
      IF ( nhist.lt.99 ) then
         nhist = nhist+1
         hist(nhist) = '*** History of I/p RMF file: '//inrmfl
         CALL fcrhky(iunit,100,nhist,hist,status)
      ENDIF

c Close input RMF

      CALL ftclos(iunit, status)
      contxt = 'CPHDEB : Failed to close matrix extension'

      RETURN
      END
