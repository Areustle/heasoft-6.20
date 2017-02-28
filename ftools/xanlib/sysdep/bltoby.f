      SUBROUTINE bltoby(bytear,lenbyt,buf,clen,cpage,lenbuf,iunit)

c		bltoby		fwj haberl 9 march 1987
c		SHF subroutine the accumulates sequential blocks from a
c		direct access file into a byte buffer
c	bytear	b(lenbyt)	r: string of bytes to be accumulated
c	lenbyt	i4		i: no. of bytes to accumulate
c	buf	b(lenbuf)	i/r: accumulation buffer
c	clen	i4		i/r: current location in buffer
c	cpage	i4		i/r: current record in file
c	lenbuf	i4		i: size of buffer (BYTYES !!!)
c	iunit	i4		i: Fortran unit no.  File must have been
c				previously opend as a Fortran Unformatted
c				direct access file.  Be sure that the record
c				length in the open statement was a
c				number of full-words (4 byte quantitites).
c				(Thus lenbuf must be a multiple of 4)

      INTEGER lenbuf
      character(1) bytear(*), buf(lenbuf)
      INTEGER lenbyt, nrec, iunit, cpage, clen, blen, bres
      INTEGER j, i

      IF (lenbyt .LE. 0) RETURN

      blen=0
      IF (clen .GT. 0) THEN
         bres=lenbuf-clen
         DO j=1,min(bres,lenbyt)
            clen=clen+1
            blen=blen+1
            bytear(blen)=buf(clen)
         ENDDO
      ELSE
         bres=0
      ENDIF

      nrec=(lenbyt-bres)/lenbuf

      DO i=1,nrec
          clen=0
          cpage=cpage+1
          READ(iunit,rec=cpage)buf
          DO j=1,lenbuf
            blen=blen+1
            clen=clen+1
            bytear(blen)=buf(j)
          ENDDO
      ENDDO

      IF (blen .LT. lenbyt) THEN
          clen=0
          cpage=cpage+1
          READ(iunit,rec=cpage)buf
          bres=lenbyt-blen
          DO j=1,bres
            clen=clen+1
            blen=blen+1
            bytear(blen)=buf(j)
          ENDDO
      ENDIF

      RETURN
      END
