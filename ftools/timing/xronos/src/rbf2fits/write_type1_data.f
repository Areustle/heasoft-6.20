C*******************************************************************
      SUBROUTINE write_type1_data(rbf_unit, fits_unit, newrec1
     &                           , newrec2, status)
C*******************************************************************
      include 'xrrbstr.h'
c
      integer blocksize, reclen, tnull
      parameter (blocksize = 2880, reclen = 256, tnull = 254)
      character(60)  comment
      character(1) y(blocksize)
      byte count1(reclen)
      INTEGER frow, irec, ii, i, naxis1, naxis2, rbf_unit, fits_unit
     &   , status, j, k, nelem, test
      RECORD /rnewbuf_rec_1/newrec1
      RECORD /rnewbuf_rec_2/newrec2

c     Fetch the full width of the binary table (naxis1; fixed in write_deftype1)
c     and the number of rows (naxis2; subject to change).
      CALL FTGKYJ(fits_unit, 'NAXIS1', naxis1, comment, status)
      CALL FTGKYJ(fits_unit, 'NAXIS2', naxis2, comment, status)

c     Initialization of fits row counter and rate buffer record #.
      k    = 0
      frow = 0
      irec = 4
c
      DO WHILE (irec.ne.-1)
         k = k + 1
         DO 10 j = 1, blocksize
            IF ((ii.GT.reclen).or.(irec.eq.4)) THEN 
c              Read another record.
               ii = 1
               irec = irec + 1
               READ(rbf_unit,REC=irec,ERR=990) (count1(i), i=1, reclen)
            ENDIF
90          continue

c           Flag for end of file, gaps and overflow.
            test = ichar(char(count1(ii)))
            if (test.eq.253) then
c              End of data reached.  
               irec = -1
               GOTO 800
            elseif((test.eq.254).or.(test.eq.255)) then
c              It's either a gap or an overflow.
               y(j) = char(tnull)
            else
c              Prepare y value.
               y(j) = char(count1(ii))
            endif
c           Increase bin counter
            ii = ii + 1
10       CONTINUE

800      CONTINUE

c        Make more space in the binary table, as necessary.
         if( naxis2.le.frow + blocksize) then
            CALL FTDDEF(fits_unit, naxis1*(frow + blocksize), status)
         endif 

c        Write the current block to FITS.  
         frow = (k - 1)*blocksize + 1 
         nelem = j - 1 
         CALL FTPCLB(fits_unit, 1, frow, 1, nelem,  y, status) 
         if(status.ne.0) go to 970 

         if(irec .eq. -1) go to 10000

      ENDDO
 
970   CONTINUE
      call Xwrite(' Error writing to FITS: ftpclb',10)
      go to 10000
c980   CONTINUE
c      call xwrite('Error writing to FITS: ftpclu',10)
c      go to 10000
990   CONTINUE
      call xwrite('write_type1_data Error reading data', 10)

10000 CONTINUE

c     Fix the keyword for the number of rows.
      naxis2 = frow + nelem
      CALL FTMKYJ(fits_unit, 'NAXIS2', naxis2, comment, status)

      RETURN
      END

