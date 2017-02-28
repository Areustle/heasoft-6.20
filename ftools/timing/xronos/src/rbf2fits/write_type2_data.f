C*******************************************************************
      SUBROUTINE write_type2_data(rbf_unit, fits_unit, newrec1
     &                           , newrec2, status)
C*******************************************************************
      include 'xrrbstr.h'
c
      character(80)  context
      character(60)  comment
      INTEGER*2 count1(num_rdata_2), err1(num_rdata_2)
      integer frow, irec, ii, i, naxis1, naxis2, rbf_unit, fits_unit
     &   , status, ierr
      REAL y, sy, XRRSL3, r1, r2
      EXTERNAL XRRSL3
      RECORD /rnewbuf_rec_2/newrec2
      RECORD /rnewbuf_rec_1/newrec1
c
c     Correction factors specific to type 2 files.
      r1 = 2.**float(newrec1.power1)
      r2 = 2.**float(newrec1.power2)
c
c     Fetch the full width of the binary table (naxis1; fixed in write_deftype0)
c     and the number of rows (naxis2; subject to change).
      CALL FTGKYJ(fits_unit, 'NAXIS1', naxis1, comment, status)
      CALL FTGKYJ(fits_unit, 'NAXIS2', naxis2, comment, status)
c
c     Initialization of fits row counter and rate buffer record #.
      frow = 0
      irec = 4
c 
      DO WHILE (irec.ne.-1)
c
c        Make more space in the binary table, as necessary.
         if( naxis2.le.frow ) then
            CALL FTDDEF(fits_unit, naxis1*(frow+128), status)
         endif
         IF ((ii.GT.num_rdata_2).or.(irec.eq.4)) THEN 
c           Read another record.
            ii = 1
            irec = irec + 1
            
            READ(rbf_unit,REC=irec,IOSTAT=ierr, ERR=990)
     &                (count1(i), err1(i), i=1, num_rdata_2)
         ENDIF
c         
         frow = frow + 1
c        Flag for end of file, gaps and overflow.
         if (err1(ii).eq.-2) then
c           End of data reached.  Fix the keyword for the number of rows.
            CALL FTMKYJ(fits_unit, 'NAXIS2', frow - 1, comment, status)
            irec = -1
            GOTO 10000
         elseif((err1(ii).eq.-1).or.(err1(ii).eq.-3)) then
c           It's either a gap or an overflow.
c           Write undefined elements to FITS.
            CALL FTPCLU(fits_unit, 1, frow, 1, 1,     status)
            CALL FTPCLU(fits_unit, 2, frow, 1, 1,     status)
         else
c           Prepare values
            y  = xrrsl3(count1(ii))*r1
            sy = xrrsl3(  err1(ii))*r2
c           Write values to FITS.
            CALL FTPCLE(fits_unit, 1, frow, 1, 1,  y, status)
            CALL FTPCLE(fits_unit, 2, frow, 1, 1, sy, status)
         endif
c        Increase bin counter
         ii = ii + 1
      ENDDO
c
      GOTO 10000
990   continue
      CALL FTMKYJ(fits_unit, 'NAXIS2', frow , comment, status)
      write(context,500) ierr, irec, frow 
500   format('ierr, irec, naxis', i3, 1x, i4, 1x, i4)
      call xwrite(context,10)
      write(context,600) count1(i),err1(i)
600   format(' count, err at the error', f17.5, 1x,f17.5)
      call xwrite(context, 10)
      call xwrite(' write_type2_data : Error reading data ', 10)
10000 CONTINUE
      RETURN
      END
