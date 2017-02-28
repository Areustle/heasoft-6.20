C*******************************************************************
      SUBROUTINE write_type0_data(rbf_unit, fits_unit,newrec1,
     &                     newrec2, timezero, status)
C*******************************************************************
      include 'xrrbstr.h'
c
      character(80)  context
      character(60)  comment
      integer rbf_unit, fits_unit,irec, frow
      integer ii, i, naxis1, naxis2 ,status  ,time(num_rdata_0)
      INTEGER*2    nday(num_rdata_0),  dt(num_rdata_0)
      REAL y, sy, count(num_rdata_0), err(num_rdata_0), deadc, norm
      integer*4 key(5)
      INTEGER shfkey
      double precision dyear, shfdyr
      double precision dtime, tmtest, timezero
      RECORD /rnewbuf_rec_1/newrec1
      RECORD /rnewbuf_rec_2/newrec2
      RECORD /rnewbuf_le/newle
      RECORD /rnewbuf_me/newme
      RECORD /rnewbuf_gs/newgs
c
c retrive year 
         dyear=shfdyr(newrec1.utctime1, status)
         key(1)=dyear
c     Deadtime normalization (detector-dependent).
      if(newrec1.EXPT.le.2) then
         newle = newrec2.newle
         norm = 1./(float(newle.norm2)/3000.*10.0**newle.norm1)
      elseif(newrec1.EXPT.eq.3) then
         newme = newrec2.newme
         norm = 1.
      elseif(newrec1.EXPT.eq.4) then
         newgs = newrec2.newgs
         norm = 1.
      else
         norm = 1.
      endif
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
         IF ((ii.GT.num_rdata_0).or.(irec.eq.4)) THEN 
c           Read another record.
            ii = 1
            irec = irec + 1
            READ(rbf_unit,REC=irec, ERR=990)
     &                 (nday(i), time(i), dt(i), count(i), err(i),
     &                  i=1,num_rdata_0)
         ENDIF
c         
         frow = frow + 1
c        Flag for end of file, gaps and overflow.
         if (nday(ii).eq.-1) then
c           End of data reached.  Fix the keyword for the number of rows.
            CALL FTMKYJ(fits_unit, 'NAXIS2', frow - 1, comment, status)
            irec = -1
            GOTO 10000
c         elseif(err(ii).eq.-0.99) then
         elseif(err(ii).eq.-1.00) then
c           It's a gap.  Write undefined elements to FITS.
            CALL FTPCLU(fits_unit, 3, frow, 1, 1,       status)
            CALL FTPCLU(fits_unit, 4, frow, 1, 1,       status)
         else
c           Prepare values and write to FITS.
            y  = count(ii)
            sy =   err(ii)
            CALL FTPCLE(fits_unit, 3, frow, 1, 1,     y, status)
            CALL FTPCLE(fits_unit, 4, frow, 1, 1,    sy, status)
         endif
c        Prepare time.  
c         dtime = (dble(dfloat(nday(ii)))+
c     &            dble(time(ii))/86400000.d0)*86400.d0
         key(2)=nday(ii)
c convert y day hh min sec into shf
         call timak(key, shfkey)
c         write(*,*)'shfkey',shfkey
         dtime=dfloat(shfkey)+dfloat(time(ii))/1000.d0
c         write(*,*)'dtime',dtime
         tmtest=dtime-timezero
c         write(*,*)tmtest
c        Prepare dead time.
         deadc = (1. - float(dt(ii))/10000.)*norm
c        Write to FITS.
c         CALL FTPCLD(fits_unit, 1, frow, 1, 1, dtime, status)
         CALL FTPCLD(fits_unit, 1, frow, 1, 1, tmtest, status)
         CALL FTPCLE(fits_unit, 2, frow, 1, 1, deadc, status)
c        Increase bin counter
         ii = ii + 1
      ENDDO

      GOTO 10000
990   continue
      write(context,500) irec, frow
500   format(' irec, naxis ',  i4, 1x, i4)
      call xwrite(context,10)
      write(context,600) count(i), err(i)
600   format(' count, err at the error', f17.5, 1x,f17.5)
      call xwrite(' write_type0_data: Error reading data', 10)
10000 CONTINUE
      RETURN
      END

