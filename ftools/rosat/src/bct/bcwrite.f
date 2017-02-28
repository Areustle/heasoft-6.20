      subroutine bcwrite (luo,frow,nrows,jdi,frci,jdo,frco,ftstat)

c WRITE Barycenter Corrections to output fits file.

c  I  luo      (i)  Lu of output FITS file.
c  I  frow     (i)  Current row in the table
c  I  nrows    (i)  Total number of rows in the extension
c  I  jdi      (i)  Input time, integer part, scc corrected but not bary
c  I  frci     (d)  Input time, real    part, scc corrected but not bary
c  I  jdo      (i)  Input time, integer part, scc corrected and     bary
c  I  frco     (d)  Input time, real    part, scc corrected and     bary

c Author:  eal  GSFC/HSTX  February, 1994

      integer nfield,luo,frow,nrows,pcount,ftstat,jdi,jdo
      parameter (nfield = 4)
      character(16) ttype(nfield), tform(nfield), tunit(nfield)
     &   ,extname,routine
      character(80) comm,errm
      double precision frci,frco
      data pcount /0/
      data extname,routine /'TIMEREF','bcwrite'/

c Column definitions.

      data ttype  /'TIMEI', 'TIMEF','REFI','REFBSOLS'/
      data tform /'J','D','J','D'/
      data tunit /4*'d'/

      if(ftstat.ne.0) return

c Special procedures on first and last calls.

      if(frow.eq.1) then

c Create and write the header for a new extension.

         CALL ftcrhd(luo,ftstat)

c Manadatory keywords and binary table data structure.

         CALL ftphbn(luo,nrows,nfield,ttype,tform,tunit,
     &               extname,pcount,ftstat)
         CALL ftbdef(luo,nfield,tform,pcount,nrows,ftstat)

c Timing keywords.

         comm = 'Time system'
         CALL ftpkys(luo,'TIMESYS' ,'JD',comm,ftstat)
         comm = 'Time unit in header keywords'
         CALL ftpkys(luo,'TIMEUNIT','d'  ,comm,ftstat)
         comm = 'Start time'
         CALL ftpkyj(luo,'TSTARTI' ,jdi ,   comm,ftstat)
         CALL ftpkyd(luo,'TSTARTF' ,frci,16,comm,ftstat)

      elseif(frow.eq.nrows) then

c Stop time keyword.
         comm = 'Stop time'
         CALL ftpkyj(luo,'TSTOPI' ,jdi ,   comm,ftstat)
         CALL ftpkyd(luo,'TSTOPF' ,frci,16,comm,ftstat)
c Redefine the number of rows in case a redundancy has occured.
         CALL ftmkyj(luo,'NAXIS2' ,nrows,'&',ftstat)

      endif

c Write times to table.

      CALL ftpclj(luo, 1 ,frow, 1, 1, jdi , ftstat)
      CALL ftpcld(luo, 2 ,frow, 1, 1, frci, ftstat)
      CALL ftpclj(luo, 3 ,frow, 1, 1, jdo , ftstat)
      CALL ftpcld(luo, 4 ,frow, 1, 1, frco, ftstat)

      IF(ftstat.ne.0) then
         write(errm,*) 'bcwrite: fitsio error',ftstat
         CALL xaerror(errm,1)
      endif

      return
      end
