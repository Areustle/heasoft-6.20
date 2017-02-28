      subroutine bcread(lui,itype,extn,icols,frow,nrows,jd,frc,reae)

c READ current time (jd) and satellite position (reae) from input orbit file.

c This routine simply reads from the input file according to the value of
c itype.  Times are returned in JD and the satellite vector is in meters.

c  I  lui     (i)  Lu of input file.
c  I  itype   (i)  = 11 for ROSAT old-type; = 12 for ROSAT new-type;
c                  = 13 for ROSAT German file
c  I  extn    (i)  Number of current FITS extension
c  I  icols   (i)  Array of relevant column numbers
c  I  frow    (i)  Current row in the table
c  I  nrows   (i)  Total number of rows in the extension.
c  O  jd      (i)  Julian day
c  O  frc     (d)  Fractional part of JD (between 0 and 1)
c  O  reae    (d)  Satellite position vector (meters)

c subroutines called:  ftmahd,ftgcvj,rmvlbk,ar_jdc,ftgcvd

c Author:  eal  GSFC/HSTX,  February, 1994
c
c Modification: Peter D Wilson, February 9, 1998:
c                   ftgcvd called with nulld now instead of nullj
c               Ning Gan,  June 1998
c		    Converted the two digit year read from the old-style fits 
C		    file to 4 digit year to be compatible with the ar_jdcn. 
c                   Replaced the ar_jdc with ar_jdcn.
c
      LOGICAL*4 anynul
      character(16) sbuf
      character(80) comm,errm
      INTEGER*4 jd,ftstat,lui,extn,frow,ierr,nrows,icols(*)
     &   ,xtend,nulvj,ibuf,month,day,year,seconds,itype
      DOUBLE PRECISION frc,reae(3),nulvd,jdd
      data nulvd,nulvj /-1.2d34, -99/

      ftstat=0

c Move to the chosen extension header in the input file, and get the number
c of rows, if this is the first row.

      IF(frow.eq.1) THEN
         CALL ftmahd(lui,extn,xtend,ftstat)
         CALL ftgkyj(lui,'NAXIS2',nrows,comm,ftstat)
      ENDIF

      IF(itype.eq.11) THEN

c Date.

         CALL ftgcvj(lui,icols(1),frow,1,1,nulvj,ibuf   ,anynul,ftstat)
         write(sbuf,'(i10)') ibuf
         CALL rmvlbk(sbuf)
         read(sbuf,'(i2.2,i2.2,i2.2)') year,month,day
         year = 1900 + year
c Convert to Julian Day.  Ar_jdc returns a half-integer day (value at
c midnight), so we strip off the half and add it to the fractional part.

         CALL ar_jdcn(day,month,year,jdd,ierr)
         jd = aint(jdd)

c Time.

         CALL ftgcvj(lui,icols(2),frow,1,1,nulvj,seconds,anynul,ftstat)

c Extra read for the special case of old-type U.S. file:

         CALL ftgcvj(lui,icols(3),frow,1,1,nulvj,ibuf   ,anynul,ftstat)

c Add on ibuf fractional seconds, plus the half day from jd.
c and convert total seconds to fractional day.

         frc = (dble(seconds) + dble(ibuf)/1.d4)/86400.d0 + .5d0

      ELSEIF(itype.eq.12) THEN

c Modified Julian day

         CALL ftgcvj(lui,icols(1),frow,1,1,nulvj,ibuf,anynul,ftstat)
         CALL ftgcvd(lui,icols(2),frow,1,1,nulvd,frc ,anynul,ftstat)

c --> Julian Day.

         frc = frc + 0.5
         jd = ibuf + 2400000

      ELSEIF(itype.eq.13) THEN

c Date.  
 
         CALL ftgcvj(lui,icols(1),frow,1,1,nulvj,ibuf,anynul,ftstat)
         write(sbuf,'(i10)') ibuf
         CALL rmvlbk(sbuf)
         read(sbuf,'(i2.2,i2.2,i2.2)') year,month,day
         year = 1900 + year
 
c Convert to Julian Day.  Ar_jdc returns a half-integer day (value at
c midnight), so we strip off the half and add it to the fractional part.
 
         CALL ar_jdcn(day,month,year,jdd,ierr)
         jd = aint(jdd)

c Time.  
 
         CALL ftgcvd(lui,icols(2),frow,1,1,nulvd,frc,anynul,ftstat)
 
c Add on the half day from jd and convert total seconds to fractional day. 
 
         frc = frc/86400.d0 + .5d0
         
      ENDIF
    
c Adjust for fractions greater than one.

      IF ( frc.GE.1.0D0 ) THEN
         jd = jd + 1
         frc = frc - 1.D0
      ENDIF
 
c Satellite position.
 
      CALL ftgcvd(lui,icols(4),frow,1,1,nulvd,reae(1),anynul,ftstat)
      CALL ftgcvd(lui,icols(5),frow,1,1,nulvd,reae(2),anynul,ftstat)
      CALL ftgcvd(lui,icols(6),frow,1,1,nulvd,reae(3),anynul,ftstat)

      IF(ftstat.ne.0) THEN
         write(errm,*) 'bcread: fitsio error ',ftstat
         CALL xaerror(errm,1)
      ENDIF

      return
      end
