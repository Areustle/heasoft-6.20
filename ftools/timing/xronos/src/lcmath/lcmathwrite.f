      subroutine lcmathwrite(lui,luo,frow,trow,ynul,ivecti,ivecto
     &                   ,ynet,synet,ftstat)

c WRITE BacKGround-subtracted times, rates, and anything else to output.

c This routine writes one row of data to the output file.
c The net rate and error as calculated in the lcmath program are
c written in 1E (single value floating point real) format.
c Any other columns that are saved from the intput file are copied directly
c to their respective columns in the output file, in 1E format, except
c the time, deadtime and integration time columns, which are listed in 
c 1D format.

c  I  lui      (i)  Lu of input FITS file
c  I  luo      (i)  Lu of output FITS file.
c  I  frow     (i)  Current row in the table
c  I  trow     (i)  Current time index in the packet
c  I  ynul     (l)  = .true. if input Y-value is a gap
c  I  ivecti   (i)  column numbers in input file
c  I  ivecto   (i)  column numbers in output file
c  I  ynet     (r)  Net rate (counts/sec)
c  I  synet    (r)  Net rate error (counts/sec)
c  I  ftstat   (i)  fitsio error code

c Author:  eal  GSFC/HSTX  March, 1994

      include '../../include/io.inc'
      logical anynul,ynul
      integer lui,luo,frow,ftstat,ivecti(*),ivecto(*),trow
      real nulve,eval,ynet,synet
      double precision nulvd,dval
      data nulve,nulvd /-1.2e34, -1.2d34/
      parameter (subname = 'lcmathwrite:')

      if(ftstat.ne.0) return

c TIME column, if present.

      if(ivecti(1).gt.0) then
         anynul = .false.
         CALL ftgcvd(lui,ivecti(1),frow,1,1,nulvd,dval,anynul,ftstat)
         if(anynul) then
            CALL ftpclu(luo,ivecto(1),frow,1,1,ftstat)
         else
            CALL ftpcld(luo,ivecto(1),frow,1,1,dval,ftstat)
         endif
      endif

c RATE and ERROR columns.

      if(ynul) then
         CALL ftpclu(luo,ivecto(2),frow,trow,1,ftstat) 
         CALL ftpclu(luo,ivecto(3),frow,trow,1,ftstat) 
      else
         CALL ftpcle(luo,ivecto(2),frow,trow,1,ynet,ftstat)
         CALL ftpcle(luo,ivecto(3),frow,trow,1,synet,ftstat)
      endif

c DEADTIME column, if present.

      if(ivecti(4).gt.0) then 
         anynul = .false.   
         CALL ftgcvd(lui,ivecti(4),frow,1,1,nulvd,dval,anynul,ftstat) 
         if(anynul) then 
            CALL ftpclu(luo,ivecto(4),frow,1,1,ftstat) 
         else 
            CALL ftpcld(luo,ivecto(4),frow,1,1,dval,ftstat) 
         endif 
      endif

c DELTA-TIME column, if present.

      if(ivecti(5).gt.0) then 
         anynul = .false.   
         CALL ftgcvd(lui,ivecti(5),frow,1,1,nulvd,dval,anynul,ftstat) 
         if(anynul) then 
            CALL ftpclu(luo,ivecto(5),frow,1,1,ftstat) 
         else 
            CALL ftpcld(luo,ivecto(5),frow,1,1,dval,ftstat) 
         endif 
      endif

c EXPOSURE column, if present.

      if(ivecti(6).gt.0) then 
         anynul = .false.   
         CALL ftgcve(lui,ivecti(6),frow,1,1,nulve,eval,anynul,ftstat) 
         if(anynul) then 
            CALL ftpclu(luo,ivecto(6),frow,1,1,ftstat) 
         else 
            CALL ftpcle(luo,ivecto(6),frow,1,1,eval,ftstat) 
         endif 
      endif

c Error trap.

      IF(ftstat.ne.0) THEN
         write(errm,*) ' lcmathwrite: fitsio error ',ftstat
         errm = subname//' '//errm
         CALL xaerror(errm,1)
      ENDIF

      return
      end
