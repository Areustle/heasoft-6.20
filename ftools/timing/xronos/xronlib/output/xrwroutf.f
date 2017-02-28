c
      subroutine xrwroutf(cpro, cfilo, oftype, iflags, rflags, csumm, 
     &     rrtsta, drtsta, indx, dtnb, nkount, nbint, nintfm, dxsta, 
     &     dxstep, yr, syr, expr, xr, sxr, kmax, jmax, doo, chival, 
     &     chierr, dpera, nper, ftstat)
      implicit none
c
c This routine records all frames (or intervals, or bins) in ONE FILE.
c Two possible output: 1-all intervals in one extension with the results
c stored as an arry per row or 2-one interval per extension  
c the variable oftype distinguish between the two outputs
c Each extension contains results from one frame.  
c On the first call, the output fits file is created, an extension with
c one frame is written to it, and the file is closed.
c On subsequent calls the file is reopened and a new extension is added.
c
c The total # of fields (columns) is:
c         One scalar for the TIME of the first newbin
c       + The # of dependent variables with errors = iflags(8) - 1
c       + One exposure column           / nexp   = 1
c
c >>> Other modifications do to:
c     1) Set the column lables TTYPEnnn so they make sense. 
c <<<
c
c     I   cpro = prompt of main (c*4)
c     I   cfilo = output filename (c*160)
c     I   oftype = output file type
c     I   iflags = int*4 flags for plots, file type, analysis type (i*4x20)
c     I   rflags = real*4 flags (r*4x20)
c     I   csumm = summary from infiles (c*80)
c     I   rrtsta = interval. stat. param. (R*4x20) (see xrgetint) (r*4x20)
c     I   drtsta = interval. stat. param. (R*8x20) (see xrgetint) (r*8x5)
c     I   indx = indexes of series containing earliest and latest good newbin
c     I   dtnb = newbin integration time (secs) (r*8)
c     I   nbint = no. of newbins/intv (i*4)
c     I   nintfm = Number of intervals per frame
c     I   dxsta = start-x for result independent variable
c     I   dxstep = x-step for result independent variable
c     I   yr = result values 
c     I   syr = result errors
c     I   expr = result exposure 
c     I   xr = result x-axis
c     I   sxr = result x-axis "error"
c     I   kmax = index of best period in period search program
c     I   jmax = size of xr array
c     I   doo = unit convertion factor
c     I   chival =  array of chi**2 values for period search
c     I   chierr =  array of chi**2 error values for period search
c     I   dpera = array of the corresponding periods
c     I   nper = number of periods
c     O   ftstat = error flags
c
c Subroutines called: ftinit, ftopen, ftmahd, ftmkyj, ftphpr, ftpdef, 
c                     ftcrhd, ftphbn, ftpkys, getdat, ftpkyj, ftpkyd, 
c                     ftbdef, xrxrebin, ftpcle, ftclos, xrftpchk
c
c Author: Eric Lufkin, HEASARC/GSFC, August 1993
c             Based on subroutine xrwroutq by L. Stella and L. Angelini.
c Revised: November 1993 to set each frame in one extension, rather than one
c                        row.
c
c Input variable
      include '../include/io.inc'
       INTEGER oftype, iflags(20), indx(2), nbint, nkount, kmax(*),
     &     jmax, nper
       REAL*4 rflags(20), rrtsta(20,*) , chival(nper,*), chierr(nper,*), 
     &        yr(nkount, *), syr(nkount,*), expr(nkount)
       real*8 xr(jmax), sxr(jmax) ,ddsto
       DOUBLE PRECISION drtsta(20,*), dtnb, dxsta(*), dxstep(*),
     &      dpera(*), doo, dper
       CHARACTER cpro*8, cfilo*160, csumm*80  
c
c Output variable
c
      INTEGER ftstat
c Local variable
c 
      INTEGER istart
      CHARACTER comm*48,context*80
      INTEGER cmax, index
      PARAMETER (cmax=100) 
      character(16) colnam(cmax)
      character(32) colcom(cmax)
      INTEGER luo, icolstart,interval, nmaxa, nintfm, i, k, j, ireb,  
     &        n, iclndx(8), ndep, nexp, nstat, ibuf, felem, nelem,
     &        nfield, colnum, frow, block,  
     &        rwmode, nstacol, ihdu, idum
      DOUBLE PRECISION dval, dtzero, dtime
      real*8 dco(0:4)
c
      DATA block /2880/
      DATA frow /0/
      DATA interval /0/
      DATA dco/1.D0, 1.D0, 3600.D0, 86400.D0, 1.D0/ 
c
      SAVE
      parameter (subname = 'xrwroutf:')

c    set integer flag for lcurve time units flag = 4
      if(iflags(2).eq.4) then
         istart = 7
      else
         istart = 1
         
      endif
c
c     set initial values
      IF (ftstat.NE.0) RETURN
      interval = interval + 1
      IF(oftype.EQ.1) THEN
         frow = interval
      ELSE
         frow = 1
      ENDIF
c
c Put X-axis back into seconds!
      if (iflags(2).gt.0.and.iflags(2).ne.4) then
         do j = 1, jmax
            xr(j) = xr(j) * dco(iflags(2))
            sxr(j) = sxr(j) * dco(iflags(2))
            xr(j) = xr(j) - doo *86400.D0
         enddo
      endif
c
c Open output FITS file.
c Set file unit number.
      CALL getlun(luo)
c
      IF(interval.EQ.1) THEN

c We are writing the first, and possibly only, frame or interval.
c Open new file. 
         context= 'Writing output file: '//cfilo
         CALL xwrite(context,10)
         CALL xrftinit(luo,cfilo,ftstat)
         IF (ftstat.NE.0) THEN
            context= 'Couldn''t open output file'//cfilo
            errm = subname//' '//context
            call xaerror(errm, 5)
            goto 999
         ENDIF

c Create a new binary table extension.

         CALL ftcrhd(luo,ftstat)

c Write the bulk of the extension header keywords.
         dtzero = drtsta(istart,indx(1))
         dper=dpera(1)
c         write(*,*)'drtsta(4,indx(2))',drtsta(4,indx(2))
         CALL xrftwrhk(cpro, oftype, luo, csumm, iflags, rflags, 
     &                 dxsta(1), dxstep(1), drtsta(1,indx(1)), 
     &                 drtsta(4,indx(2)), dtzero, dtnb, nintfm, 
     &                 nbint, dper, ndep, nexp, nstat,  
     &                 nfield, ftstat)

         IF(ftstat.NE.0) THEN
            call xaerror(
     $           'Trouble initializing data table in outfile', 5)
            goto 999
         ENDIF
c            
c determinate TIMEZERO.
      ELSE
c
c We are writing an additional frame or interval.
c Open and append to the previously existing file.
         context=' '
         CALL xwrite(context,10)
         context= 'Frame ready : Writing in output : '//cfilo
         CALL xwrite(context,10)

         rwmode=1
         CALL ftopen(luo,cfilo,rwmode,block,ftstat)
c
c Move to the binary extension header.
         if(oftype.eq.1) then
            ihdu = 2
         else
            ihdu = interval
         endif
         CALL ftmahd(luo,ihdu,idum,ftstat)
         IF(oftype.EQ.1) THEN
c
c"All in one extension" array in each row
c Modify the NAXIS2 keyword to specify one more row.
c  
            CALL ftmkyj(luo,'NAXIS2',frow,'&',ftstat)
c            
c Redefine the size of the extension
c            
            CALL ftrdef(luo,ftstat)
c            
c     Modify the TSTOP keyword. Correct for half integration bin 
c            write(*,*)' drtsta(4,indx(2))', drtsta(4,indx(2))
            ddsto= drtsta(4,indx(2)) + dtnb/86400.D0/2.D0
c            write(*,*)'ddsto',ddsto
            i = int(ddsto)
            dval =  ddsto - dble(i) 
c            write(*,*)'i,dval',i,dval
            comm = 'Stop time for this extension'
            CALL xrftpdky(
     &             luo,'TSTOPI','TSTOPF','TSTOP',i,dval,comm,ftstat)
            IF(ftstat.NE.0) THEN
               CALL xaerror(
     $              'Trouble adding to data table in outfile', 5)
               GOTO 999
            ENDIF

         ELSE
c
c "One interval per extension"
c Create a new binary table extension.
            CALL ftcrhd(luo,ftstat)
c
c     Write the bulk of the extension header keywords.
            CALL xrftwrhk(cpro, oftype, luo, csumm, iflags, rflags,
     &                    dxsta(1), dxstep(1), drtsta(1,indx(1)),
     &                    drtsta(4,indx(2)), dtzero, dtnb, nintfm, 
     &                    nbint, dper, ndep, nexp, nstat, nfield, 
     &                    ftstat)
            IF(ftstat.NE.0) THEN
               CALL xaerror(
     $              'Trouble initializing new data table in outfile',5)
               GOTO 999
            ENDIF

         ENDIF
      ENDIF
c      
c     Set the number of vector elements to be written.
      nelem = iflags(13)
      felem = 1
c
c
c One interval per extension first column give the start time of the 
c interval 
      IF(oftype.EQ.1) THEN
c TIME column.
         colnum = 1
         dtime = drtsta(1,indx(1)) - dtzero
         CALL ftpcld(luo,colnum,frow,1,1,dtime,ftstat)
c
c if rflag negative frequencuy are log space. The ctype coordinates are 
c no good to describe the axis in this case
c
         IF(cpro(1:2).EQ.'ps'.AND.rflags(1).LT.0)THEN
            colnum = 2
            call ftpcld(luo,colnum,frow,felem,nelem,xr,ftstat)
            colnum = 3
            call ftpcld(luo,colnum,frow,felem,nelem,sxr,ftstat)
         ENDIF
      ELSE
c
c X column
c extension based X-axis is the 'independent variable'
c X column in folding search is dper 
         IF(cpro(1:2).EQ.'es')THEN
            colnum = 1
            call ftpcld(luo,colnum,frow,felem,nelem,dpera,ftstat)
         ELSE
            colnum = 1
            call ftpcld(luo,colnum,frow,felem,nelem,xr,ftstat)
            colnum = 2
            call ftpcld(luo,colnum,frow,felem,nelem,sxr,ftstat)
         ENDIF
      ENDIF
      
      icolstart=colnum
c
c     Dependent variables loop.
      IF(cpro(1:2).EQ.'es')THEN
         DO n = 1, ndep/2 
            colnum = colnum + 1
            CALL ftpcle(luo,colnum,frow,felem,nelem,chival,ftstat)
            colnum = colnum + 1
            CALL ftpcle(luo,colnum,frow,felem,nelem,chierr,ftstat)
         ENDDO 
      ELSE 
        DO n = 1, ndep/2
           colnum = colnum + 1
           CALL ftpcle(luo,colnum,frow,felem,nelem,yr(1,n),ftstat)
           colnum = colnum + 1
           CALL ftpcle(luo,colnum,frow,felem,nelem,syr(1,n),ftstat)
        ENDDO
      ENDIF
      
c      
c     Exposure.
      IF(nexp.EQ.1) THEN
         colnum = colnum+1
         CALL ftpcle(luo,colnum,frow,felem,nelem,expr,ftstat)
      ENDIF
c      
      IF(ftstat.NE.0) THEN
         errm = subname//' '//'Trouble writing data columns'
         call xaerror(errm, 5)
         goto 999
      ENDIF

      IF(cpro(1:2).NE.'es')THEN

         colnum=icolstart
c     
c     Check for gaps.  Loops are structured for maximum FITSIO efficiency.
         DO n = 1, ndep/2
            colnum=colnum+1
            DO i = 1, nelem
               IF(yr(i,n) .eq. -1.2e-34) then
                  if(oftype.eq.1) then
                     felem=i
                  else
                     frow=i
                  endif
                  CALL ftpclu(luo,colnum,frow,felem,1,ftstat)
               endif
            ENDDO
            colnum=colnum+1
            DO i = 1, nelem
               IF(syr(i,n) .eq. -1.2e-34)  then
                  if(oftype.eq.1) then
                     felem=i
                  else
                     frow=i
                  endif
                  CALL ftpclu(luo,colnum,frow,felem,1,ftstat)
               endif
            ENDDO
         ENDDO
         if(nexp.eq.1) then
            colnum=colnum+1
            DO i = 1, nelem
               IF(expr(i) .eq. -1.2e-34) then
                  if(oftype.eq.1) then
                     felem=i
                  else
                     frow=i
                  endif
                  CALL ftpclu(luo,colnum,frow,felem,1,ftstat)
               endif
            ENDDO
         endif
c     
c     
         IF(ftstat.NE.0) THEN
            errm = subname//' '//'Trouble writing undefined values.'
            CALL xaerror(errm, 5)
            GOTO 999
         ENDIF
         
      endif
c
c
      IF(oftype.EQ.1) THEN
c
c     Done with vector loop.  Write statistics.
c write from rrtsta(1,n)  to  rrtsta(11,n)
c            rrtsta(13,n) and  rrtsta(14,n)
c            rrtsta(19,n) and  rrtsta(20,n)
         index=0
         DO j=1,iflags(10)
            DO i = 1, 20
               IF 
     $   (i.NE.12.AND.i.NE.15.AND.i.NE.16.AND.i.NE.17.AND.i.NE.18) THEN
                  index=index+1
                  n = nfield - nstat + index
                  CALL ftpcle(luo,n,frow,1,1,rrtsta(i,j),ftstat)
               ENDIF
            ENDDO
         ENDDO 
      ELSE
         CALL xrwrsta(oftype, iflags(10), nstacol, colnam,colcom)
         index=0
         DO j=1,iflags(10)
            DO n=1,11
               index=index+1
               CALL ftpkye
     $              (luo,colnam(index),rrtsta(n,j),8,
     $              colcom(index),ftstat)
            ENDDO
               index=index+1
               CALL ftpkye(luo,colnam(index),rrtsta(13,j),
     &                     8,colcom(index),ftstat)
               index=index+1
               CALL ftpkye(luo,colnam(index),rrtsta(14,j),
     &                     8,colcom(index),ftstat)
               index=index+1
               CALL ftpkye(luo,colnam(index),rrtsta(19,j),
     &                     8,colcom(index),ftstat)
               index=index+1
               CALL ftpkye(luo,colnam(index),rrtsta(20,j),
     &                     8,colcom(index),ftstat)
         ENDDO
      ENDIF
c
      IF(ftstat.NE.0) THEN
         errm = subname//' '//'Trouble writing statistics values.'
         CALL xaerror(errm, 5)
         GOTO 999
      ENDIF
c         
c     All done with this call.  Close the file and return.
c
      CALL ftclos(luo,ftstat)
      CALL frelun(luo)
 999  CONTINUE
      RETURN
      END
