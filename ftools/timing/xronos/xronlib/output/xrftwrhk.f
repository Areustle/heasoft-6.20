c
      SUBROUTINE xrftwrhk(cpro, oftype, luo, csumm, iflags, rflags, 
     &                    dxsta, dxstep, dtsta, dtsto, dtzero, dtnb, 
     &                    nintfm, nbint, dper, ndep, nexp, nstat,  
     &                    nfield, ftstat)
      implicit none

c WRite Header Keywords to a XRonos FiTs file.

c  I  cpro   (c) = xronos program name
c  I  oftype (i) = type output 1 = extension 
c  I  luo    (i) = output file unit number
c  I  csumm  (c) = string for title plot
c  I  iflags (i) = internal xronos flags describing, # data series, etc.
c  I  rflags (r) = internal xronos flags describing, # data series, etc.
c  I  dxsta  (d) = Start value for abscissa
c  I  dxstep (d) = Increment for abscissa
c  I  dtsta  (d) = Start time for interval
c  I  dtsto  (d) = Stop time for interval (modified in calling routine 
c                  if additional intervals are added)
c  I  tzero  (d)  Zero-point offset for TIME column
c  I  dtnb   (d) = Time increment (only relevant for light curves)
c  I  nintfm (i) = Number of interval per frame
c  I  nbint  (i) = number of newbin per interval
c  I  dper   (d) = folded period 
c  O  ndep   (i) = number of results columns (from iflags(8))
c  O  nexp   (i) = 1 for light curves (no exposure column otherwise)
c  O  nstat  (i) = number of statistical variable 
c  O  nfield (i) = total number of columns to write
c  O  ftstat (i) = FITSIO error status
c 
c Subroutines called: ftpkys, ftpkyj, ftpkyd, ftbdef, ftpdat, rmvlbk
c                     ftpkns, ftpknj, ftpknd

c Author: Eric Lufkin, HEASARC/GSFC, December 1993
c
c Input 
      INTEGER luo, iflags(*), oftype, nintfm, nbint
      REAL*4 rflags(*)
      DOUBLE PRECISION dtsta, dtsto, dtnb, dxsta, dxstep,ddsta,ddsto
      CHARACTER cpro*8, csumm*80
c Output variable 
      INTEGER ndep, nexp, nstat, nfield, ftstat

c Standard FTOOLS toolname common

      character(40) taskname
      common /task/ taskname

c Local variable
c     One TIME column, ten statistics columns.  Cmax is set plenty large.
      INTEGER i, n, nstacol, index, index1
      INTEGER ntime, cmax, pcount, icolstart
      PARAMETER(cmax=100, ntime=1, pcount=0)
      CHARACTER c12*2, comm*48, comm1*80
      character(16) ttype(cmax),tform(cmax),tunit(cmax),hduclas(3)
     &     ,ctyp1(cmax),cuni1(cmax),colnam(cmax), cbuf, timeunit
      character(32) colcom(cmax)
      INTEGER nvect, crpx1(cmax), nrows, i3, ratio, sum
      DOUBLE PRECISION  cdlt1(cmax),crvl1(cmax),dtzero,dper,dv
c
      DATA ctyp1, nrows /3*'SERIES',97*' ',1/
      DATA ttype,tform,tunit /100*' ',100*' ',100*' '/
      DATA hduclas,cuni1 /3*' ',100*' '/
      DATA cbuf, timeunit /' ',' '/
c 
c Abbreviations.
c programs name 
c  
c lcurve             lc
c efold              ef
c efsearch           es
c powspec            ps
c crosscor           cc
c autocorr           ac
c lcstas             ls
c listdata           ld
c timeskew           ts
c freqstat           ds (no supported)
c

      index1=0
      c12 = cpro(1:2)
c
c     hope this fix works (looks safe) LB
c iflags(10) number of input time series up to 4
      i3=iflags(10)
c
c sets number of row
c for oftype .eq. 1 one row at the time
c for oftype .ne. 1 number of row=iflags(13) no.of results line afte rebinning 
c
      IF(oftype.EQ.1) THEN
         nrows=1
      ELSE
         nrows=iflags(13)
      ENDIF
c
c Number of results columns + errors.
c iflags(8) number of results+error+1 (exposure array) columns
      ndep = iflags(8) - 1
c
c number of statistical column
c stat variable are 10 + 7 have error
      IF (c12.EQ.'lc'.OR.c12.EQ.'ef') THEN
         nstat=15*i3
      ELSE
         nstat=15
      ENDIF
c
c Write an exposure column only for light curves.
c maybe need to be changed and write in all the task if useful
      IF(c12.EQ.'lc'.OR.c12.EQ.'ps') THEN
         nexp = 1
      ELSE
         nexp = 0
      ENDIF
c
c Total number of columns, less statistics.
c ntime = X 
      nvect  = ntime + ndep + nexp
c
c one extra column for X axis in 
c if reb is not linear
c the Xaxis + error written also in file type 1 
c  oftype 1 if reb 
c  time    frequency error  power error
c  oftype 1 if no reb 
c  time    power error
c
c  oftype ne 1 always except efsearch
c  frequency error power error 
c
      IF(oftype.NE.1.and.c12.ne.'es') THEN
         nvect=nvect+1
      ELSE
         IF(rflags(1).LT.0)THEN
           IF(c12.EQ.'ps'.OR.c12.EQ.'ds') nvect=nvect+2
         ENDIF
      ENDIF
c
c Total number of columns.
      IF(oftype.EQ.1) THEN
         nfield  = nvect + nstat
      ELSE
         nfield = nvect
      ENDIF
c
c TFORM keyword values.  
c oftype 1 first column always start-time interval (dtsta r*8)
c the other results columns are r*4 1-D vectors. 
c
c     TIME.
      IF(oftype.EQ.1) THEN
         tform(1) = 'D'
         icolstart = 1
      ELSE
         tform(1) = 'D'
         if(c12.ne.'es') then
            tform(2) = 'D'
            icolstart = 2
         else
            icolstart=1
         endif
      ENDIF
c
c     Results. iflags(13) number of results after rebinning
      IF(oftype.EQ.1) THEN
         write(cbuf,101) iflags(13), 'E'
      ELSE
         cbuf = 'E'
      ENDIF
      CALL rmvlbk(cbuf)
      DO n = icolstart+1, nvect
         tform(n) = cbuf
      ENDDO
101   FORMAT(i8,a1)
c
c
c
      IF(oftype.EQ.1)THEN
c     Statistics.
         call xrwrsta(oftype, iflags(10), nstacol, colnam, colcom)
         DO i=1,nstacol
            ttype(nvect + i ) = colnam(i)
         ENDDO
         DO n = nvect + 1, nfield
            tform(n) = 'E'
         ENDDO
      ENDIF
c
c TTYPE and TUNIT keyword values.
c
c     TIME column.
      IF(oftype.EQ.1) THEN
         ttype(1) = 'TIME'
         tunit(1) = 'd'
      ELSE
         IF(c12.EQ.'lc')THEN
            ttype(1) = 'TIME'
            tunit(1) = 's'
         ELSEIF(c12.EQ.'ef')THEN 
            ttype(1) = 'PHASE'
         ELSEIF(c12.EQ.'es')THEN 
            ttype(1) = 'PERIOD'
            tunit(1) = 's'
         ELSEIF(c12.EQ.'ps')THEN 
            ttype(1) = 'FREQUENCY'
            tunit(1) = 'Hz'
         ELSEIF(c12.EQ.'ac'.OR.c12.EQ.'cc'.OR.c12.EQ.'ts')THEN 
            ttype(1) = 'DELAY'
            tunit(1) = 's'
         ENDIF   
         if(c12.NE.'es') then
            ttype(2) = 'XAX_E'
         endif
      ENDIF
c
c     Results columns.
c     LC & EF families.
c
      IF( ((c12.EQ.'lc').OR.(c12.EQ.'ef')).AND.(i3.GE.1) ) THEN
         index=icolstart
         DO i=1,i3
            index=index+1
            call ftkeyn('RATE',i,ttype(index),ftstat)
            tunit(index)='count/s'
            index=index+1 
            call ftkeyn('ERROR',i,ttype(index),ftstat)
            tunit(index)='count/s'
         ENDDO
         IF(i3.GE.2) THEN
            index=index+1
            ttype(index) = 'RATIO_12'
            index=index+1
            ttype(index) = 'RAT_E12'
            IF(i3.EQ.2)THEN 
               index=index+1
               ttype(index) = 'SUM12'
               tunit(index) = 'count/s'
               index=index+1
               ttype(index) = 'SUM_E'
               tunit(index) = 'count/s'
            ELSE
               IF(i3.EQ.3) THEN
                  ratio=23
                  sum=123
               ELSE
                  ratio=34
                  sum=1234
               ENDIF 
               index=index+1
               call ftkeyn('RATIO',ratio,ttype(index),ftstat)
               index=index+1
               call ftkeyn('RAT_E',ratio,ttype(index),ftstat)
               index=index+1
               call ftkeyn('SUM_',sum,ttype(index),ftstat)
               tunit(index) = 'count/s'
               index=index+1
               ttype(index) = 'SUM_E'
               tunit(index) = 'count/s'
            ENDIF
         ENDIF
         IF(oftype.EQ.1) THEN 
           DO n = icolstart+1, nvect - nexp
              IF(c12.EQ.'lc') THEN
                ctyp1(n) = 'TIME'
                crpx1(n) = 1
                cuni1(n) = 's'
                crvl1(n) = 0.d0
                cdlt1(n) = dtnb
              ELSE
                ctyp1(n) = 'PHASE'
                crpx1(n) = 1
                cuni1(n) = ' '
c Deiniftion: Start folded light curves from phase = 0.
                crvl1(n) = 0.d0
                cdlt1(n) = dxstep
              ENDIF
           ENDDO
         ENDIF
      ENDIF
c
c     EFS.
      IF(c12.EQ.'es') THEN
         DO i=1,i3
            index=icolstart+i
            call ftkeyn('CHISQRD',i,ttype(index),ftstat)
            index=index+1 
            call ftkeyn('ERROR',i,ttype(index),ftstat)
         ENDDO
         IF(oftype.EQ.1)THEN
            DO n = icolstart+1, nvect
               ctyp1(n) = 'PERIOD'
               crpx1(n) = 1
               cuni1(n) = 's'
               crvl1(n) = dxsta
               cdlt1(n) = dxstep
            ENDDO
         ENDIF
      ENDIF
c
c     PS family & DPS.
      IF ((c12.EQ.'ps').OR.(cpro(1:2).EQ.'dp')) THEN
         index1=icolstart
         IF(rflags(1).LT.0.and.oftype.eq.1) THEN
            index1=index1+1 
            ttype(index1) = 'FREQUENCY'
         ENDIF
         index1=index1+1
         ttype(index1) = 'POWER'
         index1=index1+1
         ttype(index1) = 'ERROR'
         IF(c12.eq.'dp') THEN
            index1=index1+1
            ttype(index1) = 'SMOOTHED'
            index1=index1+1
            ttype(index1) = 'SM_E'
         ENDIF  
         IF(oftype.EQ.1)THEN
            DO n = icolstart+1, index1
               ctyp1(n) = 'FREQUENCY'
               crpx1(n) = 1
               cuni1(n) = 'Hz'
               crvl1(n) = dxsta
               cdlt1(n) = dxstep
            ENDDO
         ENDIF
      ENDIF
c
c     AC & CC families & TSS.
      IF((c12.EQ.'ac').OR.(c12.EQ.'cc').OR.(c12.EQ.'ts')) THEN
         IF(c12.EQ.'ac') ttype(icolstart+1) = 'AUTO_CORR'
         IF(c12.EQ.'cc') ttype(icolstart+1) = 'CROSS_CORR'
         IF(c12.EQ.'ts') ttype(icolstart+1) = 'TIME_SKEW'
         ttype(icolstart+2) = 'ERROR'
         IF(oftype.EQ.1)THEN
           DO n = 2, 3
              ctyp1(n) = 'TIME LAG'
              crpx1(n) = 1
              cuni1(n) = ' s'
              crvl1(n) = dxsta
              cdlt1(n) = dxstep
           ENDDO
         ENDIF
      ENDIF
c
c     Exposure column.
      IF(nexp.eq.1) THEN
         IF(c12.EQ.'lc')THEN
            ttype(nvect) = 'FRACEXP'
         ELSEIF(c12.EQ.'ps')THEN
            ttype(nvect) = 'NUM_POINT'
         ENDIF 
      ENDIF

c >>> HDUCLAS_ keywords still under development. <<<
c     If the calling routine is lc1,
c     we produce a rate file that conforms with the xronos standard.

      IF(c12.EQ.'lc') THEN
         hduclas(1) = 'LIGHT CURVE'
         hduclas(2) = 'TOTAL'
         hduclas(3) = 'RATE'
      ELSE
         hduclas(1) = 'TEMPORALDATA'
         IF(c12.EQ.'ef') hduclas(2) = 'FOLDED DATA'
         IF(c12.EQ.'ps') hduclas(2) = 'POWER SPECTRA'
         IF(c12.EQ.'ds') hduclas(2) = 'POWER SPECTRA'
         IF(c12.EQ.'ac') hduclas(2) = 'AUTO CORRELATION'
         IF(c12.EQ.'cc') hduclas(2) = 'CROSS CORRELATION'
         IF(c12.EQ.'ts') hduclas(2) = 'TIME SKEWNESS'
         IF(c12.EQ.'es') hduclas(2) = 'FOLDING SEARCH'
         hduclas(3) = 'RESULTS'
      ENDIF
c
c Done preparing keyword values.
c
c Column specifiers, and other mandatory keywords.

      CALL ftphbn(luo,nrows,nfield,ttype,tform,tunit,
     &            hduclas(3),pcount,ftstat)
c
c Various informative keywords.
      CALL ftpdat(luo, ftstat)
      comm = 'Name of XRONOS program that created this file'
      CALL ftpkys(luo,'CREATOR ', taskname        ,comm,ftstat)
      CALL ftpkys(luo,'HDUCLASS','OGIP            ',' ',ftstat)
      CALL ftpkns(luo,'HDUCLAS' ,1,3,hduclas       ,'&',ftstat)
      CALL ftpkys(luo,'CONTENT ','XRONOS OUTPUT   ',' ',ftstat)
      CALL ftpkys(luo,'ORIGIN  ','HEASARC/GSFC    ',' ',ftstat)
      CALL ftpkys(luo,'OBJECT  ', csumm(1:16)      ,' ',ftstat)
      CALL ftpkys(luo,'TIMVERSN','OGIP/93-003     ',' ',ftstat)
c >>> If more than one input file, do we write out several of each
c     keyword (e.g., what if more than one instrument is involved)? <<<
c
c >>> Should csumm be prepared in the FITS reader as in the rbf reader? <<<
c More column specifiers, for vector and array columns.
      IF(oftype.EQ.1)THEN
        CALL ftpkns(luo,'1CTYP',2,ndep,ctyp1(2),        '&',ftstat)
        CALL ftpknj(luo,'1CRPX',2,ndep,crpx1(2),        '&',ftstat)
        CALL ftpkns(luo,'1CUNI',2,ndep,cuni1(2),        '&',ftstat)
        CALL ftpknd(luo,'1CRVL',2,ndep,crvl1(2),  16,   '&',ftstat)
        CALL ftpknd(luo,'1CDLT',2,ndep,cdlt1(2),  16,   '&',ftstat)
      ENDIF
c 
c Timing keywords. Correct for half bin integration (Oct 1998)
      timeunit = 'd'
      ddsta=dtsta - dtnb/86400.D0/2.D0
c      write(*,*)'dtsto', dtsto, dtnb
      ddsto=dtsto + dtnb/86400.D0/2.D0
c      write(*,*)'ddsto',ddsto
      CALL xrftptky(luo,ddsta,ddsto,timeunit,dtzero,ftstat)
c     TIMESYS currently broken, should be fixed RSN
c      CALL ftpkys(luo,'TIMESYS ', 'TJD'            ,' ',ftstat)
c            123456789012345678901234567890123456789012345678901234567
      comm1='TIMESYS keyword is not currently set. If the input '//
     &      'lightcurve '
      call ftpcom(luo,comm1,ftstat)
      comm1='contains the header keyword MJDREF, the time in the'//
     &      ' xronos '
      call ftpcom(luo,comm1,ftstat)
      comm1= ' output is in TJD (JD-2440000.5)'
      call ftpcom(luo,comm1,ftstat)
c      call ftpcom(luo,'TIMESYS keyword is currently broken',ftstat)
      if (oftype.eq.1) then
         if(nintfm.gt.0) then
            dv=dble(nintfm*nbint)*dtnb/86400.d0
         else
            dv=dble(nbint)*dtnb/86400.d0
         endif
         call ftpkyd(luo,'TIMEDEL ', dv, 16, ' ',ftstat)
      else
         if (cpro(1:2).eq.'lc') 
     &        CALL ftpkyd(luo,'TIMEDEL ', dtnb/86400.d0,16 ,' ',ftstat)
      endif
c
c
      comm=' Period used to fold the data in seconds' 
      if(c12.eq.'ef')then
         call ftpkyd(luo,'PERIOD ', dper, 16, comm ,ftstat)
      endif 
c
c FiTs Binary table data structure DEFinition.
C fitsio now does this automatically
c      CALL ftbdef(luo,nfield,tform,pcount,nrows,ftstat)

      RETURN
      END
