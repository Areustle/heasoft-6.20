
      SUBROUTINE xrgetexp(nfilt, cfile, dtnb, nbint,
     &                    ipf, rpf, dpf, twia, twio, pwi,
     &                    pwia, pwio, fwia, fwio, ewia, ewio, nwi, 
     &                    iflags, rflags, nfilma, yi, syi, expi,
     &                    intsta, rntsta, dntsta)
c
c     I   nfilt = total no. of files
c     I   cfile = array of infile replies (infile+options)
c     I   dtnb = duration of a "newbin" in secs
c     I   nbint = no. of newbins per interval
c     I   ipf,rfp,dpf = integer*4, real*4, real*8 parameter file options
c     I   twia,twio,pwi,pwia,pwio,fwia,fwio,ewia,ewio,nwi = windows
c     I   iflags = int*4 flags for plots, file type, analysis type
c     I   rflags = real*4 flags for plots, file type, analysis type
c     I   nfilma = max. no. of input files/series
c     I   yi = cts/s in each new bin of interval (-1.2e34 or less means gap)
c     I   syi = error on cts/s in each new bin of interval
c     I/O expi = exposure in secs in each newbin
c     I   intsta = statistics of this interval (integer*4) (20)
c     I   rntsta =     "      "   "      "     (real*4)    (10)
c     I   dntsta =     "      "   "      "     (real*8)    (5)
c
c
c
c     xrrbrdpon, xrqdprdpon, 
c     xrapplopt,xrask,xrfilext,xrdecopt,xrtyint,xrtrere
c     xrdhms,xrinerr,xrappltwi,xrapplpwi,xrapplfwi,xrapplewi =subroutines used
c     xrrunmean =function used
c
c
      include '../include/io.inc'
      INTEGER*4 iflags(20), nfilma
c     One FITS block contains 180 8-byte pairs.
c      character(37) cfile(nfilma,iflags(10))
c      character(37) cfile(nfilma, *)
      LOGICAL dnear
      character(160) cfile(nfilma, *)
      character(160) cfil(8)
      character(10) copt(15, 8)
      CHARACTER cext*15, cpro*1, cque*1
c      integer*4 iv,lut,lch,lul,ierx,ierr,k,nbint,nfilt(iflags(10)),
c     &          intsta(20,iflags(10)),nbindex(8),m,iendm(8),
c      INTEGER*4 iv, lut, lcd, lch, lul, ierx, ierr, k, nbint, nfilt(*),
      INTEGER*4 iv, lcd, lch, ierx, ierr, k, nbint, nfilt(*),
     &          irec(8), novfl(8), nogap(8), nfil(8), iopt(15, 8),
     &     mopt(15, 8), nwi(*), ipf(*), intsta(20, *),
     $     nbindex(8),
     &     m, iendm(8), lui(8), iyrsta(8),igoodexp(8),
     &     nbindexma(8), nbindexmi(8), indbin(8), ivmi, ivma,
     &     nch,lenact,lup, ftstat, nbad(8)
      REAL*4 rv, pwia(*), pwio(*), fwia(9, *), fwio(9, *), ewia(*),
     &     ewio(*), yi(nbint,*), syi(nbint,*),
     $     expi(nbint,*),
     &     rpf(*), rntsta(20, *), y(8), sy(8), expos(8),
     &     rflags(10)
      REAL*8 dv, dtnb, twia(*), twio(*), pwi(*), dpf(*), 
     $     dntsta(20, *),
     &     dopt(15, 8), dtime(8), dtint(8), dvmi, dvma, dvleft,
     &     dvright, dbtime(8)
      EXTERNAL dnear
      SAVE
c
c set start values
      DATA ierx, ierr, nfil, irec, nbindex /0, 0, 8*0, 8*0, 8*0/
      DATA iendm, nbindexma, nbindexmi/8*0, 8*0, 8*0/
c set d/f =1
      DATA igoodexp/8*1/
      parameter (subname = 'xrgetexp:')
c
c
c Start new interval (start)
c
c     set arrays to default values (in intv.)
c
      DO m = 1, iflags(10)
         indbin(m)=0
c
         DO k = 1, nbint
c gap values
            expi(k, m) = -1.2E34
         ENDDO
      ENDDO
c
c Start new interval (end)
c
c
c Big loop for data point reading, checking etc. (start)
c
c start from first series
      m = 1
c
c
c
      GOTO 1000
c
c to prepare next series
 1402 CONTINUE
      m = m + 1
c
 1000 CONTINUE
c     condition to jump if bin was read and accepted previously
c
      IF (nbindexma(m).GT.nbint) THEN    
         GOTO 1002
      ENDIF
c
      IF (indbin(m).EQ.0) THEN
         IF (iendm(m).EQ.0) THEN
c         condition to read 1st point 
            GOTO 1003
         ELSE
c         condition if iendm=1 (i.e. one of the series is
c           already at the end upon calling the subr. again or point used to
c           decide start of intv. was the end of the file)
c prepare intv.
            IF (m.EQ.iflags(10)) THEN
               GOTO 1100
            ENDIF
            IF (m.LT.iflags(10)) THEN
c prepare next series
               GOTO 1402
            ENDIF
         ENDIF
      ENDIF
c
c     when an infile needs to be open etc.
C     Once again, we replace (for now) xrasks with assignments
c
1003  CONTINUE
      IF (irec(m+4).EQ.0) THEN
C     if a new file needs to be opened, parse filename string
C     xrfrdpon will do the actual file opening
         nfil(m) = nfil(m) + 1
         IF (nfil(m).EQ.1) iyrsta(m) = 0
         call xrparseopt(cfile(nfil(m),m),cfil(m+4),
     $        copt(1,m+4),iopt(1,m+4),mopt(1,m+4),dopt(1,m+4),ierr)
         if(ierr.ne.0) then
            errm = subname//' '//'Error parsing options from string: '
            call xaerror(errm, 5)
            errm = subname//' '//cfile(nfil(m),m)
            call xaerror(errm, 5)
            goto 999
         endif
      ENDIF

c Rev.2 : if exposure file for current series has problem go to 999
      IF (igoodexp(m+4).eq.0) GOTO 999
c
c read bin
c
c     Part for FITS files : Use EXPOSURE extension if present,
c                           otherwise use GTI's.
      CALL xrfrdpon(cfil, 2, iopt, mopt, dopt, m+4,
     &     irec, iyrsta, dtime, dtint, expos, y, sy,
     &     nbad, nogap, lui, ierr)
      IF(ierr.ne.0) THEN
         igoodexp(m+4) = 1
         go to 999
      ENDIF
      GOTO 1001
999   CONTINUE
c          if problem is found in exposure file for the first time reset 
c           relevant flag, reset error, increase irec and issue warning message
         IF(igoodexp(m+4).EQ.1) THEN
c                  reset error variables
            ierx=0
            ierr=0
            irec(m+4)=1
            igoodexp(m+4)=0
            nch=lenact(cfil(m+4))
            dtint(m+4)=8.d0
            dtime(m+4)=dntsta(1,m)-dtnb/2.d0/86400.d0-
     &                 dtint(m+4)/2.d0/86400.d0
            WRITE (*, 1203) cfil(m+4)(1:nch),m
1203        FORMAT (/,' **** Warning: problem with expos.file ',
     &              /,'      ',a,
     &              /,'               will start using 8 s bins of ',
     &                'exposure =1 for series',i2)
c            IF (lul.NE.0) WRITE (lul, 1203)
         ENDIF
c
c              now increase dtime by 1 bin (dtint) and set d/f exposure
c              (note that if an expos file bin was successfully read then 
c              dtime and dtint refer to that file)
         dtime(m+4)=dtime(m+4)+dtint(m+4)/86400.d0
         expos(m+4)=1.
c
c Process individual bins
c
 1001 CONTINUE
c
c     if last point in file
c
      IF (irec(m+4).EQ.-1) THEN
c reset to open another file      
         irec(m+4) = 0
         IF (nfil(m)+1.LE.nfilt(m)) THEN
c if not the last file open another file 
            GOTO 1000
         ELSE
c flag for end of good data
            iendm(m) = 1
c prepare intv.
            IF (m.EQ.iflags(10)) THEN
               GOTO 1100
            ENDIF
            IF (m.LT.iflags(10)) THEN
c next series (prepare)
               GOTO 1402
            ENDIF
         ENDIF
      ENDIF
c
c     apply SS and ST options to this point (other options not applied)
c     (note that to do this a fake dv=-1. is used for dtint to faka an 
c     arrival time file)
c
      IF (mopt(1,m+4).NE.0) THEN 
         dv=-1.D0
         CALL xrapplopt(iopt(1,m+4), mopt(1,m+4), dopt(1,m+4)
     &                 , dtime(m+4), dv, expos(m+4),y(m+4),sy(m+4))
      ENDIF 
c
c  do not apply time window but check that point is not beyond last windows
c
      IF (nwi(1).GT.0) THEN
         IF (dtime(m+4)-dtint(m+4)/86400.D0/2.D0.gt.twio(nwi(1))) THEN 
c end of good data (due to time window)
            iendm(m) = 1    
c close infile
            call ftclos(lui(m),ierr)
c prepare intv.
            IF (m.EQ.iflags(10)) THEN
               GOTO 1100
            ENDIF
            IF (m.LT.iflags(10)) THEN
               GOTO 1402
            ENDIF
         ENDIF
      ENDIF
c
c     apply bin exposure windows (and increase intsta(16) if necessary)
c     (note that intensity windows in bins are meaningless in this case)
c
      IF (nwi(12+(m-1)*3).GT.0) THEN
         CALL xrapplewi(ewia, ewio, nwi(12+(m-1)*3), m, 1, expos(m+4),
     &                  y(m+4),sy(m+4),intsta(16,m))
c read again
         IF (y(m+4).LT.-1.1E34) THEN
            GOTO 1000
         ENDIF
      ENDIF
c
c For qualified points
c
c statement for jump if bin was read previously
 1002 CONTINUE
      indbin(m) = indbin(m) + 1
c
c  calculate newbin index
c
      dvmi = ((dtime(m+4)-dntsta(1,m))*86400.D0-dtint(m+4)/2.d0)/
     &       dtnb+1.D0
      nbindexmi(m) = nint(dvmi)
      dvma = ((dtime(m+4)-dntsta(1,m))*86400.D0+dtint(m+4)/2.d0)/
     &       dtnb+1.D0
      nbindexma(m) = nint(dvma) 
c condition for nbindexma underflow
      IF (nbindexma(m).LT.1) THEN
c         intsta(19, m) = intsta(19, m) + 1
c read again from same series
         GOTO 1000
      ENDIF
c  condition for nbindexmi overflow (meaning current intv. is full and no 
c  part of this bin is useful for current intv.)
c  (note that the value of nbindexma is saved to use current bin in next intv)
      IF (nbindexmi(m).GT.nbint) THEN
         indbin(m) = indbin(m) - 1
c prepare intv.
         IF (m.EQ.iflags(10)) THEN
            GOTO 1100
         ENDIF
c next series (prepare)
         IF (m.LT.iflags(10)) THEN
            GOTO 1402
         ENDIF
      ENDIF
c
c  process bin in relevant newbins 
c
c      min and max newbin inedex affected by this bin 
      ivmi=max(1,nbindexmi(m))
      ivma=min(nbint,nbindexma(m))
c
      DO k =ivmi,ivma
c      reset value if necessary 
         IF (expi(k,m).LT.-1.1E34) THEN
            expi (k, m) = 0.
         ENDIF
c       work out start and stop of exposure for this newbin
         dvleft=max(dvmi,dble(k)-0.5D0)
         dvright=min(dvma,dble(k)+0.5D0)
c       work out exposure in secs
         IF(.not.dnear(dvright,dvleft,1.d-9))
     &      expi(k,m)=expi(k,m)+expos(m+4)*(dvright-dvleft)*dtnb
      ENDDO
c
c  Condition for nbindexma overflow (meaning current intv. is full and 
c  part of this bin is useful for next intv.)
c  (note that the value of nbindexma is saved to use current bin in next intv)
c
      IF (nbindexma(m).GT.nbint) THEN    
c prepare intv.
         IF (m.EQ.iflags(10)) THEN
            GOTO 1100
         ENDIF
c next series (prepare)
         IF (m.LT.iflags(10)) THEN
            GOTO 1402
         ENDIF
      ENDIF
c  
c read again from same series
      GOTO 1000
c
c interval ready 
c
 1100 CONTINUE
c
c Now apply windows in newbins (this is specific to arrival time files)
c then prepare newbins 
c
      DO m=1,iflags(10)
         DO k=1,nbint
            dbtime(m+4)=dntsta(1,m)+dble(k-1)*dtnb/86400.D0
c          time windows
            IF (nwi(1).gt.0) THEN
               CALL xrappltwi(twia,twio,nwi(1),dbtime(m+4),dtint(m+4),
     &                        expi(k,m),yi(k,m),syi(k,m),iv,iv)
            ENDIF
c          phase windows                         
            IF (nwi(2).gt.0) THEN
               CALL xrapplpwi(pwi,pwia,pwio,nwi(2),dbtime(m+4),
     &                   dtint(m+4),expi(k,m),yi(k,m),syi(k,m),iv)
            ENDIF
c
c          prepare newbins (note gaps identified based on expi)
c          reset exposed empty newbins to 0 
c to avoid expi = o as valis exposure
            IF (expi(k,m).EQ.0.0)expi(k,m)=-1.2E34
            IF (expi(k,m).GT.-1.1E34) THEN
               IF (yi(k,m).LE.-1.1E34) THEN 
c                reset exposed empty newbins to 0 
                  yi(k,m)=0.
                  syi(k,m)=0.
               ENDIF
            ELSE 
c               reset unexposed newbins to gaps  
               yi(k,m)=-1.2E34
               syi(k,m)=-1.2E34
            ENDIF        
         ENDDO
      ENDDO
      RETURN
c
c Prepare interval  (end)
c
      END
