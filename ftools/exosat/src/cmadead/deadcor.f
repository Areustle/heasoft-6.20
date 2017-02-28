      subroutine deadcor(naxis,lcrows,time,tzero,row1,bin,nhdu,filename,
     &     start,npoint,fracexp,gndtime,shftime,prtime,bctime,bad,
     &     samples,fracsum,expt,status)

C========================================================
C This subroutine reads the OBC_PACKET extension of filename and computes
C exposure and sample rate dead time correction for the given bin size
C
C I naxis        Number of rows in OBC_PACKET table in event file
C I lcrows       Number of rows in lightcurve
C I time         Time array of lcrows elements from lightcurve
C I tzero        lightcurve start time
C I row1         last bin number of the previous event file
C I bin          Integration time in seconds read from LC keyword TIMEDEL
C I nhdu         Extension number for OBC_PACKET 
C I filename     Event filename
C O start        Time of first fractional exposure bin
C O npoint       Number of (nonzero) fractional exposure bins 
C O fracexp      Fractional exposure correction array
C O gndtime      Column read from OBC_PACKET - array
C O shftime      Column read from OBC_PACKET - array
C O prtime       Column read from OBC_PACKET - array
C O bctime       Column read from OBC_PACKET - array
C O bad          Column read from OBC_PACKET - array
C O samples      Column read from OBC_PACKET - array
C O fracsum      Sum of fracexp over number of bins in file (nbin)
C O expt         Exposure time = Sum(bin*fractional_exposure)
C========================================================
      implicit none

C Input/Output variables
      character filename*160
      integer*4 naxis,shftime(naxis),npoint,status,nhdu,lcrows,row1
      real*8 fracexp(lcrows),time(lcrows)
      real*8 gndtime(naxis),prtime(naxis)
      real*8 bin,start,tzero
      real*4 bctime(naxis),fracsum
      integer*2 bad(naxis),samples(naxis)

C Local variables

      character string*255,sstring*555
      character comment*80,subname*160,errm*255,context*255
      character obcmode*10
      integer*4 num,j
      integer*4 unit,readwrite,blocksize,hdutype,i4
      integer*4 tst,penp,penb,code,idim,mbin,nbin,count,nbinold,
     &     gtinum,in,inold,ingti,lastrow
      integer*4 hkstart,i,colnum,frow,felem,nelems,nullj,rprt
      integer*4 hkstartb(99),numhk,numhk2
c      integer*2 date(5)
      integer*2 nulli
      real*8 GR,expo(50000),srdtc(50000),expt
      real*8 tstart,tstop,tscal,zero,nulld,tt,diff,gti1(255),gti2(255)
      real*8 srf,TOT,TOTAL,btime(50000)
      real*4 dexp,nulle
      logical anynull,print,first


c      DATA hkstartb /1794087813,2030147268,-2028760370/
      PARAMETER(subname= 'deadcor:')
      if(status.ne.0) return
      lastrow=0
      in=1
      inold=1
      numhk=0
      numhk2=0
      nulld=0.0d0
      nbin=0
      nbinold=row1
      mbin=0      
      npoint=0
      num=0
      tst=0
C These are all commented out. Replaced by xwrite chat=40
C For debugging. Usually HUGE output if TRUE
c      print=.TRUE.
      print=.FALSE.
      first=.true.
      idim=naxis
      expt=0.0
      readwrite=0
      status=0
      errm=' '
      fracsum=0.0

      write(sstring,10) 
 10   format("Computing deadtime corrections")
      call xwrite(sstring,10)
      do i=1,naxis
         shftime(i)=0
         prtime(i)=0.0d0
         bctime(i)=0.0

         gndtime(i)=0.0d0

         bad(i)=0
         samples(i)=0
      enddo
      do i=1,50000
         expo(i)=0.00
         srdtc(i)=0.00
      enddo

      if(bin.gt.0) then
         GR=(1.0d0/bin)*(2.0d0**(-1*14))
      else
         context="Bin value must be >0 "
         goto 999
      endif

C========================================================
C Open the event file
C========================================================


      CALL FTGIOU(unit,status)
      if(status.ne.0) then
         context= "ERROR: Can't get unit number"
         goto 999
      endif

      CALL FTOPEN(unit,filename,readwrite,blocksize,status)
      if(status.ne.0) then 
         context="ERROR: Can't find "//filename
         goto 999
      endif

C Get the OBC mode from primary header

      CALL FTGKYS(unit,'OBCMODE',obcmode,comment,status)
C========================================================
C Go to the  STDGTI  extension
C========================================================

      CALL FTMAHD(unit,2,hdutype,status)
      if(status.ne.0) then
         context="ERROR: Can't move to STDGTI extension"
         goto 999
      endif
      CALL FTGKYJ(unit,'NAXIS2',gtinum,comment,status)
      if(status.ne.0) then
         context="ERROR: Can't get NAXIS2 from STDGTI extension"
         goto 999
      endif
      write(sstring,40) gtinum
 40   format("Number of GTI intervals ",I3)
      call xwrite(sstring,30)
      do i4=1,gtinum
         CALL FTGCVD(unit,1,i4,1,1,nulld,gti1(i4),anynull,status)
         CALL FTGCVD(unit,2,i4,1,1,nulld,gti2(i4),anynull,status)
      enddo
      do i4=1,gtinum
         write(sstring,41) gti1(i4),gti2(i4)
         call xwrite(sstring,30)
      enddo
 41   format("GTI Start & Stop ",f10.0,1x,f10.0)
C========================================================
C Go to the OBC_PACKET table
C========================================================
C      nhdu=5
      CALL FTMAHD(unit,nhdu,hdutype,status)
      if(status.ne.0) then
         context="ERROR: Can't move to OBC_PACKET extension"
         goto 999
      endif

C
C Get keywords
C

C Get number of HKSTART keywords
      CALL FTGKYJ(unit,'HKKEYS',numhk,comment,status)

      if(status.ne.0) then
         status=0
C If this is a single event file it probably doesn't have HKKEYS
C Assume one HKSTART keyword is present
         CALL FTGKYJ(unit,'HKSTART',hkstart,comment,status)
         if(status.ne.0) then
            context="ERROR: Can't get HKSTART keyword"
            goto 999
         endif
         numhk=1
         numhk2=1
         hkstartb(1)=hkstart
      else
C This event file may contain multiple HKSTART values
         CALL FTGKNJ(unit,'HKSTRT',1,numhk,hkstartb,numhk2,status)
      endif

      write(sstring,47) numhk,numhk2
 47   format("Number of HKSTRT keywords",I3,1x,I3)
      call xwrite(sstring,30)
      do i=1,numhk
         write(sstring,48) hkstartb(i)
 48      format("HKSTART ",I12)
         call xwrite(sstring,30)
      enddo

      CALL FTGKYD(unit,'TSTART',tstart,comment,status)
      if(status.ne.0) then
         context="ERROR: Can't get TSTART keyword"
         goto 999
      endif
      CALL FTGKYD(unit,'TSTOP',tstop,comment,status)
      if(status.ne.0) then
         context="ERROR: Can't get TSTOP keyword"
         goto 999
      endif


      tst=(tstop-tstart)*2.0d0**14

      tscal=1.0d0
      zero=0.0d0
      CALL FTTSCL(unit,3,tscal,zero,status)
      CALL FTTSCL(unit,4,tscal,zero,status)
C========================================================
C Read OBC_PACKET table
C========================================================
      do i=1,naxis
         colnum=1
         frow=1
         felem=1
         nelems=1
         nullj=0
         nulld=0.0d0
         nulle=0.0
         nulli=0

         CALL FTGCVD(unit,colnum,i,felem,nelems,nulld,gndtime(i),
     &        anynull,status)
         colnum=2
         CALL FTGCVJ(unit,colnum,i,felem,nelems,nullj,shftime(i),
     &        anynull,status)
         colnum=3
         CALL FTGCVD(unit,colnum,i,felem,nelems,nulld,prtime(i),
     &        anynull,status)
         colnum=4
         CALL FTGCVE(unit,colnum,i,felem,nelems,nulle,bctime(i),
     &        anynull,status)
         colnum=5
         CALL FTGCVI(unit,colnum,i,felem,nelems,nulli,bad(i),
     &        anynull,status)
         colnum=6
         CALL FTGCVI(unit,colnum,i,felem,nelems,nulli,samples(i),
     &        anynull,status)
         if(shftime(i).eq.0) then
            goto 110
         endif
C========================================================
C Finds when the GTI has changed
C========================================================
C ingti contains the current GTI interval (i.e. 1 for single GTI)
C gtinum(=NAXIS2 for STDGTI) would be greater than 1 for merged files
C
         in=ingti(shftime(i),gtinum,gti1,gti2) 
         if(in .ne. inold .and. in .ne. -999) then
            lastrow=lastrow+nbin-1
            write(sstring,42) inold,in,shftime(i)
 42         format("GTI has changed from",I3," to ",I3,"at",I10)
            call xwrite(sstring,40)
            write(sstring,43) hkstartb(in),lastrow
 43         format("and HKSTART is ",I12,"and last row # was",I5) 
            call xwrite(sstring,40)
         endif
         if(in .eq. -999) then
            goto 110
         endif


C --Temporary fix---
C     Eventually put extra keywords into merged event files listing all
C     HKSTART keywords of all event files
C
C Testing hkstartb which is hardwired with 3 values of HKSTART from the 
C  three test files used
c         if(gtinum.gt.1) then
C For merged event file (contains 3 rows in GTI)
c            RPRT = IDINT(prtime(i)) - hkstartb(in)
c         else
C For three event files (each with 1 row in GTI)            
C    The original line of code
c            RPRT = IDINT(prtime(i)) - hkstart
c         endif
C ------------------

         RPRT = IDINT(prtime(i)) - hkstartb(in)


         write(sstring,9020)
         call xwrite(sstring,40)
         write(sstring,*) shftime(i)
         call xwrite(sstring,40)
         write(sstring,50) prtime(i),RPRT
 50      format(F18.3,1x,I10)
         call xwrite(sstring,40)
c         if(print) write(6,9020)
c         if(print) write(6,*) shftime(i)
c         if(print) write(6,*) prtime(i),RPRT

         if(rprt .ge. 0 .and. rprt .le. tst) then

            TOT=bad(i)+samples(i)
            
            if(bctime(i) .gt. 0) then
               TOTAL=32.0d0*TOT/bctime(i)
               PENP = RPRT + bctime(i)*(2.0d0**(-1*5))*(2.0d0**14)
            else
               TOTAL=0.d0
               PENP=RPRT
               write(sstring,33) "BCTime =< 0 ! at GNDTime ",gndtime(i)
               CALL xaerror(sstring,30)
               write(sstring,34) "                 SHFTime ",shftime(i)
               CALL xaerror(sstring,30)
               write(sstring,33) "                 PRTime  ",prtime(i)
               CALL xaerror(sstring,30)
 33            format(A,F17.6)
 34            format(A,I17)

            endif
C========================================================
C To account for bad position events not telemetered down
C See EXOSAT IA adima.f revision history V2M4 & V2M5
C========================================================
            if(obcmode.ne.'LDIR') then
               TOTAL=1.24*TOTAL
            endif

            if(TOTAL .lt. 512 .and. TOTAL.gt.0) then
               srf=-512.0d0*DLOG(1.0d0 - TOTAL/512.0d0) / TOTAL
            else
               srf=1.03
            endif

C
C The value stored in prtime may exceed the maximum size allowed for an
C integer*4
C         
C Bin number of start of packet
            NBIN = RPRT*GR + 1

            write(sstring,44) nbinold,nbin,shftime(i)
 44         format("NBINOLD,NBIN, SHFTIME ",I5,I5,I12)
            call xwrite(sstring,35)

            if(nbinold.ne.(nbin+row1)) then
               write(sstring,45) count
 45            format("Number of packets in previous bin ",I4)
               call xwrite(sstring,30)

C For a merged event file (only enters deadcor once)
               if(gtinum.gt.1) then
                  write(sstring,46) shftime(i),lastrow+nbin,nbinold
 46               format("Start time, bin #, oldbin#  ",I12,I5,I5)
                  call xwrite(sstring,30)
               else
C For multiple event files entering deadcor multiple times  
                  write(sstring,46) shftime(i),row1+nbin,nbinold
                  call xwrite(sstring,30)
               endif
               btime(nbin+lastrow)=DBLE(shftime(i))
               nbinold=nbin+row1
               count=0
            endif
     
C Bin number of end of packet
            MBIN = PENP*GR + 1

C========================================================
C This section was taken directly from EXOSAT IA 
C   exp3 subroutine of lc_part1.F
C========================================================
            IF (NBIN.LE.IDIM) THEN
               count=count+1

               write(sstring,60) NBIN,MBIN
 60            format(I5,1x,I5)
               call xwrite(sstring,40)
c               IF (PRINT) WRITE (6, FMT=*) NBIN, MBIN
               IF (MBIN.GT.NBIN) THEN
*                 
*                 end of bin (2**-14 s units)
*                 
                  PENB = NBIN/GR
                  DEXP = (PENB-RPRT)*GR
*                 
*                 1ST bin of packet
*                 
c                  EXPO(NBIN) = DEXP + EXPO(NBIN)
c                  SRDTC(NBIN) = SRF*DEXP + SRDTC(NBIN)
                  EXPO(NBIN+lastrow) = DEXP + EXPO(NBIN+lastrow)
                  SRDTC(NBIN+lastrow) = SRF*DEXP + SRDTC(NBIN+lastrow)
                  write(sstring,70) PENB, EXPO(NBIN+lastrow),
     +                 SRDTC(NBIN+lastrow)
 70               format(I9,1x,F18.6,1x,F18.6)
                  call xwrite(sstring,40)
c                  IF (PRINT) WRITE (6, FMT=*) PENB, EXPO(NBIN),
c     +                 SRDTC(NBIN)
                  NBIN = NBIN + 1

 20               IF (.NOT. (MBIN.GT.NBIN.AND.
     +                 NBIN.LE.IDIM)) GO TO 30
*                 
*                 middle bins of packet
*                 

c                  EXPO(NBIN) = 1.0
c                  SRDTC(NBIN) = SRF
                  EXPO(NBIN+lastrow) = 1.0
                  SRDTC(NBIN+lastrow) = SRF
                  write(sstring,80) NBIN+lastrow, SRF,
     +                 SRDTC(NBIN+lastrow)
 80               format(I5,1x,F18.6,1x,F18.6)
                  call xwrite(sstring,40)
c                  IF (PRINT) WRITE (6, FMT=*) NBIN, SRF,
c     +                 SRDTC(NBIN)
                  NBIN = NBIN + 1

                  GO TO 20
 30               CONTINUE
                  IF (NBIN.LE.IDIM) THEN
*                    
*                    last bin of packet
*                    
c                     EXPO(NBIN) = (PENP- (NBIN-1)/GR)*GR
c                     SRDTC(NBIN) = SRF*EXPO(NBIN)
                     EXPO(NBIN+lastrow) = (PENP- (NBIN-1)/GR)*GR
                     SRDTC(NBIN+lastrow) = SRF*EXPO(NBIN+lastrow)
                     write(sstring,90) EXPO(NBIN+lastrow),
     +                    SRDTC(NBIN+lastrow)
 90                  format(F18.6,1x,F18.6)
                     call xwrite(sstring,40)
c                     IF (PRINT) WRITE (6, FMT=*) EXPO(NBIN),
c     +                    SRDTC(NBIN)
                  END IF
               ELSE IF (MBIN.EQ.NBIN) THEN
                  DEXP = (PENP-RPRT)*GR
c                  SRDTC(NBIN) = SRF*DEXP + SRDTC(NBIN)
                  SRDTC(NBIN+lastrow) = SRF*DEXP + SRDTC(NBIN+lastrow)
c                  write(6,*) "DEXP, SRDTC, BCTIME ",
c     &                 dexp,srdtc(nbin),bctime(i) 
*                 
*                 whole packet in bin
*                 
c                  EXPO(NBIN) = DEXP + EXPO(NBIN)
                  EXPO(NBIN+lastrow) = DEXP + EXPO(NBIN+lastrow)
                  write(sstring,100) EXPO(NBIN+lastrow), NBIN,
     +                 SRDTC(NBIN+lastrow)
 100              format(F18.6,1x,I5,F18.6)
                  call xwrite(sstring,40)
c                  IF (PRINT) WRITE (6, FMT=*) EXPO(NBIN), NBIN,
c     +                 SRDTC(NBIN)
               ELSE
                  write(sstring,9040)
                  CALL xwrite(sstring,40)
c                  WRITE (6, FMT=9040)
               END IF
            END IF
            IF (NBIN.EQ.-1 .AND. RPRT.GT.TST) then
               write(sstring,9050)
               CALL xwrite(sstring,40)
c               WRITE (6, FMT=9050)
            endif

            IF (NBIN.GT.IDIM .AND. i.EQ.1) THEN
*              
*              first pkt starts after last poss time in file
*              
               NBIN = -1
               CODE = -1
            END IF
         endif

         inold=in
 110     continue
      enddo


      write(string,35) NBIN
 35   format("Number of Bins NBIN = ",I5)
      CALL xwrite(string,30)
C Compute the sample rate deadtime correction (weighted mean srf)

      do i4 = 1, nbin+lastrow
         if(SRDTC(i4).gt.0.0d0) then
            if( EXPO(i4) .gt. 0.0) then
               SRDTC(i4)=SRDTC(i4)/EXPO(i4)
            else
               SRDTC(i4)=0.0d0
            endif
         endif
      enddo

C========================================================
C Multiply the source by the ratio ( true exposure / sample rate d.t.c)
C========================================================
C Here the fractional exposure array, fracexp is written
C
      do i4 = 1, nbin+lastrow
         if(srdtc(i4) .gt. 0.0d0) then
            num=num+1

C Compute DEADC

            fracsum=fracsum+( SNGL(expo(i4)) / SNGL(srdtc(i4)) )

C Compute exposure time

            expt=expt+(bin*(expo(i4) / srdtc(i4) )  )

c            write(14,23) i4,(row1+num),srdtc(i4),fracsum,fracexp(i4),
c     &           expt
c 23         format(I6,I6,4F15.3)

         endif
      enddo

C========================================================
C Here is where the time of the lightcurve bins (t+tzero) is
C compared to the average bin time if they are within 1 bin of each other
C then compute the fracexp for the same row of the lightcurve file
C========================================================
c      write(6,*) "TZERO, LCROWS, NBIN = ",tzero,lcrows,nbin
      do i=1,lcrows
C
C tt is the time of each row in the lightcurve in SHF.
C btime is the first time of each row of the event files within the jth bin
C
         tt=time(i)+tzero
         do j=1,nbin+lastrow

            diff=(tt-btime(j))/bin

            if(diff.lt.1.0d0.and.diff.ge.0.0d0.and.i.ge.row1) then
               if(srdtc(j).gt.0.0d0) then
                  fracexp(i)=expo(j) / srdtc(j)
               else
                  fracexp(i)=0.0d0
               endif
c               write(6,*) "i,j time tt tavg fracexp expo srdtc",
c     &         i,j,time(i),tt,btime(j), fracexp(i),expo(j),srdtc(j)
            endif
         enddo         
      enddo
C========================================================
C Setting the total number of fractional exposure bins, npoint,
C  to num+lastrow-1 for multiple event files num-1 for merged
C========================================================
      if(gtinum.ne.1) then
         npoint=num-1
      else
         npoint=num-1+lastrow
      endif

      write(string,68) num
 68   format(" Number of pts where srdtc > 0 ",I6)
      CALL xwrite(string,30)
      if(num.eq.0) then
         context="Sorry but there are no points for the lightcurve"
         goto 999
      endif
 
 9020 FORMAT ('***************************************')
 9040 FORMAT (' First observation number is zero !')
 9050 FORMAT (' Obs ', I3, ' non valid OBC mode or submode - rejected')

      CALL ftclos(unit,status)
      CALL ftfiou(unit,status)
      goto 1000
 999  continue
      CALL ftclos(unit,status)
      CALL ftfiou(unit,status)
       errm=subname//' '//context
      CALL RMVXBK(errm)
      call xerror(errm,5)
 1000 continue
      RETURN
      end


      function ingti(shf,ngti,tlist1,tlist2)
C This function returns the GTI interval which the current shftime is within
      integer*4 ngti,i4,shf,ingti
      real*8 tlist1(255),tlist2(255),start,stop

      ingti=-999
      do i4=1,ngti
         start=tlist1(i4)
         stop=tlist2(i4)

         if(shf.ge.start .and. shf.lt.stop) then
            ingti=i4
         endif
      enddo
      return
      end
