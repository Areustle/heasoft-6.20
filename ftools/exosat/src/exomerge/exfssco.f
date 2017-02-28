      subroutine exfssco(tmid,tlist,cfile,nfiles,ra_nom,dec_nom,
     &     roll90,status)

C Given: That the spacecraft is operating in STR1 mode.
C
C This routine will correct the RA,DEC,TCRVL2,TCRVL3,TCROT3 and TROTA3
c keywords due to the Fine Sun Sensor error in the Roll. 
C
C I   tmid     The mid point in SHF key of the observation set
C I   tlist    The list of start and stop times for each observation
C I   cfile    List of event files
C I   nfiles   Number of event files
C O   ra_nom   The RA of the the CMA detector pointing direction
C O   dec_nom  The DEC of the the CMA detector pointing direction
C O   roll90   The roll angle + 90 deg of the spacecraft in ST coords.
C

      implicit none
      
      INTEGER*4 i,nhdu,status,iounit,hdutype,readwrite,blocksize,
     &     nfiles,tlist(256,2),naxis2,hknum,tmid,lenact
      INTEGER*4 colnum,nelem,felem,length,nrows,srow,steps,
     &     j,nullj,diff,mindiff,templinenum,even_steps
      INTEGER*4 tempshf(1000),shf,linenum,icount,rlinenum
      INTEGER*2 ioffst
      character(160) cfile(256),filename,orbfile,sstring
      character(8) keyword
      character(80) extname
c      character(80) instrume,filter
      character subname*50,context*160,comment*80,errm*255
      CHARACTER errtxt*40,stat*15
      REAL fss_misrol,fss_mispit
      REAL*8 ra_nom, dec_nom, roll90,roll(3),pitch(3),yaw(3),nulld
      real*8 mat(3,3),ra_gs,dec_gs,fss_roll,sun(3),ra,dec,theta,deg2rad
      real*8 dist,tsun(3),mismat(9),amat(9),le_mispit,le_misyaw
      LOGICAL FIRST,anynull,inside

c  Initialize to avoid warning
      templinenum = 0
      shf = 0.
c  --
      readwrite=0
      subname="exfssco: "
      errtxt=" "
      nhdu=0
      FIRST=.true.
      inside=.true.
      hknum=0
      deg2rad=(3.1415926535897932384d0/180.0d0)
      nullj=0
C
c First figure out which HK file will need to be read.
C
      do i=1,nfiles
C Which file is it within
         if(tmid.gt.tlist(i,1).and.tmid.le.tlist(i,2)) then
            hknum=i
         elseif(tmid.gt.tlist(i,2).and.tmid.lt.tlist(i+1,1)) then
C If it is between files choose the start time of the later one
            hknum=i+1
            linenum=1
            rlinenum=1
            shf=tlist(i+1,1)
            inside=.false.
         endif
      enddo
      if(hknum.eq.0) then
         write(sstring,50) hknum
 50      format("Something has gone terribly wrong!")
         CALL xaerror(sstring,5)
      endif
C Now open the appropriate HK file
C Check to see if the file is gzipped
      filename=cfile(hknum)
      length=lenact(filename)
      if(filename(length-1:length).eq."gz") then
         length=length-8
      else
         length=length-5
      endif
      filename=filename(1:length)//".hk"
      CALL FTGIOU(iounit,status)
      if(status.ne.0) then
         context="ERROR: Can't get unit number"
         goto 999
      endif
C========================================================
C     Open the HK FITS file - READ
C========================================================
      CALL FTOPEN(iounit,filename,readwrite,blocksize,status)
      if(status.ne.0) then 
         CALL FTGERR(status,errtxt)
         context="ERROR: Can't find HK file "//filename
         goto 999
      endif
C     
C Get the uncorrected attitude matrix ROLL, PITCH, YAW
C     
      nhdu=2
      CALL FTMAHD(iounit,nhdu,hdutype,status)
      
C Just in case. Get the extension name EXTNAME and check if it is ATT_MAT
      keyword="EXTNAME"
      CALL FTGKYS(iounit,keyword,extname,comment,status)
      if(status.ne.0) then 
         context="ERROR: EXTNAME keyword not found."
         goto 999
      endif
      CALL RMVBLK(extname)
      if(extname.ne."ATT_MAT") then
         context="ERROR: "
         errtxt=" ATT_MAT not 1st extension of HK"
         errm=subname//' '//context//', '//errtxt            
         CALL RMVXBK(errm)
         CALL XAERROR(errm,5)
         status=10  
         goto 999
      endif
      
      nulld=0.0d0
C Roll
      colnum=4
      nelem=3
      felem=1
      CALL FTGCVD(iounit,colnum,1,felem,nelem,nulld,roll,anynull,
     &     status)
      if(status.ne.0) then 
         context="ERROR: ROLL column "
         goto 999
      endif
C Pitch
      colnum=5
      nelem=3
      felem=1
      CALL FTGCVD(iounit,colnum,1,felem,nelem,nulld,pitch,anynull,
     &     status)
      if(status.ne.0) then 
         context="ERROR: PITCH column "
         goto 999
      endif
C Yaw
      colnum=6
      nelem=3
      felem=1
      CALL FTGCVD(iounit,colnum,1,felem,nelem,nulld,yaw,anynull,
     &        status)
      if(status.ne.0) then 
         context="ERROR: YAW column "
         goto 999
      endif
      
C Attitude matrix, mat
      mat(1,1)=roll(1)
      mat(2,1)=roll(2)
      mat(3,1)=roll(3)
      mat(1,2)=pitch(1)
      mat(2,2)=pitch(2)
      mat(3,2)=pitch(3)
      mat(1,3)=yaw(1)
      mat(2,3)=yaw(2)
      mat(3,3)=yaw(3)
C     
C Move to the next extension, read keywords and choose the correct row
C based on observation set midpoint
C     
      nhdu=3
      CALL FTMAHD(iounit,nhdu,hdutype,status)
      
C Just in case. Get the extension name EXTNAME and check if it is ATT_MAT
      keyword="EXTNAME"
      CALL FTGKYS(iounit,keyword,extname,comment,status)
      if(status.ne.0) then 
         context="ERROR: EXTNAME keyword not found."
         goto 999
      endif
      CALL RMVBLK(extname)
      if(extname.ne."ATT_DEV") then
         context="ERROR:"
         errtxt=" ATT_DEV not 2nd extension of HK"
         errm=subname//' '//context//', '//errtxt            
         CALL RMVXBK(errm)
         CALL XAERROR(errm,5)
         status=10
         goto 999
      endif
      
C Get keywords
C NAXIS2 of ATT_DEV extension
      keyword="NAXIS2"
      CALL FTGKYJ(iounit,keyword,naxis2,comment,status)
      if(status.ne.0) then 
         context="ERROR: NAXIS2 keyword not found."
         goto 999
      endif
C Catalog file - same as orbit file but with .cat extension
      keyword="CATFILE"
      CALL FTGKYS(iounit,keyword,orbfile,comment,status)
      if(status.ne.0) then 
         context="ERROR: NAXIS2 keyword not found."
         goto 999
      endif
      length=lenact(orbfile)-4
      orbfile=orbfile(1:length)//".orb"
C FSS Misalignment in Roll
      keyword="FSS_MISR"
      CALL FTGKYE(iounit,keyword,fss_misrol,comment,status)
      if(status.ne.0) then 
         context="ERROR: FSS_MISR keyword not found."
         goto 999
      endif
C FSS Misalignment in Pitch
      keyword="FSS_MISP"
      CALL FTGKYE(iounit,keyword,fss_mispit,comment,status)
      if(status.ne.0) then 
         context="ERROR: FSS_MISP keyword not found."
         goto 999
      endif
C LE - ST Misalignment in Pitch
      keyword="LEST_MSP"
      CALL FTGKYD(iounit,keyword,le_mispit,comment,status)
      if(status.ne.0) then 
         context="ERROR: LEST_MSP keyword not found."
         goto 999
      endif
C LE - ST Misalignment in Yaw
      keyword="LEST_MSY"
      CALL FTGKYD(iounit,keyword,le_misyaw,comment,status)
      if(status.ne.0) then 
         context="ERROR: LEST_MSY keyword not found."
         goto 999
      endif
C RA of Guide Star 1
      keyword="RA_GS1"
      CALL FTGKYD(iounit,keyword,ra_gs,comment,status)
      if(status.ne.0) then 
         context="ERROR: RA_GS1 keyword not found."
         goto 999
      endif
C DEC of Guide Star 1
      keyword="DEC_GS1"
      CALL FTGKYD(iounit,keyword,dec_gs,comment,status)
      if(status.ne.0) then 
         context="ERROR: DEC_GS1 keyword not found."
         goto 999
      endif
C
C Now read the HK Table extension #2. Use only the row that is closest
c in time to the tmid time and 
C only if the midpoint time lies within an HK file
C
      if(inside) then

C Determine the optimum number of rows I should read at once       

         CALL FTGRSZ(iounit,nrows,status)
         if (nrows.gt.1000) nrows=1000
         even_steps=mod(naxis2,nrows)
         if(nrows.gt.naxis2) then
            nrows=naxis2
            steps=1
         elseif(nrows.eq.naxis2) then
            steps=1
         else
            if(even_steps.eq.0) then
               steps=(naxis2/nrows)
            else
               steps=(naxis2/nrows)+1
            endif
         endif
         srow=1
         linenum=0
         mindiff=2e6
         diff = 0
         do i=1,steps
            if( (srow+nrows).gt.naxis2) then
               nrows=naxis2-(srow-1)
            endif
C SHFTIME
            CALL FTGCVJ(iounit,2,srow,1,nrows,nullj,tempshf,anynull,
     &           status)
            if(status.ne.0) then 
               if(status.ne.107) then
                  context="ERROR: Reading HK table."
                  goto 999
               else
                  status=0
               endif
            endif
            srow=i*nrows+1
C           
C Found that the SHF key returned with the HK record in CSRT corresponds
c           to the time for the previous HK record(i.e. row in FITS file). 
            do j=1,nrows
               diff=tempshf(j)-tmid
               if(diff.ge.0 .and. diff.lt.mindiff) then
                  if(diff.lt.8.and.diff.ge.0) then
                     if(diff.eq.0) then
                        linenum=(i-1)*nrows+j
                        rlinenum=linenum-1
                     else
                        linenum=(i-1)*nrows+j
                        rlinenum=linenum-2
                     endif
                     shf=tempshf(j)
                     goto 20
                  endif
                  mindiff=diff
                  templinenum=(i-1)*nrows+j
               endif
            enddo
         enddo
         linenum=templinenum
         if(mindiff.gt.8) then 
            context=" No data found within 8 sec of midpoint time"
            errtxt = " "
            errm=subname//' '//context
            CALL RMVXBK(errm)
            CALL XWARN(errm,5)
         endif
      else
         context=" No data found within 8 seconds of midpoint time"
c         errtxt="FSS correction poor "
         errtxt = " "
c         errm=subname//' '//context//', '//errtxt            
         errm=subname//' '//context
         CALL RMVXBK(errm)
         CALL XWARN(errm,5)
      endif
 20   continue
      if (rlinenum.eq.0) rlinenum=1
C Now that I know which line, read column 13 FSS_ROLL
      CALL FTGCVD(iounit,13,rlinenum,1,1,nulld,fss_roll,anynull,
     &     status)
 30   continue
      CALL ftclos(iounit,status)
      CALL ftfiou(iounit,status)
C
C Now open the Orbit file and find the appropriate row matching shf
C       
C========================================================
C     Open the ORB FITS file - READ
C========================================================
      CALL FTOPEN(iounit,orbfile,readwrite,blocksize,status)
      if(status.ne.0) then 
         CALL FTGERR(status,errtxt)
         context="ERROR: Can't find orbit file "//orbfile
         goto 999
      endif
C     
C Get the uncorrected attitude matrix ROLL, PITCH, YAW
C     
      nhdu=2
      CALL FTMAHD(iounit,nhdu,hdutype,status)
C Read NAXIS2 keyword
      keyword="NAXIS2"
      CALL FTGKYJ(iounit,keyword,naxis2,comment,status)
      if(status.ne.0) then 
         context="ERROR: NAXIS2 keyword not found."
         goto 999
      endif
      
C Determine the optimum number of rows I should read at once       
      CALL FTGRSZ(iounit,nrows,status)
         if (nrows.gt.1000) nrows=1000
      if(nrows.gt.naxis2) then
         nrows=naxis2
      endif
      if(nrows.gt.1000) then
         nrows=1000
      endif
      steps=(naxis2/nrows)+1
      srow=1
      linenum=0
      mindiff=2e6
      diff = 0
      do i=1,steps
         if((srow+nrows).gt.naxis2) then
            nrows=naxis2-srow
         endif
C SHFTIME checked against SHFTIME found in HK file
C If shftime lies between two rows then read both rows and compute the
c simple average between them
         CALL FTGCVJ(iounit,2,srow,1,nrows,nullj,tempshf,anynull,status)
         if(status.ne.0) then 
            if(status.ne.107) then
               context="ERROR: Reading ORB table."
               goto 999
            else
               status=0
            endif
         endif
         srow=i*nrows+1

         if(inside) then
            do j=1,nrows
               if(tempshf(j).eq.shf) then
                  linenum=(i-1)*nrows+j
                  goto 40
               else
                  diff=tempshf(j)-tmid
                  if(diff.ge.0 .and. diff.lt.mindiff) then
                     if(diff.lt.8.and.diff.ge.0) then
                        linenum=(i-1)*nrows+j
                        goto 40
                     endif
                     mindiff=diff
                     templinenum=(i-1)*nrows+j
                  endif
               endif
            enddo
         else
            do j=1,nrows
               diff=tempshf(j)-tmid
               if(diff.ge.0 .and. diff.lt.mindiff) then
                  if(diff.lt.8.and.diff.ge.0) then
                     linenum=(i-1)*nrows+j
                     goto 40
                  endif
                  mindiff=diff
                  templinenum=(i-1)*nrows+j
               endif
            enddo
         endif
      enddo
      linenum=templinenum
      errtxt =" "
      write(context,51) mindiff
 51   format(" FSS Corrected for a time ",I8,
     &     " seconds after midpoint time")
      errm=subname//' '//context
      CALL RMVXBK(errm)
      CALL XWARN(errm,5)
 40   continue
C Now that I know which line, read columns 9, 10 and 11 SUN_SAT_XYZ
C If the midpoint time lies between rows then get the average value
      if(diff.ne.0) then
         CALL FTGCVD(iounit,9,linenum-1,1,1,nulld,tsun(1),anynull,
     &        status)
         CALL FTGCVD(iounit,10,linenum-1,1,1,nulld,tsun(2),anynull,
     &        status)
         CALL FTGCVD(iounit,11,linenum-1,1,1,nulld,tsun(3),anynull,
     &        status)
         CALL FTGCVD(iounit,9,linenum,1,1,nulld,sun(1),anynull,
     &        status)
         CALL FTGCVD(iounit,10,linenum,1,1,nulld,sun(2),anynull,
     &        status)
         CALL FTGCVD(iounit,11,linenum,1,1,nulld,sun(3),anynull,
     &        status)
         sun(1)=(tsun(1)+sun(1))/2
         sun(2)=(tsun(2)+sun(2))/2
         sun(3)=(tsun(3)+sun(3))/2
      else
         CALL FTGCVD(iounit,9,linenum,1,1,nulld,sun(1),anynull,
     &        status)
         CALL FTGCVD(iounit,10,linenum,1,1,nulld,sun(2),anynull,
     &        status)
         CALL FTGCVD(iounit,11,linenum,1,1,nulld,sun(3),anynull,
     &        status)
      endif
      dist=dsqrt(sun(1)*sun(1)+sun(2)*sun(2)+sun(3)*sun(3))
      do i=1,3
         sun(i)=sun(i)/dist
      enddo
      CALL ftclos(iounit,status)
      CALL ftfiou(iounit,status)

      CALL clcps(mat,ra,dec,theta)
C
C Now call sunco routine to get the attitude matrix updated for roll
C SUNCO is from the Interactive Analysis under /LE/lib/att.F
C*        SUNCO   recompute the attitude matrix as function of
C*                star reference position and FSS measurements
C*
      ra_gs=ra_gs*deg2rad
      dec_gs=dec_gs*deg2rad
      fss_roll=fss_roll*deg2rad
      CALL sunco(mat,sun,ra_gs,dec_gs,fss_roll,icount)

      CALL clcps(mat,ra,dec,roll90)
      roll90=roll90+90.0d0

C
C The attitude matrix is now corrected for FSS 
C*********************************************

C Apply the 5 arcsec pitch correction
c  (subroutine checks date to see if necessary)
c     
      CALL PITCH5(mat,tmid,ioffst)

C Create the misalignment matrix from LE-ST misalignment values
      CALL MAKEMISMAT(le_mispit,le_misyaw,mismat)

C Multiply Matrices
      CALL MUL1(mat,mismat,amat)

C Find CMA pointing
      CALL clcps(amat,ra_nom,dec_nom,theta)
 900  continue
      CALL ftclos(iounit,status)
      CALL ftfiou(iounit,status)
      if (status.eq.0) goto 1000
 999  continue
      CALL ftclos(iounit,status)
      CALL ftfiou(iounit,status)
      write(stat,*) status
      CALL RMVXBK(stat)
      errm=subname//' '//context//', '//errtxt//' '//stat
      CALL RMVXBK(errm)
      CALL xaerror(errm,5)
 1000 continue
      return
      end
 

C From Interactive Analysis /LE/lib/lble.F
*
* RB pointing position
*
      SUBROUTINE CLCPS(A, RA, DEC, THETA)
      IMPLICIT CHARACTER (A-Z)

*     .. Scalar Arguments ..
      DOUBLE PRECISION DEC, RA, THETA
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION A(9)
*     ..
*     .. Local Scalars ..
      SAVE
      DOUBLE PRECISION PI2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC DACOS, DASIN, DATAN2
*     ..
      PI2 = DACOS(0.D0)/90.D0
      DEC = DASIN(A(3))/PI2
      RA = DATAN2(A(2), A(1))/PI2
      IF (RA.LT.0.D0) RA = RA + 360.D0
      THETA = DATAN2(A(6), A(5)*A(1)-A(4)*A(2))/PI2
      RETURN
      END



*
* Subroutine to perform the 5 arcsec rotation around pitch axis
*
* Input  : AMAT : Attitude matrix from AUX. data
*          SHFK : SHF Key at start of observation
*
* Output : IOFFST : = 0 if observation carried out before the offset
*                       was introduced (15.09.1983, 7:00 UT).
*                   = 1 in all other cases.
*
*
* Ed Gronenschild, July, 1985
* MOD J.O. 2/4/86: 5 arcsec sign corrected to +VE
*
* EG   5 arcsec pitch offset
*
      SUBROUTINE PITCH5(AMAT, SHFK, IOFFST)
      IMPLICIT CHARACTER (A-Z)
*
*     .. Scalar Arguments ..
      INTEGER SHFK
      INTEGER*2 IOFFST
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION AMAT(9)
*     ..
*     .. Local Scalars ..
      SAVE
      DOUBLE PRECISION CSOFF, OFF, PI, SIOFF
      INTEGER SHFK5
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC DACOS, DCOS, DSIN
*     ..
*     .. Data statements ..
*
*     offset was given at  15.09.1983 at 7:00 UT
*
      DATA SHFK5/116924400/
*     ..
*
*     see if offset was given at time of observation
*
      IOFFST = 0
      IF (SHFK.LT.SHFK5) RETURN
      IOFFST = 1
*
      PI = 180.D0/DACOS(-1.D0)
*
*     5 arcsec offset in radians
*
      OFF = 5.D0/3600.D0/PI
      SIOFF = DSIN(OFF)
      CSOFF = DCOS(OFF)
      AMAT(1) = AMAT(1)*CSOFF - AMAT(7)*SIOFF
      AMAT(7) = AMAT(1)*SIOFF + AMAT(7)*CSOFF
      AMAT(2) = AMAT(2)*CSOFF - AMAT(8)*SIOFF
      AMAT(8) = AMAT(2)*SIOFF + AMAT(8)*CSOFF
      AMAT(3) = AMAT(3)*CSOFF - AMAT(9)*SIOFF
      AMAT(9) = AMAT(3)*SIOFF + AMAT(9)*CSOFF
      RETURN
      END

      SUBROUTINE MAKEMISMAT(beta,gamma,mmat)
C Builds the Misalignment matrix from the LEST_MSP and LEST_MSY keywords
C
C See pg 275 in Best of EXOSAT Express for details on matrix and
C   Caution: They corrected one sign error but not the other! Y matrix
c     should have a negative sign for sin(delta gamma) (row 2, column 1)
C See pg 345 for the values of alpha', beta' and gamma'
c (0,-6.75",7.68") respectively
C Matrix is simplified based on alpha'=0

      implicit none
      real*8 beta,gamma,mmat(9),deg2rad

      deg2rad=(3.1415926535897932384d0/180.0d0)
      beta=beta*deg2rad
      gamma=gamma*deg2rad

      mmat(1)= dcos(beta)*dcos(gamma)
      mmat(2)= dsin(gamma)
      mmat(3)= -dsin(beta)*dcos(gamma)
      mmat(4)= -dcos(beta)*dsin(gamma)
      mmat(5)= dcos(gamma)
      mmat(6)= dsin(beta)*dsin(gamma)
      mmat(7)= dsin(beta)
      mmat(8)= 0.0d0
      mmat(9)= dcos(beta)
      end
