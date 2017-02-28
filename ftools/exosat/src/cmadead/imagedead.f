C Program is a modified version of IA adip1 routine expp
*   !!! NOTE !!!! RECORD(5) is invalid events on FOTS, RECORD(4)=0.
*                 This is the opposite to the FOTH definition.
*                 Data filed in real time has REC(4)=INVAL EV, REC(5)=0
*                 IE. is correct as defined in the FOTH.
*

c      SUBROUTINE EXPP(EXPT, JDCB, SHF1, SHF2, IPS, PERID, SHFKE, PHSL,
c     +                IWR, IDTC, IAP, IWSP)
      subroutine imagedead(naxis,nhdu,filename,gndtime,shftime,prtime,
     &     bctime,bad,samples,fracsum,expc,status)

      IMPLICIT NONE
C 
C This subroutine reads the OBC_PACKET extension of filename and computes
C the unbinned deadtime correction
C
C I naxis        Number of rows in OBC_PACKET table in event file
C I nhdu         Extension number for OBC_PACKET 
C I filename     Event filename
C O gndtime      Column read from OBC_PACKET - array
C O shftime      Column read from OBC_PACKET - array
C O prtime       Column read from OBC_PACKET - array
C O bctime       Column read from OBC_PACKET - array
C O bad          Column read from OBC_PACKET - array
C O samples      Column read from OBC_PACKET - array
C O fracsum      Sum of fractional exposure 
C O expc         Effective Exposure time (electronic+telemetry_)
C


*
*     .. Scalar Arguments ..
      integer*4 status,naxis,nhdu
      character filename*160
      real*4 fracsum
      real*8 expt,expc
*     ..
*     .. Array Arguments ..
      integer*4 shftime(naxis)
      integer*2 bad(naxis),samples(naxis)
      real*8 gndtime(naxis),prtime(naxis)
      real*4 bctime(naxis)

*     ..
*     .. Local Scalars ..
      SAVE
      REAL*4       EXPS, SRDTC, SWC, TOT,nulle
      INTEGER*4   DSIZE
c      integer*4   SHFKEY
      character obcmode*10,comment*80,subname*160,errm*255,context*255
      CHARACTER errtxt*30, stat*15, string*255
      integer*4 hkstart,i,colnum,frow,felem,nelems,nullj
      real*8 tstart,tstop,tscal,tzero,nulld
      integer*4 unit,readwrite,blocksize,hdutype,nrows
      logical anynull
      INTEGER*2 IDTC,nulli
      INTEGER*4   SHF1, SHF2
*     .. Intrinsic Functions ..
      INTRINSIC AINT, LOG
*     ..
*     .. Data statements ..
      DATA SWC, DSIZE/32., 768/
*     ..

      PARAMETER(subname= 'imagedead:')
      if(status.ne.0) return

      EXPT = 0
      EXPS = 0.
      EXPC = 0.

      write(string,30) 
 30   format("Computing deadtime corrections")
      call xwrite(string,10)

C========================================================
C Open the event file
C========================================================


      CALL FTGIOU(unit,status)
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context= "Can't get unit number"
         errm=subname//' '//context
         CALL xaerror(errm,5)
         goto 999
      endif

      CALL FTOPEN(unit,filename,readwrite,blocksize,status)
      if(status.ne.0) then 
         CALL FTGERR(status,errtxt)
         context="Can't find "//filename
         errm=subname//' '//context
         CALL xaerror(errm,5)
         goto 999
      endif

C Get the OBC mode from primary header

      CALL FTGKYS(unit,'OBCMODE',obcmode,comment,status)

C========================================================
C Go to the OBC_PACKET table
C========================================================
C      nhdu=5
      CALL FTMAHD(unit,nhdu,hdutype,status)
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context="Can't move to OBC_PACKET extension"
         errm=subname//' '//context
         CALL xaerror(errm,5)
         goto 999
      endif
      CALL FTGKYJ(unit,'NAXIS2',nrows,comment,status)
      CALL FTGKYD(unit,'TSTART',tstart,comment,status)
      if(status.ne.0) then 
         string="Can't get TSTART keyword"
         call xaerror(string,20) 
      endif
      CALL FTGKYD(unit,'TSTOP',tstop,comment,status)
      if(status.ne.0) then
         string="Can't get TSTOP keyword"
         call xaerror(string,20) 
      endif
      CALL FTGKYJ(unit,'HKSTART',hkstart,comment,status)
      if(status.ne.0) then
         string="Can't get HKSTART keyword"
         call xaerror(string,20) 
      endif

      tscal=1.0d0
      tzero=0.0d0

      CALL FTTSCL(unit,3,tscal,tzero,status)
      CALL FTTSCL(unit,4,tscal,tzero,status)

      i=0
      nullj=0
      nulld=0.0d0
      nulle=0.0
      nulli=0
      shf1=tstart
      shf2=tstop
   10 CONTINUE
        i=i+1
        if(i.ge.nrows) then
           goto 20
        endif
         colnum=1
         frow=1
         felem=1
         nelems=1

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

      IF (shftime(i).GE.SHF1 .AND. shftime(i).LE.SHF2 ) THEN
          EXPT = EXPT + bctime(i)
          TOT = bad(i)+samples(i)
          
*
*         valid+inval events. see note ^
*
          TOT = SWC*TOT/bctime(i)
          IF (obcmode.ne.'LDIR') then
             TOT = TOT*1.24
          endif
*
*         for LDIR2 (MOD 21/1/88)
*
          IF (TOT.LT.512) THEN
              SRDTC = -512*LOG(1.-TOT/512.)/TOT
          ELSE
              SRDTC = 1.03
c              WRITE (6, FMT=9000) shftime(i) - SHF1
              WRITE (string, FMT=9000) shftime(i) - SHF1
              CALL xwrite(string,20)
          END IF
          EXPC = EXPC + bctime(i)/SRDTC

      END IF
      GO TO 10
   20 EXPS = EXPT
      IF (EXPC.NE.0.) THEN
          SRDTC = EXPT/EXPC
      ELSE
          STOP 'EXPP: EXPC<0, this should not happen ...'
      END IF
      EXPS = EXPS/SWC
      EXPT = EXPS
      IDTC = SRDTC*10000. + 0.5
      EXPC=EXPC/SWC
      fracsum=SRDTC
c      write(12,*) "EXPC   = ",EXPC/SWC
c      write(12,*) "START - STOP ",SHF2-SHF1
c      write(12,*) "EXPS  = ",EXPS
c      write(12,*) "EXPT  = ",EXPT
c      write(12,*) "SRDTC = ",SRDTC
c      write(12,*) "1/SRDTC ",1.0/SRDTC
      if(status.eq.0) goto 1000
 999  continue
      CALL ftclos(unit,status)
      CALL ftfiou(unit,status)
      write(stat,*) status
      CALL RMVXBK(stat)
      errm=subname//' '//context//', '//errtxt//' '//stat
      CALL RMVXBK(errm)
      CALL xaerror(errm,5)
 1000 continue
      CALL FTCLOS(unit,status)
      CALL FTFIOU(unit,status)
c      RETURN
 9000 FORMAT (' Warning - at ', I6, ' sec into image - packet with 512',
     +       ' c/s')
      END







