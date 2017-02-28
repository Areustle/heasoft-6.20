C*******************************************************************
      SUBROUTINE write_mekey(fits_unit,newrec1,newrec2,status)
C*******************************************************************
      include 'xrrbstr.h'
c
c     Input parameters:
      integer           fits_unit
      RECORD/rnewbuf_rec_1/newrec1
      RECORD/rnewbuf_rec_2/newrec2
      RECORD/rnewbuf_me/newme
c
c     Output parameters:
      integer       status
c
c     Local parameters:
      INTEGER   LENACT
      character(80) context
c
      character(8)   DATAMODE_KEY
      character(40)  DATAMODE_COM
      character(10)  DATAMODE
      PARAMETER(DATAMODE_KEY='DATAMODE')
      PARAMETER(DATAMODE_COM='on board computer mode')
c
      character(8)   SAMPLING_KEY
      character(40)  SAMPLING_COM
      INTEGER*4 SAMPLING
      PARAMETER(SAMPLING_KEY='SAMPLING')
      PARAMETER(SAMPLING_COM='obc mode sampling')
c
      character(8)   OBS_MODE_KEY
      character(40)  OBS_MODE_COM
      character(10)  OBS_MODE
      PARAMETER(OBS_MODE_KEY='OBS_MODE')
      PARAMETER(OBS_MODE_COM='observing mode')
c
      character(8)   DETBACK_KEY
      character(50)  DETBACK_COM
      character(17)  DETBACK_VAR
      PARAMETER(DETBACK_KEY='DETBACK')
      PARAMETER(DETBACK_COM='detector monitoring background')
c
      character(8)   SRCCNT_KEY
      character(40)  SRCCNT_COM
      REAL*4 SRCCNT
      PARAMETER(SRCCNT_KEY='SRCCNT')
      PARAMETER(SRCCNT_COM ='average source count rate')
c
      character(8)   SRCCNTE_KEY
      character(40)  SRCCNTE_COM
      REAL*4 SRCCNTE
      PARAMETER(SRCCNTE_KEY='SRCCNTE')
      PARAMETER(SRCCNTE_COM ='error on average source count rate')
c
      character(8)   HKCOUNT_KEY
      character(40)  HKCOUNT_COM
      REAL*4 HKCOUNT_VAR
      PARAMETER(HKCOUNT_KEY='HKCOUNT')
      PARAMETER(HKCOUNT_COM='HK count rate')
c
      character(8)   MINCHAN_KEY
      character(40)  MINCHAN_COM
      INTEGER*4 MINCHAN_VAR
      PARAMETER(MINCHAN_KEY='CHANMIN')
      PARAMETER(MINCHAN_COM='lower selected channel ')
c 
      character(8)   MAXCHAN_KEY
      character(40)  MAXCHAN_COM
      INTEGER*4 MAXCHAN_VAR
      PARAMETER(MAXCHAN_KEY='CHANMAX')
      PARAMETER(MAXCHAN_COM='higher selected channel ')
c
      character(8)   MINBIN_KEY
      character(40)  MINBIN_COM
      INTEGER*4 MINBIN_VAR
      PARAMETER(MINBIN_KEY='MINBIN')
      PARAMETER(MINBIN_COM='lower bin,value depend on the OBC mode')
c 
      character(8)   MAXBIN_KEY
      character(40)  MAXBIN_COM
      INTEGER*4 MAXBIN_VAR
      PARAMETER(MAXBIN_KEY='MAXBIN')
      PARAMETER(MAXBIN_COM='higher bin,value depend on the OBC mode') 
c
      character(8)   VIGNET_KEY
      character(40)  VIGNET_COM
      REAL*4  VIGNET
      PARAMETER(VIGNET_KEY='VIGNET')
      PARAMETER(VIGNET_COM='vignetting or collimator correction value')
c
      character(8)   DEADC_KEY
      character(40)  DEADC_COM
      REAL*4  DEADC
      PARAMETER(DEADC_KEY='DEADC')
      PARAMETER(DEADC_COM='deadtime correction applied ')
c
      character(8)   DEADERR_KEY
      character(40)  DEADERR_COM
      REAL*4  ERRDEAD
      PARAMETER(DEADERR_KEY='ERRCOR')
      PARAMETER(
     &    DEADERR_COM='correction for error due to sampling')
c

      character(8)   BACKAPP_KEY
      character(40)  BACKAPP_COM
      LOGICAL  BACKAPP
      PARAMETER(BACKAPP_KEY='BACKAPP')
      PARAMETER(BACKAPP_COM='background subtracted')
c
      character(8)   VIGNAPP_KEY
      character(40)  VIGNAPP_COM
      LOGICAL  VIGNAPP
      PARAMETER(VIGNAPP_KEY='VIGNAPP')
      PARAMETER(VIGNAPP_COM='vignetting or collimator applied')
c
      character(8)   DEADAPP_KEY
      character(40)  DEADAPP_COM
      LOGICAL  DEADAPP
      PARAMETER(DEADAPP_KEY='DEADAPP')
      PARAMETER(DEADAPP_COM='deadtime applied ')
c
      INTEGER*2 camera,bdetstr,bdetstp,bndet
      INTEGER exp, mode
      character(4) obcmode
      REAL rv
c
      newme=newrec2.newme
c
c observation mode scanning, slew, pointing
      mode=newrec1.slew_flag
      IF (mode.EQ.0)THEN
          OBS_MODE='POINTING'
          CALL FTPKYS(fits_unit,OBS_MODE_KEY,OBS_MODE,
     &                OBS_MODE_COM, status)
      ELSEIF(mode.EQ.1)THEN
          OBS_MODE='SLEW'
          CALL FTPKYS(fits_unit,OBS_MODE_KEY,OBS_MODE,
     &                OBS_MODE_COM, status)
      ELSE
         call xwrite(' Unknown observing mode', 10)
      ENDIF
c      
      obcmode=newme.rec_type
      IF (obcmode(1:2).EQ.'E2')THEN 
               datamode='HER2'
          sampling=4096
          CALL FTPKYS(fits_unit,DATAMODE_KEY,
     &       DATAMODE(1:LENACT(DATAMODE)),DATAMODE_COM, status)
          CALL FTPKYJ(fits_unit,SAMPLING_KEY,
     &       SAMPLING,SAMPLING_COM, status)
       ELSEIF(obcmode(1:2).EQ.'E3')THEN 
               datamode='HER3'
          sampling=4096
          CALL FTPKYS(fits_unit,DATAMODE_KEY,
     &       DATAMODE(1:LENACT(DATAMODE)),DATAMODE_COM, status)
          CALL FTPKYJ(fits_unit,SAMPLING_KEY,
     &       SAMPLING,SAMPLING_COM, status)
       ELSEIF(obcmode(1:2).EQ.'E4')THEN 
               datamode='HER4'
          sampling=4096
          CALL FTPKYS(fits_unit,DATAMODE_KEY,
     &       DATAMODE(1:LENACT(DATAMODE)),DATAMODE_COM, status)
          CALL FTPKYJ(fits_unit,SAMPLING_KEY,
     &       SAMPLING,SAMPLING_COM, status)
       ELSEIF(obcmode(1:2).EQ.'E5') THEN
          datamode='HER5'
          sampling=4096
          CALL FTPKYS(fits_unit,DATAMODE_KEY,
     &       DATAMODE(1:LENACT(DATAMODE)),DATAMODE_COM, status)
          CALL FTPKYJ(fits_unit,SAMPLING_KEY,
     &       SAMPLING,SAMPLING_COM, status)
       ELSEIF(obcmode(1:2).EQ.'PE'.OR.obcmode(1:2).EQ.'PB')THEN
          datamode='PULS'
          sampling=2048
          CALL FTPKYS(fits_unit,DATAMODE_KEY,
     &       DATAMODE(1:LENACT(DATAMODE)),DATAMODE_COM, status)
          CALL FTPKYJ(fits_unit,SAMPLING_KEY,
     &       SAMPLING,SAMPLING_COM, status)
      ELSE
          write(context,'('' Unknown datamode'', a)') obcmode
          call xwrite(context, 10)
          context=' Datamode and Sampling not written in the header'
          call xwrite(context,10)
          context=' Deadtime error due to the sampling set to zero'
          call xwrite(context,10)
          sampling=0
      ENDIF 
c
      exp=newrec1.expt 
      IF (newme.coll_eff.gt.0) THEN
         vignet=1/(newme.coll_eff/10000.)
         write(context, 100)newme.coll_eff, vignet
100      FORMAT(' collimator and reverse ', i4, 1X, f10.6)
         call xwrite(context,15)
         CALL FTPKYF(fits_unit,VIGNET_KEY,VIGNET,8, VIGNET_COM,status)
      ELSE
         vignet=1.
         CALL FTPKYF(fits_unit,VIGNET_KEY,VIGNET,8, VIGNET_COM,status)
      ENDIF
      IF (newme.DTFACT.gt.0) THEN
         deadc=1./(newme.dtfact/1000.)
         CALL FTPKYF(fits_unit, DEADC_KEY, DEADC,8,  DEADC_COM,status)
      ELSE 
         deadc=1.
         CALL FTPKYF(fits_unit, DEADC_KEY, DEADC,8,  DEADC_COM,status)
      ENDIF
c
c average and error count rate
      srccnt=(newrec1.average)/1000.
      If(srccnt.ne.0)then 
        srccnte=(newrec1.sdev)/1000.
        rv=srccnt*deadc*vignet
        if(sampling.ne.0) then
          errdead=sqrt(abs(1-rv/dfloat(sampling)))
        else
          errdead=0.0
        endif 
        CALL FTPKYF(fits_unit,Deaderr_KEY,errdead,8,deaderr_com,status)
        CALL FTPKYE(fits_unit,SRCCNT_KEY,SRCCNT,3,SRCCNT_com,status)
        CALL FTPKYE(fits_unit,SRCCNTE_KEY,SRCCNTE,3,SRCCNTE_com,status)
      ELSE
        errdead=deadc
        CALL FTPKYE(fits_unit,Deaderr_KEY,errdead,3,deaderr_com,status)
      ENDIF
c
      IF (newme.dtflag.NE.0)THEN
         vignapp=.true.
         deadapp=.true.
         CALL FTPKYL(fits_unit,VIGNAPP_KEY,VIGNAPP, VIGNAPP_COM,status)
         CALL FTPKYL(fits_unit,DEADAPP_KEY,DEADAPP, DEADAPP_COM,status)
      ENDIF 
c
      IF(newme.bgflag.eq.0) THEN 
        backapp=.false.
        CALL FTPKYL(fits_unit,BACKAPP_KEY,BACKAPP, BACKAPP_COM,status)
      ELSE 
        backapp=.true.
        CALL FTPKYL(fits_unit,BACKAPP_KEY,BACKAPP, BACKAPP_COM,status)
      ENDIF 
c
      hkcount_var=(newme.hkcount)/100.
      CALL FTPKYE(fits_unit,HKCOUNT_KEY,HKCOUNT_var,4,
     &            HKCOUNT_com,status)
c
      minchan_var=newme.startchan
      CALL FTPKYJ(fits_unit,MINCHAN_KEY,MINCHAN_VAR,MINCHAN_COM,status)
      maxchan_var=newme.endchan
      CALL FTPKYJ(fits_unit,MAXCHAN_KEY,MAXCHAN_VAR,MAXCHAN_COM,status)
      minbin_var=newme.startbin
      CALL FTPKYJ(fits_unit,MINBIN_KEY,MINBIN_VAR,MINBIN_COM,status)
      maxbin_var=newme.stopbin
      CALL FTPKYJ(fits_unit,MAXBIN_KEY,MAXBIN_VAR,MAXBIN_COM,status)
c
      IF(newme.bgflag.ne.0.and.newme.nbgdet.ne.0) THEN 
         camera=newme.instrument
         bdetstr=newme.startbg
         bdetstp=newme.stopbg
         bndet=newme.nbgdet
         write(context, 200)bdetstr, bdetstp
200      format (' detector number ', 2(1X, i4)) 
         CALL xwrite(context,10)
         CALL write_detnam(exp,camera,bdetstr,bdetstp,
     &                  bndet, detback_var,status)  
         CALL FTPKYS(fits_unit,DETBACK_KEY,DETBACK_VAR,
     &     DETBACK_COM, status)
      ENDIF
      RETURN
      END

