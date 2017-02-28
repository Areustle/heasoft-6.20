C*******************************************************************
      SUBROUTINE write_gskey(fits_unit,newrec1,newrec2,status)
C*******************************************************************
      include 'xrrbstr.h'
c
c     Input parameters:
      integer           fits_unit
      RECORD/rnewbuf_rec_1/newrec1
      RECORD/rnewbuf_rec_2/newrec2
      RECORD/rnewbuf_gs/newgs
c
c     Output parameters:
      integer       status
c
c     Local parameters:
      INTEGER   LENACT
      character(80) context 
c
      character(8)   MINCHAN_KEY
      character(40)  MINCHAN_COM
      INTEGER*4 MINCHAN_VAR
      PARAMETER(MINCHAN_KEY='CHANMIN')
      PARAMETER(MINCHAN_COM='lower selected channel')
c 
      character(8)   MAXCHAN_KEY
      character(40)  MAXCHAN_COM
      INTEGER*4 MAXCHAN_VAR
      PARAMETER(MAXCHAN_KEY='CHANMAX')
      PARAMETER(MAXCHAN_COM='higher selected channel')
c
      character(8)   E_min_key
      character(40)  E_min_COM
      INTEGER*4 E_min
      PARAMETER(E_min_key='E_MIN')
      PARAMETER(E_min_com='lower energy boundery')
c
      character(8)   E_max_key
      character(40)  E_max_COM
      INTEGER*4 E_max
      PARAMETER(E_max_key='E_MAX')
      PARAMETER(E_max_com='upper energy boundery')
c
      character(8)   Eunit_key
      character(40)  Eunit_COM
      character(5) Eunit
      PARAMETER(Eunit_key='EUNIT')
      PARAMETER(EUNIT_com='E_MAX and E_MIN units')
c
      character(8)   GAIN_KEY
      character(40)  GAIN_COM
      INTEGER*4 GAIN
      PARAMETER(GAIN_KEY='GAIN_NOM')
c
      character(8)   DATAMODE_KEY
      character(40)  DATAMODE_COM
      character(10)  DATAMODE
      PARAMETER(DATAMODE_KEY='DATAMODE')
      PARAMETER(DATAMODE_COM='on board computer mode')
c
      character(8)   BLMIN_KEY
      character(40)  BLMIN_COM
      INTEGER*4 BLMIN_VAR
      PARAMETER(BLMIN_KEY='BLMIN')
      PARAMETER(BLMIN_COM='minimun burst lenght')
c 
      character(8)   BLMAX_KEY
      character(40)  BLMAX_COM
      INTEGER*4 BLMAX_VAR
      PARAMETER(BLMAX_KEY='BLMAX')
      PARAMETER(BLMAX_COM='maximum burst lenght')
c
      character(8)   VIGNET_KEY
      character(40)  VIGNET_COM
      REAL*4  VIGNET
      PARAMETER(VIGNET_KEY='VIGNET')
      PARAMETER(VIGNET_COM='vignetting or collimator correction value')
c
      character(8)   BACKAPP_KEY
      character(40)  BACKAPP_COM
      LOGICAL  BACKAPP
      PARAMETER(BACKAPP_KEY='BACKAPP')
      PARAMETER(BACKAPP_COM='if background subtracted')
c
      character(8)   VIGNAPP_KEY
      character(40)  VIGNAPP_COM
      LOGICAL  VIGNAPP
      PARAMETER(VIGNAPP_KEY='VIGNAPP')
      PARAMETER(VIGNAPP_COM='if vignetting or collimator applied')
c
      character(8)   DEADAPP_KEY
      character(40)  DEADAPP_COM
      LOGICAL  DEADAPP
      PARAMETER(DEADAPP_KEY='DEADAPP')
      PARAMETER(DEADAPP_COM='deadtime applied ')
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
      character(8)   OBS_MODE_KEY
      character(40)  OBS_MODE_COM
      character(10)  OBS_MODE
      PARAMETER(OBS_MODE_KEY='OBS_MODE')
      PARAMETER(OBS_MODE_COM='observing mode')
c
      INTEGER obc,mode  
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
         call xwrite(' Unknown observing mode',10)
      ENDIF
c
      newgs=newrec2.newgs
      obc=newgs.nprog
      IF (obc.EQ.32) THEN
          datamode='HEBL4'
          CALL FTPKYS(fits_unit,DATAMODE_KEY,
     &       DATAMODE(1:LENACT(DATAMODE)),DATAMODE_COM, status)
      ELSEIF(obc.EQ.1) THEN
          datamode='DIRECT'
          CALL FTPKYS(fits_unit,DATAMODE_KEY,
     &       DATAMODE(1:LENACT(DATAMODE)),DATAMODE_COM, status)
      ELSEIf(obc.eq.2)THEN
          datamode='HEBL2'
          CALL FTPKYS(fits_unit,DATAMODE_KEY,
     &       DATAMODE(1:LENACT(DATAMODE)),DATAMODE_COM, status)
      ELSE 
          write(context,'(''Unknown datamode'', a)') datamode
          call xwrite(context, 10)
          call xwrite(' Datamode not written in the header',10)
      ENDIF
c
      newgs=newrec2.newgs
      vignet=newgs.coll_eff/10000.
      CALL FTPKYF(fits_unit,VIGNET_KEY,VIGNET,8, VIGNET_COM,status)
c
      backapp=.true.
      vignapp=.true.
      deadapp=.true.
      CALL FTPKYL(fits_unit,VIGNAPP_KEY,VIGNAPP, VIGNAPP_COM,status)
      CALL FTPKYL(fits_unit,DEADAPP_KEY,DEADAPP, DEADAPP_COM,status)
      CALL FTPKYL(fits_unit,BACKAPP_KEY,BACKAPP, BACKAPP_COM,status)
c
c average and error count rate
      srccnt=(newrec1.average)/1000.
      IF(srccnt.NE.0)then
        CALL FTPKYE(fits_unit,SRCCNT_KEY,SRCCNT,3,SRCCNT_com,status)
        srccnte=(newrec1.sdev)/1000.
        CALL FTPKYE(fits_unit,SRCCNTE_KEY,SRCCNTE,3,SRCCNTE_com,status)
      ENDIF
c
      minchan_var=newgs.startchan
      maxchan_var=newgs.stopchan
      eunit='keV'
      IF (minchan_var.eq.15.and.maxchan_var.eq.55) THEN
          gain=1
          gain_com='orignal chan range 15-55 for gain 1'
          minchan_var=30
          maxchan_var=110
          e_min=2
          e_max=7
      ELSEIf(minchan_var.eq.60.and.maxchan_var.eq.110)THEN
          gain=1
          gain_com='original chan range 60-110 for gain 1'
          minchan_var=120
          maxchan_var=220
          e_min=8
          e_max=14
      ELSEIF(minchan_var.eq.30.and.maxchan_var.eq.110)THEN
          gain=2
          gain_com='gain '
          e_min=2
          e_max=7
      ELSEIF(minchan_var.eq.120.and.maxchan_var.eq.220)THEN
          gain=2
          gain_com='gain '
          e_min=8
          e_max=14
      ELSE
         Call xwrite(' Unknown gain ', 10)
      ENDIF
      CALL FTPKYJ(fits_unit,E_min_KEY,E_min,E_min_COM,status)
      CALL FTPKYJ(fits_unit,E_Max_KEY,E_max,E_max_com,status)
      CALL FTPKYS(fits_unit,Eunit_KEY,EUNIT,EUNIT_com,status)
      CALL FTPKYJ(fits_unit,GAIN_KEY,gain,GAIN_COM,status)
      CALL FTPKYJ(fits_unit,MINCHAN_KEY,MINCHAN_VAR,MINCHAN_COM,status)
      CALL FTPKYJ(fits_unit,MAXCHAN_KEY,MAXCHAN_VAR,MAXCHAN_COM,status)
c
      blmin_var=newgs.blmin
      CALL FTPKYJ(fits_unit,BLMIN_KEY,BLMIN_VAR,BLMIN_COM,status)
      blmax_var=newgs.blmax
      CALL FTPKYJ(fits_unit,BLMAX_KEY,BLMAX_VAR,BLMAX_COM,status)
      RETURN
      END
