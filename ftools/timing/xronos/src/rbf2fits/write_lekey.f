C*******************************************************************
      SUBROUTINE write_lekey(fits_unit,newrec1,newrec2,status)
C*******************************************************************
      include 'xrrbstr.h'
c
c     Input parameters:
      integer           fits_unit
      RECORD/rnewbuf_rec_1/newrec1
      RECORD/rnewbuf_rec_2/newrec2
      RECORD/rnewbuf_le/newle
c
c     Output parameters:
      integer       status
c
c     Local parameters:
      INTEGER   LENACT
c
      character(8)   OBS_MODE_KEY
      character(40)  OBS_MODE_COM
      character(10)  OBS_MODE
      PARAMETER(OBS_MODE_KEY='OBS_MODE')
      PARAMETER(OBS_MODE_COM='observing mode')
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
      character(8)   EMIN_KEY
      character(40)  EMIN_COM
      REAL*4 EMIN_VAR
      PARAMETER(EMIN_KEY='E_MIN')
      PARAMETER(EMIN_COM='lower energy')
c 
      character(8)   EMAX_KEY
      character(40)  EMAX_COM
      REAL*4 EMAX_VAR
      PARAMETER(EMAX_KEY='E_MAX')
      PARAMETER(EMAX_COM='higher energy')
c
      character(8)   EUNIT_KEY
      character(40)  EUNIT_COM
      character(4) EUNIT_VAR
      PARAMETER(EUNIT_KEY='EUNIT')
      PARAMETER(EUNIT_COM='unit for E_MIN, E_MAX')
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
      character(8)   AREA_KEY
      character(40)  AREA_COM
      REAL*4 AREA_VAR
      INTEGER mode
      PARAMETER(AREA_KEY='AREASCAL')
      PARAMETER(AREA_COM='scaling factor source/back area')
c
      newle=newrec2.newle
c
c OBS_MODE
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
c SRCCNT SRCCNTE
c average and error count rate
      srccnt=(newrec1.average)/1000.
      If(srccnt.ne.0)then 
        srccnte=(newrec1.sdev)/1000.
        CALL FTPKYE(fits_unit,SRCCNT_KEY,SRCCNT,3,SRCCNT_com,status)
        CALL FTPKYE(fits_unit,SRCCNTE_KEY,SRCCNTE,3,SRCCNTE_com,status)
      ENDIF
c
c E_MIN E_MAX EUNIT
      Emin_var= 0.05
      CALL FTPKYF(fits_unit,EMIN_KEY,EMIN_VAR,8,EMIN_COM,status)
      emax_var=2.5
      CALL FTPKYF(fits_unit,EMAX_KEY,EMAX_VAR,8,EMAX_COM,status)
      eunit_var='keV'
      CALL FTPKYS(fits_unit,EUNIT_KEY,eunit_var,EUNIT_COM,status)
c
c DEADAPP BACKAPP VIGNAPP
      IF(newle.bgflag.ne.1) THEN 
        backapp=.false.
        CALL FTPKYL(fits_unit,BACKAPP_KEY,BACKAPP, BACKAPP_COM,status)
        IF (newle.dtflag.EQ.1)THEN
         deadapp=.true.
         CALL FTPKYL(fits_unit,DEADAPP_KEY,DEADAPP, DEADAPP_COM,status)
        ENDIF
      ENDIF 
      IF(newle.bgflag.eq.1) THEN 
       backapp=.true.
       CALL FTPKYL(fits_unit,BACKAPP_KEY,BACKAPP, BACKAPP_COM,status)
       IF (newle.dtflag.EQ.1)THEN
        deadapp=.true.
        vignapp=.true.
        CALL FTPKYL(fits_unit,VIGNAPP_KEY,VIGNAPP, VIGNAPP_COM,status)
        CALL FTPKYL(fits_unit,DEADAPP_KEY,DEADAPP, DEADAPP_COM,status)
       ENDIF 
      ENDIF 
c
c DEADC
      IF (newle.DTFACT.gt.0) THEN
         deadc=1./(newle.dtfact/1000.)
         CALL FTPKYF(fits_unit, DEADC_KEY, DEADC,8,  DEADC_COM,status)
      ENDIF
c
c AREASCAL
      AREA_VAR= newle.sb_rat/10000.0
      CALL FTPKYF(fits_unit,AREA_KEY,AREA_VAR,8,AREA_COM,status)
c     
      RETURN
      END

