c======================================
c gisspec.f
c     Spectrum Analysis Routines
c  11/10 1993 created by H.Kubo
c  11/19 1993 H.Kubo EVS GISSPEC:BEGIN,ENTRY,OK
c  12/3  1993 H.Kubo BNK_ -> BNK
c  12/6  1993 M.Hirayama add ANL_put_version
c  12/6  1993 M.Hirayama include '/utk/lib/asca_anl_v0.3/include/asca_anl.inc'
c  12/10 1993 M.Hirayama change var. name (recid -> eventid)
c  12/12 1993 M.Hirayama change include file('/utk/lib/...' -> 'Includes.inc')
c   3/30 1994 H.Kubo     update for asca_anl_v0.5
c   4/10 1994 Y.Ishisaki GIS: -> ANL:, use ANL:SENSOR
c   6/30 1994 Y.Ishisaki add RT, RTI, DETX/Y, X/Y, and correct bin cut
c  11/29 2005 Y.Ishisaki check sensor id, before BNKGET
c======================================
c
      Subroutine GISSPEC_init (status)
      Implicit NONE
c output
      Integer status
c include
      Include 'Includes.inc'
c EVS
      Call EVSdef('GISSPEC:BEGIN')
      Call EVSdef('GISSPEC:ENTRY')
      Call EVSdef('GISSPEC:OK')
c
      Call ANL_put_version('GISSPEC' , 'version 4.1')
      status = ASCA_ANL_OK
      Return
      End
c
      Subroutine GISSPEC_com (status)
      Implicit NONE
c output
      Integer status
c include
      Include 'Includes.inc'
c
      status = ASCA_ANL_OK
      Return
      End
c
      Subroutine GISSPEC_his (status)
      Implicit NONE
c output
      Integer status
c include
      Include 'Includes.inc'
c local
      Character*80 title
c Define Histogram
      Call Hbook1(200, 'GIS2 PHA', 1024, -0.5, 1023.5, 0.0)
      Call Hbook1(300, 'GIS3 PHA', 1024, -0.5, 1023.5, 0.0)
c PI is 0-1023
      Call Hbook1(210, 'GIS2 PI', 1024, -0.5, 1023.5, 0.0)
      Call Hbook1(310, 'GIS3 PI', 1024, -0.5, 1023.5, 0.0)
c RISE_TIME is 0-255
      Call Hbook1(220, 'GIS2 RISE_TIME', 256, -0.5, 255.5, 0.0)
      Call Hbook1(320, 'GIS3 RISE_TIME', 256, -0.5, 255.5, 0.0)
c RTI is 0-255
      Call Hbook1(230, 'GIS2 RTI', 256, -0.5, 255.5, 0.0)
      Call Hbook1(330, 'GIS3 RTI', 256, -0.5, 255.5, 0.0)
c RAWX/Y is 0-255
      Call Hbook2(240, 'GIS2 RAWX vs RAWY',
     &    256, -0.5, 255.5, 256, -0.5, 255.5, 0.0)
      Call Hbook2(340, 'GIS3 RAWX vs RAWY',
     &     256, -0.5, 255.5, 256, -0.5, 255.5, 0.0)
c DETX/Y is 1-256
      Call Hbook2(250, 'GIS2 DETX vs DETY',
     &    256, 0.5, 256.5, 256, 0.5, 256.5, 0.0)
      Call Hbook2(350, 'GIS3 DETX vs DETY',
     &     256, 0.5, 256.5, 256, 0.5, 256.5, 0.0)
c SKYX/Y is 1-256
      Call Hbook2(260, 'GIS2 X vs Y',
     &    256, 0.5, 256.5, 256, 0.5, 256.5, 0.0)
      Call Hbook2(360, 'GIS3 X vs Y',
     &     256, 0.5, 256.5, 256, 0.5, 256.5, 0.0)
c
      status = ASCA_ANL_OK
      Return
      End
c
      Subroutine GISSPEC_bgnrun (status)
      Implicit NONE
c output:
      Integer status
c include
      Include 'Includes.inc'
c  EVS set
      Call EVSset('GISSPEC:BEGIN')
c
      status = ASCA_ANL_OK
      Return
      End
c
      Subroutine GISSPEC_ana (nevent, eventid, status)
      Implicit NONE
c input:
      Integer nevent
      Integer eventid
c output:
      Integer status
c include
      Include 'Includes.inc'
c function:
      Logical EVS
c local
      Integer size, offset
      Integer sensor, pha, pi, rt, rti
      Integer rawx, rawy, detx, dety, skyx, skyy
c EVS
      Call EVSset('GISSPEC:ENTRY')

c check if PH mode or not
      If ( EVS('GIS:PH mode') ) then
         Continue
      Else
         Write(*,*) 'DATAMODE is not PH'
         status = ASCA_ANL_QUIT
         Return
      Endif

c get sensor id
      CALL BNKget('ANL:SENSOR', 4, size, sensor)

c check sensor id
      If ( (2 .eq. sensor) .or. (3 .eq. sensor) ) then
      
c get pha and pi
         CALL BNKget('GIS:PHA', 4, size, pha)
         CALL BNKget('GIS:PI', 4, size, pi)

c get rt and rti
         CALL BNKget('GIS:RISE_TIME', 4, size, rt)
         CALL BNKget('GIS:RTI', 4, size, rti)

c get rawx and rawy
         CALL BNKget('GIS:RAWX', 4, size, rawx)
         CALL BNKget('GIS:RAWY', 4, size, rawy)

c get detx and dety
         CALL BNKget('GIS:DETX', 4, size, detx)
         CALL BNKget('GIS:DETY', 4, size, dety)

c get skyx and skyy
         CALL BNKget('GIS:X', 4, size, skyx)
         CALL BNKget('GIS:Y', 4, size, skyy)

c make spectrum & image
         offset = sensor * 100
         Call Hf1(offset+00, real(pha), 1.0)
         Call Hf1(offset+10, real(pi), 1.0)
         Call Hf1(offset+20, real(rt), 1.0)
         Call Hf1(offset+30, real(rti), 1.0)
         Call Hf2(offset+40, real(rawx), real(rawy), 1.0)
         Call Hf2(offset+50, real(detx), real(dety), 1.0)
         Call Hf2(offset+60, real(skyx), real(skyy), 1.0)

      Endif

c set GISSPEC:OK for ASCA_ANL_OK
      Call EVSset('GISSPEC:OK')
      status = ASCA_ANL_OK
c
      Return
      End
c
      Subroutine GISSPEC_endrun (status)
      Implicit NONE
c output
      Integer status
c include
      Include 'Includes.inc'
c
      status = ASCA_ANL_OK
      Return
      End
c
c
      Subroutine GISSPEC_exit (status)
      Implicit NONE
c output
      Integer status
c include
      Include 'Includes.inc'
c
      status = ASCA_ANL_OK
      Return
      End
