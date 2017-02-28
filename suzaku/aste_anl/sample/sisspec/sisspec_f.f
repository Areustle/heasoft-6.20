c======================================
c
c sisspec.f
c
c 93/11/19 created by M.Hirayama
c 93/11/19 H.Kubo EVS SISSPEC:BEGIN,ENTRY,OK
c 93/12/4  H.Kubo BNK_ -> BNK
c 93/12/4  H.Kubo change system flag
c 93/12/06 M.Hirayama add ANL_put_version
c 93/12/06 M.Hirayama include '/utk/lib/asca_anl_v0.3/include/asca_anl.inc'
c 93/12/10 M.Hirayama change var. name (recid -> eventid)
c 93/12/06 M.Hirayama change include file ('/utk/lib/...' -> 'Includes.inc')
c 94/03/28 M.Hirayama update for asca_anl_v0.5
c 94/04/10 Y.Ishisaki GIS: -> ANL:, use ANL:SENSOR
c 94/07/01 M.Hirayama change coverage of histograms
c               spectra: 0.0-1280.0 (2048bin) -> -0.5-4095.5 (512bin)
c               images : 0.0-1280.0 (256bin) -> 0.5-1280.5 (256bin)
c 2005/11/28 Y.Ishisaki check sensor id, before BNKGET
c 2006/06/08 Y.Ishisaki change format 'X' -> '1X' in SISspec_com() for g95
c
c======================================
c
      Subroutine SISspec_init (status)
      Implicit NONE
c output
      Integer status
c include
      Include 'Includes.inc'
c EVS
      Call EVSdef('SISSPEC:BEGIN')
      Call EVSdef('SISSPEC:ENTRY')
      Call EVSdef('SISSPEC:OK')
c
      Call ANL_put_version('SISSPEC' , 'version 3.1')
      status = ASCA_ANL_OK
      Return
      End
c
      Subroutine SISspec_com (status)
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
      Subroutine SISspec_his (status)
      Implicit NONE
c output
      Integer status
c include
      Include 'Includes.inc'
c local
      Character*30 title
      Integer sensor, chip, grade, id
c Define Histogram
      Do sensor=0,1
         Do chip=0,3
            Do grade=0,7
               id = 4000 + sensor*100 + chip*10 + grade
               Write(title,'(1X,A,I1,A,I1,A,I1,A)')
     &              'SIS', sensor, ' CHIP', chip,
     &              ' GRADE', grade, ' PHA'
               Call Hbook1(id,title,512,-0.5,4095.5,0.0)
            End do
         End do
      End do
      title = 'SIS0 DETX vs DETY'
      Call Hbook2(4500,title,256,0.5,1280.5,256,0.5,1280.5,0.0)
      title = 'SIS1 DETX vs DETY'
      Call Hbook2(4600,title,256,0.5,1280.5,256,0.5,1280.5,0.0)
c
      status = ASCA_ANL_OK
      Return
      End
c
      Subroutine SISspec_bgnrun (status)
      Implicit NONE
c output:
      Integer status
c include
      Include 'Includes.inc'
c function:
      Call EVSset('SISSPEC:BEGIN')
c
      status = ASCA_ANL_OK
      Return
      End
c
      Subroutine SISspec_ana (nevent, eventid, status)
      Implicit NONE
c input:
      Integer nevent
      Integer eventid
c output:
      Integer status
c include
      Include 'Includes.inc'
c local:
c _BNK
      Integer size
      Integer sensor, detx, dety
      Integer pha, ccdid, grade
c function:
      Logical EVS
c
      Call EVSset('SISSPEC:ENTRY')
c
c check if bright mode or not
      If (EVS('SIS:BRIGHT mode')) then
         status = ASCA_ANL_OK
      Else
         Write(*,*) 'DATAMODE is not BRIGHT'
         status = ASCA_ANL_QUIT
         Return
      End if
c
c get sensor id
      Call BNKget('ANL:SENSOR',4,size,sensor)
c
c check sensor id
      If ( (0 .eq. sensor) .or. (1 .eq. sensor) ) then
c
c get pha
         Call BNKget('SIS:PHA', 4, size, pha)
         Call BNKget('SIS:CCDID', 4, size, ccdid)
         Call BNKget('SIS:GRADE', 4, size, grade)
c
c get detx and dety
         Call BNKget('SIS:DETX',4,size, detx)
         Call BNKget('SIS:DETY',4,size, dety)
c
c make spectrum & image
         Call Hf1(4000+sensor*100+ccdid*10+grade, real(pha),1.0)
         Call Hf2(4500+sensor*100,Real(detx),Real(dety),1.0)

      End if
c
      Call EVSset('SISSPEC:OK')
      status = ASCA_ANL_OK
      Return
      End
c
      Subroutine SISspec_endrun (status)
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
      Subroutine SISspec_exit (status)
      Implicit NONE
c output
      Integer status
c include
      Include 'Includes.inc'
c
      status = ASCA_ANL_OK
      Return
      End
