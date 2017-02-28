c anl_initroot.f
c
c Initialize routine for root module of ASCA_ANL
c
c   94/06/02  Authors: M.Hirayama(ISAS) and Y.Ogasaka(ISAS)
c                copied from fitsread.f
c   94/06/28  H.Kubo GIS:SPREAD introduced
c   94/09/10  Y.Ishisaki ANL:SKYREF, ANL:EULER introduced

c -------------
c init routine
c -------------
      Subroutine ANL_initroot(status)
      Implicit NONE
c output
      Integer status ! reserved
c const
      Integer MAXFILENAME
      Parameter (MAXFILENAME=256)
c
c EVS Definition
c (Instrument)
      Call EVSdef('SIS:SIS0 Event')
      Call EVSdef('SIS:SIS1 Event')
      Call EVSdef('GIS:GIS2 Event')
      Call EVSdef('GIS:GIS3 Event')
      Call EVSdef('SIS:PSEUDO Event')
      Call EVSdef('GIS:PSEUDO Event')
      Call EVSdef('SIS:SIS0 Pseudo')
      Call EVSdef('SIS:SIS1 Pseudo')
      Call EVSdef('GIS:GIS2 Pseudo')
      Call EVSdef('GIS:GIS3 Pseudo')
c (Data mode)
      Call EVSdef('ANL:BIT HIGH')
      Call EVSdef('ANL:BIT MEDIUM')
      Call EVSdef('ANL:BIT LOW')
      Call EVSdef('GIS:PH mode')
      Call EVSdef('GIS:MPC mode')
      Call EVSdef('SIS:BRIGHT mode')
      Call EVSdef('SIS:BRIGHT2 mode')
      Call EVSdef('SIS:FAINT mode')
      Call EVSdef('SIS:FAST mode')
c
c BNK Definition
c (PSEUDO interval)
      Call BNKdef('ANL:PSEUDO_INTERVAL', 8)
c (sensor ID & file name)
      Call BNKdef('ANL:SENSOR',4)
      Call BNKdef('ANL:FILENAME', MAXFILENAME)
c (common data)
c -- ANL --
      Call BNKdef('ANL:X',4)
      Call BNKdef('ANL:Y',4)
      Call BNKdef('ANL:PHA',4)
      Call BNKdef('ANL:PI',4)
      Call BNKdef('ANL:TIME',8)
      Call BNKdef('ANL:RAWX',4)
      Call BNKdef('ANL:RAWY',4)
      Call BNKdef('ANL:DETX',4)
      Call BNKdef('ANL:DETY',4)
      Call BNKdef('ANL:EULER', 8*3)
      Call BNKdef('ANL:SKYREF', 8*3)
c -- GIS --
      Call BNKeqv('GIS:X',4,'ANL:X',1)
      Call BNKeqv('GIS:Y',4,'ANL:Y',1)
      Call BNKeqv('GIS:PHA',4,'ANL:PHA',1)
      Call BNKeqv('GIS:PI',4,'ANL:PI',1)
      Call BNKeqv('GIS:TIME',8,'ANL:TIME',1)
      Call BNKeqv('GIS:RAWX',4,'ANL:RAWX',1)
      Call BNKeqv('GIS:RAWY',4,'ANL:RAWY',1)
      Call BNKeqv('GIS:DETX',4,'ANL:DETX',1)
      Call BNKeqv('GIS:DETY',4,'ANL:DETY',1)
c -- SIS --
      Call BNKeqv('SIS:X',4,'ANL:X',1)
      Call BNKeqv('SIS:Y',4,'ANL:Y',1)
      Call BNKeqv('SIS:PHA',4,'ANL:PHA',1)
      Call BNKeqv('SIS:PI',4,'ANL:PI',1)
      Call BNKeqv('SIS:TIME',8,'ANL:TIME',1)
      Call BNKeqv('SIS:RAWX',4,'ANL:RAWX',1)
      Call BNKeqv('SIS:RAWY',4,'ANL:RAWY',1)
      Call BNKeqv('SIS:DETX',4,'ANL:DETX',1)
      Call BNKeqv('SIS:DETY',4,'ANL:DETY',1)
c (GIS specific data)
      Call BNKdef('GIS:PHAS',4*256)
      Call BNKdef('GIS:RISE_TIME',4)
      Call BNKdef('GIS:RTI',4)
      Call BNKdef('GIS:SPREAD',4)
c (SIS specific data)
      Call BNKdef('SIS:PHAS',4*9)
      Call BNKdef('SIS:CCDID',4)
      Call BNKdef('SIS:GRADE',4)
c
      status = 0
c
      Return
      End
