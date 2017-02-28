c======================================
c evdump.f
c     sis/gis data dump routines
c
c     H.Kubo     11/10 1993
c     H.Kubo     11/18 1993 EVS DUMP:BEGIN,ENTRY,OK
c     H.Kubo     system flags are changed
c     M.Hirayama 12/06 1993 subprogram name dump -> evdump
c     M.Hirayama 12/06 1993 add ANL_put_version
c     M.Hirayama 12/06 1993 include '/utk/lib/asca_anl_v0.3/include/asca_anl.inc'
c     M.Hirayama 12/10 1993 change var. name (recid -> eventid)
c     M.Hirayama 12/12 1993 change include file ('/utk/lib/...' -> 'Includes.inc')
c     Y.Ishisaki 04/10 1994 GIS: -> ANL:, use ANL:SENSOR
c======================================
c
      Subroutine evdump_init (status)
      Implicit NONE
c output
      Integer status
c include
      Include 'Includes.inc'
c EVS
      Call EVSdef('EVDUMP:BEGIN')
      Call EVSdef('EVDUMP:ENTRY')
      Call EVSdef('EVDUMP:OK')
c
      Call ANL_put_version('EVDUMP' , 'version 2.0')
      status = ASCA_ANL_OK
      Return
      End
c
      Subroutine evdump_com (status)
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
      Subroutine evdump_his (status)
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
      Subroutine evdump_bgnrun (status)
      Implicit NONE
c output:
      Integer status
c include
      Include 'Includes.inc'
c  EVS set
      Call EVSset('EVDUMP:BEGIN')
c
      status = ASCA_ANL_OK
      Return
      End
c
      Subroutine evdump_ana_sis(sensor, ascatime)
      Implicit none
c input
      Integer sensor
      Real*8 ascatime
c local
      Integer size
      Integer i
c event data
      Integer x, y
      Integer rawx, rawy
      Integer detx, dety
      Integer ccdid
      Integer grade
      Integer pha, phas(9)
c funtion:
      Logical EVS
c format
 1    Format(A12,I12)
 2    Format(A12,F17.4)
c
      Write(*,*)
      Write(*,1) 'SIS', sensor
c get event data
      Call BNKget('ANL:X',4,size,x)
      Call BNKget('ANL:Y',4,size,y)
      Call BNKget('ANL:RAWX',4,size,rawx)
      Call BNKget('ANL:RAWY',4,size,rawy)
      Call BNKget('ANL:DETX',4,size,detx)
      Call BNKget('ANL:DETY',4,size,dety)
      Call BNKget('ANL:TIME',8,size,ascatime)
      Call BNKget('SIS:CCDID',4,size,ccdid)
      Write(*,1) 'ANL:X ',x
      Write(*,1) 'ANL:Y ',y
      Write(*,1) 'ANL:RAWX ',rawx
      Write(*,1) 'ANL:RAWY ',rawy
      Write(*,1) 'ANL:DETX ',detx
      Write(*,1) 'ANL:DETY ',dety
      Write(*,2) 'ANL:TIME ', ascatime
      Write(*,1) 'SIS:CCDID ',ccdid
      If (EVS('SIS:BRIGHT mode').or.EVS('SIS:BRIGHT2 mode').or.
     &                              EVS('SIS:FAST mode')) then
        Call BNKget('ANL:PHA',4,size,pha)
        Call BNKget('SIS:GRADE',4,size,grade)
        Write(*,1) 'ANL:PHA ',pha
        Write(*,1) 'SIS:GRADE ',grade
      else if (EVS('SIS:FAINT mode')) then
        Call BNKget('SIS:PHAS',4*9,size,phas)
        Do i=1,9
          Write(*,1) 'SIS:PHAS ',phas(i)
        Enddo
      Endif
      Return
      End
c
      Subroutine evdump_ana_gis(sensor, ascatime)
      Implicit none
c input
      Integer sensor
      Real*8 ascatime
c local
      Integer i
c event data
      Integer x, y
      Integer pha, pi
      Integer phas(256)
      Integer rawx, rawy
      Integer detx, dety
      Integer rt, rti
c BNK
      Integer size
c function
      Logical EVS
c format
 1    Format(A15,I12)
 2    Format(A15,F17.4)
 3    Format(1X,A9,I3,A1,1X,256I12)
c
      Write(*,*)
      Write(*,1) 'GIS', sensor
c get event data
      Call BNKget('ANL:X',4,size,x)
      Call BNKget('ANL:Y',4,size,y)
      Call BNKget('ANL:TIME',8,size,ascatime)
      Write(*,1) 'ANL:X ',x
      Write(*,1) 'ANL:Y ' ,y
      Write(*,2) 'ANL:TIME ' ,ascatime
      If (EVS('GIS:PH mode')) then
        Call BNKget('ANL:PHA',4,size,pha)
        Call BNKget('ANL:PI',4,size,pi)
        Call BNKget('ANL:RAWX',4,size,rawx)
        Call BNKget('ANL:RAWY',4,size,rawy)
        Call BNKget('ANL:DETX',4,size,detx)
        Call BNKget('ANL:DETY',4,size,dety)
        Call BNKget('GIS:RISE_TIME',4,size,rt)
        Call BNKget('GIS:RTI',4,size,rti)
        Write(*,1) 'ANL:PHA ',pha
        Write(*,1) 'ANL:PI ',pi
        Write(*,1) 'ANL:RAWX ' ,rawx
        Write(*,1) 'ANL:RAWY ' ,rawy
        Write(*,1) 'ANL:DETX ' ,detx
        Write(*,1) 'ANL:DETY ' ,dety
        Write(*,1) 'GIS:RISE_TIME ' ,rt
        Write(*,1) 'GIS:RTI ' ,rti
      else if (EVS('GIS:MPC mode')) then
        Call BNKget('GIS:PHAS',4,size,phas)
        size = size / 4
        Write(*,3) 'GIS:PHAS(', size, ')', (phas(i),i=1,size)
      Endif
      Return
      End
c
      Subroutine evdump_ana (nevent, eventid, status)
      Implicit NONE
c input:
      Integer nevent
      Integer eventid
c output:
      Integer status
c include
      Include 'Includes.inc'
c function:
      Integer size, sensor
      Real*8 ascatime
c format
 1    Format(A12,256I12)
 2    Format(A12,F17.4)
c
c
      Call EVSset('EVDUMP:ENTRY')
c
      Call BNKget('ANL:SENSOR',4,size,sensor)
      Call BNKget('ANL:TIME',8,size,ascatime)
c
      If ( SENSOR_PSEUDO .eq. sensor ) then
         Write(*,*)
         Write(*,1) 'PSEUDO'
         Write(*,2) 'ANL:TIME',ascatime
      Else if ( (0 .eq. sensor) .or. (1.eq.sensor) ) then
        Call evdump_ana_sis(sensor, ascatime)
      Else if ( (2 .eq. sensor) .or. (3.eq.sensor) ) then
        Call evdump_ana_gis(sensor, ascatime)
      Endif
c
      Call EVSset('EVDUMP:OK')
c
      status = ASCA_ANL_OK
      Return
      End
c
      Subroutine evdump_endrun (status)
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
      Subroutine evdump_exit (status)
      Implicit NONE
c output
      Integer status
c include
      Include 'Includes.inc'
c
      status = ASCA_ANL_OK
      Return
      End
