c======================================
c
c circreg_f.f
c     filter module to select events with circular region on its image
c     with DETX/Y as a reference
c
c     94/06/26 Created by M.Hirayama
c   2006/06/08 Y.Ishisaki, small modifications for g95
c
c======================================
c
      Subroutine circreg_init (status)
      Implicit NONE
c output
      Integer status
c include
      Include 'Includes.inc'
c EVS
      Call EVSdef('CIRCREG:BEGIN')
      Call EVSdef('CIRCREG:ENTRY')
      Call EVSdef('CIRCREG:OK')
c
      Call ANL_put_version('CIRCREG' , 'version 1.0')
      status = ASCA_ANL_OK
      Return
      End
c
      Subroutine circreg_com (status)
      Implicit NONE
c output
      Integer status
c include
      Include 'Includes.inc'
c local
      Integer sensor
      Save    sensor
      Data    sensor /0/
      Real cx(4), cy(4) ! center of the region
      Real r(4)         ! radius of the region
c
      Do While (.true.)
c title
         Write(*,*)
         Write(*,*) '*** Circular Region ***'
         Write(*,*)
c ask user for sensor ID
         Call Intrd('Sensor to set (-1=end of set)', sensor)
         If ((sensor.lt.0).or.(sensor.gt.3)) Goto 199
c ask user for center and radius of selection region
         Call Fltrd('DETX of Center (pixel)', cx(sensor+1))
         Call Fltrd('DETY of Center (pixel)', cy(sensor+1))
         Call Fltrd('Radius (pixel)', r(sensor+1))
      End do
c
 199  Continue
      Call circreg_ana_init(cx, cy, r)
      status = ASCA_ANL_OK
      Return
      End
c
      Subroutine circreg_his (status)
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
      Subroutine circreg_bgnrun (status)
      Implicit NONE
c output:
      Integer status
c include
      Include 'Includes.inc'
c function:
      Logical EVS
c  EVS set
      Call EVSset('CIRCREG:BEGIN')
c
      status = ASCA_ANL_OK
      Return
      End
c 
      Subroutine circreg_ana (nevent, eventid, status)
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
      Integer i
      Integer sensor
      Integer detx, dety
      Real r, dx, dy
      Integer size
c static
      Real center_x(4)          ! center of the region
      Real center_y(4)
      Real radius(4)            ! radius of the region
      Save center_x, center_y, radius
      Data center_x /640.5, 640.5, 128.5, 128.5/
      Data center_y /640.5, 640.5, 128.5, 128.5/
      DAta radius   /1024.0, 1024.0, 120.0, 120.0/
c variable for ENTRY
      Real ent_x(4), ent_y(4), ent_r(4)
c
      Call EVSset('CIRCREG:ENTRY')
c select OBS events
      If ((.not.EVS('SIS:SIS0 Event'))
     &     .and.(.not.EVS('SIS:SIS1 Event'))
     &     .and.(.not.EVS('GIS:GIS2 Event'))
     &     .and.(.not.EVS('GIS:GIS3 Event'))) then
         Call EVSset('CIRCREG:OK')
         status = ASCA_ANL_OK
         Return
      End if
c BNKget
      Call BNKget('ANL:SENSOR', 4, size, sensor)
      Call BNKget('ANL:DETX', 4, size, detx)
      Call BNKget('ANL:DETY', 4, size, dety)
c check sensor ID
      If ((sensor.lt.0).or.(sensor.gt.3)) then
         Call EVSset('CIRCREG:OK')
         status = ASCA_ANL_OK
         Return
      End if
c calculate distance from region center
      dx = Real(detx) - center_x(sensor+1)
      dy = Real(dety) - center_y(sensor+1)
      r = Sqrt(dx**2 + dy**2)
c check if the event is in or out of specified circle
      If (r.gt.radius(sensor+1)) then
         status = ASCA_ANL_SKIP
      Else
         Call EVSset('CIRCREG:OK')
         status = ASCA_ANL_OK
      End if
      Return
c
c entry to pass values
      Entry circreg_ana_init(ent_x, ent_y, ent_r)
      Do i=1,4
         center_x(i) = ent_x(i)
         center_y(i) = ent_y(i)
         radius(i) = ent_r(i)
      End do
      Return
      End
c
      Subroutine circreg_endrun (status)
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
      Subroutine circreg_exit (status)
      Implicit NONE
c output
      Integer status
c include
      Include 'Includes.inc'
c
      status = ASCA_ANL_OK
      Return
      End









