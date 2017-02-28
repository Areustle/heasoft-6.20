*- xangle - extract in degrees from value and units
      real*8 function xangle(angle,units,status)
       
      include 'status.codes'
      include 'dpi.inc'
* Author :
*  Andy Pollock (EXOSAT::ANDY)
* History :
*  16 February 1990 : original
* Import :
      real*8 angle
      character*(*) units
* Status :
      integer status
* Local variables :
      real*8 a
      character(10) u
      integer lu
      integer junit
* External reference :
      integer lenact
* Unit definition :
      integer nunit
      parameter (nunit=7)
      character(10) unit(nunit)
      data unit /'DEGREES',
     &           'ARCMINUTES',
     &           'ARCSECONDS',
     &           '''',
     &           '''''',
     &           '"',
     &           'RADIANS'/
*-
      if(status.ne.ok__) THEN
	xangle = 0.0
	return
      ENDIF
* check the units
      u=units
      call upc(u)
      lu=lenact(u)
      if(u(lu:lu).eq.'S')u(lu:lu)=' '
      call gmatch(unit,nunit,u,junit,status)
      if(status.eq.ok__)then
         if(u.eq.'DEGREES')then
            a=angle
         else if((u.eq.'ARCMINUTES').or.(u.eq.''''))then
            a=angle/6d1
         else if((u.eq.'ARCSECONDS').or.(u.eq.'"')
     &                              .or.(u.eq.''''''))then
            a=angle/36d2
         else if(u.eq.'RADIANS')then
            a=angle/qi
         endif
      endif

      if(status.eq.ok__)xangle=a

      return

      end
