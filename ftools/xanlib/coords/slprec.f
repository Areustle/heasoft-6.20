**==sPRECES.FOR
      SUBROUTINE slprec(ra,dc,ep0,ep1)
c
*  precession - fk5 (fricke, post-iau1976) as required.
c  g.t. 04-01-1990
c name changed to be six letters new 9-8-91
c changed to directly call sla_preces instead of copying it NEW 14-10-92 
c
*  given:
*     ep0,ep1    dp     starting and ending epoch
*     ra,dc      dp     ra,dec, mean equator & equinox of epoch ep0
*
*  returned:
*     ra,dc      dp     ra,dec, mean equator & equinox of epoch ep1
*
*  notes:
*  if an invalid system is supplied, values of -99d0,-99d0 will
*  be returned for both ra and dc.
*
*
      character(3) system
      real*8 ep0, ep1, ra, dc, rar, dcr
c 
      system = 'FK5'
c
c change to radians
c      
      call degrad(ra,dc,rar,dcr)
c
c do it
c
      call sla_preces (system,ep0,ep1,rar,dcr)

      call raddeg(rar,dcr,ra,dc)

      return
      END

