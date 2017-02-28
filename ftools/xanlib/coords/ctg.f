*+ctg
       subroutine CTG (ra,dec,sys,equinox,lii,bii)
c
c CELESTIAL TO GALACTIC COORDINATES 
c input is ra dec and equinox, output lii and bii
c for specified equinox year 
c all input and output is real*8, except system
c system must be 'fk5', 'fk4', 'j', 'b', '5', '4'
c uses slalib routines
c
c
c author NEW 15-oct-1992 
c
*- version 1.0
       character*(*) sys
       character(3) system
       real*8 lii, bii, ra, dec, rar, decr
       real*8 equinox, epoch, liir, biir
c
c check system is correctly specified
c
       if(sys(3:3).eq.'4'.or.sys(1:1).eq.'4')then
        system='FK4'
       elseif(sys(3:3).eq.'5'.or.sys(1:1).eq.'4')then
        system='FK5'
       elseif(sys(1:1).eq.'b'.or.sys(1:1).eq.'B')then
        system='FK4'
       elseif(sys(1:1).eq.'j'.or.sys(1:1).eq.'J')then
        system='FK5'
       else
         lii=0.0
         bii=0.0
         write(*,1)
1        format(' Error in ctg, system must be FK4, FK5, B or J') 
         return
       endif
c
c convert to radians
c
       call degrad(ra,dec,rar,decr)
c
c precess from equinox
c
       if(equinox.ne.1950.0.and.system.eq.'FK4')then

        epoch=1950
        CALL sla_preces (system, equinox, epoch, rar, decr)

       elseif(equinox.ne.2000.0.and.system.eq.'FK5')then

        epoch=2000
        CALL sla_preces (system, equinox, epoch, rar, decr)

       endif
c
c convert l & b to ra and dec Fk4 1950 or FK5 2000
c
       if(system.eq.'FK5')then
        call sla_eqgal(rar,decr,liir,biir)
       else
        call sla_eg50(rar,decr,liir,biir)
       endif
       call raddeg(liir,biir,lii,bii)
       RETURN
       END
