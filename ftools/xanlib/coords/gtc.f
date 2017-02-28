*+ gtc
       subroutine GTC (lii,bii,sys,equinox,ra,dec)
c
c GALACTIC TO CELESTIAL COORDINATES 
c  
c input lii and bii in degrees, output ra and dec in degrees for the 
c specified equinox year 
c all input and output is real*8, except system
c system must be either 'fk5' or 'fk4'
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
1        format(' Error in gtc, system must be FK4 or FK5') 
         return
       endif
c
c convert l & b to ra and dec Fk4 1950 or FK5 2000
c
       call degrad(lii,bii,liir,biir)
       if(system.eq.'FK5')then
        call sla_galeq(liir,biir,rar,decr)
       else
        call sla_ge50(liir,biir,rar,decr)
       endif
c
c precess to equinox
c
       if(equinox.ne.1950.0.and.system.eq.'FK4')then

        epoch=1950
        CALL sla_preces (system, epoch, equinox, rar, decr)

       elseif(equinox.ne.2000.0.and.system.eq.'FK5')then

        epoch=2000
        CALL sla_preces (system, epoch, equinox, rar, decr)

       endif

c
c convert from radians to degrees
c
       call raddeg(rar,decr,ra,dec)

       RETURN
       END
