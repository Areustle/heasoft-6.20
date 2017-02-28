      subroutine xrftgdky(lui,keynami,keynamf,keynams,dval,ftstat)
      implicit none

c XRonos FiTs routine to Get a Double KeYword value.

c This general-purpose routine could actually be a fitsio routine.
c All it does is to allow the possibilty that a double keyword value
c may be represented in a file as two keywords: one integer and one
c double precision.  

c First, the routine looks for the integer-double pair given by
c keynami and keynamf, respectively.  If it finds those keywords,
c it returns their sum in dval.  If the pair are not found, it looks
c for the single keyword given by keynams and returns its value in dval.

c  I  lui      (i)  = lu of input fits file
c  I  keynami  (c)  = Name of the integer part of the keyword
c  I  keynamf  (c)  = Name of the fractional part of the keyword
c  I  keynams  (c)  = Name of the single keyword in case the double is not there
c  O  dval     (d)  = Returned double precision value [dble(i) + f]
c  O  ftstat   (i)  = 0 if succesful.

c Subroutines called: ftgkyj, ftgkyd

c Author: EAL, HEASARC/GSFC  HSTX, April 1994

      character*(*) keynami,keynamf,keynams
      character(16) comm
      integer lui,ival,ftstat
      double precision dval

      ftstat = 0

c Search for paired keyword first.

      CALL    ftgkyj(lui,keynami,ival,comm,ftstat)
      if(ftstat.eq.0) then
         CALL ftgkyd(lui,keynamf,dval,comm,ftstat)
         dval = dble(ival) + dval
      else

c Search for single keyword.

         ftstat = 0
         CALL ftgkyd(lui,keynams,dval,comm,ftstat)
      endif

      return
      end
