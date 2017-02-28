      subroutine xrftpdky(luo,keynami,keynamf,keynams,ival,dval,comm
     &                   ,ftstat)

c XRonos FiTs routine to Put (replace) a Double KeYword in the header.

c  I  luo      (i)  = lu of output fits file
c  I  keynami  (c)  = Name of the integer part of the keyword
c  I  keynamf  (c)  = Name of the fractional part of the keyword
c  I  keynams  (c)  = Name of the single keyword in case it is there
c  I  ival     (i)  = Integer part of keyword value
c  I  dval     (d)  = Fractional part of keyword value
c  O  ftstat   (i)  = 0 if succesful.

c Subroutines called: ftgpyj, ftgpyd, ftdkey

c Author: EAL, HEASARC/GSFC  HSTX, April 1994

      character*(*) keynami,keynamf,keynams,comm
      integer luo,ival,ftstat
      double precision dval

c Delete single or double keywords if present.

      ftstat = 0
      CALL ftdkey(luo,keynams,ftstat)
      ftstat = 0
      CALL ftdkey(luo,keynami,ftstat)
      ftstat = 0
      CALL ftdkey(luo,keynamf,ftstat)

c Write out double keyword.

      ftstat = 0
      CALL ftpkyj(luo,keynami,ival,comm,ftstat)
      CALL ftpkyg(luo,keynamf,dval,16,comm,ftstat)

      return
      end
