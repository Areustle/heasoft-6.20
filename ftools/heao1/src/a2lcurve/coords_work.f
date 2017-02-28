C----------------------------------------------------------------------------
C This subroutine performs coord transformation stuff
C
C Author: Lorraine Breedon (Hughes STX; HEASARC/GSFC/NASA)
C History:
C  Version 1.0 Oct 20 1998 

      subroutine coords_work(rastr,decstr,dtr,equinox,ecp,sra,sdec,
     &                       sor,errstat)

      implicit none
      character*(*) rastr,decstr
      double precision equinox, dtr
      REAL sra , sdec , ecp(3) , sor(3)
      integer errstat
      

C Common block declarations

      common /TASK/ taskname
      
      character(40) taskname

C Local variables

      real ecpra,ecpdec
      DATA ecpra/4.71238898D0/ , ecpdec/1.161651D0/
      integer equi,status
      double precision radeg , decdeg
      character(160) message

C Initialise variables

      errstat=0
      status=0

C Convert Right Ascension and Declination inputs into B1950.0 coordinates
C in degrees.
 
      equi = INT(equinox)
      CALL PARSERA(rastr,equi,radeg,status)
      IF ( status.NE.0 ) THEN
         message = '  parsera: Error traslating RA string'
         CALL XAERROR(message,1)
         errstat=1
         GOTO 99
      ENDIF
      CALL PARSEDEC(decstr,equi,decdeg,status)
      IF ( status.NE.0 ) THEN
         message = '  parsdec: Error traslate DEC string'
         CALL XAERROR(message,1)
         errstat=1
         GOTO 99
      ENDIF
 
C Note the code is hardwired to assume input from parameters was in
C FK4.  However, the difference between FK4 and FK5 systems is well
C below the resolving power of the A2 instrument.
 
      sra = radeg*dtr
      sdec = decdeg*dtr
      IF ( equinox.NE.1950.0D0 ) CALL SLA_PRECES('FK4',equinox,1950.0D0,
     &     sra,sdec)
C  returned values from sla routine are sra, sdec (in radians)
 

C ecp, sor will eventually be used in routine xdirck.f
C sra, sdec will eventually be used in routine dsc5.f

      CALL CVXYZ(ecpra,ecpdec,ecp)
      CALL CVXYZ(sra,sdec,sor)

99    return

      end


