**==xpisavepar.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
*
* $Id: xpisavepar.f,v 3.7 2013/05/21 19:08:47 irby Exp $
*
* $Log: xpisavepar.f,v $
* Revision 3.7  2013/05/21 19:08:47  irby
* Change character*n to character(n) to silence warnings: "Obsolescent
* feature: Old-style character length".
*
* Revision 3.6  1998/02/19 15:16:48  pwilson
* Introduce PFPROTECT environment flag... prevents rewriting par files after
* executing a tool (intended for pipelines where par files should never be
* deleted).
*
* Revision 3.5.1.1  1996/04/16 01:39:37  dunfee
* Start of pristine ftools CVS...
*
c Revision 1.2  1995/06/02  14:09:05  oneel
c multiple parameter file changes
c
* save a parameter file
 
 
      SUBROUTINE XPISAVEPAR(Status)
 
 
      INCLUDE 'tbl.inc'
 
      INTEGER Status
      integer i
      character(40) protect

C  Do not rewrite par files if environment variable PFPROTECT is defined

      call xpigetenv('PFPROTECT',protect)
      if ( protect .eq. '(UNDEF)' ) then

         do i=1,tblpcpf
            CALL TBSVPR(TBLpfname(i),i,Status)
         end do

      endif
 
      RETURN
      END
