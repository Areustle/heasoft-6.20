C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      SUBROUTINE rosatpsf
C
C
C  $Id: rosatpsf.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C*    Effect:  Calculate radial dependence of the ROSAT PSF using
c	indicated by IEL,IEH and a x-ray spectrum
C*             based on parameter's provided by his psfscal.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:    by  JRM
C=======================================================================
C  $Log: rosatpsf.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:43  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:53:19  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:15  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE rosatpsf
C
C  Common blocks used:
C
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'

      save

      character(80) id
      double precision e, sigma, rc, fn1, fn2, fn3, bk1, bk2, alpha,
     +     x, xx, offax

      common /id/id

      id = '$Id: rosatpsf.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      LOC='rosatpsf'


      Call   psf_scal(1,ierr,e, offax, sigma, rc, fn1, fn2,
     +     fn3, bk1, bk2, alpha )
      if (ierr.eq.1) stop 97
      x=0                       !!!!!!

      xx=psf_rosat(ierr, e, sigma, rc, fn1, fn2,
     +     fn3, bk1, bk2, alpha, x )
      if (ierr.eq.1) stop 98

      return
      end
