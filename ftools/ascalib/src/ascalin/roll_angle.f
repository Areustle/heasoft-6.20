C  $Header: /headas/headas/ftools/ascalib/src/ascalin/roll_angle.f,v 3.6 1996/04/16 23:52:21 dunfee Exp $
C          
C******************************************************************************
C SUBROUTINE:
C      roll_angle
C
C      roll angle from quaterian.
C
C      By Fujimoto-san (ISAS)
C
C******************************************************************************

      subroutine roll_angle (q, roll)
      
      double precision q(4)
      real roll
      
      include 'asca_defs.inc'
      
      double precision phi, theta, psi
      double precision boresight(3,3), xrt(3,3)
      
      include 'asca_common.inc'
      
C     quaternion transformation matrix elements
      
      call dir_cos(q, boresight)
        
C     compensate for boresight / telescope optical axis mis-alignment
      
      call matrix_mult3(misalign, boresight, xrt)
      
C     convert to euler angle
      
      call cosine2euler(xrt, phi, theta, psi)
      
C     definition of roll angle
      
c      if (psi .lt. 0.0) then
c         roll = -real(halfpi - psi - twopi)
c      else
c         roll = -real(halfpi - psi)         
c      end if

      roll = -real(halfpi - psi)
      if (roll .lt. -pi) roll = roll + twopi
      
      return
      
      end



