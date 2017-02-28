CCCCCCCCCCCCCCCCCCCCCCCCCCC LINTERP (SOURCE) CCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  LINTERP
CH1
CH1  Version: 1.00                  Date: 6 August 1990
CH1
CH1  Programmer(s) and Completion Date:
CH1     Mark Fardal - Stanford University - 6 August 1990
CH1     Patrick Nolan - converted to SunOS - January 1991
CH1
CH1  Function:  Find linear interpolation coefficients for theta
CH1     and phi.
CH1
CH1  Software System and Spacecraft:  SPECTRAL, EGRET project
CH1
CH1  Computer and Language:  VAXSTATION II - VAX FORTRAN V4.0
CH1
CH2  Calling Sequence:  Call LINTERP (THETA, PHI, ITHLO, IPHLO, U, V, CALSET)
CH2
CH2  Argument            Type   I/O                 Description
CH2  --------            ----   ---  ----------------------------------------
CH2  THETA               Real    I   Tip angle in spacecraft coordinates
CH2  PHI                 Real    I   Azimuth angle in s.c. (0 is x-z plane)
CH2  ITHLO              Integer  O   Number of lower bracketing theta
CH2  IPHLO              Integer  O   Number of lower bracketing phi
CH2  U                   Real    O   Position within theta bracket
CH2  V                   Real    O   Position within phi bracket
CH2  CALSET             Char*2   I   Which set of calibration modes
CH2
CH2  Calls:
CH2   LOCATE: Finds position of value in table
CH2
CH3  COMMON Block Name:  TABLES (Contains energies,angles for cal'n files)
CH3   Variable
CH3     Name            Type                   Definition
CH3  -----------       -------  ---------------------------------------------
CH3    E_TABLE(20)      Real    Incident energies used in cal'n files
CH3    THETA_TABLE(9)   Real    Tip angles used in cal'n files
CH3
CH3  Significant Local Variables:
CH3  Variable      Type   Ini. Val.          Description
CH3  --------      ----   ---------  --------------------------------------
CH3  PPHI         Real       -       phi folded into 0-45 range (calset 00)
CH3
CH4  Method:
CH4    Look up theta in table
CH4    Find index numbers and interpolation coefficients for theta, phi
CH4    Abort program if theta or phi is outside allowed range
CH4
CH4  Requirements and Assumptions:
CH4    The calibration file formats are those found in the CALFIL document
CH4      as of December 1989.
C
C  @(#) linterp.f 1.3@(#)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine linterp(theta,phi,ithlo,iphlo,u,v,calset)

*     implicit none

*     ARGUMENTS
      integer ithlo,iphlo
      real theta,phi,u,v
      character(2) calset

*     COMMON BLOCKS
      real e_table(20),theta_table(9)
      common/tables/e_table,theta_table

*     LOCAL VARIABLES
      real pphi

      save


*---------------------------------------------------------------------*


C Find interpolating point for theta
      call locate(theta_table,9,theta,ithlo)
      if (ithlo.lt.1.or.ithlo.gt.8) then
         if (ithlo.eq.0.and.theta.eq.0.) then
            ithlo = 1
            u = 0.0
         else if (ithlo.eq.9.and.theta.eq.theta_table(9)) then
            ithlo = 8
            u = 1.0
         else
            write (6,22) int(theta_table(9))
22          format (' Source theta outside allowed range (0-',i2,
     >       ')--',/,'   Zero effective area.')
            ithlo = -1
         end if
      else
         u = (theta - theta_table(ithlo))/
     >   (theta_table(ithlo+1)-theta_table(ithlo))
      end if

C Find interpolating point for phi
      if (phi.lt.0.or.phi.gt.360.) then
         write (6,*) ' Source phi outside allowed range (0-360)--'
         write (6,*) '   Matrix generation aborted.'
         STOP
      end if
      if (calset.eq.'00') then
         pphi = amod(phi,90.)
         if (pphi.gt.45.) pphi = 90.-pphi
         if (pphi.lt.22.5) then
            iphlo = 1
            v = pphi/22.5
         else
            iphlo = 2
            v = (pphi-22.5)/22.5
         end if
      else
         iphlo = 1+phi/22.5
         v = (phi-22.5*(iphlo-1))/22.5
      end if

      return

      end

