CCCCCCCCCCCCCCCCCCCCCCCCCCC DETEFF(SOURCE) CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  DETEFF
CH1
CH1  Version: 1.00                  Date: 7 August 1990
CH1
CH1  Programmer(s) and Completion Date:
CH1     Mark Fardal - Stanford University - 7 August 1990
CH1     Patrick Nolan - converted to SunOS - January 1991
CH1
CH1  Function:  Interpolation for the value of the detector sensitive
CH1      area. 
CH1
CH1  Software System and Spacecraft:  SPECTRAL, EGRET project
CH1
CH1  Computer and Language:  VAXSTATION II - VAX FORTRAN V4.0
CH1
CH2  Calling Sequence:  (Real function) DETEFF (ETRUE, EFF_TABLE)
CH2
CH2  Argument            Type   I/O                 Description
CH2  --------            ----   ---  ----------------------------------------
CH2  ETRUE               Real    I   Incident energy
CH2  EFF_TABLE(N_E)      Real    I   Area table
CH2
CH2  Calls:
CH2    LOCATE: Finds position of value in array.
CH2
CH3  COMMON Block Name:  TABLES (standard values of energy, angle)
CH3   Variable
CH3     Name            Type                   Definition
CH3  -----------       -------  ---------------------------------------------
CH3  E_TABLE(N_E)       Real    Values of incident energy in cal'n files
CH3
CH3  Significant Local Variables:
CH3  Variable      Type   Ini. Val.          Description
CH3  --------      ----   ---------  --------------------------------------
CH3  ITRUE        Integer    -       Position of ETRUE in energy table
CH3
CH3  Significant Local Constants:
CH3     Name       Type                   Definition
CH3  -----------  -------  ---------------------------------------------
CH3     N_E       Integer  Number of values in E_TABLE
CH3
CH3  Logical Units Used: None
CH3
CH4  Method:
CH4    If (energy is outside cal file range then)
CH4      Make assumptions for energies beyond cal file range
CH4    Else
CH4      Use linear interpolation for area
CH4    Endif
CH4
CH4  Requirements and Assumptions:
CH4    Values must be saved between calls.
CH4    The effective area below the lowest cal'n file energy (15 MeV)
CH4      is assumed to be 0.
CH4    The effective area above the highest cal'n file energy (10 GeV)
CH4      decreases exponentially with a 34 GeV e-folding energy.
C
C   @(#) deteff.f 1.2@(#)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      real function deteff(Etrue,eff_table)


      implicit none

*     PARAMETER
      integer n_e
      parameter (n_e = 20)

*     ARGUMENTS
      real Etrue,eff_table(n_e)

*     COMMON BLOCK
      real e_table(n_e),theta_table(9)
      common/tables/e_table,theta_table

*     LOCAL VARIABLES
      integer itrue
      real t

      save

*----------------------------------------------------------------------*

*THE AD HOC ASSUMPTIONS FOR LOW AND HIGH RANGES
      call locate(e_table,n_e,Etrue,itrue)
      if (itrue.lt.1) then
         deteff = 0.0
         RETURN
      else if (itrue.ge.n_e) then
         deteff = eff_table(n_e) *exp(-(Etrue-e_table(n_e))/34000.)
         RETURN
      end if


*LINEAR INTERPOLATION
       t = (Etrue-e_table(itrue)) / (e_table(itrue+1)-e_table(itrue))
       deteff = (1.-t)*eff_table(itrue) + t*eff_table(itrue+1)


      return

      end

