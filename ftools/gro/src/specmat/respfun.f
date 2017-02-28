CCCCCCCCCCCCCCCCCCCCCCCCCCCCC RESPFUN (SOURCE) CCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  RESPFUN
CH1
CH1  Version: 1.00                  Date: 7 August 1990
CH1
CH1  Programmer(s) and Completion Date:
CH1     Mark Fardal - Stanford University - 7 August 1990
CH1     Patrick Nolan - converted to SunOS - January 1991
CH1      removed cubic splines.  completed for version 2.7, Dec 94
CH1
CH1  Function:  Linear interpolation for the energy resolution
CH1     function
CH1
CH1  Software System and Spacecraft:  SPECTRAL, EGRET project
CH1
CH1  Computer and Language:  VAXSTATION II - VAX FORTRAN V4.0
CH1
CH2  Calling Sequence:  Call RESPFUN (RES_TABLE, RESOLN, ERESOV,
CH2     OV1, ETABLE, ETRUE)
CH2
CH2  Argument            Type   I/O                 Description
CH2  --------            ----   ---  ----------------------------------------
CH2  RES_TABLE(100,20)  Real*8    I   Master table of energy resolution fn
CH2  RESOLN(100)        Real*8    O   Resolution values at ETRUE
CH2  ERESOV(20)         Real*8    I   Master table of overflow bins
CH2  OV1                Real*8    O   Value of overflow bin at ETRUE
CH2  ETABLE(20)          Real     I   Incident energies in cal'n files
CH2  ETRUE               Real     I   Incident energy
CH2
CH2  Calls:
CH2    LOCATE: Finds position of value in table
CH2
CH3  COMMON Use: None
CH3
CH3  Significant Local Variables:
CH3  Variable      Type   Ini. Val.          Description
CH3  --------      ----   ---------  --------------------------------------
CH3  ITRUE        Integer    -       Place of ETRUE in table
CH3
CH3  Logical Units Used: None
CH3
CH4  Method:
CH4    Find place of ETRUE in energy table
CH4    If (ETRUE is outside range of energy table) then
CH4      Make assumptions about resolution
CH4    Else
CH4      Interpolate linearly in true energy 
CH4        and overflow bin
CH4    End If
CH4
CH4  Requirements and Assumptions:
CH4    The assumptions on values of true energy outside cal'n file
CH4      range are these: no effective area below 15 MeV and constant
CH4      energy resolution above 10 GeV.
CH4
C
C   @(#) respfun.f 1.2@(#)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine respfun(res_table,resoln,eresov,ov1,etable,etrue)

      implicit none

*     ARGUMENTS
      real etable(20),etrue
      real*8 res_table(100,20),resoln(100),eresov(20),ov1 

*     LOCAL VARIABLES
      integer itrue,i
      real p,q

      save


      call locate(etable,20,etrue,itrue)

      if (itrue.lt.1) then     
*	 ad hoc assumption 1: no effective area below etable(1) 
         do i = 1,100
            resoln(i) = 0.0
         end do
         ov1 = 0.0
      else if (itrue.ge.20) then   
* 	 ad hoc assumption 2: constant energy resolution above etable(20)
         do i = 1,100
            resoln(i) = res_table(i,20)
         end do
         ov1 = eresov(20)
      else
         q = (etrue-etable(itrue)) / (etable(itrue+1)-etable(itrue))
         p = (1.-q)
         do i = 1,100
            resoln(i) = p*res_table(i,itrue) + q*res_table(i,itrue+1)
         end do
         ov1 = p*eresov(itrue) + q*eresov(itrue+1)
      end if

      return

      end



