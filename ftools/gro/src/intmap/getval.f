CCCCCCCCCCCCCCCCCCCCCCCC INTMAP.SOURCE(GETVAL) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  GETVAL
CH1
CH1  Version: 1.00                  Date: 10/26/90
CH1  Version: 2.00                  Date: 06/20/91
CH1  $Id: getval.f,v 1.2 2013/05/21 19:08:24 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - S.T.X. - 10/26/90
CH1     E.S.Panduranga - S.T.X. - 06/20/91
CH1
CH1  Function: Gets values for the ACS and for the wall distance from
CH1            a Namelist.
CH1
CH1  Software System and Spacecraft:  EGRET project
CH1
CH1  Computer and Language:  IBM 3081 - VS FORTRAN
CH1
CH2  Calling Sequence:  Call GETVAL(acs,walldi,strtim,endtim,iret)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2     acs         I*4     O   ACS value from Namelist
CH2     walldi      I*4     O   Wall distance value from SAGE Namelist
CH2     strtim      R*8     I   Start time of the data
CH2     endtim      R*8     I   End time of the data
CH2     iret        I*4     O   Subroutine return code (0 = Good)
CH2
CH2  Called by:  EXPOSR
CH2
CH2  Calls:
CH2   CONVRT: EGRET Utility Routine to convert TJD/MSD into single R*8
CH2   GRDTJD: EGRET Utility Routine to convert from gregorian to TJD/MSD
CH2
CH3  COMMON Use: None
CH3
CH3  Significant Local Variables:
CH3  Variable   Type   Ini. Val.               Description
CH3  --------   ----   ---------  --------------------------------------
CH3  astim1     R*8        -      Start for the 1st ACS value
CH3  aetim1     R*8        -      End for the 1st ACS value
CH3  astim2     R*8        -      Start for the 2nd ACS value
CH3  aetim2     R*8        -      End for the 2nd ACS value
CH3  acsval(2)  I*4        -      The 2 possible ACS values
CH3  acstm1(14) I*4        -      First ACS time range (mm1,dd1,yy1,hr1,
CH3                               mn1,sc1,ml1,mm2,dd2,yy2,hr2,mn2,sc2,
CH3                               ml2)
CH3  acstm2(14) I*4        -      Secnd ACS time range (mm1,dd1,yy1,hr1,
CH3                               mn1,sc1,ml1,mm2,dd2,yy2,hr2,mn2,sc2,
CH3                               ml2)
CH3  tjd        I*4        -      Current truncated Julian day
CH3  msd        I*4        -      Current millisecond of day
CH3
CH4  Logical Units Used:
CH4     Unit#  Variable                   Description
CH4     -----  --------  -----------------------------------------------
CH4       5              ACS Namelist input file
CH4       6              Printer report
CH4       7              SAGE Namelist input file
CH4
CH4  Method:
CH4    Read the ACS values and their time ranges from the Namelist
CH4    Convert the time ranges to combined TJD/MSD real*8 format
CH4    If (the data time is within the first ACS time range) then
CH4       Set the ACS value to the value for the first time range
CH4       If (the data time also intersects the second time range) then
CH4          Write a warning message
CH4       End if
CH4    Else if (the data time is within the second ACS time range) then
CH4       Set the ACS value to the value for the second time range
CH4    Else
CH4       Write an error message and terminate
CH4    End if
CH4    Read the wall distance from the SAGE Namelist
CH4  End GETVAL
CH4
CH5  Modifications Between Versions:
CH5     Mod #   Modifier    Date                  Description
CH5     -----   --------  --------   -----------------------------------
CH5	2.00	E.S.Panduranga	06/20/91
CH5				Moved source from IBM to SUN.
CH5				Stripped off trailing blanks.
CH5				Defined variables not defined on IBM.
CH5				Formed name for same namelistfile and opened it.
CH5     2.01    S. Bansal       Read misc_dir from intmap.par in intmap.f and
CH5                             pass it to this function in parameter list.
CH5                             Comment out the code to get misc_dir env var.
CH5
CH5 $Log: getval.f,v $
CH5 Revision 1.2  2013/05/21 19:08:24  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.1  2002/04/16 20:24:03  irby
CH5 New GRO tool intmap.
CH5
c Revision 2.2  1999/03/08  21:35:02  lmm
c more Y2K date format changes
c
c Revision 2.1  1991/09/09  18:08:49  nancy
c First controlled version on the Sun.
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      Subroutine GETVAL(acs,walldi,strtim,endtim,iret)


c      real    bondry,nwire(4),w(3)
      real*8  CONVRT,strtim,endtim,astim1,aetim1,astim2,aetim2
      integer acs,walldi,iret,acsval(2),acstm1(14),acstm2(14),tjd,msd

Cesp  ! Declaring varibale undeclared on IBM !
c      integer	jump2, namver
c      real	xtrang, tthbot
      save

c      namelist /input/ nwire,jump2,w,xtrang,tthbot,namver,bondry
c      namelist /acsnam/ acsval,acstm1,acstm2

Cesp  ! file name for sage name list !
c      integer		i

      character(80)	id
      common	/id/id
      id = '$Id: getval.f,v 1.2 2013/05/21 19:08:24 irby Exp $'

      iret = 0
C---> Read the ACS sensitivity and times from the NAMELIST
c     read(5,acsnam)
      acsval(1)  = 0
      acsval(2)  = 1

      acstm1(1)  = 1
      acstm1(2)  = 1
      acstm1(3)  = 86
      acstm1(4)  = 0
      acstm1(5)  = 0
      acstm1(6)  = 0
      acstm1(7)  = 0
      acstm1(8)  = 12
      acstm1(9)  = 31
      acstm1(10) = 86
      acstm1(11) = 23
      acstm1(12) = 59
      acstm1(13) = 59
      acstm1(14) = 999
      
      acstm2(1)  = 1
      acstm2(2)  = 1
      acstm2(3)  = 87
      acstm2(4)  = 0
      acstm2(5)  = 0
      acstm2(6)  = 0
      acstm2(7)  = 0
      acstm2(8)  = 12
      acstm2(9)  = 31
      acstm2(10) = 10
      acstm2(11) = 23
      acstm2(12) = 59
      acstm2(13) = 59
      acstm2(14) = 999


C---> Convert the first ACS start time, first adjust for Y2K
      if (acstm1(3) .lt. 65) acstm1(3)=acstm1(3)+100
      call GRDTJD(tjd,msd,acstm1(3)+1900,acstm1(1),acstm1(2),
     &     acstm1(4),acstm1(5),acstm1(6),acstm1(7),iret)
      if (iret .ne. 0) goto 100
      astim1 = CONVRT(tjd,msd)

C---> Convert the first ACS end time, first adjust for Y2K
      if (acstm1(10) .lt. 65) acstm1(10)=acstm1(10)+100
      call GRDTJD(tjd,msd,acstm1(10)+1900,acstm1(8),acstm1(9),
     &     acstm1(11),acstm1(12),acstm1(13),acstm1(14),iret)
      if (iret .ne. 0) goto 100
      aetim1 = CONVRT(tjd,msd)

C---> Convert the 2nd ACS start time, first adjust for Y2K
      if (acstm2(3) .lt. 65) acstm2(3)=acstm2(3)+100
      call GRDTJD(tjd,msd,acstm2(3)+1900,acstm2(1),acstm2(2),
     &     acstm2(4),acstm2(5),acstm2(6),acstm2(7),iret)
      if (iret .ne. 0) goto 100
      astim2 = CONVRT(tjd,msd)

C---> Convert the 2nd ACS end time, first adjust for Y2K
      if (acstm2(10) .lt. 65) acstm2(10)=acstm2(10)+100
      call GRDTJD(tjd,msd,acstm2(10)+1900,acstm2(8),acstm2(9),
     &     acstm2(11),acstm2(12),acstm2(13),acstm2(14),iret)
      if (iret .ne. 0) goto 100
      aetim2 = CONVRT(tjd,msd)

C---> Find which ACS time range the data falls under
      if (astim1.le.endtim.and.aetim1.ge.strtim) then
         acs = acsval(1)
         if (astim2.le.endtim) then
            write(6,*)' '
            write(6,*)'WARNING: the input data time range spans both
     &           time ranges supplied for the ACS values'
            write(6,*)'         The ACS value used may be incorrect',acs
            write(6,*)'         ACS time range 1:',acstm1
            write(6,*)'         ACS time range 2:',acstm2
         end if
      else if (astim2.le.endtim.and.aetim2.ge.strtim) then
         acs = acsval(2)
      else
         goto 200
      endif

C---> Read the wall distance, bondry, from the SAGE NAMELIST
Cesp  ! Construct the name of the sage name list and open it !
Csb   Read timeline (misc_dir) from intmap.par in intmap.f
cdlb  call getenv('MISC_DIR', sageList)
cdlb  i = index(sageList, ' ') - 1
cdlb  sageList = sageList(1:i) // '/pdbpre.cntl.name2'
cdlb  open(unit=7, file=sageList)

cdlb  read(7,input)

Cesp  ! close the file after reading !
cdlb  close(7)

cdlb  walldi = bondry
      walldi = 6.0   ! Hardcode for simplicity.  DLBertsch 

      return

C---> Invalid time
 100  continue
      write(6,*) 'GETVAL: invalid ACS time read:'
      write(6,*) acstm1,acstm2
      iret = 8
      return

C---> No match for ACS time range
 200  continue
      write(6,*) 'GETVAL: No ACS value for data time range'
      iret = 8
      return

CCCCCCCCCCCCCCCCCCCC END INTMAP.SOURCE(GETVAL) CCCCCCCCCCCCCCCCCCCCCCCCC
      end
