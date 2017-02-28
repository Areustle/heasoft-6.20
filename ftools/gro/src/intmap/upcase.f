CCCCCCCCCCCCCCCCCCCCCCCC MAPGEN.SOURCE(UPCASE) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  Routine Name:  UPCASE
CH1
CH1  Version: 2.00                 Date: 06/25/91
CH1  Version: 1.05                 Date: 06/07/91
CH1  $Id: upcase.f,v 1.3 2013/05/21 19:08:25 irby Exp $
CH1
CH1  Programmer(s) and Completion Date:
CH1     Albert Etienne - S.T.X. - 06/04/91
CH1
CH1  Function: Convert the timeline file keywords to uppercase.
CH1
CH1  Software System and Spacecraft:  EGRET Project
CH1
CH1  Computer and Language:  IBM 3081 - VS FORTRAN
CH1
CH2  Calling sequence:  string = UPCASE(str)
CH2     Argument    Type   I/O                 Description
CH2     --------    ----   ---  ----------------------------------------
CH2     str         Ch*8    I   String to convert
CH2     string      Ch*8    O   String in upper case
CH2
CH2  Called by:  UPCASE
CH2
CH2  Calls: N.A.
CH2
CH3  Common Use: none
CH3
CH3 Significant Local Variables:
CH3     variable   type   ini. val.               description
CH3     --------   ----   ---------  -----------------------------------
ch3     oword      I*4        -      Output string in I*4
CH3     ostr       Ch*8       -      Ouput string
CH3     ch         Ch         -      Current character in string
CH3
CH4  Logical Units Used:   Unit #                Description
CH4                        ------    -----------------------------------
CH4
CH4  Method:
CH4     for i=1 to 8
CH4        if (character at i is in lower case) set to upper case
CH4        save character at position i to the output string
CH4     end for
CH4  end UPCASE
CH4
CH5  Modifications Between Versions:
CH5     Mod #   Modifier    Date                  Description
CH5     -----   --------  --------   -----------------------------------
CH5     1.04    A.Etienne 06/04/91   Added this routine to MAPGEN code
CH5     1.05    A.Etienne 06/07/91   Corrected error in equivalence stmt
CH5	2.00	E.S.Panduranga	06/25/91
CH5			Made the routine character set independent.
CH5			Changed variable name char to ch, since char is a
CH5			builtin function.
CH5
CH5 $Log: upcase.f,v $
CH5 Revision 1.3  2013/05/21 19:08:25  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.2  2002/12/26 17:16:32  irby
CH5 Fix variable declarations for f90 compatibility, e.g.:
CH5       INTEGER         EVTTJD*2
CH5 is properly declared as:
CH5       INTEGER*2       EVTTJD
CH5
CH5 Revision 1.1  2002/04/16 20:24:04  irby
CH5 New GRO tool intmap.
CH5
c Revision 2.1  1991/09/09  18:08:49  nancy
c First controlled version on the Sun.
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      character(8) function UPCASE(istr)

      integer     oword,i
      character   ch
      character(8) istr,ostr
      equivalence (oword,ch)

      character(80)	id
      common	/id/id
      id = '$Id: upcase.f,v 1.3 2013/05/21 19:08:25 irby Exp $'

      do i=1,8
         ch = istr(i:i)
         if (ch.ge.'a' .and. ch.le.'z')
     +        ch = char(ichar(ch) + ichar('A') - ichar('a'))
         ostr(i:i) = ch
      enddo

      upcase = ostr
      return
CCCCCCCCCCCCCCCCCCCCCCCC END MAPGEN.SOURCE(UPCASE) CCCCCCCCCCCCCCCCCCCCC
      end
