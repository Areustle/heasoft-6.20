!**********************************************************************
!
!	I N T E G R A L   S C I E N C E   D A T A   C E N T E R
!
!	P A R A M E T E R   I N T E R F A C E   L I B R A R Y
!
! Copyright:	(c) 1998 ISDC http://obswww.unige.ch/isdc/
! File:		pilfdemo.f90
! Copyright (c) 1998 ISDC http://obswww.unige.ch/isdc/
!
! this program demonstrates how to call PIL library from Fortran 90 code
! note : this program uses only PIL library (calls PILINIT)
! so it is _NOT_ appropriate for ISDC executables which should
! call COMMON_INIT. Should you wish to see sample ISDC compliant
! program look in the common library directory ...
!
! This program does _NOTHING_ usefull, it simply displays all
! parameters from parameter file prompting for new value, if
! default value is out of range.
!


PROGRAM PILFDEMO

USE PIL_F90_API			! use definitions from module file.


INTEGER 		:: r, ival, bval
REAL (KIND=8)		:: rval
CHARACTER*200		:: sval
INTEGER			:: i3(3)
REAL (KIND=4)		:: f5(5)
REAL (KIND=8)		:: d2(2)

r = PILINIT()
write (*,*) 'return from PILINIT : ', r

r = PILGETBOOL("iavgreb", bval)
write (*,*) 'return from PILGETBOOL : ', r, " value read = ", bval

r = PILGETINT("ipow2", ival)
write (*,*) 'return from PILGETINT : ', r, " value read = ", ival

r = PILGETREAL("rescale", rval)
write (*,*) 'return from PILGETREAL : ', r, " value read = ", rval

! please note that sval is padded with blanks (SPACES) so write (*,*)
! writes several empty lines

r = PILGETSTRING("mytitle", sval)
write (*,*) 'return from PILGETSTRING : ', r, " value read = ", sval

r = PILGETFNAME("infile", sval)
write (*,*) 'return from PILGETFNAME : ', r, " value read = ", sval

r = PILGETBOOL("iavgreb", bval)
write (*,*) 'return from PILGETBOOL : ', r, " value read = ", bval

r = PILGETINTVECTOR("i3", 3, i3(1))
write (*,*) 'return from PILGETINTVECTOR : ', r, " values read = ", i3

r = PILGETREALVECTOR("d2", 2, d2(1))
write (*,*) 'return from PILGETREALVECTOR : ', r, " values read = ", d2

r = PILGETREAL4VECTOR("f5", 5, f5(1))
write (*,*) 'return from PILGETREAL4VECTOR : ', r, " values read = ", f5

r = PILCLOSE(0)
write (*,*) 'return from PILCLOSE : ', r, " value read = ", bval

END
