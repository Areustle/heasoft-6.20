!**********************************************************************
!
!	I N T E G R A L   S C I E N C E   D A T A   C E N T E R
!
!	P A R A M E T E R   I N T E R F A C E   L I B R A R Y
!
! Copyright:	(c) 1998 ISDC http://obswww.unige.ch/isdc/
! File:		pil_f90_unix.f90
! Description:	Parameter Interface Library - ersatz USE F90_UNIX module
! Authors:	Jerzy.Borkowski@obs.unige.ch (JB)
! History:	17-Aug-98 (JB) : initial release
!
!**********************************************************************
!
! this is dummy module, never referenced in F90 code. It is used only to
! make NAG F90 compiler more/less compatible with the rest of the world.
! All other compilers predefine and know about GETARG and IARGC functions
! but stupid NAG F90 requires explicit USE F90_UNIX statement. Any hints
! how to implement conditional compilation in F90 source code ???
!
! 03-Feb-99: (JB) added required prototypes for IARGC and GETARG. Those are
! necessary for NAG f90 on Solaris


MODULE	F90_UNIX

INTERFACE IARGC
  FUNCTION IARGC()
    INTEGER     :: IARGC

  END FUNCTION IARGC
END INTERFACE

INTERFACE GETARG
  SUBROUTINE GETARG(IDX, ARGV)
    INTEGER		:: IDX
    CHARACTER*(*)	:: ARGV

  END SUBROUTINE GETARG
END INTERFACE

END MODULE F90_UNIX
