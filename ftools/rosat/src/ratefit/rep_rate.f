        SUBROUTINE REPORT(IO,MESS,A,FIX)
C
CC  Writes out parameter values to a unit
C
C************************ FFORM VERSION 1.0 ************ 22-MAR-88 08:22
C
CA  author : J. Bloch          date: ??-SEP-1987
CU  update : SLS               date: 10-MAR-1988 14:32
CU  update : SLS               date: 22-MAR-1988 08:22
C
CT  status: not tested
C
C   general description 
CG  Writes out parameter values to a unit
C
C   call_var.          type I/O description 
CP  IO                  I4  I   
CP  MESS                C       
CP  A                   R4  I   
CP  FIX                 C   I   
C
C   include_block name          description 
CI  R$COMMON:CGENL.CMN          general parameter common block
C
C   routines_called    type     description 
CR  HFLAG               R4      output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description 
CX 
C
C***********************************************************************
C
C   variables   meaning
C
c        IMPLICIT NONE
C
        INTEGER*4 IO
C
        REAL*4 I0, I1, I2, I3, I4, IS0, IS1, IP0, IP1, PHI
        REAL*4 TEMP(10), A(10)
C
        COMMON /DUMMY/ I0, I1, I2, I3, I4, IS0, IS1, IP0, IP1, PHI
C
        character(1) FIX(10)
        CHARACTER*(*) MESS
C
        EQUIVALENCE(TEMP(1),I0)
C
        DO 5 I=1,10
            TEMP(I) = A(I)
    5   ENDDO
C
        IF(IO .GT. 0) THEN
            WRITE(IO,*) MESS
            WRITE(IO,10) ' ',FIX(1),'I0=',I0,
     +                  '  ',FIX(2),'I1=',I1,
     +                  '  ',FIX(3),'I2=',I2,
     +                  '  ',FIX(4),'I3=',I3
            WRITE(IO,10) ' ',FIX(5),'I4=',I4,
     +                  '  ',FIX(6),'IS0=',IS0,
     +                  '  ',FIX(7),'IS1=',IS1
C            WRITE(IO,10) '  ',FIX(8),'IP0=',IP0,
C     +          '    ',FIX(9),'IP1=',IP1,
C     +          '    ',FIX(10),'PHI=',PHI
        ELSE
            WRITE(*,*) MESS
            WRITE(*,10) ' ',FIX(1),'I0=',I0,
     +                 '  ',FIX(2),'I1=',I1,
     +                 '  ',FIX(3),'I2=',I2,
     +                 '  ',FIX(4),'I3=',I3
            WRITE(*,10) ' ',FIX(5),'I4=',I4,
     +                 '  ',FIX(6),'IS0=',IS0,
     +                 '  ',FIX(7),'IS1=',IS1
C            WRITE(*,10) '  ',FIX(8),'IP0=',IP0,
C     +          '    ',FIX(9),'IP1=',IP1,
C     +          '    ',FIX(10),'PHI=',PHI
        ENDIF
   10   FORMAT(1H ,3A,F11.5,3A,F11.5,3A,F11.5,3A,F11.5)
C
        RETURN
        END
