C
C+++++ This is COMHELP.FOR             B. Gabioud Nov 1982
C
      Subroutine COMHLP(NVAL,NAMES,HELP)
C
      Implicit None
C constant
      Character * 1  BSLA
C common
      Include 'comode.inc'
C input
      Integer  NVAL
      Character*(*) NAMES(NVAL), HELP(NVAL)
C local
      Integer  J, LH
C function
      Integer  Lenrd
C
C begin
       BSLA = CHAR(92)
       If (HELP(1)(1:1).eq.'$'.or.HELP(1)(1:1).eq.BSLA) Then
         Call CGETHL ( HELP(1)(2:) )
C        PRINT *, '%COM-F-HLPFIL, External Help is not supported yet.'
       Else
         Do 10 J = 1,NVAL
           LH = Lenrd(HELP(J))
           If (NAMES(J).ne.' ' .or. NAMES(J-1).ne.' ' .or.
     &           HELP(J).ne.' ' .or. HELP(J-1).ne.' ')
     &         Write (LUNCOM,104) NAMES(J),HELP(J)(1:LH)
10       Continue
       End If
       Write (LUNCOM,*)
104    Format (1X,A,2X,A)
       Return
       End
C
C----------
C HLPLIB
C----------
C
       Subroutine HLPLIB ( DSN )
C
C ARGUMENTS
C
       Character * (*)   DSN
C
C COMMON
C
       Common / COM_HL / HLPDSN, LHLP
       Character * 40    HLPDSN
       Integer * 4       LHLP
C
C VARIABLES
C
       Logical * 4       STATUS
C
C FUNCTIONS
C
       Integer * 4       LKBRD
C
C MAIN
C
C      CALL DSNCHK ( DSN, STATUS )
       INQUIRE ( FILE=DSN, EXIST=STATUS )
       If ( .not. STATUS ) Then
         PRINT *, '%COM-F-NOHLP, Help Dataset not found'
         PRINT *, 'STATUS = ', STATUS
         PRINT *, 'DSN = ', DSN
       Else
         HLPDSN = DSN
         LHLP = LKBRD ( HLPDSN, 0 )
       End If
C
       Return
       End
C
C----------
C CGETHL
C----------
C
       Subroutine CGETHL ( HELP )
C
C ARGUMENTS
C
       Character * (*)  HELP
C
C COMMON
C
       Common / COM_HL / HLPDSN, LHLP
       Character * 40    HLPDSN
       Integer * 4       LHLP
C
C VARIABLES
C
       Logical * 4       STATUS
       Character * 80    MEMBER
       Integer * 4       LMEM
       Character * 72    LINBUF
C
C FUNCTIONS
C
       Integer * 4       LKBRD
C
C MAIN
C
       LMEM = LKBRD ( HELP, 0 )
       MEMBER = HLPDSN(1:LHLP)//'('//HELP(1:LMEM)//')'
C      CALL DSNCHK ( MEMBER, STATUS )
       INQUIRE ( FILE=MEMBER, EXIST=STATUS )
       If ( .not. STATUS ) Then
         PRINT *, '%COM-F-NOHLP, Help not found for the item'
       Else
C         OPEN ( UNIT=91, FILE=MEMBER, STATUS='OLD', FORM='FORMATTED',
C     #          ACTION='READ' )
         OPEN ( UNIT=91, FILE=MEMBER, STATUS='OLD', FORM='FORMATTED' )
10       Continue
           Read ( 91, '(A)', ERR=99, End=99 ) LINBUF
           Write ( 6, * ) LINBUF
         Goto 10
99       Continue
         CLOSE ( UNIT=91 )
       End If
       Return
       End
