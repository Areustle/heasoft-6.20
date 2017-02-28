C
C History:
C     27-Jul-2011, Y.Ishisaki, moved from modval.f for gfortran-4.6 --pedantic
C
C====================
C
      Function Lcmval( VALUE )
      Implicit  None
      Logical*4 VALUE, LCMVAL
C
      LCMVAL = VALUE
      Return
      End
C
C====================
C
      Function Icmval( value )
      Implicit  None
      Integer*4 VALUE, ICMVAL
C
      ICMVAL = VALUE
      Return
      End
C
C====================
C
      Function Rcmval( value )
      Implicit  None
      Real*4 VALUE, RCMVAL
C
      RCMVAL = VALUE
      Return
      End
C
C====================
C
      Real * 8  Function Dcmval(value)
      Implicit None
      Real * 8  value
C
      Dcmval = value
      Return
      End
C
C=====================
C
      Character * (*) Function Ccmval(value)
      Implicit  None
      Character*(*) VALUE
C
      CCMVAL = VALUE
      Return
      End
C
       Subroutine LCMMOD(L,J,ISTAT)
C@@    Implicit  None
c__    Include 'comode.inc'
       Include 'comode.inc'
       Integer*4 J, ISTAT
       Logical*4 L(J), M
       Character*8 TEXT
C
C..... MODIFY A Logical FROM DESCRIPTOR
C
       Call GETTXT('?L*4',TEXT)
       If (ICOMER.ne.1) Return
       ISTAT = 2
       If (TEXT.eq.' ') Return
       Read ( TEXT, '(L1)' ) M
       L(J) = M
       ISTAT = 1
       End
C
C====================
C
      Subroutine Icmmod(i,j,istat)
      Implicit  None
      Include 'comode.inc'
      Integer*4 J, ISTAT
      Integer*4 I(J), K
      Character*16 TEXT
CFACOM      Character*5  RUNFRM
CFACOM      Integer*4    LTEXT
CFACOM      Integer*4    LKBRD
C
      Call GETTXT('?I*4',TEXT)
      If (ICOMER.ne.1) Return
      ISTAT = 2
      If (TEXT.eq.' ') Return
CFACOMLTEXT = LKBRD ( TEXT, 0 )
CFACOMWRITE ( RUNFRM, '(A,I2,A)' ) '(I',LTEXT,')'
CFACOMLTEXT = LKBRD ( RUNFRM, 0 )
CFACOMREAD ( TEXT, RUNFRM(1:LTEXT) ) K
      Read ( text,* ) k
      I(J) = K
      ISTAT = 1
      End
C
C====================
C
      Subroutine Rcmmod(r,j,istat)
      Implicit  None
      Include 'comode.inc'
      Integer*4 J, ISTAT
      Real*4 R(J), S
      Character*24 TEXT
C
      Call GETTXT('?R*4',TEXT)
      If (ICOMER.ne.1) Return
      ISTAT = 2
      If (TEXT.eq.' ') Return
CFACOMREAD ( TEXT, '(F12.5)' ) S
      Read( text,* ) s
      R(J) = S
      ISTAT = 1
      End
C
C===================
C
      Subroutine Dcmmod(R,J,ISTAT)
      Implicit  None
      Include 'comode.inc'
      Integer*4 J, ISTAT
      Real*8 R(J), S
      Character*24 TEXT
C
      Call GETTXT('?R*8',TEXT)
      If (ICOMER.ne.1) Return
      ISTAT = 2
      If (TEXT.eq.' ') Return
CFACOMREAD ( TEXT, '(F12.5)' ) Return
      Read ( text, * ) s
      R(J) = S
      ISTAT = 1
      Return
      End
C
C====================
C
      Subroutine Ccmmod(c,j,istat)
      Implicit None
      Include 'comode.inc'
      Integer*4 J, ISTAT
      Character C(J)*(*), TEXT*64
C
      Call GETTXT('?C* ',TEXT)
      If (ICOMER.ne.1) Return
      ISTAT = 2
      If (TEXT.eq.' ') Return
      C(J) = TEXT
      ISTAT = 1
      Return
      End
C
C----------
C CHRLEN
C----------
C
      Integer Function CHRLEN ( ARRAY )
C
C ARGUMENTS
C
      Character * (*) ARRAY (*)
C
C MAIN
C
      CHRLEN = LEN ( ARRAY(1) )
      Return
      End
C
C----------
C CHRCPY
C----------
C
      Subroutine CHRCPY ( INARY, OUTARY, IND )
C
C ARGUMENTS
C
      Character * (*)  INARY(*), OUTARY(*)
      Integer * 4      IND
C
C MAIN
C
      OUTARY ( IND ) = INARY ( IND )
      Return
      End
