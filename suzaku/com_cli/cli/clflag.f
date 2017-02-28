C
C File: clflag.f (formerly flgrd.f)
C Description: set/get flags for control of command line
C Original: extracted from CHARD.FOR
C
C Public:
C   CLerok              ... discard the remaining input
C   Integer  CLILVL     ... command procedure nesting level, 0 for console
C   CLIlun              ... get current LUN of command input stream
C   LINRD               ... discard the remaining input by LINRD(1,1)
C   FLGRD/CLflaS        ... set flags, IFLQ & IFCR
C   FLARD/CLflaG        ... get flags, IFLQ & IFCR
C   CLstar              ... enable/disable temporary reading from console
C   CLIopt              ... set/get various CLI options, see also CLIset
C   CLLAST              ... returns the length of buffer remaining
C
C History:
C     10-Jun-1992, add Clerok
C     05-Feb-1992, import CLilvl, CLilun from clgetl.f
C     04-Apr-1994, add CLIopt
C     22-Feb-2005 Y.ISHISAKI, renamed from 'flgrd.f' into 'clflag.f'
C     22-Feb-2005 Y.ISHISAKI, CLstar() moved from clgetl.f
C     26-Feb-2005 Y.ISHISAKI, add entries CLflaS (=FLGRD), CLflaG (=FLARD)
C     26-Feb-2005 Y.ISHISAKI, CLlast moved from chard.f
C     26-Feb-2005 Y.ISHISAKI, removed dummy parameter of CLILVL
C
C ---------
C   CLerok      ... discard the remaining input
C ---------
      Subroutine CLEROK()
      Implicit None
C begin
      Call Linrd(1,1)
      Return
      End
C
C ----------
C   CLLAST      ... returns the length of internal input buffer remaining
C ----------
      Subroutine CLlast( Length )
      Implicit NONE
C common
      include 'clidef.inc'
      include 'clitxt.inc'
C output
      Integer  Length
C begin
      Length = MAX( cLAST+1-IPNT,0 )
C
      Return
      End
C
C ----------
C   Integer  CLILVL     ... command procedure nesting level, 0 for console
C ----------
      Integer  Function CLILVL()
      Implicit None
C common
      include 'clidef.inc'
      include 'clunit.inc'
C begin
      CLILVL = LUNPTR
      Return
      End
C
C ----------
C   CLILUN      ... get current LUN of command input stream
C ----------
      Subroutine CLilun( Lun )
      Implicit None
C
      include 'clidef.inc'
      Include 'clunit.inc'
C output
      Integer  Lun
C begin
      Lun = LUNLST(LunPtr)
      Return
      End
C
C ---------
C   LINRD   ... discard the remaining input by LINRD(1,1)
C ---------
      Subroutine LINRD(IFLQQ,IFCRR)
      Implicit None
C common
      include 'clidef.inc'
      include 'clflag.inc'
      include 'clitxt.inc'
C input
      Integer  IFLQQ, IFCRR
C begin
      IPNT = 0
      IFLQ = IFLQQ
      IFCR = IFCRR
      Return
      End
C
C ---------
C   FLGRD/CLflaS        ... set flags, IFLQ & IFCR
C ---------
      Subroutine FLGRD(IFLQin,IFCRin)
      Implicit None
C common
      include 'clflag.inc'
C input
      Integer  IFLQin, IFCRin
C entry
      Entry  CLflaS(IFLQin, IFCRin)
C begin
      IFLQ = IFLQin
      IFCR = IFCRin
C
      Return
      End
C
C ---------
C   FLARD/CLflaG        ... get flag, IFLQ & IFCRs
C ---------
      Subroutine FLARD(IFLQout,IFCRout)
      Implicit None
C common
      include 'clflag.inc'
C output
      Integer  IFLQout, IFCRout
C entry
      Entry  CLflaG(IFLQout, IFCRout)
C begin
      IFLQout = IFLQ
      IFCRout = IFCR
C
      Return
      End
C
C ----------
C   CLSTAR   ... enable/disable temporary reading from console
C ----------
       Subroutine CLSTAR( IICOM )
       Implicit NONE
C ARG
       Integer * 4  IICOM
C
C Common
       include 'clidef.inc'
       include 'clflag.inc'
       include 'clitxt.inc'
C
C BEGIN
       ICOM = IICOM
       cQUECOM = ' '
       Return
       End
C
C ----------
C   CLIopt   ... put/get various CLI options
C ----------
      Subroutine CLIopt( Copt,Ival )
      Implicit None
C input
      Character * (*)  Copt
      Integer  Ival
C common
      Include 'clflag.inc'
C local
      Logical  Lbreak
C function
      Logical  CLbreak
C begin
      If( Copt.eq.'AT' ) Then
        Opt_AT = Ival
      Else If( Copt.eq.'ECHO' ) Then
        Opt_ECHO = Ival
      Else If( Copt.eq.'BREAK' ) Then
        If ( 0.ne.Ival ) Ival = 1
        Lbreak = CLbreak( Ival, 0 )
      Else If( Copt.eq.'DEBUG' ) Then
        Opt_DEBUG = Ival
      Else If( Copt.eq.'ALIAS' ) Then
        Opt_ALIAS = Ival
      Else If( Copt.eq.'DOLLAR' ) Then
        Opt_DOLLAR = Ival
      Else If( Copt.eq.'PERCENT' ) Then
        Opt_PERCENT = Ival
      Else If( Copt.eq.'BSLASH' ) Then
        Opt_BSLASH = Ival
      Else If( Copt.eq.'INCO' ) Then
        Opt_INCO = Ival
      Else If( Copt.eq.'HISTORY' ) Then
        Opt_HISTORY = Ival
      Else If( Copt.eq.'EOF' ) Then
        Opt_EOF = Ival
      Else If( Copt.eq.'?AT' ) Then
        Ival = Opt_AT
      Else If( Copt.eq.'?ECHO' ) Then
        Ival = Opt_ECHO
      Else If( Copt.eq.'?BREAK' ) Then
        Ival = Opt_BREAK
      Else If( Copt.eq.'?DEBUG' ) Then
        Ival = Opt_DEBUG
      Else If( Copt.eq.'?ALIAS' ) Then
        Ival = Opt_ALIAS
      Else If( Copt.eq.'?DOLLAR' ) Then
        Ival = Opt_DOLLAR
      Else If( Copt.eq.'?PERCENT' ) Then
        Ival = Opt_PERCENT
      Else If( Copt.eq.'?BSLASH' ) Then
        Ival = Opt_BSLASH
      Else If( Copt.eq.'?INCO' ) Then
        Ival = Opt_INCO
      Else If( Copt.eq.'?HISTORY' ) Then
        Ival = Opt_HISTORY
      Else If( Copt.eq.'?EOF' ) Then
        Ival = Opt_EOF
      Else
        Write(*,*) 'CLIopt: unknown option: ', Copt
      End If
C
      Return
      End
