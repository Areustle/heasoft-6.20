C
C File: keyrd.f
C Description: Collection of routines to parse keyword
C
C History:
C     10-Aug-1992, allows alias for @directive
C     27-Aug-1992, more care for list of avaiable keywords
C     05-Oct-1992, systematic mode flag
C     28-May-2005 Y.ISHISAKI, Character * 256 -> Character * (LINSIZ)
C     11-Nov-2005 Y.ISHISAKI, Character * 11 -> 16  Form in KeyRD
C
C Public: Keyrd, Ambrd
C
      Subroutine KeyRD(MODE,PROMPT,CWORD,CNAME,NNAME,Igo)
      Implicit None
C
C Mode =  0 : several line mode
C         1 : one line mode
C        -1 : no line control/no alias/force input
C        -2 : no line control/alias substituion/force input
C        -3 : no line contrl/alias substitution/just return
C
C Bit (0:1) 00: serveral line mode
C           01: one line mode
C           10: no line control
C Bit (2)    0: disables alias substitution
C            1: enables alias substitution
C Bit (3)    0: force input in case of error
C            1: just return in case of error
C
C 0010 (2) =  2 (D) := no line control/disables alias/force input
C 0110 (2) =  6 (D) := no line control/enables alias/force input
C 1110 (2) = 14 (D) := no line control/enables alias/just return
C
C common
      include 'clidef.inc'
      include 'clunit.inc'
C input
      Integer  Mode, Nname
      Character * (*)  Prompt, Cname(Nname)
C input/output
      Character * (*)  Cword
C output
      Integer  Igo
C local
      Character * 16  Form
      Character * (LINSIZ)  WORK, Input
      Integer  lp, il, I, Lmode, Lname, Lform, Lin
      Integer  Iali
      Logical  aliasf, forcef
C function
      Integer  Lenrd
C data
      Save  Iali
      Data  Iali / 0 /
C
C..... Get a keyword with a list of possible answers
C
      If( Mode.gt.0 ) Then
        Lmode = 1
      Else If( Mode.eq.0 ) Then
        Lmode = 0
      Else If( Mode.lt.0 ) Then
        Lmode = -1
      End If
C
      If( Mode.eq.-2 .or. Mode.eq.-3 .or.
     &    (Mode.gt.0 .and. Iand(Mode,4).ne.0) ) Then
        aliasf = .TRUE.
      Else
        aliasf = .FALSE.
      End If
C
      If( Mode.eq.-3 .or.
     &    (Mode.gt.0 .and. Iand(Mode,8).ne.0) ) Then
        forcef = .FALSE.
      Else
        forcef = .TRUE.
      End If
C
      Work = Prompt
C
      If( Lmode.ge.0 ) Call Linrd(1,Abs(Lmode))
   10 Continue
      If( Lmode.ge.0 ) Cword = ' '
      Input = Cword
      If( Lmode.ge.0 .and. Prompt(1:1).ne.'?' ) Then
        lp = Lenrd(work)
        Call Txtrd('?'//Work(:lp),Input)
      Else
        Call Txtrd(Prompt,Input)
      End If
      Lin = Lenrd(Input)
      Cword = Input
      If (Cword(1:1).eq.' ') Then
        Call Linrd(1,Abs(Lmode))
        Goto 10
      End If
C
C ...alias substitution (9-June-1992, A.Shirahashi)
      If( aliasf .and. Iali.eq.0 ) Then
        Call CLalir( Input,Work,il )
        If( il.gt.0 ) Then
C         Write(*,*) 'ALIAS:',Work(1:il),':'
          If( Work(1:1).eq.'@' ) Then
            Call CLicmd( Work,il )
          Else
            Call Ugetrd( Work,il )
            Iali = 1
          End If
          Goto 10
        End If
      Else
        Iali = 0
      End If
C
C ...check help, validity
      If ( Input(:Lin).eq.'?' ) Goto 30
      Call AmbRD(Cname,Nname,Input(:Lin),Igo)
      If ( Igo.gt.0 ) Then
        Lname = Lenrd(Cname(Igo))
        Cword(:Lname) = Cname(Igo)
      End If
      If (.not.forcef .or. Igo.gt.0) Return
      Work = Cword
C
C ...dump keyword tables if error occured, this is ugly!
C        but requested by T.Takahashi.
      If( Igo.eq.0 ) Then
        Call CLIERR( 2,'no such keyword: '//Input(:Lin) )
        Goto 40
      Else If( Igo.lt.0 ) Then
        Call CLIERR( 1,'ambiguous keyword: '//Input(:Lin) )
        Goto 40
      End If
C
C ...I like the following simple messages.
C     If( Igo.eq.0 ) Call CLIERR( 2,'no such keyword: '//WORK )
C     If( Igo.lt.0 ) Call CLIERR( 1,'ambiguous keyword: '//WORK )
C
      Call Linrd(1,Abs(Lmode))
      Goto 10
C
   30 Continue
      Write (*,100)
  100 Format ( ' CLI: Available Keywords:' )
C
   40 Continue
      Write(*,'(1X)')
C     Write(Form,110) Min(9,72/(Len(Cword)+4))
C 110 Format( '('I1'(4XA))' )
      Lname = 0
      Do I = 1, Nname
        Lname = Max(Lname,Lenrd(Cname(I)))
      End Do
      Write(Form,110) Min(9,72/(Lname+4)), Lname
  110 Format( '(',I1,'(4X,A',I2,'))' )
      Lform = Lenrd(Form)
      Write(*,Form(:Lform)) (Cname(I),I=1,Nname)
      Write(*,'(1X)')
C
      Call Linrd(1,Abs(Lmode))
      Goto 10
C
      End
C
C
      Subroutine AmbRD(TABL,NTAB,ELEM,IT)
      Implicit None
C input
      Integer  NTAB
      Character * (*)  TABL(NTAB)
C input/output
      Character * (*)  ELEM
C output
      Integer  IT
C local
      Character * 64  TEST, COMP
      Integer  LCOMP, LTEST, LMATCH, NPAR, IPAR, I
C function
      Integer  LENRD
C
C  String maching routine, return the table element
C
      LCOMP = LENRD(ELEM)
      COMP = ELEM(:LCOMP)
      Call CLstrupc(LCOMP, COMP)
C
C COMPARE
C
      NPAR = 0
      LMATCH = 0
      Do 10 I=1,NTAB
        TEST = TABL(I)
        LTEST = LENRD(TEST)
        Call CLstrupc(LTEST, TEST)
        If      ( COMP(:LCOMP).eq.TEST(:LTEST) ) Then
          IT = I
          ELEM = TABL(IT)
          Return
        Else If ( COMP(:LCOMP).eq.TEST(:LCOMP) ) Then
          NPAR = NPAR + 1
          If ( LMATCH.lt.LCOMP ) NPAR = 1
          IPAR = I
          LMATCH = LCOMP
        Else If ( COMP(:LTEST).eq.TEST(:LTEST) ) Then
          If ( LMATCH.lt.LTEST ) Then
            NPAR = 1
            IPAR = I
            LMATCH = LTEST
          Else If ( LMATCH.eq.LTEST ) Then
            NPAR = NPAR + 1
            IPAR = I
          End If
        End If
10    Continue
C
C   Match(It>0), no match (It=0) ambiguous (It<0)
C
      If( Npar.eq.1 ) Then
        IT = IPAR
        ELEM=TABL(IT)
      Else If( Npar.eq.0 ) Then
        IT = 0
      Else
        IT = -1
      End If
C
      Return
      End
