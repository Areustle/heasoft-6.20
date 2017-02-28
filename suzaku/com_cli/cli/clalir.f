C
C File: clalir.f
C Description: CLI Alias Support Routines
C Author: A.Shirahashi, Univ. of Tokyo
C
C History:
C     29-Sep-1992, add CLalip
C     15-Feb-1993, check duplicate registration
C     18-Aug-1995 Y.ISHISAKI, MAXALI 100 -> 1024
C     24-Aug-1995 Y.ISHISAKI, COMMON definition move to clalir.inc
C     22-Feb-1997 Y.ISHISAKI, add CLaliU
C     22-Feb-2005 Y.ISHISAKI, rename MAC -> ALI
C     25-Feb-2005 Y.ISHISAKI, sort aliases in CLaliP
C     11-Nov-2005 Y.ISHISAKI, Character * 80 -> (LINSIZ) Buffer in CLaliR
C     11-Nov-2005 Y.ISHISAKI, Character * 16 -> (LINSIZ) CARGS(10) in CLaliR
C     11-Nov-2005 Y.ISHISAKI, ignore # in alias in CLaliR
C     11-Nov-2005 Y.ISHISAKI, Character * 3 -> 16 Pstr in CLaliR
C
C     Subroutine CLaliR( input,output,length )
C     Subroutine CLaliL( input,output,length )
C     Subroutine CLaliI( key,value )
C     Subroutine CLaliP
C     Subroutine CLaliU( key )
C
C ********
C  CLALIR  Alias Replace
C ********
      Subroutine CLaliR( INPUT,OUTPUT,LENGTH )
      Implicit None
C common
      include 'clidef.inc'
C ARG
      Character * (*)  INPUT, OUTPUT
      Integer * 4      LENGTH
C
C VAR
      Integer * 4  NARGS, MARGS, LARGS(10)
      Character * 16  Pstr
      Character * (LINSIZ)  CARGS(10)
      Character * (LINSIZ)  Buffer
      Integer * 4  i, j, iargs
C
C FUNCION
      Integer  Lenrd
C
C BEGIN
      length = Lenrd(input)
      Call CLALIL( INPUT,OUTPUT,LENGTH )
      If( LENGTH.lt.0 ) Return
C
      NARGS = 0
      MARGS = 0
      Do 100 I = 1, LENGTH
C       If( OUTPUT(I:I).eq.'%' .or. OUTPUT(I:I).eq.'#' ) Then
        If( OUTPUT(I:I).eq.'%' ) Then
          NARGS = NARGS + 1
          Read( OUTPUT(I+1:I+1),'(I1)' ) IARGS
          MARGS = MAX( MARGS,IARGS )
        End If
100   Continue
      If( NARGS.eq.0 ) Return
C
      Do 200 I = 1, MARGS
        Write(Pstr,'(1H?,I1,1H?)') I
        Call TXTRD( Pstr,CARGS(I) )
        LARGS(I) = LENRD( CARGS(I) )
200   Continue
C
      I = 1
      J = 1
300   Continue
C        If( OUTPUT(I:I).eq.'%' .or. OUTPUT(I:I).eq.'#' ) Then
        If( OUTPUT(I:I).eq.'%' ) Then
          Read( OUTPUT(I+1:I+1),'(I1)' ) IARGS
          Buffer(J:J+LARGS(IARGS)-1) = CARGS(IARGS)(:LARGS(IARGS))
          I = I + 2
          J = J + LARGS(IARGS)
        Else
          Buffer(J:J) = OUTPUT(I:I)
          I = I + 1
          J = J + 1
        End If
      If( I.le.Length ) Goto 300
C
      Length = J - 1
      OUTPUT = ' '
      OUTPUT(:Length) = Buffer(:Length)
C     PRINT *, 'Length=',Length
C     PRINT *, 'Buffer=',Buffer(:Length)
C
      Return
      End
C
C ********
C  CLALIL  Alias Lookup
C ********
      Subroutine CLaliL( INPUT,OUTPUT,Length )
      Implicit None
C ARG
      Character * (*)  INPUT, OUTPUT
      Integer * 4      Length
C local
      Integer * 4  i
C include
      Include 'clalir.inc'
C function
      Integer * 4  Lenrd
C BEGIN
      Length = Lenrd(input)
      Do 100 I = NALI, 1, -1
C       PRINT *, Length,LKALI(I)
C       PRINT *, '/',INPUT(:Length),'/',ALIKEY(I)(:Length),'/'
        If( Length.ne.LKALI(I) ) Goto 100
        Call CLstrupc( Length,INPUT )
        If( INPUT(:Length).eq.ALIKEY(I)(:Length) ) Then
          OUTPUT = ALIBUF(I)(:LMALI(I))
          Length = LMALI(I)
          Return
        End If
100   Continue
C
      Length = -1
      Return
      End
C
C ********
C  CLALII  Alias Install
C ********
      Subroutine CLaliI( key, value )
      Implicit None
C ARG
      Character * (*) key, value
C include
      Include 'clalir.inc'
C local
      Integer  Lkey, Iali
      Character * 80  ckey
C function
      Integer * 4  Lenrd
C BEGIN
C     ... check if already registered alias
      Lkey = Lenrd(key)
      ckey(:Lkey) = key
      Call CLstrupc(Lkey,ckey)
C
      Do Iali = Nali, 1, -1
        If( Lkey.eq.Lkali(Iali) ) Then
          If( ckey(:Lkey).eq.Alikey(Iali)(:Lkey) ) Goto 100
        End If
      End Do
C
      If( Nali.ge.MAXALI ) Then
        Call CLIerr(2,'too many aliases')
        Return
      End If
C
      Nali = Nali + 1
      Iali = Nali
C
 100  Continue
C
      alikey(Iali) = ckey(:Lkey)
      lkali(Iali) = Lkey
C
      alibuf(Iali) = value
      lmali(Iali) = Lenrd(value)
C
C     PRINT *, STRING
C     PRINT *, NALI
C     PRINT *, LKALI(NALI),':',ALIKEY(NALI)(:LKALI(NALI))
C     PRINT *, LMALI(NALI),':',ALIBUF(NALI)(:LMALI(NALI))
      Return
      End
C
C ********
C  CLALIP  Alias Print
C ********
      Subroutine CLaliP
      Implicit None
C include
      Include 'clalir.inc'
C local
      Integer  I, J, LKmax, LKTMP, LMTMP
      Character * (ALIKSIZ)  TMPKEY
      Character * (ALIBSIZ)  TMPBUF
C begin
      Do I = 1, NALI - 1
        Do J = I + 1, NALI
          If ( ALIKEY(I).gt.ALIKEY(J) ) Then
            LKTMP = LKALI(J)
            LMTMP = LMALI(J)
            TMPKEY = ALIKEY(J)
            TMPBUF = ALIBUF(J)
            LKALI(J) = LKALI(I)
            LMALI(J) = LMALI(I)
            ALIKEY(J) = ALIKEY(I)
            ALIBUF(J) = ALIBUF(I)
            LKALI(I) = LKTMP
            LMALI(I) = LMTMP
            ALIKEY(I) = TMPKEY
            ALIBUF(I) = TMPBUF
          End If
        End Do
      End Do
C
      LKmax = 0
      Do I = 1, NALI
        LKmax = Max( LKmax, LKali(I) )
      End Do
C
      Do I = 1, NALI
        Write(*,*) AliKey(I)(:LKmax), ' ', AliBuf(I)(:LMali(I))
      End Do
C
      Call CLstatus('ALIAS')
C
      Return
      End
C
C ********
C  CLALIU  Alias Uninstall
C ********
      Subroutine CLaliU( key )
      Implicit None
C ARG
      Character * (*) key
C include
      Include 'clalir.inc'
C local
      Integer  Lkey, Iali, Jali
      Character * 80  ckey
C function
      Integer * 4  Lenrd
C BEGIN
C     ... check if already registered alias
      Lkey = Lenrd(key)
      ckey(:Lkey) = key
      Call CLstrupc(Lkey,ckey)
C
      Do Iali = Nali, 1, -1
         If( Lkey.eq.Lkali(Iali) ) Then
            If( ckey(:Lkey).eq.Alikey(Iali)(:Lkey) ) Then
               If ( Iali.lt.Nali ) Then
                  Do Jali = Iali, Nali-1
                     alikey(Jali) = alikey(Jali+1)
                     lkali(Jali) = lkali(Jali+1)
                     alibuf(Jali) = alibuf(Jali+1)
                     lmali(Jali) = lmali(Jali)
                  End Do
               End If
               Nali = Nali - 1
               Return
            End If
         End If
      End Do
C
      Return
      End
