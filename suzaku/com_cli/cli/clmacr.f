C
C File: clmacr.f
C Description: CLI Macro Support Routines
C Author: A.Shirahashi, Univ. of Tokyo
C
C History:
C     10-Aug-1992, CLmaci, 2 arguments (sym,str), CLmacr, $(sym)
C     26-Oct-1992, CLmaci -> CLsetv
C     27-Oct-1992, add CLmacS
C     18-Nov-1992, add CLgetv
C     11-Jan-1995, recycle symbol area by Y.Ishisaki
C     12-Sep-1995, Character*132 subst, output -> Character*256
C     01-Jan-1996, CLseti, CLsetf, CLsetd
C     04-Dec-1996 Y.ISHISAKI, COMMON definition move to 'clmacr.inc'
C               check macro buffer overflow
C               reuse macro of length = 0
C     09-Jun-1997 Y.ISHISAKI, bug fix in reusing macro of length = 0
C     25-Feb-2005 Y.ISHISAKI, add CLmacP, and sort macros before printing
C     25-Feb-2005 Y.ISHISAKI, removed CLmacR(). use CLeval() instead
C     27-Feb-2005 Y.ISHISAKI, removed CLmacL(). use CLgetv() instead
C     27-Feb-2005 Y.ISHISAKI, check TAB in CLmacS()
C     02-Mar-2005 Y.ISHISAKI, $(?symbol) support in CLgetv()
C     28-May-2005 Y.ISHISAKI, Character * 256 -> Character * (LINSIZ)
C
C Public: CLmacS, CLmacP, CLsetv, CLseti, CLsetf, CLsetd
C
C ********
C  CLmacS       ... replace one macro
C ********
      Subroutine CLmacS( input,length )
      Implicit None
C input
      Character * (*)  input
C input/output
      Integer  length
C common
      Include 'clidef.inc'
      Include 'clmacr.inc'
C local
      Integer  N, N1, M, M1, L
      Character * (MACBSIZ)  subst
      Character * (LINSIZ)  output
C begin
ccc      Write(*,*) 'CLmacr: ', length, '<',input(:length),'>'
      If( NMAC.le.0 ) Return
      If ( length .lt. Len(input) ) Then
        input(length+1:) = ' '
      End If
C
      N = Index(input,'$')
      If( N.eq.0 .or. N.ge.length ) Return
      N1 = N - 1        ! char position before '$'
C
      M = N             ! search for char position of symbol end
      Do While ( M.lt.length
     &     .and. input(M+1:M+1).ne.SPACE
     &     .and. input(M+1:M+1).ne.TAB )
        M = M + 1       ! char position of symbol end
      End Do
      M1 = M + 1        ! char position next to symbol end
C
      If( input(N+1:N+1).eq.'(' ) Then
        N = N + 2
        M = Index( input(N:), ')' )
        If ( M.eq.0 ) Return            ! no matching ')'
        M = N + M - 2                   ! char position before ')'
        M1 = M + 2                      ! char position after ')'
      Else
        N = N + 1
      End If
      Call CLgetv( input(N:M),subst,L )
      If( L.gt.0 ) Then                ! zero length macro equiv. undefined
        output = input(:N1)//subst(:L)//input(M1:length)
        length = N1 + L + (length-M1+1)
        input = output(:length)
      End If
ccc      Write(*,*) '/',input(:length),'/'
      Return
      End
C
C ********
C  CLgetv       ... Macro Lookup
C ********
      Subroutine CLgetv( INPUT,OUTPUT,LENGTH )
      Implicit None
C input
      Character * (*)  INPUT
C output
      Character * (*)  OUTPUT
      Integer  LENGTH
C common
      Include 'clmacr.inc'
C local
      Integer  I
C begin
      LENGTH = LEN( INPUT )
      If ( LENGTH.le.0 ) Return
      If ( LENGTH.gt.1 .and. INPUT(1:1).eq.'?' ) Then
        LENGTH = LENGTH - 1
        Do I = NMAC, 1, -1
          If( LENGTH.eq.LKMAC(I)
     &         .and. INPUT(2:).eq.MACKEY(I)(:LENGTH)
     &         .and. LMMAC(I).gt.0 ) Then
            LENGTH = 1
            OUTPUT = '1'
            Return
          End If
        End Do
        LENGTH = 1
        OUTPUT = '0'
        Return
      End If
C
      Do I = NMAC, 1, -1
        If( LENGTH.eq.LKMAC(I) .and. INPUT.eq.MACKEY(I)(:LENGTH) ) Then
          LENGTH = LMMAC(I)
          OUTPUT = MACBUF(I)(:LENGTH)
          Return
        End If
      End Do
C
      LENGTH = -1
      Return
      End
C
C ********
C  CLmacP       ... Macro Print
C ********
      Subroutine CLmacP
      Implicit None
C common
      Include 'clmacr.inc'
C local
      Integer  I, J, LKTMP, LMTMP
      Character * (MACKSIZ)  TMPKEY
      Character * (MACBSIZ)  TMPBUF
C begin
      Do I = 1, NMAC - 1
        Do J = I + 1, NMAC
          If ( LMMAC(J).gt.0 ) Then     ! check for zero length for far side
            If ( LMMAC(I).le.0          ! check for zero length for current
     &      .or. MACKEY(I).gt.MACKEY(J) ) Then ! compare keys
              LKTMP = LKMAC(J)
              LMTMP = LMMAC(J)
              TMPKEY = MACKEY(J)
              TMPBUF = MACBUF(J)
              LKMAC(J) = LKMAC(I)
              LMMAC(J) = LMMAC(I)
              MACKEY(J) = MACKEY(I)
              MACBUF(J) = MACBUF(I)
              LKMAC(I) = LKTMP
              LMMAC(I) = LMTMP
              MACKEY(I) = TMPKEY
              MACBUF(I) = TMPBUF
            End If
          End If
        End Do
ccc        Write(*,*) I, MACKEY(I)(:LKMAC(I))//'='//MACBUF(I)(:LMMAC(I))
        If ( LMMAC(I).le.0 ) Then
          NMAC = I - 1
          Goto 100
        End If
      End Do
C
 100  Continue
      If ( NMAC.gt.0 .and. LMMAC(NMAC).le.0 ) Then      ! check for last item
        NMAC = NMAC - 1
      End if
C
      Do I = 1, NMAC
ccc        If ( LMMAC(I).gt.0 ) then
          Write(*,*) MACKEY(I)(:LKMAC(I))//'='//MACBUF(I)(:LMMAC(I))
ccc        End If
      End Do
C
      Call CLstatus('VAR')
C
      Return
      End
C
C ********
C  CLsetv       ... Macro Install
C ********
      Subroutine CLsetv( symbol,string )
      Implicit None
C ARG
      Character * (*) symbol, string
C
C COMMON
      Include 'clmacr.inc'
C local
      Integer  I, Lsym
C function
      Integer  Lenrd
C BEGIN
      Lsym = Lenrd(symbol)
C
      If ( 0.eq.Lsym ) then
        Call CLmacP
        Return
      End If
C
      Do I = NMAC, 1, -1
        If ( Lsym.eq.LKMAC(I) ) Then
          If ( symbol(:Lsym).eq.MACKEY(I)(:Lsym) ) Goto 200
        End If
      End Do
C
      Do I = NMAC, 1, -1
        If ( Lsym.eq.LKMAC(I) ) Then
          If ( LMMAC(I).le.0 ) Goto 200 ! zero length macro equiv. undefined
        End If
      End Do
C
      If( NMAC.ge.MAXMAC ) Then
        Call CLIerr(2,'too many macros')
        Return
      End If
C
      NMAC = NMAC + 1
      I = NMAC
 200  Continue
C
      MACKEY(I) = symbol(:Lsym)
      LKMAC(I) = Lsym
      MACBUF(I) = string
      LMMAC(I) = Len(string)
C
      Return
      End
C
C
      Subroutine CLseti( symbol,value )
      Implicit None
C input
      Character * (*)  symbol
      Integer  value
C local
      Integer  length
      Character * 32  varstr
C function
      Integer  Lenrd
C begin
      Call CLitoa( value, varstr )
      length = Lenrd( varstr )
      Call CLsetv( symbol, varstr(:length) )
C
      Return
      End
C
C
      Subroutine CLsetf( symbol,value )
      Implicit None
C input
      Character * (*)  symbol
      Real * 4 value
C local
      Integer  length
      Character * 32  varstr
C function
      Integer  Lenrd
C begin
      Call CLftoa( value, varstr )
      length = Lenrd( varstr )
      Call CLsetv( symbol, varstr(:length) )
C
      Return
      End
C
C
      Subroutine CLsetd( symbol,value )
      Implicit None
C input
      Character * (*)  symbol
      Real * 8  value
C local
      Integer  length
      Character * 32  varstr
C function
      Integer  Lenrd
C begin
      Call CLdtoa( value, varstr )
      length = Lenrd( varstr )
      Call CLsetv( symbol, varstr(:length) )
C
      Return
      End
