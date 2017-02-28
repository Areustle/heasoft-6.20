C
C File: miscunix.f
C Description: Miscellaneous I/O routines for UNIX
C
C History:
C     01-Apr-1994, add CLsigI, CLsigC, CLsigR, CLsigX, CLsigF
C                      remove dummy routines
C     05-Jun-1998 Y.ISHISAKI, CLsig*() moved to clsignal.c
C     17-Feb-2005 Y.ISHISAKI, add CLstrupc(), CLstrdwc(), CLstricmp()
C     20-Feb-2005 Y.ISHISAKI, remove IPFCMD()
C     20-Feb-2005 Y.ISHISAKI, set CLI variable "?" in CLexec()
C     20-Feb-2005 Y.ISHISAKI, remove IFTSS(),TPUT(),TGET(),IFATN(),RSTATN()
C     02-Mar-2005 Y.ISHISAKI, CLmkpn() removed
C     02-Mar-2005 Y.ISHISAKI, CLpath() moved to clpath.f
C     02-Mar-2005 Y.ISHISAKI, Filtyp() moved to clpath.f & renamed CLaddext()
C     09-Mar-2005 Y.ISHISAKI, remove CLgetc()
C     09-Mar-2005 Y.ISHISAKI, add functions CLfgetc/CLfputc/CLftell()
C     09-Mar-2005 Y.ISHISAKI, add subroutines CLfseek/CLgetenv/CLexit()
C     09-Mar-2005 Y.ISHISAKI, CLGETS(), CLPUTS() moved from clgetl.f
C     28-May-2005 Y.ISHISAKI, Character * 256 -> Character * (LINSIZ)
C     10-Nov-2005 Y.ISHISAKI, check buffer size in CLexec()
C     04-Jun-2006 Y.ISHISAKI,
C        CLsyso, CLfgetc, CLfputc, CLfseek, CLftell moved to g77/95_fileio.f
C     02-Feb-2006 Y.ISHISAKI, LISTC removed, and moved into CLISTC in qopen.f
C
C Public:
C     CLitty, CLowrt, CLexec, CLfdel,
C     CLstrupc, CLstrdwc, CLstricmp,
C     CLgetenv, CLexit, CLGETS, CLPUTS
C
      Logical Function CLitty()
      Implicit None
C
      Logical cli__Fisatty
C begin
      CLitty = cli__Fisatty(0)
C
      Return
      End
C
C
      Subroutine CLowrt( filename )
      Implicit None
C
C     ... delete a file to be overwritten (for UNIX)
C const
      Include 'clidef.inc'
C input
      Character * (*)  filename
C local
      Character * (LINSIZ)  file
      Logical  fexist, yes
      Integer  L
C function
      Integer  Lenrd
C begin
      L = Lenrd(filename)
      Inquire( File=filename(:L),Exist=fexist,Err=900 )
CDEBUGWrite(*,*) 'CLowrt:',filename(:L),':',fexist
      If( fexist ) Then
        yes = .False.
        file = filename(:L)
        Call Affirm(
     &    QUOTE//file(:L)//QUOTE//' already exists, delete',yes)
        If( yes ) Then
          Call cli__Fdelete( filename(:L) )
        End If
      End If
 900  Continue
      Return
      End
C
C
      Subroutine CLexec( comand,input,output )
      Implicit None
C const
      Include 'clidef.inc'
C input
      Character * (*)  comand, input, output
C local
      Integer  L, Lcom, Lin, Lout
      Character * (LINSIZ)  cmdline
C function
      Integer  Lenrd, CLexitcode
C begin
      Lcom = Lenrd(comand)
      Lin = Lenrd(input)
      Lout = Lenrd(output)
C
      If ( Lin.eq.0 .and. Lout.eq.0 ) Then
        Call cli__Fsystem( comand(:Lcom) )
        Goto 999
      End If
C
      If ( Lcom .gt. Len(cmdline) ) Then
        Write(*,*)
     &       'CLexec: too long command, redirection ignored'
        Call cli__Fsystem( comand(:Lcom) )
        Goto 999
      End If
C
      cmdline(:Lcom) = comand(:Lcom)
C
      If( Lin.gt.0 ) Then
        L = Lcom+Lin+2
        If ( L .gt. Len(cmdline) ) Then
          Write(*,*)
     &       'CLexec: too long command, input redirection ignored'
        Else
          cmdline(Lcom+1:L) = ' <'//input(:Lin)
          Lcom = L
        End If
      End If
C
      If( Lout.gt.0 ) Then
        L = Lcom+Lout+2
        If ( L .gt. Len(cmdline) ) Then
          Write(*,*)
     &       'CLexec: too long command, output redirection ignored'
        Else
          cmdline(Lcom+1:L) = ' >'//output(:Lout)
          Lcom = L
        End If
      End If
C
CDEBUGWrite(*,*) Lin,Lout,Lcom,'[',cmdline(:Lcom),']'
C
      Call cli__Fsystem( cmdline(:Lcom) )
C
 999  Continue
C
      Call CLseti( '?', CLexitcode() )
C
      Return
      End
C
C
      Subroutine CLfdel( file )
      Implicit None
C
C input
      Character * (*)  file
C local
      Integer  Lfile
C function
      Integer  Lenrd
C begin
      Lfile = Lenrd(file)
CDEBUGWrite(*,*) 'CLfdel:file=[',file(:Lfile),']'
      Call cli__Fdelete( file(:Lfile) )
      Return
      End
C
C
      Subroutine CLstrupc( LEN, S )
      Implicit None
C
C ARG
      Integer  LEN
      Character * (*)  S
C LOCAL
      Integer  I
C
      Entry STRUPC( LEN, S )
C
C BEGIN
      Do I = 1, LEN
        If( S(I:I).ge.'a' .and. S(I:I).le.'z' ) Then
          S(I:I) = Char( Ichar(S(I:I)) - Ichar('a') + Ichar('A') )
        End If
      End Do
      Return
      End
C
C
      Subroutine CLstrdwc( LEN, S )
      Implicit None
C
C ARG
      Integer  LEN
      Character * (*)  S
C LOCAL
      Integer  I
C
C BEGIN
      Do I = 1, LEN
        If( S(I:I).ge.'A' .and. S(I:I).le.'Z' ) Then
          S(I:I) = Char( Ichar(S(I:I)) - Ichar('A') + Ichar('a') )
        End If
      End Do
      Return
      End
C
C
      Integer Function CLstricmp( S1, S2 )
      Implicit None
C
C ARG
      Character * (*)  S1, S2
C LOCAL
      Character * 1  C1, C2
      Integer  I, L1, L2
C
C BEGIN
      L1 = LEN(S1)
      L2 = LEN(S2)
C
      Do I = 1, L1
        If ( I .gt. L2 ) Then
          CLstricmp = 1
          Return
        End If
        C1 = S1(I:I)
        C2 = S2(I:I)
        If ( C1.ge.'a' .and. C1.le.'z' ) Then
          C1 = Char( Ichar(C1) - Ichar('a') + Ichar('A') )
        End If
        If ( C2.ge.'a' .and. C2.le.'z' ) Then
          C2 = Char( Ichar(C2) - Ichar('a') + Ichar('A') )
        End If
        If ( C1 .lt. C2 ) Then
          CLstricmp = -1
          Return
        End If
        If ( C1 .gt. C2 ) Then
          CLstricmp = 1
          Return
        End If
      End Do
C
      If ( L1 .eq. L2 ) Then
        CLstricmp = 0
        Return
      End If
C
      CLstricmp = -1
      Return
      End
C
C
      Subroutine CLgetenv( Name, Value )
      Implicit None
C ... equivalent to Getenv(), get the value of environmental variable
C input
      Character * (*)  Name
C output
      Character * (*)  Value
C begin
      Call Getenv(Name, Value)
C
      Return
      End
C
C
      Subroutine CLexit( Status )
      Implicit None
C ... equivalent to Exit(), exit the program with status
C input
      Integer  Status
C begin
      Call Exit( Status )
C
      Return
      End
C
C ----------
C   CLGETS   ... read a single line from input stream
C ----------
      Integer Function CLGETS( LUN,BUFFER,LENGTH )
      Implicit None
C common
      include 'clidef.inc'
      include 'clunit.inc'
C ARG
      Integer * 4  LUN
      Character * (*)  buffer
      Integer * 4  LENGTH
C local
      Character * 1  C, C2
      Integer * 4  I, L, IERR
C function
      Integer * 4  CLfgetc
C begin
      If ( LUN.ne.5 ) Goto 900
C ... need a special treatment on redirection of STDIN for g77
      L = LEN(buffer)
      Read( LUN,'(A)',End=800 ) buffer(:L)
C ... deprive trailing blnaks
      Do Length = L,1,-1
        If ( buffer(Length:Length) .ne. ' ' ) Goto 120
      End Do
      Length = 0
 120  Continue
      buffer(Length+1:) = ' '
C
      CLgets = 0
      Return
C
 800  Continue
      CLgets = -1
      Return
C
 900  Continue
C ... use CLfgetc() when LUN != 5
      I = 1
      Length = 0
      CLgets = 0
C ... read until CR or LF
      Do While (.TRUE.)
        IERR = CLfgetc( LUN, C )
        If ( 0.ne.IERR ) Then
ccc          Write (*,*) 'IERR=', IERR
          If ( Length.eq.0 ) Then
            CLgets = -1
          Else If ( Length.lt.Len(buffer) ) Then
            buffer(Length+1:) = ' '
          End If
          Return
        Else If ( C.eq.CRET .or. C.eq.LINEFEED ) Then
          Goto 10
        Else If ( I.le.Len(buffer) ) Then
          buffer(I:I) = C
          Length = I
          I = I + 1
        End If
      End Do
C
 10   Continue
      If ( Length.lt.Len(buffer) ) Then
        buffer(Length+1:) = ' '
      End If
ccc      Write (*,*) 'lun=',lun,' L=',Length,' buffer=',buffer(:Length)
C ... check for UNIX, LF only
      If ( C.eq.LINEFEED ) Return
C ... check for Mac, CR only
      IERR = CLfgetc( LUN, C2 )
      If ( IERR.ne.0 ) Return
C ... CR+LF detected, probably DOS/Windows
      If ( C2.eq.LINEFEED ) Return
C ... back to one char for Mac
      Call CLfseek(LUN, -1, 1)
C
      Return
      End
C
C ----------
C   CLPUTS   ... write a single line into file
C ----------
      Integer Function CLPUTS( LUN,BUFFER )
      Implicit None
C common
      include 'clidef.inc'
      include 'clunit.inc'
C ARG
      Integer * 4  LUN
      Character * (*)  buffer
C local
      Integer * 4  I, IERR
C function
      Integer * 4  CLfputc
C begin
      CLputs = 0
C
      Do I = 1, Len(buffer)
        IERR = CLfputc( LUN, buffer(I:I) )
        If ( 0.ne.IERR ) Goto 999
      End Do
C
      IERR = CLfputc( LUN, LINEFEED )
      If ( 0.eq.IERR ) Return
C
 999  Continue
ccc      Write (*,*) 'IERR=', IERR
      CLputs = -1
C
      Return
      End
