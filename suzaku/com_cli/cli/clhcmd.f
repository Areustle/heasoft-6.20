C
C File: clhcmd.f
C Description: Collection of routines to support HISTORY facility
C Author: A.Shirahashi
C
C Routines: CLhcmd, CLhins, CLhrec, Clhget, CLhall
C
C History:
C     21-Oct-1992, renamed from CLhins.f
C     20-Feb-2005 Y.ISHISAKI, insert 'Implicit None'
C     21-Feb-2005 Y.ISHISAKI, include 'clhcmd.inc' for common
C     21-Feb-2005 Y.ISHISAKI, show all history with single '!'
C     21-Feb-2005 Y.ISHISAKI, allow direct numbering by '!number'
C     02-Mar-2005 Y.ISHISAKI, allow !n:nword in CLhcmd()
C     02-Mar-2005 Y.ISHISAKI, allow history command at mid of line
C     02-Mar-2005 Y.ISHISAKI, add CLHEVAL()
C     28-May-2005 Y.ISHISAKI, Character * 256 -> Character * (LINSIZ)
C
C ----------
C   CLHEVAL     .... replace history command
C ----------
      Subroutine CLheval( input,output,outlen )
      Implicit None
C input
      Character * (*) input
C output
      Character * (*) output
      Integer  outlen
C common
      include 'clflag.inc'
C local
      Integer  I, L
C begin
      output = input
      outlen = Min( Len(input), Len(output) )
      If ( Opt_HISTORY.eq.0 ) Return
C ... symbol substition
      I = 1
      Do While ( I.le.outlen )
        If ( output(I:I).eq.'!' ) Then
          L = outlen - I + 1
          Call CLhcmd( output(I:),L )
          outlen = Min( I + L - 1, Len(output) )
        End If
        I = I + 1
      End Do
C
      Return
      End
C
C ----------
C   CLHCMD      .... replace one history command
C ----------
      Subroutine CLhcmd( input,length )
      Implicit None
C input/output
      Character * (*)  input
      Integer  length
C common
      include 'clidef.inc'
      include 'clhcmd.inc'
C local
      Character * 1  C
      Integer  N, N1, M1, L
      Integer  no, iword
      Character * (LINSIZ)  subst, output
C begin
ccc      Write (*,*) '* CLhcmd: ', input(:length), ', len=', length
      iword = -1
      If ( length .lt. Len(input) ) Then
        input(length+1:) = ' '
      End If
C
      N = Index(input,'!')
      If( N.eq.0 .or. N.ge.length ) Return
      N1 = N - 1        ! char position before '!'
C
      If ( input(N+1:N+1).eq.'!' ) Then
        no = -1
        M1 = N + 2
        If ( input(N+2:N+2).eq.':' ) Then
          Call CLharg( input(N+3:), iword, L )
          M1 = N + 3 + L
          If ( L.eq.0 ) iword = -1
        End If
      Else If ( input(N+1:N+1).eq.'-' ) Then
        Call CLharg( input(N+2:), no, L )
        no = - no
        If ( L.eq.0 ) Then
          no = -1
        End If
        M1 = N + 2 + L
        If ( input(M1:M1).eq.':' ) Then
          Call CLharg( input(M1+1:), iword, L )
          M1 = M1 + 1 + L
          If ( L.eq.0 ) iword = -1
        End If
      Else
        Call CLharg( input(N+1:), no, L )
        If ( L.eq.0 ) Then
          no = 0                ! search string
          M1 = N + 1
          Do While ( M1.le.length )
            C = input(M1:M1)
            If ( C.eq.SPACE .or. C.eq.TAB ) Then
              Goto 100
            Else if ( C.eq.':' ) Then
              Call CLharg( input(M1+1:), iword, L )
              M1 = M1 + 1 + L
              If ( L.eq.0 ) iword = -1
              Goto 100
            End If
            M1 = M1 + 1
          End Do
 100      Continue
        Else
          M1 = N + 1 + L
          If ( input(M1:M1).eq.':' ) Then
            Call CLharg( input(M1+1:), iword, L )
            M1 = M1 + 1 + L
            If ( L.eq.0 ) iword = -1
          End If
        End if
      End If
C
      subst = input(N:M1-1)
      Call CLhget( no, iword, subst, L )
ccc      Write (*,*) 'no=',no,' iword=',iword,' L=',L, ' subst=',subst(:L)
ccc      Write (*,*) 'N1=',N1, ' M1=', M1
      If ( L.le.0 ) Then
        output = input(:N1)//input(M1:length)
        length = N1 + (length-M1+1)
      Else
        output = input(:N1)//subst(:L)//input(M1:length)
        length = N1 + L + (length-M1+1)
      End If
      input = output(:length)
C
      Return
      End
C
C ----------
C   CLHINS   .... insert history
C ----------
       Subroutine CLHINS( LINE )
       Implicit None
C input
       Character * (*)  LINE
C common
      include 'clidef.inc'
      include 'clhcmd.inc'
C begin
      If( IHEAD.eq.ITAIL .and. NENT.ge.MAXHIS ) Then
        ITAIL = MOD( ITAIL+1,MAXHIS )
      Else
        NENT = NENT + 1
      End If
      HISBUF( IHEAD ) = LINE
      LHIS( IHEAD ) = Len( LINE )
      IHEAD = MOD( IHEAD+1,MAXHIS )
C
      Return
      End
C
C ----------
C   CLHGET   .... get line/word from history buffer
C ----------
       Subroutine CLhget( no,iword,buffer,length )
       Implicit None
C input
       Integer  no, iword
C input/output
       Character * (*)  buffer
C output
       Integer  length
C common
      include 'clidef.inc'
      include 'clhcmd.inc'
C local
      Integer I, L, irecal
C function
      Integer Lenrd
C begin
       length = -1
       If ( no.gt.0 ) Then
c ... direct number
         If ( no.gt.Nent ) Goto 999
         irecal = MOD(ITAIL + no - 1, MAXHIS)
       Else If ( no.lt.0 ) Then
c ... relative number
         If ( Nent+no.lt.0 ) Goto 999
         irecal = IHEAD + no
         If ( irecal.lt.0 ) Then
           irecal = irecal + MAXHIS
         End If
       Else If ( no.eq.0 ) Then
c ... search mathing string
         irecal = IHEAD
         L = Lenrd( buffer )
         Do I = 1, Nent
           irecal = irecal - 1
           If ( irecal.lt.0 ) irecal = irecal + MAXHIS
           If ( buffer(2:L).eq.Hisbuf(irecal)(1:L-1) ) Then
C             write(*,*) L,'<',buffer(2:L),'><',
C     &                  Hisbuf(irecal)(1:L-1),'>'
             Goto 100
           End If
         End Do
         Goto 999
       End If
C
 100   Continue
       If ( iword.ge.0 ) Then
         Call CLpart2(Hisbuf(irecal), iword+1, buffer, length)
         If ( length.eq.0 ) Then
           Call CLIerr(0, 'bad word selector')
         End If
       Else
         buffer = Hisbuf(irecal)
         length = Lhis(irecal)
       End If
C
       Return
C
 999   Continue
       Call CLIerr(0, 'no match in history buffer')
       Return                   ! no match, return with length = -1
       End
C
C ----------
C   CLHREC   .... recall history
C ----------
      Subroutine CLHREC( IDIR,LINE,LENGTH )
      Implicit None
C input
      Integer * 4  IDIR
C output
      Integer * 4  LENGTH
      Character * (*)  LINE
C common
      include 'clidef.inc'
      include 'clhcmd.inc'
C local
      Integer * 4  IRECAL, NRECAL
C begin
       If( IDIR.eq.0 ) Then
         IRECAL = IHEAD
         NRECAL = 0
         Return
       End If
C
       If( NENT.eq.0 ) Then
         LENGTH = 0
         Return
       End If
C
       If( IDIR.lt.0 ) Then
         If( NRECAL.lt.NENT ) Then
           NRECAL = NRECAL + 1
           IRECAL = IRECAL - 1
           If( IRECAL.lt.0 ) IRECAL = MAXHIS-1
         End If
         LINE = HISBUF( IRECAL )
         LENGTH = LHIS( IRECAL )
       Else If( IDIR.gt.0 ) Then
         If( NRECAL.eq.0 ) Then
           LENGTH = 0
           Return
         Else If( NRECAL.gt.1 ) Then
           NRECAL = NRECAL - 1
           IRECAL = MOD( IRECAL+1,MAXHIS )
         End If
         LINE = HISBUF( IRECAL )
         LENGTH = LHIS( IRECAL )
       End If
C
       Return
       End
C
C ----------
C   CLHALL   .... print all
C ----------
      Subroutine CLHALL()
      Implicit None
C common
      include 'clidef.inc'
      include 'clhcmd.inc'
C local
      Integer * 4  I, J, L
C begin
       If( NENT.eq.0 ) Then
         Call CLIerr(0, 'no history in current buffer')
         Return
       End If
C
       J = ITAIL
       Do I = 1, NENT
         L = LHIS( J )
         Write (*, '(1X,I3,1X,A)') I, HISBUF( J )(:L)
         J = MOD( J+1,MAXHIS )
       End Do
C
       Return
       End
C
C ----------
C   CLHARG   .... read history number argument and string length
C ----------
       Subroutine CLHARG( word, num, length )
       Implicit None
C input
       Character * (*)  word
C output
       Integer  num, length
C local
       Character * 1  C
       Integer  I
C begin
       num = 0
       length = 0
       Do I = 1, Len(word)
         C = word(I:I)
         If ( C.ge.'0' .and. C.le.'9' ) Then
           num = num * 10 + ICHAR(C) - ICHAR('0')
           length = length + 1
         Else
           Return
         End if
       End Do
C
       Return
       End
