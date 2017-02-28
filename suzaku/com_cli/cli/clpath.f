C
C File: clpath.f
C Description: CLI command file search path support routines
C Author: Y.ISHISAKI, Tokyo Metropolitan University
C
C History:
C     01-Mar-2005 Y.ISHISAKI, file created & CLpath() moved from miscunix.f
C     09-Mar-2005 Y.ISHISAKI, use CLgetenv() instead of Getenv() in CLpath()
C
C Public: CLsetpath, CLpath, CLfindpath, CLaddext
C
C ----------
C   CLsetpath   ... set CLI command file serch path
C ----------
      Subroutine CLsetpath( PATH )
      Implicit NONE
C input
      Character * (*)  PATH
C common
      include 'clidef.inc'
      include 'clunit.inc'
C begin
      SEARCH_PATH = PATH
      LSEARCH_PATH = Len( PATH )
C
      Return
      End
C
C ----------
C   CLpath      ... set environmental variable as search path (obsolete)
C ----------
      Subroutine CLpath( ENV_NAME )
      Implicit NONE
C input
      Character * (*)  ENV_NAME
C common
      include 'clidef.inc'
      include 'clunit.inc'
C function
      Integer  Lenrd
C function
      Call CLgetenv( ENV_NAME, SEARCH_PATH )
      LSEARCH_PATH = Lenrd( SEARCH_PATH )
C
       Return
       End
C
C ----------
C   CLfindpath  ... find command file in search path
C ----------
      Subroutine CLfindpath( FILE, EXT, PATH, L )
      Implicit NONE
C input
      Character * (*)  FILE, EXT
C output
      Character * (*) PATH
      Integer  L
C common
      include 'clidef.inc'
      include 'clunit.inc'
C local
      Integer  I
      Logical  LEXIST
C function
      Integer  Lenrd
C function
      PATH = FILE
      L = Lenrd( FILE )
      INQUIRE( FILE=PATH(:L),EXIST=LEXIST,ERR=10 )
      If ( LEXIST ) Return
C
 10   Continue
      Call CLaddext( PATH, EXT, L )
      INQUIRE( FILE=PATH(:L),EXIST=LEXIST,ERR=20 )
      If ( LEXIST ) Return
C
 20   Continue
      If ( Index(FILE, '/').gt.0 ) Then
        L = -1
        Return
      End If
C
      I = 1
C
      Do While ( I.le.LSEARCH_PATH )
        PATH = SEARCH_PATH(I:)
        L = Index( PATH, ':' )
        I = I + L
        If ( L.eq.0 ) Then
          L = Lenrd( PATH ) + 1
          I = LSEARCH_PATH + 1
        End If
        If ( L.eq.1 ) Then
          L = -1                ! not found
          Return
        End If
C
        If ( PATH(L-1:L-1) .eq. '/' ) Then
          PATH(L:) = FILE
        Else
          PATH(L:) = '/' // FILE
        End if
        L = Lenrd( PATH )
C
ccc        Write (*,*) 'PATH=', Path(:L)
        INQUIRE( FILE=PATH(:L),EXIST=LEXIST,ERR=30 )
        If ( LEXIST ) Return
C
 30     Continue
        Call CLaddext( PATH, EXT, L )
ccc        Write (*,*) 'PATH=', Path(:L)
        INQUIRE( FILE=PATH(:L),EXIST=LEXIST,ERR=40 )
        If ( LEXIST ) Return
C
 40     Continue
      End Do
C
      L = -1
      Return
      End
C
C ----------
C   CLaddext    ... add file extention EXT to file name FILE
C ----------
      Subroutine CLaddext( FILE, EXT, L )
      Implicit None
C input
      Character * (*)  EXT
C input/output
      Character * (*)  FILE
C local
      Integer  I, L, LEXT
C function
      Integer  Lenrd
C BEGIN
      IF ( EXT.eq.' ' ) Return
      L = Lenrd(FILE)
      LEXT = Lenrd(EXT)
      Do I = L, 1, -1
        If ( '.'.eq.FILE(I:I) ) Then
          Return
        End If
        If ( '/'.eq.FILE(I:I) ) Then
          Goto 100
        End If
      End Do
C
 100  Continue
      IF( EXT(1:1).eq.'.' ) THEN
        If ( L + LEXT .le. Len(FILE) ) Then
          File(L+1:) = EXT(:LEXT)
          L = L + LEXT
        End If
      Else
        If ( L + LEXT + 1 .le. Len(FILE) ) Then
          File(L+1:) = '.'//EXT(2:LEXT)
          L = L + 1 + LEXT
        End If
      End If
C
      Return
      End
