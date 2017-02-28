C
C File: iopen.f
C Description: Utility routine to open file
C
C History:
C     15-Jun-1992 A.Shirahashi, support 'ACCESS=DIRECT'
C     02-Mar-2005 Y.ISHISAKI, use CLaddext() instead of FILTYP()
C     28-May-2005 Y.ISHISAKI, Character * 256 -> Character * (LINSIZ)
C
C
C      Modified by FOREST ROUSE for PEP4 8/84.  It can now open files
C      by LOGICAL NAMES (It opens the file under the FORTRAN open with
C      a default file name supplied which has the EXT specified by the
C      User).
C
       Integer Function IOPEN(LU,RFILE,EXT,KIND)
       Implicit NONE
C common
      Include 'clidef.inc'
      Include 'clunit.inc'
C input
      Integer  LU
      Character * (*)  Rfile, Ext, Kind
C local
      Integer  LZ
      Character * (LINSIZ) File
      Character Access*10, Status*7, Form*11
C function
      Integer  Lenrd
C begin
      If (INDEX(KIND,'R').ne.0) Then
        Status = 'OLD'
      Else
        Status = 'NEW'
      End If
C
      If (INDEX(KIND,'A').ne.0) Then
        ACCESS = 'APPEND'
        Status = 'UNKNOWN'
      Else If(INDEX(KIND,'D').ne.0) Then
        ACCESS = 'DIRECT'
      Else
        ACCESS = 'SEQUENTIAL'
      End If
C
C      If (INDEX(KIND,'L').ne.0) Then
C        CARRIAGECONTROL = 'LIST'
C      Else
C       CARRIAGECONTROL = 'FORTRAN'
C      End If
C
      If (INDEX(KIND,'U').ne.0) Then
        FORM = 'UNFORMATTED'
      Else
        FORM = 'FORMATTED'
      End If
C
      LZ = LENRD(RFILE)
      If (RFILE(LZ:LZ).eq.':') Then
        If (Status(1:3).eq.'NEW' .or.
     &      Status(1:7).eq.'UNKNOWN') Then
          OPEN (LU,FILE=RFILE(1:LZ),STATUS=Status,FORM=FORM,
     &          ACCESS=ACCESS,ERR=99)
        Else If(Status(1:3).eq.'OLD') Then
          OPEN (LU,FILE=RFILE(1:LZ),STATUS=Status,FORM=FORM,
     &          ACCESS=ACCESS,ERR=99)
        Else
          Write(*,*) 'IOPEN: Unknown Status: ', Status
        End If
      Else
C
        File = Rfile
        Call CLaddext(File,Ext,Lz)
C
        If (Status(1:3).eq.'NEW') Then
          OPEN (UNIT=LU,FILE=FILE(1:LZ),STATUS='NEW',FORM=FORM,
     &          ACCESS=ACCESS,ERR=99)
        Else If(Status(1:3).eq.'OLD' .and.
     &          Access(1:6).ne.'DIRECT') Then
          OPEN (LU,FILE=FILE(1:LZ),STATUS='OLD',FORM=FORM,
     &          ACCESS=ACCESS,ERR=99)
        Else If(Status(1:3).eq.'OLD' .and.
     &          Access(1:6).eq.'DIRECT') Then
          OPEN (UNIT=LU,FILE=FILE(1:LZ),STATUS='OLD',
     &          FORM=FORM,ACCESS='DIRECT',ERR=99)
        ELse If(Status(1:7).eq.'UNKNOWN') Then
          OPEN (UNIT=LU,FILE=FILE(1:LZ),STATUS='UNKNOWN',
     &          FORM=FORM,ACCESS=Access,ERR=99)
        Else
          Write(*,*) 'IOPEN: Unknown Status: ', Status
        End If
      End If
      IOPEN = 0
      Return
C
   99 Continue
      IOPEN = -1
      Return
      End
