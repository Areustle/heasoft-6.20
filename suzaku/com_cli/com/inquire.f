C   11/11/86 612122030  MEMBER NAME  INQUIRE  (FORT)     M  FORTRAN
C
C History:
C     17-Jul-1998 Y.Ishisaki, COMKEY(PROMPT,RESP) -> COMKEY(PROMPT,RESP0,RESP)
C
C+++++ THIS IS INQUIRE.FOR
C
      Subroutine Inquire(quest,nval,names,help,nreply,lord)
C
      Implicit None
C     ...8-Jun-1992, A.Shirahashi
C     ...synonym for Inquir
C
      include 'comode.inc'
C input
      Integer  nval, nreply, lord(0:MAXENT)
      Character * (*)  quest, names(nval), help(nval)
C begin
      Call Inquir(quest,nval,names,help,nreply,lord)
      Return
      End
C
C
       Subroutine INQUIR(QUEST,NVAL,NAMES,HELP,NREPLY,LORD)
C
C@@    IMPLICIT NONE
C
       Include 'comode.inc'
C
       Integer * 4 NVAL,NREPLY,LORD(0:MAXENT)
       Character*(*) NAMES(NVAL),HELP(NVAL),QUEST
C
       Integer * 4 LABEL,NLEN,LPRMPT,J
       Integer * 4 IFLQ,IFCR,LENRD
       Logical * 4 NANSW
       Character * 256 RESP0,RESP,PROMPT
       Character * 12 Form
C
C..... ASK THE USER TO SELECT ONE OR MORE OPTIONS
C
       If (ICOMER.lt.0) Goto LABEL
       If (LUNCOM.eq.0) Call INICOM ('BUG')
       NLEN = LEN(NAMES(1))
 3     RESP = ' '
       PROMPT = '?'//COMPRM//'> '
       LPRMPT = 6
       LORD(0) = 0
C
C..... TYPE THE OPTIONS
C
   1    Continue
       Write (LUNCOM,100) COMPRM, QUEST
 100   Format (' '/' ',A,':  ',A,/' ')
       If (RESP.eq.'?') Then
         Call COMHLP(NVAL,NAMES,HELP)
       End If
       If (RESP.ne.'?' .or. HELP(1)(1:1).eq.'$') Then
         Lmax = 0
         Do i = 1, Nval
           Lmax = Max( Lmax,Lenrd(Names(i)) )
         End Do
         Lmax = Min( Lmax, 20 )
         Write(Form,110) Min(9,72/(Lmax+4))
 110     Format( '(',I1,'(1X,A))' )
         Write (LUNCOM,Form) (NAMES(i)(:Lmax),i=1,Nval)
       End If
C
C..... GET A KEYWORD
C
       NANSW = .FALSE.
       Call LINRD(1,1)
 2     Call FLARD(IFLQ,IFCR)
       If (NREPLY.gt.0) Then
         If (IFLQ.ne.0) Write (LUNCOM,102) COMPRM,NREPLY
 102     Format (' '/' ',A,': Select',I3,' Option')
 80      Assign 80 TO LABEL
         Call COMKEY(PROMPT(1:LPRMPT),RESP0,RESP)
         If (ICOMER.ne.1) Return
       Else
         If (IFLQ.ne.0) Write (LUNCOM,103) COMPRM,-NREPLY
 103     Format (' '/' ',A,': Select',I3,' or More, Then OK')
 81      Assign 81 TO LABEL
         Call COMKEY(PROMPT(1:LPRMPT),RESP0,RESP)
         If (ICOMER.gt.1 .and. LPRMPT.gt.6) Goto 3
         If (ICOMER.ne.1) Return
         If ((RESP.eq.'OK' .or. RESP.eq.'ok')
     &       .and. LORD(0).ge.-NREPLY) Then
           Call LINRD(1,1)
           Return
         End If
       End If
       If (RESP.eq.' ' .and. NANSW) Then
         NANSW = .FALSE.
         Call LINRD(1,1)
         Goto 2
       End If
       If (RESP.eq.' ' .or. RESP.eq.'?') Goto 1
       Call FLGRD(0,0)
       Call COMTCH(NAMES,NVAL,RESP(1:NLEN),J)
       If (J.le.0) Then
         Call LINRD(1,1)
         If (ICOMER.eq.3)  Return
         Goto 2
       End If
       LORD(0) = LORD(0)+1
       LORD(LORD(0)) = J
       If (LORD(0).eq.NREPLY) Then
         Call LINRD(1,1)
         Return
       End If
C
C..... NEXT KEYWORD
C
       NANSW = .TRUE.
       PROMPT = PROMPT(1:LPRMPT)//RESP0
       LPRMPT = LPRMPT+1+LENRD(RESP0)
       Goto 2
       End
C=====
       Subroutine INQUR2(QUEST,NVAL,NAMES,HELP,NREPLY,LANSWR)
C@@    IMPLICIT NONE
       Include 'comode.inc'
       Integer*4 NVAL,NREPLY,LANSWR
       Character*(*) QUEST,NAMES(NVAL),HELP(NVAL)
       Integer*4 LORD(0:MAXENT),J
C
C..... SAME AS INQUIRE, RETURNS A BIT PATTERN
C
       Call INQUIR(QUEST,NVAL,NAMES,HELP,NREPLY,LORD)
       If (ICOMER.ne.1) Return
       LANSWR = 0
       If (LORD(0).eq.0) Return
       Do 10 J = 1,LORD(0)
         LANSWR = IOR ( LANSWR, ISHFT(1,LORD(J)-1) )
10     Continue
       End
C=====
       Subroutine CHOSIT(QUEST,NVAL,NAMES,HELP,LANSWR)
C@@    IMPLICIT NONE
       Include 'comode.inc'
       Integer*4 NVAL,LANSWR
       Character*(*) QUEST,NAMES(NVAL),HELP(NVAL)
       Integer*4 LORD(0:1)
C
C..... SAME AS INQUIRE, FOR ONE VALUE ONLY
C
       Call INQUIR(QUEST,NVAL,NAMES,HELP,1,LORD)
       If (ICOMER.ne.1) Return
       LANSWR = LORD(1)
       End
