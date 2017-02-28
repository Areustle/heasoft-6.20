C+
C (Filename) CHVAL
C (Purpose ) COM Utility Routine (extended version of MODVAL)
C (Author  ) A.Shirahashi
C (Date    ) 8-Jan-1987
C
C History:
C     09-Jun-1992, CNVFLT -> CLFTOA => CLI.OLB
C     17-Jul-1998 Y.Ishisaki, COMKEY(PROMPT,RESP) -> COMKEY(PROMPT,RESP,RESP)
C     17-Jul-1998 Y.ISHISAKI, Character*64 RESP, TEXT -> Character*256
C     04-Jun-2006 Y.ISHISAKI, remove unused variable, NAME1, in SHVAL()
C-
       Subroutine CHVAL( QUEST,NVAL,NAMES,HELP,VARTYP,ARRAY )
C
C ARG
       Integer   *  4  NVAL
       Character * (*) NAMES(NVAL), HELP(NVAL), QUEST
       Character * (*) VARTYP
       Integer   *  4  ARRAY(NVAL)
C
C COMMON
       Include 'comode.inc'
C
C VAR
C
       Integer * 4  IBUF(MAXENT)
       Real    * 4  RBUF(MAXENT)
       Logical * 4  LBUF(MAXENT)
       Equivalence( IBUF,RBUF,LBUF )
C
       Logical   * 4  MFLAG(MAXENT), MODIFY
       Character * 1  VTYPE(MAXENT)
C
       Integer   *  4  MAXNAM
       Character * 256 RESP, TEXT
C
       Integer * 4  LABEL
C
C FUNC
       Integer * 4  LENRD
C
C BEGIN
       If( ICOMER .lt. 0 ) Goto LABEL
       If( LUNCOM .eq. 0 ) Call INICOM( 'BUG' )
C
       If( NVAL .gt. MAXENT ) Then
         Write( LUNCOM,1000 ) NVAL
1000     Format( ' COM: Too Many Entries: ',I3,'in CHVAL' )
         Return
       End If
C
       If( LENRD( VARTYP ) .eq. 1 ) Then
         Do 10 J = 1, NVAL
           VTYPE( J ) = VARTYP( 1:1 )
10       Continue
       Else
         Do 20 J = 1, NVAL
           VTYPE( J ) = VARTYP( J:J )
20       Continue
       End If
C
       MAXNAM = 0
       Do 100 J = 1, NVAL
         If( VTYPE(J) .ne. 'I' .and.
     &       VTYPE(J) .ne. 'R' .and.
     &       VTYPE(J) .ne. 'L'
     &   ) Then
           Write( LUNCOM,1010 ) VTYPE(J)
1010       Format( ' COM: Unknown Variable Type: ',A1,' in CHVAL' )
           Return
         End If
         IBUF( J ) = ARRAY( J )
         MFLAG( J ) = .FALSE.
         MAXNAM = MAX( MAXNAM, LENRD( NAMES( J ) ) )
100    Continue
C
200    Continue
       RESP = ' '
       MODIFY = .FALSE.
C
300    Continue
C       IF( .NOT. BATCH ) CALL CM$CLR
       Write( LUNCOM,1020 ) COMPRM,QUEST
1020   Format( ' ',/,' ',A,':  ',A,/,' ' )
C
       If( RESP .eq. '?' ) Then
         Call COMHLP( NVAL,NAMES,HELP )
       End If
C
       Do 400 J = 1, NVAL
         Call SHVAL( NAMES(J)(1:MAXNAM),VTYPE(J),
     &               ARRAY(J),MFLAG(J),IBUF(J) )
400    Continue
C
       Call LINRD( 1,1 )
500    Continue
       Call FLARD( IFLQ,IFCR )
       If( .not.MODIFY ) Then
         If( IFLQ .ne. 0 ) Then
           Write( LUNCOM,1030 ) COMPRM
1030       Format( ' ',/,' ',A,': Press <ENTER> to Accept' )
         End If
800      Assign 800 TO LABEL
         Call COMKEY( '?'//COMPRM//'> ',RESP,RESP)
         If( ICOMER.ne.1 ) Return
         If( RESP.eq.' ' .or. RESP.eq.'OK' .or. RESP.eq.'ok' ) Then
           Call LINRD(1,1)
           Return
         End If
       Else
         If( IFLQ .ne. 0 ) Then
           Write( LUNCOM,1040 ) COMPRM
1040       Format( ' ',/,' ',A,': Type OK to Accept' )
         End If
810      Assign 810 TO LABEL
         Call COMKEY( '?'//COMPRM//'> ',RESP,RESP)
         If( ICOMER.gt.1 ) Goto 200
         If( ICOMER.ne.1 ) Return
         If( RESP.eq.'OK' .or. RESP.eq.'ok' ) Then
           Do 600 J = 1, NVAL
             ARRAY( J ) = IBUF( J )
600        Continue
           Call LINRD(1,1)
           Return
         End If
       End If
       If( RESP.eq.' ' .or. RESP.eq.'?' ) Goto 300
       Call FLGRD( 0,0 )
       Call COMTCH( NAMES,NVAL,RESP(1:MAXNAM),J )
       If( J .le. 0 ) Then
         Call LINRD( 1,1 )
         If( ICOMER .eq. 3 ) Return
         Goto 500
       End If
C
820    Assign 820 TO LABEL
       Call GETTXT( '?'//VTYPE(J)//'*4',TEXT )
       If( ICOMER .ne. 1 ) Return
       If( TEXT .eq. ' ' ) Goto 700
C
       If( VTYPE(J) .eq. 'I' ) Then
         Read( TEXT,*,ERR=700 ) IBUF(J)
       Else If( VTYPE(J) .eq. 'R' ) Then
         Read( TEXT,*,ERR=700 ) RBUF(J)
       Else If( VTYPE(J) .eq. 'L' ) Then
         Read( TEXT,*,ERR=700 ) LBUF(J)
       End If
C
       MODIFY = .TRUE.
       MFLAG( J ) = .TRUE.
       Goto 500
C
700    Continue
       Call LINRD(1,1)
       Write( LUNCOM,* ) 'COM: Eroor in Decoding ',RESP(1:MAXNAM)
       Goto 500
C
       End
C
C (* SUBROUTINE SHVAL *)
       Subroutine SHVAL( NAME,VTYPE,VAL,MFLAG,NEW )
C
C ARG
       Character * (*)  NAME
       Character *  1   VTYPE
       Integer   *  4   VAL
       Logical   *  4   MFLAG
       Integer   *  4   NEW
C
C COMMON
       Include 'comode.inc'
C
C VAR
       Integer * 4  VALI, NEWI
       Real    * 4  VALR, NEWR
       Logical * 4  VALL, NEWL
       Equivalence( VALI,VALR,VALL )
       Equivalence( NEWI,NEWR,NEWL )
C
       Character * 16  STRVAL, STRNEW
       Integer   *  4  LENVAL, LENNEW
C
C BEGIN
       VALI = VAL
       NEWI = NEW
C
       If( VTYPE .eq. 'I' ) Then
         Write( STRVAL,* ) VALI
         Write( STRNEW,* ) NEWI
       Else If( VTYPE .eq. 'R' ) Then
         Call CLFTOA( VALR,STRVAL )
         Call CLFTOA( NEWR,STRNEW )
       Else If( VTYPE .eq. 'L' ) Then
         Write( STRVAL,* ) VALL
         Write( STRNEW,* ) NEWL
       End If
C
       LENVAL = LENRD( STRVAL )
       LENNEW = LENRD( STRNEW )
C
       If( MFLAG ) Then
         Write( LUNCOM,* ) NAME,' = ',STRVAL(1:LENVAL),
     &                                      ' ',STRNEW(1:LENNEW)
       Else
         Write( LUNCOM,* ) NAME,' = ',STRVAL(1:LENVAL)
       End If
C
       Return
       End
