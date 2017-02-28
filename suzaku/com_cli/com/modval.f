C   11/11/86 701081945  MEMBER NAME  MODVAL   (FORT)     M  FORTRAN
C+++++ THIS IS MODVAL.FOR
C
C History:
C     08-Jun-1992, A.Shirahashi, add Vartyp 'D' (R*8)
C     5-Jan-1992, A.Shirahashi, Double format -> 'G15.7'
C     13-Nov-1996 Y.Ishisaki, BSIZE 640 -> 2048
C     24-Jan-1997 Y.Ishisaki, BSIZE 2048 -> MAXENT*32
C     17-Jul-1998 Y.Ishisaki, COMKEY(PROMPT,RESP) -> COMKEY(PROMPT,RESP,RESP)
C     17-Jul-1998 Y.Ishisaki, Character*64 CCMVAL, RESP, BESP -> Character*256
C     27-Jul-2011 Y.Ishisaki, Use IBUF instead of CBUF in calling CHRCPY()
C     27-Jul-2011 Y.Ishisaki, make modval_sub.f for "gfortran-4.6 --pedantic"
C
      Subroutine MODVAL(QUEST,NVAL,NAMES,HELP,VARTYP,ARRAY)
C
      Implicit None
      Include 'comode.inc'
      Integer*4 NVAL
      Character*(*) NAMES(NVAL), HELP(NVAL), QUEST
      Character*1   VARTYP
      Integer * 4   ARRAY ( * )
      Integer*4 LABEL, ISTAT, I, J
      Integer*4 LENX, NLEN, IFLQ, IFCR
      Character*256 RESP, BESP
      Character*5 PREFIX        ! 'nnn: '
C
      Integer * 4   BSIZE
      Parameter ( BSIZE = MAXENT*32 )
      Logical*1 MODIF, MFLAG(MAXENT)
      Logical * 1      BUFFER (BSIZE)
      Logical * 4      LBUF (BSIZE/4)
      Integer * 4      IBUF (BSIZE/4)
      Real    * 4      RBUF (BSIZE/4)
      Real    * 8      DBUF (BSIZE/8)
      Character * 32   CBUF (BSIZE/32)
      Equivalence ( BUFFER, IBUF, LBUF, RBUF, DBUF, CBUF )
C funcion
      Logical*4 Lcmval
      Integer*4 Icmval
      Real*4    Rcmval
      Real*8    Dcmval
      Character*256 Ccmval
C
      Integer * 4     CHRLEN
C
C..... MODIFY A TABLE OF VALUES (L*4, I*4, R*4, R*8 OR CHAR)
C      Support for CHAR type was temporary removed in FACOM vers. (RSI)
C
      If (ICOMER.lt.0) Goto LABEL
      If (LUNCOM.eq.0) Call INICOM('BUG')
      If (VARTYP .ne. 'L' .and. VARTYP .ne. 'I' .and.
     &    VARTYP .ne. 'R' .and. VARTYP .ne. 'D' ) Then
        Write (LUNCOM,104) VARTYP
 104    Format (' COM: ILLEGAL VARIABLE TYPE:',A1,' IN MODVAL')
        Return
      End If
      If (VARTYP.eq. 'C') Then
        LENX = CHRLEN ( ARRAY )
      Else If(VARTYP.eq.'D') Then
        LENX = 8
      Else
        LENX = 4
      End If
      If (LENX*NVAL.gt.BSIZE) Then
        Write (LUNCOM,105) LENX*NVAL,BSIZE
 105    Format (' COM: TOO MANY CHARACTERS:',I5,'.gt.',I4,' IN MODVAL')
        Return
      End If
C
C..... SET THE INITIAL VALUES
C
 3     RESP = ' '
       MODIF = .FALSE.
       Do 10 I = 1, NVAL
         MFLAG(I) = .FALSE.
         If ( VARTYP .eq. 'C' ) Then
           Call CHRCPY ( ARRAY, IBUF, I )
         Else If( VARTYP .eq. 'D' ) Then
           ibuf( I*2-1 ) = array( I*2-1 )
           ibuf( I*2 ) = array( I*2 )
         Else
           IBUF ( I ) = ARRAY ( I )
         End If
10     Continue
C
C..... TYPE THE OPTIONS
C
 1     Continue
C      If ( .not. BATCH ) Call CM$CLR
       Write (LUNCOM,110) COMPRM,QUEST
 110   Format (' '/' ',A,':  ',A,/' ')
       If (RESP.eq.'?') Then
         Call COMHLP(NVAL,NAMES,HELP)
       End If
       If (VARTYP.eq.'L') Then
         Do 20 J = 1, NVAL
           Write (PREFIX, '(I3,A)') J, ': '
           If (MFLAG(J)) Then
             Write (LUNCOM,*) PREFIX,NAMES(J),' ',
     &       LCMVAL(ARRAY(J)),LBUF(J)
           Else
             Write (LUNCOM,*) PREFIX,NAMES(J), ' ',
     &       LCMVAL(ARRAY(J))
           End If
20       Continue
       Else If (VARTYP.eq.'I') Then
         Do 30 J = 1, NVAL
           Write (PREFIX, '(I3,A)') J, ': '
           If (MFLAG(J)) Then
             Write (LUNCOM,*) PREFIX,NAMES(J), ' ',
     &       ICMVAL(ARRAY(J)),IBUF(J)
           Else
             Write (LUNCOM,*) PREFIX,NAMES(J),' ',
     &       ICMVAL(ARRAY(J))
           End If
30       Continue
       Else If (VARTYP.eq.'R') Then
         Do 40 J = 1, NVAL
           Write (PREFIX, '(I3,A)') J, ': '
           If (MFLAG(J)) Then
             Write (LUNCOM,*) PREFIX,NAMES(J),' ',
     &       RCMVAL(ARRAY(J)),RBUF(J)
           Else
             Write (LUNCOM,*) PREFIX,NAMES(J),' ',
     &       RCMVAL(ARRAY(J))
           End If
40       Continue
       Else If (vartyp.eq.'D') Then
         Do J = 1, NVAL
           Write (PREFIX, '(I3,A)') J, ': '
           If (MFLAG(J)) Then
             Write(LUNCOM,'(1X,2A,1X,2G15.7)') PREFIX,NAMES(J),
     &            Dcmval(array(J*2-1)),DBUF(J)
           Else
             Write(LUNCOM,'(1X,2A,1X,1G15.7)') PREFIX,NAMES(J),
     &            Dcmval(array(J*2-1))
           End If
         End Do
       Else If (VARTYP.eq.'C') Then
         Do 50 J = 1, NVAL
           Write (PREFIX, '(I3,A)') J, ': '
           RESP = Ccmval(ARRAY(J))
           BESP = Ccmval(CBUF(J))
           If (MFLAG(J)) Then
             Write (LUNCOM,113) PREFIX,NAMES(J),
     &            RESP(1:LENX),BESP(1:LENX)
 113         Format (1X,2A,2X,A,2X,A)
           Else
             Write (LUNCOM,114) PREFIX,NAMES(J),
     &            RESP(1:LENX)
 114         Format (1X,2A,2X,A)
           End If
50       Continue
       End If
C
C..... GET A KEYWORD
C
       Call LINRD(1,1)
 2     Call FLARD(IFLQ,IFCR)
       If (.not.MODIF) Then
         If (IFLQ.ne.0) Write (LUNCOM,122) COMPRM
 122     Format (' '/' ',A,': Press <ENTER> to Accept')
 80      Assign 80 TO LABEL
         Call COMKEY('?'//COMPRM//'> ',RESP,RESP)
         If (ICOMER.ne.1) Return
         If (RESP.eq.' ' .or. RESP.eq.'OK' .or. RESP.eq.'ok') Then
           Call LINRD(1,1)
           Return
         End If
       Else
         If (IFLQ.ne.0) Write (LUNCOM,123) COMPRM
 123     Format (' '/' ',A,': Type OK to Accpet')
 81      Assign 81 TO LABEL
         Call COMKEY('?'//COMPRM//'> ',RESP,RESP)
         If (ICOMER.gt.1) Goto 3
         If (ICOMER.ne.1) Return
         If (RESP.eq.'OK' .or. RESP.eq.'ok') Then
C@@@       Call COMCPY(NVAL,BESCR,DESCR)
           Do 601 I = 1, NVAL
             If ( VARTYP .eq. 'C' ) Then
               Call CHRCPY ( IBUF, ARRAY, I )
             Else If( vartyp.eq.'D' ) Then
               array(i*2-1) = ibuf(i*2-1)
               array(i*2) = ibuf(i*2)
             Else
               ARRAY ( I ) = IBUF ( I )
             End If
601        Continue
           Call LINRD(1,1)
           Return
         End If
       End If
       If (RESP.eq.' ' .or. RESP.eq.'?') Goto 1
       Call FLGRD(0,0)
       NLEN = LEN(NAMES(1))
       Call COMTCH(NAMES,NVAL,RESP(1:NLEN),J)
       If (J.le.0) Then
         Call LINRD(1,1)
         If (ICOMER.eq.3)  Return
         Goto 2
       End If
C
C..... GET THE VALUE
C
       If (VARTYP.eq.'L') Then
 82      Assign 82 TO LABEL
         Call LCMMOD(BUFFER,J,ISTAT)
         If (ICOMER.ne.1) Return
       Else If (VARTYP.eq.'I') Then
 83      Assign 83 TO LABEL
         Call ICMMOD(BUFFER,J,ISTAT)
         If (ICOMER.ne.1) Return
       Else If (VARTYP.eq.'R') Then
 84      Assign 84 TO LABEL
         Call RCMMOD(BUFFER,J,ISTAT)
         If (ICOMER.ne.1) Return
       Else If (vartyp.eq.'D') Then
 85      Assign 85 to label
         Call Dcmmod(buffer,j,istat)
         If (icomer.ne.1) Return
       Else If (VARTYP.eq.'C') Then
 86      Assign 86 TO LABEL
         Call CCMMOD(CBUF,J,ISTAT)
         If (ICOMER.ne.1) Return
       End If
       If (ISTAT.ne.1) Then
         Call LINRD(1,1)
         Write (LUNCOM,*) 'COM: ERROR DECODING ',RESP(1:NLEN)
         Goto 2
       End If
       MODIF = .TRUE.
       MFLAG(J) = .TRUE.
       Goto 2
       End
