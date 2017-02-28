      SUBROUTINE POLYLBL (Map, Szx, Szy, Mapid, Maptype, Boxsize, MAXPT,
     &                    NPT, X, Y, Status)
      IMPLICIT NONE
c
c  I  Map     (r)  Image map
c  I  Szx/y   (i)  Size of map
c  I  Mapid   (s)  Map id string
c  I  Maptype (c)  Map type (I=integer R=real)
c  I  Boxsize (i)  Size of box to be printed
c  I  MAXPT   (i)  Maximum number of points
c  O  NPT     (i)  Number of points clicked
c  O  X/Y     (r)  Points clicked
c  O  Status  (i)  Error flag (0=OK)
c
C Adapted from PGOLIN
C   This version plots the data value for the selected image pixels,
C   centered on the selection point, as they are selected This couldn't
C   have been accomplished with PGOLIN itself, because the values
C   need to be placed on the screen as they are clicked

      INTEGER MAXPT, NPT, Szx, Szy, Boxsize, Status
      REAL*4 Map(Szx,Szy)
      CHARACTER*(*) Mapid, Maptype
      REAL    X(*), Y(*)
C     INTEGER SYMBOL
C--------------------------------------------------------------------
C
C Interactive routine for user to enter data points by use of
C the cursor.  Routine allows user to Add and Delete points.  The
C points are returned in the order that they were entered (unlike
C PGNCUR).
C
C Arguments:
C  HEADER (input)  : internal ximage header
C  MAXPT  (input)  : maximum number of points that may be accepted.
C  NPT    (in/out) : number of points entered; should be zero on
C                    first call.
C  X      (in/out) : array of x-coordinates.
C  Y      (in/out) : array of y-coordinates.
C
C Note (1): The dimension of arrays X and Y must be greater than or
C equal to MAXPT.
C
C Note (2): On return from the program, cursor points are returned in
C the order they were entered. Routine may be (re-)called with points
C already defined in X,Y (number in NPT), and they will be plotted
C first, before editing.
C
C Note (3): User commands: the user types single-character commands
C after positioning the cursor: the following are accepted:
C A (Add)    - add point at current cursor location.
C D (Delete) - delete the last point entered.
C X (eXit)   - leave subroutine.
C--
C  4-Nov-1985 - new routine (adapted from PGNCUR) - TJP.
C 13-Dec-1990 - change warnings to messages [TJP].
C  7-Sep-1994 - use PGBAND [TJP].
C  2-Aug-1995 - remove dependence on common block [TJP].
C-----------------------------------------------------------------------

      character(1) LETTER
      character(10) VALUE
      INTEGER  PGBAND, LENACT, vlen
      REAL     XP, YP, XREF, YREF
      REAL     XBLC, XTRC, YBLC, YTRC
      REAL     XCH, YCH, xpos, ypos, mapval
      LOGICAL isrnull
      INTEGER di
      REAL*8 dd

      Status = 0
C
C Put current points on screen.  Position cursor on last point,
C or in middle viewport if there are no current points.
C
      CALL PGQWIN(XBLC, XTRC, YBLC, YTRC)
      IF (NPT.NE.0) THEN
C         CALL PGPT(NPT,X,Y,SYMBOL)
          XP = X(NPT)
          YP = Y(NPT)
      ELSE
          XP = 0.5*(XBLC+XTRC)
          YP = 0.5*(YBLC+YTRC)
      END IF
C
C Loop over cursor inputs.
C
  100 XREF = XP
      YREF = YP
      IF (PGBAND(0,0,XREF,YREF,XP,YP,LETTER).NE.1) RETURN
      IF (LETTER.EQ.CHAR(0)) RETURN
      CALL UPC(LETTER)

C
C A (ADD) command:
C
      IF (LETTER .EQ. 'A') THEN

          IF (ABS(XBLC-XP)+ABS(XTRC-XP) .gt. ABS(XBLC-XTRC) .or.
     &        ABS(YBLC-YP)+ABS(YTRC-YP) .gt. ABS(YBLC-YTRC)) THEN
              CALL XWRITE('Selected point is outside image.',5)
          ELSE IF (NPT.GE.MAXPT) THEN
              CALL XWRITE('ADD ignored (too many points).',5)
          ELSE
              NPT = NPT + 1
              X(NPT) = XP
              Y(NPT) = YP
C Plot array value rather than symbol
C             CALL PGPT(1,X(NPT),Y(NPT),SYMBOL)
C Get MAP value for selection
              call prbox(Map,Szx,Szy,Mapid,Maptype,Boxsize,X(NPT),
     &                   Y(NPT),status)
              if ( status.ne.0 ) return
              call imgpix(Mapid,X(NPT),Y(NPT),xpos,ypos, 2, status)
              if ( status.ne.0 ) return
              mapval = Map(NINT(xpos),NINT(ypos))
              if ( isrnull(mapval) ) then
                 VALUE = 'N'
              elseif ( Maptype.eq.'I' ) then
                 di = INT(mapval)
                 call xistr(di, VALUE, vlen)
              else
                 dd = mapval
                 call xdstr(dd, 5, VALUE, vlen)
              endif
              CALL PGQCS(4,XCH,YCH)
              call jrnlab(X(NPT),Y(NPT),VALUE(1:LENACT(VALUE)),
     &                    ' ',-1,-1.0,-1,-1,-1,-1.0,-1,'center',0.0)
c             CALL PGPTXT (X(NPT),Y(NPT)-YCH/3.,0.0,0.5,
c    &                     VALUE(1:LENACT(VALUE)))
          END IF
C
C X (EXIT) command:
C
      ELSE IF (LETTER.EQ.'X') THEN
          RETURN
C
C Illegal command:
C
      ELSE
          CALL XWRITE('Commands are A (add), X (exit).', 5)
      END IF
C
      GOTO 100
      END
