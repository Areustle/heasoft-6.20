      SUBROUTINE POLYBOX (Map, Szx, Szy, Mapid, Maptype, Boxsize, 
     &                    MAXPT, NPT, X, Y, SYMBOL,Status)
      IMPLICIT NONE
c
c  I  Map     (r)  Image map
c  I  Szx/y   (i)  Size of map
c  I  Mapid   (s)  Map id string
c  I  Maptype (c)  Map data type (I=integer, R=real)
c  I  Boxsize (i)  Size of box to print (n=n pixels around selection)
c  I  MAXPT   (i)  Maximum number of points
c  O  NPT     (i)  Number of points clicked
c  O  X/Y     (r)  Points clicked
c  I  SYMBOL  (i)  Symbol code
c  O  STATUS  (i)  Error flag (0=OK)
c
c  Adapted from PGOLIN
c   This version prints a box of values to command screen for selected 
c   pixels, as they are selected.
c
      INTEGER MAXPT, NPT, Szx, Szy, Boxsize, Status
      REAL*4 Map(Szx,Szy)
      CHARACTER*(*) Mapid, Maptype
      REAL    X(*), Y(*)
      INTEGER SYMBOL

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
      INTEGER  PGBAND
      REAL     XP, YP, XREF, YREF
      REAL     XBLC, XTRC, YBLC, YTRC

      Status = 0
C
C Put current points on screen.  Position cursor on last point,
C or in middle viewport if there are no current points.
C
      CALL PGQWIN(XBLC, XTRC, YBLC, YTRC)
      IF (NPT.NE.0) THEN
          CALL PGPT(NPT,X,Y,SYMBOL)
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
              CALL PGPT(1,X(NPT),Y(NPT),SYMBOL)
              call prbox(Map,Szx,Szy,Mapid,Maptype,Boxsize,X(NPT),
     &                   Y(NPT),Status)
              if ( Status.ne.0 ) return
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
