C*PSDRIV -- PGPLOT PostScript drivers
C+
      SUBROUTINE PSDRIV (IFUNC, RBUF, NBUF, CHR, LCHR, MODE)

      IMPLICIT NONE

      INTEGER IFUNC, NBUF, LCHR, MODE
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C
C PGPLOT driver for PostScript devices.
C
C Version 1.2  - 1987 Aug  5 - T. J. Pearson.
C Version 1.3  - 1987 Nov 16 - add "bind" commands to prolog - TJP.
C Version 1.4  - 1988 Jan 28 - change dimensions so whole field can be
C                              plotted - TJP.
C Version 1.5  - 1988 Oct 27 - make EOF characters optional - TJP.
C Version 1.6  - 1988 Dec 15 - standard Fortran - TJP.
C Version 1.7  - 1989 Jul  5 - change color indices so most colors
C                              are black - TJP.
C Version 2.0  - 1990 Sep 10 - parameterize dimensions; correct
C                              bounding box; add color support (from
C                              D. Meier's CPdriver) - TJP.
C Version 2.1  - 1991 Nov 29 - update Document Structuring Conventions
C                              to version 3.0.
C Version 3.0  - 1992 Sep 22 - add marker support; add CPS and VCPS
C                              modes - TJP.
C Version 3.1  - 1992 Nov 12 - up to 256 colors.
C Version 3.2  - 1993 May 26 - correct error in marker support.
C Version 4.0  - 1993 Sep 20 - trap Fortran I/O errors.
C Version 4.1  - 1994 Aug  4 - make marker support optional.
C Version 5.0  - 1994 Aug 30 - support for images.
C Version 5.1  - 1994 Sep  7 - support for PGQCR.
C Version 5.2  - 1994 Oct 12 - add IDENT option.
C Version 5.3  - 1995 May  8 - recognise '-' as standard output; keep
C                              track of bounding box; use upper case
C                              for all defined commands; move
C                              showpage outside save/restore.
C Version 5.4  - 1995 Aug 19 - correct usage of PS_BBOX.
C Version 6.0  - 1995 Dec 28 - reject concurrent access.
C Version 6.1  - 1996 Apr 29 - decode environment variables using GRCTOI.
C Version 6.2  - 1996 Oct  7 - correct bounding-box error (K-G Adams);
C                              correct error in use of GCTOI (G Gonczi);
C                              suppress <0 0 C> commands (R Scharroo);
C                              allow arbitrary page size.
C Version 7.0  - 2006 May 19 - better support for writing of text through
C                              IFUNC=31 option (K Arnaud).
C                              
C
C Supported device: 
C   Any printer that accepts the PostScript page description language, 
C   eg, the LaserWriter (Apple Computer, Inc.).
C   PostScript is a trademark of Adobe Systems Incorporated.
C
C Device type code: 
C   /PS (monochrome landscape mode, long edge of paper horizontal).
C   /CPS (color landscape mode, long edge of paper horizontal).
C   /VPS (monochrome portrait mode, short edge of paper horizontal).
C   /VCPS (color portrait mode, short edge of paper horizontal).
C
C Default file name:
C   pgplot.ps
C
C Default view surface dimensions:
C   10.5 inches horizontal x  7.8 inches vertical (landscape mode),
C    7.8 inches horizontal x 10.5 inches vertical (portrait mode).
C   These dimensions can be changed with environment variables.
C
C Resolution:
C   The driver uses coordinate increments of 0.001 inch, giving an
C   ``apparent'' resolution of 1000 pixels/inch. The true resolution is
C   device-dependent; eg, on an Apple LaserWriter it is 300 pixels/inch
C   (in both dimensions). 
C
C Color capability (monochrome mode): 
C   Color indices 0-255 are supported. Color index 0 is white (erase
C   or background color), indices 1-13 are black, 14 is light grey,
C   and 15 is dark grey.
C
C Color capability (color mode):
C   Color indices 0-255 are supported. Color index 0 is white (erase
C   or background color), index 1 is black, and indices 2-15 have the
C   standard PGPLOT color assignments.
C
C
C Input capability: none.
C
C File format: the file contains variable length records (maximum 132
C characters) containing PostScript commands. The commands use only
C printable ASCII characters, and the file can be examined or modified 
C with a text editor. 
C
C Obtaining hardcopy: use the operating system print or copy command to
C send the file to a suitable device.
C
C Environment variables:
C
C  PGPLOT_PS_WIDTH      default  7800
C  PGPLOT_PS_HEIGHT     default 10500
C  PGPLOT_PS_HOFFSET    default   350
C  PGPLOT_PS_VOFFSET    default   250
C These variables tell PGPLOT how big an image to produce. The defaults
C are appropriate for 8.5 x 11-inch paper. The maximum dimensions of
C a PGPLOT image are WIDTH by HEIGHT, with the lower left corner offset
C by HOFFSET horizontally and VOFFSET vertically from the lower left
C corner of the paper. The units are milli-inches. The "top" of the
C paper is the edge that comes out of the printer first.
C
C  PGPLOT_IDENT
C If this variable is set, the user name, date and time are written
C in the bottom right corner of each page.
C
C  PGPLOT_PS_BBOX
C If this variable has value MAX, PGPLOT puts standard (full-page)
C bounding-box information in the header of the PostScript file. If
C the variable is unset or has some other value, PGPLOT puts the
C correct (smallest) bounding box information in the trailer of the
C PostScript file.
C
C  PGPLOT_PS_EOF
C Normally the output file does not contain special end-of-file
C characters. But if environment variable PGPLOT_PS_EOF is defined
C (with any value) PGPLOT writes a control-D job-separator character at 
C the beginning and at the end of the file. This is appropriate for
C Apple LaserWriters using the serial interface, but it may not be 
C appropriate for other PostScript devices.
C
C  PGPLOT_PS_MARKERS
C Specify "NO" to suppress use of a PostScript font for the graph
C markers; markers are then emulated by line-drawing. 
C
C Document Structuring Conventions:
C
C  The PostScript files conform to Version 3.0 of the Adobe Document 
C  Structuring Conventions (see ref.3) and to version 3.0 of the
C  encapsulated PostScript file (EPSF) format. This should allow
C  the files to be read by other programs that accept the EPSF format.
C  Note, though, that multi-page plots are not valid EPSF files. The
C  files do not contain a screen preview section.
C
C References:
C
C (1) Adobe Systems, Inc.: PostScript Language Reference Manual.
C Addison-Wesley, Reading, Massachusetts, 1985.
C (2) Adobe Systems, Inc.: PostScript Language Tutorial and Cookbook.
C Addison-Wesley, Reading, Massachusetts, 1985.
C (3) Adobe Systems, Inc.: PostScript Language Reference Manual, Second 
C Edition. Addison-Wesley, Reading, Massachusetts, 1990.
C-----------------------------------------------------------------------
      INTEGER DWD, DHT, DOFFW, DOFFH
      INTEGER NPH
      CHARACTER*(*) PTYPE, LTYPE, CPTYPE, CLTYPE, DEFNAM
      PARAMETER (
     : PTYPE= 'VPS   (PostScript file, portrait orientation)',
     : LTYPE= 'PS    (PostScript file, landscape orientation)',
     : CPTYPE='VCPS  (Colour PostScript file, portrait orientation)',
     : CLTYPE='CPS   (Colour PostScript file, landscape orientation)')
C     PARAMETER (PTYPE='VPS', LTYPE='PS', CPTYPE='VCPS', CLTYPE='CPS')
      PARAMETER (DEFNAM='pgplot.ps')
C -- printable paper area: in milli-inches; (WIDTH, HEIGHT) are
C    the dimensions of the printable area; OFFW, OFFH the offset from
C    the lower left corner of the paper
      PARAMETER (DWD=7800, DHT=10500, DOFFW=350, DOFFH=250)
      PARAMETER (NPH=10)
C
      INTEGER WIDTH, HEIGHT, OFFW, OFFH
      SAVE    WIDTH, HEIGHT, OFFW, OFFH
      INTEGER  IER, I0, J0, I1, J1, L, LL, LASTI, LASTJ, UNIT, LOBUF
     x
      SAVE                                 LASTI, LASTJ, UNIT, LOBUF
     x
      INTEGER  CI, LW, NPTS, NPAGE, IOERR, LFNAME
      SAVE         LW, NPTS, NPAGE, IOERR, LFNAME
      INTEGER  STATE
      SAVE     STATE
      INTEGER  NXP, NYP, XORG, YORG, XLEN, YLEN, N, RGB(3)
      INTEGER  HIGH, LOW, I, K, KMAX, POSN, LD, LU
      INTEGER  BBOX(4), BB1, BB2, BB3, BB4
      SAVE     BBOX
      INTEGER  GROPTX, GRCTOI, GRTRIM
      LOGICAL  START, LANDSC, COLOR, STDOUT
      SAVE     START,         COLOR, STDOUT
      REAL     BBXMIN, BBXMAX, BBYMIN, BBYMAX
      SAVE     BBXMIN, BBXMAX, BBYMIN, BBYMAX
      REAL     RVALUE(0:255), GVALUE(0:255), BVALUE(0:255)
      SAVE     RVALUE,        GVALUE,        BVALUE
      LOGICAL  FNTUSE(11)
      SAVE FNTUSE
      CHARACTER*20  SUSER, SDATE
      CHARACTER*255 INSTR, MSG
      CHARACTER*2048 OBUF
      SAVE          OBUF
      CHARACTER*255 FNAME, PLINE
      SAVE          FNAME
      INTEGER       MARKER(0:31), NSYM, RAD(0:31)
      SAVE          MARKER, RAD
      REAL          MFAC
      SAVE          MFAC
      REAL          SHADE(0:15), RINIT(0:15), GINIT(0:15), BINIT(0:15)
      SAVE          SHADE,       RINIT,       GINIT,       BINIT
      CHARACTER*1   HEXDIG(0:15)
      INTEGER FIRST
      DATA HEXDIG/'0','1','2','3','4','5','6','7',
     1            '8','9','A','B','C','D','E','F'/
      DATA SHADE /1.00, 13*0.00, 0.33, 0.67/
      DATA RINIT 
     1     / 1.00, 0.00, 1.00, 0.00, 0.00, 0.00, 1.00, 1.00,
     2       1.00, 0.50, 0.00, 0.00, 0.50, 1.00, 0.33, 0.67/
      DATA GINIT
     1     / 1.00, 0.00, 0.00, 1.00, 0.00, 1.00, 0.00, 1.00,
     2       0.50, 1.00, 1.00, 0.50, 0.00, 0.00, 0.33, 0.67/
      DATA BINIT
     1     / 1.00, 0.00, 0.00, 0.00, 1.00, 1.00, 1.00, 0.00,
     2       0.00, 0.00, 0.50, 1.00, 1.00, 0.50, 0.33, 0.67/
      DATA RAD/ 6,  1,  7,  6, 7, 5, 6, 8,
     :          7,  7,  9, 10, 9, 8, 6, 8,
     :          4,  5,  9, 12, 2, 4, 5, 7,
     :         11, 17, 22, 41, 9, 9, 9, 9/
      DATA STATE/0/
      DATA FIRST/1/
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230,240,250,260,270,280,290,300,
     3     310), IFUNC
      GOTO 900
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
   10 IF (MODE.EQ.1) THEN
C         -- landscape, monochrome
          CHR = LTYPE
          LCHR = LEN(LTYPE)
      ELSE IF (MODE.EQ.2) THEN
C         -- portrait, monochrome
          CHR = PTYPE
          LCHR = LEN(PTYPE)
      ELSE IF (MODE.EQ.3) THEN
C         -- landscape, color
          CHR = CLTYPE
          LCHR = LEN(CLTYPE)
      ELSE
C         -- portrait, color
          CHR = CPTYPE
          LCHR = LEN(CPTYPE)
      END IF
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = -1
      RBUF(3) = 0
      RBUF(4) = -1
      RBUF(5) = 0
      RBUF(6) = 255
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C
   30 RBUF(1) = 1000.0
      RBUF(2) = 1000.0
      RBUF(3) = 5
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, Area fill, 
C    Thick lines, QCR, Markers [optional])
C
   40 CONTINUE
      CHR = 'HNNATNQNYM'
C     -- Marker support suppressed?
      CALL GRGENV('PS_MARKERS', INSTR, L)
      IF (L.GE.2) THEN
         IF (INSTR(1:L).EQ.'NO' .OR. INSTR(1:L).EQ.'no') THEN
            CHR(10:10) = 'N'
         END IF
      END IF
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name. ------------------------------
C
   50 CHR = DEFNAM
      LCHR = LEN(DEFNAM)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot. ------------------
C
   60 RBUF(1) = 0
      RBUF(3) = 0
      LANDSC = MODE.EQ.1 .OR. MODE.EQ.3
      IF (LANDSC) THEN
          RBUF(2) = HEIGHT-1
          RBUF(4) = WIDTH-1
      ELSE
          RBUF(2) = WIDTH-1
          RBUF(4) = HEIGHT-1
      END IF
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults. ----------------------------------
C
   70 RBUF(1) = 8
      NBUF = 1
      RETURN
C
C--- IFUNC = 8, Select plot. -------------------------------------------
C
   80 CONTINUE
      RETURN
C
C--- IFUNC = 9, Open workstation. --------------------------------------
C
   90 CONTINUE
   
C     -- check for concurrent access

      IF (STATE.EQ.1) THEN
         CALL GRWARN('a PGPLOT PostScript file is already open')
         RBUF(1) = 0
         RBUF(2) = 0
         RETURN
      END IF
      
C     -- Color mode?
      CALL GRGENV('PS_COLOR', INSTR, L)
      COLOR = L.GT.0 .OR. MODE.EQ.3 .OR. MODE.EQ.4
      IF (COLOR) THEN
         DO CI = 1,3
            RBUF(CI) = 0
         END DO
         DO 91 CI=0,15
            RVALUE(CI) = RINIT(CI)
            GVALUE(CI) = GINIT(CI)
            BVALUE(CI) = BINIT(CI)
 91      CONTINUE
      ELSE
         DO 92 CI=0,15
            RVALUE(CI) = SHADE(CI)
            GVALUE(CI) = SHADE(CI)
            BVALUE(CI) = SHADE(CI)
 92      CONTINUE
      END IF
      DO 93 CI=16,255
         RVALUE(CI) = 0.0
         GVALUE(CI) = 0.0
         BVALUE(CI) = 0.0
 93   CONTINUE
C     -- Device dimensions
      WIDTH = DWD
      HEIGHT = DHT
      OFFW = DOFFW
      OFFH = DOFFH
      CALL GRGENV('PS_WIDTH', INSTR, L)
      LL = 1
      IF (L.GT.0) WIDTH = GRCTOI(INSTR(:L),LL)
      CALL GRGENV('PS_HEIGHT', INSTR, L)
      LL = 1
      IF (L.GT.0) HEIGHT = GRCTOI(INSTR(:L),LL)
      CALL GRGENV('PS_HOFFSET', INSTR, L)
      LL = 1
      IF (L.GT.0) OFFW = GRCTOI(INSTR(:L),LL)
      CALL GRGENV('PS_VOFFSET', INSTR, L)
      LL = 1
      IF (L.GT.0) OFFH = GRCTOI(INSTR(:L),LL)
      STDOUT =CHR(1:LCHR).EQ.'-'
      IF (STDOUT) THEN
         UNIT = 6
C        -- machine-dependent!
      ELSE
         CALL GRGLUN(UNIT)
      END IF
      NBUF = 2
      RBUF(1) = UNIT
      IF (.NOT.STDOUT) THEN
         IER = GROPTX(UNIT, CHR(1:LCHR), DEFNAM, 1)
         IF (IER.NE.0) THEN
            MSG = 'Cannot open output file for PostScript plot: '//
     1           CHR(:LCHR)
            CALL GRWARN(MSG)
            RBUF(2) = 0
            CALL GRFLUN(UNIT)
            RETURN
         ELSE
            INQUIRE (UNIT=UNIT, NAME=CHR)
            LCHR = LEN(CHR)
 94         IF (CHR(LCHR:LCHR).EQ.' ') THEN
               LCHR = LCHR-1
               GOTO 94
            END IF
            RBUF(2) = 1
            FNAME = CHR(:LCHR)
            LFNAME = LCHR
         END IF
      ELSE
         RBUF(2) = 1
         FNAME = '-'
         LFNAME= 1
      END IF
      STATE = 1
      IOERR = 0
      LOBUF = 0
      LASTI = -1
      LASTJ = -1
      LW = 1
      NPTS = 0

C Write the header information for the output PS file

      CALL GRGENV('PS_EOF', INSTR, L)
      IF (L.GT.0) CALL GRPS02(IOERR, UNIT, CHAR(4))
      PLINE = '%!PS-Adobe-3.0 EPSF-3.0'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      CALL GRUSER(INSTR, L)
      IF (L.GT.0) CALL GRPS02(IOERR, UNIT, '%%For: '//INSTR(1:L))
      PLINE = '%%Title: PGPLOT PostScript plot'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '%%Creator: PGPLOT'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      CALL GRDATE(INSTR, L)
      IF (L.GT.0) CALL GRPS02(IOERR, UNIT,
     :    '%%CreationDate: '//INSTR(1:L))
      CALL GRGENV('PS_BBOX', INSTR, L)
      CALL GRTOUP(INSTR(1:3), INSTR(1:3))
      PLINE = '%%BoundingBox: (atend)'

      IF (INSTR(1:3).EQ.'MAX') THEN
C        -- bounding box is based on maximum plot dimensions, not
C           actual dimensions
         CALL GRFAO('%%BoundingBox: # # # #', L, INSTR,
     :        NINT(OFFW*0.072), NINT(OFFH*0.072),
     :        NINT((WIDTH+OFFW)*0.072), NINT((HEIGHT+OFFH)*0.072))
         CALL GRPS02(IOERR, UNIT, INSTR(:L))
      ELSE
         CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)) )
      END IF
      LANDSC = MODE.EQ.1 .OR. MODE.EQ.3
      IF (LANDSC) THEN
         PLINE = '%%Orientation: Landscape'
         CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      ELSE
         PLINE = '%%Orientation: Portrait'
         CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      END IF
      PLINE = '%%Pages: (atend)'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '%%EndComments'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))

C Write the prolog info to the file

      CALL GRPS10(IOERR, UNIT)

C Initialize array tracking which fonts are used

      DO i = 1, 11
         FNTUSE(i) = .FALSE.
      ENDDO

      FIRST = 0
      CALL GRFLUN(UNIT)
      NPAGE = 0
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
      CALL GRPS02(IOERR, UNIT, ' ')
      CALL GRPS02(IOERR, UNIT, '%%Trailer')
      CALL GRGENV('PS_BBOX', INSTR, L)
      CALL GRTOUP(INSTR(1:3), INSTR(1:3))
      IF (INSTR(1:3).NE.'MAX') THEN
         CALL GRFAO('%%BoundingBox: # # # #', L, INSTR,
     :        BBOX(1), BBOX(2), BBOX(3), BBOX(4))
         CALL GRPS02(IOERR, UNIT, INSTR(:L))
      END IF

C Set up and write string of fonts which have been used

      MSG = '%%DocumentFonts:'
      IF ( FNTUSE(1) ) MSG = MSG(1:GRTRIM(MSG)) // ' Times-Roman'
      IF ( FNTUSE(2) ) MSG = MSG(1:GRTRIM(MSG)) // ' Times-Bold'
      IF ( FNTUSE(3) ) MSG = MSG(1:GRTRIM(MSG)) // ' Times-Italic'
      IF ( FNTUSE(4) ) MSG = MSG(1:GRTRIM(MSG)) // ' Times-BoldItalic'
      IF ( FNTUSE(5) ) MSG = MSG(1:GRTRIM(MSG)) // ' Helvetica'
      IF ( FNTUSE(6) ) MSG = MSG(1:GRTRIM(MSG)) // ' Helvetica-Oblique'
      IF ( FNTUSE(7) ) MSG = MSG(1:GRTRIM(MSG)) // ' Helvetica-Narrow'
      IF ( FNTUSE(8) ) MSG = MSG(1:GRTRIM(MSG)) // 
     &                      ' Helvetica-NarrowBold'
      IF ( FNTUSE(9) ) MSG = MSG(1:GRTRIM(MSG)) // ' Helvetica-Bold'
      IF ( FNTUSE(10) ) MSG = MSG(1:GRTRIM(MSG)) // 
     &                      ' Zapf-Chancery-MediumItalic'
      IF ( FNTUSE(11) ) MSG = MSG(1:GRTRIM(MSG)) // ' Symbol'
      CALL GRPS02(IOERR, UNIT, MSG)

      CALL GRFAO('%%Pages: #', L, INSTR, NPAGE, 0, 0, 0)
      CALL GRPS02(IOERR, UNIT, INSTR(:L))
      CALL GRPS02(IOERR, UNIT, '%%EOF')
      CALL GRGENV('PS_EOF', INSTR, L)
      IF (L.GT.0) CALL GRPS02(IOERR, UNIT, CHAR(4))
      IF (IOERR.NE.0) THEN
          CALL GRWARN('++WARNING++ Error '//
     1       'writing PostScript file: file is incomplete')
          CALL GRWARN('Check for device full or quota exceeded')
          CALL GRWARN('Filename: '//FNAME(:LFNAME))
      END IF
      IF (.NOT.STDOUT) THEN
         CLOSE (UNIT, IOSTAT=IOERR)
         IF (IOERR.NE.0) THEN
           CALL GRWARN('Error closing PostScript file '//FNAME(:LFNAME))
         END IF
         CALL GRFLUN(UNIT)
      END IF
      STATE = 0
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
      LANDSC = MODE.EQ.1 .OR. MODE.EQ.3
      IF (LANDSC) THEN
         HEIGHT = RBUF(1)
         WIDTH = RBUF(2)
      ELSE
         WIDTH = RBUF(1)
         HEIGHT = RBUF(2)
      END IF
      NPAGE = NPAGE+1
      CALL GRPS02(IOERR, UNIT, ' ')
      CALL GRFAO('%%Page: # #', L, INSTR, NPAGE, NPAGE, 0, 0)
      CALL GRPS02(IOERR, UNIT, INSTR(:L))

      PLINE = '%%BeginPageSetup'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/PGPLOT save def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = 'pgscale pgscale scale'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))

      LANDSC = MODE.EQ.1 .OR. MODE.EQ.3
      IF (LANDSC) THEN
          CALL GRFAO('# # translate 90 rotate', L, INSTR, WIDTH+OFFW, 
     1               OFFH, 0, 0)
      ELSE
          CALL GRFAO('# # translate', L, INSTR, OFFW, OFFH, 0, 0)
      END IF
      CALL GRPS02(IOERR, UNIT, INSTR(:L))

      PLINE = '1 setlinejoin 1 setlinecap 1 SLW 1 SCF'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '%%EndPageSetup'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '%%PageBoundingBox: (atend)'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))

      CALL GRFLUN(UNIT)
      DO 112 NSYM=0,31
          MARKER(NSYM) = 0
  112 CONTINUE
      MFAC = 0.0
      BBXMIN = WIDTH
      BBYMIN = HEIGHT
      BBXMAX = 0.0
      BBYMAX = 0.0
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C
  120 CONTINUE

      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
      IF (I0.EQ.LASTI .AND. J0.EQ.LASTJ) THEN
C        -- suppress zero-length continuation segment
         IF (I0.EQ.I1 .AND. J0.EQ.J1) RETURN
         CALL GRFAO('# # C', L, INSTR, (I1-I0), (J1-J0), 0, 0)
      ELSE
         CALL GRFAO('# # # # L', L, INSTR, (I1-I0), (J1-J0), I0, J0)
      END IF
      LASTI = I1
      LASTJ = J1
      BBXMIN = MIN(BBXMIN, I0-LW*5.0, I1-LW*5.0)
      BBXMAX = MAX(BBXMAX, I0+LW*5.0, I1+LW*5.0)
      BBYMIN = MIN(BBYMIN, J0-LW*5.0, J1-LW*5.0)
      BBYMAX = MAX(BBYMAX, J0+LW*5.0, J1+LW*5.0)

      GOTO 800
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
  130 CONTINUE
      I1 = NINT(RBUF(1))
      J1 = NINT(RBUF(2))
      CALL GRFAO('# # D', L, INSTR, I1, J1, 0, 0)
      LASTI = I1
      LASTJ = J1
      BBXMIN = MIN(BBXMIN, I1-LW*5.0)
      BBXMAX = MAX(BBXMAX, I1+LW*5.0)
      BBYMIN = MIN(BBYMIN, J1-LW*5.0)
      BBYMAX = MAX(BBYMAX, J1+LW*5.0)
      GOTO 800
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
      IF (LOBUF.NE.0) THEN
          CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          LOBUF = 0
      END IF
      LANDSC = MODE.EQ.1 .OR. MODE.EQ.3
C     -- optionally write identification
      CALL GRGENV('IDENT', INSTR, L)
      IF (L.GT.0) THEN
         CALL GRUSER(SUSER, LU)
         CALL GRDATE(SDATE, LD)
         POSN = WIDTH - 1
         IF (LANDSC) POSN = HEIGHT - 1
         CALL GRFAO('('//SUSER(:LU)//' '//SDATE(:LD)//
     :        ' [#]) # # 100 /Helvetica RS',
     :        L, INSTR, NPAGE, POSN, 50, 0)
         CALL GRPS02(IOERR, UNIT, '0.0 setgray')
         CALL GRPS02(IOERR, UNIT, INSTR(1:L))
      END IF
C     -- optionally draw bounding box
      CALL GRGENV('PS_DRAW_BBOX', INSTR, L)
      IF (L.GT.0) THEN
         CALL GRFAO('0.0 setgray 0 SLW newpath # # moveto', L, INSTR,
     :              NINT(BBXMIN), NINT(BBYMIN), 0, 0)
         CALL GRPS02(IOERR, UNIT, INSTR(1:L))
         CALL GRFAO('# # lineto # # lineto', L, INSTR,
     :        NINT(BBXMIN), NINT(BBYMAX), NINT(BBXMAX), NINT(BBYMAX))
         CALL GRPS02(IOERR, UNIT, INSTR(1:L))
         CALL GRFAO('# # lineto closepath stroke', L,INSTR,
     :              NINT(BBXMAX), NINT(BBYMIN), 0, 0)
         CALL GRPS02(IOERR, UNIT, INSTR(1:L))
      END IF
      CALL GRPS02(IOERR, UNIT, 'PGPLOT restore showpage')
      CALL GRPS02(IOERR, UNIT, '%%PageTrailer')

      CALL GRGENV('PS_BBOX', INSTR, L)
      IF (INSTR(1:3).EQ.'MAX') THEN
C        -- bounding box is based on maximum plot dimensions, not
C           actual dimensions
         BB1 = NINT(OFFW*0.072)
         BB2 = NINT(OFFH*0.072)
         BB3 = NINT((WIDTH+OFFW)*0.072)
         BB4 = NINT((HEIGHT+OFFH)*0.072)
      ELSE
         IF (LANDSC) THEN
              BB1 = INT((WIDTH-BBYMAX+OFFW)*0.072)
              BB2 = INT((BBXMIN-OFFH)*0.072)
              BB3 = 1+INT((WIDTH-BBYMIN+OFFW)*0.072)
              BB4 = 1+INT((BBXMAX+OFFH)*0.072)
         ELSE
              BB1 = INT((BBXMIN-OFFW)*0.072)
              BB2 = INT((BBYMIN-OFFH)*0.072)
              BB3 = 1+INT((BBXMAX+OFFW)*0.072)
              BB4 = 1+INT((BBYMAX+OFFH)*0.072)
         END IF
      END IF
      CALL GRFAO('%%PageBoundingBox: # # # #', L, INSTR,
     :     BB1, BB2, BB3, BB4)
      CALL GRPS02(IOERR, UNIT, INSTR(1:L))
      
      IF (NPAGE.EQ.1) THEN
         BBOX(1) = BB1
         BBOX(2) = BB2
         BBOX(3) = BB3
         BBOX(4) = BB4
      ELSE
         BBOX(1) = MIN(BBOX(1),BB1)
         BBOX(2) = MIN(BBOX(2),BB2)
         BBOX(3) = MAX(BBOX(3),BB3)
         BBOX(4) = MAX(BBOX(4),BB4)
      END IF
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C
  150 CONTINUE

      CI = NINT(RBUF(1))

      IF (COLOR) THEN
          if ( rvalue(ci).gt.1.0.or.rvalue(ci).lt.0) rvalue(ci) = 0.
          if ( gvalue(ci).gt.1.0.or.gvalue(ci).lt.0) gvalue(ci) = 0.
          if ( bvalue(ci).gt.1.0.or.bvalue(ci).lt.0) bvalue(ci) = 0.
          WRITE(INSTR,'(3(F5.3,1X),''setrgbcolor'')')
     1          RVALUE(CI), GVALUE(CI), BVALUE(CI)
          L = 29
      ELSE
          WRITE(INSTR,'(F5.3,1X,''setgray'')') RVALUE(CI)
          L = 13
      END IF
      LASTI = -1
      GOTO 800
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C
  160 CONTINUE
      IF (LOBUF.NE.0) THEN
          CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          LOBUF = 0
      END IF
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C    (Not implemented: should not be called.)
C
  170 GOTO 900
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C    (Null operation: there is no alpha screen.)
C
  180 CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C    (Not implemented: should not be called.)
C
  190 GOTO 900
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C
  200 CONTINUE
      IF (NPTS.EQ.0) THEN
          NPTS = RBUF(1)
          START = .TRUE.
          RETURN
      ELSE
          NPTS = NPTS-1
          I0 = NINT(RBUF(1))
          J0 = NINT(RBUF(2))
          IF (START) THEN
              CALL GRFAO('# # BP', L, INSTR, I0, J0, 0, 0)
              START = .FALSE.
              LASTI = I0
              LASTJ = J0
          ELSE IF (NPTS.EQ.0) THEN
              CALL GRFAO('# # EP', L, INSTR, (I0-LASTI), 
     1                   (J0-LASTJ), 0, 0)
              LASTI = -1
              LASTJ = -1
          ELSE
              CALL GRFAO('# # LP', L, INSTR, (I0-LASTI), 
     1                   (J0-LASTJ), 0, 0)
              LASTI = I0
              LASTJ = J0
          END IF
          BBXMIN = MIN(BBXMIN, I0-LW*5.0)
          BBXMAX = MAX(BBXMAX, I0+LW*5.0)
          BBYMIN = MIN(BBYMIN, J0-LW*5.0)
          BBYMAX = MAX(BBYMAX, J0+LW*5.0)
          GOTO 800
      END IF
C
C--- IFUNC=21, Set color representation. -------------------------------
C
  210 CONTINUE
      IF (COLOR) THEN
          CI = RBUF(1)
          RVALUE(CI) = RBUF(2)
          GVALUE(CI) = RBUF(3)
          BVALUE(CI) = RBUF(4)
      ELSE
          CI = RBUF(1)
          RVALUE(CI) = 0.30*RBUF(2) + 0.59*RBUF(3) + 0.11*RBUF(4)
          GVALUE(CI) = RVALUE(CI)
          BVALUE(CI) = RVALUE(CI)
      END IF
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C
  220 CONTINUE
      LW = NINT(RBUF(1))
      CALL GRFAO('# SLW', L, INSTR, LW, 0, 0, 0)
      LASTI = -1
      GOTO 800
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
  230 CONTINUE
      IF (LOBUF.NE.0) THEN
C         -- flush buffer first
          CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          LOBUF = 0
      END IF
      CALL GRPS02(IOERR, UNIT, CHR(:LCHR))
      LASTI = -1
      RETURN
C
C
C--- IFUNC=24, Rectangle fill - unimplemented --------------------------
C
  240 CONTINUE
      GOTO 900
C
C--- IFUNC=25, Set fill pattern - unimplemented ------------------------
C
  250 CONTINUE
      GOTO 900
C
C--- IFUNC=26, Image.---------------------------------------------------
C
  260 CONTINUE
      N = RBUF(1)
      IF (N.EQ.0) THEN
C         -- First: setup for image
C         -- Set clipping region (RBUF(2...5))
          NXP = RBUF(2)
          NYP = RBUF(3)
          XORG = RBUF(4)
          XLEN = RBUF(5) - RBUF(4)
          YORG = RBUF(6) 
          YLEN = RBUF(7) - RBUF(6)
          BBXMIN = MIN(BBXMIN, RBUF(4), RBUF(5))
          BBXMAX = MAX(BBXMAX, RBUF(4), RBUF(5))
          BBYMIN = MIN(BBYMIN, RBUF(6), RBUF(7))
          BBYMAX = MAX(BBYMAX, RBUF(6), RBUF(7))
C      
          CALL GRPS02(IOERR, UNIT, 'gsave newpath')
          CALL GRFAO('# # moveto # 0 rlineto 0 # rlineto', L, INSTR,
     :               XORG, YORG, XLEN, YLEN)
          CALL GRPS02(IOERR, UNIT, INSTR(:L))
          CALL GRFAO('# 0 rlineto closepath clip', L, INSTR, -XLEN,
     :                0, 0, 0)
          CALL GRPS02(IOERR, UNIT, INSTR(:L))
C         -- 
          CALL GRFAO('/picstr # string def', L, INSTR, NXP, 0, 0, 0)
          CALL GRPS02(IOERR, UNIT, INSTR(:L))
          CALL GRFAO('# # 8 [', L, INSTR, NXP, NYP, 0, 0)
          CALL GRPS02(IOERR, UNIT, INSTR(:L))
          WRITE (INSTR, '(6(1PE10.3, 1X), '']'')') (RBUF(I),I=8,13)
          CALL GRPS02(IOERR, UNIT, INSTR(:67))
          IF (COLOR) THEN
              CALL GRPS02(IOERR, UNIT, 
     :      '{currentfile picstr readhexstring pop} false 3 colorimage')
          ELSE
              CALL GRPS02(IOERR, UNIT, 
     :      '{currentfile picstr readhexstring pop} image')
          END IF
      ELSE IF (N.EQ.-1) THEN
C         -- Last: terminate image
          CALL GRPS02(IOERR, UNIT, 'grestore')
      ELSE 
C         -- Middle: write N image pixels; each pixel uses 6 chars
C            in INSTR, so N must be <= 20.
          L = 0
          KMAX = 1
          IF (COLOR) KMAX = 3
          DO 262 I=1,N
              CI = RBUF(I+1)
              RGB(1) = NINT(255.0*RVALUE(CI))
              RGB(2) = NINT(255.0*GVALUE(CI))
              RGB(3) = NINT(255.0*BVALUE(CI))
              DO 261 K=1,KMAX
                  HIGH = RGB(K)/16
                  LOW  = RGB(K)-16*HIGH
                  L = L+1
                  INSTR(L:L) = HEXDIG(HIGH)
                  L = L+1
                  INSTR(L:L) = HEXDIG(LOW)
 261          CONTINUE
 262      CONTINUE
          CALL GRPS02(IOERR, UNIT, INSTR(1:L))
      END IF
      RETURN
C
C--- IFUNC=27, Scaling info - unimplemented ----------------------------
C
  270 CONTINUE
      GOTO 900
C
C--- IFUNC=28, Marker.--------------------------------------------------
C
  280 CONTINUE
      NSYM = NINT(RBUF(1))
C     -- Output code for this marker if necessary
      IF (MARKER(NSYM).EQ.0) THEN
          IF (LOBUF.GT.0) CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          LOBUF = 0
          CALL GRPS03(IOERR, NSYM, UNIT)
          MARKER(NSYM) = 1
      END IF
C     -- Output scale factor
      IF (RBUF(4).NE.MFAC) THEN
          IF (LOBUF.GT.0) CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          LOBUF = 0
          MFAC = RBUF(4)
          WRITE (INSTR, '(''/MFAC '',F10.3,'' def'')') MFAC
          CALL GRPS02(IOERR, UNIT, INSTR(1:24))
      END IF
C     -- Output an instruction to draw one marker
      I1 = NINT(RBUF(2))
      J1 = NINT(RBUF(3))
      CALL GRFAO('# # M#', L, INSTR, I1, J1, NSYM, 0)
      LASTI = -1
      BBXMIN = MIN(BBXMIN, I1-MFAC*RAD(NSYM))
      BBXMAX = MAX(BBXMAX, I1+MFAC*RAD(NSYM))
      BBYMIN = MIN(BBYMIN, J1-MFAC*RAD(NSYM))
      BBYMAX = MAX(BBYMAX, J1+MFAC*RAD(NSYM))
      GOTO 800
C
C--- IFUNC=29, Query color representation.------------------------------
C
 290  CONTINUE
      CI = NINT(RBUF(1))
      NBUF = 4
      RBUF(2) = RVALUE(CI)
      RBUF(3) = GVALUE(CI)
      RBUF(4) = BVALUE(CI)
      RETURN
C
C--- IFUNC=30, Scroll rectangle - unimplemented ----------------------------
C
 300  CONTINUE
      GOTO 900
C
C--- IFUNC=31, Write Text. -------------------------------------------------
C
 310  CONTINUE
      IF (LOBUF.NE.0) THEN
C         -- flush buffer first
          CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          LOBUF = 0
      END IF
C
C If PS_VERBOSE_TEXT set then write comment string to PS file - this option is
C obsolete since we now write the text to the PS file instead of plotting it
C as vectors
C
      CALL GRGENV ('PS_VERBOSE_TEXT', INSTR, I)
      IF (I.GT.0) THEN
         OBUF = '% Start "' // CHR(1:LCHR) // '"'
         LOBUF = GRTRIM(OBUF)
         CALL GRPS02(IOERR,UNIT,OBUF(1:LOBUF))
         LOBUF = 0
         OBUF = ' '
      END IF
C
C Convert the PGPLOT text into PS and write to file
C
      CALL GRPSSY(CHR,OBUF,RBUF(3),RBUF(4),RBUF(1),RBUF(2),FNTUSE)
      LOBUF = GRTRIM(OBUF)
      CALL GRPS02(IOERR,UNIT,OBUF(1:LOBUF))
      LOBUF = 0
      OBUF = ' '
C
C Optionally write the end comment
C
      IF (I.GT.0) THEN
         OBUF = '% End "' // CHR(1:LCHR) // '"'
         LOBUF = GRTRIM(OBUF)
         CALL GRPS02(IOERR,UNIT,OBUF(1:LOBUF))
         LOBUF = 0
         OBUF = ' '
      END IF
C
C Update the bounding box
C
      BBXMIN = MIN(BBXMIN, RBUF(5))
      BBYMIN = MIN(BBYMIN, RBUF(6))
      BBXMAX = MAX(BBXMAX, RBUF(7))
      BBYMAX = MAX(BBYMAX, RBUF(8))

      RETURN
C
C-----------------------------------------------------------------------
C Buffer output if possible.
C
  800 IF ( (LOBUF+L+1). GT. 132) THEN
          CALL GRPS02(IOERR, UNIT, OBUF(1:LOBUF))
          OBUF = INSTR(1:L)
          LOBUF = L
      ELSE
          IF (LOBUF.GT.1) THEN
              LOBUF = LOBUF+1
              OBUF(LOBUF:LOBUF) = ' '
          END IF
          OBUF(LOBUF+1:LOBUF+L) = INSTR(1:L)
          LOBUF = LOBUF+L
      END IF
      RETURN
C-----------------------------------------------------------------------
C Error: unimplemented function.
C
  900 WRITE (MSG,
     1  '(''Unimplemented function in PS device driver: '',I10)') IFUNC
      CALL GRWARN(MSG)
      NBUF = -1
      RETURN
C-----------------------------------------------------------------------
      END

C*GRPS03 -- PGPLOT PostScript driver, marker support
C+
      SUBROUTINE GRPS03(IOERR, NSYM, UNIT)
      IMPLICIT NONE
      INTEGER IOERR, NSYM, UNIT
C
C Write PostScript instructions for drawing graph marker number NSYM
C on Fortran unit UNIT.
C-----------------------------------------------------------------------
      CHARACTER*80 T(6)
      INTEGER I, N
C
      IF (NSYM.LT.0 .OR. NSYM.GT.31) RETURN
      GOTO (100, 101, 102, 103, 104, 105, 106, 107, 108,
     1      109, 110, 111, 112, 113, 114, 115, 116, 117,
     2      118, 119, 120, 121, 122, 123, 124, 125, 126,
     3      127, 128, 129, 130, 131) NSYM+1
C
  100 T(1)='/M0 {MB -6 -6 moveto 0 12 rlineto 12 0 rlineto'
      T(2)='0 -12 rlineto closepath stroke ME} bind def'
      N=2
      GOTO 200
  101 T(1)='/M1 {MB 0 0 1 FC ME} bind def'
      N=1
      GOTO 200
  102 T(1)='/M2 {MB 0 7 moveto 0 -14 rlineto -7 0 moveto'
      T(2)='14 0 rlineto stroke ME} bind def'
      N=2
      GOTO 200
  103 T(1)='/M3 {MB 0 6 moveto 0 -6 lineto -5 3 moveto 5 -3 lineto'
      T(2)='5 3 moveto -5 -3 lineto stroke ME} bind def'
      N=2
      GOTO 200
  104 T(1)='/M4 {MB 0 0 7 CC ME} bind def'
      N=1
      GOTO 200
  105 T(1)='/M5 {MB -5 -5 moveto 10 10 rlineto -5 5 moveto'
      T(2)='10 -10 rlineto stroke ME} bind def'
      N=2
      GOTO 200
  106 T(1)='/M6 {MB -6 -6 moveto 0 12 rlineto 12 0 rlineto'
      T(2)='0 -12 rlineto closepath stroke ME} bind def'
      N=2
      GOTO 200
  107 T(1)='/M7 {MB 0 8 moveto -7 -4 lineto 7 -4 lineto closepath'
      T(2)='stroke ME} bind def'
      N=2
      GOTO 200 
  108 T(1)='/M8 {MB 0 7 moveto 0 -14 rlineto -7 0 moveto 14 0 rlineto'
      T(2)='stroke 0 0 7 CC ME} bind def'
      N=2
      GOTO 200
  109 T(1)='/M9 {MB 0 0 1 FC 0 0 7 CC ME} bind def'
      N=1
      GOTO 200
  110 T(1)='/M10 {MB -9 9 moveto -8 7 lineto -7 3 lineto -7 -3 lineto'
      T(2)='-8 -7 lineto -9 -9 lineto -7 -8 lineto -3 -7 lineto'
      T(3)='3 -7 lineto 7 -8 lineto 9 -9 lineto 8 -7 lineto'
      T(4)='7 -3 lineto 7 3 lineto 8 7 lineto 9 9 lineto 7 8 lineto'
      T(5)='3 7 lineto -3 7 lineto  -7 8 lineto closepath stroke'
      T(6)='ME} bind def'
      N=6
      GOTO 200
  111 T(1)='/M11 {MB 0 10 moveto -6 0 lineto 0 -10 lineto 6 0 lineto'
      T(2)='closepath stroke ME} bind def'
      N=2
      GOTO 200
  112 T(1)='/M12 {MB 0 9 moveto -2 3 lineto -8 3 lineto -3 -1 lineto'
      T(2)='-5 -7 lineto 0 -3 lineto 5 -7 lineto 3 -1 lineto 8 3'
      T(3)='lineto 2 3 lineto closepath stroke ME} bind def'
      N=3
      GOTO 200
  113 T(1)='/M13 {MB 0 8 moveto -7 -4 lineto 7 -4 lineto closepath'
      T(2)='fill ME} bind def'
      N=2
      GOTO 200
  114 T(1)='/M14 {MB -2 6 moveto -2 2 lineto -6 2 lineto -6 -2 lineto'
      T(2)='-2 -2 lineto -2 -6 lineto 2 -6 lineto 2 -2 lineto'
      T(3)='6 -2 lineto 6 2 lineto 2 2 lineto 2 6 lineto closepath'
      T(4)='stroke ME} bind def'
      N=4
      GOTO 200
  115 T(1)='/M15 {MB 0 8 moveto -7 -4 lineto 7 -4 lineto closepath'
      T(2)='0 -8 moveto 7 4 lineto -7 4 lineto closepath stroke ME}'
      T(3)='bind def'
      N=3
      GOTO 200
  116 T(1)='/M16 {MB -4 -4 moveto 0 8 rlineto 8 0 rlineto 0 -8'
      T(2)='rlineto closepath fill ME} bind def'
      N=2
      GOTO 200
  117 T(1)='/M17 {MB 0 0 4.5 FC ME} bind def'
      N=1
      GOTO 200
  118 T(1)='/M18 {MB 0 9 moveto -2 3 lineto -8 3 lineto -3 -1 lineto'
      T(2)=' -5 -7 lineto 0 -3 lineto 5 -7 lineto 3 -1 lineto 8 3'
      T(3)='lineto 2 3 lineto closepath fill ME} bind def'
      N=3
      GOTO 200
  119 T(1)='/M19 {MB -12 -12 moveto 0 24 rlineto 24 0 rlineto 0 -24'
      T(2)='rlineto closepath stroke ME} bind def'
      N=2
      GOTO 200
  120 T(1)='/M20 {MB 0 0 2 CC ME} bind def'
      N=1
      GOTO 200
  121 T(1)='/M21 {MB 0 0 4 CC ME} bind def'
      N=1
      GOTO 200
  122 T(1)='/M22 {MB 0 0 5 CC ME} bind def'
      N=1
      GOTO 200
  123 T(1)='/M23 {MB 0 0 7 CC ME} bind def'
      N=1
      GOTO 200
  124 T(1)='/M24 {MB 0 0 11 CC ME} bind def'
      N=1
      GOTO 200
  125 T(1)='/M25 {MB 0 0 17 CC ME} bind def'
      N=1
      GOTO 200
  126 T(1)='/M26 {MB 0 0 22 CC ME} bind def'
      N=1
      GOTO 200
  127 T(1)='/M27 {MB 0 0 41 CC ME} bind def'
      N=1
      GOTO 200
  128 T(1)='/M28 {MB -6 2 moveto -9 0 lineto -6 -2 lineto -3 5'
      T(2)='moveto -8 0 lineto -3 -5 lineto -8 0 moveto 9 0 lineto'
      T(3)='stroke ME} bind def'
      N=3
      GOTO 200
  129 T(1)='/M29 {MB 6 2 moveto 9 0 lineto 6 -2 lineto 3 5 moveto'
      T(2)='8 0 lineto 3 -5 lineto 8 0 moveto -9 0 lineto stroke ME}'
      T(3)='bind def'
      N=3
      GOTO 200
  130 T(1)='/M30 {MB 2 6 moveto 0 9 lineto -2 6 lineto 5 3 moveto'
      T(2)='0 8 lineto -5 3 lineto 0 8 moveto 0 -9 lineto stroke ME}'
      T(3)='bind def'
      N=3
      GOTO 200
  131 T(1)='/M31 {MB 2 -6 moveto 0 -9 lineto -2 -6 lineto 5 -3'
      T(2)='moveto 0 -8 lineto -5 -3 lineto 0 -8 moveto 0 9 lineto'
      T(3)='stroke ME} bind def'
      N=3
      GOTO 200
C
  200 DO 210 I=1,N
          CALL GRPS02(IOERR, UNIT, T(I))
  210 CONTINUE
C
      END

C*GRPS02 -- PGPLOT PostScript driver, copy buffer to file
C+
      SUBROUTINE GRPS02 (IER, UNIT, S)
C
C Support routine for PSdriver: write character string S on
C specified Fortran unit.
C
C Error handling: if IER is not 0 on input, the routine returns
C immediately. Otherwise IER receives the I/O status from the Fortran
C write (0 => success).
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER, UNIT
      CHARACTER*(*) S
      INTEGER GRTRIM
      EXTERNAL GRTRIM
C
      IF (IER.EQ.0) THEN
          WRITE (UNIT, '(A)', IOSTAT=IER) S(1:GRTRIM(S))
          IF (IER.NE.0) CALL 
     1        GRWARN('++WARNING++ Error writing PostScript file')
      END IF
C-----------------------------------------------------------------------
      END

C*GRPSSY -- PGPLOT PostScript driver, create PS commands to write string
C+          at specified position, orientation and font size
C+
      SUBROUTINE GRPSSY (INSTR, OUTSTR, ORIENT, FONTSC, X0, Y0, FNTUSE)
C
C   direct postscript driver for pgplot B. Dorman L3 Com EER / NASA-GSFC 11/2002
C   maps pgplot '\x' to postscript control sequences
C   remaps Hershey greek characters to Symbol font
C   expects ISOLatin1 encoded fonts (for special times, multiply, dot, and Angstrom).
C   encloses entire string in 'gsave / grestore ' pair
C-----------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER*(*) INSTR, OUTSTR
      REAL ORIENT, FONTSC, X0, Y0
      LOGICAL FNTUSE(11)

      REAL FONTSZ
      INTEGER LEN, I, J, K, GRTRIM, IF, LEVEL
      LOGICAL CONTRL, HERSHY
      CHARACTER BSLASH*1, C*1, DN*2, FONT*4, CURFNT*4, TEXT*2048

      CHARACTER GRPS04*1
      LOGICAL GRPS06, GRPS07
      EXTERNAL GRPS04, GRPS06, GRPS07

      IF (INSTR(1:1).EQ.CHAR(0)) RETURN
      BSLASH = CHAR(92)
      DN = CHAR(92)//'d'
      LEN = GRTRIM(INSTR)
C
C  set the current font
C
      CALL GRQFNT(IF)
      IF ( IF .EQ. 1 ) THEN
         FONT = 'HV  '
         CURFNT = 'HVC '
         FNTUSE(5) = .TRUE.
      ELSEIF ( IF .EQ. 2 ) THEN
         FONT = 'TR  '
         CURFNT = 'TRC '
         FNTUSE(1) = .TRUE.
      ELSEIF ( IF .EQ. 3 ) THEN
         FONT = 'TI  '
         CURFNT = 'TIC '
         FNTUSE(3) = .TRUE.
      ELSEIF ( IF .EQ. 4 ) THEN
         FONT = 'Z'
         CURFNT = 'ZC '
         FNTUSE(10) = .TRUE.
      ENDIF
C
C  set the font size - this is an attempt to make the text appear the
C  same size as screen fonts
C
      FONTSZ = FONTSC * 1.3
C
C  initialize the sub/superscript level
C
      LEVEL = 0
C
C  set the font scale and the start position and rotation of the text
C
      OUTSTR = ' '
      WRITE (OUTSTR(1:),'(f6.2,1x," pgscale div FS gs")') FONTSZ
      K = GRTRIM(OUTSTR) + 1
      WRITE (OUTSTR(K:),'(1x,f6.2,2(1x,f14.3)," moveto rotate")') 
     1    ORIENT,X0,Y0
      K = GRTRIM(OUTSTR) + 1

      TEXT  = ' '
      J = 0
C
C  loop over the characters in the input text string
C
      I = 1
      DO WHILE ( I .le . LEN )
         C = INSTR(I:I)
C
C  it's a standard character so copy it into the output string
C
         IF ( C .NE. BSLASH ) THEN
            I = I + 1
            J = J + 1
C
C   add a \ character for '(' operators which are special to postscript.
C
            IF ( C .EQ. '(' .OR. C .EQ. ')' ) THEN 
               TEXT(J:J) = BSLASH
               J = J + 1
            ENDIF
            TEXT(J:J) = C
C
C  it's a backslash indicating a special character
C
         ELSE
            I = I + 1
            C = INSTR(I:I)
C
C  we may be changing a font or switching sub/superscripting level so close 
C  out the current text string
C
            IF ( J .GT. 0 ) THEN
               WRITE(OUTSTR(K:),'(" (",a,") ",a)') TEXT(1:J), CURFNT
            ENDIF
C
            TEXT = ' '
            J = 0
            K = GRTRIM(OUTSTR) + 1
C
C  check whether we can do a simple translation from PGPLOT control sequence
C  to PS command
C
            CONTRL = GRPS06 (INSTR, TEXT, FONT, I, J, FNTUSE)
            WRITE(CURFNT,'(a,"C")') FONT(1:GRTRIM(FONT))             
C
C  If the control sequence is \g, \u, \d, or \( we have to handle it specially
C
            IF (.NOT.CONTRL) THEN

               IF ( C .EQ. 'G' .OR. C. eq. 'g' .OR.
     &              C .EQ. 'U' .OR. C .EQ. 'u' .OR.
     &              C .EQ. 'D' .OR. C .EQ. 'd' .OR.
     &              C .EQ. '(' ) THEN

                  I = I + 1
C
C  It's a Greek character
C
                  IF ( C .EQ. 'G' .OR. C. eq. 'g' ) THEN
C
                     WRITE(OUTSTR(K:),
     1                  '(" (",a,")",1x," SC")') GRPS04(INSTR(I:I))
                     I = I + 1
C
C  Special case for the Solar symbol since there is no direct translation
C  of this using a standard font so we have to construct it
C 
                  ELSEIF ( INSTR(I:I+4) .EQ. '2281)' ) THEN

                     I = I + 5
                     WRITE(OUTSTR(K:),'(5(1x,a))') '(O)', 'HVC', 
     1                  'half 1.0275 mul neg h quart 1.25 mul v (.)', 
     2                  'HVC', 'quart 1.25 mul neg v quart h'

C
C  Special case for the approx equals symbol since there is no direct 
C  translation of this using a standard font so we have to construct it
C 
                  ELSEIF ( INSTR(I:I+4) .EQ. '0248)' ) THEN

                     I = I + 5
                     WRITE(OUTSTR(K:),'(5(1x,a))') 
     1                  'quart 0.25 mul neg v (~)', CURFNT, 
     2                  'half 1.1 mul neg h quart 0.5 mul v (~)', 
     3                  CURFNT, 'quart 0.25 mul neg v'

C
C  It's a Hershey symbol specification
C
                  ELSEIF ( C .EQ. '(' ) THEN

                     HERSHY = GRPS07(INSTR, TEXT, CURFNT, I, J, FNTUSE)
                     IF ( HERSHY ) THEN
                        WRITE(OUTSTR(K:),'(1x,a)') TEXT(1:J)
                        TEXT = ' '
                        J = 0
                     ENDIF
C
C  Go up or down
C
                  ELSE

                     IF ( C .EQ. 'U' .OR. C .EQ. 'u') THEN
                        LEVEL = LEVEL + 1
                        IF ( LEVEL .LE. 0 ) THEN
                           WRITE (OUTSTR(K:),'(1x,a,f6.2,a)')
     &                       'up ', FONTSZ*(0.6**ABS(LEVEL)), 
     &                       ' pgscale div FS'
                        ELSE
                           WRITE (OUTSTR(K:),'(1x,f6.2,a)')
     &                       FONTSZ*(0.6**ABS(LEVEL)), 
     &                       ' pgscale div FS up'
                        ENDIF
                     ELSE
                        LEVEL = LEVEL - 1
                        IF ( LEVEL .GE. 0 ) THEN
                           WRITE (OUTSTR(K:),'(1x,a,f6.2,a)')
     &                       'dn ', FONTSZ*(0.6**ABS(LEVEL)), 
     &                       ' pgscale div FS'
                        ELSE
                           WRITE (OUTSTR(K:),'(1x,f6.2,a)')
     &                       FONTSZ*(0.6**ABS(LEVEL)), 
     &                       ' pgscale div FS dn'
                        ENDIF
                     ENDIF

                  ENDIF

                  K = GRTRIM(OUTSTR) + 1                  
C
C  It's an unrecognised character following a backslash
C
               ELSE

                  J = J + 1
                  TEXT(J:J) = BSLASH

               ENDIF

            ENDIF
         ENDIF
      
      ENDDO
      
C
C WRITE whatever's left and put the grestore on the end
C

      IF ( J .GT. 0 ) THEN
         WRITE(OUTSTR(K:),'(" (",a,") ",a)') TEXT(1:J),CURFNT
      ENDIF
      K = GRTRIM(OUTSTR) + 1                  
      WRITE(OUTSTR(K:),'(" gr")')

      RETURN
      END
      
C*GRPS04 -- PGPLOT PostScript driver, Greek text translation required because
C           PS and PGPLOT use different Latin characters to represent some
C           Greek characters.
C+
      FUNCTION GRPS04(C)
C
C-----------------------------------------------------------------------

      IMPLICIT NONE
      CHARACTER C*1, GRPS04*1

      CHARACTER*52 PSGRK, PGGRK
      PARAMETER (PGGRK = 'ABGDEZYHIKLMNCOPRSTUFXQWJV' //
     1                     'abgdezyhiklmncoprstufxqwjv' )
      INTEGER IFIND
      DATA PSGRK
     1    /'ABGDEZHQIKLMNXOPRSTUFCYWJVabgdezhqiklmnxoprstufcywjv'/   
     
         
      IFIND = INDEX(PGGRK,C)
      GRPS04 = PSGRK(IFIND:IFIND)

      RETURN
      END
     
C*GRPS06 -- PGPLOT PostScript driver, translates PGPLOT control sequences to PS
C+
      LOGICAL FUNCTION GRPS06( TXTSTR, TEXT, FONT, I, J, FNTUSE)
C
C fc  Helvetica-Narrow
C fC  Helvetica-Narrow-Bold
C fr  Times-Roman
C fR  Times-Bold
C fn  Helvetica
C fN  Helvetica-Bold
C fi  Times-Italic
C fI  Times-BoldItalic
C fs  Zapf-Chancery-MediumItalic
C A   Angstrom symbol (\305)
C x   times symbol (\267)
C .   dot symbol (\327)
C o   divide symbol (\367)
C 
C  
C-----------------------------------------------------------------------

      IMPLICIT NONE
      CHARACTER*(*) TXTSTR
      CHARACTER TEXT*6, FONT*4
      INTEGER I, J
      LOGICAL FNTUSE(11)

      CHARACTER*3 ANGSTROM,DOT,TIMES,DIV
      PARAMETER (ANGSTROM="305",DOT="267",TIMES="327",DIV="367")

      CHARACTER*1 C, BSLASH

      GRPS06 = .true.     
      BSLASH = CHAR(92)
      C = TXTSTR (I:I)
      IF ( C .EQ. 'f' .OR. C .EQ. 'F' ) THEN
         I = I + 1
         IF (TXTSTR(I:I).EQ.'c') THEN
            FONT = 'HN '
            FNTUSE(7) = .TRUE.
         ENDIF
         IF (TXTSTR(I:I).EQ.'C') THEN
            FONT = 'HNB'
            FNTUSE(8) = .TRUE.
         ENDIF
         IF (TXTSTR(I:I).EQ.'r') THEN
            FONT = 'TR '
            FNTUSE(1) = .TRUE.
         ENDIF
         IF (TXTSTR(I:I).EQ.'R') THEN
            FONT = 'TB '
            FNTUSE(2) = .TRUE.
         ENDIF
         IF (TXTSTR(I:I).EQ.'n') THEN
            FONT = 'HV '
            FNTUSE(5) = .TRUE.
         ENDIF
         IF (TXTSTR(I:I).EQ.'N') THEN
            FONT = 'HB '
            FNTUSE(9) = .TRUE.
         ENDIF
         IF (TXTSTR(I:I).EQ.'i') THEN
            FONT = 'TI '
            FNTUSE(3) = .TRUE.
         ENDIF
         IF (TXTSTR(I:I).EQ.'I') THEN
            FONT = 'TBI'
            FNTUSE(4) = .TRUE.
         ENDIF
         IF (TXTSTR(I:I).EQ.'s') THEN
            FONT = 'Z  '
            FNTUSE(10) = .TRUE.
         ENDIF
         I = I + 1
      ELSE IF ( C .EQ. 'A') THEN
         J = J + 1
         WRITE (TEXT(J:J+3),'(a,a3)') BSLASH, ANGSTROM
         J = J + 3
         I = I + 1
      ELSE IF ( C .EQ. 'x' .OR. C .EQ. 'X' ) THEN
         J = J + 1
         WRITE (TEXT(J:J+3),'(a,a3)') BSLASH, TIMES
         J = J + 3
         I = I + 1
      ELSE IF ( C .EQ. '.') THEN
         J = J + 1
         WRITE (TEXT(J:J+3),'(a,a3)') BSLASH, DOT
         J = J + 3
         I = I + 1
      ELSE IF ( C .EQ. 'o') THEN
         J = J + 1
         WRITE (TEXT(J:J+3),'(a,a3)') BSLASH, DIV
         J = J + 3
         I = I + 1
      ELSE
         GRPS06 = .false.
      ENDIF

      RETURN 
      END

C*GRPS07 -- PGPLOT PostScript driver, translates a Hershey code specification
C+          into a PS octal code
C+
      LOGICAL FUNCTION GRPS07(TXTSTR, TEXT, FONT, I, J, FNTUSE)
C
C-----------------------------------------------------------------------

      IMPLICIT NONE
      CHARACTER*(*) TXTSTR, TEXT
      CHARACTER FONT*4
      INTEGER I, J
      LOGICAL FNTUSE(11)


      INTEGER SYMNUM, OCTNUM, IERR
      CHARACTER*4 CURFNT
      CHARACTER*1 BSLASH

      INTEGER GRTRIM, GRPS09
      CHARACTER*3 GRPS08
      EXTERNAL GRPS08, GRTRIM, GRPS09

C Check whether this is a valid Hershey font

      GRPS07 = .FALSE.
      IF ( TXTSTR(I+4:I+4) .NE. ')' ) RETURN
      READ(TXTSTR(I:I+3),'(i4)',IOSTAT=IERR) SYMNUM
      I = I + 5
      IF ( IERR .NE. 0 ) RETURN
      IF ( SYMNUM .LT. 1 .OR. SYMNUM .GT. 2932 ) RETURN

C The default behaviour will be to return the code for a space

      GRPS07 = .TRUE.
      BSLASH = CHAR(92)
      CURFNT = FONT
      TEXT = '040'

C Big set of IF block doing the conversion to PS octal codes

C 1 - 26 Capitals in small font

      IF ( SYMNUM .GE. 1 .AND. SYMNUM .LE. 26 ) THEN
         CURFNT = 'HVC'
         FNTUSE(5) = .TRUE.
         OCTNUM = 64 + SYMNUM
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 27 - 50 Greek capitals in small font

      IF ( SYMNUM .GE. 27 .AND. SYMNUM .LE. 50 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         OCTNUM = 64 + GRPS09(SYMNUM-26)
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 200 - 209 Integers in small font

      IF ( SYMNUM .GE. 200 .AND. SYMNUM .LE. 209 ) THEN
         CURFNT = 'HVC'
         FNTUSE(5) = .TRUE.
         OCTNUM = 47 + SYMNUM - 199
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 210 - 259 Symbols in small font

      IF ( SYMNUM .EQ. 210 ) TEXT = '056'
      IF ( SYMNUM .EQ. 211 ) TEXT = '054'
      IF ( SYMNUM .EQ. 212 ) TEXT = '072'
      IF ( SYMNUM .EQ. 213 ) TEXT = '073'
      IF ( SYMNUM .EQ. 214 ) TEXT = '041'
      IF ( SYMNUM .EQ. 215 ) TEXT = '077'
      IF ( SYMNUM .EQ. 216 ) TEXT = '234'
      IF ( SYMNUM .EQ. 217 ) TEXT = '042'
      IF ( SYMNUM .EQ. 218 ) TEXT = '232'
      IF ( SYMNUM .EQ. 219 ) TEXT = '044'
      IF ( SYMNUM .EQ. 220 ) TEXT = '204'
      IF ( SYMNUM .EQ. 221 ) TEXT = '050'
      IF ( SYMNUM .EQ. 222 ) TEXT = '051'
      IF ( SYMNUM .EQ. 223 ) TEXT = '174'
      IF ( SYMNUM .EQ. 224 ) TEXT = '055'
      IF ( SYMNUM .EQ. 225 ) TEXT = '053'
      IF ( SYMNUM .EQ. 226 ) TEXT = '075'
      IF ( SYMNUM .EQ. 227 ) TEXT = '327'
      IF ( SYMNUM .EQ. 228 ) TEXT = '052'
      IF ( SYMNUM .EQ. 229 ) TEXT = '056'
      IF ( SYMNUM .EQ. 230 ) TEXT = '140'
      IF ( SYMNUM .EQ. 231 ) TEXT = '047'
      IF ( SYMNUM .EQ. 232 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         TEXT = '256'
      ENDIF
      IF ( SYMNUM .EQ. 233 ) TEXT = '043'
      IF ( SYMNUM .EQ. 234 ) TEXT = '046'
      IF ( SYMNUM .EQ. 235 ) TEXT = '244'
      IF ( SYMNUM .EQ. 236 ) TEXT = '031'
      IF ( SYMNUM .EQ. 240 ) TEXT = '055'
      IF ( SYMNUM .EQ. 242 ) TEXT = '237'
      IF ( SYMNUM .EQ. 250 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         TEXT = '100'
      ENDIF
      IF ( SYMNUM .EQ. 256 ) TEXT = '031'
      IF ( SYMNUM .EQ. 258 ) TEXT = '055'
      IF ( SYMNUM .EQ. 259 ) TEXT = '055'

C 261 - 271 Fractions

      IF ( SYMNUM .EQ. 261 ) TEXT = '275'
      IF ( SYMNUM .EQ. 270 ) TEXT = '274'
      IF ( SYMNUM .EQ. 271 ) TEXT = '276'

C 272 - 284 Symbols

      IF ( SYMNUM .EQ. 272 ) TEXT = '243'
      IF ( SYMNUM .EQ. 273 ) TEXT = '276'
      IF ( SYMNUM .EQ. 274 ) TEXT = '251'
      IF ( SYMNUM .EQ. 276 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         TEXT = '274'
      ENDIF
      IF ( SYMNUM .EQ. 278 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         TEXT = '253'
      ENDIF
      IF ( SYMNUM .EQ. 282 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         TEXT = '047'
      ENDIF

C 501 - 525 Capitals in normal font

      IF ( SYMNUM .GE. 501 .AND. SYMNUM .LE. 525 ) THEN
         CURFNT = 'HVC'
         FNTUSE(5) = .TRUE.
         OCTNUM = 64 + SYMNUM - 500
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 527 - 550 Greek capitals

      IF ( SYMNUM .GE. 527 .AND. SYMNUM .LE. 550 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         OCTNUM = 64 + GRPS09(SYMNUM-526)
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 551 - 576 Script capitals

      IF ( SYMNUM .GE. 551 .AND. SYMNUM .LE. 576 ) THEN
         CURFNT = ' ZC'
         FNTUSE(10) = .TRUE.
         OCTNUM = 64 + SYMNUM - 550
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 583  del

      IF ( SYMNUM .EQ. 583 ) THEN
         CURFNT = ' SC'
         FNTUSE(11) = .TRUE.
         TEXT = '321'
      ENDIF

C 590  underbar

      IF ( SYMNUM .EQ. 590 ) TEXT = '137'

C 601 - 626 letters in normal font

      IF ( SYMNUM .GE. 601 .AND. SYMNUM .LE. 626 ) THEN
         CURFNT = 'HVC'
         FNTUSE(5) = .TRUE.
         OCTNUM = 96 + SYMNUM - 600
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 627 - 650 greek letters

      IF ( SYMNUM .GE. 627 .AND. SYMNUM .LE. 650 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         OCTNUM = 96 + GRPS09(SYMNUM-626)
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 651 - 676 script letters

      IF ( SYMNUM .GE. 651 .AND. SYMNUM .LE. 676 ) THEN
         CURFNT = 'ZC'
         FNTUSE(10) = .TRUE.
         OCTNUM = 96 + SYMNUM - 650
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 677 - 687 symbols

      IF ( SYMNUM .GE. 677 .AND. SYMNUM .LE. 687 ) THEN
         CURFNT = 'ZC'
         FNTUSE(10) = .TRUE.
         IF ( SYMNUM .EQ. 677 ) TEXT = '154'
         IF ( SYMNUM .EQ. 683 ) TEXT = '266'
         IF ( SYMNUM .EQ. 684 ) TEXT = '145'
         IF ( SYMNUM .EQ. 685 ) TEXT = '161'
         IF ( SYMNUM .EQ. 686 ) TEXT = '146'
         IF ( SYMNUM .EQ. 687 ) TEXT = '126'
      ENDIF

C 700 - 709 integers

      IF ( SYMNUM .GE. 700 .AND. SYMNUM .LE. 709 ) THEN
         CURFNT = 'HVC'
         FNTUSE(5) = .TRUE.
         OCTNUM = 47 + SYMNUM - 699
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 710 - 735 symbols : copies of 210 - 235

      IF ( SYMNUM .EQ. 710 ) TEXT = '056'
      IF ( SYMNUM .EQ. 711 ) TEXT = '054'
      IF ( SYMNUM .EQ. 712 ) TEXT = '072'
      IF ( SYMNUM .EQ. 713 ) TEXT = '073'
      IF ( SYMNUM .EQ. 714 ) TEXT = '041'
      IF ( SYMNUM .EQ. 715 ) TEXT = '077'
      IF ( SYMNUM .EQ. 716 ) TEXT = '222'
      IF ( SYMNUM .EQ. 717 ) TEXT = '235'
      IF ( SYMNUM .EQ. 718 ) TEXT = '232'
      IF ( SYMNUM .EQ. 719 ) TEXT = '044'
      IF ( SYMNUM .EQ. 720 ) TEXT = '204'
      IF ( SYMNUM .EQ. 721 ) TEXT = '050'
      IF ( SYMNUM .EQ. 722 ) TEXT = '051'
      IF ( SYMNUM .EQ. 723 ) TEXT = '174'
      IF ( SYMNUM .EQ. 724 ) TEXT = '055'
      IF ( SYMNUM .EQ. 725 ) TEXT = '053'
      IF ( SYMNUM .EQ. 726 ) TEXT = '075'
      IF ( SYMNUM .EQ. 727 ) TEXT = '327'
      IF ( SYMNUM .EQ. 728 ) TEXT = '052'
      IF ( SYMNUM .EQ. 729 ) TEXT = '056'
      IF ( SYMNUM .EQ. 730 ) TEXT = '140'
      IF ( SYMNUM .EQ. 731 ) TEXT = '047'
      IF ( SYMNUM .EQ. 732 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         TEXT = '256'
      ENDIF
      IF ( SYMNUM .EQ. 733 ) TEXT = '043'
      IF ( SYMNUM .EQ. 734 ) TEXT = '046'
      IF ( SYMNUM .EQ. 735 ) TEXT = '244'

C 737 - 910 symbols : mostly not-supported

      IF ( SYMNUM .GE. 738 .AND. SYMNUM .LE. 766 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         IF ( SYMNUM .EQ. 738 ) TEXT = '136'
         IF ( SYMNUM .EQ. 739 ) TEXT = '320'
         IF ( SYMNUM .EQ. 738 ) TEXT = '134'
         IF ( SYMNUM .EQ. 766 ) TEXT = '245'
      ENDIF

C 2001 - 2026 Capitals in roman font

      IF ( SYMNUM .GE. 2001 .AND. SYMNUM .LE. 2026 ) THEN
         CURFNT = 'TRC'
         FNTUSE(1) = .TRUE.
         OCTNUM = 64 + SYMNUM - 2000
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 2027 - 2050 Greek capitals in bold font

      IF ( SYMNUM .GE. 2027 .AND. SYMNUM .LE. 2076 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         OCTNUM = 64 + GRPS09(SYMNUM-2026)
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 2051 - 2076 Italic capitals

      IF ( SYMNUM .GE. 2051 .AND. SYMNUM .LE. 2076 ) THEN
         CURFNT = 'TIC'
         FNTUSE(3) = .TRUE.
         OCTNUM = 64 + SYMNUM - 2050
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 2077 aleph

      IF ( SYMNUM .EQ. 2077 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         TEXT = '300'
      ENDIF

C 2078 Angstrom

      IF ( SYMNUM .EQ. 2078 ) TEXT = '305'

C 2101 - 2126 letters in roman font

      IF ( SYMNUM .GE. 2101 .AND. SYMNUM .LE. 2126 ) THEN
         CURFNT = 'TRC'
         FNTUSE(1) = .TRUE.
         OCTNUM = 96 + SYMNUM - 2100
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 2127 - 2150 greek letters in bold font

      IF ( SYMNUM .GE. 2127 .AND. SYMNUM .LE. 2150 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         OCTNUM = 96 + GRPS09(SYMNUM-2126)
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 2151 - 2176 italic letters

      IF ( SYMNUM .GE. 2151 .AND. SYMNUM .LE. 2176 ) THEN
         CURFNT = 'TIC'
         FNTUSE(3) = .TRUE.
         OCTNUM = 96 + SYMNUM - 2150
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 2177 - 2199 symbols - roman font

      IF ( SYMNUM .GE. 2177 .AND. SYMNUM .LE. 2199 ) THEN

      ENDIF

C 2200 - 2209 integers - roman font

      IF ( SYMNUM .GE. 2200 .AND. SYMNUM .LE. 2209 ) THEN
         CURFNT = 'TRC'
         FNTUSE(1) = .TRUE.
         OCTNUM = 47 + SYMNUM - 2199
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 2210 - 2412 symbols - roman font

      IF ( SYMNUM .EQ. 2210 ) TEXT = '056'
      IF ( SYMNUM .EQ. 2211 ) TEXT = '054'
      IF ( SYMNUM .EQ. 2212 ) TEXT = '072'
      IF ( SYMNUM .EQ. 2213 ) TEXT = '073'
      IF ( SYMNUM .EQ. 2214 ) TEXT = '041'
      IF ( SYMNUM .EQ. 2215 ) TEXT = '077'
      IF ( SYMNUM .EQ. 2216 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         TEXT = '242'
      ENDIF
      IF ( SYMNUM .EQ. 2217 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         TEXT = '262'
      ENDIF
      IF ( SYMNUM .EQ. 2218 ) TEXT = '260'
      IF ( SYMNUM .EQ. 2219 ) TEXT = '052'
      IF ( SYMNUM .EQ. 2220 ) TEXT = '204'
      IF ( SYMNUM .EQ. 2221 ) TEXT = '050'
      IF ( SYMNUM .EQ. 2222 ) TEXT = '051'
      IF ( SYMNUM .EQ. 2223 ) TEXT = '133'
      IF ( SYMNUM .EQ. 2224 ) TEXT = '135'
      IF ( SYMNUM .EQ. 2225 ) TEXT = '173'
      IF ( SYMNUM .EQ. 2226 ) TEXT = '175'
      IF ( SYMNUM .EQ. 2227 ) TEXT = '341'

      IF ( SYMNUM .GE. 2228 .AND. SYMNUM .LE. 2246 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         IF ( SYMNUM .EQ. 2228 ) TEXT = '361'
         IF ( SYMNUM .EQ. 2229 ) TEXT = '347'
         IF ( SYMNUM .EQ. 2231 ) TEXT = '055'
         IF ( SYMNUM .EQ. 2232 ) TEXT = '053'
         IF ( SYMNUM .EQ. 2233 ) TEXT = '261'
         IF ( SYMNUM .EQ. 2235 ) TEXT = '264'
         IF ( SYMNUM .EQ. 2236 ) TEXT = '056'
         IF ( SYMNUM .EQ. 2237 ) TEXT = '270'
         IF ( SYMNUM .EQ. 2238 ) TEXT = '075'
         IF ( SYMNUM .EQ. 2239 ) TEXT = '271'
         IF ( SYMNUM .EQ. 2240 ) TEXT = '272'
         IF ( SYMNUM .EQ. 2241 ) TEXT = '074'
         IF ( SYMNUM .EQ. 2242 ) TEXT = '076'
         IF ( SYMNUM .EQ. 2243 ) TEXT = '243'
         IF ( SYMNUM .EQ. 2244 ) TEXT = '263'
         IF ( SYMNUM .EQ. 2245 ) TEXT = '265'
         IF ( SYMNUM .EQ. 2246 ) TEXT = '176'
      ENDIF

      IF ( SYMNUM .EQ. 2247 ) TEXT = '223'
      IF ( SYMNUM .EQ. 2248 ) TEXT = '221'
      IF ( SYMNUM .EQ. 2249 ) TEXT = '222'
      IF ( SYMNUM .EQ. 2250 ) TEXT = '226'
      IF ( SYMNUM .EQ. 2251 ) TEXT = '047'
      IF ( SYMNUM .EQ. 2252 ) TEXT = '140'

      IF ( SYMNUM .GE. 2255 .AND. SYMNUM .LE. 2272 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         IF ( SYMNUM .EQ. 2255 ) TEXT = '326'
         IF ( SYMNUM .EQ. 2256 ) TEXT = '314'
         IF ( SYMNUM .EQ. 2257 ) TEXT = '310'
         IF ( SYMNUM .EQ. 2258 ) TEXT = '311'
         IF ( SYMNUM .EQ. 2259 ) TEXT = '307'
         IF ( SYMNUM .EQ. 2260 ) TEXT = '316'
         IF ( SYMNUM .EQ. 2261 ) TEXT = '256'
         IF ( SYMNUM .EQ. 2262 ) TEXT = '255'
         IF ( SYMNUM .EQ. 2263 ) TEXT = '254'
         IF ( SYMNUM .EQ. 2264 ) TEXT = '257'
         IF ( SYMNUM .EQ. 2265 ) TEXT = '266'
         IF ( SYMNUM .EQ. 2266 ) TEXT = '321'
         IF ( SYMNUM .EQ. 2267 ) TEXT = '326'
         IF ( SYMNUM .EQ. 2268 ) TEXT = '362'
         IF ( SYMNUM .EQ. 2270 ) TEXT = '245'
         IF ( SYMNUM .EQ. 2271 ) TEXT = '045'
         IF ( SYMNUM .EQ. 2272 ) TEXT = '046'
      ENDIF

      IF ( SYMNUM .EQ. 2273 ) TEXT = '100'
      IF ( SYMNUM .EQ. 2274 ) TEXT = '044'
      IF ( SYMNUM .EQ. 2275 ) TEXT = '043'
      IF ( SYMNUM .EQ. 2276 ) TEXT = '247'
      IF ( SYMNUM .EQ. 2277 ) TEXT = '201'
      IF ( SYMNUM .EQ. 2278 ) TEXT = '202'
      IF ( SYMNUM .EQ. 2279 ) THEN
         CURFNT = 'SC'
         FNTUSE(11) = .TRUE.
         TEXT = '044'
      ENDIF

C 2501 - 2526 Capitals in bold font

      IF ( SYMNUM .GE. 2501 .AND. SYMNUM .LE. 2526 ) THEN
         CURFNT = 'TBC'
         FNTUSE(2) = .TRUE.
         OCTNUM = 64 + SYMNUM - 2500
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 2551 - 2576 Script capitals in bold font

      IF ( SYMNUM .GE. 2551 .AND. SYMNUM .LE. 2576 ) THEN
         CURFNT = 'ZC'
         FNTUSE(10) = .TRUE.
         OCTNUM = 64 + SYMNUM - 2550
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 2601 - 2626 letters in bold font

      IF ( SYMNUM .GE. 2601 .AND. SYMNUM .LE. 2626 ) THEN
         CURFNT = 'TBC'
         FNTUSE(2) = .TRUE.
         OCTNUM = 96 + SYMNUM - 2600
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 2651 - 2676 script letters in bold font

      IF ( SYMNUM .GE. 2651 .AND. SYMNUM .LE. 2676 ) THEN
         CURFNT = 'ZC'
         FNTUSE(10) = .TRUE.
         OCTNUM = 96 + SYMNUM - 2650
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 2700 - 2709 numbers in bold font

      IF ( SYMNUM .GE. 2700 .AND. SYMNUM .LE. 2709 ) THEN
         CURFNT = 'TBC'
         FNTUSE(2) = .TRUE.
         OCTNUM = 47 + SYMNUM - 2699
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 2710 - 2729 symbols in bold font

      IF ( SYMNUM .GE. 2710 .AND. SYMNUM .LE. 2729 ) THEN
         CURFNT = 'TBC'
         FNTUSE(2) = .TRUE.
         IF ( SYMNUM .EQ. 2710 ) TEXT = '056'
         IF ( SYMNUM .EQ. 2711 ) TEXT = '054'
         IF ( SYMNUM .EQ. 2712 ) TEXT = '072'
         IF ( SYMNUM .EQ. 2713 ) TEXT = '073'
         IF ( SYMNUM .EQ. 2714 ) TEXT = '041'
         IF ( SYMNUM .EQ. 2715 ) TEXT = '077'
         IF ( SYMNUM .EQ. 2716 ) TEXT = '140'
         IF ( SYMNUM .EQ. 2717 ) TEXT = '047'
         IF ( SYMNUM .EQ. 2718 ) TEXT = '046'
         IF ( SYMNUM .EQ. 2719 ) TEXT = '044'
         IF ( SYMNUM .EQ. 2720 ) TEXT = '204'
         IF ( SYMNUM .EQ. 2721 ) TEXT = '050'
         IF ( SYMNUM .EQ. 2722 ) TEXT = '051'
         IF ( SYMNUM .EQ. 2723 ) TEXT = '052'
         IF ( SYMNUM .EQ. 2724 ) TEXT = '055'
         IF ( SYMNUM .EQ. 2725 ) TEXT = '053'
         IF ( SYMNUM .EQ. 2726 ) TEXT = '075'
         IF ( SYMNUM .EQ. 2727 ) THEN
            CURFNT = 'SC'
            FNTUSE(11) = .TRUE.
            TEXT = '242'
         ENDIF
         IF ( SYMNUM .EQ. 2728 ) THEN
            CURFNT = 'SC' 
            FNTUSE(11) = .TRUE.
            TEXT = '262'
         ENDIF
         IF ( SYMNUM .EQ. 2729 ) TEXT = '260'
      ENDIF

C 2750 - 2759 italic numbers

      IF ( SYMNUM .GE. 2750 .AND. SYMNUM .LE. 2759 ) THEN
         CURFNT = 'TIC'
         FNTUSE(3) = .TRUE.
         OCTNUM = 47 + SYMNUM - 2749
         TEXT = GRPS08(OCTNUM)
      ENDIF

C 2760 - 2779 italic symbols

      IF ( SYMNUM .GE. 2760 .AND. SYMNUM .LE. 2779 ) THEN
         CURFNT = 'TIC'
         FNTUSE(3) = .TRUE.
         IF ( SYMNUM .EQ. 2760 ) TEXT = '056'
         IF ( SYMNUM .EQ. 2761 ) TEXT = '054'
         IF ( SYMNUM .EQ. 2762 ) TEXT = '072'
         IF ( SYMNUM .EQ. 2763 ) TEXT = '073'
         IF ( SYMNUM .EQ. 2764 ) TEXT = '041'
         IF ( SYMNUM .EQ. 2765 ) TEXT = '077'
         IF ( SYMNUM .EQ. 2766 ) TEXT = '140'
         IF ( SYMNUM .EQ. 2767 ) TEXT = '047'
         IF ( SYMNUM .EQ. 2768 ) TEXT = '046'
         IF ( SYMNUM .EQ. 2769 ) TEXT = '044'
         IF ( SYMNUM .EQ. 2770 ) TEXT = '204'
         IF ( SYMNUM .EQ. 2771 ) TEXT = '050'
         IF ( SYMNUM .EQ. 2772 ) TEXT = '051'
         IF ( SYMNUM .EQ. 2773 ) TEXT = '052'
         IF ( SYMNUM .EQ. 2774 ) TEXT = '055'
         IF ( SYMNUM .EQ. 2775 ) TEXT = '053'
         IF ( SYMNUM .EQ. 2776 ) TEXT = '075'
         IF ( SYMNUM .EQ. 2777 ) THEN
            CURFNT = 'SC'
            FNTUSE(11) = .TRUE.
            TEXT = '242'
         ENDIF
         IF ( SYMNUM .EQ. 2778 ) THEN
            CURFNT = 'SC'
            FNTUSE(11) = .TRUE.
            TEXT = '262'
         ENDIF
         IF ( SYMNUM .EQ. 2779 ) TEXT = '260'
      ENDIF

C 2801 - 2832 cyrillic capitals

      IF ( SYMNUM .GE. 2801 .AND. SYMNUM .LE. 2832 ) THEN

      ENDIF

C 2901 - 2932 cyrillic letters

      IF ( SYMNUM .GE. 2901 .AND. SYMNUM .LE. 2932 ) THEN

      ENDIF
      
C Encase the string in parentheses and specify the font to use

      TEXT = '('//BSLASH//TEXT(:GRTRIM(TEXT))//') '//CURFNT
      J = GRTRIM(TEXT)


      RETURN 
      END

C*GRPS08 -- PGPLOT PostScript driver, converts a number into a three
C+          character octal string
C+          into a PS octal code
      CHARACTER*3 FUNCTION GRPS08(NUMBER)
C
C-----------------------------------------------------------------------

      IMPLICIT NONE
      INTEGER NUMBER, IREM
      CHARACTER*3 OCTSYM

      WRITE(OCTSYM(1:1),'(i1)') NUMBER/64
      IREM = NUMBER - 64 * ( NUMBER/64 )
      WRITE(OCTSYM(2:2),'(i1)') IREM/8
      IREM = IREM - 8 * ( IREM/8 )
      WRITE(OCTSYM(3:3),'(i1)') IREM

      GRPS08 = OCTSYM

      END

C*GRPS09 -- PGPLOT PostScript driver, converts between Greek letter order
C+          in Hershey fonts and PS. NB counts from alpha = 1.
C+
      INTEGER FUNCTION GRPS09(HERNUM)
C
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER NUMB
      PARAMETER (NUMB=24)

      INTEGER HERNUM
      INTEGER PSNUM(NUMB)

      DATA PSNUM / 1, 2, 7, 4, 5,26, 8,17, 9,11, 
     &            12,13,14,24,15,16,18,19,20,21,
     &             6, 3,25,23 /

      GRPS09 = PSNUM(HERNUM)

      END

C*GRPS10 -- PGPLOT PostScript driver, write the prolog
C+
      SUBROUTINE GRPS10 (IOERR, UNIT)
C
C Support routine for PSdriver: write the file prolog
C
C Error handling: if IOERR is not 0 on input, the routine returns
C immediately. Otherwise IOERR receives the I/O status from the Fortran
C write (0 => success).
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IOERR, UNIT
      INTEGER GRTRIM

      CHARACTER*255 PLINE

      IF ( IOERR .NE. 0 ) RETURN

      PLINE = '%%BeginProlog'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))

      PLINE = '/gs /gsave load def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/gr /grestore  load def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))

      PLINE = '/L {moveto rlineto currentpoint stroke moveto} bind def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/C {rlineto currentpoint stroke moveto} bind def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/D {moveto 0 0 rlineto currentpoint stroke moveto}'
     &      //' bind def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/SLW {5 mul setlinewidth} bind def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/SCF /pop load def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/BP {newpath moveto} bind def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/LP /rlineto load def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/EP {rlineto closepath eofill} bind def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/MB {gsave translate MFAC dup scale 1 setlinewidth 2'
     &      //' setlinecap 0 setlinejoin newpath} bind def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/ME /grestore load def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/CC {0 360 arc stroke} bind def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/FC {0 360 arc fill} bind def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))

      PLINE = '% font stuff'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))

      PLINE = '/pgscale 0.072 def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))

      PLINE = '% ISOLATIN encoding with built in linespacing'
     &      //' definitions.'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '% from tinydict by David Byram-Wigfield,'
     &      //' http://www.capella.demon.co.uk'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))


      PLINE = '% fontsize default setting.'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/FS	{ /fs exch def } def	 12 FS '
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))

      PLINE = '% linespacing ratio to fontsize: 1.3 is my default'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/LR {/lr exch def } def 1.3 LR'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/LG { /lg exch def } def   12 LG'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))

      PLINE = '% calculate linespacing from chosen fontsize'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/F { exch dup lr mul LG dup FS exch findfont exch'
     &      //' scalefont setfont } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))


      PLINE = '/ISOFONT { findfont dup length dict begin'
     &      //' { 1 index /FID ne {def} {pop pop}'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = 'ifelse } forall /Encoding ISOLatin1Encoding def'
     &      //' currentdict end /tmpfont'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = 'exch definefont exch dup lr mul LG dup FS'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = ' scalefont setfont }  def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))

      PLINE = '/half { fs 2 div } bind def % half linespacing'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/quart { fs 4 div } bind def % quarter linespacing'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '% vertical kerning: e.g. 5 v or -10 v'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/v { 0 exch rmoveto } def '
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '% horizontal kerning: e.g. 5 h or -10 h'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/h { 0 rmoveto } def '
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '% move up half the linespacing'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/up { half v } def '
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '% move down half the linespacing'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/dn { half neg v } def '
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))

      PLINE = '/TRL {/Times-Roman ISOFONT} bind def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/TBL {/Times-Bold ISOFONT} bind def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/TIL {/Times-Italic ISOFONT} bind def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/TBIL {/Times-BoldItalic ISOFONT} bind def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/HVL  {/Helvetica ISOFONT} bind def '
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/HOL  {/Helvetica-Oblique ISOFONT} bind def '
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/HNL  {/Helvetica-Narrow ISOFONT} bind def '
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/HNBL  {/Helvetica-NarrowBold ISOFONT} bind def '
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/HBL  {/Helvetica-Bold ISOFONT} bind def '
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/ZL  {/Zapf-Chancery-MediumItalic ISOFONT} bind def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/SL { /Symbol F } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/RS{findfont exch scalefont setfont moveto dup'
     &      //' stringwidth neg exch neg exch rmoveto show} bind def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/TR  { fs TRL   moveto rotate show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/TB  { fs TBL   moveto rotate show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/TI  { fs TIL   moveto rotate show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/TBI { fs TBIL  moveto rotate show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/HV  { fs HVL   moveto rotate show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/H0  { fs HOL   moveto rotate show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/HB  { fs HBL   moveto rotate show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/HN  { fs HNL   moveto rotate show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/HNB { fs HNBL  moveto rotate show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/Z   { fs ZL    moveto rotate show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/S   { fs SL    moveto rotate show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/TRC  { fs TRL   show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/TBC  { fs TBL   show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/TIC  { fs TIL   show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/TBIC { fs TBIL  show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/HVC  { fs HVL   show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/H0C  { fs HOL   show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/HNBC { fs HNBL  show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/HBC  { fs HBL   show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/HNC  { fs HNL   show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/ZC   { fs ZL    show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '/SC   { fs SL    show currentpoint } def'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))
      PLINE = '%%EndProlog'
      CALL GRPS02(IOERR, UNIT, PLINE(1:GRTRIM(PLINE)))

      RETURN
      END

C*GRPSLF -- PGPLOT PostScript driver, returns the character length correction
C+
      FUNCTION GRPSLF (SYMNUM)
C
C Support routine for PSdriver: adjusts the standard length of the character
C specified by SYMNUM to correct for the different size on the screen and in 
C the PS output file. This is called by GRLEN and is necessary to ensure that
C string justification works correctly.
C-----------------------------------------------------------------------

      REAL GRPSLF
      INTEGER SYMNUM

      GRPSLF = 0.9
C
C case of a-z
C
      IF ( (SYMNUM .GE. 601 .AND. SYMNUM .LE. 626) .OR.
     &     (SYMNUM .GE. 2101 .AND. SYMNUM .LE. 2126) .OR.
     &     (SYMNUM .GE. 2151 .AND. SYMNUM .LE. 2176) .OR.
     &     (SYMNUM .GE. 2651 .AND. SYMNUM .LE. 2676) ) GRPSLF = 0.8
C
C case of A-Z
C
      IF ( (SYMNUM .GE. 501 .AND. SYMNUM .LE. 526) .OR.
     &     (SYMNUM .GE. 2001 .AND. SYMNUM .LE. 2026) .OR.
     &     (SYMNUM .GE. 2051 .AND. SYMNUM .LE. 2076) .OR.
     &     (SYMNUM .GE. 2551 .AND. SYMNUM .LE. 2576) ) GRPSLF = 0.95
C
C case of dot
C
      IF ( SYMNUM .EQ. 710 .OR. SYMNUM .EQ. 2210 .OR.
     &     SYMNUM .EQ. 2760 ) GRPSLF = 0.78
C
C case of ( or )
C
      IF ( SYMNUM .EQ. 721 .OR. SYMNUM .EQ. 722 .OR.
     &     SYMNUM .EQ. 2221 .OR. SYMNUM .EQ. 2222 .OR.
     &     SYMNUM .EQ. 2771 .OR. SYMNUM .EQ. 2772 ) GRPSLF = 0.67
C
C case of minus sign (dash)
C
      IF ( SYMNUM .EQ. 724 .OR. SYMNUM .EQ. 2231 .OR.
     &     SYMNUM .EQ. 2774 ) GRPSLF = 0.62
C
C case of space
C
      IF ( SYMNUM .EQ. 699 .OR. SYMNUM .EQ. 2199 ) GRPSLF = 0.48
C
C case of 0 to 9
C
      IF ( (SYMNUM .GE. 700 .AND. SYMNUM .LE. 709) .OR.
     &     (SYMNUM .GE. 2200 .AND. SYMNUM .LE. 2209) .OR. 
     &     (SYMNUM .GE. 2750 .AND. SYMNUM .LE. 2759) ) GRPSLF = 0.77
C
C The 1.3 factor is included in the PS driver to better match the text size
C to the screen output
C
      GRPSLF = GRPSLF * 1.3

      END

