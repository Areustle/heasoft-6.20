C Program CHECK.FOR
      INTEGER   MXFIL
      PARAMETER (MXFIL=150)
      INTEGER   LENACT
C
      CHARACTER CBUF*132, CTOK*50, CDIR*50
      CHARACTER CVFIL(MXFIL)*50
      CHARACTER CSFIL(MXFIL)*50
      CHARACTER CYEAR*2, CLAS*2
      INTEGER   I, ICMON, IER, IMONTH, IYEAR, J, JLO
      INTEGER   KP, LBUF, LDIR, LTOK, LVFIL, NSUN, NVAX
C---
   11 FORMAT(A)
C---
      CALL GTBUF(' ', IER)
      CALL GTCHAR(CDIR, LDIR)
C---
C Under SUN UNIX if a file is less than 6 months old (?) then
C the time-of-day is provided, otherwise the year is provided.
C We need the current date to sort this mess out.
      CALL GETDAT(CTOK)
      CALL UPC(CTOK)
      CALL LMONTH(CTOK(4:6),CBUF,ICMON)
      CYEAR=CTOK(8:9)
      READ(CYEAR,101) IYEAR
      WRITE(CLAS,101) IYEAR-1
  101 FORMAT(I2)
C---
C Read the VAX directory listing.
      OPEN(UNIT=1,FILE='/tmp/vaxdir.lis',STATUS='OLD',ERR=100)
      NVAX=0
C---
  100 READ(1,11,ERR=190,END=190) CBUF
      NVAX=NVAX+1
      LBUF=LENACT(CBUF)
      KP=0
      CALL ALF(CBUF,LBUF,KP,CTOK,LTOK)
      CVFIL(NVAX)=' '
      CVFIL(NVAX)(1:2)=CTOK(8:9)
      CALL LMONTH(CTOK(4:6),CVFIL(NVAX),IMONTH)
      CVFIL(NVAX)(5:6)=CTOK(1:2)
      CALL ALF(CBUF,LBUF,KP,CTOK,LTOK)
      CVFIL(NVAX)(7:8) =CTOK(1:2)
      CVFIL(NVAX)(9:10)=CTOK(4:5)
      CALL ALF(CBUF,LBUF,KP,CTOK,LTOK)
      CVFIL(NVAX)(12:)=CTOK(:LTOK)
      IF(NVAX.LT.MXFIL) GOTO 100
      WRITE(*,*) 'FCHECK--Could not read entire VAX directory.'
C---
C Now read the SUN directory listing.
  190 CLOSE(UNIT=1)
      OPEN(UNIT=1,FILE='/tmp/sundir.lis',STATUS='OLD',ERR=100)
      NSUN=0
C---
  200 READ(1,11,ERR=290,END=290) CBUF
      LBUF=LENACT(CBUF)
      IF(LBUF.GT.0) THEN
         NSUN=NSUN+1
         KP=0
         CALL ALF(CBUF,LBUF,KP,CTOK,LTOK)
         CSFIL(NSUN)=' '
         CALL LMONTH(CTOK(:3),CSFIL(NSUN),IMONTH)
         CALL ALF(CBUF,LBUF,KP,CTOK,LTOK)
         IF(LTOK.EQ.1) THEN
            CSFIL(NSUN)(5:6) ='0'//CTOK(1:1)
         ELSE
            CSFIL(NSUN)(5:6) =CTOK(1:2)
         END IF
         CALL ALF(CBUF,LBUF,KP,CTOK,LTOK)
         IF(CTOK(3:3).EQ.':') THEN
            IF(IMONTH.LE.ICMON) THEN
C- This year
               CSFIL(NSUN)( 1:2)=CYEAR
            ELSE
C- Last year
               CSFIL(NSUN)( 1:2)=CLAS
            END IF
            CSFIL(NSUN)(7:10)=CTOK(1:2)//CTOK(4:5)
         ELSE
            CSFIL(NSUN)( 1:2)=CTOK(3:4)
            CSFIL(NSUN)(7:10)='0000'
         END IF
         CALL ALF(CBUF,LBUF,KP,CTOK,LTOK)
         CSFIL(NSUN)(12:)=CTOK(:LTOK)
      END IF
      IF(NSUN.LT.MXFIL) GOTO 200
      WRITE(*,*) 'FCHECK--Could not read entire SUN directory.'
C---
  290 CLOSE(UNIT=1)
C---
C- For each file on the VAX, make sure there is a newer version on
C- the Sun.
  300 CALL OPENWR(2,'/tmp/new.lis','NEW',' ','L',0,0,IER)
      JLO=1
      DO 380 I=1,NVAX
         LVFIL=LENACT(CVFIL(I))
         DO 370 J=JLO,NSUN
            IF(CVFIL(I)(12:).EQ.CSFIL(J)(12:)) THEN
               JLO=J+1
               IF(CSFIL(J)(1:10).LT.CVFIL(I)(1:10)) THEN
C- This file is older on the Sun, list it to be copied
                  WRITE(2,*) CDIR(:LDIR),CVFIL(I)(12:LVFIL)
               ELSE
C                  LSFIL=LENACT(CSFIL(J))
C                  WRITE(*,*) CSFIL(J)(:LSFIL)
C                  WRITE(*,*) CVFIL(I)(:LVFIL)
               END IF
               GOTO 380
            END IF
  370    CONTINUE
C- This file does not exist on the Sun, list it to be copied
         WRITE(2,*) CDIR(:LDIR),CVFIL(I)(12:LVFIL)
  380 CONTINUE
C---
      END
C*********
      SUBROUTINE LMONTH(CTOK, CBUF, IMONTH)
      CHARACTER CTOK*3, CBUF*(*)
      INTEGER   I, IMONTH
C
      CHARACTER CMONTH(12)*3
      CHARACTER CNUM(12)*2
      DATA CMONTH/'JAN','FEB','MAR','APR','MAY','JUN',
     :            'JUL','AUG','SEP','OCT','NOV','DEC'/
      DATA CNUM  / '01', '02', '03', '04', '05', '06',
     :             '07', '08', '09', '10', '11', '12'/
C---
      DO 170 I=1,12
         IF(CTOK(1:3).EQ.CMONTH(I)(1:3)) THEN
            CBUF(3:4)=CNUM(I)
            IMONTH=I
            RETURN
         END IF
  170 CONTINUE
      WRITE(*,*) 'LMONTH--Error, CTOK=',CTOK
      STOP
      END
