c
      SUBROUTINE xrshfcon(tim, yr, day, hr, mn, sec)
c
c  Written by RS (?) in late 1986 (?)
c  ls1 -- modified by LS to work until 1995 --  20/6/88
c  ls2       "      "  "  "   "  before 1980 --  25/7/89
c
c  This routine converts a Short History File time, shf, to
c  a year, day, hour, minute and second.  Tim is the SHF key
c  after the words have been swapped.
c
      INTEGER yr, day, hr, mn, sec, tim
      INTEGER*4 time,i,iy
c
c  Yrs stores the offsets for the beginnings of each year between 1968 and
c  2007(ls2)
c
c ls2
      INTEGER*4 yrs(40)/-378691200,-347068800,-315532800,-283996800,
     &                  -252460800,-220838400,-189302400,-157766400,
     &                  -126230400, -94608000, -63072000, -31536000, 
     &                           0,  31622400,  63158400,  94694400, 
     &	                 126230400, 157852800, 189388800, 220924800,
     &                   252460800, 284083200, 315619200, 347155200,
     &                   378691200, 410313600, 441849600, 473385600,
     &                   504921600, 536544000, 568080000, 599616000,
     &                   631152000, 662774400, 694310400, 725846400,
     &                   757382400, 789004800, 820540800, 852076800/
c
c     &                   473385600, 505008000, 536544000, 568080000,
c     &                   599616000, 631152000, 662688000, 694224000,
c     &                   725760000, 757382400, 788918400, 820454400/
c
c
c
      time = tim
c
      DO i = 1, 40
         IF (yrs(i).GT.time) THEN
            GOTO 10
         ENDIF
      ENDDO
      i = 41
 10   CONTINUE
      iy = i - 1
      yr = 1968 + iy - 1
c
      time = time - yrs(iy)
      day = time/86400
      time = time - 86400*day
      day = day + 1
c
      hr = time/3600
      time = time - 3600*hr
c
      mn = time/60
      time = time - 60*mn
c
      sec = time
c
      END
c
c
