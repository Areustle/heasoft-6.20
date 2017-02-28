**==SHF_YMD.FOR
*- shf_ymd - convert shf key to calendar char*11 representation
      character(11) FUNCTION shf_ymd(shf,status)
* history :
*  3 october 1988 : original
*  6 march 1990 : nick modified to make it do to 1999 and print yr-mon-day
*  15 April 1992 : get the leap year right
*  17 Jun  1998 : modified to write the four digit year.
*  27 Feb  2008 : modified to extend to 2031
* author :
*  andy pollock (exosat::andy)
       
c      include 'status.codes'
* import :
      INTEGER shf
* status :
      INTEGER status
* local variables :
      INTEGER dt, year, day
      character(11) string
* local data :
      integer shf0(1968:2031)
      character(3) month(12)
      INTEGER*4 dayno_month(12), id, date

      data shf0 /-378691200,-347068800,-315532800,-283996800,
     &           -252460800,-220838400,-189302400,-157766400,
     &           -126230400, -94608000, -63072000, -31536000,
     &                    0,  31622400,  63158400,  94694400,
     &            126230400, 157852800, 189388800, 220924800,
     &            252460800, 284083200, 315619200, 347155200,
     &            378691200, 410313600, 441849600, 473385600,
     &            504921600, 536544000, 568080000, 599616000,
     &            631152000, 662774400, 694310400, 725846400,
     &            757382400, 789004800, 820540800, 852076800,
     &            883612800, 915235200, 946771200, 978307200,
     &        1009843200, 1041465600, 1073001600, 1104537600,
     &        1136073600, 1167696000, 1199232000, 1230768000,
     &        1262304000, 1293926400, 1325462400, 1356998400,
     &        1388534400, 1420156800, 1451692800, 1483228800,
     &        1514764800, 1546387200, 1577923200, 1609459200/
*-
      DATA month/'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG',
     &     'SEP', 'OCT', 'NOV', 'DEC'/
      DATA dayno_month/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
 
      year = 1968
      DO WHILE ((shf.GE.shf0(year+1)) .AND. (year.LT.2031))
        year = year + 1
      END DO
      WRITE (string(8:11),'(i4)') year 
      dt = shf - shf0(year)
      day = dt/86400
      dt = dt - 86400*day
      day = day + 1
      string(3:3) = '-'
c
      dayno_month(2) = 28
      IF ( mod(year,4).EQ.0 )then
         dayno_month(2) = 29
      else
         dayno_month(2) = 28
      endif
      date = day
      id = 1
      DO WHILE (id.LE.12 .AND. date.GT.dayno_month(id))
        date = date - dayno_month(id)
        id = id + 1
      END DO
c
      WRITE (string(4:6),'(a3)') month(id)
      string(7:7) = '-'
      WRITE (string(1:2),'(i2.2)') date
      shf_ymd = string
 
      END
