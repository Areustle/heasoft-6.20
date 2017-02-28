*+CSHFDMY
	subroutine cshfdmy(shf, datstr, timstr, status)

	IMPLICIT NONE
	integer shf, status
	character*(*) datstr, timstr
c
c Description
c  Converts SHF key to calendar dd/mm/yy & hh:mm:ss representation
c
c Passed Parmeters
c  SHF        i  : SHF key
c  DATSTR       o: O/p date string in dd/mm/yy format
c  TIMSTR       o: O/p time string in hh:mm:ss format
c  STATUS       o: Error flag (zero = OK)
c
c Authors/Modification History
c  Andy Pollock (3 october 1988), original
c  Nick        (6 march 1990), modified to make it do 1999 & print yr-mon-day
c  Anon        (15 April 1992), get the leap year right
c  Ian M George (1993 Jun 21), nicked for callib & o/p format changed
c  Ian M George (1996 Dec 04), fixed shf0 declaration to be standard fortran
c  Ning Gan (1998 July 20) extended the shf0 data to 2007 as the shf_ymd in
c                          xanlib. Y2k compliance. Use fttm2s and ftdt2s
c                          to construct the date string. 
*-

c local variables :
      INTEGER dt, year, day, hh, mm 
      double precision ss
C      character(8) string

c local data :
C    	integer shf0(1968:2000)
C	data shf0/ -378691200 , -347068800 , -315532800 ,
C     &     -283996800, -252460800, -220838400, -189302400,
C     &     -157766400, -126230400, -94608000, -63072000, -31536000,
C     &     0, 31622400, 63158400, 94694400, 126230400, 157852800,
C     &     189388800, 220924800, 252460800, 284083200, 315619200,
C     &     347155200, 378691200, 410313600, 441849600, 473385600,
C     &     504921600, 536544000, 568080000, 599616000, 631152000/
* local data :
      integer shf0(1968:2007)
      data shf0
     &          /-378691200,-347068800,-315532800,-283996800,
     &           -252460800,-220838400,-189302400,-157766400,
     &           -126230400, -94608000, -63072000, -31536000,
     &                    0,  31622400,  63158400,  94694400,
     &            126230400, 157852800, 189388800, 220924800,
     &            252460800, 284083200, 315619200, 347155200,
     &            378691200, 410313600, 441849600, 473385600,
     &            504921600, 536544000, 568080000, 599616000,
     &            631152000, 662774400, 694310400, 725846400,
     &            757382400, 789004800, 820540800, 852076800/

c
c      character(2) month(12)
      INTEGER*4 dayno_month(12), id, date
      DATA dayno_month/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
c The DATE
      year = 1968
      DO WHILE ((shf.GT.shf0(year+1)) .AND. (year.LT.2007))
        year = year + 1
      END DO
      dt = shf - shf0(year)
      day = dt/86400
      dt = dt - 86400*day
      day = day + 1
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
c      WRITE (string(1:2),'(i2.2)') date
c      string(3:3) = '/'
c      WRITE (string(4:5),'(i2.2)') id
c      string(6:6) = '/'
c      WRITE (string(7:8),'(i2)') year - 1900
c      datstr = string
      status = 0
      call ftdt2s(year,id,date,datstr,status)

c TIME of day
	ss = MOD(dt,60)
	dt = dt/60
	mm = MOD(dt,60)
	dt = dt/60
	hh = MOD(dt,24)

C      WRITE (string(1:2),'(i2.2)')hh
C      string(3:3) = ':'
C      WRITE (string(4:5),'(i2.2)') mm
C      string(6:6) = ':'
C      WRITE (string(7:8),'(i2.2)') ss
C      timstr = string
      status = 0
      call fttm2s(0,0,0,hh,mm,ss,0,timstr,status)
      return 
      end
