
      SUBROUTINE xrpltvar (cpro, iflags, drtsta, dpera, dxsta, 
     &                     indx, ito, ita, itu, doo)
      implicit none
c
c
c cpro   I  c  program name 
c iflags I  I  iflags 
c drtsta I  R  *8 real array of double precison variable
c dpera  I  R  real array of periods
c dxsta  I  D  start for the array colum
c indx   O  I  index of drstat variable ofor start and stop 
c ito    O  I  integer array for start time
c ita    O  I  integer array for stop time
c itu    O  I  integer array for units start time
c doo    O  R  unit convertion factor  
c kmax   O  I  index for best period array
c
c input variable
      INTEGER*4 iflags(*) 
      REAL*8  drtsta(20,*), dxsta(*), dpera(*)
      CHARACTER cpro*(*)
c output variable
      INTEGER*4 ita(5), ito(5), itu(5)
      INTEGER*4 indx(2), kmax
      REAL*8 doo 
c local variable
      INTEGER*4 m, j, k  
      REAL*4 chitest 
      REAL*8 dv
      logical first_time
      data first_time /.true./
      save first_time

      doo = 0.
      do j=1,5
         ita(j) = 0
         ito(j) = 0
         itu(j) = 0
      enddo
c 
c
c set start/stop time label 
c find indx(1)=min (indx(2)=max) of min (max) drtsta(3,iflags(10))
c This array will be use to write out header info as well to be carry out
c
      indx(1) = 1
      indx(2) = 1
c
c loop iflags(10) (no. of time series) to get start and stop from the 
c new bin 
      DO k = 1, iflags(10)
         IF (drtsta(3,k).LE.drtsta(3,indx(1))) indx(1) = k
         IF (drtsta(4,k).GE.drtsta(4,indx(2))) indx(2) = k
      ENDDO
c
c
c If the  requested number of interval is higher than 1
c keep in memory into a save variable the first start time of the 
c first qualified  bin
c
c drtsta(3,iflags(10))
c  take the difference between the first start time and the 
c  current start time call offsets . 
c  add the offset to dtrsta(7) (first start time for the first 
c  interval)
c call to xrdhms 
c  after 
c     convert it to d,h,m,s,ms
c     for label
c
      CALL xrdhms(drtsta(3,indx(1)), dv, ita, 1)
      CALL xrdhms(drtsta(4,indx(2)), dv, ito, 1)
c
c set start time for data (xaxis) depending on units)
c this needs to be carry out for FITS output TIMEZERO
c itu(5)=msec itu(4)=sec, itu(3)=min
c itu(2)= hour of day itu(1)=day
c

      if(first_time) drtsta(7,1) = drtsta(1,1)
      first_time = .false.

      if(iflags(2).eq.4) then
         call xrdhms(drtsta(7,1),dv,itu,1)
         doo = drtsta(1, 1) - drtsta(7,1)
         
      else
         CALL xrdhms(drtsta(1,1), dv, itu, 1)
         IF (iflags(2).GE.1) THEN
            itu(5) = 0
            itu(4) = 0
            itu(3) = 0
         ENDIF
         IF (iflags(2).GE.2) itu(2) = 0
         IF (iflags(2).GE.3) itu(1) = 0
c     
c     
c     convert back to days (doo)
c     subtract from time of 1st newb.
c     
         CALL xrdhms(doo, dv, itu, 3)
         doo = drtsta(1, 1) - doo
      endif
c     
c     To do max for chival for efs. 
c
      IF(cpro(1:2).EQ.'es') THEN
         dxsta(1)=dpera(1)        
      ENDIF
      END

