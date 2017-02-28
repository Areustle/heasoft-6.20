C     Subroutine xrfindstart
C
C     Description:
C     
C        Finds interval start time.
C
C     Arguments:
c     I   iflags = int*4 flags for plots, file type, analysis type
c     I   iendm = array of series flags =1 end of good data, =2 end of 
C         good data and last intv. reject.
c     I   ipf = integer*4 parameter file options
c     I   nwi,twia = windows
c     I   dtnb = duration of a "newbin" in secs
c     I   dtime = current series bin time
c     I   dtint = bin width
c     I   intsta = statistics of this interval (integer*4) (20)
C     I   y = counts of current bin
c     I/O dntsta =     "      "   "      "     (real*8)    (5)
C     I/O m = current series number
C     I   progtype = TIME, FOLD, or FOLDSEARCH analysis type
      subroutine xrfindstart(iflags,iendm,ipf,nwi,twia,dtnb,
     $     dtime,dtint,intsta,y,dntsta,m,progtype)
      implicit none
      character(10) progtype
      integer iflags(*),iendm(*),ipf(*),nwi(*),intsta(20,*),m
      real y(8)
      double precision twia(*),dtnb,dtime(*),dtint(*),
     $     dntsta(20,*)
C     LOCAL
      double precision dv
      
      
      IF (intsta(3,m).EQ.0) THEN
c     condition not for dummy start time in case data have ended
         IF (iendm(m).EQ.0) THEN
c     condition to force 1st interval to start with start of 1st time window
            IF (ipf(1).EQ.1 .AND. intsta(9,m).EQ.1 .AND. nwi(1).GE.1)
     &           THEN
               dntsta(1, m) = twia(1) + dtnb/86400.D0/2.D0
c     set value of first qualified bin time 
               if(progtype(1:4).eq.'FOLD') 
     $              dntsta(3, m) = twia(1)+dtnb/86400.D0/2.D0
            ELSE
c     otherwise interval starts at the first qualified point of all series
c     (this is to calculate the time center of first newbin of series m)
               dntsta(1, m) = dtime(m) + dtnb/86400.D0/2.D0
c     (if not arrival time file shift by bin/2 to the left)
               IF (dtint(m).GT.0.D0) dntsta(1, m) = dntsta(1, m)
     &              - dtint(m)/86400.D0/2.D0
c     if arrival time file shift by newbin/100. to the left,
c     but not if a GTI start time was used.
               IF ((dtint(m).LT.0.D0).AND.(y(m).GE.0.0))
     &              dntsta(1, m) = dntsta(1, m) - dtnb/86400.D0/100.D0
c     set value of first qualified bin time (note difference with xrgetint)
               if(progtype(1:4).eq.'FOLD') 
     $              dntsta(3, m) = dtime(m)
            ENDIF
         ENDIF
c     increase series no.
         m = m + 1
         IF (m.GT.iflags(10)) THEN
            dv = 1.D34
c     determine earliest start
            DO m = 1, iflags(10)
               IF (dv.GT.dntsta(1,m)) dv = dntsta(1, m)
            ENDDO
c     make start for all series equal
            DO m = 1, iflags(10)
               dntsta(1, m) = dv
            ENDDO
c     reset series no.
            m = 1
         ENDIF
      endif
      return
      end
