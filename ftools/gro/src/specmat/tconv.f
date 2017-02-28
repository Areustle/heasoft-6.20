*Converts time variable character strings to a real number.  Assumes
*local variables are saved between calls.
*This version uses dates in mm/dd/yy format.

*  @(#) tconv.f 1.3@(#)

      subroutine tconv(chdate,chtime,time)
            
      IMPLICIT NONE

*     ARGUMENTS
      character(8) chdate
      character(12) chtime
      real*8 time

*     LOCAL VARIABLES
      integer ierr,itjd,imsd,month,day,year,hour,minute,second,fractn
      integer fullyear
      real*8 sconvrt

      save

*---------------------------------------------------------------------*

      ierr = 0
      fullyear=0
      if (chdate.ne.'        ') then
         read (chdate,70,err=100) month,day,year
70       format(2(i2,1x),i2)
      end if
      if (chtime.ne.'            ') then
         read (chtime,71,err=101) hour,minute,second,fractn
71       format(3(i2,1x),i3)
      end if

* Y2K change - if 2 digit year var is greater than 50 then year less than 2000
* changed made 6/98 - lmm
*     call sgrdtjd(itjd,imsd,year+1900,month,day,hour,minute,
*    >      second,fractn,ierr)

      if (year.ge.50) then
         fullyear=1900+year
      else
         fullyear=2000+year
      end if

      call sgrdtjd(itjd,imsd,fullyear,month,day,hour,minute,
     >      second,fractn,ierr)
* End Y2K Change

      if (ierr.ne.0) then
         write (6,*) 'Error in converting times.'
         STOP
      end if
      time = sconvrt(itjd,imsd)

      return
100   write (6,*) 'Error reading date:',chdate,'.'
      STOP
101   write (6,*) 'Error reading time:',chtime,'.'
      STOP

      end

*Converts time variable character strings to a real number.  Assumes
*local variables are saved between calls.
*This version uses dates in dd/mm/yy format

      subroutine tconv1(chdate,chtime,time)
            
      IMPLICIT NONE

*     ARGUMENTS
      character(8) chdate
      character(12) chtime
      real*8 time

*     LOCAL VARIABLES
      integer ierr,itjd,imsd,month,day,year,hour,minute,second,fractn
      real*8 sconvrt

*---------------------------------------------------------------------*

      ierr = 0
      if (chdate.ne.'        ') then
         read (chdate,70,err=100) day,month,year
70       format(2(i2,1x),i2)
      end if
      if (chtime.ne.'            ') then
         read (chtime,71,err=101) hour,minute,second,fractn
71       format(3(i2,1x),i3)
      end if
      call sgrdtjd(itjd,imsd,year+1900,month,day,hour,minute,
     >      second,fractn,ierr)
      if (ierr.ne.0) then
         write (6,*) 'Error in converting times.'
         STOP
      end if
      time = sconvrt(itjd,imsd)

      return
100   write (6,*) 'Error reading date:',chdate,'.'
      STOP
101   write (6,*) 'Error reading time:',chtime,'.'
      STOP

      end


*********************************************************************
*This is intended to be an imitation of the Goddard subroutine.
*I'm not sure it's exactly accurate, but it should do for testing.
* The tjd should probably be called mjd -- julday seems to drop the
* half day offset.

      subroutine sgrdtjd(itjd,imsd,year,month,day,hh,mm,ss,ff,ierr)

      integer itjd,imsd,ierr,year,month,day,hh,mm,ss,ff

      itjd = julday(month,day,year) - 2440000
      imsd = ff+1000*(ss+60*(mm+60*hh))

      return
      end
                                                                  


*********************************************************************
* Combine MJD and MSD into a floating-point MJD
* PLN, 1/1/93: Used to return elapsed seconds since 1/1/90;
*               no reason for this arbitrary system.

      real*8 function sconvrt(itjd,imsd)

      parameter (mscday = 24*60*60*1000)

      sconvrt = dble(itjd) + dble(imsd)/dble(mscday)

      return
      end


*********************************************************************
*Brazenly copied from Press & Co.

      integer function julday(mm,dd,yy)
 
      integer mm,dd,yy       

      parameter (igreg=15+31*(10+12*1582))

      if (yy.lt.0) yy = yy + 1
      if (mm.gt.2) then
         jy = yy
         jm = mm + 1
      else 
         jy = yy - 1
         jm = mm + 13
      end if
      julday = int(365.25*jy) + int(30.6001*jm) + dd + 1720995
      if (dd+31*(mm+12*yy).ge.igreg) then
         ja = int(0.01*jy)
         julday = julday + 2 - ja + int(0.25*ja)
      end if
      julday = julday - 1   ! Kluge.  Result always seems to be off.
			    ! Probably due to half-day misunderstanding.

      return
      end
