*- year - decimal year for shf key
      real*8 function shfdyr(shf,status)
* History :
*  2 February 1989 : original
* 19 June 1991: modified to go to 1976
*               name of routine changed from obs_year
* 14 Oct 1992: changed to duplicate obsdyr
* 10 Jul 1998: Modified to go from 1968 to 2007
* 27 Feb 2008: Modified to go from 1968 to 2031
* Author :
*  Andy Pollock (EXOSAT::ANDY)

* Import :
      integer shf                          
* Status :
      integer status
* Local data :
      integer shf0(1968:2031)               
* Local variables :
      integer year
c shf keys at 00:00 01-JAN
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
      if((shf.lt.shf0(1968)).or.(shf.gt.shf0(2031)))then
         shfdyr = 0.0
         status=1
         write(*,*)' shfdyr error: outside 1968-2031 shf key range'
      else
         year=1968
         do while(shf.gt.shf0(year+1))
            year=year+1
         end do
         shfdyr=dble(year)+dble(shf-shf0(year))
     *                      /dble(shf0(year+1)-shf0(year))
      endif

      end
