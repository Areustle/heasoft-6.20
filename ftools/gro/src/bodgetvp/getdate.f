      subroutine getdate(date,year,mon,day)
      integer*4 year,mon,day
      character*(*) date

      if (date(1:1) .eq. ' ' .or. date(1:1) .eq. '0') then
         read(date(2:2),1)day  
      else 
         read(date(1:2),1)day
      end if

      if (date(4:4) .eq. ' ' .or. date(4:4) .eq. '0') then
         read(date(5:5),1) mon
      else 
         read(date(4:5),1)mon
      end if
  1   format(I2)   

      read(date(7:8),10)year
 10   format(i4)

      if (date(7:7) .eq. ' ' .or. date(7:7) .eq. '0') then
         year =year + 2000
      else 
         year =year + 1900
      end if

      return
      end
