*- xequinox.for - decode celestial coordinate system specification
      subroutine xequinox(equinox,system,epoch,status)
* Description :
*  Decode a string like 'J1981', for example, into system='fk5' epoch=1981d0
* Author :
*  Andy Pollock (HEAGIP::POLLOCK)
*  Computer & Scientific Co. Ltd., 34 Westwood Road, Sheffield S11 7EY, England.
* History :
*  27 January 1993 : original
*  14 April 1993 : take care of systemless strings like '1976'

      include 'status.codes'

* Import :
      character*(*) equinox
* Export :
      character*(*) system
      real*8 epoch
* Status :
      integer*4 status
* Local constants :
      character(3) default_system
      parameter (default_system='fk4')
      real*8 default_epoch
      parameter (default_epoch=1950d0)
* Local variable :
      character(20) q
* External reference :
      logical*4 isdigit
*-
      if(status.ne.ok__)return

      q=equinox
      call upc(q)
      if((q.eq.' ').or.(q(1:1).eq.char(0)))then
         system=default_system
         epoch=default_epoch
      else if(q(1:1).eq.'B')then
         system='fk4'
         read(q(2:),*,iostat=status)epoch
      else if(q(1:1).eq.'J')then
         system='fk5'
         read(q(2:),*,iostat=status)epoch
      else if(isdigit(q(1:1)))then
         system=default_system
         read(q,*,iostat=status)epoch
      else
         status=warning__
      endif

      if(status.ne.ok__)then
         system=default_system
         epoch=default_epoch
      endif

      return

      end
