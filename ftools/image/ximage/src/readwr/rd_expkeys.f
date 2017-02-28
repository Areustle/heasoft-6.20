      subroutine rd_expkeys(Lun, Exposure, Dtcor, Vignapp, Ierr)

      implicit none
c
c Read exposure and deadtime correction keywords
c
c  I  Lun      (i)  Logical unit of open FITS file
c  O  Exposure (d) 
c  O  Dtcor    (d)  Deadtime correction
c I/O Vignapp  (l)  Whether vignetting has been applied (exposure map)
c  O  Ierr     (i)  Error flag  (0=OK)
c
      integer Lun, Ierr
      real*8 Exposure, Dtcor
      logical Vignapp

      include '../include/io.inc'
c
c  Local variables
c
      integer status, LENACT
      character(80) comment
      integer i, expnum, deadnum
      logical deadapp, corrected
      character(10) ekeyused

      parameter ( expnum = 9 )
      character(10) expkeys(expnum)
      logical corexp(expnum)
      data expkeys /'EXPOSURE','LIVTIME','ONTIME','TELAPSE','XS-ONTI',
     &              'TIME_LIV','XS-LIVTI','TIME_NET','EXP_TIME'/
      data corexp  /.TRUE.,.TRUE.,.FALSE.,.FALSE.,.FALSE.,
     &              .FALSE.,.FALSE.,.FALSE.,.FALSE./

      parameter ( deadnum = 4 )
      character(10) deadkeys(deadnum)
      data deadkeys /'DEADC','DTCOR','XS-DTCOR','DEADTIME'/

      Ierr = 0
      status = 0
      Exposure = 0.
      Dtcor = 1.
c
c Get exposure keyword
c
      status = 0
      i = 1
      status = -1
      do while ( i.le.expnum .and. status.ne.0 ) 
         status = 0
         call ftgkyd(Lun,expkeys(i),Exposure,comment,status)
         i = i + 1
      enddo
      i = i - 1
      if ( status.eq.0 ) then
         corrected = corexp(i)
         ekeyused = expkeys(i)
         write(ZWRite,'(3a)') ' Using ',ekeyused(1:LENACT(ekeyused)),
     &                    ' keyword for exposure'
         call XWRITE(ZWRite,15)
      else
         corrected = .FALSE.
         ekeyused = 'none'
         exposure = 0.
         Ierr = -1
      endif
c
c  Look for DEADAPP
c
      status = 0
      deadapp = .FALSE.
      call ftgkyl(Lun,'DEADAPP',deadapp,comment,status) 

      if ( .not.deadapp .and. .not.corrected ) then
c
c Get dead time correction keyword
c
         i = 1
         status = -1
         do while ( i.le.deadnum .and. status.ne.0 ) 
            status = 0
            call ftgkyd(Lun,deadkeys(i),Dtcor,comment,status)
            i = i + 1
         enddo
         i = i - 1
         if ( status.eq.0 ) then
            write(ZWRite,'(3a)') ' Using ',
     &                       deadkeys(i)(1:LENACT(deadkeys(i))),
     &                       ' keyword for deadtime correction'
            call XWRITE(ZWRite,15)
            write(ZWRite,'(3a)') ' If ', ekeyused(:LENACT(ekeyused)), 
     &               ' should not have a deadtime correction applied,'
            call XWRITE(ZWRite,15)
            call XWRITE('    set CHH/KEY=DTIME/VAL=1', 15)
            if ( Dtcor.gt.1. ) Dtcor = 1./Dtcor
         else
            Ierr = -1
         endif
      endif
      
      write(ZWRite,'(a,f10.3)') ' Deadtime = ',Dtcor
      CALL XWRITE(ZWRite,15)
c
c Get VIGNAPP
c  if not found, leave Vignapp unchanged
c
      status = 0
      call ftgkyl(Lun,'VIGNAPP',Vignapp,comment,status) 

      return
      end
