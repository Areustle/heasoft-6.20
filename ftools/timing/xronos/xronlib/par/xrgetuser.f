C***************************************************************
C
C subroutine:  xrgetuser.f
C              get resolution for epoch folding searches
C
C Description:
C  Gets parameters named [ibdrs]usern for up to n=maxuserp.
C  Places the values in array elements [ibdrs]user(n).
C
C written by:
C      Lawrence E Brown
C      HEASARC/GSFC/NASA  Hughes STX
C      6/22/95
C
C modification history:
C
C notes:
C
C calling sequence:
C      call xrgetuser(iuser,buser,duser,ruser,suser,status)
C
C variables:
c     O   iuser - array of user defined integer parameters
c     O   buser - array of user defined boolean parameters
c     O   duser - array of user defined double precision parameters
c     O   ruser - array of user defined real parameters
c     O   suser - array of user defined string parameters
c     O   status
C
C***************************************************************
      subroutine xrgetuser(iuser,buser,duser,ruser,suser,status)
      implicit none
      include '../include/xronos.inc'
      integer i
      character(9) iuserp,buserp,duserp,ruserp,suserp
      include '../include/xronos_init.inc'
      
      if(status.ne.0) return

      do i=1,maxuserp
         call ftkeyn('iuser',i,iuserp,status)
         call uclgsi(iuserp,iuser(i),status)
         if (status.ne.0) goto 10
      enddo
 10   status = 0
      do i=1,maxuserp
         call ftkeyn('buser',i,buserp,status)
         call uclgsb(buserp,buser(i),status)
         if (status.ne.0) goto 20
      enddo
 20   status = 0
      do i=1,maxuserp
         call ftkeyn('duser',i,duserp,status)
         call uclgsd(duserp,duser(i),status)
         if (status.ne.0) goto 30
      enddo
 30   status = 0
      do i=1,maxuserp
         call ftkeyn('ruser',i,ruserp,status)
         call uclgsr(ruserp,ruser(i),status)
         if (status.ne.0) goto 40
      enddo
 40   status = 0
      do i=1,maxuserp
         call ftkeyn('suser',i,suserp,status)
         call uclgst(suserp,suser(i),status)
         if (status.ne.0) goto 50
      enddo
 50   status = 0
      return
      end

