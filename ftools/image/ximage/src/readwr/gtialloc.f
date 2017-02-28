      subroutine gtialloc(mode, num, istart, istop, status)
c
c  Allocates memory for two double arrays, representing
c  GTI start and stop times
c
c  I  mode   (i)  1=Allocate, 0=Deallocate
c  I  num    (i)  Number of GTIs
c  O  istart (i)  Memory offset for allocated start time array
c  O  istop  (i)  Memory offset for allocated stop time array
c  O  status (i)  Error flag (0=OK)
c
      integer mode, num, istart, istop, status
c
c  Local variables
c
      character(100) ds

      if ( mode.eq.1 ) then
         write(ds,'(a,i8,a)') 'Allocating ',num,' GTIs'
         call RMVXBK(ds)
         call XWRITE (ds,20)
         if ( num.gt.0 ) then
            istart = -1
            istop = -1
            call udmget(num, 7, istart, status)
            call udmget(num, 7, istop, status)
         endif
         if ( status.ne.0 ) then
            write(ds,'(a,i8)') 
     &                'Could not allocate GTI arrays of size ', num
            call RMVXBK(ds)
            call XAERROR(ds,5)
         endif
      else if ( mode.eq.0 ) then
         if ( num.gt.0 ) then
            write(ds,'(a,i8,a)') 'Deallocating ',num,' GTIs'
            call RMVXBK(ds)
            call XWRITE (ds,20)
            call udmfre(istart, 7, status)
            call udmfre(istop, 7, status)
         endif
         if ( status.ne.0 ) then
            call XAERROR('Could not free GTI arrays',5)
         endif
      endif

      return
      end
