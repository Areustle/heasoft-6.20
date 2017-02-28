      subroutine def_nevents(unit,evfname,nevf,nevtot,status)
c=======================================================================
c     This procedure returns the number of events 
c     to be read in from given set of files 
C   
c     Created: Fri Jan  6 20:49:10 MSK 1995
c     Modified: Thu Sep 16 15:16:38 EDT 1999 (Ilana Harrus)
c=======================================================================
c---- list of event files and directory with event files
c     it is assumed, that complete filename is given


      implicit none
      integer nevfm
      parameter(nevfm=50)
      character(200) evfname(nevfm,4)
      integer nevf
      integer unit,status,nevtot

      integer k,nev1,len_trim
      character(200) evf_name

c---- for fitsio lib
      integer rwmode,block,hdutype
      character errtext*30, comment*80

c==== Number of events in data files
      status=0
      nevtot=0
      do k=1,nevf
         evf_name=evfname(k,1)
c------- open fits file
         rwmode=0
         call ftopen(unit, evf_name, rwmode, block, status)
c------- move to EVENTS extension
         call ftmahd(unit,2,hdutype,status)
c------- number of events in list
         nev1=0
         call ftgkyj(unit,'NAXIS2',nev1,comment,status)
c------- close file
         call ftclos(unit,status)
c------- some info
         print*, 'Events file:',evf_name(:len_trim(evf_name)),
     &   ' contains:',nev1,' events'
         if (status.ne.0) then
            print*,'I/O error in def_nevents'
            call ftgerr(status,errtext)
            print*,' FITSIO context: ',errtext 
            stop
         endif
         nevtot=nevtot+nev1
      end do

      return
      end



