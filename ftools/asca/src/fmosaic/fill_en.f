c=======================================================================
      subroutine fill_en(rmfname,nevl)
c-----------------------------------------------------------------------
c     Read the RMF file for energy definitions and 
c     fill energy columns 
c-----------------------------------------------------------------------
      implicit none
      include 'common_channels.inc'
      include 'common_evts.inc'
      include 'common_defpixel.inc'

      integer nchan,nevl
      real scale
      character*(*) rmfname
      integer k,ch

c---- for fitsio lib
      integer iunit,status
      integer rwmode,block,hdutype,len_trim
c      logical exact,anyf
c      real*4 enull
      character comment*80,contxt*80

c==== open RMF file for energy equivalence
      rwmode=0
      iunit=22
      status=0
      call ftopen(iunit,rmfname,rwmode,block,status) 
c==== move to boundaries extension
      call ftmahd(iunit,3,hdutype,status)

C   Compare the rmf number of channels with the one in the file considered and 
C    rebin the file if necessary

c==== number of channels in rmf
      call ftgkyj(iunit,'NAXIS2',nchan,comment,status)
C     Compare with phabins saved from events file.
      scale=float(nchan/phabins)
      call ftclos(iunit,status)
      contxt = 'Error in getting rmf file number of channels.'
      if(scale.ne.1.) then
       print*,'RMF ', rmfname(:len_trim(rmfname)),
     & ' have',nchan ,' channels'
       print*,'Event file has', phabins ,'. We stop here.'
      stop 
      endif
c---------- Get boundaries with the RMF bin
      k = instr_id
      call gchan_eb(rmfname,ch2e1(0,k),ch2e2(0,k),nchan)

c==== Loop over events
      do k=1,nevl
c------- Fill energy values
            ch=pi(k)
            ene1(k)=ch2e1(ch,instr_id)
            ene2(k)=ch2e2(ch,instr_id)
      end do 

      return
      end





