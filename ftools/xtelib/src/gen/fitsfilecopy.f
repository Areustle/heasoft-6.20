c**********************************************************************
c
c
c
c
c
c**********************************************************************
      subroutine fits_file_copy(iunit1,extinfile1,
     &   ounit,extoufile,abort,status)

      implicit none
      integer extinfile1,extoufile,iunit1,ounit,status
      integer xtend,ikeywords,i
      logical abort
      character(70) cval

      abort=.FALSE.
      status=0

c     Okay copy the original data file from the INPUT file to
c the output file, up to the point of the data extension that is
c being processed!

      ikeywords=2

      if (extinfile1 .eq. 1) then
        do 100 i = 1, extinfile1+1
          if(i.ne.1)then
            if(i.ge.2.and.status.eq.0)then
              call ftpcks(ounit,status)
              if(status.ne.0)status=0
            endif
            call ftcrhd(ounit, status)
          endif
          call ftmahd(iunit1, i, xtend, status)
          if(i.eq.extinfile1+1)then
            call ftcopy (iunit1, ounit, ikeywords, status)
          else
            call ftcopy (iunit1, ounit, 0, status)
          endif
          
100     continue
        if (status .ne. 0) then
          call fcerr ('ERROR copying extensions')
          abort=.TRUE.
          goto 999
        endif
      else
        do 101 i = 1, extinfile1
          if(i.ne.1)then
            if(i.ge.2.and.status.eq.0)then
              call ftpcks(ounit,status)
              if(status.ne.0)status=0
            endif
            call ftcrhd (ounit, status)
          endif
          call ftmahd (iunit1, i, xtend, status)
          if(i.eq.extinfile1+1)then
            call ftcopy (iunit1, ounit, ikeywords, status)
          else
            call ftcopy (iunit1, ounit, 0, status)
          endif
101     continue
        if (status .ne. 0) then
          call fcerr ('ERROR copying extensions')
          abort=.TRUE.
          goto 999
        endif
      endif

c     Since the output file is identical to the input file except for the
c addition of the barycentric correction, we set the outputfile extension
c equal to the input file extension.
      extoufile=extinfile1
      
c      Move to the second CHDU to the second (or extnum)
c      to read all pertinent processing information.
      call ftmahd(iunit1,extinfile1+1,xtend,status)
      if(status.ne.0)then
        call fcerr('Error moving to extinfile1')
        call fcerrm(status)
        status=0
      endif

c      Move to the second CHDU to the second (or extnum)
c      to read all pertinent processing information.
      call ftmahd(ounit,extoufile+1,xtend,status)
      if(status.ne.0)then
        call fcerr('Error moving to extoufile')
        call fcerrm(status)
        status=0
      endif

      cval='This file created by BEMERGE_3.6.'
      call ftphis(ounit,cval,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error updating HISTORY keyword')
        status=0
      endif
      
      cval='FILEN# shows ALL files processed to create PHA files.'
      call ftpcom(ounit,cval,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error updating COMMENT keyword')
        status=0
      endif

999   continue
      
      return
      end

