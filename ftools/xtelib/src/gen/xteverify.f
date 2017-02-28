c     This subroutine is used to check a FITS file to see if it
c has CHECKSUM and DATASUM keywords defined and if they agree with what is
c actually there. Note that this subroutine only returns and error if it
c cannot find the proper keywords inserted in this file. 
      subroutine xteverify(iunit,status)

      integer iunit,status
      integer idataok, ihduok
      idataok=0
      ihduok=0
      status=0
      
      call ftvcks(iunit,idataok,ihduok,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Unable to verify CHKSUM and DATASUM')
        call fcecho('keywords. Is this an XTE data file?')
        call fcecho('If not, there is no problem,')
        call fcecho('if so your file MAY be corrupt.')
      else
        if(idataok.eq.0.or.ihduok.eq.0)then
          call fcecho(' ')
          call fcecho('CHECKSUM/DATASUM keywords not present.')
        endif
        if(idataok.eq.1.and.ihduok.eq.1)then
          call fcecho(' ')
          call fcecho('CHECKSUM/DATASUM verification correct.')
        endif
        if(idataok.eq.-1)then
          call fcecho(' ')
          call fcecho('Verification code for DATA incorrect.')
          call fcecho('This MAY mean that this file is corrupt.')
          call fcecho('If this run fails, check your data')
          call fcecho('using FVERIFY, FSTATISTIC, and FCHECKSUM.')
        endif
        if(ihduok.eq.-1)then
          call fcecho(' ')
          call fcecho('Verification code for this HDU incorrect.')
          call fcecho('This MAY mean that this file is corrupt.')
          call fcecho('If this run fails, check your data')
          call fcecho('using FVERIFY, FSTATISTIC, and FCHECKSUM.')
        endif
      endif
            
      return
      end
      
