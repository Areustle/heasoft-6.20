c======================================================================


c**********************************************************************

      subroutine wrtlcgti(ounit,realbin,timemin,timemax,
     &   tfirst,binsz,totsecs,doffset,icount,dpernoarray,
     &   icycles,lwritegti,gstrt,gstp,isav,
     &   dpernosave,labort)
      implicit none
      
      integer ngti,nc,ichan
      parameter (nc = 5)
       
      character(80) gtistart,gtistop
      double precision timemin,timemax,totsecs,binsz,
     &   timearay,gstrt(*),gstp(*),
     &   mjdref,realbin(*), dtruc,
     &   tfirst
      real equino,ra,dec
      
      integer ounit,status,icount,kval,
     &   irow,itest,isav,imjdref

      double precision dpernoarray(*),doffset(2,*)
      
      character(80) instrume,
     &     radecsys,object,dateobs,timeobs,dateend,timeend      

      character(40) ttypes(nc),tforms(nc),tunits(nc),
     &     extnam,timesys,timeunit

      integer i,j,k,pcount,gcount,nfield,bitpix,
     &   naxis,istrt,istp,
     &   icycles,itimemin,itimemax
      double precision dpernosave,dtestpernoarray,
     &   timeminf, timemaxf, timezerof
      logical lor,land,lwritegti,labort,liter

      character(40) taskname        
      common /task/ taskname

      common/gtinfo/mjdref,imjdref,ra,dec,equino,dateobs,timeobs
     &     ,dateend,timeend,
     &     instrume,object,radecsys,timesys,timeunit
      common/gtistuff/gtistart,gtistop
      common/gtiinfo/ichan,ngti
      
c      print*,'We are gtiwrite',ounit
      
c      Define the information about the number of columns and the
c      like. This information will have to change at a later date as
c      the options for calculating errors and other information are
c      included. Actually the above will have to be expanded
c      as well... 

      if(labort)return
      if(icount.eq.0.and.(.not.lwritegti))return
c      lfirst=.FALSE.
c      lsecond=.FALSE.
      lor=.FALSE.
      land=.FALSE.
      bitpix = 8
      naxis = 0
      pcount = 0
      gcount = 1
      istrt = 1
      istp = 0
      status=0
      extnam='STDGTI'
      dtestpernoarray=0.0d0
      timezerof=0.0d0
      timeminf=0.0d0
      timemaxf=0.0d0
      itimemin=0
      itimemax=0
      
      nfield=2

      kval=0
      
      ttypes(1)=gtistart
      tforms(1)='1D'
      tunits(1)='s'
      
      ttypes(2)=gtistop
      tforms(2)='1D'
      tunits(2)='s'

c**********************************************************************
c      Let's find out some information that we must know in order to
c      write out the GTI information that is pertinent to this file.

      liter=.FALSE.
      
      if(icycles.eq.1)then
        istrt=1
        irow=0
        itest=0
      else
        if(isav.lt.1)then
          isav=1
          liter=.TRUE.
        endif
        
        istrt=isav
        irow=0
        itest=0
      endif

c      print*,'icount is',icount,' tfirst is ',tfirst

      if(icount.le.0)goto 12
      
      do 10 k=1,icount

        itest=itest+1

        if(k.gt.1)dtestpernoarray=dtruc(dpernoarray(itest-1))

        if(k.eq.1.and.(icycles.ne.1.and.(.not.liter)))
     &     dtestpernoarray=dtruc(dpernosave)
        if(k.eq.1.and.(icycles.eq.1.or.liter))
     &     dtestpernoarray=dtruc(dpernoarray(itest)-1.0d0)

        kval=itest        
        dpernosave=dtruc(dpernoarray(kval)-1.0d0)
        dpernoarray(kval)=dtruc(dpernoarray(kval))
        
c        Print*,'timearay is ',timearay,kval,dpernosave,
c     &     dpernoarray(kval),dtestpernoarray

        if((dpernoarray(kval)).eq.(dtruc(dtestpernoarray+1.0d0)))then
c          print*,'Up transition two are equal',istrt,
c     &       dpernoarray(kval),dtruc(dtestpernoarray+1.0d0)
          if(gstrt(istrt).eq.0.0d0)then
c            print*,'Up transition eq 0.0d0',istrt,kval,itest,
c     $         dtestpernoarray,
c     &         dpernoarray(kval),dtruc(dtestpernoarray+1.0d0)
            
            timearay=tfirst+((dpernoarray(kval)-1.0d0)*binsz)
            gstrt(istrt)=timearay
c            print*,'GSTART1 istrt is',gstrt(istrt),istrt,kval,itest,
c     &         dpernoarray(kval),dtestpernoarray
          endif
        else
          if(dtestpernoarray.gt.0.0d0)then
c            print*,'Down transition not equal',kval,itest,
c     &         dtestpernoarray,dpernoarray(kval)
            timearay=tfirst+(((dtestpernoarray)+0.5d0)*binsz)
            timearay=tfirst+(((dtestpernoarray))*binsz)
            gstp(istrt)=timearay
c            print*,'GSTRT2, GSTP istrt is ',gstrt(istrt),gstp(istrt),
c     &         istrt,dtestpernoarray,kval
            istrt=istrt+1
            if(dpernoarray(kval).ne.0)then
              timearay=tfirst+(((dpernoarray(kval))-0.5d0)*binsz)
              timearay=tfirst+(((dpernoarray(kval))-1.0d0)*binsz)
              gstrt(istrt)=timearay
c              print*,'GSTART3 istrt is',gstrt(istrt),istrt,
c     &           dpernoarray(kval),timearay,dtestpernoarray
            else
c              print*,'Could not set next value',dtestpernoarray
            endif
            
          endif
        endif
        
11      continue
10    continue
      
12    continue
      
      if(lwritegti.and.icount.ne.0)then
c        print*,'Down transition and lwritegti is true'
        timearay=tfirst+(((dpernoarray(kval))+0.5d0)*binsz)
        timearay=tfirst+(((dpernoarray(kval)))*binsz)        
c        print*,'4 Set gstp for istrt',istrt,timearay,
c     &     dpernoarray(kval)
        gstp(istrt)=timearay
      elseif(lwritegti.and.gstp(isav).eq.0.0d0)then
c        print*,'5 Doing last write and setting gstp',isav,
c     &     gstp(isav)
        gstp(isav)=tfirst+((dpernosave+0.5d0)*binsz)
        gstp(isav)=tfirst+((dpernosave)*binsz)        
      endif
      
c      dpernosave=dpernoarray(kval)

      if(kval.gt.0.and.dpernoarray(kval).gt.1.0d-12.and.
     &   icount.ne.0)then
        dpernosave=dpernoarray(kval)
      else
        dpernosave=0.0d0
      endif
 
16    continue

c      print*,'************************'
c      do k=1,istrt
c        print*,'GSTRT and GSTP from LC is ',k,gstrt(k),gstp(k)
c      enddo
     
c      print*,'ISAV first is',isav,istrt
      if(istrt.gt.ngti)then
        call fcecho(' ')
        call fcecho('TOO MANY GTIs!!!')
        call fcecho('The code can only handle 12800 at this time')
        call fcecho('Do not use fine phase filtering !')
        call fcecho('Use TIMEMIN and TIMEMAX to shorten data.')
        call fcecho('Aborting...')
        status=0
        call ftclos(ounit,status)
        stop
      endif
      
      isav=istrt
c      print*,'ISAV after increment is',isav
      
      if(.not.lwritegti)then
        return
      endif
      
c      print*,'ISTRT is ',istrt

c        Unfortunately due to the limited accuracy of the machine
c we have to use a rounding function to remove the garbage that
c accumulates at the end of the double precisions number, thus we limit our
c accuracy to 1.0d-7 seconds which should be accurate enough for our
c applications. So we first multiply by 1.0d7 and then round our value
c and then expand it back out.
         

      do 351 i=1,istrt
         gstrt(i)=gstrt(i)*1.0d8
         gstrt(i)=(dnint(gstrt(i)))
         gstrt(i)=(gstrt(i))/1.0d8

         gstp(i)=gstp(i)*1.0d8
         gstp(i)=(dnint(gstp(i)))
         gstp(i)=(gstp(i))/1.0d8
        
c        print*,'Gstrt and gstp is ',i,gstrt(i),gstp(i)
351   continue
      
c        print*,'Gstp is ',(gstp(i),i=1,istrt)
c        print*,'itimnoall and ipermno',itimnoall,ipermno

c        print*,'Tmjds are ',(tmjds(i),i=1,itimnoall)
c        print*,'Tmjde are ',(tmjde(i),i=1,itimnoall)
c        print*,'Tmjde are ',(tmjds(i),i=itimnoall+1,itimnoall+ipermno)       
c        print*,'Tmjde are ',(tmjde(i),i=itimnoall+1,itimnoall+ipermno)
       
c      Create another Header Data Unit

      call ftpcks(ounit,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('ERROR creating/updating primary CHKSUM keyword.')
        status=0
      endif
      
c         print*,'Creating third header unit'
      call ftcrhd(ounit,status)

      if(status.ne.0)then
         call fcerr('ERROR creating GTI header extension....')
         call fcerrm(status)
         status=0
         call ftclos(ounit,status)
         stop
       endif

c      Write out the header information standard keywords for
c      writing a binary table extension.
           call ftphbn(ounit,istrt,nfield,ttypes,tforms,tunits,
     &          extnam,pcount,status)
           if(status.ne.0)then
              call fcerr('Error in writing header information')
              call fcerrm(status)
              status=0
              call ftclos(ounit,status)
              stop
           endif

c----------------------------------------------------------------------
c      Now that we have the storage information entered. We will enter
c      the pertinent information with regard to HDUCLASS.

        call ftpkys(ounit,'HDUCLASS','OGIP',
     &       'format conforms to OGIP/GSFC standards',status)
         if(status.ne.0)then
            call fcecho('Error writing keyword HDUCLASS')
            call fcerrm(status)
            status=0
         endif

        call ftpkys(ounit,'HDUCLAS1','GTI',
     &       'Extension contains Good Time Intervals',status)
         if(status.ne.0)then
            call fcecho('Error writing keyword HDUCLAS1')
            call fcerrm(status)
            status=0
         endif

        call ftpkys(ounit,'HDUCLAS2','ALL',
     &       'Extension contains Good Time Intervals',status)        
         if(status.ne.0)then
            call fcecho('Error writing keyword HDUCLAS2')
            call fcerrm(status)
            status=0
         endif

         call ftpkys(ounit,'INSTRUME',instrume,
     &        'Instrument used for observation',status)
         if(status.ne.0)then
            call fcecho('Error writing keyword INSTRUME')
           call fcerrm(status)
           status=0
        endif

        if(imjdref.eq.0)then
          call ftpkyd(ounit,'MJDREF',mjdref,16,'1993.0',status)
          if(status.ne.0)then
            call fcecho('Error writing keyword MJDREF')
            call fcerrm(status)
            status=0
          endif
        else
          call ftpkyj(ounit,'MJDREFI',imjdref,
     &       'Integer part of MJDREF',status)
          if(status.ne.0)then
            call fcecho('Error writing keyword MJDREFI')
            status=0
          endif
          call ftpkyd(ounit,'MJDREFF',mjdref,15,
     &       'Fractional part of MJDREF ',status)
          if(status.ne.0)then
            call fcecho('Error writing keyword MJDREFF')
            call fcerrm(status)
            status=0
          endif
        endif

c        Unfortunately due to the limited accuracy of the machine when
c performing additions to accumulate the total time we have to perform
c a truncation such that we chop off all of the noise at the end.
c          timemin=timemin*1.0d8
c          timemin=(dnint(timemin))
c          timemin=(timemin)/1.0d8
          
c          timemax=timemax*1.0d8
c          timemax=(dnint(timemax))
c          timemax=(timemax)/1.0d8

        itimemin=(dint(timemin))
        timeminf=timemin-(dfloat(itimemin))
      
        itimemax=(dint(timemax))
        timemaxf=timemax-(dfloat(itimemax))

c        Unfortunately due to the limited accuracy of the machine
c we have to use a rounding function to remove the garbage that
c accumulates at the end of the double precisions number, thus we limit our
c accuracy to 1.0d-8 seconds which should be accurate enough for our
c applications. So we first multiply by 1.0d8 and then round our value
c and then expand it back out.
         
        timeminf=timeminf*1.0d8
        timeminf=(dnint(timeminf))
        timeminf=(timeminf)/1.0d8
        
        timemaxf=timemaxf*1.0d8
        timemaxf=(dnint(timemaxf))
        timemaxf=(timemaxf)/1.0d8

        call ftpkyd(ounit,'TIMEZERO',timezerof,15,
     &     'Offset to be added to TSTART, TSTOP, and Time',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword TIMEZERO')
          call fcerrm(status)
          status=0
        endif
          
        call ftpkyj(ounit,'TSTARTI',itimemin,
     &     'Integer observation start time',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword TSTARTI')
          call fcerrm(status)
          status=0
        endif
      
        call ftpkyd(ounit,'TSTARTF',timeminf,15,
     &     'Fractional observation start time',status)
c      print*,' wrote TSTART'
        if(status.ne.0)then
          call fcecho('Error writing keyword TSTARTF')
          call fcerrm(status)
          status=0
        endif

c         print*,'calculated tstopi ',timemax
         
        call ftpkyj(ounit,'TSTOPI',itimemax,
     &     'Integer observation stop time',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword TSTOPI')
          call fcerrm(status)
          status=0
        endif
         
        call ftpkyd(ounit,'TSTOPF',timemaxf,15,
     &     'Fractional observation stop time',status)
        if(status.ne.0)then
          call fcecho('Error writing keyword TSTOPF')
          call fcerrm(status)
          status=0
        endif

c        call ftpkyd(ounit,'TSTART',timemin,15,
c     &       'Observation start time',status)
c      print*,' wrote TSTART'
c        if(status.ne.0)then
c           call fcecho('Error writing keyword TSTART')
c           call fcerrm(status)
c           status=0
c        endif

c         print*,'calculated tstopi ',timemax

c        call ftpkyd(ounit,'TSTOP',timemax,15,
c     &       'Observation stop time',status)
c         print*,' wrote TSTOP'
c        if(status.ne.0)then
c           call fcecho('Error writing keyword TSTOP')
c           call fcerrm(status)
c           status=0
c        endif

         call ftpkys(ounit,'OBJECT',object,
     &        'OBJECT from the FIRST input file',status)
         if(status.ne.0)then
            call fcecho('Error writing keyword OBJECT')
           call fcerrm(status)
           status=0
        endif

         call ftpkye(ounit,'RA_PNT',ra,8,
     &        'RA of First input object',status)
         if(status.ne.0)then
            call fcecho('Error writing keyword RA_PNT')
           call fcerrm(status)
           status=0
        endif

         call ftpkye(ounit,'DEC_PNT',dec,8,
     &        'DEC of First input object',status)
         if(status.ne.0)then
            call fcecho('Error writing keyword DEC_PNT')
           call fcerrm(status)
           status=0
        endif

         call ftpkyf(ounit,'EQUINOX',equino,2,
     &        'Equinox of the FIRST object',status)
c         print*,'wrote EQUINOX'
         if(status.ne.0)then
            call fcecho('Error writing keyword EQUINOX')
           call fcerrm(status)
           status=0
        endif

         call ftpkys(ounit,'RADECSYS',radecsys,
     &        'Co-ordinate frame used for equinox',status)
c        print*,'wrote RADECSYS'
         if(status.ne.0)then
            call fcecho('Error writing keyword RADECSYS')
           call fcerrm(status)
           status=0
        endif

         call ftpkys(ounit,'DATE-OBS',dateobs,
     &        'EARLIEST observation date of files',status)
c         print*,'wrote date-obs',dateobs
         if(status.ne.0)then
            call fcecho('Error writing keyword DATE-OBS')
           call fcerrm(status)
           status=0
        endif

         call ftpkys(ounit,'DATE-END',dateend,
     &        'LATEST observation date of files',status)
c         print*,'wrote date-end',dateend
         if(status.ne.0)then
            call fcecho('Error writing keyword DATE-END')
           call fcerrm(status)
           status=0
        endif
        
         call ftpkys(ounit,'TIME-OBS',timeobs,
     &        'EARLIEST time of all input files',status)
c         print*,'wrote time-obs',timeobs
         if(status.ne.0)then
            call fcecho('Error writing keyword TIME-OBS')
           call fcerrm(status)
           status=0
        endif

         call ftpkys(ounit,'TIME-END',timeend,
     &        'LATEST time of all input files',status)
c         print*,'wrote time-end',timeend
         if(status.ne.0)then
            call fcecho('Error writing keyword TIME-END')
           call fcerrm(status)
           status=0
        endif

        call ftpkys(ounit,'TIMESYS',timesys,
     &       'Time measured from 1993 Jan 1 00:00 UT',status)
        if(status.ne.0)then
           call fcecho('Error writing keyword TIMESYS')
           call fcerrm(status)
           status=0
        endif
        
        call ftpkys(ounit,'TIMEUNIT',timeunit,
     &       'unit for time related keywords',status)
        if(status.ne.0)then
           call fcecho('Error writing keyword TIMEUNIT')
           call fcerrm(status)
           status=0
        endif

        call ftpkyd(ounit,'ONTIME',totsecs,15,         
     &        'time on source',status)
         if(status.ne.0)then
            call fcecho('Error writing keyword ONTIME')
            call fcerrm(status)
            status=0
         endif

c       Write out the name of the code and its version number         
         call ftpkys(ounit,'CREATOR',taskname,
     &        'Program name that produced this file',status)
         if(status.ne.0)then
           call fcecho('Error writing keyword CREATOR')
           call fcerrm(status)
           status=0
        endif

c       call ftbdef(ounit,nfield,tforms,pcount,istrt,status)
c       if(status.ne.0)then
c         call fcecho(' ')
c         call fcecho('Error creating second extension in LC_GTI')
c         call fcecho('Cannot continue. Aborting...')
c         status=0
c         call ftclos(ounit,status)
c         stop
c       endif
c 3Dec97 - No longer necessary and conflicts w/FITSIO v5.04 (MJT)
          
c         print*,'Created second data header',status
         
        do 200 j=1,istrt
          call ftpcld(ounit,1,j,1,1,gstrt(j),status)

c          print*,'gstrt(j) and gstp is ',j,gstrt(j),gstp(j)

          if (status .ne. 0) then
            call fcerr('Error in writing GSTRT value')
            call fcerrm(status)
            status=0
            call ftclos(ounit,status)
            stop
          endif

          call ftpcld(ounit,2,j,1,1,gstp(j),status)
          if (status .ne. 0) then
            call fcerr('Error in writing GSTP value')
            call fcerrm(status)
            call ftclos(ounit,status)
          endif
          
200     continue

        call ftpcks(ounit,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('ERROR creating/updating primary CHKSUM keyword.')
          status=0
        endif

c      Close the file... 
        call ftclos(ounit,status)

        if (status .ne. 0) then
           call fcerr('Error in closing up output file')
           call fcerrm(status)
        endif

        call ftfiou(ounit,status)
        if(status.ne.0)then
          call fcecho('Error freeing logical output number')
          status=0
        endif

        return
        end
      
      
