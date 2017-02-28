      subroutine wrtphagti(ounit,igti,gstrt,gstp,
     &     timemin,timemax,totsecs)

      implicit none
      double precision timemin, timemax,totsecs,
     &   gstrt(*),gstp(*)
      integer ounit,igti
      integer ngti,ichan,itemp1,itemp2,ngti_temp,status

      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
      
      common/gtiinfo/ichan,ngti
      
      status=0
      ngti_temp=ngti

c     initialize "pointer" variables for udmget to avoid realloc
c     MJT 29Jan2002
      itemp1 = 0
      itemp2 = 0
      
      call udmget(ngti,7,itemp1,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TEMP1')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        stop
      endif
            
      call udmget(ngti,7,itemp2,status)
      if(status.ne.0)then
        call fcecho('Error allocating memory for TEMP2')
        call fcecho('Free up some memory, by closing windows')
        call fcecho('or terminating background jobs.')
        call fcecho('Then try running this again.')
        call fcerrm(status)
        status=0
        stop
      endif
      
      call wrtphagti_root(ounit,igti,gstrt,gstp,
     &   timemin,timemax,totsecs,
     &   ngti_temp,memd(itemp1),memd(itemp2))

      call udmfre(itemp1,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TEMP1')
        call fcerrm(status)
        status=0
      endif

      call udmfre(itemp2,7,status)
      if(status.ne.0)then
        call fcecho('Error freeing memory for TEMP2')
        call fcerrm(status)
        status=0
      endif

      
      return
      end
      
      subroutine wrtphagti_root(ounit,igti,gstrt,gstp,
     &     timemin,timemax,totsecs,
     &   ngti,temp1,temp2)
      implicit none
      
      integer ngti,isiz,nc
      parameter (nc = 5)
      parameter (isiz = 100)
       
      character(80) gtistart,gtistop
      double precision timemin,timemax,totsecs,mjdref,
     &     gstrt(*),gstp(*),temp1(*),temp2(*),
     &   timeminf,timemaxf,timezerof
      real equino,ra,dec
      integer ounit,status,imjdref,
     &   itimemin,itimemax
      character(80) instrume,
     &     radecsys,object,dateobs,timeobs,dateend,timeend      

      character(40) ttypes(nc),tforms(nc),tunits(nc),
     &     contxt,extnam,timesys,timeunit

      integer j,k,pcount,gcount,nfield,igti,bitpix,
     &     naxis,istrt,istp,itest2,itest
      logical lor,land,lfirst,lsecond,lold,lagain

      character(40) taskname        
      common /task/ taskname

      common/gtinfo/mjdref,imjdref,ra,dec,equino,dateobs,timeobs
     &     ,dateend,timeend,
     &     instrume,object,radecsys,timesys,timeunit
      common/gtistuff/gtistart,gtistop
      
c      print*,'We are gtiwrite',ounit
      
c      Define the information about the number of columns and the
c      like. This information will have to change at a later date as
c      the options for calculating errors and other information are
c      included. Actually the above will have to be expanded
c      as well... 


      lfirst=.FALSE.
      lsecond=.FALSE.
      lor=.FALSE.
      land=.FALSE.
      bitpix = 8
      naxis = 0
      pcount = 0
      gcount = 1
      istrt = 0
      istp = 0
      status=0
      extnam='STDGTI'
      timezerof=0.0d0
      timeminf=0.0d0
      timemaxf=0.0d0
      itimemin=0
      itimemax=0

      call dinitial(ngti,temp1)
      call dinitial(ngti,temp2)
      
      nfield=2
      
      ttypes(1)=gtistart
      tforms(1)='1D'
      tunits(1)='s'
      
      ttypes(2)=gtistop
      tforms(2)='1D'
      tunits(2)='s'

c**********************************************************************
c      Let's find out some information that we must know in order to
c      write out the GTI information that is pertinent to this file.

      lold=.FALSE.
      lfirst=.FALSE.
      lsecond=.FALSE.
      istrt=1

16    continue

      if(igti.gt.ngti)then
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
      
      
      itest=igti
      itest2=1
      temp1(1)=gstrt(1)
      temp2(1)=gstp(1)

      do 14 k=2,igti
        itest2=itest2+1
        temp1(itest2)=gstrt(k)
        temp2(itest2)=gstp(k)
        if(gstrt(k).eq.gstp(k-1))then
c            print*,'Found one',k,gstrt(k),gstp(k-1)
          itest=itest-1
          itest2=itest2-1
          temp2(itest2)=gstp(k)
        endif
14    continue

      lagain=.FALSE.
      if(igti.ne.itest)lagain=.TRUE.

      igti=itest
      do 15 k=1,igti
        gstrt(k)=temp1(k)
        gstp(k)=temp2(k)
        temp1(k)=0.0d0
        temp2(k)=0.0d0
15    continue

c        print*,'lagain is ',lagain
      if(lagain)goto 16
        
c        print*,'IGTI is ',igti
c        print*,'Gstrt is ',(gstrt(i),i=1,igti)
c        print*,'Gstp is ',(gstp(i),i=1,igti)

c        print*,'************************'
c        do k=1,istrt
c          print*,'GSTRT and GSTP  from PHA is ',k,gstrt(k),gstp(k)
c        enddo
        
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
        contxt='ERROR creating GTI header extension....'
        call fcerr(contxt)
        call fcerrm(status)
        status=0
        call ftclos(ounit,status)
        stop
      endif

c      Write out the header information standard keywords for
c      writing a binary table extension.
      call ftphbn(ounit,igti,nfield,ttypes,tforms,tunits,
     &   extnam,pcount,status)
      if(status.ne.0)then
        contxt='Error in writing header information'
        call fcerr(contxt)
        call fcerrm(status)
        status=0
        call ftclos(ounit,status)
        stop
      endif

c----------------------------------------------------------------------
c      Now that we have the storage information entered. We will enter
c      the pertinent information with regard to HDUCLASS.

      call ftpkys(ounit,'HDUCLASS','OGIP',
     &   'format conforms to OGIP/GSFC standards',status)
      if(status.ne.0)then
        contxt='Error writing keyword HDUCLASS'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      call ftpkys(ounit,'HDUCLAS1','GTI',
     &   'Extension contains Good Time Intervals',status)
      if(status.ne.0)then
        contxt='Error writing keyword HDUCLAS1'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      call ftpkys(ounit,'HDUCLAS2','ALL',
     &   'Extension contains Good Time Intervals',status)        
      if(status.ne.0)then
        contxt='Error writing keyword HDUCLAS2'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      call ftpkys(ounit,'INSTRUME',instrume,
     &   'Instrument used for observation',status)
      if(status.ne.0)then
        contxt='Error writing keyword INSTRUME'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      if(imjdref.eq.0)then
        call ftpkyd(ounit,'MJDREF',mjdref,16,'1993.0',status)
        if(status.ne.0)then
          contxt='Error writing keyword MJDREF'
          call fcecho(contxt)
          call fcerrm(status)
          status=0
        endif
      else
        call ftpkyj(ounit,'MJDREFI',imjdref,
     &     'Integer part of MJDREF',status)
        if(status.ne.0)then
          contxt='Error writing keyword MJDREFI'
          call fcecho(contxt)
          status=0
        endif
        call ftpkyd(ounit,'MJDREFF',mjdref,15,
     &     'Fractional part of MJDREF ',status)
        if(status.ne.0)then
          contxt='Error writing keyword MJDREFF'
          call fcecho(contxt)
          call fcerrm(status)
          status=0
        endif
      endif

c        Unfortunately due to the limited accuracy of the machine when
c performing additions to accumulate the total time we have to perform
c a truncation such that we chop off all of the noise at the end.

c      timemin=timemin*1.0d8
c      timemin=(dnint(timemin))
c      timemin=(timemin)/1.0d8
          
c      timemax=timemax*1.0d8
c      timemax=(dnint(timemax))
c      timemax=(timemax)/1.0d8

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
     &   'Offset to be added to TSTART, TSTOP, and Time',status)
      if(status.ne.0)then
        call fcecho('Error writing keyword TIMEZERO')
        call fcerrm(status)
        status=0
      endif
      
      call ftpkyj(ounit,'TSTARTI',itimemin,
     &   'Integer observation start time',status)
      if(status.ne.0)then
        call fcecho('Error writing keyword TSTARTI')
        call fcerrm(status)
        status=0
      endif
      
      call ftpkyd(ounit,'TSTARTF',timeminf,15,
     &   'Fractional observation start time',status)
c      print*,' wrote TSTART'
      if(status.ne.0)then
        call fcecho('Error writing keyword TSTARTF')
        call fcerrm(status)
        status=0
      endif

c         print*,'calculated tstopi ',timemax
         
      call ftpkyj(ounit,'TSTOPI',itimemax,
     &   'Integer observation stop time',status)
      if(status.ne.0)then
        call fcecho('Error writing keyword TSTOPI')
        call fcerrm(status)
        status=0
      endif
         
      call ftpkyd(ounit,'TSTOPF',timemaxf,15,
     &   'Fractional observation stop time',status)
      if(status.ne.0)then
        call fcecho('Error writing keyword TSTOPF')
        call fcerrm(status)
        status=0
      endif

c      call ftpkyd(ounit,'TSTOP',timemax,14,
c     &   'Observation stop time',status)
c         print*,' wrote TSTOP'
c      if(status.ne.0)then
c        contxt='Error writing keyword TSTOP'
c        call fcecho(contxt)
c        call fcerrm(status)
c        status=0
c      endif

      call ftpkys(ounit,'OBJECT',object,
     &   'OBJECT from the FIRST input file',status)
      if(status.ne.0)then
        contxt='Error writing keyword OBJECT'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      call ftpkye(ounit,'RA_PNT',ra,8,
     &   'RA of First input object',status)
      if(status.ne.0)then
        contxt='Error writing keyword RA_PNT'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      call ftpkye(ounit,'DEC_PNT',dec,8,
     &   'DEC of First input object',status)
      if(status.ne.0)then
        contxt='Error writing keyword DEC_PNT'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      call ftpkyf(ounit,'EQUINOX',equino,2,
     &   'Equinox of the FIRST object',status)
c         print*,'wrote EQUINOX'
      if(status.ne.0)then
        contxt='Error writing keyword EQUINOX'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      call ftpkys(ounit,'RADECSYS',radecsys,
     &   'Co-ordinate frame used for equinox',status)
c        print*,'wrote RADECSYS'
      if(status.ne.0)then
        contxt='Error writing keyword RADECSYS'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      call ftpkys(ounit,'DATE-OBS',dateobs,
     &   'EARLIEST observation date of files',status)
c         print*,'wrote date-obs',dateobs
      if(status.ne.0)then
        contxt='Error writing keyword DATE-OBS'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      call ftpkys(ounit,'TIME-OBS',timeobs,
     &   'EARLIEST time of all input files',status)
c         print*,'wrote time-obs',timeobs
      if(status.ne.0)then
        contxt='Error writing keyword TIME-OBS'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      call ftpkys(ounit,'DATE-END',dateend,
     &   'LATEST observation date of files',status)
c         print*,'wrote date-end',dateend
      if(status.ne.0)then
        contxt='Error writing keyword DATE-END'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      call ftpkys(ounit,'TIME-END',timeend,
     &   'LATEST time of all input files',status)
c         print*,'wrote time-end',timeend
      if(status.ne.0)then
        contxt='Error writing keyword TIME-END'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif
        
      call ftpkys(ounit,'TIMESYS',timesys,
     &   'Time measured from 1993 Jan 1 00:00 UT',status)
      if(status.ne.0)then
        contxt='Error writing keyword TIMESYS'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif
        
      call ftpkys(ounit,'TIMEUNIT',timeunit,
     &   'unit for time related keywords',status)
      if(status.ne.0)then
        contxt='Error writing keyword TIMEUNIT'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

      call ftpkyd(ounit,'ONTIME',totsecs,15,         
     &   'time on source',status)
      if(status.ne.0)then
        contxt='Error writing keyword ONTIME'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

c       Write out the name of the code and its version number         
      call ftpkys(ounit,'CREATOR',taskname,
     &   'Program name that produced this file',status)
c         print*,'wrote CREATOR'
      if(status.ne.0)then
        contxt='Error writing keyword CREATOR'
        call fcecho(contxt)
        call fcerrm(status)
        status=0
      endif

c     call ftbdef(ounit,nfield,tforms,pcount,igti,status)
c     if(status.ne.0)then
c       call fcecho(' ')
c       call fcecho('Error creating second extension in PHA_GTI')
c       call fcecho('Cannot continue. Aborting...')
c       stop
c     endif
c 3Dec97 - No longer necessary and conflicts w/FITSIO v5.04 (MJT)
        
c         print*,'Created second data header',status

      do 200 j=1,igti
        call ftpcld(ounit,1,j,1,1,gstrt(j),status)
c              print*,'gstrt(j) is ',gstrt(j),gstp(j)
        if (status .ne. 0) then
          contxt='Error in writing GSTRT value'
          call fcerr(contxt)
          call fcerrm(status)
          status=0
          call ftclos(ounit,status)
          stop
        endif
          
        call ftpcld(ounit,2,j,1,1,gstp(j),status)
        if (status .ne. 0) then
          contxt='Error in writing GSTP value'
          call fcerr(contxt)
          call fcerrm(status)
          status=0
          call ftclos(ounit,status)
          stop
        endif

200   continue

      call ftpcks(ounit,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('ERROR creating/updating primary CHKSUM keyword.')
        status=0
      endif

           
c      Close the file... 
      call ftclos(ounit,status)
      
      if (status .ne. 0) then
        contxt='Error in closing up output file'
        call fcerr(contxt)
        call fcerrm(status)
      endif
      
      call ftfiou(ounit,status)
      if(status.ne.0)then
        contxt='Error freeing logical output number'
        call fcecho(contxt)
        status=0
      endif
      
      return
      end
