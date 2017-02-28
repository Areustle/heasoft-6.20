c**********************************************************************
c     This subroutine takes the number of bins that have been stored in
c dchanall and dchan and decides how many time cycles must be completed
c in order to process all input files but only with ICHAN bins accumulated
c in each cycle.
C
C 08Jun2006 (MJT) 
C   Changed loop variable from double to integer to conform
C   to f90 standard (specifically g95) and cleaned up code.

      subroutine timeinterval(ichan,itimecycle, dchanlower,
     &   timelower, timeupper, dchanall, doffset,
     &   inocols, time, tims, binsz, no)
      implicit none

      integer ichan, itimecycle,
     &   inocols, ihold, icycles,
     &   status, d
      double precision timelower(*), timeupper(*), time(*), tims(*),
     &   binsz, doffset(2,*), dchanlower(*), dchanall,
     &   dchansave

      character(20) cval
      character(80) contxt
      integer ifile,ibins,no,ikchan,id
      double precision timearay,dtest,dtruc,dtemp,dfull
      logical ldchanallchange

      dfull=0.0d0
      ihold=0
      icycles=0
      status=0
      itimecycle=1
      ldchanallchange=.FALSE.
      dchansave=dchanall
      cval=' '
      contxt=' '

      if(idint(dchanall*dfloat(inocols)).gt.ichan)then
        dchanall=dfloat(ichan/inocols)
        ldchanallchange=.TRUE.
      endif

      if(dchansave/dchanall.gt.50)then
        call fcecho(' ')
        call fcecho('***************WARNING!!!******************')
        call fcecho('You are being very inefficient in processing your d
     &ata!')
        call fcecho('You have files that range over a long time, and a v
     &ery small bin-size.')
        call fcecho('Either increase your bin-size to a larger value,')
        call fcecho('or run GROSSTIMEFILT on your data!')
        call fcecho(' ')
        call fcecho('Since SA(E)EXTRCT cycles through data in lcbinarray
     & element sections.')

        call ftd2f(dchansave,2,cval,status)
        if(status.ne.0)then
          contxt='Error in double dchansave to characters'
          call fcecho(contxt)
          status=0
        endif
        contxt = 'And you are trying to process '
        contxt(31:51)=cval(1:20)
        contxt(53:80)='light-bins using an array'
        call fcecho(contxt)
        contxt=' '

        cval=' '
        call fti2c(ichan,cval,status)
        if(status.ne.0)then
          contxt='Error in integer ichan to characters'
          call fcecho(contxt)
          status=0
        endif
        contxt(1:21)=cval
        contxt(23:80)='elements in size.'

        call fcecho(contxt)
        call fcecho(' You can change this by setting lcbinarray.')
        contxt=' '

        call fcecho('At the moment the extactor will have to cycle throu
     &gh all of your input data ')
        contxt=' '
        contxt='a total of'
        cval=' '
        call ftd2f((dchansave/dchanall),2,cval,status)
        contxt(12:32)=cval(1:20)
        contxt(34:80)='times!!! This will take a VERY long time.'
        call fcecho(contxt)
        call fcecho('A wiser course would be to use GROSSTIMEFILT to fil
     &ter this data')
        call fcecho('before using this extractor code to process it.')
        call fcecho(' ')
        call fcecho('If you KNOW your data is in time ascending order')
        call fcecho('you may want to set the parameter MAXMISS to')
        call fcecho('speed processing - but you could lose data.')
        call fcecho('*******************************************')
        call fcecho('Use a Cntl-C now to kill this process, or prepare t
     &o wait a VERY LONG TIME!!!')
        call fcecho('*******************************************')
      endif
      
      icycles=idint((dchanall*dfloat(inocols))/dfloat(ichan+1))
      
      if(icycles.gt.1)then
        call fcecho(' ')
        call fcecho('************************************************')
        call fcecho('Too many bins are necessary to process all files')
        call fcecho('so processing will be broken into several cycles')
        call fcecho('with each cycle containing the maximum number of')
        call fcecho('light bins that can be processed for efficiency.')
        call fcecho('The light curve will be written out in several')
        call fcecho('parts. Since all input files will be processed')
        call fcecho('for each cycle, no time intervals are missed,')
        call fcecho('but speed may be affected.')
        call fcecho('************************************************')
        call fcecho(' ')
      endif
      
      timelower(itimecycle)=tims(1)
      
      ifile=1
      dtest=0.0d0
      dtemp=0.0d0
      ibins=0
      ikchan=0
      
C     do 290 d = 1.0d0, dchansave, 1.0d0
      do 290 d = 1, int(dchansave), 1

C       id=dint(d)
        id=d
        
        ikchan=ikchan+1
        
        dtest=dtest+1.0d0
        ibins=ibins+1

        if(mod(id,ichan).eq.0)then
          call fti2c((id/ichan),cval,status)
          status=0
          contxt=' '
          contxt='Setup              time cycles, continuing.'
          contxt(7:18)=cval(10:20)
          call fcecho(contxt)
        endif
        
        if(d.ge.dchansave)goto 290
          
        if(ibins.gt.doffset(2,ifile))then
          ifile=ifile+1
          ibins=1
          dtest=dtest+doffset(1,ifile)
        endif

        dtemp=dtruc(dtest-1.0d0)
        timearay=tims(1)+(dtemp*binsz)
        
        if(ikchan.eq.(ichan/inocols))then
          timeupper(itimecycle)=timearay
          itimecycle=itimecycle+1
          dfull=dfull+ikchan
          dchanlower(itimecycle)=dtemp
          dchanlower(itimecycle)=dtruc(dfull-1.0d0)
          timelower(itimecycle)=timearay
          ikchan=0
        endif
        
290   continue
      
      if(timelower(itimecycle).gt.time(no))then
        itimecycle=itimecycle-1
      else
        timeupper(itimecycle)=time(no)   
      endif
      
      dchanlower(itimecycle+1)=dfull+ikchan-1
      
      return
      end
