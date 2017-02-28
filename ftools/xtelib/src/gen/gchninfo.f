
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

c**********************************************************************
c      This subroutine reads in and sorts all of the channel filtering
c      information so that the code knows which channel interval values to
c      process and which to ignore. In order to save on allocation space
c      initially "cints" and "cinte" store the start and stop channels
c      that are associated with each file in files. Several actions are
c      performed within this subroutine.
c  1.) The GTI and PHA files are read and that information processed
c      with the final information sorted so as to arrive as a set of
c      good channel intervals that are to be processed.
c  2.) This listing is compared against the channel values stored in TMJD
c      to see if any of the files fall outside of the channel ranges so that
c      we can simply ignore those from the beginning.
c  3.) Once we have a full listing of valid channel intervals and files
c      to be processed, the values in TMJD are modified to reflect
c      this information and returned in those variable names. This
c      information will be used in subsequent file processing....
c**********************************************************************
c      
      subroutine gchninfo(chmin,chmax,chint,cints,cinte,
     &           ichno,getcmin,getcmax,status)
      implicit none

      integer nf,nb,itemp,isiz

c      Define all of the common parameters used in the arrays.
      parameter (nb = 512)
      parameter (nf = 258)
      parameter (itemp = 100)
      parameter (isiz = 100)
      
      character*(*) chint
      character(3000) chtemp1,chtemp2
      character(1) cmatch
      character(20) cval
      character(80) cval2

      integer ichno,cints(nb),cinte(nb),chmin,chmax,
     &     holds(nb),holde(nb),fcstln,ilen,outlen
      
      logical getcmin,getcmax,lchantrans
      
      integer i,j,k,l,nchs,ipos, ichunit
      integer status, iomode, recl
      logical lmatch, lbailout
      integer kval,kstart,kstop,kold      

      common/bail/lbailout
      
      outlen=fcstln(chint)

      nchs=0
      status=0
      cmatch='@'
      lmatch=.FALSE.
      lchantrans=.TRUE.
      chtemp1=' '
      chtemp2=' '
      cval2=' '
      iomode = 1
      recl = 133

      kold=1
      
      do i=1,nb
        cints(i)=0
        cinte(i)=0
      enddo

      if(getcmin)chmin=0
      if(getcmax)chmax=2047

      if(chint.ne.' ')then
        ilen=fcstln(chint)
        call scnstrforchar(chint,ilen,cmatch,lmatch,ipos)

        if(lmatch)then
c         A match was found for the character to denote that
c a file was input rather than a character string. So we will have
c to open this input file - assumed to be ASCII and in a format
c such that it contains N rows with:
c  Chan-start       Chan-stop

          nchs=0
          
c      Assign a unit file number to the input file.
          call ftgiou(ichunit,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error getting input ichunit number')
            call fcecho('Setting to logical unit 15')
            status=0
            ichunit=15
          endif

c      Open the ASCII file containing channels.
          call faopen(ichunit,chint(ipos+1:ilen),iomode,recl,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Could not open CHINT input channel file.')
            call fcerr('Cannot continue....')
            call fcecho('Aborting...')
            stop
          endif


c         First lets assume that the file contains a string or
c several strings that we will/may have to append together to
c create one long input string.

          j=0
500       continue
          read(ichunit,120,err=501,end=501)chtemp1
120       format(A3000)
          outlen=fcstln(chtemp1)
          do i=1,outlen
            if(chtemp1(i:i).eq.'-')lchantrans=.FALSE.
            if(chtemp1(i:i).eq.'~')lchantrans=.FALSE.            
            if(chtemp1(i:i).eq.';')lchantrans=.FALSE.
            if(chtemp1(i:i).eq.':')lchantrans=.FALSE.
            if(chtemp1(i:i).eq.'(')lchantrans=.FALSE.
            if(chtemp1(i:i).eq.')')lchantrans=.FALSE.
            if(chtemp1(i:i).eq.',')lchantrans=.FALSE.
            if(chtemp1(i:i).ne.' ')then
              j=j+1
              chtemp2(j:j)=chtemp1(i:i)
            endif
          enddo
          
          goto 500
            
501       continue

c----------------------------------------------------------------------
c Since it is possible that the file contains channels that were not
c specified as per the CPIX keyword standard, we will have to rewind
c the file and re-read it if the logical lchantrans is .TRUE., i.e., no
c "special characters were encountered in the input file.

          if(lchantrans)then

c           Input file was created by chantrans 
            
            rewind(ichunit)

700         nchs = nchs+1
            read(ichunit,*,err=701,end=701)cints(nchs),cinte(nchs)
            goto 700
701         continue
            nchs = nchs-1


c           Now that we have constructed the start and stop channels
c from the input file, we have to construct a string that gives the same
c infomation as is contained in the file. Once we are finished we reset
c the value of the input variable such that it contains a descriptive
c string that is ready to be output to the .pha files.
            if(nchs.gt.0)then
              chint=' '
            endif
            
            chint(kold:kold)='('
            kold=kold+1
            
            do i=1,nchs

              cval=' '
              call fti2c(cints(i),cval,status)
              if(status.ne.0)then
                call fcecho(' ')
                call fcecho('Cannot convert channel start value to')
                call fcecho('a character. Your CPIX keyword')
                call fcecho('placed in ouput files will be wrong.')
                call fcecho('All other results will be unaffected.')
              endif
              
              outlen=fcstln(cval)
              kstart=0
              kstop=0

              do j=1,outlen

                if(kstart.eq.0)then
                  if(cval(j:j).ne.' ')kstart=j
                endif
                if(kstart.ne.0)then
                  if(cval(j:j).ne.' ')kstop=j
                endif
                
              enddo
              
              kval=1
              cval2(kval:kval+kstop-kstart+1)=cval(kstart:kstop)
              kval=kval+kstop-kstart+1

              if(cints(i).ne.cinte(i))then
              
                cval2(kval:kval)='~'
                kval=kval+1

                cval=' '
                call fti2c(cinte(i),cval,status)
                if(status.ne.0)then
                  call fcecho(' ')
                  call fcecho('Cannot convert channel stop value to')
                  call fcecho('a character. Your CPIX keyword')
                  call fcecho('placed in ouput files will be wrong.')
                  call fcecho('All other results will be unaffected.')
                  if(lbailout)then
                    call fcecho(' ')
                    call fcecho('You have the bailout option set!')
                    call fcecho('Aborting...')
                    stop
                  endif
            
                endif
                
                outlen=fcstln(cval)
                kstart=0
                kstop=0
                
                do j=1,outlen
                  
                  if(kstart.eq.0)then
                    if(cval(j:j).ne.' ')kstart=j
                  endif
                  if(kstart.ne.0)then
                    if(cval(j:j).ne.' ')kstop=j
                  endif
                  
                enddo
                
                cval2(kval:kval+kstop-kstart+1)=cval(kstart:kstop)
                kval=kval+kstop-kstart+1
              
              endif

              outlen=fcstln(cval2)
              chint(kold:kold+outlen)=cval2(:outlen)
              cval2=' '
              kold=kold+outlen
              
              if(i.lt.nchs)then
                chint(kold:kold)=','
                kold=kold+1
              endif
              
              outlen=fcstln(chint)
             
            enddo

            
            outlen=fcstln(chint)
            chint(outlen+1:outlen+1)=')'
            outlen=fcstln(chint)
            call fcecho(' ')
            call fcecho('Input file contained raw channels')
            call fcecho('specified using standard:')
            call fcecho('CH_START    CH_STOP ')
            call fcecho(' ')
            call fcecho('The constructed CPIX string is:')
            call fcecho(chint(1:outlen))

          else

            chint=chtemp2
            call fcecho(' ')
            call fcecho('Input file contained a channel string')
            call fcecho('specified using the same type of syntax')
            call fcecho('as CPIX. The constructud CPIX string is:')
            call fcecho(' ')
            call fcecho(chint(1:j))
            
            call parsebd(chint,nchs,nb,cints,cinte,.FALSE.,status)
            if(status.ne.0)then
              call fcecho(' ')
              call fcecho('Could not parse columns for channel info.')
              call fcecho('Error in parsing channel information in')
              call fcecho('chint parameter. Aborting...')
              stop
            endif

          endif
          

c---------------------------------------------------------------------- 

c      Close the ASCII file 
          close(ichunit)

c      Free the logical unit number used. 
          call ftfiou(ichunit,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Error freeing input ichunit number')
            call fcecho('Setting to logical unit 15')
            status=0
          endif

        else

c      We were not given a filename as input, so assume it is
c      an input string.
          
c      Call the subroutine that will parce the input string of
c      good time intervals that are to be included in performing the
c      calculations.
         
c          call xtecolparse (chint,1,chmax,nchs,cints,cinte,cinc,status)
          call parsebd(chint,nchs,nb,cints,cinte,.FALSE.,status)
          if(status.ne.0)then
            call fcecho(' ')
            call fcecho('Could not parse columns for channel info.')
            call fcecho('Error in parsing channel information in')
            call fcecho('chint parameter. Aborting...')
            stop
          endif

        endif
      
        do 10 i=1,nchs
          holds(i)=cints(i)
          holde(i)=cinte(i)
c          print*,'holds and holdi are ',i,holds(i),holde(i)
10      continue

c----------------------------------------------------------------------
      
        k=nchs
        j=0
      
        do 100 i=1,k
          if(holde(i).lt.chmin)then
c            print*,'*** holde(i) is less than chmin',holde(i),i
            nchs=nchs-1
            
            do 101 l=i-j,nchs
              cints(l)=cints(l+1)
              cinte(l)=cinte(l+1)
101         continue
            j=j+1

          endif

          if(holds(i).gt.chmax)then
            nchs=nchs-1
          endif
            
100     continue

        if(getcmin)chmin=cints(1)
        if(getcmax)chmax=cinte(nchs)
        
        if(chmin.ge.cints(1))then
          cints(1)=chmin
        else
          chmin=cints(1)
        endif
        
        if(cinte(nchs).ge.chmax)then
          cinte(nchs)=chmax
        else
          chmax=cinte(nchs)
        endif
        
      else

c      If the interval file is not set than set it to be the maximum and
c      minimum channel... 
        cints(1)=chmin
        cinte(1)=chmax
        nchs=1
      endif
      
      ichno=nchs

c      print*,'cints is ',(cints(i),i=1,ichno)
c      print*,'cinte is ',(cinte(i),i=1,ichno)


c      print*,'Number of Channels are',nchs
c      do i=1,nchs
c        print*,cints(i),cinte(i)
c      enddo
      
      return
      end
      
