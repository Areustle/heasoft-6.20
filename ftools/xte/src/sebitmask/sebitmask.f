c   This code is designed to read in filtering information supplied by the
c the user and read in the Data Descriptor in the file and from that
c information create a command line expression that is capable of being
c used to call FSELECT which will then create a data file containing only
c information which meets all of the selected criteria of the expression.
c This code is best when run via the Script SESELECT which will prompt
c the user and then run this code creating the information needed for FSELECT
c and then running FSELECT. 

      subroutine sebitk()
      implicit none
      integer MAXF_FNAME,nb,cfils,ibuf
      parameter (MAXF_FNAME = 3000)
      parameter (nb = 512)
      parameter (cfils = 40)
      parameter (ibuf = 10)

      character(1) cnum(10)
      character(2) cbegin,cend
      character(8) keyword
      character(20) cval,mtoken
      character*(MAXF_FNAME) infile,file1,cfilter,bitfile,cfiles(cfils)
      character*(MAXF_FNAME) bitmask,cexpr
      character*(nb) bitmasks(cfils),bitmask_base,bitmask_cmp,
     &   bitmaskt
      character(40) column,ttype(nb),tform(nb),tunit(nb),extnam
      character*(MAXF_FNAME) tddes, comm, temp
      character*(MAXF_FNAME) tddesm(ibuf)
      integer outlin, fcstln, status, iunit,extnum, block, nrows, cno,
     &   nfield,pcount,colpos,icol,xtend,i,dtype,rcount,width,ounit,j,
     &   outlinc,k,l,jsize,inotddesm,istart(ibuf), istop(ibuf),ival,
     &   imtoken,ibitmasks,mbitmasks(ibuf),outlin1,
     &   outlint,ilin,jlin,klin,lengarf
      logical exact,abort,lcfile,lmatch,lhelp,ldryrun,
     &   mfound,cfound,zfound,efound,dfound,lmfound,
     &   lcfound,lzfound,lefound,ldfound,ltrue,lfalse,
     &   land,lor,lnot,lgreater,llesser,lsimplify,lmzero
              
c      Set up an array where an integer value is assigned an
c      ascii character value
      data (cnum(i),i=1,10)/'0','1','2','3','4','5',
     &   '6','7','8','9'/

      character(40) taskname
      common/task/taskname

      taskname='sebitmask_4.0'
      temp=' '
      bitmask_base=' '
      bitmask_cmp=' '
      imtoken=1
      ibitmasks=1
      land=.FALSE.
      lor=.FALSE.
      lnot=.FALSE.
      lgreater=.FALSE.
      llesser=.FALSE.
      mfound=.FALSE.
      cfound=.FALSE.
      zfound=.FALSE.
      efound=.FALSE.
      dfound=.FALSE.
      lmfound=.FALSE.
      lcfound=.FALSE.
      lzfound=.FALSE.
      lefound=.FALSE.
      ldfound=.FALSE.
      lfalse=.FALSE.
      ltrue=.TRUE.
      lmzero=.FALSE.
      mtoken=' '
      cbegin=' '
      cend=' '
      lmatch=.FALSE.
      lhelp=.FALSE.
      ldryrun=.FALSE.
      lsimplify=.FALSE.
      exact=.TRUE.
      status=0
      keyword=' '
      cexpr=' '
      lcfile=.FALSE.
      cfilter=' '
      lengarf=0
      
      do i=1,cfils
        cfiles(i)=' '
      enddo

      call fcecho(' ')
      call fcecho('Running SEBITMASK version 4.0')
      call fcecho('==============================================')

      
      call uclgst('infile',infile,status)
      if(status.ne.0)then
        call fcecho('Could not get INFILE parameter from file')
        call fcecho('Aborting.')
        goto 999
      endif

      call uclgst('filter',cfilter,status)
      if(status.ne.0)then
        call fcecho('Could not get FILTER parameter from file')
        call fcecho('Aborting.')
        goto 999
      endif

      outlin=fcstln(cfilter)
      
      do i=1,outlin
        if(cfilter(i:i).eq.'@')lcfile=.TRUE.
      enddo

      if(lcfile)then
        call fcgcls(cfilter,cfiles,cno,abort)
        outlinc=0
        j=0
        do i=1,cno
          j=j+outlinc+1
          outlinc=fcstln(cfiles(i))
          cexpr(j:j+outlinc)=cfiles(i)
        enddo
      else
        cexpr(1:outlin)=cfilter(1:outlin)
      endif

c     Get the output file to write the bitmask into
      call uclgst('mtoken',mtoken,status)
      if(status.ne.0)then
        call fcerr('Could not get M-token parameter')
        goto 999
      endif

      if(mtoken(1:1).ne.'M')then
        call fcecho(' ')
        call fcecho('ERROR! M-token must be exactly as in TEVTB')
        call fcecho('You have not entered M-token properly.')
        call fcecho('The syntax is:M[nnn]{m}')
        call fcecho(' ')
        call fcecho('Supported M-tokens are:')
        call fcecho('M[1]{1} - The Default')
        call fcecho('M[127]{8}, M[1]{8}, M[2]{8}, and M[4]{8}')
        call fcecho(' ')
        call fcecho('M[0]{1} is SPECIAL since it can replace')
        call fcecho('any of the M[nnn]{8} values!')
        call fcecho('Cannot continue. Aborting...')
        return
      endif

C  get the columns
      call uclgst('column',column,status)
      if (status .ne. 0) then
        call fcerr('Could not get COLUMN parameter')
        goto 999
      endif

c     Get the output file to write the bitmask into
      call uclgst('bitfile',bitfile,status)
      if(status.ne.0)then
        call fcerr('Could not get BITFILE parameter')
        goto 999
      endif

c     Get the logical that tells if this is a "help" run
      call uclgsb('help',lhelp,status)
      if (status .ne. 0) then
        call fcerr('Could not get HELP parameter')
        goto 999
      endif      

c     Get the logical that tells if this is a "dryrun"
      call uclgsb('dryrun',ldryrun,status)
      if (status .ne. 0) then
        call fcerr('Could not get DRYRUN parameter')
        goto 999
      endif     

c      Assign a unit file number used in outputting 
      call ftgiou(ounit,status)
      if(status.ne.0)then
        call fcecho('Error getting output unit number')
        call fcecho('Setting to logical unit 9')
        status=0
        ounit=9
      endif

c      Assign a unit file number used in inputting file.
      call ftgiou(iunit,status)
      if(status.ne.0)then
        call fcecho('Error getting input unit number')
        call fcecho('Setting to logical unit 10')
        status=0
        iunit=10
      endif

      file1=' '
c      Parse the character input file name 
      call fcpars(infile,file1,extnum,status)
      if (status.ne.0)then
        call fcecho('Could not parse infile into file and extension')
        call fcecho('Aborting...')
        call fcerrm(status)
        goto 999
      endif
      
c      Since we know that the BINTABLE will be the 2nd file at the
c      mininum we check the 'extnum' and if it is less than 2 we
c      force it to 2. 
      if (extnum.lt.1) extnum=1

      lengarf=fcstln(file1)
      call fcecho(' ')
      call fcecho('Input file being operated on is:')
      call fcecho(file1(:lengarf))

      block=0
      status=0

c      Open the file that is of interest. Note that files have
c      been sorted in chktime according to time of observation.
      call ftopen(iunit,file1(:lengarf),0,block,status)
      if(status.ne.0)then
        call fcerr('Failure to open input file - aborting')
        call fcerrm(status)
        status=0
        call ftclos(iunit,status)
        return
      endif      
            
c      Skip the primary header and go to the second (or extnum)
c      to read all pertinent processing information.
      call ftmahd(iunit,extnum+1,xtend,status)
      if(status.ne.0)then
        call fcerr('Error moving to extnum')
        call fcerrm(status)
        status=0
      endif

c      Read the information about how the data is stored - see the
c      fitsio.for file for a description of this call. 
      call ftghbn(iunit,nb,nrows,nfield,ttype,
     &   tform,tunit,extnam,pcount,status)

      if(nrows.eq.0)then
        call fcecho(' ')
        call fcecho('This file contains no information under')
        call fcecho('the COLUMNS name or file is empty')
        call fcecho('You should check this file...')
      endif
            
c      Print out any error information about accessing the files
      if (status.ne.0)then
        call fcecho('Could not get FIELDS information')
        call fcerrm(status)
        return
      endif

c      Find the postition of the COLUMN and store it in
c      colpos
      call ftgcno(iunit,exact,column,colpos,status)
      if(status.ne.0)then
        call fcecho('Could not find COLUMN number - check input')
        call fcecho('aborting... cannot continue')
        call fcerrm(status)
        return
      endif

c      Parse the TFORM value to find out the type of data storage
c      and the number of 8 byte values that there are. The type
c      is returned in "dtype" where 11 or 1 = 'X', and the rcount value
c      is the number of bits/8. We have to parse the proper TDDES
c      field to find out where the bits which tell which channel detected
c      the photon resides at a later point.

      call ftbnfm(tform(colpos),dtype,rcount,width,status)
c      print*,'tform(colpos) is ',tform(colpos)
c      print*,'dtype,rcount,width',dtype,rcount,width
      if((dtype.ne.1).and.(dtype.ne.11))then
c        print*,'Dtype is ',dtype
        call fcecho('DTYPE != 1 or 11!!! Errors may occur')
      endif
      
c      print*,'Column position is ',colpos,status

      keyword(1:5)='TEVTB'
      icol=6   
      if(colpos.lt.10)then
        keyword(icol:icol)=cnum(colpos+1)
      elseif (colpos.lt.100)then
        keyword(icol:icol)=cnum(colpos/10+1)
        keyword(icol+1:icol+1)=cnum(mod(colpos,10)+1)
      elseif (colpos.lt.1000)then
        keyword(icol:icol)=cnum(colpos/100+1)
        keyword(icol+1:icol+1)=cnum((mod(colpos,100)/10)+1)
        keyword(icol+2:icol+2)=cnum(mod(mod(colpos,100),10)+1)
      elseif(colpos.ge.1000)then
        comm='ERROR column number greater than 999'
        call fcerr(comm)
      endif

c      print*,'Keyword is ',keyword,status

      call ftgkys(iunit,keyword,tddes,comm,status)
      if(status.ne.0)then
        call fcecho('*** ERROR ***')
        call fcecho('Cannot find TEVTBnnn KEYWORD for COLUMN')
        call fcecho('Cannot figure out the bitmask!')
        call fcecho('ABORTING....')
        goto 999
      endif


c       This little cludge is necessary because the HEXTE IT added a
c space between the ] and the { in the DDL which XFF does not. So this
c code moves through the TDDES and removes any spaces between those
c two special characters before the rest of the code is executed.
      if(tddes.ne.' ')then
        outlint=fcstln(tddes)
        jlin=0
        klin=0
        do ilin=1,outlint
          klin=klin+1
          if(klin.le.outlint)then
            if(tddes(klin:klin).eq.' ')then

            else
              jlin=jlin+1
              temp(jlin:jlin)=tddes(klin:klin)
            endif
          endif
        enddo
        
        outlint=fcstln(temp)
        tddes=' '
        tddes=temp
        
      endif
      temp=' '
      
      bitmask= ' '
      outlin=fcstln(tddes)
      if(lhelp)then
        ival=1
        call sebhelp(ival)
        call fcecho('--------------------------------------')
        call fcecho('--------------------------------------')
      endif
      
      
      if(ldryrun.or.lhelp)then
        call fcecho(' ')
        call fcecho('TEVTB value stored in file is:')
        call fcecho(tddes(:outlin))
      endif

      j=1
      k=1
      istart(1)=0
      istop(1)=0

      do jsize=1,outlin
        if(tddes(jsize:jsize+1).eq.'(M')then
          mfound=.TRUE.
          istart(k)=jsize+1
          if(tddes(jsize+1:jsize+8).eq.mtoken(:7))imtoken=k
        endif
        if(mfound.and.tddes(jsize:jsize).eq.')')then
          istop(k)=jsize-1
          k=k+1
        endif
      enddo

      k=k-1
      
      if(mfound)then
        call fcecho(' ')
        call fcecho('The M-token selected for processing is:')
        call fcecho(mtoken)
        inotddesm=k

        call fcecho(' ')
        call fcecho('The number of TDDES M values found is: ')
        call fti2c(inotddesm,cval,status)
        call fcecho(cval)
        call fcecho('The values stored in TDDESM variable is: ')

        do k=1,inotddesm
          tddesm(k)=tddes(istart(k):istop(k))
          call fcecho(tddes(istart(k):istop(k)))
        enddo

      else
        inotddesm=1
        imtoken=1
        tddesm(1)=tddes(:outlin)
        istart(imtoken)=1
        istop(imtoken)=outlin
        
      endif

      call fcecho(' ')
      call fcecho('The TDDESM value which interests us is:')
      call fcecho(tddes(istart(imtoken):istop(imtoken)))

      
      cbegin='Z['
      cend=']{'
      call scnremstr(tddesm(imtoken),cbegin,cend,temp,ltrue,
     &   ltrue,lzfound,abort)

      cbegin='C['
      cend=']'
      call scnremstr(cexpr,cbegin,cend,temp,ltrue,
     &   lhelp,lcfound,abort)
      if(lcfound)then
        ival= 6
        call sebhelp(ival)
      endif

      cend=']{'
      call scnremstr(tddesm(imtoken),cbegin,cend,temp,ltrue,
     &   lfalse,cfound,abort)
      
      lmfound = .FALSE.
      cbegin='M['
      call scnremstr(cexpr,cbegin,cend,temp,lfalse,
     &   ldryrun,lmfound,abort)
      if(lmfound)then
        ival=2
        call sebhelp(ival)
      endif
      
      lmfound=.FALSE.

      if(lhelp.and.(.not.ldryrun))return

      if(mtoken.eq.'M[0]{1}')lmzero=.TRUE.
      
      outlin=fcstln(cexpr)
      cfilter = cexpr

      if(mfound)then

        if(cfilter.ne.' '.and. cfilter .ne. '-')then
          cexpr = ' & ' // cfilter(:outlin)
          outlin=fcstln(cexpr)
          cfilter=cexpr(:outlin)
          outlin=fcstln(cfilter)
        endif

        if(mtoken.eq.'M[1]{1}')
     &     cexpr = 'M[1] == 1 ' // cfilter(:outlin)
      
        if(mtoken.eq.'M[127]{8}')
     &     cexpr = 'M[127] == 127 ' // cfilter(:outlin)
        
        if(mtoken.eq.'M[1]{8}')
     &     cexpr = 'M[1] == 1 ' // cfilter(:outlin)

        if(mtoken.eq.'M[2]{8}')
     &     cexpr = 'M[2] == 2 ' // cfilter(:outlin)

        if(mtoken.eq.'M[4]{8}')
     &     cexpr = 'M[4] == 4 ' // cfilter(:outlin)
      endif

      outlin=fcstln(cexpr)

      if(outlin.ge.1)then
        call fcecho(' ')
        call fcecho('Total CEXPR constructed is:')
        call fcecho(cexpr(:outlin))
      else
        call fcecho(' ')
        call fcecho('#################################################')
        call fcecho('You input no filtering expression!')
        call fcecho('No M-tokens are present in this file!')
        call fcecho('Insufficient information to create bit-mask.')
        call fcecho(' ')
        call fcecho('***No bit-mask can be generated!!***')
        call fcecho(' ')
        call fcecho('Check input file, and your filtering expression.')
        call fcecho('           *** Aborting ***')
        call fcecho('#################################################')
        goto 999
      endif
      

      if(ldryrun)goto 999

c     If the M-token is "normal" i.e. only affects 1 value we
c can generate our expression normally. 
      if(.not.lmzero)then
        call boolparse(tddesm(imtoken),rcount,column,
     &     cexpr,bitmask,status)
        if(status.ne.0)then
          call fcecho('Problem in parsing bitmask - check output')
          call fcecho('Proceeding...')
          status=0
        endif

      else
        
c     If the M-token is the "special" case where M[0]{1} is given then
c we cannot accept an "expression" since the "bitmask" changes for each
c M-token. Thus we can only deal with the "special case" and then proceed
c from that point. 
        bitmask = column
        outlin1=fcstln(bitmask)
        bitmask(outlin1+1:outlin1+13) = ' == b0xxxxxxx'
        outlin1=outlin1+13

        do i=1,rcount-8
          bitmask(outlin1+1:outlin1+1) = 'x'
          outlin1=outlin1+1
        enddo

        outlin1=fcstln(bitmask)

        status=0
        call faopen(ounit,bitfile,2,3,status)
        if(status.eq.117)then
          call fcecho('File already exists and clobber = NO')
          call fcecho('Cannot proceed - aborting.')
          status=0
          goto 999
        endif
        if(status.ne.0)then
          call fcecho('Unable to open ASCII output file')
          call fcecho('File does not exist. Unable to create')
          call fcecho('Cannot proceed - aborting.')
          call fcerrm(status)
          goto 999
        endif

        write(ounit,*)bitmask(:outlin1)
        close(ounit)
        goto 999
        
      endif

c      outlin1=fcstln(bitmask)
c      print*,'OUTLIN1 is ',outlin1
c      call uclpst('finput',bitmask(:outlin1),status)
c      if(status.ne.0)then
c        call fcecho('Could not write out FINPUT to par-file.')
c        call fcecho('Proceeding to write out ASCII file.')
c        status=0
c      endif

      outlin=fcstln(bitmask)

      status=0
      call faopen(ounit,bitfile(:outlin),2,3,status)
      if(status.eq.117)then
        call fcecho('File already exists and clobber = NO')
        call fcecho('Cannot proceed - aborting.')
        status=0
        goto 999
      endif
      if(status.ne.0)then
        call fcecho('Unable to open ASCII output file')
        call fcecho('File does not exist. Unable to create')
        call fcecho('Cannot proceed - aborting.')
        call fcerrm(status)
        goto 999
      endif
      
      j=1
      k=0
      
      do i=1,outlin
        if(bitmask(i:i).eq.'|')lor=.TRUE.
        if(bitmask(i:i).eq.'!')lnot=.TRUE.
c        if(bitmask(i:i).eq.'(')lor=.TRUE.
c        if(bitmask(i:i).eq.')')lor=.TRUE.
        if(bitmask(i:i).eq.'<')llesser=.TRUE.
        if(bitmask(i:i).eq.'>')lgreater=.TRUE.
        if(bitmask(i:i).eq.'&')land=.TRUE.
      enddo

      if((.not.lor).and.(.not.lnot))then
        call fcecho(' ')
        call fcecho('No ORs, NOTs, or ()s are in the bitmask string.')
        call fcecho('It is possible that this string *may* be')
        call fcecho('used as input to SEEXTRCT without running')
        call fcecho('FSELECT first.')
        call fcecho(' ')
      endif

      j=1
      k=0
      l=1

      do i=1,outlin
        k=k+1
        if(bitmask(k:k).eq.'&'.or.bitmask(k:k).eq.'|')then
          k=k+2
          if(l.eq.1)then
            bitmasks(l)=bitmask(j:k)
          else
            bitmasks(l)=bitmask(j+1:k)
          endif
c          print*,'bitmasks(l) is ',l,mbitmasks(l),bitmasks(l)
          
          if(lor.or.lnot)then
c            print*,'Writing value ',l
            if(l.eq.1)then
c              print*,bitmask(j:k)
              write(ounit,*)bitmask(j:k)
            else
              write(ounit,*)bitmask(j+1:k)
c              print*,bitmask(j+1:k)
            endif
          endif

          l=l+1
          j=k
        endif
        
        if(bitmask(k:k+3).eq.' == ')then
          mbitmasks(l)=1
        endif
        
        if(bitmask(k:k+3).eq.' >= ')then
          mbitmasks(l)=2
        endif
        
        if(bitmask(k:k+3).eq.' <= ')then
          mbitmasks(l)=3
        endif
          
        if(bitmask(k:k+2).eq.' > ')then
          mbitmasks(l)=4
        endif
          
        if(bitmask(k:k+2).eq.' < ')then
          mbitmasks(l)=5
        endif

      enddo


      if(l.eq.1)then
        bitmasks(l)=bitmask(j:k)
      else
        bitmasks(l)=bitmask(j+1:k)
      endif

      if(lor.or.lnot)then
        call fcecho(' ')
        call fcecho('Cannot use this bitmask in SEEXTRCT')
        call fcecho('Use the output for FSELECT input only')
        if(l.eq.1)then
c          print*,bitmask(j:k)
          write(ounit,*)bitmask(j:k)
        else
c          print*,bitmask(j+1:k)
          write(ounit,*)bitmask(j+1:k)
        endif
        close (ounit)
        goto 999
      endif

      
      ibitmasks=l

c      print*,'bitmasks(l) is ',l,mbitmasks(l),bitmasks(l)      

c      print*,'----------------------------------'
      outlin = fcstln(bitmasks(1))

c       Write out all of the strings that cannot be simplified due
c to have >, or < symbols within them. These will have to be handled
c in a special fashion within SEEXTRCT as it isn't a straight forward
c bit comparison as can be done with the == case.
        
      i=0
c      print*,'IBITMASKS is ',ibitmasks
        do k=1,ibitmasks
          if(mbitmasks(k).ne.1)then
c            print*,'MBITMASKS is GREATER than 3'
            if(k.ne.ibitmasks)then
              bitmaskt=bitmasks(k)
              outlin1=fcstln(bitmaskt)
              write(ounit,*)bitmaskt(:outlin1)
            else
              bitmaskt=bitmasks(k)
              outlin1=fcstln(bitmaskt)
              if(outlin1.ge.1)then
                bitmaskt(outlin1+1:outlin1+4)=' && '
                write(ounit,*)bitmaskt(:outlin1+4)
              endif
            endif
          else
c            print*,'mbitmasks eq 1'            
            i=i+1
            if(i.ne.k)then
              mbitmasks(i)=mbitmasks(k)
              bitmasks(i)=bitmasks(k)
            endif
            if(k.eq.ibitmasks)then
c              print*,'k and ibitmasks ',k,ibitmasks
              mbitmasks(i)=mbitmasks(k)
c              print*,'K eq ibitmasks  and ',mbitmasks(i)
            endif
          endif
        enddo

        ibitmasks=i
        
        bitmask_base=bitmasks(1)
c        print*,'ibitmasks is',ibitmasks,i
        
        if(ibitmasks.gt.1)then
          do k = 2,ibitmasks
            bitmask_cmp=bitmasks(k)
c            print*,'comparing ', bitmask_base, bitmask_cmp
            do i=2,outlin
              if(bitmask_base(i:i).eq.'x'.or.
     &           bitmask_base(i:i).eq.'1'.or.
     &           bitmask_base(i:i).eq.'0')then

                if((bitmask_base(i:i).eq.'x').and.
     &             bitmask_cmp(i:i).ne.'x')then
                  bitmask_base(i:i)=bitmask_cmp(i:i)

                else if((bitmask_base(i:i).ne.'x').and.
     &               bitmask_cmp(i:i).ne.'x')then
                  if(bitmask_base(i:i).ne.bitmask_cmp(i:i))then
c                    if(mbitmasks(k).eq.1)then
                      call fcecho(' ')
                      call fcecho('***ERROR***')
                      call fcecho('Mutually exclusive conditions exist')
                      call fcecho('between these bitmasks:')
                      call fcecho(bitmask_base)
                      call fcecho(bitmask_cmp)
                      call fcecho('Both cannot occur simultaneously!')
                      call fcecho('Check your input expression!')
                      call fcecho('Cannot continue... Aborting...')
                      close (ounit)
                      goto 999
c                    else
c                      print*,'In same comparison'
c                      bitmask_base(i:i)='X'
c                    endif
                    
                  endif
                  
                endif

              else
                if(bitmask_base(i:i).eq.'&')bitmask_base(i:i)=' '
              endif
            enddo
          enddo
        endif

        outlin1=fcstln(bitmask_base)
c        print*,'bitmask_base end is ',bitmask_base(:outlin1)
        do i=1,outlin1
          if(bitmask_base(i:i).eq.'X')bitmask_base(i:i)='x'
          if(bitmask_base(i:i).eq.'&')bitmask_base(i:i)=' '
        enddo
        
        write(ounit,*)bitmask_base(:outlin1)
        close (ounit)
        
c      endif

      
999   continue

      call ftfiou(iunit,status)
      if(status.ne.0)then
        call fcecho('Error freeing input unit number')
        status=0
      endif

      call ftfiou(ounit,status)
      if(status.ne.0)then
        call fcecho('Error freeing output unit number')
        status=0
      endif
  
      return
      end

      subroutine scnremstr(string,cbegin,cend,
     &   strdelete,ldelete,ldryrun,lfound,abort)
      character*(*) string, strdelete, cbegin, cend
      character(80) comment
      character(1) blank
      integer fcstln,outlin,j,k,jsize,l,m,
     &   ibegsize,iendsize
      logical abort,ldryrun,lfound,ldelete

      lfound = .FALSE.
      ibegsize=fcstln(cbegin)
      iendsize=fcstln(cend)

      strdelete=' '
      k=0
      comment=' '
      blank=' '
      abort=.FALSE.
      outlin=fcstln(string)

c      call fcecho('In scnremstr with a string:')
c      call fcecho(string(:outlin))
      
      jsize=0
      
      do l=1,outlin
        jsize=jsize+1
        
        if(string(jsize:jsize+1).eq.cbegin)then
          lfound = .TRUE.
          jsize=jsize+2

          j=jsize-1

          do m=jsize,outlin
            j=j+1

            if(string(j:j+1).ne.cend)then
              k=k+1
              strdelete(k:k)=string(j:j)
              if(ldelete)string(j:j)=blank
            else
              j=outlin+1
            endif
            
            if(j.gt.outlin)goto 10
            
          enddo
          
10        continue
          
        endif

        if(jsize.gt.outlin)goto 20
        
      enddo

20    continue
      
      outlin=fcstln(strdelete)
c      print*,'outlin is',outlin
      outlin=k
c      print*,'strdelete is ',outlin

      if(ldryrun.and.lfound)then
        if(strdelete.ne.blank)then
          call fcecho(blank)
          comment='Value contained in             was:'
          comment(20:20+ibegsize)=cbegin(:ibegsize)
          comment(21+ibegsize:21+ibegsize+iendsize)
     &       =cend(:iendsize)
          call fcecho(comment)
          call fcecho(strdelete(:outlin))
        endif
      endif
      
      return
      end
