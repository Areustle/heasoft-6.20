      
      subroutine bitmasksort(ibitmask,icompare,
     &   iskipfirst,iopval,iskipend,iopnum,
     &   bitmask,columns,abort)
        implicit none

      integer isiz
      parameter (isiz=100)
      character*(*) bitmask(5*isiz),columns
      character(80) tempmask1, tempmask2,cols
      integer ibitmask,icompare(*),iskipfirst(*),
     &   iopval(*),iskipend(*),iopnum(*),status,i,j,k,
     &   outlin, fcstln, outcomp, icolfound, istart,
     &   iend,isize
      logical abort,colfound,lallxs
        
      status=0
      i=0
      j=0
      istart=0
      iend=0
      icolfound=0
      outcomp = fcstln(columns)
      tempmask1 = ' '
      tempmask2 = ' '
      cols=' '
      cols=columns
      call ftupch(cols)

c     ICOMPARE will contain a value which tells what type of comparison
c the is being performed in each part of the bitmask.
c 1 = ==   equals
c 2 = <    less than
c 3 = >    greater than
c 4 = <=   less than or equal to
c 5 = >=   greater than or equal to

      call fcecho(' ')
      call fcecho('Original Bit_Mask is:')
      
      do i=1,ibitmask
        icompare(i)=0
        iskipfirst(i)=0
        iskipend(i)=0
        icompare(i)=0
        iopval(i)=0
        iopnum(i)=0
        colfound=.FALSE.
        lallxs=.TRUE.
        tempmask1 = ' '
        tempmask1 = bitmask(i)
        call ftupch(tempmask1)
        outlin=fcstln(tempmask1)

c       Let's check the bitmask for characters that would lead one to
c believe that they may have something in them that isn't supported
c by the dumb filtering in SEEXTRCT. If that is the case then print a
c warning message and continue processing without any filtering.
        do j = 1,outlin

c         Let's check to see if the entire expression is X's, i.e.,
c let's look for 1's or 0's and if any are found then set the logical
c l"ALL_X's" to FALSE since something other than X's were found. 
          if((tempmask1(j:j).eq.'1').or.
     &       (tempmask1(j:j).eq.'0'))lallxs=.FALSE.

c         Let's look for any OR symbols that are in this file, i.e., let's
c check to see if the user attempted to input foolish things. As they
c tend to do... 
          if(tempmask1(j:j).eq.'|')then
            abort = .TRUE.
            call fcecho(' ')
            call fcecho('ERROR! ')
            call fcecho('Bitmask file contains OR symbol.')
            call fcecho('SEEXTRCT cannot handle OR comparisons!')
            call fcecho('Use this bitmask file in FSELECT to filter')
            call fcecho('data first. Aborting... ')
            goto 999
          endif

c         Let's look for a heirarchy symbol - i.e., a ( or ) just to see
c if a user attempted to creat this bitmask and it is wrong... 
          if(tempmask1(j:j).eq.'('.or.
     &       tempmask1(j:j).eq.')')then
            abort = .TRUE.
            call fcecho(' ')
            call fcecho('ERROR!')
            call fcecho('Bitmask file contains heirarchy symbol ( ).')
            call fcecho('SEEXTRCT cannot handle such comparisons!')
            call fcecho('Use this bitmask file in FSELECT to filter')
            call fcecho('data first. Aborting... ')
            goto 999
          endif

        enddo

        if(lallxs)then
          call fcecho(' ')
          call fcecho('This expression contains no useful ')
          call fcecho('information, it is all Xs so that everything')
          call fcecho('is passed through. But this will slow the')
          call fcecho('code down tremendously.')
          call fcecho(' ')
          call fcecho('Continuing...')
        endif

        j=0
        do k = 1,outlin
          j=j+1
          
          if(tempmask1(j:j+outcomp).eq.cols)then
            colfound=.TRUE.
            icolfound=j+outcomp
            j=icolfound
          endif

          if(tempmask1(j:j+1).eq.'==')then
            icompare(i)=1
            j=j+2
          endif
          
          if(tempmask1(j:j+1).eq.'<=')then
            icompare(i)=4
            j=j+2
          endif

          if(tempmask1(j:j+1).eq.'>=')then
            icompare(i)=5
            j=j+2
          endif

          if(icompare(i).eq.0.and.tempmask1(j:j).eq.'<')then
            icompare(i)=2
            j=j+1
          endif

          if(icompare(i).eq.0.and.tempmask1(j:j).eq.'>')then
            icompare(i)=3
            j=j+1
          endif

          if(tempmask1(j:j).eq.'B')istart=j+1
          
          if(tempmask1(j:j).eq.'&')then
            iend=j-1
            goto 2
          endif

        enddo

3       continue
        
        outlin=fcstln(tempmask1)
        if(iend.eq.0)iend=outlin

2       continue

        tempmask2 = tempmask1(istart:iend)

        call fcecho(tempmask1(istart:iend))

        bitmask(i)=' '
        bitmask(i)=tempmask2
        tempmask1=tempmask2

      enddo

c     Since this subroutine was written under the assumption that all
c     bitmasks would have clustered values and would not be broken into
c     several clusters we have to make a change which will split the
c     bitmasks up into clustered values. And then procede.

c     The following subroutine will do this.
      call bitmaskexpand(ibitmask,bitmask,icompare)

      call fcecho(' ')
      call fcecho('Separated Bit_Mask is:')

      do i=1,ibitmask
        tempmask2=bitmask(i)
        tempmask1=tempmask2

        call fcecho(tempmask1)
        
c     Okay, now we have a bit pattern that is all upper X's and 0's and 1's
        outlin=fcstln(tempmask2)

c     
        iskipfirst(i)=0
        iopval(i)=0
        iskipend(i)=0
        iopnum(i)=0

        do j = 1,outlin
          if(tempmask1(j:j).eq.'X'.and.iopval(i).eq.0)then
            iskipfirst(i)=iskipfirst(i)+1
          elseif(iopval(i).ne.0.and.
     &         (tempmask1(j:j).ne.'1'.and.
     &          tempmask1(j:j).ne.'0'))then
            iskipend(i)=iskipend(i)+1
          endif
          
          if(tempmask1(j:j).eq.'1'.or.tempmask1(j:j).eq.'0')then
            iopval(i)=iopval(i)+1
          endif
        enddo

        isize=2**(iopval(i)-1)
        if(isize.eq.0)isize=1
        
        do j=1,iopval(i)
          if(tempmask1(iskipfirst(i)+j:iskipfirst(i)+j).eq.'1')
     &       iopnum(i)=iopnum(i)+isize
          isize=isize/2
        enddo

        if(icompare(i).eq.2)then
          icompare(i)=4
          iopnum(i)=iopnum(i)-1
        endif

        if(icompare(i).eq.3)then
          icompare(i)=5
          iopnum(i)=iopnum(i)+1
        endif

      enddo
      
999   continue
      
      return
      end


      subroutine bitmaskexpand(ibitmask,bitmask,icompare)
      implicit none
      integer isiz
      parameter (isiz=100)
      character*(*) bitmask(5*isiz)
      integer icompare(*)
      character(80) tempbitmask(5*isiz), tempmask,
     &   tempmask2, tempmask_start
      integer ibitmask, itotbitmask,
     &   fcstln, outlin, outlin2, itempcompare(5*isiz)
      integer i,j,ilastx
      logical zoro
      tempmask=' '

      itotbitmask=0
      zoro=.FALSE.
      tempmask=' '
      tempmask2=' '
      tempmask_start=' '
      tempmask=bitmask(1)
      outlin=fcstln(tempmask)
      
c     Let's initialize all possible bitmasks initially to X's
      do i=1,outlin
        tempmask2(i:i)='X'
      enddo

      do i=1,5*isiz
        tempbitmask(i)=' '
        tempbitmask(i)=tempmask2
      enddo
      
      
c     Okay, now we are ready to actually start processing the bitmask
      do i=1,ibitmask
        tempmask=' '
        tempmask=bitmask(i)

c     Let's find out how long this bitmask is.
        outlin2=fcstln(tempmask)

        if(outlin.ne.outlin2)then
          call fcecho(' ')
          call fcecho('******ERROR******')
          call fcecho('You have bitmasks of different lengths!')
          call fcecho('Cannot continue!!! Aborting!!!!')
          stop
        endif

        ilastx=0
        tempmask_start=tempmask2
        
        do j=1,outlin2
          if(tempmask(j:j).ne.'X')then
            zoro=.TRUE.
c            print*,'TEMPMASK(J:J) is ',tempmask(j:j),ilastx,j
            if((ilastx+1).eq.j)then
              tempmask_start(j:j)=tempmask(j:j)
              ilastx=j
c              print*,'tempmask_start is ',tempmask_start
            else
              tempmask_start(j:j)=tempmask(j:j)
              ilastx=j
            endif
          else
            if(zoro)then
              itotbitmask=itotbitmask+1
              itempcompare(itotbitmask)=icompare(i)
              tempbitmask(itotbitmask)=tempmask_start
              tempmask_start=tempmask2
              zoro=.FALSE.
            endif
            zoro=.FALSE.
            
          endif

          if(j.eq.outlin2.and.zoro)then
            itotbitmask=itotbitmask+1
            itempcompare(itotbitmask)=icompare(i)
            tempbitmask(itotbitmask)=tempmask_start
          endif

        enddo

      enddo

      ibitmask=itotbitmask

      do i=1,ibitmask
        icompare(i)=itempcompare(i)
        bitmask(i)=tempbitmask(i)
      enddo

      return
     
      end
      
