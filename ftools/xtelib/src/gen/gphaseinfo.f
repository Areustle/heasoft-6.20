
c**********************************************************************
c      This subroutine takes as input phase interval information and
c      process that information and store the start and stop times
c      at the end of TMJDS and TMJDE and the number of such intervals
c      in iphaseno

      subroutine gphaseinfo(timemin,timemax,mjdref,imjdref,
     &   ephem,period,
     &   phaseint,binsz,phasefil,tmjds,tmjde,itimnoall,
     &   ipermno,iphaseno)
      implicit none

      integer nb,isiz,nd
      parameter (nd=128)
      parameter (nb=512)
      parameter (isiz=999)
      double precision timemin,timemax,mjdref,ephem,period,
     &     binsz,phases(nd),phasee(nd),tmjds(*),tmjde(*)
      character*(*) phaseint,phasefil
      character(80) contxt
      character(40) files(isiz)
      integer iphaseno,i,no,status,itimnoall,ipermno,imjdref
      logical negflag,lphase

      common/genphase/lphase
      
      status=0
      lphase=.FALSE.
      no=0
      
c      Call the subroutine that will parce the input string of
c      phase intervals that are to be included in the calculations.

c      Note that if a phase interval is supplied in the par-file it
c      takes precedence over all information supplied in a phasefile.

      if(phaseint.ne.' '.and.phaseint.ne.'INDEF')then
        lphase=.TRUE.
        call str2real(phaseint,1,iphaseno,phases,phasee)

c        print*,'phaseint,iphaseno,phases,phasee is'
c        print*,phaseint,iphaseno,(phases(i),i=1,iphaseno),
c     &     (phasee(i),i=1,iphaseno)

c       Since there is phase information but we cannot include it in
c the GTIs due to the number of GTIs that would be generated, we will
c have to write out a file which will contain this information in a
c format that Xronos can understand.
        call fcecho(' ')
        call fcecho('Phase filtering can cause problems!!!')
        call fcecho('We are working on this. The problem is')
        call fcecho('the excessive number of GTIs that may be')
        call fcecho('created. This will be fixed in the next')
        call fcecho('version. Output DATA is correct.')
c        call fcecho('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%')
c        call fcecho('Writing out a PHASE_FILE containing')
c        call fcecho('phase selection criteria specified.')
c        call fcecho('This information will NOT be contained')
c        call fcecho('in the GTIs since it would be too large')
c        call fcecho('and cause downstream software to die.')
c        call fcecho('The user must use this PHASE_FILE for')
c        call fcecho('Xronos filtering, no farther warnings')
c        call fcecho('will be given.')
c        call wrtphasefile(ephem,period,iphaseno,phases,phasee)
        goto 150
      else
        iphaseno=1
        phases(1)=0.0d0
        phasee(1)=1.0d0
      endif

c      if(.not.lphase.and.phasefil.ne.' '.and.phasefil.ne.'INDEF')then
        
C --- Check to see if the input phasefil file is a single file or
C --- if it is a file which contains filenames, i.e., if it has
C --- "@" in the first character position which denotes that the value
C --- contained in INFILE denotes a filename containing filenames.
c --- If there is more than 1 phasefil specified this is an error!

        status = 0
      
C find the file(s) in the first input string
        if (.not.lphase.and.(phasefil .ne. ' ').
     &     and.(phasefil.ne.'INDEF')) then

          call fcecho(' ')
          call fcecho('Phase filtering can cause problems!!!')
          call fcecho('We are working on this.')
          
          lphase=.TRUE.
          call fcgcls (phasefil, files, no, negflag)
c          print*,'files read in are ',i,no,(files(i),i=1,no)
          call rphasefile(files(1),mjdref,imjdref,ephem,period,
     &       iphaseno,phases,phasee)
c          print*,'iphaseno out is ',iphaseno
c          print*,'ephem and period are',ephem,period
c          print*,'phases(i)',(phases(i),i=1,iphaseno)
c          print*,'phasee(i)',(phasee(i),i=1,iphaseno)          

          if(no.ne.1)then
            contxt='Error only one PHASEFILE can be processed'
            call fcecho(contxt)
            contxt='Proceeding using the first one only!'
            call fcecho(contxt)
        else
          no = 0
        endif

      endif
      
150   continue
        
c      print*,'no is ', no
        
      if (no .gt. 100) then
        contxt = ' Only 1 (one) phasefil can be processed.'
        call fcecho (contxt)
        no = 1
      else if (no .le. 0) then
        no=0
      endif

      do 100 i=1,iphaseno
        tmjds(itimnoall+ipermno+i)=phases(i)
        tmjde(itimnoall+ipermno+i)=phasee(i)
c        print*,'phaseint,iphaseno,phase,phasee is'
c        print*,phaseint,iphaseno,phases(i),phasee(i)
c        print*,'stored in the ...  value'
c        print*,itimnoall+ipermno+i
c        print*,tmjds(itimnoall+ipermno+i),tmjde(itimnoall+ipermno+i)
        
100   continue
      
c      print*,'ephem and period is ',ephem,period,timemin

      return
      end
