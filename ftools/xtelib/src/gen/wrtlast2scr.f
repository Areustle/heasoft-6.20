
      subroutine wrtlast2scr(cols,devents,dgood,dtim,dphase,dpha,
     &     inocols,accumulate,dthres,dlcinten,curvetype,totsecsa)
c      implicit none

      double precision devents(*),dgood(*),dtim(*),dphase(*),dpha(*),
     &   dlcinten(*),dthres(*)
      integer inocols,icols
      parameter (icols = 40)
      
      double precision deventstemp,dgoodtemp,dphasetemp,
     &   dphatemp,dthrestemp,dlcintentemp
      double precision totsecsa(*),sum
      character*(*) cols(icols),accumulate,curvetype
      character(90) charall,contxt
      character(20) cval
      integer ic,status,itempcols,i,ilen
      logical lmany

      status=0
      charall=' '
      ilen=21

      itempcols=inocols
      lmany=.FALSE.
      
      deventstemp=devents(1)
      dgoodtemp=dgood(1)
      dphasetemp=dphase(1)
      dphatemp=dpha(1)
      dthrestemp=dthres(1)
      dlcintentemp=dlcinten(1)
      
      if(accumulate.eq.'ONE'.and.inocols.gt.1)then
        do 20 i=2,inocols
          devents(1)=devents(1)+devents(i)
          dgood(1)=dgood(1)+dgood(i)
          dphase(1)=dphase(1)+dphase(i)
          dpha(1)=dpha(1)+dpha(i)
          dthres(1)=dthres(1)+dthres(i)
          dlcinten(1)=dlcinten(1)+dlcinten(i)
20      continue
        lmany=.TRUE.
        itempcols=1
      endif
      
      call fcecho(' ')
      call fcecho('^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^')
      charall='Total number of columns output is:'
      cval=' '
      call fti2c(itempcols,cval,status)
      if(status.ne.0)then
        contxt='Error in inocols to characters'
        call fcecho(contxt)
        status=0
      endif
      
      charall(39:58)=cval
      call fcecho(charall)
      
      do 10 ic=1,itempcols

        call fcecho(' ')
        
        call fcecho('------------------------------------------')
          charall=' '
          charall(1:36)='Column that is being output is:  '

          if(.not.lmany)then
            charall(37:56)=cols(ic)
          elseif(lmany)then
            charall(37:72)='Combination of many columns'
          else
            charall(37:70)='This should not happen.'
          endif
            
          call fcecho(charall)
          charall=' '
          
          charall(1:22)='Total Counts for FITS '
          charall(23:36)=curvetype(1:14)

          cval=' '
          call ftd2f(dgood(ic),2,cval,status)
          if(status.ne.0)then
            contxt='Error in integer dgood to characters'
            call fcecho(contxt)
            status=0
          endif
          
          charall(37:56)=cval(1:20)
          call fcecho(charall)
          charall=' '

          charall(1:20)='Total Time for FITS '
          charall(21:34)=curvetype(1:14)

          cval=' '
          call ftd2f(totsecsa(ic),6,cval,status)
          
          if(status.ne.0)then
            contxt='Error in double totsecs to characters'
            call fcecho(contxt)
            call fcerrm(status)
            status=0
          endif
          
          charall(43:60)=cval(3:20)
          call fcecho(charall)
          charall=' '

c         Here we test to see if any time has been accumulated
c if it hasn't then we print that the rate is not applicable and move
c on.
          if(totsecsa(ic).ne.0.0d0)then
            sum=(dgood(ic))/totsecsa(ic)

            cval=' '
            call ftd2f(sum,6,cval,status)
            if(status.ne.0)then
              contxt='Error in double SUM to characters'
              call fcecho(contxt)
              status=0
            endif

          else
            sum=0.0d0
            cval='    Not Applicable'
          endif
          
        
          charall='Total Counts/Time for FITS '
          charall(28:41)=curvetype(1:14)
          
          charall(42:60)=cval(2:20)
          call fcecho(charall)

          if(dthres(ic).gt.0)then
            if(itempcols.gt.1)then
              charall=
     &           'Total counts removed MFRACEXP:'
            else
              charall='Total counts removed by MFRACEXP:'
            endif

            cval=' '
            call ftd2f(dthres(ic),2,cval,status)
            if(status.ne.0)then
              contxt='Error in integer dtotpha to characters'
              call fcecho(contxt)
              status=0
            endif

            charall(39:58)=cval
            call fcecho(charall)

            call fcecho(' ')
            call fcecho('The number of counts removed by this filtering
     &is not reflected above!')
          
            charall='Set mfracexp=0.0 or INDEF to include these.'
            call fcecho(charall)
          endif

c          print*,'dlcinten(ic) is',ic,dlcinten(ic)
          if(dlcinten(ic).gt.0)then
            charall=
     &         'Total counts removed by MLC(SP)INTEN:'

            cval=' '
            call ftd2f(dlcinten(ic),2,cval,status)
            if(status.ne.0)then
              contxt='Error in integer dlcinten to characters'
              call fcecho(contxt)
              status=0
            endif

            charall(39:58)=cval
            call fcecho(charall)

            call fcecho(' ')
            call fcecho('The number of counts removed by this filtering
     &is not reflected above!')

            charall='Set MLC(SP)INTEN=INDEF to include this data!'
            call fcecho(charall)
          endif

10    continue

      devents(1)=deventstemp
      dgood(1)=dgoodtemp
      dphase(1)=dphasetemp
      dpha(1)=dphatemp
      dthres(1)=dthrestemp
      dlcinten(1)=dlcintentemp
      
      return
      end
