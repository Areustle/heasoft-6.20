      
      subroutine wrtupdate2scr(cols,devents,dgood,
     &   dtim,dphase,dpha,dbitmask,inocols)
c      implicit none

      double precision devents(*),dgood(*),
     &   dtim(*),dphase(*),dpha(*),dbitmask(*)
      integer inocols
      character(80) cols(*)
      character(90) charall,contxt
      character(20) cval
      integer ic,status

      status=0
      charall=' '
      cval=' '

      do 10 ic=1,inocols

        charall(1:13)=cols(ic)

        call ftd2f(devents(ic),2,cval,status)
        if(status.ne.0)then
          contxt='Error in double devents to characters'
          call fcecho(contxt)
          status=0
        endif

        charall(15:24)=cval(10:20)
        
        cval=' '
        
        call ftd2f(dgood(ic),2,cval,status)
        if(status.ne.0)then
          contxt='Error in double dgood to characters'
          call fcecho(contxt)
          status=0
        endif

        charall(26:35)=cval(10:20)

        cval=' '
        call ftd2f(dtim(ic),2,cval,status)
        if(status.ne.0)then
          contxt='Error in double dtim to characters'
          call fcecho(contxt)
          status=0
        endif

        charall(37:46)=cval(10:20)
        
        cval=' '
        call ftd2f(dphase(ic),2,cval,status)
        if(status.ne.0)then
          contxt='Error in double dphase to characters'
          call fcecho(contxt)
          status=0
        endif

        charall(48:57)=cval(10:20)
        
        cval=' '
        call ftd2f(dpha(ic),2,cval,status)
        if(status.ne.0)then
          contxt='Error in double dpha to characters'
          call fcecho(contxt)
          status=0
        endif

        charall(59:68)=cval(10:20)

        if(dbitmask(ic).gt.0.0d0)then
          
        cval=' '
        call ftd2f(dbitmask(ic),2,cval,status)
        if(status.ne.0)then
          contxt='Error in double dbitmask to characters'
          call fcecho(contxt)
          status=0
        endif

        charall(70:79)=cval(10:20)

        endif
        
        call fcecho(' ')
        if(dbitmask(ic).eq.0.0d0)then
        contxt='Column         Total      Good   Bad :  Time       Phase
     &     Channel '
        else
        contxt='Column         Total      Good   Bad :  Time       Phase
     &     Channel    Bitmask'
        endif
      
        call fcecho(contxt)
        call fcecho(charall)

10    continue
      
        return
        end
