      subroutine find53(stmpp,etmpp,ntmp,efnd,sg,jlo,lun11,lpri)
c
c     this routine finds the continuum bin index.
c     author T. Kallman
c
      implicit none
c
      integer ntmp
      real*8 stmpp(ntmp),etmpp(ntmp)
      integer jlo,lun11,lpri
      real*8 efnd,sg
      integer ml2,mlp
      real*8 del1,del2,alg1,alg2,algtmp
c
c      lpri=0
c
c      if ((efnd.ge.etmpp(1)).and.(efnd.le.etmpp(ntmp))) then
      if (lpri.ne.0) write (lun11,*)'in find53:',efnd,ntmp,
     $    etmpp(1),etmpp(ntmp),stmpp(1),stmpp(ntmp)
      if ((efnd.ge.0.).and.(efnd.le.etmpp(ntmp))) then
        call hunt3(etmpp,ntmp,efnd,jlo,0,lun11)
        ml2=max(jlo,1)
        ml2=min(ml2,ntmp-1)
        mlp=ml2+1
        if (mlp.eq.ntmp) then
            alg1=log(max(stmpp(mlp),1.e-26)/max(stmpp(ml2),1.e-26))
            alg2=log(max(etmpp(mlp),1.e-26)/max(etmpp(ml2),1.e-26))
            algtmp=alg1/alg2
            sg=stmpp(ml2)*(efnd/etmpp(ml2))**algtmp
          else
            del1=(efnd-etmpp(ml2))/(etmpp(mlp)-etmpp(ml2))
            del2=(efnd-etmpp(mlp))/(etmpp(mlp)-etmpp(ml2))
            sg=-stmpp(ml2)*del2+stmpp(mlp)*del1
          endif
         sg=max(0.,sg)
         if (lpri.ne.0)
     $     write (lun11,*)sg,ml2,stmpp(ml2),stmpp(mlp),
     $           del1,del2,efnd,etmpp(ml2)
         else
              sg=0.
         endif
c
      return
      end
