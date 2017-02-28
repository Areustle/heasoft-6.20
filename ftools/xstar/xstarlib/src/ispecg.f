      subroutine ispecg(eptmp,zrtmp,nret,epi,ncn2,zremsz,xlum,
     $                  lpri,lun11)
c
c     this subroutine generates the initial spectrum.
c     brems stores the flux to be used
c     generic renormalization
c     author:  T. Kallman
c
      implicit none
c
      include './PARAM'
c
      integer nret
      real*8 zremsz(ncn),epi(ncn)
      integer ncn2,lpri,lun11
      real*8 ergsev,const,xlum
      real*8 sum,tmp,tmpo, exp10
      integer numcon
      integer jlo,kl
      real*8 zremsi(ncn),eptmp(nret),zrtmp(nret)
      real*8 x,epmx,epmn,zr1,zr2,ep1,ep2,alx,aly,y
      integer jk
c
      data ergsev/1.602197e-12/
c
c        linear interpolation in log
      jlo = 0
c     if (lpri.ge.1) write (lun11,*)'in ispecg:',nret
c     if ( lpri.gt.2 ) write (lun11,*) (ll,eptmp(ll),zrtmp(ll),ll=1,nret)
      numcon=ncn2
      do kl = 1,numcon
         x = epi(kl)
         zremsi(kl) = 0.
         epmx = max(eptmp(1),eptmp(nret))
         epmn = min(eptmp(1),eptmp(nret))
         if ( lpri.gt.2 ) write (lun11,*) kl,x,epmx,epmn
         if ( (x.le.epmx) .and. (x.ge.epmn) ) then
            call hunt3(eptmp,nret,x,jlo,lpri,lun11)
            jlo = max0(jlo,1)
            zr1 = log10(max(zrtmp(jlo+1),1.e-24))
            zr2 = log10(max(zrtmp(jlo),1.e-24))
            ep1 = log10(max(eptmp(jlo+1),1.e-24))
            ep2 = log10(max(eptmp(jlo),1.e-24))
            alx = log10(x)
            alx = max(alx,ep2)
            alx = min(alx,ep1)
            aly = (zr1-zr2)*(alx-ep2)/(ep1-ep2+1.e-24) + zr2
            y = exp10(aly)
            zremsi(kl) = y
            if ( lpri.gt.2 ) write (lun11,*) kl,x,jlo,zr1,zr2,
     &                              ep1,ep2,y
         endif
         enddo
c
      sum = 0.
      tmp = zremsi(1)
      if ( lpri.gt.2 ) write (lun11,*) ' in ispecg'
      do jk = 2,ncn2
         tmpo = tmp
         tmp = zremsi(jk)
         if ( lpri.gt.2 ) write (lun11,*) jk,epi(jk),tmp,tmpo,sum
         if ( (epi(jk).ge.13.6) .and. (epi(jk).le.1.36e+4) ) then
            sum = sum + (tmp+tmpo)*(epi(jk)-epi(jk-1))/2.
            endif
         enddo
      sum = sum*ergsev
      const = xlum/sum
      do jk = 1,ncn2
         if ( lpri.gt.2 ) write (lun11,*) jk,epi(jk),zremsz(jk),
     &                                zremsi(jk)
         zremsz(jk) = zremsz(jk) + zremsi(jk)*const
         enddo
c
      return
      end
