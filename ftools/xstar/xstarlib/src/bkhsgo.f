      subroutine bkhsgo(sg,et,d,b,na,a,epi,ncn2,t,lpri,lfast,lun11)
c
c     this routine does the work in computing cross sections by the
c     method of barfield, et. al.
c     author:  T. Kallman (from xstar1)
c
      implicit none
c
      include './PARAM'

      integer na
c
      real*8 sg(ncn),b(na),a(11,na),epi(ncn)
      integer lpri,lfast,lun11,ncn2,nbinc
      real*8 d,t
      integer lprisv,jj,nb1,j,i,nphint,lk,kk,nskp,lrcalc
      real*8 tmp,xx,yy,sgtmp,et,epii
c
      if (lpri.gt.1) write (lun11,*)'in bkhsgo:'
     $      ,na,b,t
      lprisv=lpri
c
      jj = 1
      yy=0.
      tmp=0.
      nb1=max(1,nbinc(et,epi,ncn2))
      do j=1,nb1
         sg(j)=0.
         enddo
      i=nb1
      do while ((i.le.nphint).and.(jj.le.na))
        epii = epi(i)
        if (lpri.gt.1) write (lun11,*)i,epii,et
        xx = epii*(1.e-3) - d
        if ( xx.gt.0. ) then
          if (lpri.gt.1) write (lun11,*)d,xx,jj
          if ( xx.ge.b(jj) ) jj = jj + 1
          xx = max(xx,0.)
          yy = log10(xx)
          tmp = 0.
          do  lk = 1,11
             kk = 12 - lk
             tmp = a(kk,jj) + yy*tmp
             if (lpri.gt.1)
     $                 write (lun11,*)lk,kk,yy,tmp,a(kk,jj)
             enddo
          tmp = min(max(-50.,tmp),24.)
          sgtmp = 10.**(tmp-24.)
          sg(i)=sgtmp
          if (lpri.gt.1)
     $             write (lun11,*)i,epii,xx,
     $               tmp,sgtmp
         endif
       call enxt(et,nb1,lpri,epi,ncn2,t,lfast,lun11,
     $                  i,nskp,nphint,lrcalc)
       i=i+nskp
       enddo
      if (i.lt.nphint) then
        do j=i,nphint
          sg(j)=0.
          enddo
        endif
c
      if (lpri.gt.1) write (lun11,*)'leaving bkhsgo'
      lpri=lprisv
c
      return
      end
