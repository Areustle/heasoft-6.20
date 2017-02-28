      subroutine msolvelucy(ajisb,cjisb,indb,nindb,nsup,nspmx,
     $   ipmat,bmat,x,ht,cl,niter,nit2,nit3,nitmx,nitmx2,lun11,lpri)
c
c     solves lucy iteration
c     author:  T. Kallman
c
      implicit none
c
c     solves lucy iteration
c     author:  T. Kallman
c
      include './PARAM'
c
c
      real*8 ajisb(2,ndb),cjisb(ndb)
      integer indb(2,ndb)
      real*8 x(nd),bmat(nd),xo(nd),xoo(nd)
      real*8 bmatsup(ndss),ajissup(ndss,ndss),xsup(ndss),rr(nd)
      real*8 cjissup(ndss,ndss)
      real*8 wkarea(1)
      real*8 tt1, crit, crit2, diff, tt2, diff2
      real*8 riu(nds),ril(nds),rui(nds),rli(nds),xsum,tst,cl
      real*8 ht, clp, htp
      integer nsup(nd), mm, ipmat, ll
      integer lpril, lpri, lprisv, idgt, ier
      integer lun11, niter, nit3, nitmx, nspmx
      integer nn, nsp, ngood, nspm, nspn, nspcon
      integer nit2, nitmx2, m2, nindb
c
c
c       step thru levels, and form calculate superlevel quantities
      lprisv=lpri
      call remtms(tt1)
c      lpri=0
      if (lpri.gt.1)
     $ write (lun11,*)'in msolvelucy',lpri,nindb
c      crit=1.e-2
c      crit2=1.e-2
      crit=1.e-4
      crit2=1.e-4
      diff=1.
      niter=0
      nit3=0
      do while ((diff.gt.crit).and.(niter.lt.nitmx))
        niter=niter+1
        if (lpri.gt.1) write (lun11,*)'iteration=',niter
        if (lpri.gt.1) write (lun11,*)'initial populations:'
        do mm=1,ipmat
          xo(mm)=x(mm)
          if (lpri.gt.3) write (lun11,*)mm,x(mm),nsup(mm)
          enddo
        do mm=1,nspmx
          xsup(mm)=0.
          bmatsup(mm)=0.
          do nn=1,nspmx
            ajissup(mm,nn)=0.
            cjissup(mm,nn)=0.
            enddo
          enddo
        do mm=1,ipmat
          nsp=nsup(mm)
          xsup(nsp)=xsup(nsp)+x(mm)          
          enddo
        call remtms(tt2)        
        if (lpri.gt.1) 
     $    write (lun11,*)'before constucting matrix',abs(tt2-tt1)
        tt1=tt2          
        if (lpri.gt.1) 
     $      write (lun11,*)'constucting the condensed matirx:'
        ngood=0
        do mm=1,ipmat
          nspm=nsup(mm)
          rr(mm)=x(mm)/(1.e-36+xsup(nspm))
          if (xsup(nspm).le.1.e-36) rr(mm)=1.
          enddo
        do ll=1,nindb
          mm=min(ipmat,indb(1,ll))
          nn=min(ipmat,indb(2,ll))
          nspm=nsup(mm)        
          nspn=nsup(nn)          
          if ((nspn.ne.nspm)
     $         .and.(nspn.ne.0).and.(nspm.ne.0)
     $         .and.((abs(rr(mm)).gt.1.e-36)
     $           .or.(abs(rr(nn)).gt.1.e-36))
     $         .and.((abs(ajisb(1,ll)).gt.1.e-36)
     $           .or.(abs(ajisb(2,ll)).gt.1.e-36))) then
              ajissup(nspm,nspn)=ajissup(nspm,nspn)
     $            +(ajisb(1,ll))*rr(nn)
              ajissup(nspm,nspm)=ajissup(nspm,nspm)
     $            -(ajisb(2,ll))*rr(mm)
c              ajissup(nspn,nspm)=ajissup(nspn,nspm)
c     $            +(ajisb(2,ll))*rr(nn)
c              ajissup(nspn,nspn)=ajissup(nspn,nspn)
c     $            -(ajisb(1,ll))*rr(mm)
              cjissup(nspm,nspn)=cjissup(nspm,nspn)
     $            +(cjisb(ll))*rr(mm)
              ngood=ngood+1
              if (lpri.gt.3) write (lun11,91)ll,mm,nn,nspm,nspn,
     $              rr(mm),rr(nn),ajisb(1,ll),ajisb(2,ll),
     $              ajissup(nspm,nspn),ajissup(nspm,nspm)
91            format (1x,'used ',5i6,6(1pe13.5))
            endif
          enddo
        call remtms(tt2)
        if (lpri.gt.1) 
     $     write (lun11,*)'after constucting matrix',abs(tt2-tt1),
     $                       ngood
        if (lpri.gt.1) then
          if (lpri.gt.1) write (lun11,*)'the condensed populations:'
          do nsp=1,nspmx
            write (lun11,*)nsp,xsup(nsp)
            enddo
          if (lpri.gt.1) write (lun11,*)'the condensed matrix:'
          do nspm=1,nspmx
            do nspn=1,nspmx
              if (abs(ajissup(nspm,nspn)).gt.1.e-37)
     $         write (lun11,*)nspm,nspn,ajissup(nspm,nspn)
              enddo
            enddo
          endif
c        put in number conservation
c         nspcon=1
         nspcon=nspmx
         do mm=1,nspmx
           ajissup(nspcon,mm)=1.
           bmatsup(mm)=0.
           enddo
        bmatsup(nspcon)=1.
        lpril=0
        call remtms(tt1)
        if (lpri.gt.2) 
     $    write (lun11,*)'before leqt',abs(tt2-tt1)
        call leqt2f(ajissup,1,nspmx,ndss,bmatsup,idgt,wkarea,ier,
     $                      lun11,lpril)
         call remtms(tt2)
         if (lpri.gt.2) 
     $    write (lun11,*)'after leqt',abs(tt2-tt1)
        if (lpri.gt.2) write (lun11,*)'the new condensed populations:'
        do mm=1,nspmx
          xsup(mm)=bmatsup(mm)
          if (lpri.gt.2) write (lun11,*)mm,xsup(mm)
          enddo
        if (lpri.gt.3) write (lun11,*)'new populations'
        do mm=1,ipmat
          nsp=nsup(mm)
          x(mm)=rr(mm)*xsup(nsp)
          if (lpri.gt.3) write (lun11,*)mm,nsp,rr(mm),x(mm)
          enddo
        nit2=0
        diff2=10.
        do while ((nit2.lt.nitmx2).and.(diff2.ge.crit2))
          nit2=nit2+1
          nit3=nit3+1
          if (lpri.gt.2) write (lun11,*)'before calculate new x(mm)',
     $                                   nit2,nit3
          call remtms(tt2)
          if (lpri.gt.2) 
     $    write (lun11,*)'in diff2 loop',abs(tt2-tt1)
          tt1=tt2
          do mm=1,ipmat
            riu(mm)=0.
            rui(mm)=0.
            ril(mm)=0.
            rli(mm)=0.
            enddo
          if (lpri.gt.3) write (lun11,*)'the riu calculation'
          do ll=1,nindb              
            mm=indb(1,ll)
            nn=min(ipmat,indb(2,ll))
            if (nn.gt.mm) then
                riu(mm)=riu(mm)+abs(ajisb(2,ll))
                rui(mm)=rui(mm)+abs(ajisb(1,ll))*x(nn)
                if (lpri.gt.3) write (lun11,*)mm,nn,ajisb(2,ll),
     $           ajisb(1,ll),x(nn),riu(mm),rui(mm)
              endif
            enddo
          if (lpri.gt.3) write (lun11,*)'the ril calculation'
          do ll=1,nindb              
            mm=indb(1,ll)
            nn=min(ipmat,indb(2,ll))
            if (nn.lt.mm) then
c               I hope the indeces are in the right order here
                ril(mm)=ril(mm)+abs(ajisb(2,ll))
                rli(mm)=rli(mm)+abs(ajisb(1,ll))*x(nn)
                if (lpri.gt.3) write (lun11,*)mm,nn,ajisb(2,ll),
     $           ajisb(1,ll),x(nn),ril(mm),rli(mm)
              endif
            enddo
          do mm=1,ipmat
            xoo(mm)=x(mm)
            x(mm)=(rli(mm)+rui(mm))/(ril(mm)+riu(mm)+1.e-24)
            if (lpri.gt.3) write (lun11,*)mm,riu(mm),rui(mm),ril(mm),
     $                                     rli(mm),x(mm),xoo(mm)
            enddo
          xsum=0.
          do mm=1,ipmat
            xsum=xsum+x(mm)
            enddo
          if (lpri.gt.3) write (lun11,*)'new and old populations',
     $                      xsum
          do mm=1,ipmat
            x(mm)=x(mm)/(1.e-24+xsum)
            enddo
          m2=1
          diff2=0.
          tst=0.
          do while ((diff2.lt.1.e+3)
     $           .and.(m2.le.ipmat).and.(tst.lt.1.e+3))
            if (lpri.gt.3) write (lun11,*)m2,x(m2),xoo(m2),xo(m2),
     $            diff2
            tst=1.  
            if (x(m2).gt.1.e-22) tst=xoo(m2)/x(m2)
            diff2=diff2+(tst-1.)*(tst-1.)
            m2=m2+1
            enddo
          if (lpri.gt.2) write (lun11,*) 'diff2=',diff2,nit2
          enddo
        diff=0.
        m2=1
        do while ((m2.le.ipmat).and.(diff.lt.1.e+3))
          tst=xo(m2)*(1.e-30)
          if ((x(m2).gt.tst).and.(diff.lt.1.e+10).and.(x(m2).gt.1.e-35))
     $     diff=diff+(min(1.e+10,(xo(m2)-x(m2))/(xo(m2)+x(m2))))**2
          if (lpri.gt.3) write (lun11,*)m2,x(m2),xo(m2),
     $            diff
          m2=m2+1
          enddo
        if (lpri.gt.2) write (lun11,*) 'diff=',diff
      enddo
c
      if (lpri.ge.1) write (lun11,*)'heating-cooling in msolvelucy:'
      cl=0.
      ht=0.
      do ll=1,nindb
        mm=min(indb(1,ll),ipmat)
        nn=min(indb(2,ll),ipmat)
        if (cjisb(ll).gt.0.) then
              cl=cl+x(mm)*cjisb(ll)
            else
              ht=ht-x(mm)*cjisb(ll)
            endif
          if ((lpri.ge.1).and.(abs(cjisb(ll)).gt.1.e-24))
     $         write (lun11,981)ll,mm,nn,x(mm),cjisb(ll),ht,cl
 981           format (1x,3i6,4(1pe11.3))
        enddo
      go to 9090
      if (lpri.gt.2) write (lun11,*)'heating-cooling superlevels:'
      clp=0.
      htp=0.
      do mm=1,nspmx
        do nn=1,nspmx
          if (cjissup(mm,nn).gt.0.) then
              clp=clp+xsup(mm)*cjissup(mm,nn)
            else
              htp=htp-xsup(mm)*cjissup(mm,nn)
            endif
          if ((lpri.gt.2).and.(abs(cjissup(mm,nn)).gt.1.e-24))
     $         write (lun11,*)mm,nn,xsup(mm),cjissup(mm,nn),htp,clp
          enddo
        enddo
      ht=htp
      cl=clp
 9090 continue
c
      lpri=lprisv
c
      return
      end
