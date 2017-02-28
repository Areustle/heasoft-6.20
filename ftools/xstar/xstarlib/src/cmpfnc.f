      real*8 function cmpfnc(decomp,ecomp,sxcomp,ee,sxx,lun11,lpri)
c
c     this routine is used in the relativistic compton calculation
c
      implicit none
c
      include './PARAM'
c
      real*8 decomp(ncomp,ncomp),ecomp(ncomp),sxcomp(ncomp)
      real*8 ee,eetp,sxx,ddedsx,ddede,dele,delsx,sxtp
      integer lun11,lpri,mm,ll,mmm1,llm1,nc2
c
c     Not used
      integer javi

      javi=lpri
      lpri=javi
c
      eetp=ee
      sxtp=sxx
      nc2=ncomp
      cmpfnc=0.
      if (eetp.gt.1.e-4) then
        call hunt3(ecomp,nc2,eetp,mm,0,lun11)
        call hunt3(sxcomp,nc2,sxtp,ll,0,lun11)
c       if ((mm.gt.1).and.(ll.gt.1)) then
               mm=max(2,min(ncomp,mm))
               ll=max(2,min(ncomp,ll))
               mmm1 = mm - 1
               llm1 = ll - 1
               ddedsx = (decomp(ll,mm)-decomp(llm1,mm)
     $                  +decomp(ll,mmm1)-decomp(llm1,mmm1))
     $                     /(2.*(sxcomp(ll)-sxcomp(llm1)))
               ddede = (decomp(ll,mm)-decomp(ll,mmm1)
     $                  +decomp(llm1,mm)-decomp(llm1,mmm1))
     &                 /(2.*(ecomp(mm)-ecomp(mmm1)))
               dele = ee - ecomp(mmm1)
               delsx = sxx - sxcomp(llm1)
               cmpfnc = ddedsx*delsx + ddede*dele + decomp(llm1,mmm1)
        else
               cmpfnc = 4.*sxx - ee
        endif
c      if (lpri.ne.0)
c     $ write (lun11,*)'in cmpfnc',ee,sxx,ll,mm,cmpfnc
c
      return
      end
