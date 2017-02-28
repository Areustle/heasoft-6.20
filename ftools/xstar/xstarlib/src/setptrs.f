      subroutine setptrs(lun11,lpri,
     $ idat1,rdat1,kdat1,nptrs,np2,
     $ npnxt,npfi,npar,npfirst,nplin,
     $ nplini,npcon,npconi,npilev,npilevi,
     $ npconi2,nlevs,nlsvn,ncsvn,abcosmic,abel)
c
c     this program set the pointers of the database
c       Written by Ke Zhang, Oct.8 2001
c
c     data structures are:
c      data: the database arrays (integer, real, character)
c       idat1(nidat1)
c       rdat1(nrdat1),
c       kdat1(nkdat1)
c     descriptions of database entries, and pointers
c       nptrs(nptt,ndat2)
c         nptrs(2,nx)=data type
c         nptrs(3,nx)=rate type
c         nptrs(4,nx)=continuation flag
c                       (n=number of continuations to come)
c         nptrs(5,nx)=number of reals
c         nptrs(6,nx)=number of integers
c         nptrs(7,nx)=number of characters
c         nptrs(8,nx)=pointer to reals
c         nptrs(9,nx)=pointer to integers
c         nptrs(10,nx)=pointer to characters
c
c       pointers:
c       next record:
c         npnxt(ndat2)
c       parent record (=ion header or element header)
c         npar(ndat2)
c       first record of a given rate type
c         npfirst(ntyp)
c       first record of rate type ntyp for ion nni
c         npfi(ntyp,nni)
c       pointer for line data from array containing luminosities
c         nplin(nnnl)
c       (inverse) pointer for line data to array containing luminosities
c          from database array
c         nplini(ndat2)
c       pointer for continuum data (pi xsection) from array containing luminosities
c         npcon(nnml)
c       pointer to abundance array to first level of element nni
c         npconi2(ndat2)
c       (inverse) pointer for continuum data (pi xsection) from array containing
c           luminosities
c         npconi(ndat2)
c
c
      implicit none
      include './PARAM'
c
c     master data
      real*8 rdat1(nrdat1)
      real*8 abel(nl),abcosmic(30)
c
      integer idat1(nidat1)
      integer nptrs(nptt,ndat2)
      integer npnxt(ndat2),npar(ndat2)
      integer npfirst(ntyp)
      integer npnxt2(ndat2)
      integer npfi(ntyp,nni)
      integer nplin(nnnl),nplini(ndat2),npcon(nnml)
      integer npilev(nd,nni),npilevi(nnml)
      integer npconi2(ndat2)
      integer npconi(ndat2)
      integer nlevs(nni)
      integer melpt(nl)
      integer mlold(ntyp)
      integer indx, iion, ilev, icon, iline, i, j
      integer iel2, iel, lpri, lun11, np2, lrtp
      integer iilev, mltmpn, mlfnd, nclev, mltst
      integer mltmp, npartmpn, nlsvn, ncsvn
      integer lsrt, mml, niter, melptmp, npfirst2
      integer mllo, mlloo, itst, ltyp, lrtyp2, lcon
      integer nrdt, nidt, nkdt, mll,mlm
      integer itmp,mm,np1i,np1r,np1k,ntptmp
c
      character(1) kdat1(nkdat1)

c            pointer structure
c     type    desc         nr  ni  nk      daught  par
c     1       rr, a&p      2   1   0               14
c     2       hcx          4   1   0               14
c     3       ai           2   1   0               14
c     4       line dat 1   2   3   0        5      14
c     5       line dat 2   4   3   0                4
c     6       lev dat  1   4   3   0               14
c     7       dr a&p       5   1   0               14
c     8       dr a&r       0   0   0               14
c     9       hecx         4   1   0               14
c     10      lev dat 2    0   2  30                6
c     11      2 ph         2   2   0               14
c     12      pixc, bpl    5   2   0               14
c     13      el           2   2  30       14       0
c     14      ion          1   2   8       all     13
c     15      pixc bkh 1   5   1   0       20      14
c     16      pixc bkh     0   0   0               14
c     17      cx: cota     4   3   0               14
c     18      rr: cota     3   1   0               14
c     19      pixc hullac  0   0   0               14
c     20      pixc bkh 2   5   1   0       21      15
c     21      pixc bkh 3   4   4  11               20
c     22      dr stroey    5   1   0               14
c     23      pixc clark   5   2   0       24      14
c     24      pixc clark 2 4   4   0               23
c     25      ci r&s       0   0   0               14
c     26      ci cota      2   2   0               14
c
        indx=1
c
c the main data index
      go to 9009
      if (lpri.ne.0) then
c       first an experimental print
        write (lun11,*)'np2=',np2
        do itmp=1,np2
          CALL DRD(ltyp,lrtyp2,lcon,nrdt,np1r,nidt,np1i,nkdt,np1k,
     &          itmp-1,Nptrs,0,Lun11)
          write (lun11,*)'itmp=',itmp
c          write (lun11,*)'nkdt=',nkdt,(kdat1(np1k-1+mm),mm=1,nkdt)
          call dprinto(ltyp,lrtyp2,lcon,
     $    nrdt,np1r,nidt,np1i,nkdt,np1k,rdat1,idat1,kdat1,lun11)
          enddo
        endif
 9009   continue

c the ion index
      iion=1

c the level index
      ilev=1

c the continum index
      icon=1

c the line index
      iline=1

c initialize pointers

      do i=1,ntyp
        npfirst(i)=0
      enddo

      do i=1,nni
        do j=1,ntyp
          npfi(j,i)=0
        enddo
        nlevs(i)=0
      enddo

      do i=1,ndat2
        npnxt(i)=0
      enddo
c
      do i=1,nl
        abcosmic(i)=0.
        enddo

      mlold(11)=0
      iel2=0
      do while ((iel2.le.nl).and.(indx.lt.np2))
        iel2=iel2+1
        iel=iel2
c        if (abel(iel).lt.1.e-15) then
c
          if (lpri.ne.0)
     $     write (lun11,*)'iel=',iel2,iel,abel(iel)
c  pass by elements that has neglectable abundance
c          indx=indx+1
c          do while((nptrs(3,indx).ne.11).and.(indx.lt.np2))
c            indx=indx+1
c          enddo
c
c        else

c  register element record
c  npfirst,npnxt,npar,mlold

          if (npfirst(11).eq.0) then
            npfirst(11)=indx
          else
            npnxt(mlold(11))=indx
          endif
c
          CALL DRD(ltyp,lrtyp2,lcon,nrdt,np1r,nidt,np1i,nkdt,np1k,
     &          indx-1,Nptrs,0,Lun11)
          iel=idat1(np1i)
          abcosmic(iel)=rdat1(np1r)     

          if (lpri.ne.0)
     $     write (lun11,*)'registering element:',iel,abel(iel),indx,
     $                         mlold(11),abcosmic(iel)
          mlold(11)=indx
          npar(indx)=0
          indx=indx+1

c  go through ions

          ltyp=nptrs(2,indx)
          lrtp=nptrs(3,indx)
          if (lpri.ne.0)
     $     write (lun11,*)'lrtp=',lrtp,indx
          do while(lrtp.eq.12)

c
          if (lpri.ne.0)
     $     write(lun11,*) iel,iion

c  register ion record
c  npfirst,npnxt,npar,mlold

            if (npfirst(12).eq.0) then
              npfirst(12)=indx
            else
              npnxt(mlold(12))=indx
            endif
            if (lpri.ne.0)
     $       write (lun11,*)'npfirst(12)=',npfirst(12),indx
            mlold(12)=indx
            npar(indx)=mlold(11)
            indx=indx+1

c  level records, rate type 13
c  npfirst,npnxt,npar,mlold,npfi,npilev,npilevi

            if (nptrs(3,indx).eq.13) then
              npfi(13,iion)=indx
              iilev=1
              if (npfirst(13).eq.0) then
                npfirst(13)=indx
              else
                npnxt(mlold(13))=indx
              endif
              if (lpri.ne.0)
     $         write (lun11,*)'filling npilev:'
              do while(nptrs(3,indx).eq.13)
                npar(indx)=mlold(12)
                npnxt(indx)=indx+1
                npilev(iilev,iion)=ilev
                npilevi(ilev)=iilev
            if(npilev(iilev,iion).eq.0)print *, 'AJA **** ', iilev,iion
                if (lpri.ne.0)
     $           write (lun11,*)ilev,iilev,indx,iion
                iilev=iilev+1
                ilev=ilev+1
                indx=indx+1
              enddo
              mlold(13)=indx-1
              npnxt(indx-1)=0
            endif


            do i=1,2
              if (i.eq.1) then
                lrtp=7
              else
                lrtp=1
              endif
              if (nptrs(3,indx).eq.lrtp) then
                npfi(lrtp,iion)=indx
                if (npfirst(lrtp).eq.0) then
                  npfirst(lrtp)=indx
                else
                  npnxt(mlold(lrtp))=indx
                endif
                if (lpri.ne.0)
     $           write (lun11,*)'npconi loop',indx
                do while(nptrs(3,indx).eq.lrtp)
c                 npcon points from the array of continuum emissivities
c                    to the photoionization data
c                 npconi points from the levels to the arrays of
c                    array of continuum emissivities
c                 npconi2 points from the photoionization data
c                    to the array of continuum emissivities
c                    (inverse if npcon)
c                 icon is the index of the continuum emissivity array
c                    element
c                 indx is the index of the photoionization data
                  npar(indx)=mlold(12)
                  npnxt(indx)=indx+1
                  npcon(icon)=indx
                  if (lpri.ne.0)
     $             write (lun11,*)'index into continuum  array:',
     $                icon
                  if (lpri.ne.0)
     $             write (lun11,*)'index of photoionization element:',
     $                indx
c                 now search for the level that goes with this
c                    photoionization data
                  mltmpn=npfi(13,iion)
                  mlfnd=0
                  nclev=idat1(nptrs(6,indx)+nptrs(9,indx)-2)
                  if (nclev.gt.nlevs(iion)) nlevs(iion)=nclev
                  mltst=nclev
                  if (lpri.ne.0)
     $             write (lun11,*)'searching for level:'
                  mltmp=mltmpn
                  if (mltmpn.ne.0) then
                      npartmpn=npar(mltmpn)
                    else
                      npartmpn=0
                    endif
                  do while ((mlfnd.ne.mltst).and.(mltmpn.ne.0)
     $             .and.(indx.ne.0).and.(npartmpn.eq.npar(indx)))
                    mltmp=mltmpn
                    mlfnd=idat1(nptrs(6,mltmp)+nptrs(9,mltmp)-2)
                    mltmpn=npnxt(mltmp)
                    if (mltmpn.ne.0) then
                        npartmpn=npar(mltmpn)
                      else
                        npartmpn=0
                      endif
                    if (lpri.ne.0)
     $              write (lun11,*)mltmp,mlfnd,mltmpn,npartmpn,
     $                               npar(indx),nclev
                    enddo
                  npconi2(indx)=icon
c                  npconi(icon)=npfi(13,iion)-1+nclev
                  if (mltmp.ne.0) then
                    npconi(mltmp)=icon
                    endif
                  if (lpri.ne.0)
     $             write (lun11,*)indx,npar(indx),icon,nclev,
     $                nptrs(3,indx),lrtp,mltmp
                  indx=indx+1
                  icon=icon+1
                enddo
                mlold(lrtp)=indx-1
                npnxt(indx-1)=0
              endif
           enddo
c
c  lines data and lines pointers, rate type 4, 9 & 14
c  npfirst,npnxt,npar,mold,npfi,nplin,nplini

            if (lpri.ne.0)
     $       write (lun11,*)'nplin,nplini,:'
            do i=1,3
              if (i.eq.1) then
                lrtp=4
              elseif (i.eq.2) then
                lrtp=9
c               I don't think 2 photon should be treated as a line
c                lrtp=-99
              else
                lrtp=14
              endif
              if (lpri.ne.0) write (lun11,*)' indx=',indx,lrtp,iion
              if (nptrs(3,indx).eq.lrtp) then
                npfi(lrtp,iion)=indx
                if (npfirst(lrtp).eq.0) then
                  npfirst(lrtp)=indx
                else
                  npnxt(mlold(lrtp))=indx
                endif
                do while(nptrs(3,indx).eq.lrtp)
                  npar(indx)=mlold(12)
                  npnxt(indx)=indx+1
                  nplin(iline)=indx
                  nplini(indx)=iline
                  if (lpri.ne.0)
     $             write (lun11,*)indx,iline
                  indx=indx+1
                  iline=iline+1
                enddo
                mlold(lrtp)=indx-1
                npnxt(indx-1)=0
              endif
            enddo

c  pointers for rate types 6,8,3,5,40
c  npfirst,npnxt,npar,mold,npfi

            do i=1,5
              if (i.eq.1) then
                lrtp=6
              elseif (i.eq.2) then
                lrtp=8
              elseif (i.eq.3) then
                lrtp=3
              elseif (i.eq.4) then
                lrtp=5
              else
                lrtp=40
              endif
              if (nptrs(3,indx).eq.lrtp) then
                npfi(lrtp,iion)=indx
                if (npfirst(lrtp).eq.0) then
                  npfirst(lrtp)=indx
                else
                  npnxt(mlold(lrtp))=indx
                endif
                do while(nptrs(3,indx).eq.lrtp)
                  npar(indx)=mlold(12)
                  npnxt(indx)=indx+1
                  indx=indx+1
                enddo
                mlold(lrtp)=indx-1
                npnxt(indx-1)=0
              endif
            enddo

c  pointers for other rate types
c  npfirst,npnxt,npar,mold,npfi

            lrtp=nptrs(3,indx)
            do while((lrtp.ne.12).and.(lrtp.ne.11).and.(lrtp.ne.0))
              npar(indx)=mlold(12)
              if (npfirst(lrtp).eq.0) then
                npfirst(lrtp)=indx
              else
                npnxt(mlold(lrtp))=indx
              endif
              mlold(lrtp)=indx
              if (npfi(lrtp,iion).eq.0) npfi(lrtp,iion)=indx
c              write (lun11,*)iion,lrtp,indx,npfi(lrtp,iion)
              indx=indx+1
              lrtp=nptrs(3,indx)
            enddo

c  ionization data and continum pointers, rate type 7 & 1
c  npfirst,npnxt,npar,mlold,npfi,npcon,npconi,npconi2,nlevs


            iion=iion+1

          enddo
c        endif
      enddo

      nlsvn=iline-1
      ncsvn=icon-1
      write (lun11,*)'nlsvn=',nlsvn,ncsvn
c
      go to 9000
c
c     sort the element abundances
      lsrt=0
      do mml=1,nl
        melpt(mml)=mml
      enddo
      niter=0
      do while (lsrt.eq.0)
        lsrt=1
        niter=niter+1
        do mml=1,nl-1
          if (abel(melpt(mml)).lt.abel(melpt(mml+1))) then
            melptmp=melpt(mml)
            melpt(mml)=melpt(mml+1)
            melpt(mml+1)=melptmp
            lsrt=0
          endif
        enddo
      enddo
c
c
c     now redo the element pointers
c     zero the new next pointers
      do mml=1,np2
        npnxt2(mml)=0
        enddo
      npfirst2=0
      mllo=0
c     step thru elements
      do mml=1,nl
        mlloo=mllo
        mll=npfirst(11)
        itst=0
        do while ((mll.ne.0).and.(itst.ne.melpt(mml)))
          mlm=mll-1
          call drd(ltyp,lrtyp2,lcon,nrdt,np1r,nidt,np1i,nkdt,np1k,
     &      mlm,nptrs,0,lun11)
          itst=idat1(np1i-1+nidt)
          mllo=mll
          mll=npnxt(mll)
          enddo
        if (mllo.ne.0) then
          if (npfirst2.eq.0) then
              npfirst2=mllo
            else
              npnxt2(mlloo)=mllo
            endif
          endif
        enddo
      npnxt2(mlloo)=0
      npfirst(11)=npfirst2
      do mml=1,np2
        if ((npnxt2(mml).ne.0).or.(mml.eq.mlloo)) then
          npnxt(mml)=npnxt2(mml)
          endif
        enddo
c
c
 9000  continue
c
       return
c
c     now print stuff sorted
      ntptmp=11
        mll=npfirst(ntptmp)
        write (lun11,*)'ntptmp=',ntptmp
        do while (mll.ne.0)
          CALL DRD(ltyp,lrtyp2,lcon,nrdt,np1r,nidt,np1i,nkdt,np1k,
     &          mll-1,Nptrs,0,Lun11)
          write (lun11,*)'mll=',mll
          call dprinto(ltyp,lrtyp2,lcon,
     $    nrdt,np1r,nidt,np1i,nkdt,np1k,rdat1,idat1,kdat1,lun11)
          mll=npnxt(mll)
          enddo
c

c
      return
      end
