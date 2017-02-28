      subroutine dprinto2(ltyp,lrtyp,lcon,
     $  nrdt,rdat,nidt,idat,nkdt,kdat,lun11)
c
c     this  routine prints one element of the database
c     author:  T. Kallman
c
c
      implicit none
c
      integer nptmpdim
      parameter (nptmpdim=200000)
c
      real*8 rdat(nptmpdim)
      integer idat(nptmpdim)
      character(1) kblnk,kperc,kdat(nptmpdim)
      integer ltyp,lrtyp,lcon,nrdt,nidt,nkdt,lun11,lsp,ml2,nkd,
     $        nkd2,ll2,ml,mm,ll
c
      character(400000) kdtt
      character(1) ktst,kdtt2(400000) 
c
      data kblnk/' '/,kperc/'%'/
c
c      write (lun11,*)ltyp,lrtyp,lcon,nrdt,nidt,nkdt,np1r,np1i,np1k
c     $  (rdat(mm),mm=1,nrdt),(idat1(np1i-1+mm),mm=1,nidt),
c     $  kblnk,(kdat1(np1k-1+mm),mm=1,nkdt),kblnk,kperc
c      return
c
      write (kdtt(1:18),9823)ltyp,lrtyp,lcon
      write (kdtt(19:37),9823)nrdt,nidt,nkdt
c
c
      lsp=0
      ml2=0
      if (lsp.eq.1) go to 9009
      nkd=38
      nkd2=39
      if (nrdt.gt.0) then
c        nkd2=nkd+nrdt*13
        nkd2=nkd
        do 303 ml=1,nrdt
           ml2=nkd+(ml-1)*15
             write (kdtt(ml2:ml2+14),'(1pe15.7)')rdat(ml)
             nkd2=nkd2+15
 303       continue
        endif
      nkd=nkd2
      write (kdtt(nkd:nkd),'(a1)')kblnk
      nkd=nkd2+1
      if (nidt.gt.0) then
        nkd2=nkd+nidt*8
        do 302 ml=1,nidt
           ml2=nkd+(ml-1)*8
           write (kdtt(ml2:ml2+7),'(i8)')idat(ml)
           ml2=ml2+8
 302       continue
        endif
      nkd=nkd2
      if (nkdt.gt.0) then
        write (kdtt(nkd:nkd),'(a1)')kblnk
        nkd=nkd+1
        nkd2=nkd+nkdt
c        write (lun11,*)nkd,nkdt,nkd2,(kdat1(np1k-1+mm),mm=1,nkdt)
        do 301 ml=1,nkdt
          ml2=nkd+ml-1
          write (kdtt(ml2:ml2),'(a1)')kdat(ml)
           ml2=ml2+1
 301      continue
         endif
 9823    format (3i6)
c       write (lun11,*)'before write:'
c       write (lun11,*)kdtt
      ml2=ml2-1
c
c
      ll2=0
c     remove spaces
      ktst=kperc
      do 3301 ll=1,ml2
c         ktsto=ktst
         read(kdtt(ll:ll),'(a1)')ktst
c         if ((ktst.eq.kblnk).and.(ktsto.eq.kblnk)) go to 3301
         ll2=ll2+1
         kdtt2(ll2)=ktst
 3301    continue
c
      write (lun11,911)(kdtt2(mm),mm=1,ll2),kblnk,kperc
 911  format (400000a1)
c
      return
c
 9009 continue
c      call dprints(ltyp,lrtyp,lcon,
c     $  nrdt,rdat,nidt,idat,nkdt,kdat,lun11)
c
c
      return
      end
