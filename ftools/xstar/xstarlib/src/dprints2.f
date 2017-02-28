      subroutine dprints2(ltyp,lrtyp,lcon,
     $  nrdt,rdat,nidt,idat,nkdt,kdat,lun11)
c
c     this  routine prints one element of the database
c     author:  T. Kallman
c
      implicit none
      integer nptmpdim
      parameter (nptmpdim=200000)
      real*8 rdat(nptmpdim)
      integer idat(nptmpdim)
      character(1) kdat(nptmpdim)
      character(20000) kdtt
      character(1) kblnk,ktst,kperc,kdtt2(nptmpdim)
      integer ltyp,lrtyp,lcon,nrdt,nidt,nkdt,lun11,
     $        nkd,nkd2,ml2,itmp,ml,ll2,ll,mm
      real*8 rtmp
c
      data kblnk/' '/,kperc/'%'/
c
c      do ll=1,20000
c         write (kdtt(ll:ll),'(a1)')kblnk
c         endif
      write (kdtt(1:18),9823)ltyp,lrtyp,lcon
      write (kdtt(19:37),9823)nrdt,nidt,nkdt
c
      nkd=38
      nkd2=37
      rtmp=rdat(1)
      if (1.gt.nrdt) rtmp=0.
      ml2=nkd2
      write (kdtt(ml2:ml2+12),'(1pe13.5)')rtmp
      ml2=nkd+13
      write (kdtt(ml2-1:ml2-1),'(a1)')kblnk
      rtmp=0.
      if (2.le.nrdt) rtmp=rdat(nrdt)
      write (kdtt(ml2:ml2+12),'(1pe13.5)')rtmp
      nkd2=nkd+2*13
      nkd=nkd2
c
      write (kdtt(nkd:nkd),'(a1)')kblnk
      nkd=nkd2+1
      ml2=nkd
      itmp=idat(1)
      if (1.gt.nidt) itmp=0
      ml2=nkd2
      write (kdtt(ml2:ml2+5),'(i6)') itmp
      ml2=ml2+6
      itmp=idat(nidt)
      if (1.gt.nidt) itmp=0
      write (kdtt(ml2:ml2+5),'(i6)') itmp
      nkd2=nkd+2*6-1
        write (kdtt(nkd:nkd),'(a1)')kblnk
      nkd=nkd2
c
      nkd=nkd2
      ml2=nkd
      if (nkdt.ne.0) then
        write (kdtt(nkd:nkd),'(a1)')kblnk
        nkd=nkd+1
        nkd2=nkd+nkdt
c        write (lun11,*)nkd,nkdt,nkd2,(kdat(mm),mm=1,nkdt)
        do  ml=1,nkdt
          ml2=nkd+ml-1
          write (kdtt(ml2:ml2),'(a1)')kdat(ml)
           ml2=ml2+1
          enddo
        endif
 9823    format (4i6)
c       write (lun11,*)'before write:'
c       write (lun11,*)kdtt
      ml2=ml2-1
c
      ll2=0
c     remove spaces
      ktst=kperc
      do ll=1,ml2
         read(kdtt(ll:ll),'(a1)')ktst
         ll2=ll2+1
         kdtt2(ll2)=ktst
         enddo
c
      write (lun11,911)(kdtt2(mm),mm=1,ll2),kblnk,kperc
 911  format (20000a1)
c
      return
      end
