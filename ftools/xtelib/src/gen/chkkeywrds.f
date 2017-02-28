      subroutine chkkeywrds(iunit1,iunit2,keywrd,lagree)

      integer iunit1, iunit2, status1, status2
      character*(*) keywrd
      character(80) strval1, comm1, strval2, comm2
      logical lagree
      strval1=' '
      strval2=' '
      comm1=' '
      comm2=' '
      status1=0
      status2=0

      call ftgkys(iunit1,keywrd,strval1,comm1,status1)
      call ftgkys(iunit2,keywrd,strval2,comm2,status2)

      if(strval1.eq.strval2)lagree=.TRUE.
      if(status1.ne.0.and.status2.ne.0)then
        lagree=.TRUE.
      endif

      return
      end
