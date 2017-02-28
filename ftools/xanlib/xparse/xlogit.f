      SUBROUTINE XLOGIT(CSTR, IPARSE, IFLAG)
      CHARACTER CSTR*(*)
      INTEGER   IPARSE, IFLAG
C---
C XSPEC subroutine to open a log file... if the file name
C is none then the file is closed.  Also sets the logging
C of command file info.
C { [ <filename> <base log> <step size> ] |
C        [   NONE   ] |
C        [   SET     [ [ DEFAULT <default name> ] |
C              [ APPEND [ ON | OFF ] ]    |
C              [ STAMP  [ ON | OFF ] ]    |
C              [ ID <id string> ] ] ] }
C---
C CSTR    I    parse string
C IPARSE  I/O  parse position string
C IFLAG     O  Status flag 0=success, -1=EOF during parse
C---
C 1990-Sep-14 - Modified to support LOG file driver [AFT]
C version 2.0 (XLOGIT)  New XPARSE version for log file opening syntax
C setlog   rashafer 7 Aug 1984
C---
      INCLUDE  'xparinc.inc'
      integer   nset_opt
      parameter (nset_opt=4)
      integer   lenact
C
      character(60) filename
      character(60) keyword
      character(60) id
      character(40) default_file
      character(35) descr(2)
      character    ctmp
      character(7) set_opt(nset_opt)
      real      rbuf
      integer*4 ios, lstr, nbuf
      integer*4 nret,inqunit,idelim
      integer*4 chatvl(2)
      integer*4 iset_opt
      logical*4 exist, opnlog
      logical*4 qpart,xqmtch,xgtonf,qjunk
      logical*4 append, stamp
      data default_file/'log.log'/
      data filename/'log.log'/
      data set_opt/'append','default','id','stamp'/
      data descr/'chattyness level to log commands',
     &      'increment for indirect command files'/

      keyword=filename
      call xgtstr( CSTR, iparse, 1,
     &    'log file name, `none'', or `set''', 1, keyword, nret,
     &    iflag, -1)
      CALL LOGGER(2, RBUF, NBUF, CTMP, LSTR)
      opnlog = RBUF.NE.0.
      if(nret.le.0) then
C** if the file is already open, or if an EOF occured during the
C** handling of the '?', return, otherwise use the current filename
         if((opnlog).or.(iflag.lt.0)) return
      end if
      if(xqmtch('none',keyword,qpart)) then
         if(opnlog) call xclslg('keep')
      else if(xqmtch('set',keyword,qpart)) then
         call xgtmch( CSTR, iparse, set_opt, nset_opt,'SET option',
     &        iset_opt,
     &        iflag, idelim)
         if(iflag.ne.0) return
         goto (110,120,130,140) iset_opt
C** do CASE ISET_OPT
C**  APPEND handler
  110    continue
         append = xgtonf(CSTR,iparse,'APPEND log file status',append,
     &       iflag)
         goto 199
C**  DEFAULT handler
  120    continue
         call xgtstr(CSTR,iparse,1,'default log file name',1,
     &       default_file,nret,iflag,idelim)
         goto 199
C**  ID handler
  130    continue
         call xgtstr(CSTR,iparse,1,'log file ID string',1,id,nret,
     &       iflag,idelim)
         goto 199
C**  STAMP handler
  140    continue
         qjunk = xgtonf(CSTR, iparse, 'log time stamp status', stamp,
     &       iflag)
         goto 199
C** end CASE ISET_OPT
  199    continue
      else
         inquire(file=keyword,name=filename,exist=exist,number=inqunit
     &        ,iostat=ios)
         if(.not.(opnlog.and.exist.and.(inqunit.eq.NINT(RBUF)))) then
C** The file is not opened currently to the input file
            if(opnlog) call xclslg('keep')
            inqunit=RBUF
            call xopnlg(filename, append, id, stamp,
     &       inqunit, iflag)
            if(iflag.ne.0) then
               call xwrite(' Unable to open the log file `'
     &           //filename(:lenact(filename))//'''',5)
               return
            end if
         end if
C** get the command file chattyness levels
         call xgtint(CSTR,iparse,2,descr,2,chatvl,nret,iflag,-1)
      end if
      return
      end
