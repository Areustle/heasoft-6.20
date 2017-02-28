         SUBROUTINE xpxgts(string,parse, nin, descr, ndes, char,
     &                  nret, iflag, idelim)

c
c prompt for all the inputs, unless they ar given on the command line
c
c a pre-xpi fudged routine by Nick and Lorella 9/15/92
c
c
         character*(*) string, descr(*), char
         character(256) instring
         integer*4 idelim, iflag, iparse, ibeg, iend
         integer*4 nin, ndes, nret, parse, lenact, i
         logical qskip

         idelim = 0
         iflag = 0
c
         instring = ' '
         iparse = 0
c
         i=1
         do while(i.le.nin)

         CALL XGTARG(string, PARSE, ibeg, iend , QSKIP, IFLAG,
     &   IDELIM)
          if(iend.eq.0)then
           instring = instring(:lenact(instring))//' ?'
          else
           instring = instring(:lenact(instring))//' '//
     &        string(ibeg:iend)
          endif
c
         i = i + 1
         enddo
c
          call xgtstr(instring, iparse, nin, descr, ndes, char,
     &                  nret, iflag, idelim)
c
      return
      END
