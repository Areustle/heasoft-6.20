      subroutine xrgettrend(itre, itremo, status)
c     O   itre = trend removal flag (order of polynomial trend)
c     O   itremo = trend rem. mode (1=subtract, 2=divide, 3=return trend)
c
c  Ask for trend removal (degree of polynom.)
c

      include '../include/io.inc'
      include '../include/xronos.inc'
      parameter (subname = 'xrgettrend:')

      call uclgsi('itre',itre,status)
      if(status.NE.0) then
         context= 'Couldn''t get ITRE parameter'
         errm = subname//' '//context
         call xaerror(errm, 5)
         goto 999
      endif      

      IF (itre.LT.0 .OR. itre.GT.4) then
         status = 1002
         write(context,'(I9,
     $        ''is not a legal value for order of trend removal'')')
     $        itre
         errm = subname//' '//context
         call xaerror(errm, 5)
         GOTO 999
      ENDIF         


      write (context, '('' itre = '',i10)') itre
      call xwrite (context, 15)
c
c  Ask for trend removal (mode)
c
      IF (itre.NE.0) THEN
         call uclgsi('itremo',itremo,status)
         if(status.NE.0) then
            context= 'Couldn''t get ITREMO parameter'
            errm = subname//' '//context
            call xaerror(errm, 5)
            goto 999
         endif      

         IF (itremo.LT.1 .OR. itremo.GT.3) then
            status = 1002
            write(context,'(I9,
     $           ''is not a legal value for trend removal operation'')')
     $           itremo
            errm = subname//' '//context
            call xaerror(errm, 5)
            errm = subname//' '//'Must be 1,2,or 3'
            call xaerror(errm, 5)
            GOTO 999
         ENDIF         
         WRITE (context, '('' itremo = '',i10)')itremo
         call xwrite (context,15)
      endif
      
 999  continue
      return
      end
