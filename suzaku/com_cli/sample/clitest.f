C
C File: CLITEST.FOR
C Description: Test program for CLI
C
      Implicit NONE
      Integer  i, nw, it, lun, length, ierr
      Real  a, b, c
      Character * 80  string
      Character * 16  text
      Character * 16  words(8), word, key
      Character * 2  table(5)
C
      Integer * 4  status
      Logical * 4  lstat
C
      Integer * 4  ival
      Real * 4  rval
      Real * 8  dval
      Integer * 4  hval
      Logical * 4  lval
C function
      Integer * 4  Iopen, Lenrd, CLgets
      Logical * 4  Qopen
C data
      Data lun / 1 /
      Data string / ' ' /
      Data  ival / 123 /
      Data  rval / 1.23E0 /
      Data  dval / 1.23D0 /
      Data  hval / 255 /
      Data  lval / .TRUE. /
      Data table / 'aa','ab','c','d','e' /
C begin
C ...test for basic input routines
      Call Intrd('Input INTEGER',ival)
      Print *, 'ival =', ival
      Call IntrdL('Input INTEGER (IntrdL)',ival,1,10)
      Print *, 'ival =', ival
      Call IntrdX('Input INTEGER (IntrdX)',ival,text)
      Print *, 'ival =', ival, ' text =', text
C
      Call Fltrd('Input REAL*4',rval)
      Print *, 'rval =', rval
      Call FltrdL('Input REAL*4 (FltrdL)',rval,1.0,10.0)
      Print *, 'rval =', rval
      Call FltrdX('Input REAL*4 (FltrdX)',rval,text)
      Print *, 'rval =', rval, ' text =', text
C
      Call Fdprd('Input REAL*8',dval)
      Print *, 'dval =', dval
      Call FdprdL('Input REAL*8 (FdprdL)',dval,1.0D0,10.0D0)
      Print *, 'dval =', dval
      Call FdprdX('Input REAL*8 (FdprdX)',dval,text)
      Print *, 'dval =', dval, ' text =', text
C
      Call Hexrd('Input HEX',hval)
      Print *, 'hval =', hval
      Call HexrdL('Input HEX',hval,1,10)
      Print *, 'hval =', hval
C
      Call Logrd('Input LOGICAL',lval)
      Print *, 'lval =', lval
C
C ...test for conversion routines
      Call Txtrd('?Input Real',text)
      Call CLatof(text,rval)
      Print *, 'text,rval=',text,rval
      Call Txtrd('?Input Integer',text)
      Call CLatoi(text,ival)
      Print *, 'text,ival=',text,ival
      Call Fltrd('?Input Real',rval)
      Call CLftoa(rval,text)
      Print *, 'rval,text=',rval,text
      Call Fdprd('?Input Double',dval)
      Call CLdtoa(dval,text)
      Print *, 'dval,text=',dval,text
C
C ...test for string maipulation routines
      Call CLword('word1, word2, word3',' ,',nw,words)
      Do i = 1, nw
        Print *, 'word(',i,')=',words(i)
      End Do
      Call CLpart('word1 word2 word3',2,word)
      Print *, '2nd part is:',word
C
C ...test for OPNRD
      Call Opnrd('clitest.com')
      Call Txtrd('OPNRD>',string)
      Call Txtrd('OPNRD>',string)
      Call Txtrd('OPNRD>',string)
      Call Txtrd('OPNRD>',string)
C
C ...test for LUNRD
      Open(Unit=lun,File='clitest.com',Status='OLD',Form='FORMATTED')
      Call Lunrd(lun)
      Call Txtrd('LUNRD>',string)
      Call Txtrd('LUNRD>',string)
      Call Txtrd('LUNRD>',string)
      Call Txtrd('LUNRD>',string)
      close(lun)
C
C ... test for PRINTF
      a = 1.0
      b = 1.99
      c = 0.12345
      Call Printf(6,'a = %f, b = %6f, c = %6.3f, d = %d, e = (%5d)$',
     &            a,b,c,100,12)
C
C ... test for IOPEN
      call CLfdel('clitest.dat')
      status = Iopen(lun,'clitest','.dat',' ')
      write(*,*) 'Iopen (w) status = ', status
      write(lun,*) 'THIS IS A TEST (1)'
      close(lun)
      status = Iopen(lun,'clitest','.dat','R')
      write(*,*) 'Iopen (R) status = ', status
      read(lun,'(A)') string
      write(*,*) string
      close(lun)
C
C ... test for QOPEN
      string = '*.com'
      lstat = Qopen('filename>',lun,string,'READ','FORMATTED','OLD')
      if ( lstat ) then
        write(*,*) string(:Lenrd(string))
        length = Len(string)
        ierr = CLgets(lun, string, length)
        If ( ierr .eq. 0 ) Then
          write(*,*) 'line=' // string(:length)
        End If
        Call CLclos(lun, ierr)
      else
        write(*,*) 'failed'
      end if

      string = 'clitest.dat'
      lstat = Qopen('filename>',lun,string,'WRITE','FORMATTED','NEW')
      write(1,*) 'THIS IS A TEST (2)'
      close(lun)
C
C ... test for KEYRD
      Call Keyrd(-3,'?KEY>',key,table,5,it)
      Print *, it,',',key
C
      Stop
      End
C
