**==uclgsg.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
      SUBROUTINE UCLGSG(Parname,Buffer,N,Rmin,Rmax,Nr,Status)
 
*
* parname is the parameter name (c*(*))
* buffer1 is a 2xn real*4 array to hold the resulting ranges
* n is the size of buffer
* rmin and rmax are the min and max for the range
* nr is the number ranges in buffer which are good
* status is the return status
*
* return a range value related to parname
*
*
      CHARACTER*(*) Parname
      INTEGER N
      REAL Buffer(2,N) , Rmin , Rmax
      INTEGER Nr
      INTEGER Status
      character(1000) buffer1
      character(80) line
      INTEGER LENACT , j , i
 
      INCLUDE 'yaccfor.inc'
 
      IF ( DEBug ) THEN
         WRITE (line,*,IOSTAT=j) 'uclgsg called for ' , 
     &          Parname(:MIN(len(line)-62,LENACT(Parname))) , 
     &                           ' Min and max ' , Rmin , Rmax
         CALL XWRITE(line,5)
      ENDIF
      CALL XPIQUERYPAR(Parname,'sg',buffer1,.FALSE.,.TRUE.,.FALSE.,
     &                 Status)
 
 
      IF ( DEBug ) THEN
         WRITE (line,*,IOSTAT=j) 'status of ' , Status , 
     &                           ' value is ' //
     &          buffer1(:MIN(len(line)-32,LENACT(buffer1)))
         CALL XWRITE(line,5)
      ENDIF
 
 100  IF ( Status.NE.0 ) RETURN
      CALL UCLGSGPARSE(buffer1,Buffer,N,Rmin,Rmax,Nr,Status)
      IF ( DEBug ) THEN
         WRITE (line,*,IOSTAT=j) 'Final return: nr is ' , Nr
         CALL XWRITE(line,5)
         DO 150 i = 1 , Nr
            WRITE (line,*,IOSTAT=j) 'buffer is ' , i , Buffer(1,i) , 
     &                              Buffer(2,i)
            CALL XWRITE(line,5)
 150     CONTINUE
      ENDIF
      RETURN
 
      ENTRY UCLGSGD(Parname,Buffer,N,Rmin,Rmax,Nr,Status)
 
      CALL XPIDEFAULTPAR(Parname,'sg',buffer1,Status)
      CALL UCLGSGPARSE(buffer1,Buffer,N,Rmin,Rmax,Nr,Status)
 
      GOTO 100
      END
**==uclgsgparse.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
c$$$      IMPLICIT NONE
c$$$      CHARACTER*(*) Parname
c$$$      INTEGER N
c$$$      REAL*4 Buffer1(2,N) , Rmin , Rmax
c$$$      INTEGER Nr
c$$$      INTEGER Status
c$$$      character(100) buffer
c$$$
c$$$      INTEGER TBLFPR
c$$$      character(80) str1 , str2
c$$$      INTEGER ierr
c$$$
c$$$      INCLUDE 'tbl.inc'
c$$$      INCLUDE 'yaccfor.inc'
c$$$
c$$$      INTEGER i
c$$$      INTEGER j
c$$$      INTEGER LENACT
c$$$
c$$$      IF ( Status.NE.0 ) RETURN
c$$$
c$$$      buffer = ' '
c$$$
c$$$      i = TBLFPR(Parname)
c$$$
c$$$      IF ( i.EQ.0 ) THEN
c$$$         Status = 1
c$$$         RETURN
c$$$      ENDIF
c$$$
c$$$      IF ( i.GT.TBLpcnt ) THEN
c$$$         Status = 1
c$$$         RETURN
c$$$      ENDIF
c$$$
c$$$
c$$$      IF ( TBLptype(i).NE.'g' ) THEN
c$$$         Status = 2
c$$$         RETURN
c$$$      ENDIF
c$$$
c$$$      str1 = Parname
c$$$      CALL UPC(str1)
c$$$
c$$$      DO 100 j = 1 , NPArs
c$$$         str2 = SPArs(j)
c$$$         CALL UPC(str2)
c$$$
c$$$         IF ( str1.EQ.str2 ) THEN
c$$$            buffer = SVAl(j)
c$$$            IF ( INDEX(TBLpupd(i),'l').NE.0 ) THEN
c$$$               TBLpdefl(i) = buffer
c$$$               CALL YSTCLQ1(TBLpdefl(i))
c$$$*               CALL TBSVPR(Tblpfname,ierr)
c$$$            ENDIF
c$$$         ENDIF
c$$$ 100  CONTINUE
c$$$
c$$$      IF ( (buffer.EQ.' ' .OR. .NOT.TBLpstat(i)) .AND.
c$$$     &     INDEX(TBLpupd(i),'q').EQ.0 ) buffer = TBLpdefl(i)
c$$$
c$$$      IF ( (buffer.EQ.' ' .AND. INDEX(TBLpupd(i),'h').EQ.0) ) THEN
c$$$         CALL XCREAD(TBLpdesc(i)(1:LENACT(TBLpdesc(i)))
c$$$     &               //'['//TBLpdefl(i)(1:LENACT(TBLpdefl(i)))//']',
c$$$     &               buffer,ierr)
c$$$         IF ( buffer.EQ.' ' ) buffer = TBLpdefl(i)
c$$$         IF ( buffer(1:1).EQ.'"' ) CALL YSTCLQ1(buffer)
c$$$         IF ( INDEX(TBLpupd(i),'l').NE.0 ) THEN
c$$$            TBLpdefl(i) = buffer
c$$$            CALL YSTCLQ1(TBLpdefl(i))
c$$$*            CALL TBSVPR(Tblpfname,ierr)
c$$$         ENDIF
c$$$      ENDIF
c$$$
c$$$ 200  CALL UCLGSGPARSE(buffer,Buffer1,N,Rmin,Rmax,Nr,Status)
c$$$
c$$$
c$$$      RETURN
c$$$* Get the default value
c$$$
c$$$      ENTRY UCLGSGD(Parname,Buffer1,N,Rmin,Rmax,Nr,Status)
c$$$
c$$$      IF ( Status.NE.0 ) RETURN
c$$$
c$$$
c$$$      i = TBLFPR(Parname)
c$$$
c$$$      IF ( i.EQ.0 ) THEN
c$$$         Status = 1
c$$$         RETURN
c$$$      ENDIF
c$$$
c$$$      IF ( i.GT.TBLpcnt ) THEN
c$$$         Status = 1
c$$$         RETURN
c$$$      ENDIF
c$$$
c$$$
c$$$      IF ( TBLptype(i).NE.'g' ) THEN
c$$$         Status = 2
c$$$         RETURN
c$$$      ENDIF
c$$$
c$$$      buffer = TBLpdefl(i)
c$$$
c$$$      GOTO 200
c$$$
c$$$
c$$$      END
 
 
      SUBROUTINE UCLGSGPARSE(Buffer,Buffer1,N,Rmin,Rmax,Nr,Status)
 
 
      IMPLICIT NONE
      INTEGER N
      REAL*4 Buffer1(2,N) , Rmin , Rmax
      CHARACTER*(*) Buffer
      character(100) dsnafu , line
      INTEGER Nr
      INTEGER Status
 
      INTEGER ierr
 
      INCLUDE 'yaccfor.inc'
 
      INTEGER i
      INTEGER j
      INTEGER k
      INTEGER bn
      INTEGER LENACT
      INTEGER asdf
 
* clean up the range
 
* replace all spaces with commas
 
      IF ( DEBug ) THEN
         line = 'Start buffer is ' // Buffer(1:LENACT(Buffer))
         CALL XWRITE(line,5)
      ENDIF
 
 
      DO 100 i = 1 , LEN(Buffer)
         IF ( Buffer(i:i).EQ.' ' ) Buffer(i:i) = ','
 100  CONTINUE
 
 
* remove extra commas
 
      i = 1
      DO WHILE ( i.LE.LEN(Buffer)-1 )
         IF ( Buffer(i:i).EQ.',' .AND. Buffer(i+1:i+1).EQ.',' ) THEN
            Buffer = Buffer(1:i-1)//Buffer(i+1:LEN(Buffer))
            Buffer(LEN(Buffer):LEN(Buffer)) = ' '
         ELSE
            i = i + 1
         ENDIF
      ENDDO
      IF ( DEBug ) THEN
         line = 'Start g parse, buffer is ' // Buffer(1:LENACT(Buffer))
         CALL XWRITE(line,5)
      ENDIF
 
 
 
*
* Now parse the range
*
 
      DO 200 i = 1 , N
         Buffer1(1,i) = Rmin
         Buffer1(2,i) = Rmax
 200  CONTINUE
 
      i = 1
      j = 1
      k = 1
      bn = 1
      Nr = 0
      DO WHILE ( i.LE.LENACT(Buffer) )
         IF ( Buffer(i:i).EQ.',' ) THEN
            dsnafu = Buffer(bn:i-1)
            READ (dsnafu,*,IOSTAT=ierr) Buffer1(k,j)
            IF ( dsnafu.EQ.'**' ) THEN
               IF ( k.EQ.1 ) THEN
                  Buffer1(k,j) = Rmin
               ELSE
                  Buffer1(k,j) = Rmax
               ENDIF
            ENDIF
            IF ( DEBug ) THEN
               WRITE (line,*,IOSTAT=asdf) 1 , dsnafu(1:10) , 
     &                Buffer(bn:i-1) , ' ' , Buffer1(k,j) , k , j , ierr
               CALL XWRITE(line,5)
            ENDIF
            IF ( k.EQ.1 ) Buffer1(2,j) = Buffer1(1,j)
            bn = i + 1
            j = j + 1
            k = 1
            Nr = Nr + 1
         ELSEIF ( Buffer(i:i).EQ.'-' ) THEN
            dsnafu = Buffer(bn:i-1)
            READ (dsnafu,*,IOSTAT=ierr) Buffer1(k,j)
            IF ( dsnafu.EQ.'**' ) THEN
               IF ( k.EQ.1 ) THEN
                  Buffer1(k,j) = Rmin
               ELSE
                  Buffer1(k,j) = Rmax
               ENDIF
            ENDIF
            IF ( DEBug ) THEN
               WRITE (line,*,IOSTAT=asdf) 2 , dsnafu(1:10) , 
     &                Buffer(bn:i-1) , ' ' , Buffer1(k,j) , k , j , ierr
               CALL XWRITE(line,5)
            ENDIF
            bn = i + 1
            k = 2
         ENDIF
         i = i + 1
      ENDDO
 
      IF ( DEBug ) THEN
         WRITE (line,*,IOSTAT=j) 'Final return: nr is ' , Nr
         CALL XWRITE(line,5)
         WRITE (line,*,IOSTAT=j) 'min and max' , Rmin , Rmax
         DO 250 i = 1 , Nr
            WRITE (line,*,IOSTAT=j) 'buffer1 is ' , i , Buffer1(1,i) , 
     &                              Buffer1(2,i)
            CALL XWRITE(line,5)
 250     CONTINUE
      ENDIF
 
 
      RETURN
 
      END
 
