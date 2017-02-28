**==STRNUM.spg  processed by SPAG 3.09I  at 09:47 on 20 Aug 1992
      SUBROUTINE STRNUM(String,Type,Real8,Ierr)
c
c  converts an input char string to numeric for given type
c  Nick 18.6.90, from update_database (Arvind?)
c
 
c
      INCLUDE 'estec.inc'
c
      REAL*8 Real8
      REAL*4 real4
      INTEGER*2 int2
      INTEGER*4 int4 , Type
      CHARACTER*(*) String
      INTEGER*4 Ierr
c
      CALL UPC(String)
c      numeric=.true.
c      IF ( type.GE.-10 ) THEN
c     	ilen=lenact(string)
c     	if(ilen.le.0)then
c      	 real8=0.0
c     	 return
c        endif
c
c     	ii=1
c     	do while(ii.le.ilen)
c        numeric = .FALSE.
c        IF ( string(ii:ii).EQ.'1' .OR. string(ii:ii).EQ.'2'
c     &       .OR. string(ii:ii).EQ.'3' .OR. string(ii:ii).EQ.'4'
c     &       .OR. string(ii:ii).EQ.'5' .OR.
c     &       string(ii:ii).EQ.'6' .OR. string(ii:ii).EQ.'7'
c     &       .OR. string(ii:ii)
c     &       .EQ.'8' .OR. string(ii:ii).EQ.'9' .OR.
c     &       string(ii:ii).EQ.'+' .OR.
c     &       string(ii:ii).EQ.'-' .OR. string(ii:ii).EQ.'.'
c     &       .OR. string(ii:ii)
c     &       .EQ.'0') numeric = .TRUE.
c	  if( string(ii:ii).eq.'E'.or.string(ii:ii).eq.'D') then
c	   if(type.eq.2.or.type.eq.-4.or.type.eq.-5)then
c	   write(zwrite,*)
c     & ' Input string contains exponential for an integer value'
c         call xwrite(zstring,5)
c	   ierr=1
c	   return
c	   endif
c	  else
c	   numeric=.true.
c	  endif
c        IF ( .NOT.numeric ) THEN
c          call xwrite(' Input string is not numeric',5)
c     	  ierr=1
c     	  return
c        END IF
c     	ii=ii+1
c     	enddo
c      else
c       call xwrite('Error: data type is not numeric',5)
c       return
c      END IF
c
c decode string according to type
c
      IF ( Type.LT.-10 ) THEN
         String = String
         Real8 = 0.0
      ELSEIF ( Type.EQ.8 ) THEN
         READ (String,FMT=*,IOSTAT=Ierr) Real8
      ELSEIF ( Type.EQ.4 ) THEN
         READ (String,FMT=*,IOSTAT=Ierr) real4
         Real8 = real4
      ELSEIF ( Type.EQ.-4 .OR. Type.EQ.-5 ) THEN
         READ (String,FMT=*,IOSTAT=Ierr) int4
         Real8 = int4
         IF ( Type.EQ.-5 ) THEN
            CALL XWRITE(' Warning: assuming time given as SHF key',15)
         ENDIF
      ELSEIF ( Type.EQ.2 ) THEN
         READ (String,FMT=*,IOSTAT=Ierr) int2
         Real8 = int2
      ELSE
         WRITE (Zwrite,99001) Type
         CALL XWRITE(Zwrite,5)
         Ierr = 1
         RETURN
      ENDIF
      IF ( Ierr.NE.0 ) THEN
         CALL XWRITE(' Error in data type of input string',5)
      ENDIF
c
      RETURN
99001 FORMAT (' Error:',I7,' is an unknown data type')
      END
