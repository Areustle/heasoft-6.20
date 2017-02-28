**==LSTBLK.spg  processed by SPAG 3.09I  at 09:44 on 20 Aug 1992
      SUBROUTINE LSTBLK(Num,String_list,String_descrip,Pages)
c
c   to list a block of browse commands (from ?) command    nick 9/2/90
c
      INTEGER*4 i , ij , nhalf , j , nn , num_start
      INTEGER*4 npages , ndone , num_to_go , Num , nlines , num_list
      CHARACTER*(*) String_list(*) , String_descrip(*)
c
      character(255) zwrite
      character(80) line
      LOGICAL*4 odd , finish , Pages
c
c
      line = 'Characters before the * indicate the minimum abbreviation'
      WRITE (zwrite,'(1x,a)') line
      CALL XWRITE(zwrite,10)
c
c  is there an odd one
c
      nhalf = Num/2
      nn = nhalf*2
      odd = .FALSE.
      IF ( Num.NE.nn ) odd = .TRUE.
c
      num_to_go = nhalf
      nlines = 20
      npages = num_to_go/nlines + 1
c
      num_start = 1
      ndone = 0
      i = 0
      DO WHILE ( npages.GT.ndone )
c
         num_list = nlines
         IF ( num_to_go.LT.num_list ) num_list = num_to_go
c
         DO 50 j = 1 , num_list
            i = i + 1
            line(1:10) = String_list(i)
            line(11:13) = '-> '
            line(14:37) = String_descrip(i)
            line(38:38) = ' '
c
            ij = i + num_list
c
            line(39:48) = String_list(ij)
            line(49:51) = '-> '
            line(52:78) = String_descrip(ij)
c
            WRITE (zwrite,'(1x,a)') line(1:78)
            CALL XWRITE(zwrite,10)
 50      CONTINUE
         ndone = ndone + 1
         num_to_go = num_to_go - num_list
         i = i + num_list
c
         IF ( num_to_go.GT.0 .AND. Pages ) THEN
            CALL PAGBRK(finish)
            IF ( finish ) RETURN
         ENDIF
c
      ENDDO
c
c   do the odd one
c
      IF ( odd ) THEN
         line(1:38) = ' '
         line(39:45) = String_list(Num)
         line(46:48) = '-> '
         line(49:78) = String_descrip(Num)
c
         WRITE (zwrite,'(1x,a)') line
         CALL XWRITE(zwrite,10)
      ENDIF
c
      RETURN
      END
