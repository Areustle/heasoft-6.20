c **************************************************************************
c  function to strip leading blanks from a character string

      integer function lchop(string)

      integer itotlen, inlen

      character*(*) string

      itotlen = len(string)
      inlen = 1
      do while((string(inlen:inlen).eq.' ')
     &	.and.(inlen.le.itotlen))
        inlen = inlen + 1
      end do
      lchop = inlen

      return

      end

c ****************************************************************************
