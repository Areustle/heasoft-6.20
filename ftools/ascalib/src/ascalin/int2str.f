C     subroutine int2str(n,k,string)
C
C Format a integer into a string using silly algorithum for dinosaur
C FORTRAN compiler compatibility. EVG @ GSFC/NASA.
C
C          n = integer (input)
C          k = number of digits in formated string (output)
C     string = formated string with left margin justification (output)
C
        subroutine int2str(n,k,string)
        integer k, n
        character*(*) string

        if (n .eq. 0) then

           k = 1
           string = '0'

        else

c        if (n .gt. 0) k = int(log10(real(n)))+1
c        write(temp, '(i<k>)') n

           k = int(log10(real(abs(n))))+1

           if (n .lt. 0) k = k + 1

           if (k.eq.1) then
              write(string, '(i1)') n
           else if (k .eq. 2) then
              write(string, '(i2)') n
           else if (k .eq. 3) then
              write(string, '(i3)') n
           else if (k .eq. 4) then
              write(string, '(i4)') n
           else if (k .eq. 5) then
              write(string, '(i5)') n
           else if (k .eq. 6) then
              write(string, '(i6)') n
           else if (k .eq. 7) then
              write(string, '(i7)') n
           else if (k .eq. 8) then
              write(string, '(i8)') n
           else if (k .eq. 9) then
              write(string, '(i9)') n
           else if (k .eq. 10) then
              write(string, '(i10)') n
           else
              write(string, '(i12)') n
           end if

        end if

        end
