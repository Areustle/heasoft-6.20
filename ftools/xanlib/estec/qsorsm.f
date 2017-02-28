**==QSORSM.spg  processed by SPAG 3.09I  at 09:47 on 20 Aug 1992
      SUBROUTINE QSORSM(Nmax,Carray,Carra1,Nbros)
c
c ,quicksort for characters small version
C modified from algol code to structured fortran,j.c.lewis
C see n.wirth 'algorithms+data structures=programs'
C -quicksort takes an element k (near the middle say),scans the array
C  from the left till an el. gt k is found,scans from the right till an
C  el. lt k is found,& swaps these 2 els.
C  this is continued till all els to left of k are lt k,and all to right
C  are gt k.
C  now do the same for these 'subpartitions'.when all subpartitions
C  are of length 1,array is sorted
c !!!
C   use a stack array to keep track of partitions yet to be sorted-
C  always sort the smaller partition first
C  use stack size=log2(nmax)
C
C  sorts only on the first 4 chars (10000 char*4 = 20000 words)
C  uses ema array as real    since chars are not allowed in ema
C  recovers them via internal i/o  (mod by l.chiappetti 231085)
c  this version should sort 16 characters
c changes to have *(*) character strings nick
C
      DIMENSION st(2,30)
      INTEGER r , s , st , temp , Nbros(*) , Nmax , l , i , j , lmed
      character(16) testr , lcvar , lcvaj , tempr
      CHARACTER*(*) Carray(*) , Carra1(*)
      LOGICAL loop


C
C initialise-
      s = 1
      st(1,s) = 1
      st(2,s) = Nmax
 100  l = st(1,s)
      r = st(2,s)
      s = s - 1
      loop = .TRUE.
      DO WHILE ( loop )
         i = l
         j = r
         lmed = (l+r)/2
         testr = Carray(lmed)
         lcvar = Carray(i)
         DO WHILE ( .TRUE. )
            IF ( lcvar.GE.testr ) THEN
               lcvaj = Carray(j)
               DO WHILE ( testr.LT.lcvaj )
                  j = j - 1
                  lcvaj = Carray(j)
               ENDDO
               IF ( i.LE.j ) THEN
                  tempr = Carray(i)
                  Carray(i) = Carray(j)
                  Carray(j) = tempr
                  temp = Nbros(i)
                  Nbros(i) = Nbros(j)
                  Nbros(j) = temp
                  i = i + 1
                  j = j - 1
               ENDIF
               IF ( i.LE.j ) THEN
                  lcvar = Carray(i)
               ELSE
                  IF ( (j-l).LT.(r-i) ) THEN
                     IF ( i.LT.r ) THEN
                        s = s + 1
                        st(1,s) = i
                        st(2,s) = r
                     ENDIF
                     r = j
                  ELSE
                     IF ( l.LT.j ) THEN
                        s = s + 1
                        st(1,s) = l
                        st(2,s) = j
                     ENDIF
                     l = i
                  ENDIF
                  IF ( l.LT.r ) GOTO 200
                  IF ( s.NE.0 ) GOTO 100
                  RETURN
               ENDIF
            ELSE
               i = i + 1
               lcvar = Carray(i)
            ENDIF
         ENDDO
 200  ENDDO
      END
