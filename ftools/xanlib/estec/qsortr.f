      SUBROUTINE qsortr(nmax,array,array1,ipoint)
C modified from algol code to structured fortran,j.c.lewis
C see n.wirth 'algorithms+data structures=programs'
C -quicksort takes an element k (near the middle say),scans the array
C  from the left till an el. gt k is found,scans from the right till an
C  el. lt k is found,& swaps these 2 els.
C  this is continued till all els to left of k are lt k,and all to right
C  are gt k.
C  now do the same for these 'subpartitions'.when all subpartitions
C  are of length 1,array is sorted!!!!
C   use a stack array to keep track of partitions yet to be sorted-
C  always sort the smaller partition first
C  use stack size=log2(nmax)
C
      DIMENSION st(2,30)
      INTEGER r, s, st, itemp, nmax, l, i, j, lmed
      REAL*4 testr, tempr
      INTEGER*4 ipoint(*)
      REAL*4 array(*),array1(*)
C initialise-
      s = 1
      st(1,s) = 1
      st(2,s) = nmax
 100  CONTINUE
      l = st(1,s)
      r = st(2,s)
      s = s - 1
 200  CONTINUE
      i = l
      j = r
      lmed = (l+r)/2
      testr = array(lmed)
 300  CONTINUE
      IF ( array(i).GE.testr ) THEN
 350    CONTINUE
        IF ( testr.GE.array(j) ) THEN
          IF ( i.LE.j ) THEN
            tempr = array(i)
            array(i) = array(j)
            array(j) = tempr
            tempr = array1(i)
            array1(i) = array1(j)
            array1(j) = tempr
            itemp = ipoint(i)
            ipoint(i) = ipoint(j)
            ipoint(j) = itemp
            i = i + 1
            j = j - 1
          END IF
          IF ( i.LE.j ) GO TO 300
          IF ( (j-l).LT.(r-i) ) THEN
            IF ( i.LT.r ) THEN
              s = s + 1
              IF ( s.GT.30 ) WRITE (*,*) ' error in qsort'
              st(1,s) = i
              st(2,s) = r
            END IF
            r = j
          ELSE
            IF ( l.LT.j ) THEN
              s = s + 1
              IF ( s.GT.30 ) WRITE (*,*) ' error in qsort'
              st(1,s) = l
              st(2,s) = j
            END IF
            l = i
          END IF
          IF ( l.LT.r ) GO TO 200
          IF ( s.NE.0 ) GO TO 100
          RETURN
        ELSE
          j = j - 1
          GO TO 350
        END IF
      ELSE
        i = i + 1
        GO TO 300
      END IF
      END
