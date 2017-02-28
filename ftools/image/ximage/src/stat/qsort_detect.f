      SUBROUTINE QSORT_DETECT(Sortcrit, Nbros, Ndim, Nmax, Status)
      IMPLICIT NONE
c
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
c  Fixed limitation inherit in stack array using dynamic memory.
c  Actual size should be log2(Nmax), not fixed at 15
C
c  this is a modification of paolos qsort.
c
c I/O sortcrit  (r)  Array containing values to be sorted on
c I/O nbros     (i)  Output order corresponding to sort
c  I  ndim      (i)  Dimension of arrays
c  I  nmax      (i)  Number of elements in arrays
c  O  status    (i)  Error flag (0=OK)
c
      INTEGER*4 Ndim, Nmax, Status
      REAL*4 Sortcrit(Ndim)
      INTEGER*4 Nbros(Ndim)

      include '../include/dynmem.inc'
c
c  Local variables
c
      INTEGER*4 r , s , temp, frstat
c     INTEGER*4 st(2,15)
      INTEGER*4 ist, p_st1, p_st2
      INTEGER*4 l , j , i , lmed
      REAL*4 testr , tempr
      logical loop
c
c  Allocate stack array
c
      ist = INT(log10(float(Ndim))/log10(2.0)) + 1
      call workalloc(1, ist, 1, p_st1, status)
      if ( status.ne.0 ) goto 500
      call workalloc(1, ist, 1, p_st2, status)
      if ( status.ne.0 ) goto 500
c
C initialise-
      s = 1
      memi(p_st1+s-1) = 1
      memi(p_st2+s-1) = Nmax
 100  l = memi(p_st1+s-1)
      r = memi(p_st2+s-1)
      s = s - 1
 200  i = l
      j = r
      lmed = (l+r)/2
      testr = Sortcrit(lmed)
      loop = .true.
      DO WHILE ( loop )
         IF ( Sortcrit(i).GE.testr ) THEN
            DO WHILE ( testr.LT.Sortcrit(j) )
               j = j - 1
            ENDDO
            IF ( i.LE.j ) THEN
               tempr = Sortcrit(i)
               Sortcrit(i) = Sortcrit(j)
               Sortcrit(j) = tempr
               temp = Nbros(i)
               Nbros(i) = Nbros(j)
               Nbros(j) = temp
               i = i + 1
               j = j - 1
            ENDIF
            IF ( i.GT.j ) THEN
               IF ( (j-l).LT.(r-i) ) THEN
                  IF ( i.LT.r ) THEN
                     s = s + 1
                     memi(p_st1+s-1) = i
                     memi(p_st2+s-1) = r
                  ENDIF
                  r = j
               ELSE
                  IF ( l.LT.j ) THEN
                     s = s + 1
                     memi(p_st1+s-1) = l
                     memi(p_st2+s-1) = j
                  ENDIF
                  l = i
               ENDIF
               IF ( l.LT.r ) GOTO 200
               IF ( s.NE.0 ) GOTO 100
               goto 500
            ENDIF
         ELSE
            i = i + 1
         ENDIF
      ENDDO
c
c  Free stack array
c
  500 continue
      if ( status.ne.0 ) then
         call xwrite(' Failed to allocate stack for qsort', 5)
         return
      endif
      call workalloc(0, ist, 1, p_st1, frstat)
      call workalloc(0, ist, 1, p_st2, frstat)

      END
