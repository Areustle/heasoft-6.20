C  FTOOLs info $Header: /headas/headas/ftools/asca/src/sqaplot/gradev50.f,v 3.6 1996/04/16 23:42:25 dunfee Exp $
C      
c	grade routine 
c
c	input:	handle(0:8) integer array forpulse height,
c                                 6 7 8
c                                 4 0 5
c                                 1 2 3
c               split       split threshold
c               echo        echo fraction (current estimate 0.014 for S0 and 0.0087 for S1)
c               qfancy      if true then use fancy echo correction
c	output:	sumph       total ph
c               type        grade
c               above       numbers of pixels summed in *sumph
c
c		version 3.0	91-04-23	by K.Yoshida
c		version 3.1	91-05-01	by K.Yoshida
c		version 4.0	92-12-01	by T.Dotani
c               version 5.0     93-06-10        by K.Arnaud
c    version4.0
c       Modified to adapt the new grade definition:
c         0  single
c         1  single+
c         2  vertical split
c         3  left split
c         4  right split
c         5  single-sided+
c         6  L-shaped & square
c         7  others
c
c      However in this program, 8 is assigned to L-shaped event to use
c      the old version PH summation algorithm.  The number is changed 
c      in the output stage.
c
c    version5.0
c       Added echo correction using Keith Gendreau's algorithm
c
c adapted to fortran and speeded up  93-6-9 kaa
C
C ported to sqaplot for comparisons  93-7-14 gbc

c
c  Parallel to grade_setup of grade.c
c
      SUBROUTINE sqastp(split, echo, style)

      INTEGER csplit,split,style
      LOGICAL cqfancy
      REAL    cecho,echo
      COMMON /gbcyuk/ csplit, cecho, cqfancy

      csplit = split
      cecho = echo
      if (style .gt. 1) then
	cqfancy = .true.
      else
	cqfancy = .false.
      end if

      RETURN
      END
      

c
c  Parallel to do_grading of grade.c
c
      SUBROUTINE sqadgr(handle, sumph, type, above)

      INTEGER*2 handle(0:8), sumph, type, above
      INTEGER   split
      REAL      echo
      LOGICAL   qfancy
      COMMON   /gbcyuk/ split, echo, qfancy

      INTEGER i, n
      LOGICAL qabove(8)
      INTEGER lookup(0:255), pow2(8)

      DATA lookup /
     &          0,1,2,5,1,1,5,7,3,5,8,6,3,5,7,7,
     &          4,4,8,7,5,5,6,7,7,7,7,7,7,7,7,7,
     &          1,1,2,5,1,1,5,7,5,7,7,7,5,7,7,7,
     &          4,4,8,7,5,5,6,7,7,7,7,7,7,7,7,7,
     &          2,2,7,7,2,2,7,7,8,7,7,7,8,7,7,7,
     &          8,8,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     &          5,5,7,7,5,5,7,7,6,7,7,7,6,7,7,7,
     &          7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     &          1,1,2,5,1,1,5,7,3,5,8,6,3,5,7,7,
     &          5,5,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     &          1,1,2,5,1,1,5,7,5,7,7,7,5,7,7,7,
     &          5,5,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     &          5,5,7,7,5,5,7,7,7,7,7,7,7,7,7,7,
     &          6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     &          7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     &          7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7/
      DATA pow2 / 1,2,4,8,16,32,64,128 /

c perform the simple echo correction

        handle(5) = handle(5) - INT(echo * handle(0))

c if the fancy flag is set then also correct 0, 2, 3, 7, 8. In
c general this is not necessary since it will be << 1% effect.

        IF (qfancy) THEN
           handle(0) = handle(0) - INT(echo * handle(4))
           handle(3) = handle(3) - INT(echo * handle(2))
           handle(2) = handle(2) - INT(echo * handle(1))
           handle(8) = handle(8) - INT(echo * handle(7))
           handle(7) = handle(7) - INT(echo * handle(6))
        ENDIF

c use a binary encoding scheme to indicate the pixels above the split
c threshold.

	n = 0
        DO 100 i = 1, 8
           IF (handle(i) .GE. split) THEN
              n = n + pow2(i)
              qabove(i) = .TRUE.
           ELSE
              qabove(i) = .FALSE.
           ENDIF
100	continue

	type = lookup(n)

c sum up the total PHA - this only adds corner pixels for type 6.

	above = 1
	sumph = handle(0)

        IF ( qabove(2) ) THEN
           above = above + 1
           sumph = sumph + handle(2)
        ENDIF
        IF ( qabove(4) ) THEN
           above = above + 1
           sumph = sumph + handle(4)
        ENDIF
        IF ( qabove(5) ) THEN
           above = above + 1
           sumph = sumph + handle(5)
        ENDIF
        IF ( qabove(7) ) THEN
           above = above + 1
           sumph = sumph + handle(7)
        ENDIF

        IF (type .EQ. 6) THEN

           IF ( qabove(1) ) THEN
              IF (qabove(2) .AND. qabove(4)) THEN
                 above = above + 1
                 sumph = sumph + handle(1)
              ENDIF
           ENDIF

           IF ( qabove(3) ) THEN
              IF (qabove(2) .AND. qabove(5)) THEN
                 above = above + 1
                 sumph = sumph + handle(3)
              ENDIF
           ENDIF

           IF ( qabove(6) ) THEN
              IF (qabove(4) .AND. qabove(7)) THEN
                 above = above + 1
                 sumph = sumph + handle(6)
              ENDIF
           ENDIF

           IF ( qabove(8) ) THEN
              IF (qabove(5) .AND. qabove(7)) THEN
                 above = above + 1
                 sumph = sumph + handle(8)
              ENDIF
           ENDIF

        ENDIF

c Set the grade of L-shaped events to 6.

       IF (type .EQ. 8) type = 6

       RETURN
       END



