!      
!	grade routine 
!
!	input:	handle(0:8) integer array forpulse height,
!                                 6 7 8
!                                 4 0 5
!                                 1 2 3
!               split       split threshold
!               echo        echo fraction (current estimate 0.014 for S0 and 0.0087 for S1)
!               qfancy      if true then use fancy echo correction
!	output:	sumph       total ph
!               type        grade
!               above       numbers of pixels summed in *sumph
!
!		version 3.0	91-04-23	by K.Yoshida
!		version 3.1	91-05-01	by K.Yoshida
!		version 4.0	92-12-01	by T.Dotani
!               version 5.0     93-06-10        by K.Arnaud
!    version4.0
!       Modified to adapt the new grade definition:
!         0  single
!         1  single+
!         2  vertical split
!         3  left split
!         4  right split
!         5  single-sided+
!         6  L-shaped & square
!         7  others
!
!      However in this program, 8 is assigned to L-shaped event to use
!      the old version PH summation algorithm.  The number is changed 
!      in the output stage.
!
!    version5.0
!       Added echo correction using Keith Gendreau's algorithm
!
! adapted to fortran and speeded up  93-6-9 kaa

      SUBROUTINE faint_v50(handle, split, echo, qfancy, sumph, 
     &                     type, above)

      INTEGER handle(0:8), split, sumph, type, above
      REAL echo
      LOGICAL qfancy

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

! perform the simple echo correction

!      write(*,*) 'handle: ',handle
!      write(*,*) 'split: ',split
!      write(*,*) 'echo: ',echo
!      write(*,*) 'qfancy: ',qfancy
!      write(*,*) 'sumph: ',sumph
!      write(*,*) 'type: ',type
!      write(*,*) 'above: ',above
      
      handle(5) = handle(5) - INT(echo * handle(0))

! if the fancy flag is set then also correct 0, 2, 3, 7, 8. In
! general this is not necessary since it will be << 1% effect.

        IF (qfancy) THEN
           handle(0) = handle(0) - INT(echo * handle(4))
           handle(3) = handle(3) - INT(echo * handle(2))
           handle(2) = handle(2) - INT(echo * handle(1))
           handle(8) = handle(8) - INT(echo * handle(7))
           handle(7) = handle(7) - INT(echo * handle(6))
        ENDIF

! use a binary encoding scheme to indicate the pixels above the split
! threshold.

	n = 0
        DO 100 i = 1, 8
           IF (handle(i) .LT. 4095) THEN
              IF (handle(i) .GE. split) THEN
                 n = n + pow2(i)
                 qabove(i) = .TRUE.
              ELSE
                 qabove(i) = .FALSE.
              ENDIF
           ELSE
              qabove(i) = .FALSE.
           ENDIF
100	continue
	type = lookup(n)

! sum up the total PHA - this only adds corner pixels for type 6.

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

! Set the grade of L-shaped events to 6.

       IF (type .EQ. 8) type = 6

       RETURN
       END



