      SUBROUTINE xgtrng(string,iparse,nrange,rngnam,descr,irnglw,
     &                  irnghi,irngmn,irngmx,idefrg,qsing,iflag,
     &                  idelim,*,*,*)

c		rashafer   8 June 1986
c			XPARSE subroutine to get a general range from
c			the prompt string (see the XPRRNG subroutine
c			for the details of what constitutes a general
c			string

      CHARACTER*(*)	string
      INTEGER*4	iparse
      INTEGER*4	nrange
      CHARACTER*(*)	rngnam(*),descr
      INTEGER*4	irnglw(*),irnghi(*),irngmn(*),irngmx(*),idefrg(*)
      INTEGER*4	qsing
      INTEGER*4	iflag	

c  Error Conditions 0 - A range parsed (or
c  the field was skipped
c  -1 EOF on corrections
c   1 End of string reached
c   2 Infinite skip
      INTEGER*4	idelim
c     ** Alternate returns
c			*1	EOF
c			*2	end of string
c			*3	Infinite skip
c
      LOGICAL*4 qskip,qdone
      INTEGER*4 ibeg,iend

      CALL xgtarg(string,iparse,ibeg,iend,qskip,iflag,idelim)
      IF(iflag.NE.0) THEN
         IF(iflag.EQ.1) THEN
            RETURN 2
         ELSEIF(iflag.LT.0) THEN
            RETURN 1
         ELSEIF(iflag.EQ.2) THEN
            RETURN 3
         ELSE
            RETURN
         ENDIF
      ENDIF

      IF(qskip)RETURN

      qdone=.false.
      DO WHILE(.NOT.qdone)
         CALL xprrng(string(ibeg:iend),nrange,rngnam,descr,irnglw,
     &               irnghi,irngmn,irngmx,idefrg,.true.,qsing,iflag)
         IF(iflag.LT.0) RETURN 1
         qdone=iflag.EQ.0
         IF(.NOT.qdone) THEN
            CALL xinfix('Replace entire argument:',string,iparse,iflag)
            IF(iflag.NE.0) THEN
               iflag=-1
               RETURN 1
            ENDIF
         ENDIF
      ENDDO
      RETURN
      END
