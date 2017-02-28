C
C       AECON --   ASCII-EBCDIC CONVERSION SUBROUTINE
C
C	$Id: aecon.f,v 3.2 2013/05/21 19:08:27 irby Exp $
C       VERSION:  V.03
C
C       MODE<0 IS EBCDIC TO ASCII;  MODE>=0 IS ASCII TO EBCDIC;
C       'A' IS ARRAY TO BE CONVERTED;  'LENG' IS LENGTH (IN BYTES)
C       'IERR' IS NUMBER OF CONVERSION ERRORS.
C
C       WALTER PAULI, NASA GSFC, O8/01/76
C
C
C       MODIFIER        DATE            EXPLANATION
C       --------        ----            -----------
C
C       N.LAUBENTHAL    12/80   CHANGE CODE SO THAT PROCESSING
C                               DOES NOT BRANCH OUT OF LOOP AND
C                               THEN BACK IN FOR CONVERSION ERRORS.
C                               ALSO, ADD CONVERSIONS FOR 6 OTHER
C                               CHARACTERS WITH ASCII VALUES OF
C                               134,137,140,173,175,176 (OCTAL).
C
C       A.ETIENNE        2/85   CHANGE DECLARATIONS OF ARRAYS B AND
C                               C FROM L*1 TO I*2 SINCE LOCICALS
C                               CANNOT BE TREATED AS INTEGERS UNDER
C                               IBM FORTRAN. REMOVE VARIABLE CHAR
C                               AND PERFORM CONVERSION OF A(I) BY
C                               MOVING BYTES (KMVC) INSTEAD OF BY
C                               ASSIGNMENT. RENAME VARIABLE LEN TO LENG
C                               TO AVOID CONFLICTS WITH BUILTIN FUNCTION
C	E.S.Panduranga	09/15/91
C				Moved code to SUN.
C				Added RCS Id and Log lines. 
C				Declared variables not declared on IBM.
C ----------------------------------------------------------------------------
C $Log: aecon.f,v $
C Revision 3.2  2013/05/21 19:08:27  irby
C Change character*n to character(n) to silence warnings: "Obsolescent
C feature: Old-style character length".
C
C Revision 3.1  2002/04/16 20:32:07  irby
C Additions to libgro - previously these codes existed in the following
C libraries:
C
C   libsenstv
C   libsysutil
C   libutil
C   libftio
C
c Revision 2.1  1991/10/08  21:48:04  esp
c First controlled version on the SUN.
c
C ----------------------------------------------------------------------------

        SUBROUTINE AECON(MODE,A,LENG,IERR)

	integer	  mode, leng, ierr, i
        LOGICAL*1 A(LENG)
        INTEGER*2 DIG,B(128),C(256)
        DATA DIG/0/
C
C       ASCII TO EBCDIC SUBSTITUTE ARRAY
        DATA B/0,1,2,3,55,45,46,47,22,5,37,11,12,13,
     *         14,15,16,17,18,64,60,61,50,38,24,25,63,
     *         39,34,64,53,64,64,90,127,123,91,108,
     *         80,125,77,93,92,78,107,96,75,97,240,
     *         241,242,243,244,245,246,247,248,249,122,
     *         94,76,126,110,111,124,193,194,195,196,
     *         197,198,199,200,201,209,210,211,212,213,
     *         214,215,216,217,226,227,228,229,230,231,
     *          232,233,64,224,64,64,109,121,
     *          129,130,131,132,133,134,135,
     *         136,137,145,146,147,148,149,150,151,152,
     *         153,162,163,164,165,166,167,168,169,192,79,
     *         208,161,7/
C
C       EBCDIC TO ASCII SUBTITUTE ARRAY
        DATA C/0,1,2,3,32,9,32,127,2*0,32,11,12,13,14,
     *         15,16,17,18,3*32,8,32,24,25,8*32,28,
     *         0,32,10,23,27,2*0,2*32,0,5,6,7,2*0,22,
     *         0,32,30,32,4,3*0,32,20,21,0,26,32,9*0,
     *         32,46,60,40,43,124,38,9*0,33,36,42,41,
     *         59,32,45,47,9*0,44,37,95,62,63,9*0,96,58,
     *         35,64,39,61,34,0,97,98,99,100,101,102,
     *         103,104,105,7*0,106,107,108,109,110,111,
     *         112,113,114,7*0,126,115,116,117,118,119,120,
     *         121,122,22*0,123,65,66,67,68,69,70,71,72,73,
     *         6*0,125,74,75,76,77,78,79,80,81,82,6*0,92,0,
     *          83,84,
     *         85,86,87,88,89,90,6*0,48,49,50,51,52,53,
     *         54,55,56,57,124,5*0/

	character(80)	rcsid
	common	/id/	rcsid
	rcsid = '$Id: aecon.f,v 3.2 2013/05/21 19:08:27 irby Exp $'

        IERR=0
        IF(LENG.EQ.0) RETURN
        IF(MODE.LT.0) GOTO 50
C
C       ASCII TO EBCDIC ROUTINE
C
        DO 20 I=1,LENG
           CALL KMVC(DIG,2,A(I),1,1)
           IF(DIG.LT.128) GO TO 40
              IERR=IERR+1
              GOTO 20
   40      CONTINUE
              CALL KMVC(A(I),1,B(DIG+1),2,1)
20      CONTINUE
        RETURN
C
C       EBCDIC TO ASCII ROUTINE
C
   50   CONTINUE
        DO 60 I=1,LENG
           CALL KMVC(DIG,2,A(I),1,1)
           CALL KMVC(A(I),1,C(DIG+1),2,1)
60      CONTINUE
C
        RETURN
        END
