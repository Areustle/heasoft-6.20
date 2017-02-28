*+TRTELE

       SUBROUTINE TRTELE(TELVAL, TELLEN, STATUS)

       character*(*) TELVAL
       integer tellen, status
 
C*****************************************************************
C Description:  TRTELE translates a TELESCOP/MISSION name and 
C               returns the official CALDB name.
C               For example, if ASTRO-D were passed in, then
C               ASCA would be returned.
C               If the MISSION name is unknown, then it is
C               returned without error.
C               Warning: The official name is returned using the
C               passed variable, and assumes that input name is 
C               already uppercase.
C
C Arguments:    TELVAL      (I) : The mission name
C               TELLEN      (r) : The length of the returned 
C                                 Telescop value 
C               STATUS      (r) : The status of the returned 
C                                 value -- Always = 0
C
C Origin:       Written for the Calibration Database
C
C Authors/Modification History:
C               Ron Zellar (1993 Mar 19) original version
C		Ron Zellar (1994 Feb 22) Will not return error
C                                        if telval unknown.  Added
C                                        GENERAL and GEN.
C
C*****************************************************************
*-Version 2.0

         integer length, fcstln

C        set the status flag to 'OK!'
         status = 0

C        get the length of telval
         length = fcstln(telval)

C	If telval is one of these values, then its OK.
	If( (Telval(:length) .eq. 'ASCA'     ) .or.
     &	    (Telval(:length) .eq. 'ROSAT'    ) .or.
     &	    (Telval(:length) .eq. 'EINSTEIN' ) .or.
     &	    (Telval(:length) .eq. 'EXOSAT'   ) .or.
     &	    (Telval(:length) .eq. 'VELA5B'   ) .or.
     &	    (Telval(:length) .eq. 'ARIEL-V'  ) .or.
     &	    (Telval(:length) .eq. 'COS-B'    ) .or.
     &	    (Telval(:length) .eq. 'BBXRT'    ) .or.
     &	    (Telval(:length) .eq. 'GINGA'    ) .or.
     &	    (Telval(:length) .eq. 'HEAO-1'   ) .or.
     &	    (Telval(:length) .eq. 'CGRO'     ) .or.
     &	    (Telval(:length) .eq. 'GEN'      ) .or.
     &	    (Telval(:length) .eq. 'SAS3'     ) ) Then
		TELLEN = length
		Return
	Endif

C       If TELVAL doesn't equal any of the above, then
C       find out which TELVAL has been input and correct it

	If( (Telval(:length) .eq. 'ASTRO-D' ) .or.
     &	    (Telval(:length) .eq. 'ASTRO_D' ) .or.
     &	    (Telval(:length) .eq. 'ASTROD'  ) .or.
     &	    (Telval(:length) .eq. 'ASUKA'   ) ) THEN
		Telval='ASCA'
		TELLEN = 4

	Else If( (Telval(:length) .eq. 'ARIELV' ) .or.
     &		 (Telval(:length) .eq. 'ARIEL5' ) .or.
     &		 (Telval(:length) .eq. 'ARIEL-5') ) THEN
		 Telval='ARIEL-V'
		 TELLEN = 7

	Else If  (Telval(:length) .eq. 'COSB' ) THEN
		 Telval = 'COS-B'
		 TELLEN = 5

	Else If( (Telval(:length) .eq. 'HEAO-2' ) .or.
     &		 (Telval(:length) .eq. 'HEAO2'  ) .or.
     &		 (Telval(:length) .eq. 'HEAO-B' ) .or.
     &		 (Telval(:length) .eq. 'HEAOB'  ) ) THEN
		 Telval = 'EINSTEIN'
		 TELLEN = 8

	Else If( (Telval(:length) .eq. 'ASTRO-C' ) .or.
     &		 (Telval(:length) .eq. 'ASTRO_C' ) .or.
     &		 (Telval(:length) .eq. 'ASTROC'  ) ) THEN
		 Telval = 'GINGA'
		 Tellen = 5

	Else If( (Telval(:length) .eq. 'HEAO1'  ) .or.
     &		 (Telval(:length) .eq. 'HEAO_1' ) ) Then
		 Telval = 'HEAO-1'
		 Tellen = 6

	Else If  (Telval(:length) .eq. 'GENERAL' ) Then
		 Telval = 'GEN'
		 Tellen = 3

C       If telval doesn't equal any of the above then,
C       assume that the mission value is valid and that 
C       this subroutine has not been updated to include it.

	ENDIF

	TELLEN = length
 
	Return
	END
