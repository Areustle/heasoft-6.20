*+TRINST
        subroutine TRINST(inst, inslen, status)
        character*(*) inst
        integer inslen, status

C-----------------------------------------------------------------------
C Description: Translates the passed value inst into the caldb OGIP
C              statndard.  For example, if GIS-1 were passed in then
C              GIS1 would be returned.
C              Warning: The official name is returned using the
C              passed variable, and assumes that input name is
C              already uppercase.
C              NOTE: This is a temporary version which will translate
C                    strange instrument names for the ASCA mission only.
C                    A better version should first check to see if the 
C                    name is valid; if not then check for alternate
C                    spellings.
C
C Arguments:   inst   (i/r): the instrument name to be translated
C              inslen (r):   the length of the inst string
C              status (r):   The status of the returned value
C                            Always returned as 0
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar (1993 Jun 14) Original Version
C              Ron Zellar (1994 Feb 22) Added PSPC-B and PSPC-C
C              Ron Zellar (1994 Jul 18) removed XRT# -> XRT-# conversion
C-----------------------------------------------------------------------
*-Version 1.1

        integer fcstln, length

C       set the status flag to OK
        status = 0

C       get the length of the untranslated string
        length = fcstln(inst)

C        if(inst(:length).eq.'XRT1')then
C             inst = 'XRT-1'
C             inslen = 5
C             return
C        else if(inst(:length).eq.'XRT2')then
C             inst = 'XRT-2'
C             inslen = 5
C             return
C        else if(inst(:length).eq.'XRT3')then
C             inst = 'XRT-3'
C             inslen = 5
C             return
C        else if(inst(:length).eq.'XRT4')then
C             inst = 'XRT-4'
C             inslen = 5
C             return
C        else if(inst(:length).eq.'GIS-2')then

        if(inst(:length).eq.'GIS-2')then
             inst = 'GIS2'
             inslen = 4
             return
        else if(inst(:length).eq.'GIS-3')then
             inst = 'GIS3'
             inslen = 4
             return
        else if(inst(:length).eq.'SIS-0')then
             inst = 'SIS0'
             inslen = 4
             return
        else if(inst(:length).eq.'SIS-1')then
             inst = 'SIS1'
             inslen = 4
             return
	else if(inst(:length).eq.'PSPC-B')then
	     inst = 'PSPCB'
	     inslen = 5
	     return
	else if(inst(:length).eq.'PSPC-C')then
	     inst = 'PSPCC'
	     inslen = 5
	     return
        else
	     inslen = length
             return
        endif

        end
