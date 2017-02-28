
C******************************************************************************
C SUBROUTINE:
C     copyhead
C
C DESCRIPTION:
C     This subroutine copies keywords from iunit to ounit under certain
C     conditions.
C
C USAGE:
C
C     copyhead(iunit,ounit,no_basic,no_scale,BADKEY,GOODKEY,status)
C
C ARGUMENTS:
C     iunit - input logical unit positioned at desired extension
C     ounit - output logical unit positioned at desired (existing) extension
C     no_basic - if TRUE, do not copy keywords that contain:
C        SIMPLE
C        BITPIX
C        NAXIS
C        NAXISn
C        EXTEND
C        EXTNAME
C        XTENSION
C        PCOUNT
C        GCOUNT
C        TFIELDS
C        TTYPEn
C        TBCOLn
C        TFORMn
C        TUNITn
C        THEAP
C        TDIMn
C        GROUPS
C        DATASUM
C        CHECKSUM
C        END
C        ZIMAGE
C        ZCMPTYPE
C        ZNAMEn
C        ZVALn
C        ZTILEn
C        ZBITPIX
C        ZNAXISn
C        ZSCALE
C        ZZERO
C        ZBLANK
C        
C     no_scale - if TRUE do not copy keywords which contain:
C        TSCALn
C        TZEROn
C        TNULLn
C        TDISPn
C        BSCALE
C        BZERO
C        BUNIT
C        BLANK
C        CTYPEn
C        CRPIXn
C        CROTAn
C        CRVALn
C        CDELTn
C        TLMINn
C        TLMAXn
C        OPTICn
C        TCRPXn
C        TCRVLn
C        TCDLTn
C        TCTYPn
C        TCDnnnn
C        TCROTn
C        PLTSCLn
C     BADKEY - the name of a logical function which takes a character(80)
C              argument (a fits header keyword record) and returns .TRUE. if
C              it should not be copied. I.e. BADKEY(KEY_RECORD) returns the
C              answer to the question "Is this a bad key to copy?"  Give the
C              name BADDUMMY (remember to declare it EXTERNAL) if you don't
C              have any bad keywords to specify.
C
C     GOODKEY - the name of a logical function which takes a character(80)
C              argument (a fits header keyword record) and returns .TRUE. if
C              it MUST be copied. I.e. GOODKEY(KEY_RECORD) returns the
C              answer to the question "Is this a good key that I MUST copy
C              regardless of what anyone else has told me?"  Thus, GOODKEY
C              overides the prohibitions of the three previous parameters.
C              Give the name GOODDUMMY (remember to declare it EXTERNAL) if
C              you don't have any "good" keywords to specify.
C
C NOTES:
C
C     1. iunit and ounit should be positioned to the correct extensions.  The
C        output extension must already exist (e.g. via ftcrhd) and if you
C        specify no_basic, you should have already written the basic keywords
C        (or at least the XTENSION keyword) to the output extension, probably
C        with FTPHBN or some such.
C
C     2. Anytime no_basic is FALSE, the END keyword WILL be copied.
C
C     3. Calling with no_basic=.FALSE. and no_scale=.TRUE. is NOT
C        recommended (frankly I can't imagine why you'd want to). In
C        particular the END keyword could cause problems and should
C        be handled by your BADKEY routine.
C
C     4. You may call BADDUMMY and/or GOODDUMMY (located in the library where
C        you found this routine) for BADKEY and/or GOODKEY if you do not wish
C        to use that functionality (see examples).
C
C     5. To remember the, somewhat confusing, specifications for BADKEY and
C        GOODKEY, the BADKEY function should answer the question "Is it a
C        BAD keyword to copy?".  The GOODKEY function should answer "Is it
C        a GOOD keyword that I MUST copy regardless of what anyone else has
C        told me?"
C
C     6. If you call  copyhead(.false.,.false.,BADDUMMY,GOODDUMMY,status) it
C        will copy ALL the keywords in the input unit, so be careful.
C
C     7. If you call copyhead(.false.,.false.,BADDUMMY,ANYFUNCTION,status) it
C        will still copy ALL the keywords in the input unit.  The GOODKEY
C        function only OVERIDES previous prohibitions.
C
C
C EXAMPLES:
C
C     To duplicate the functionality of the old xcopyscale routine
C     (i.e. copy everything but the basic header keywords):
C
C     call copyhead(lin,lout,.true.,.false.,BADDUMMY,GOODDUMMY,status)
C
C     To duplicate the functionality of the old xcopynoscale routine
C     (i.e. don't copy ANY column related keywords)
C
C     call copyhead(lin,lout,.true.,.true.,BADDUMMY,GOODDUMMY,status)
C
C     To copy "all the usual things" except the DETCHANS keyword
C
C     call copyhead(lin,lout,.true.,.true.,badkey,GOODDUMMY,status)
C
C     where badkey is defined
C
C      logical function badkey(keyrec)
C      character(80) keyrec
C      badkey=(index(keyrec(1:8),'DETCHANS') .gt. 0)
C      return
C      end
C
C     To copy no basic keywords and no scaling keywords except the OPTICn
C     keywords and copy all other keywords:
C
C     call copyhead(lin,lout,.true.,.true.,BADDUMMY,T_IF_OPTIC,status)
C
C     where T_IF_OPTIC is a function which returns .TRUE. if the
C     first five characters of it's argument are 'OPTIC'.
C
C     To copy only the HISTORY records:
C
C     call copyhead(lin,lout,.false.,.false.,T_IF_NOT_HISTORY,GOODDUMMY,status)
C
C     where T_IF_NOT_HISTORY is a function which returns true if the
C     first 7 characters of the input string are NOT 'HISTORY'. Notice that
C     this will prevent copying of the END keyword.
C
C
C
C AUTHOR/DATE:
C
C     Lawrence E. Brown 1/9/95
C
C MODIFICATION HISTORY:
C
C     BASIC and SCALE keyword lists taken from XCOPYSCALE and XCOPYNOSCALE.
C
C
C CALLED ROUTINES:
C      subroutine ftghsp - get number of keywords in extension
C      subroutine ftgrec - get keyword record
C      subroutine ftprec - put keyword record
C
C******************************************************************************
      subroutine copyhead(iunit,ounit,no_basic,no_scale,
     $     BADKEY,GOODKEY,status)
      implicit none
      integer iunit,ounit,status
      logical no_basic,no_scale
      logical BADKEY,GOODKEY
      external BADKEY,GOODKEY
C     LOCAL variables
      logical copy
      integer i,nkeys,nmore,fcstln
      character(80)  record


C find how many keys there are:
      call ftghsp(iunit,nkeys,nmore,status)

C loop over all keys
      do 10 i = 1, nkeys
         call ftgrec(iunit,i,record,status)
C        the default behavior is to copy a record
         copy=.true.
         if(no_basic) then
            copy=copy.and.(index(record(1:6),'SIMPLE') .le. 0)
            copy=copy.and.(index(record(1:6),'BITPIX') .le. 0)
            copy=copy.and.(index(record(1:5),'NAXIS') .le. 0)
            copy=copy.and.(index(record(1:6),'EXTEND') .le. 0)
            copy=copy.and.(index(record(1:8),'XTENSION') .le. 0)
            copy=copy.and.(index(record(1:6),'PCOUNT') .le. 0)
            copy=copy.and.(index(record(1:6),'GCOUNT') .le. 0)
            copy=copy.and.(index(record(1:7),'TFIELDS') .le. 0)
            copy=copy.and.(index(record(1:5),'TTYPE') .le. 0)
            copy=copy.and.(index(record(1:5),'TBCOL') .le. 0)
            copy=copy.and.(index(record(1:5),'TFORM') .le. 0)
            copy=copy.and.(index(record(1:5),'TUNIT') .le. 0)
            copy=copy.and.(index(record(1:5),'THEAP') .le. 0)
            copy=copy.and.(index(record(1:4),'TDIM') .le. 0)
            copy=copy.and.(index(record(1:6),'GROUPS') .le. 0)
            copy=copy.and.(index(record(1:3),'END') .le. 0)
            copy=copy.and.(index(record(1:7),'EXTNAME') .le. 0)
            copy=copy.and.(index(record(1:8),'CHECKSUM') .le. 0)
            copy=copy.and.(index(record(1:7),'DATASUM') .le. 0)
            if (record(1:1) .eq. 'Z') then
                copy=copy.and.(index(record(1:6),'ZIMAGE') .le. 0)
                copy=copy.and.(index(record(1:8),'ZCMPTYPE') .le. 0)
                copy=copy.and.(index(record(1:5),'ZNAME') .le. 0)
                copy=copy.and.(index(record(1:4),'ZVAL') .le. 0)
                copy=copy.and.(index(record(1:5),'ZTILE') .le. 0)
                copy=copy.and.(index(record(1:7),'ZBITPIX') .le. 0)
                copy=copy.and.(index(record(1:6),'ZNAXIS') .le. 0)
                copy=copy.and.(index(record(1:6),'ZSCALE') .le. 0)
                copy=copy.and.(index(record(1:5),'ZZERO') .le. 0)
                copy=copy.and.(index(record(1:6),'ZBLANK') .le. 0)
            endif
         endif
         if(no_scale) then
            copy=copy.and.(index(record(1:5),'TSCAL') .le. 0)
            copy=copy.and.(index(record(1:5),'TZERO') .le. 0)
            copy=copy.and.(index(record(1:5),'TNULL') .le. 0)
            copy=copy.and.(index(record(1:5),'TDISP') .le. 0)
            copy=copy.and.(index(record(1:6),'BSCALE') .le. 0)
            copy=copy.and.(index(record(1:5),'BZERO') .le. 0)
            copy=copy.and.(index(record(1:5),'BUNIT') .le. 0)
            copy=copy.and.(index(record(1:5),'BLANK') .le. 0)
            copy=copy.and.(index(record(1:5),'CTYPE') .le. 0)
            copy=copy.and.(index(record(1:5),'CRPIX') .le. 0)
            copy=copy.and.(index(record(1:5),'CROTA') .le. 0)
            copy=copy.and.(index(record(1:5),'CRVAL') .le. 0)
            copy=copy.and.(index(record(1:5),'CDELT') .le. 0)
            copy=copy.and.(index(record(1:5),'TLMIN') .le. 0)
            copy=copy.and.(index(record(1:5),'TLMAX') .le. 0)
            copy=copy.and.(index(record(1:5),'OPTIC') .le. 0)
            copy=copy.and.(index(record(1:5),'TCRPX') .le. 0)
            copy=copy.and.(index(record(1:5),'TCRVL') .le. 0)
            copy=copy.and.(index(record(1:5),'TCDLT') .le. 0)
            copy=copy.and.(index(record(1:5),'TCTYP') .le. 0)
            copy=copy.and.(index(record(1:5),'TCROT') .le. 0)
            copy=copy.and.(index(record(1:6), 'PLTSCL') .le. 0)
            copy=copy.and.(index(record(1:3), 'TCD') .le. 0 .and.
     &           fcstln(record) .ge. 7)
      endif
      copy=copy.and.(.not.(BADKEY(record)))
      copy=copy.or.(GOODKEY(record))

      if ( copy) call ftprec(ounit,record,status)

      if(status.ne.0) then
         call fcerr(
     $'COPYHEAD had trouble while copying the header record:')
         call fcerr(record)
         call fcerr('Copying aborted.')
         return
      endif

 10   continue
      return
      end
