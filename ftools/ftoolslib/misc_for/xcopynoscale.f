
C******************************************************************************
C SUBROUTINE:
C      xcopynoscale
C
C DESCRIPTION:
C      This subroutine moves the extra keywords,i.e., the
C      keywords which don't contain:
C        SIMPLE
C        BITPIX
C        NAXIS
C        NAXISn
C        EXTEND
C        XTENSION
C        EXTNAME
C        PCOUNT
C        GCOUNT
C        TFIELDS
C        TTYPEn
C        TBCOLn
C        TFORMn
C        TSCALn
C        TZEROn
C        TNULLn
C        TUNITn
C        THEAP
C        TDIMn
C        TDISPn
C        GROUPS
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
C        TDMINn
C        TDMAXn
C        OPTICn
C        TCRPXn
C        TCRVLn
C        TCDLTn
C        TCTYPn
C        TCUNIm
C        TCDnnnn
C        TCROTn
C        PLTSCLn
C        CHECKSUM
C        DATASUM
C        END
C      from the input file to the output file
C
C AUTHOR/DATE:
C
C      James Kent Blackburn 11/05/91
C
C MODIFICATION HISTORY:
C       Copied from FPROJECT routine FIMEXK
C              and modified for FAINT task   EAG  8/4/92
C               modified for FBURST task             EAG 10/14/92
C               and now for FMASKFILT  EAG 4/20/93
C       and to xcopynoscale so this doesn't have to keep being done!
C               EAG 4/21/93
C       7/22/93 Should not copy EXTNAME
C       10/7/93 EAG should not copy TLMINn or TLMAXn
C       12/16/93 EAG should not copy OPTICn, TCRPXn, TCRVLn, TCDLTn, TCTYPn
C       1/25/94 EAG should not copy TCDnnnn, TCROT, PLTSCLn
C       4/1/97  LEB should not copy DATASUM, CHECKSUM
C
C NOTES:
C
C USAGE:
C      call xcopynoscale (iunit,ounit,status)
C
C ARGUMENTS:
C      iunit - input unit number
C      ounit - output unit number
C      status  - error number
C
C PRIMARY LOCAL VARIABLES:
C      i* -index to substring
C      l(*) - substring presence flag
C      nkeys - number of keywords
C      copyflg - copy keyword flag
C
C CALLED ROUTINES:
C      subroutine ftghsp - get number of keywords in extension
C      subroutine ftgrec - get keyword record
C      subroutine ftprec - put keyword record
C
C******************************************************************************
      subroutine xcopynoscale (iunit,ounit,status)

      integer     iunit,ounit,status

      INTEGER NTEST
      PARAMETER (NTEST=48)

      INTEGER     reclen
      integer     i,j,nkeys,nmore,fcstln
      logical     copyflg
      character(8) rectst(NTEST)
      character(80)  record

      SAVE rectst

      DATA rectst/'SIMPLE', 'BITPIX', 'NAXIS', 'EXTEND', 'XTENSION', 
     &            'PCOUNT', 'GCOUNT', 'TFIELDS', 'TTYPE', 'TBCOL', 
     &            'TFORM', 'TSCAL', 'TZERO', 'TNULL', 'TUNIT', 'THEAP',
     &            'TDIM', 'TDISP', 'GROUPS', 'BSCALE', 'BZERO', 
     &            'BUNIT', 'BLANK', 'CTYPE', 'CRPIX', 'CROTA', 'CRVAL',
     &            'CDELT', 'END', 'EXTNAME', 'TLMIN', 'TLMAX', 'OPTIC',
     &            'TCRPX', 'TCRVL', 'TCDLT', 'TCTYP', 'TCROT', 
     &            'PLTSCL', 'TCD', 'CHECKSUM', 'DATASUM', 'MTYPE', 
     &            'MFORM', 'TCNAM', 'TCUNI', 'TDMIN', 'TDMAX'/



C find how many keys there are:
      call ftghsp(iunit,nkeys,nmore,status)

C loop over all keys

      do 10 i = 1, nkeys

         call ftgrec(iunit,i,record,status)

C check if this is a good record

         copyflg = .TRUE.
         DO j = 1, NTEST
            reclen = fcstln(rectst(j))
            copyflg = copyflg .and. 
     &          (index(record(1:reclen),rectst(j)(1:reclen)) .le. 0)
         ENDDO

c if not one of the special keywords then write it out

         if ( copyflg ) call ftprec(ounit,record,status)

 10   continue

C save any additional space that is in the input header
C       if (nmore .gt. 0) call fthdef (ounit, nmore, status)

      return
      end
