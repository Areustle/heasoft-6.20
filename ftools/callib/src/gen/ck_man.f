
*CK_MAN
      subroutine ck_man(rec,man_key,chatter)
c     --------------------------------------
c --- DESCRIPTION -----------------------------------------------------
c
c This routine checks if rec, the current record contains OGIP PHA
c extension mandatory keywords. If it does the logical man_key is
c set to true.
c
c --- VARIABLES -------------------------------------------------------
c
      IMPLICIT NONE
      character(80) rec
      integer chatter
      logical man_key
      character(70) subinfo
      character(8) keyword
c
c --- VARIABLE DIRECTORY ----------------------------------------------
c
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Rehana Yusaf (1993 April 8)
c Rehana Yusaf (1993 July 21) : DETCHANS added to list of mandatories

      character(5) version
      parameter (version = '1.0.1')
*-
c ---------------------------------------------------------------------
c
c --- USER INFO ---
c
      IF (chatter.GE.20) THEN
         subinfo = ' ... using CK_MAN Ver '//version
         call fcecho(subinfo)
      ENDIF
c
c --- CHECK RECORD WITH MANDATORY KEYWORDS ---
c
      keyword = rec(1:8)
      IF (keyword.EQ.'XTENSION') THEN
        man_key = .true.
      ELSEIF (keyword.EQ.'BITPIX  ') THEN
        man_key = .true.
      ELSEIF (keyword(1:5).EQ.'NAXIS') THEN
        man_key = .true.
      ELSEIF (keyword.EQ.'PCOUNT ') THEN
        man_key = .true.
      ELSEIF (keyword.EQ.'GCOUNT ') THEN
        man_key = .true.
      ELSEIF (keyword.EQ.'TFIELDS ') THEN
        man_key = .true.
      ELSEIF (keyword(1:5).EQ.'TTYPE') THEN
        man_key = .true.
      ELSEIF (keyword(1:5).EQ.'TFORM') THEN
        man_key = .true.
      ELSEIF (keyword(1:5).EQ.'TUNIT') THEN
        man_key = .true.
      ELSEIF (keyword.EQ.'EXTNAME ') THEN
        man_key = .true.
      ELSEIF (keyword.EQ.'STAT_ERR') THEN
        man_key = .true.
      ELSEIF (keyword.EQ.'POISSERR') THEN
        man_key = .true.
      ELSEIF (keyword.EQ.'SYS_ERR ') THEN
        man_key = .true.
      ELSEIF (keyword.EQ.'GROUPING') THEN
        man_key = .true.
      ELSEIF (keyword.EQ.'QUALITY ') THEN
        man_key = .true.
      ELSEIF (keyword.EQ.'DETCHANS') THEN
        man_key = .true.
      ENDIF
      return
      end
c --------------------------------------------------------------------
c     END OF CK_MAN
c --------------------------------------------------------------------

