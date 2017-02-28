      SUBROUTINE TRLNM(TABLE, LT, NAME, LN, VALUE, LV)
C---
C Translate the logical name NAME(:LN) in the table TABLE(:LT)
C to return VALUE(:LV)
* History :
*  20 September 1992 : original UNIX version ignoring TABLE
C---
C TABLE   I    The table containing string to translate
C LT      I    The number of valid characters in TABLE (can be zero)
C NAME    I    The string to translate
C LN      I    The number of valid characters in NAME (can be zero)
C VALUE     O  The translated string
C LV        O  The number of valid characters in VALUE (can be zero)
C---
* Import :
      character*(*) table
      integer lt
      character*(*) name
      integer ln
* Export :
      character*(*) value
      integer lv
*-
      call trlog(name,ln,value,lv)
      return
      end
