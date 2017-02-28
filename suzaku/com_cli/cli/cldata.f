C
C File: cldata.f
C Description: Block Data initialization for common variables
C Author: Y.ISHISAKI, Tokyo Metropolitan University
C
C History:
C     23-Feb-2005 Y.ISHISAKI, created for block data initialization for common
C     23-Feb-2005 Y.ISHISAKI, block data CLIBLK -> CLDATA moved from chard.f
C
C *********************************************************************

      BLOCK Data CLDATA
      Implicit None
C
C Common
      include 'clidef.inc'
      include 'clflag.inc'
      include 'clunit.inc'
      include 'clitxt.inc'

C      include 'clidef.inc'
C
C      Data IBSLA     / '5C5C5C5C'X /     ! back slash
C      Data ICRET     / '0D0D0D0D'X /     ! carriage return
C      Data IQUOTE    / '27272727'X /     ! single quote
C      Data IBQUOTE   / '60606060'X /     ! back quote
C      Data ISPACE    / '20202020'X /     ! white space
C      Data ITAB      / '09090909'X /     ! tab
C      Data ICONTMARK / '5C5C5C5C'X /     ! continulation line mark
C
C Changed the above to their integer equivalents to avoid type disagreement
C problems at compile time:
C
      Data IBSLA     / 1549556828 /     ! back slash
      Data ICRET     /  218959117 /     ! carriage return
      Data IQUOTE    /  656877351 /     ! single quote
      Data IBQUOTE   / 1616928864 /     ! back quote
      Data ISPACE    /  538976288 /     ! white space
      Data ITAB      /  151587081 /     ! tab
      Data ICONTMARK / 1549556828 /     ! continulation line mark
      Data INULL     /          0 /     ! null character
      Data ILINEFEED /  168430090 /     ! line feed
C
C      include 'clflag.inc'
C
      Data IFLQ / 1 /
      Data IFCR / 1 /
      Data ICOM / 0 /
      Data ILOG / 0 /
      Data INHLOG / 0 /
      Data ICONCAT / 0 /
      Data ISKIP / 0 /
      Data ISKIP_BEFORE_COMMENT / 0 /
      Data FLAG_COMMENT / 0 /
      Data IF_DEPTH / 0 /
C
      Data Opt_AT       / 1 /
      Data Opt_ECHO     / 1 /
      Data Opt_BREAK    / 0 /
      Data Opt_DEBUG    / 0 /
      Data Opt_ALIAS    / 1 /
      Data Opt_DOLLAR   / 1 /
      Data Opt_PERCENT  / 0 /
      Data Opt_BSLASH   / 1 /
      Data Opt_INCO     / 1 /
      Data Opt_HISTORY  / 1 /
      Data Opt_EOF      / 1 /
C
C      include 'clunit.inc'
C
      Data LUNPTR / 0 /
      Data LUNLST / 5,81,82,83,84,85,86,87,88 /
      Data LOOP_DEPTH / 0 /
      Data LOOP_READ_DEPTH / 0 /
      Data LARGS  / 0, 0, 0, 0, 0, 0, 0, 0, 0 /
      Data NLABEL / 0 /
      Data LSEARCH_PATH / 0 /
      Data NSUB / 0 /
      Data FLAG_SUB_READING / 0 /
C
C      include 'clitxt.inc'
C
      Data cLAST / 0 /
      Data IPNT / 0 /
C
      End
