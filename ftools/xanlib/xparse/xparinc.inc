C xparinc.inc
C Include file for xparse to define the xparsc common block
C
C XPRMSG   c256 Message work space, used for input and output
C               -of messages
C XPRMSW   c256 Additional string work space
C IXPIOE   i4   Last io status value
C QXCASE   l4   If ture, case is not significant in matches
C QXPART   L4   If true, the entire string need not be matched
C QXPFIR   L4   If ture, the first partial match is accepted, without
C               -raising the error condition
C XPREOF   c4   Special EOF string
C LXPEOF   i4   Lenact of xpreof
C BLANK    c1   Blank
C COMMNT   c1   Comment character
C OPNSTR   c1   Open string character
C CLSSTR   c1   Close string character
C COMMA    c1   Ordinary break character
C SKPALL   c1   Special break character (end of parse)
C SPCBR1   c1   Special break 1
C SPCBR2   c1   Special break 2
C INDRCT   c1   Indirect file indicator
C CONTIN   c1   Continuation char
C TAB      c1   Tab char
C COMMAND  c1   Command char
C OPSYS    c1   Operating systems char
C INQUIRY  c1   The inquiry char (for help purposes)
C XXXSPR   c2   Spare
C LGUNIT   i4   NO LONGER USED, SHOULD BE DELETED NEXT TIME FILE IS MODIFIED.
C TRMCHT   i4   The terminal chattyness
C LOGCHT   i4   The logfile chattyness
C REQUEST_INQUIRY  c4   The request prompt
C REQUIRE_INQUIRY  l4   If true, then prompts are required
C RETURN_INQUIRY   l4   Give prompts on the current line
C*****
C  Added by AMTP - 14 September 1993
C TESTPR           l4   If true, then ignore delimiters within parentheses
C OPNPR            c1   Opening parenthesis
C CLSPR            c1   Closing parenthesis
C
      character(256) XPRMSG,XPRMSW
      character(4) XPREOF, REQUEST_INQUIRY
      character(2) XXXSPR
      character(1) BLANK,COMMNT,OPNSTR,CLSSTR,COMMA,SKPALL,SPCBR1,
     &            SPCBR2,INDRCT,CONTIN,TAB,COMMAND,OPSYS,INQUIRY,
     &            OPNPR,CLSPR
      INTEGER   IXPIOE,LXPEOF,LGUNIT,TRMCHT,LOGCHT
      LOGICAL   QXCASE,QXPART,QXPFIR,REQUIRE_INQUIRY,RETURN_INQUIRY
      LOGICAL   TESTPR
      COMMON/XPARSC/XPRMSG,XPRMSW,IXPIOE,QXCASE,QXPART,QXPFIR,XPREOF,
     &      LXPEOF,BLANK,COMMNT,OPNSTR,CLSSTR,COMMA,SKPALL,SPCBR1,
     &      SPCBR2,INDRCT,CONTIN,TAB,COMMAND,OPSYS,INQUIRY,XXXSPR,
     &      LGUNIT,TRMCHT,LOGCHT,
     &      REQUEST_INQUIRY,REQUIRE_INQUIRY,RETURN_INQUIRY,
     &      TESTPR,OPNPR,CLSPR
      SAVE   /XPARSC/
C
