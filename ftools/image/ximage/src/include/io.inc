c io.inc - general i/o
c
c  log file write string (used by xwrite)
c
      character(255) zwrite
      COMMON /zwritebuf/ zwrite
c
c parsing stuff (used by most of the xparse routines)
c
c	zstring		: input string from parser
c	zparse		: parse location
c	zbeg		: start location of current option
c	zend		: end location of current option
c
      INTEGER*4 zbeg, zend, zparse
      character(256) zstring
c
      COMMON /istring/ zstring
      common /ilength/ zparse, zbeg, zend
c
