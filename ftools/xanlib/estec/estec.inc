c
c common includes for estec 
c
c root directory name (filled by get_root)
c current directory name
c
      character(100) zrootname, zcurrent_directory, zsysdir
      COMMON /estecrt/ zrootname, zcurrent_directory, zsysdir
c
c  log file write string (used by xwrite)
c
      character(256) zwrite
      COMMON /estecw/ zwrite
c
c defaults file lu number
c
      integer*4 zlun_def
      common /estecd/zlun_def
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
      COMMON /estecp/ zstring, zparse, zbeg, zend
c
c
