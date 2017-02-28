c  key               Keywords used in selections
c  keyval            Lower and upper bounds for selections
c  nkeys             Number of keywords

      DOUBLE PRECISION    keyval(3,MAXCOLS)
      INTEGER             nkeys
      character(20)        key(MAXCOLS)


      COMMON / ext_keys / keyval, nkeys, key



