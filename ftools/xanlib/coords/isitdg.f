      function isitdg(string)
c
c provide context sensitive interpretation of coordinate input
c
c string is an input of what the user specifes for ra or dec
c return is a logical spcifying the type true = decimal degrees
c                                      false = hr mn sec or deg mn sec
c
      character*(*) string
      logical*4 isitdg
      integer*4 lenact, ibl, in, idot
c
c get rid of leading blanks
c
      call rmvlbk(string)
      in = lenact(string)
c
c if string is blank return assuming hr mn sec
c
      if(in.eq.0)then
       isitdg = .false.
       return
      endif
c
c find the first '.'
c
      idot = index(string,'.')
c
c if no dots must be hms or dms
c
      if(idot.eq.0)then
       isitdg=.false.
       return
      endif
c
c check for 1st blank
c
      ibl = index(string,' ')
c
c is 1st blank after dot, if so it must be isitdg otherwise its not
c
      if(ibl.ge.idot)then
       isitdg = .true.
      else
       isitdg = .false.
      endif
      return
      end
