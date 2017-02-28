      subroutine evaltmpl (line, mapid, keyword, di, dd, ds, type,
     &                     comment, status)

      implicit none
c
c  Evaluates header template line and returns result in
c    appropriate type variable (di,dd,ds) indicated by type
c
c  I  line    (c)  header template line
c  I  mapid   (c)  map id used to evaluate keywords
c  O  keyword (c)  keyword name from template line
c  O  di      (i)  integer value
c  O  dd      (d)  double value
c  O  ds      (d)  string value
c  O  type    (c)  data type ( I = integer, D = double, S = string )
c                   blank for no value
c  O  comment (d)  comment from template line
c  O  status  (i)  error flag (0=OK)
c
      character*(*) line, mapid, keyword, ds, comment
      integer*4 di
      real*8 dd
      character(1) type
c
c  Local variables
c
      integer*4 hdtype, status, LENACT
      character(80) value, card, hdrkey, longkey
      integer*4 ibeg, iend, keylen
      character(255) cmd

      character(1) singlequote, backslash

      singlequote = ''''
      backslash = CHAR(92)
      status = 0
      keyword = ' '
      type = ' '
      ds = ' '

      if ( line(1:1).eq.'#' .or. line(1:1).eq.'!') return

      call ftgthd(line, card, hdtype, status)
      if ( hdtype.ne.0 ) return
      call ftgknm(card, longkey, keylen, status)
      if ( keylen .gt. 8 ) then
         call xwrite(' WARNING: Truncating keyword to eight characters', 
     &               10)
      endif
      keyword = longkey(1:8)

      call ftpsvc(card, value, comment, status)
      ibeg = 1
      iend = LENACT(value)
c
c  Strip single quotes
c
      if ( value(ibeg:ibeg).eq.singlequote .and. 
     &     value(iend:iend).eq.singlequote) then
         ibeg = ibeg + 1
         iend = iend - 1
      endif
c
c  Message type uses backslash to begin 'keyword'
c
      if ( keyword(1:1).eq.backslash ) then
         keyword = keyword(2:LENACT(keyword))
         type = 'M'
         ds = value(ibeg:iend)
         return
      endif
c
c  Get type
c
      if ( value(ibeg:ibeg).eq.backslash ) then
         type = 'S'
      elseif ( value(ibeg:ibeg).eq.'(' ) then
c
c        Evaluate expression
c
         type = 'D'
         cmd = 'xan::chhexpr '//mapid//' {'//value(ibeg:iend)//'}'
         call tclresd(cmd, dd, status)
         return
      elseif ( value(ibeg:ibeg).eq.' ' ) then 
         type = '?'
      else
         type = value(ibeg:ibeg)
         if ( type.eq.'!' ) type = 'I'
         if ( type.eq.'#' ) type = 'D'
         if ( type.eq.'$' ) type = 'S'
         ibeg = ibeg + 1
      endif
c
c  Take care of quoted values
c
      if ( value(ibeg:ibeg).eq.backslash ) then
         ibeg = ibeg + 1
         ds = value(ibeg:iend)
         if ( type.ne.'S' ) then 
            call strnum(value(ibeg:iend), 8, dd, status)
            di = int(dd)
         endif
         return
      endif
c
c  If blank assume we want keyword as named
c
      if ( value(ibeg:iend).eq.' ' ) then
         hdrkey = keyword
      else
         hdrkey = value(ibeg:iend)
      endif
c
c  Look up type if '?'
c
      if ( type.eq.'?' ) then
         call gheadtype(hdrkey, type, status)
         status = 0
      endif
c
c  Lookup keyword
c
      call upc(type)
      if ( type.eq.'I' ) then
         call gheadi(mapid, hdrkey, di, 0, status)
      elseif ( type.eq.'D' ) then
         call gheadd(mapid, hdrkey, dd, 0, status)
      elseif ( type.eq.'S' ) then
         call gheads(mapid, hdrkey, ds, 0, status)
      endif
c
c  NOTE: Unknown types (e.g. ' ') quietly ignored
c
      return
      end
