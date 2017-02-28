
*+MVER
c     -----------------------------------------------------
      subroutine mver(iunit,extnum,ninstr,instr,nsearch,
     &                 next,outhdu,extnames,outver,
     &                 extname,errflg,chatter)
c     -----------------------------------------------------
c --- DESCRIPTION ---------------------------------------------------
c
c This routine moves to the desired extension, either specifed as 
c extnum, or by searching for HDUCLAS/EXTNAME values. If more than one 
c of the desired extension is found an error is returned.
c NOTE: If extnum .LT.0 then HDUCLAS/EXTNAME are used to move to 
c extension.
c
c --- VARIABLES -----------------------------------------------------
c
      IMPLICIT NONE
      character*(*) instr(*),extname
      integer iunit,ninstr,nsearch,errflg,chatter
      integer next(*),extnum
      character*(*) outhdu(9,*),extnames(*),outver(9,*)
c
c --- VARIABLE DIRECTORY --------------------------------------------
c
c  extnum    int    i : extnum, if less than 0, than HDUCLAS/EXTNAME
c                       used to move to desired extension
c  iunit     int    o : i/o unit number
c  ninstr    int    i : Number of hduclas values to be searched for
c  instr     char   i : array of hduclass strings to serach for
c  nsearch   int    i : Number of extensions searched
c  extname   char   i : extension name to search for,if HDUCLAS keys 
c                       not present
c  extnames  char   o : array of extnames found
c  chatter   int    i : chattines flag
c  errflg    int    i : Error flag       = 0  okay
c                                        = 2  >1 extensions found
c                                        = 3  0  extensions found
c                                        = 4  problem moving to ext  
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c     Rehana Yusaf (1994 May 24) 1.0.0; Stripped out from MVEXT
c     Rehana Yusaf (1994 aug 24) 1.0.1; outver updated to be 2D
c     Rehana Yusaf (1995 Dec 13) 1.0.2; add wtinfo and friends
c
c Banashree M. Seifert (1996 Aug) 1.1.0: modified on line#70 so that
c                         it can handle input file with extn 0, e.g.
c                         infile[0]
c Peter D Wilson (1998 Feb 23) 1.1.1: modified line #127 for same reason
c Peter D Wilson (1998 Jun 23) 1.1.2: When extnum is specified, copy its
c                         value to next(1)
c ------------------------------------------------------------------- 
      character(5) version
      parameter (version = '1.1.2')
*-
c -------------------------------------------------------------------
c
c --- LOCAL VARIABLES ---
c
      character(4) subname
      parameter (subname='mver')
      character(70) subinfo,errinfo
      integer status,imove,i,j,nfound
      integer htype
c
c --- USER INFO ---
c
      subinfo = ' using '//subname//' '//version
      call wtinfo(chatter,15,1,subinfo)
      errflg = 0
c 
c --- FIND EXTNUM if EXTNUM NOT SPECIFIED AT THE END OF INFILE --- 
c
ccc      IF (extnum.LE.0) THEN
      IF (extnum .lt. 0) THEN
        call fndhdu(chatter,iunit,ninstr,instr,nsearch,nfound,next,
     &              outhdu,outver,extnames,status)            
        
c --- CHECK FOR (old-style) EXTNAME if HDUCLASS not found ---

        IF (nfound.LE.0) THEN
          call wtinfo(chatter,20,2,
     &    'Ext with allowed HDUCLASn keywords not found')
          errinfo = ' searching for EXTNAME= '//extname
          call wtinfo(chatter,20,2,errinfo)
          call fndext(chatter,iunit,extname,
     &        nsearch,nfound,next,outhdu,outver,extnames,status)
        ENDIF
        
        IF (nfound.GT.1) THEN
          errinfo = 
     &            ' Input file contains >1 '//extname//'datasets'
          call wtwarm(subname,version,chatter,15,errinfo)
          write(errinfo,'(i12,a)')nfound,' extensions found'
          call wtinfo(chatter,15,1,errinfo)
          do i=1,nfound
            write(errinfo,'(a,i12,a)')' Ext ',next(i),':'
            call wtinfo(chatter,15,1,errinfo)
            errinfo = '    EXTNAME = ' // extnames(i)
            call wtinfo(chatter,15,1,errinfo)
            do j=1,4
              write(errinfo,'(4X,a,i2,2a)') 'HDUCLAS', j, ' = ',
     &              outhdu(j,i)(:MIN(len(outhdu(j,i)),len(errinfo)-32))
              call wtinfo(chatter,15,1,errinfo)
            enddo
          enddo
          errinfo =
     & '... Extension number must be specified via infile parameter'
          call wtinfo(chatter,15,1,errinfo)
          errinfo = ' for example infile[1]'
          call wtinfo(chatter,15,1,errinfo)
          errflg = 2
          return
        ELSEIF (nfound.EQ.0) THEN
          IF (chatter.GE.20) THEN
            errinfo = ' Unable to locate a '//extname
     &//'extension'
            call wterrm(subname,version,errinfo)
          ENDIF
          errflg = 3
          return
        ENDIF
      ENDIF

c --- MOVE TO APPROPRIATE PLACE IN FILE ---

ccc      IF (extnum.LE.0) THEN
      IF (extnum.lt.0) THEN
        IF (next(1).GT.0) THEN 
           imove = next(1)
           status = 0
           call ftmrhd(iunit,imove,htype,status)
           errinfo = 'Problem moving to '//extname
     &                //'xtens'
           IF (status.NE.0) THEN
             call wterrm(subname,version,errinfo)
             errflg = 4 
             return
           ENDIF
        ENDIF 
        extnum = next(1)
      ELSE
        status = 0
        call ftmahd(iunit,extnum+1,htype,status) 
        IF (status.NE.0) THEN
          call wterrm(subname,version,errinfo)
          errflg = 4 
        ENDIF
        next(1) = extnum
      ENDIF
      IF ((chatter.GE.20).AND.(errflg.EQ.0)) THEN
        subinfo = ' successfully moved to desired extension'
        call wtinfo(chatter,20,2,subinfo)
      ENDIF
      return
      end
c --------------------------------------------------------------------
c     END OF MVER
c -------------------------------------------------------------------- 

