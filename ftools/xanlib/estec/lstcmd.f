**==LSTCMD.spg  processed by SPAG 3.09I  at 22:17 on 17 Nov 1992
      SUBROUTINE LSTCMD()
c
      INCLUDE 'estec.inc'
      INCLUDE 'commands.inc'
 
      INCLUDE 'estec_tbl.inc'
c
c  list browse commands  nick 7/2/90
c
c command list info (in common)
c
c	integer*4 zcom_no, zlist_no
c  no read in, no to be listed
c     	integer*4 zncom,zlist_nolist
c  max com, no not to be listed
c     	character(10) zcom_list(zncom)
c  commands to be listed
c     	character(30) zcom_descrip_list(zncom)
c  description of them
c     	character(10) zcom_name(zncom)
c  all valid commandsc
c	character(30) zcom_descrip(zncom)
c  description
c     	character(10) zcom_nolist(zncom)
c  hidden command list
c     	character(30) zcom_descrip_nolist(zncom)
c  description
      LOGICAL*4 pages , finish
 
* keyword printing vars
      character(80) current , outline
      INTEGER*4 j
 
* external functions
*
c
      INTEGER*4 nnn , jj , ii , i , iflag , idelim
      LOGICAL*4 hidden , full
      character(30) name(30)
c
      LOGICAL*4 qskip
c
c parsing crapola
c
      INTEGER*4 NQUAL , status , nq
      PARAMETER (NQUAL=2)
      character(10) quals(NQUAL)
      character(80) qualifier(NQUAL)
      INTEGER*4 lq(NQUAL)
c
      DATA quals/'HIDDEN' , 'FULL'/
      hidden = .FALSE.
      finish = .FALSE.
      nnn = 0
      status = 0
      full = .FALSE.
      pages = .TRUE.
c
c start by getting parsed commands
c
      CALL DOQUAL(quals,qualifier,lq,NQUAL,nq,status)
      IF ( status.NE.0 ) RETURN
c
      IF ( nq.GT.0 ) THEN
c
         DO 50 jj = 1 , nq
c
c  check to see if hidden parms to be listed
c
            status = 0
            CALL QLF0L(qualifier(jj),'HIDDEN',hidden,status)
            IF ( status.NE.0 ) hidden = .FALSE.
            status = 0
            CALL QLF0L(qualifier(jj),'FULL',full,status)
            IF ( status.NE.0 ) full = .FALSE.
c
 50      CONTINUE
      ENDIF
c
      IF ( full ) pages = .FALSE.
c
c  get parsed name
c
      ii = 0
      IF ( Zend.NE.0 ) Zend = 1
      DO WHILE ( Zend.NE.0 .AND. ii.LE.30 )
         ii = ii + 1
         name(ii) = ' '
         CALL XGTARG(Zstring,Zparse,Zbeg,Zend,qskip,iflag,idelim)
         IF ( Zend.NE.0 ) name(ii) = Zstring(Zbeg:Zend)
      ENDDO
 
      IF ( Tblwcnt.EQ.0 ) THEN
*
* Old cold
*
c
c  parsed string given, partial match it
c
         IF ( name(1).NE.' ' ) THEN
            i = 1
            IF ( ii.GT.1 ) ii = ii - 1
            DO WHILE ( i.LE.ii )
               IF ( .NOT.(hidden) ) THEN
                  CALL MCHLST(name(i),Zcom_descrip_list,Zcom_list,
     &                        Zlist_no,nnn,pages)
               ELSE
                  CALL MCHLST(name(i),Zcom_descrip_nolist,Zcom_nolist,
     &                        Zlist_nolist,nnn,pages)
               ENDIF
               i = i + 1
            ENDDO
c
c no string given
c
         ELSEIF ( hidden ) THEN
            CALL LSTBLK(Zlist_nolist,Zcom_nolist,Zcom_descrip_nolist,
     &                  pages)
c
         ELSE
c
            CALL LSTBLK(Zlist_no,Zcom_list,Zcom_descrip_list,pages)
         ENDIF
c
 
*
* New code
*
c
c ? given
c
      ELSEIF ( name(1)(1:1).EQ.'?' ) THEN
         IF ( hidden ) THEN
            CALL LSTBLK(Zlist_nolist,Zcom_nolist,Zcom_descrip_nolist,
     &                  pages)
c
         ELSE
c
            CALL LSTBLK(Zlist_no,Zcom_list,Zcom_descrip_list,pages)
         ENDIF
 
      ELSE
* They want to see the keyword table
 
         i = 1
         j = 1
         outline = ' '
         current = ' '
         IF ( name(1).EQ.' ' ) THEN
            CALL XWRITE(' Enter one of the following keywords or ??'//
     &                  ' for a full command listing:',5)
            DO WHILE ( i.LE.Tblwcnt )
               IF ( Tblwname(i).NE.current ) THEN
                  current = Tblwname(i)
                  IF ( j.EQ.7 ) THEN
                     CALL XWRITE(' '//outline,5)
                     outline = current
                     j = 1
                  ENDIF
                  outline((j-1)*12+1:) = current
                  j = j + 1
               ENDIF
               i = i + 1
            ENDDO
 
            CALL XWRITE(' '//outline,5)
 
         ENDIF
 
 
      ENDIF
      RETURN
      END
