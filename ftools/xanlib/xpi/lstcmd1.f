**==lstcmd1.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
      SUBROUTINE LSTCMD1()
c
      INCLUDE 'commands.inc'
 
      INCLUDE 'tbl.inc'
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
      LOGICAL*4 pages
 
* keyword printing vars
      character(80) current , outline
      INTEGER*4 j
 
* external functions
*
c
      INTEGER*4 nnn , i
      LOGICAL*4 hidden , full
      character(30) name
      character(30) comm
c
c
c parsing crapola
c
      INTEGER*4 status
c
      hidden = .FALSE.
      nnn = 0
      status = 0
      full = .FALSE.
      pages = .TRUE.
      CALL YGETCMD(comm)
C BEGIN ============================================== BEO 93/1/8
c
c start by getting parsed commands
c
C      CALL DOQUAL(quals,qualifier,lq,NQUAL,nq,status)
C      IF ( status.NE.0 ) RETURN
c
C      IF ( nq.GT.0 ) THEN
c
C         DO 50 jj = 1 , nq
c
c  check to see if hidden parms to be listed
c
C            status = 0
C            CALL QLF0L(qualifier(jj),'HIDDEN',hidden,status)
C            IF ( status.NE.0 ) hidden = .FALSE.
C            status = 0
C            CALL QLF0L(qualifier(jj),'FULL',full,status)
C            IF ( status.NE.0 ) full = .FALSE.
c
C 50      CONTINUE
C      ENDIF
c
C      IF ( full ) pages = .FALSE.
c
c  get parsed name
c
C      ii = 0
C      IF ( Zend.NE.0 ) Zend = 1
C      DO WHILE ( Zend.NE.0 .AND. ii.LE.30 )
C        ii = ii + 1
C         name(ii) = ' '
C         CALL XGTARG(Zstring,Zparse,Zbeg,Zend,qskip,iflag,idelim)
C         IF ( Zend.NE.0 ) name(ii) = Zstring(Zbeg:Zend)
C      ENDDO
C
      status = 0
      CALL UCLGSB('hidden',hidden,status)
      IF ( status.NE.0 ) CALL XERROR('Status not 0 after hidden',5)
      status = 0
      CALL UCLGSB('full',full,status)
      IF ( status.NE.0 ) CALL XERROR('Status not 0 after full',5)
      status = 0
      CALL UCLGST('name',name,status)
      IF ( status.NE.0 ) CALL XERROR('Status not 0 after name',5)
 
 
      IF ( comm.EQ.'??' ) name = '?'
* END ===================================================== BEO 93/1/8
c
c  parsed string given, partial match it
c
      IF ( name.EQ.' ' ) THEN
* They want to see the keyword table
         i = 1
         j = 1
         outline = ' '
         current = ' '
         IF ( name.EQ.' ' ) THEN
            CALL XWRITE(' Enter one of the following keywords or ??'//
     &                  ' for a full command listing:',5)
            DO WHILE ( i.LE.TBLwcnt )
               IF ( TBLwname(i).NE.current ) THEN
                  current = TBLwname(i)
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
      ELSEIF ( name.EQ.'?' ) THEN
         IF ( hidden ) THEN
            CALL LSTBLK1(ZLIst_nolist,ZCOm_nolist,ZCOm_descrip_nolist,
     &                   pages)
c
         ELSE
c
            CALL LSTBLK1(ZLIst_no,ZCOm_list,ZCOm_descrip_list,pages)
 
         ENDIF
      ELSEIF ( .NOT.(hidden) ) THEN
         CALL MCHLST(name,ZCOm_descrip_list,ZCOm_list,ZLIst_no,nnn,
     &               pages)
      ELSE
         CALL MCHLST(name,ZCOm_descrip_nolist,ZCOm_nolist,ZLIst_nolist,
     &               nnn,pages)
 
      ENDIF
      RETURN
      END
**==lparm.spg  processed by SPAG 4.50F  at 15:17 on 26 Aug 1994
 
 
 
 
******************************************************
*
      SUBROUTINE LPARM
 
      character(100) name
      INTEGER*4 status , i , j , TBLFPR , LENACT , k
      LOGICAL*4 title
      character(100) strpar , strcmd , strtmp
      character(2) ty
 
      INCLUDE 'tbl.inc'
 
      status = 0
      CALL UCLGST('name',name,status)
      IF ( status.NE.0 ) CALL XERROR('Status not 0 after name',5)
 
      IF ( name.EQ.' ' ) THEN
         CALL XWRITE(' No name given:  Try LPARM command',5)
         RETURN
      ENDIF
      CALL UPC(name)
      title = .FALSE.
 
      DO 100 k = 1 , 2
         DO 50 i = 1 , TBLkcnt
            strcmd = TBLkname(i)
            CALL UPC(strcmd)
            IF ( name.EQ.strcmd ) THEN
* got a good command, find the parameter
               strpar = TBLkparm(i)
               CALL UPC(strpar)
               j = TBLFPR(strpar)
               IF ( j.EQ.0 ) THEN
                  CALL XWARN(' Could not found parameter '//strpar,5)
               ELSE
 
* print an entry
                  IF ( .NOT.title ) THEN
                     title = .TRUE.
                     CALL XWRITE(' NAME          TYPE    PROMPT   '//
     &                          '                               DEFAULT'
     &                          ,5)
                     CALL XWRITE(' ==============================='//
     &                          '======================================'
     &                          ,5)
                  ENDIF
 
                  ty = TBLptype(j)
                  CALL UPC(ty)
                  IF ( ty.EQ.'S' ) THEN
                     strtmp = 'STRING'
                  ELSEIF ( ty.EQ.'R' ) THEN
                     strtmp = 'REAL'
                  ELSEIF ( ty.EQ.'B' ) THEN
                     strtmp = 'BOOLEAN'
                  ELSEIF ( ty.EQ.'I' ) THEN
                     strtmp = 'INTEGER'
                  ELSEIF ( ty.EQ.'D' ) THEN
                     strtmp = 'DOUBLE'
                  ELSE
                     strtmp = 'UNKNOWN'
                  ENDIF
                  ty = TBLpupda(j)
                  strpar = ' '
                  IF ( INDEX(ty,'h').EQ.0 .AND. k.EQ.1 ) THEN
                     strpar = ' '//TBLpname(j)(1:14)//strtmp(1:8)
     &                        //TBLpdesc(j)(1:39)//' '//TBLpdefl(j)
                  ELSEIF ( INDEX(ty,'h').NE.0 .AND. k.EQ.2 ) THEN
                     strpar = ' ['//TBLpname(j)(1:12)
                     strpar = strpar(1:LENACT(strpar))//']'
                     strpar(16:80) = strtmp(1:8)//TBLpdesc(j)(1:39)
     &                               //' '//TBLpdefl(j)
                  ENDIF
                  IF ( strpar.NE.' ' ) CALL XWRITE(strpar(1:80),5)
               ENDIF
            ENDIF
 50      CONTINUE
 100  CONTINUE
      RETURN
      END
 
 
 
