**==ALIAUI.spg  processed by SPAG 3.09I  at 09:41 on 20 Aug 1992
      SUBROUTINE ALIAUI(Alias_command,Command,Show,All,Delete,System,
     &                  Previous,New,Page,Status)
      INCLUDE 'estec.inc'
      INTEGER*4 NQUAL , Status
      PARAMETER (NQUAL=8)
      INTEGER*4 lq(NQUAL)
      character(30) quals(NQUAL)
      CHARACTER*(*) Alias_command , Command
      character(80) qualifier(NQUAL)
      LOGICAL*4 Show , Delete , System , Previous , All
      INTEGER*4 iflag , idelim , nq , jj
      LOGICAL*4 qskip , list , user
      LOGICAL*4 Page , New , full
      DATA quals/'SHOW' , 'LIST' , 'DELETE' , 'SYSTEM' , 'USER' , 
     &     'PREVIOUS' , 'NEW' , 'FULL'/
c
      Delete = .FALSE.
      Show = .FALSE.
      All = .FALSE.
      list = .FALSE.
      System = .FALSE.
      user = .FALSE.
      Previous = .FALSE.
      full = .FALSE.
      Page = .TRUE.
      New = .FALSE.
c      string = ' '
c
c see if a string to match has been specified
c
      Alias_command = ' '
      Command = ' '
      CALL XGTARG(Zstring,Zparse,Zbeg,Zend,qskip,iflag,idelim,*100,*100,
     &            *100)
      Alias_command = Zstring(Zbeg:Zend)
      CALL XGTARG(Zstring,Zparse,Zbeg,Zend,qskip,iflag,idelim,*100,*100,
     &            *100)
      Command = Zstring(Zbeg:Zend)
 100  CALL DOQUAL(quals,qualifier,lq,NQUAL,nq,Status)
      IF ( Status.NE.0 ) RETURN
      IF ( nq.GT.0 ) THEN
         DO 150 jj = 1 , nq
            Status = 0
            CALL QLF0L(qualifier(jj),'DELETE',Delete,Status)
            Status = 0
            CALL QLF0L(qualifier(jj),'SHOW',Show,Status)
            Status = 0
            CALL QLF0L(qualifier(jj),'LIST',list,Status)
            Status = 0
            CALL QLF0L(qualifier(jj),'SYSTEM',System,Status)
            Status = 0
            CALL QLF0L(qualifier(jj),'USER',user,Status)
            Status = 0
            CALL QLF0L(qualifier(jj),'PREVIOUS',Previous,Status)
            Status = 0
            CALL QLF0L(qualifier(jj),'FULL',full,Status)
            Status = 0
            CALL QLF0L(qualifier(jj),'NEW',New,Status)
            Status = 0
 150     CONTINUE
      ENDIF
c
      IF ( full ) Page = .FALSE.
c
      IF ( Delete .AND. list .OR. Delete .AND. Previous .OR. 
     &     Delete .AND. Show ) THEN
         CALL XWRITE(' Illegal qualifier combination',5)
         Status = 1
         RETURN
      ENDIF
c
      IF ( New .AND. Delete .OR. New .AND. Show .OR. New .AND. list )
     &     THEN
         CALL XWRITE(' Illegal qualifier combination',5)
         Status = 1
         RETURN
      ENDIF
c
      IF ( Previous .AND. list .OR. Previous .AND. Show ) THEN
         CALL XWRITE(' Illegal qualifier combination',5)
         Status = 1
         RETURN
      ENDIF
c
      IF ( list ) Show = .TRUE.
      IF ( list .OR. Show ) THEN
         All = .TRUE.
         IF ( System .AND. .NOT.user ) All = .FALSE.
         IF ( .NOT.System .AND. user ) All = .FALSE.
      ENDIF
      RETURN
      END
