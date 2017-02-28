c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C        SUBROUTINE JGETSYSTEM
C
c
C  $Id: jgetsyst.f,v 1.3 2013/05/21 19:08:25 irby Exp $
c
c	Purpose: Issue a system (unix OS) command
c
c	Effect: After issuing the system command the routine will
C		check to see if shell level 3 environment was changed
c		and if so, change the likelihood environment (shell 
c		level 2).  This routine also will keep and output a 
c		history listing of the last 50 system commands.
c
c		shlvl2: can be altered (e.g. cd, setenv xxx yyy, etc.)
c			reset to original setting (use #R), or 
c			written	to console (use #?).  Commands
c			in the history can be reissued through the
c			#nn command where nn is a number within the
c			history limits of 0 to N (N <= 50)
c
c		Up to 50 lines of the System command history can be 
c		written to console (#H)
c
c	Comment: This routine is difficult to write in FORTRAN.  It is
c		 up to the user to make sure that shlvl2 is as he/she
c		 expects after issuing environment changing commands.
c		 Commands which involve environment variable lines longer
c		 than 1024 characters WILL NOT be affected by this routine
c		 as it is presently written.
c
c	BUGS:	 The shell level 2 environment will occasionally be 
c		 compromised by a system command.  One symptom of this
c		 is that files which are written by the program do not
c		 appear where expected (if at all).  A fix is to reset the
c		 shlvl2 environment by issuing the #R command.
c
c	Programmer: J.A.ESPOSITO delivered 25 AUG 1995
c
c
c
c
C  $Log: jgetsyst.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/04/18 19:34:09  irby
C  Bug fixes for OSF build:
C
C  - change "dreal" to acceptable replacement "dble"
C  - changed null string comparisons '' to ' ' - this may not be the best
C    solution.
C  - removed malloc.h from like.h (unnecessary, and not available under Darwin)
C  - Makefile generated using: mkmk version 1.81
C
C  Revision 1.1  2002/04/16 20:27:34  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:48:21  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:20  jae
c Subroutine Module for like V5.00
c
c
      SUBROUTINE JGETSYSTEM

c---  > COMMON blocks included
c
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'

      save

      character(80) id
      common /id/id
c     
      logical FEXIST,jfileflg
      integer jjv1,jjv2,jjv
      integer jputenv,chdir,jsetenv
c
      character tinput*1024,infile*260,home1*260,tmpcmd*260,local1*512
      character(20) user1,cwd1,ttin,host1
      character(128) env_shlvl2(200),jcom(50),tjcom
      character newenv1*512,oldenv1*512,tmpenvvar*512,newcwd*512
      character(1024)  envl_shlvl2(10),string
c
c---> get size of tinput string. Last entry is cwd
c---> get environment variable $USER to set up my shell prompt
c
      id = '$Id: jgetsyst.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      LOC='JGETSYST'

      if(jjv.eq.0)then
         jloop=0
         CALL GETENV('PWD',local1)
         in_local1 = index(local1, ' ') 
         mkmk=1
 2       do ll= 1,1024
            string(ll:ll)=' '
         enddo
         jj=jsetenv(string,mkmk)
         if(jj.lt.0)goto 3
         if(jj.lt.128)then
            env_shlvl2(jjv1+1)=string(1:128)
            jjv1=jjv1+1
         else
            if(jj.lt.1024)then
               envl_shlvl2(jjv2+1)=string(1:1024)
               jjv2=jjv2+1
            else
               write(*,'(/,"Shell environment variable too long !",/,
     &              A,/,"Variable NOT stored")')string(1:1024)
            endif
         endif
         mkmk=mkmk+1
         goto 2
 3       if(jj.eq.-1)then
            
            jjv=1
            goto 4
         endif
         if(jj.lt.-1)then 
            print *,' Error on SHLVL2 environment input'
            print *,' Further input aborted with storage for:'
            print *,' Stored environment variables:',jjv1+jjv2
            jjv=1
            goto 4
         endif
      endif
 4    continue
      do kk=1,260
         home1(kk:kk)=' '
         infile(kk:kk)=' '
         tmpcmd(kk:kk)=' '
      enddo
      CALL GETENV('HOME',home1)
      indhome = index(home1, ' ')
 5    do kk=1,512
         newcwd(kk:kk)=' '
         tmpenvvar(kk:kk)=' '
         string(kk:kk)=' '
         if(kk.le.260)then
            tinput(kk:kk)=' '
         endif
         if(kk.le.20)then
            user1(kk:kk)=' '
            cwd1(kk:kk)=' '
            host1(kk:kk)=' '
         endif
      enddo
      if(indhome.le.1)then
         infile='98%01o50S%ER'
      else
         infile=home1(1:indhome-1)//'/98%01o50S%ER'
      endif
      in_dexf = index(infile, ' ') - 1
      tmpcmd='/bin/rm '//infile(1:in_dexf)
      jfileflg=.false.
      in_dex2=0
      in_dexp=0
      in_dexh=0
      CALL GETENV('HOST',host1)
      in_dexh = index(host1, ' ') - 1
      if(in_dexh.le.0)in_dexh=1
      CALL getcwd(tinput)
      in_dexp = index(tinput, ' ')
      do kk=260,1,-1
         if(tinput(kk:kk).eq.'/')then
            in_dex1=kk+1
            goto 7
         endif
      enddo
      in_dex2 = index(tinput, ' ') 
      goto 10
 7    in_dex2 = index(tinput, ' ')
      tinput=tinput(in_dex1:in_dex2)//'   '
      in_dex2 = index(tinput, ' ')
 10   if(in_dex2.le.0)cwd1=' '
      if(in_dex2.ge.1)then
         cwd1=tinput(1:in_dex2)
         numcwd1 = index(cwd1, ' ') - 1
         if(numcwd1.le.0)numcwd1=1
      endif
      CALL GETENV('USER',tinput)
      in_dex2 = index(tinput, ' ') - 1
      if(in_dex2.gt.20)in_dex2=20
      if(in_dex2.ge.1)then
         user1=tinput(1:in_dex2)
         numuser1=in_dex2
      endif
      if(in_dex2.le.0)user1=' '
      if(in_dex2.le.0)numuser1=1
c
 20   continue
      do kk=1,260
         tinput(kk:kk)=' '
      enddo
      jltst = MOD(jloop,10)
      if(jltst.eq.0)then
         print *,' '
         print *,' Enter system command; enter exit to return to like'
         print *,' '
      endif
 22   write(*,'("[",A,"]",A,":like:",I3.1,">"$)')user1(1:numuser1),
     &     cwd1(1:numcwd1),jloop
      jloop = jloop + 1
      numcar=0
      read(LU(12),'(a)') tinput
      numcar = index(tinput, ' ') - 1
      if(numcar.le.0)goto 22
      jcomtst=mod(jloop,50)
      if(jcomtst.eq.0)jcomtst=50
      jcom(jcomtst)=tinput(1:128)
 23   do kk = 1,128
         if(tinput(1:1).ne.' ')goto 24
         tinput(1:127)=tinput(2:128)
         tinput(128:128)=' '
      enddo
 24   continue
      if(tinput(1:1).eq.'#')goto 60
      do kk=1,20
         ttin(kk:kk)=tinput(kk:kk)
         if(ICHAR(ttin(kk:kk)).ge.97.and.
     &        ICHAR(ttin(kk:kk)).le.122)ttin(kk:kk)=
     *        char(ICHAR(ttin(kk:kk))-32)
      enddo
      do kk=1,19
         if(ttin(kk:kk).eq.' ')then
            do kkk=kk,19
               ttin(kkk:kkk)=ttin(kkk+1:kkk+1)
            enddo
         endif
      enddo
      if(ttin(1:4).eq.'EXIT')goto 40
      tinput=tinput(1:numcar)//
     &     ';set | grep cwd > '//infile(1:in_dexf)//
     &     '; env >> '//infile(1:in_dexf)
      INQUIRE(file=infile,EXIST=FEXIST)
      if(FEXIST)call system(tmpcmd)
      jibbt = index(tinput,'>>')
      jibbt = jibbt + index(tinput(jibbt:1024),'/')
      jibbt = jibbt + index(tinput(jibbt:1024),' ')
      
      call system(tinput(1:jibbt))
      do kk = 1, 260
         tinput(kk:kk)=' '
      enddo
      loop = 0
      open(47,file=infile,err=45)
 25   continue
      do kk = 1,512
         newenv1(kk:kk)=' '
         oldenv1(kk:kk)=' '
         tmpenvvar(kk:kk)=' '
         string(kk:kk)=' '
      enddo
      read(47,'(a)',end=29,err=50) newenv1
      numcar = index(newenv1, ' ') - 1
      loop=loop+1
      if(loop.eq.1)goto 26
      in_dexm=index(newenv1,'=')
      if(in_dexm.le.2)goto 25
      if(newenv1(1:in_dexm-1).eq.'SHLVL'.or.
     &     newenv1(1:in_dexm-1).eq.'TERMCAP'.or.newenv1(1:1).eq.':'.or.
     &     newenv1(1:4).eq.'PWD=')goto 25
      oldenv1(1:in_dexm)=newenv1(1:in_dexm)
      if(numcar.le.128)then
         do kk=1,jjv1
            string=env_shlvl2(kk)
            if(string(1:in_dexm).eq.oldenv1(1:in_dexm))goto 251
         enddo
      else
         do kk=1,jjv2
            string=envl_shlvl2(kk)
            if(string(1:in_dexm).eq.oldenv1(1:in_dexm))goto 251
         enddo
      endif
      do kk=in_dexm+1,1024
         string(kk:kk)=' '
      enddo
 251  CALL GETENV(oldenv1(1:in_dexm-1),oldenv1(in_dexm+1:512))
      in_dexn=0
      in_dexo=0
      tmpenvvar='0'
      do kk=512,in_dexm,-1
         if(in_dexn.eq.0.and.newenv1(kk:kk).ne.' ')in_dexn=kk
         if(in_dexo.eq.0.and.oldenv1(kk:kk).ne.' ')in_dexo=kk
         if(in_dexe.eq.0.and.string(kk:kk).ne.' ')in_dexe=kk
         if(oldenv1(kk:kk).ne.newenv1(kk:kk).and.
     &        newenv1(kk:kk).ne.string(kk:kk))tmpenvvar='1'
         if(string(kk:kk).ne.newenv1(kk:kk).and.
     &        string(kk:kk).ne.oldenv1(kk:kk).and.
     &        oldenv1(kk:kk).ne.newenv1(kk:kk))tmpenvvar='1'
         if(oldenv1(kk:kk).ne.newenv1(kk:kk).and.
     &        oldenv1(kk:kk).ne.string(kk:kk).and.
     &        newenv1(kk:kk).eq.string(kk:kk))tmpenvvar='1'
         if(in_dexn.ne.0.and.in_dexo.ne.0.and.tmpenvvar(1:1).ne.'0')
     &        goto 28
      enddo
      goto 25
 26   jfileflg=.false.
      in_dex1=index(newenv1,'/')
      newenv1=newenv1(in_dex1:512)
      call getcwd(oldenv1)
      in_dexo = index(oldenv1, ' ') - 1
      if(in_dexo.le.0)in_dexo=1
      in_dexn = index(newenv1, ' ') - 1
      if(in_dexn.ne.in_dexo)goto 27
      do kk = 1,512
         if(newenv1(kk:kk).ne.oldenv1(kk:kk))goto 27
      enddo
      goto 25
 27   jfileflg=.true.
      newcwd=newenv1(1:in_dexn)
      in_dexncwd = index(newcwd, ' ')
      if(in_dexncwd.le.0)in_dexncwd=1
c     write(*,'(A)')newcwd(1:in_dexncwd)
      I = chdir(newcwd)
      call getcwd(oldenv1)
      in_dexo = index(oldenv1, ' ') 
c     write(*,'(A)')oldenv1(1:in_dexo)
      if(I.ne.0)then
         print *,' FAILED to change the directory'
         print *,' Old directory: ',oldenv1(1:in_dexo)
         print *,' to :',newcwd(1:in_dexncwd)
      endif
      goto 25
 28   I=-1
      newenv1(in_dexn+1:in_dexn+1)=char(0)
      I=jputenv(newenv1(1:in_dexn+1))
      if(I.ne.0)then
         print *,' FAILED to change environment variable: ',
     &        newenv1(1:in_dexm-1)
      endif
      call getenv(newenv1(1:in_dexm-1),tmpenvvar(in_dexm+1:512))
      in_dext=0
      do kk=512,in_dexm,-1
         if(in_dext.eq.0.and.tmpenvvar(kk:kk).ne.' ')in_dext=kk
      enddo
      goto 25
 29   close(unit=47)
 30   continue
      INQUIRE(file=infile,EXIST=FEXIST)
      if(FEXIST)call system(tmpcmd)
      goto 5
 40   INQUIRE(file=infile,EXIST=FEXIST)
      if(FEXIST)call system(tmpcmd)
      return
 45   close(unit=47)
      print *,' ERROR opening environment file !!!??'
      print *,' Environment changing commands DID not take effect'
      goto 30
 50   continue
      print *,' ERROR reading environment file !!!??'
      print *,' Environment changing commands MAY not take effect'
      goto 29
 60   if(tinput(2:2).eq.'h'.or.tinput(2:2).eq.'H')then
         do jj=jloop-49,jloop
            if(jj.gt.0)then
               jcomtst=mod(jj,50)
               if(jcomtst.eq.0)jcomtst=50
               tjcom=jcom(jcomtst)
               in_dexjl=index(tjcom,'   ')
               write(*,'("[",I3,"] ",A)')jj-1,tjcom(1:in_dexjl)
            endif
         enddo
         goto 5
      endif
      if(tinput(2:2).eq.'?')then
         do kk=1,jjv1
            string=env_shlvl2(kk)
	    in_dexjj = index(string, ' ') 
	    print *,kk,'>',string(1:in_dexjj)
         enddo
         do kk=1,jjv2
            string=envl_shlvl2(kk)
	    in_dexjj = index(string, ' ')
	    print *,kk,'>>',string(1:in_dexjj)
         enddo
         goto 5
      endif
      if(tinput(2:2).eq.'r'.or.tinput(2:2).eq.'R')then
         print *,' Resetting the environment'
         newcwd=local1(1:512)
         I=chdir(local1)
         call getcwd(oldenv1)
         in_dexo = index(oldenv1, ' ') - 1
         print *,'Local:',newcwd(1:in_local1)
         do kk=1,jjv1
            string = env_shlvl2(kk)
            do jj=128,1,-1
               if(string(jj:jj).ne.' ')then
                  in_string=kk
                  goto 63
               endif
            enddo
 63         I=JPUTENV(string(1:in_string))
         enddo
         tinput(2:2) = '?'
         goto 60
      endif
      tjcom=jcom(jcomtst)
      read(tjcom(2:128),*,err=65,end=65)jj
      jj=jj+1
      if(jj.lt.jloop-49)then
         print *,' Command out of range: ',jj,' < ',jloop-49
         goto 5
      endif
      jjtst=mod(jj,50)
      if(jjtst.eq.0)jjtst=50
      jcom(jcomtst)=jcom(jjtst)
      tinput=jcom(jcomtst)
      do kk = 512,1,-1
	 numcar=kk
         if(tinput(kk:kk).ne.' ')goto 64
      enddo
 64   if(tinput(1:1).eq.'#')goto 60
      goto 23
 65   in_dexjl = index(tjcom, ' ')
      if(in_dexjl.eq.0)in_dexjl=128
      print *,'ERROR on input command !! ::->',tjcom(1:in_dexjl)
      goto 5
      end
c
