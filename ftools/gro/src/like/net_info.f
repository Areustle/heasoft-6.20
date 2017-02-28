c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
C	subroutine net_info(net_file2,N)
C
C
C  $Id: net_info.f,v 1.3 2013/05/21 19:08:26 irby Exp $
c
C
C
C  $Log: net_info.f,v $
C  Revision 1.3  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2005/08/26 19:36:35  irby
C  Purely cosmetic changes to allow compilation with gfortran/g95, mostly
C  involving fixes to lines longer than 72 chars that were being truncated,
C  but in the case of the CGRO code, also moving misplaced (tabbed) line
C  continuation characters to their appropriate position in column 6.
C
C  Revision 1.1  2002/04/16 20:27:41  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.7  1997/10/02  20:04:52  jae
c Changed if statement for background command to avoid
c problems if switch '+' or '-' are used.
c
c Revision 5.6  1997/10/02  18:39:12  jae
c Fixed typo's on lines 160 and 182
c
c Revision 5.5  1997/10/02  18:34:33  jae
c Fixed error (closeing function) on line 176
c
c Revision 5.4  1997/10/02  17:04:34  jae
c Updated routine to setup ending ' &' for
c help command to WWW site assuming a UNIX
c environment.
c
c Revision 5.3  1997/10/02  16:23:35  jae
c Updated routine to use http://lheawww.gsfc.nasa.gov/~jae/like/
c website.  If 'lynx' is the browser the HTML file 'noframe.html'
c is requested automatically.  The interface between setup.f,
c like.f and net_info.f all reflect these changes.
c
c Revision 5.2  1996/04/08  16:14:26  jae
c Changed INFO_CMD to use EGRET_DOC
c
c Revision 5.1  1996/02/29  20:52:15  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/14  22:45:28  jae
c Subroutine Module for like V5.00
c
C
	subroutine net_info(net_file2,N)

	character net_file2(N)
        INCLUDE  '../COMMON/errrep.copy'
	include  '../COMMON/cnfrep.copy'

	character(80) id
	common /id/id
	character(70) input
	character(100) net_file
	character(100) output,output2,KEYSYMDB
	external JPUTENV

        save
c
	id = '$Id: net_info.f,v 1.3 2013/05/21 19:08:26 irby Exp $'
	LOC='net_info'

	if(jae_net_info)print *,'IN:',LOC
	if(N.eq.0)N=101
	do I=1,N
	   net_file(I:I)=' '
	   if(N.lt.100)net_file(I:I)=net_file2(I)
	   if(I.ge.99)N=100
	enddo
	M = index(net_file, ' ')
	if(browser_back .and. M.ge.5)then
	   N=index(net_file,'&')
	   if(N.eq.0)net_file=net_file(1:M)//'&'
	endif
	if(jae_net_info)print *,'net_file->',net_file(1:N)
	in_dex1 = index(INFO_CMD,'/ ')-1
	if(net_file(1:1).eq.'-'.or.net_file(1:1).eq.'+')goto 101
	if(jae_net_info)then
	   print *,'setting up command'
	   print *,' '
	endif
	output=INFO_CMD(1:in_dex1+1)//net_file(1:N)
	in_dex2 = index(output,'&')
	if(in_dex2.gt.0.and..not.browser_back)in_dex2=in_dex2-1
	if(jae_net_info)then
	   print *,'in_dex2: ', in_dex2
	   print *,'command:',INFO_CMD(1:in_dex1)
	endif
	CALL GETENV('XKEYSYMDB',KEYSYMDB)
	iie=index(KEYSYMDB,'netscape')
	iie1 = index(KEYSYMDB, ' ') 
	if(BROWSER(1:8).eq.'netscape')then
	   in_dex1 = index(EGRET_DOC, ' ') - 1
	   if(EGRET_DOC(in_dex1:in_dex1).ne.'/')then
	      EGRET_DOC(in_dex1:in_dex1)='/'
	      in_dex1=in_dex1+1
	   endif
	   if(in_dex1.lt.1)in_dex1=1
	   if(jae_net_info)write(*,'("XKEYSYMDB=",A)')KEYSYMDB(1:iie1)
	   output2 =
     1       'XKEYSYMDB='//EGRET_DOC(1:in_dex1)//
     2       'LIKEHTML/netscape/XKeysymDB'
	   ie=index(output2,'   ')
	   output2(ie:ie)=char(0)
	   if(jae_net_info)write(*,'(A)')output2(1:ie)
	   ie=JPUTENV(output2(1:ie))
	   if(jae_net_info)write(*,'("JPUTENV=",I5)')ie
	endif
	call system(output(1:in_dex2))
	return

 101	if(index(net_file2(1),'+').ne.0)goto 1011
	print *,' You have selected to change the Information'
	print *,' WWW parameters.'
 1011	print *,' '
	print *,' The current values are:'
	print *,' '
	if(browser_flg)then
	   in_dex1 = index(BROWSER, ' ') - 1
	   if(in_dex1.lt.1)in_dex1=1
	   print *,'BROWSER->',BROWSER(1:in_dex1)
	   in_dex1 = index(EGRET_DOC, ' ') - 1
	   if(in_dex1.lt.1)in_dex1=1
	   M=in_dex1-1
	   print *,'EGRET_DOC->',EGRET_DOC(1:in_dex1)
	   print *,' '
	   in_dex1=index(INFO_CMD,'/ ')-1
	   if(in_dex1.lt.1)in_dex1=1
	   print *,'Current Command line->',INFO_CMD(1:in_dex1)
	   print *,'BROWSER IS ON'
	else
	   in_dex1=index(EGRET_DOC,'/ ')
	   if(in_dex1.lt.1)in_dex1=1
	   print *,'EGRET_DOC->',EGRET_DOC(1:in_dex1)
	   M=in_dex1
	   in_dex1 = index(BROWSER, ' ') - 1
	   if(in_dex1.lt.1)in_dex1=1
	   if(in_dex1.gt.2)then
	      print *,'BROWSER->',BROWSER(1:in_dex1)
	      print *,'BROWSER IS OFF'
	   else
	      print *,'No BROWSER selected'
	   endif
	endif

	if((.not.browser_flg.and.in_dex1.gt.2).or.browser_flg)then
	   print *,'BROWSER will run in background ? : ',browser_back
	endif
	print *,' '
	if(net_file(1:1).eq.'+')then
	   net_file(1:1)=' '
	   return
	endif
	net_file=' '
	if(browser_flg)then
	   print *,' Change BROWSER: B'
	   print *,' Turn off BROWSER: T'
	else
	   print *,' Setup BROWSER: D'
	   if(BROWSER(1:1).ne.' ')print *, 'Turn on existing Browser: T'
	endif
	print *,' Return to like prompt: <cr> or q'
	read(5,'(a)') input
	numcar = index(input, ' ') - 1
	CALL TO_UPPER(input(1:1))
	if(numcar.eq.0.or.input(1:1).eq.'Q')return
	if(input(1:1).eq.'T')then
	   browser_flg=.NOT.browser_flg
	   if(.not.browser_flg)return
	   input(1:2)='T+'
	endif
 102	continue
	if(input(1:1).eq.'D'.or.input(1:1).eq.'B'.or.
     1     input(2:2).eq.'+')then
	   if(input(2:2).ne.'+')then
	      write(6,*)'Like documentation is best viewed using'
	      write(6,*)'netscape (>V2.0) or IE (>V2.0) for single'
	      write(6,*)'line noframe-no graphic mode use -lynx-'
	      write(6,*)'on UNIX.  Do not enter WWW site files in'
	      write(6,*)'BROWSER Name: e.g. ->netscape or'
	      write(6,*)'->iexplore or ->lynx are all valid commands'
	      write(6,*)'->lynx is the only single line mode'
	      write(6,*)'which can be used.'
	      write(7,*)'Include a path to the command as needed'
	      write(6,*)' '
	      write(6,'("Enter BROWSER Name or Command->"$)')
	      read(LU(12),'(a)') input
	      numcar = index(input, ' ') - 1
	      if(numcar.eq.0)return
	      BROWSER=input
	      browser_back=.true.
	      if(BROWSER(1:4).eq.'lynx')browser_back=.false.
	      if(BROWSER(1:numcar).ne.'netscape'.and.
     1           BROWSER(1:numcar).ne.'iexplore'.and.
     2           BROWSER(1:numcar).ne.'lynx')
     3           then
		 write(6,*)'Can '//BROWSER(1:numcar)//' run ',
     1                     'in background ? (T/n)'
		 read(LU(12),*)browser_back
	      endif
	   else
	      numcar = index(BROWSER, ' ') - 1
	      if(numcar.le.0)numcar=1
	   endif
	   if ((index(BROWSER,' ')-1) .le. 0) goto 103
	   browser_flg=.true.
	   INFO_CMD=BROWSER(1:numcar)//
     1              'http://lheawww.gsfc.nasa.gov/~jae/like/'
	   in_dex1=index(INFO_CMD,'/ ')-1
	   if(BROWSER(1:4).eq.'lynx')
     1        INFO_CMD=INFO_CMD(1:in_dex1)//'noframe.html'
	   goto 101
	endif
	goto 101
 103	continue
	print *,' '
	print *,' The BROWSER variable contains an empty string !'
	print *,' Please redefine the BROWSER or exit'
	print *,' '
	browser_flg=.false.
	browser_back=.false.
	goto 1011

	end
