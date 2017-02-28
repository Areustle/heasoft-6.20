      SUBROUTINE xrpltcmd (cpro, cqdp, iqdp, ito, ita, itu, dtnb,
     &     csumm,dxstep, kmax, dpera, iflags, rflags, cmd, ncmd)
      implicit none
c
c
c load the cmd command array for plt  
c
c cpro   I  C  program name 
c cqdp   I  C  array for qdp cpd(1)= device cpd(2)=file.pco
c iqdp   I  I  array for qdp 
c ito    I  I  integer array for start time 
c ita    I  I  integer array for stop time
c itu    I  I  integer array for units start time
c dtnb   I  R  *8 for integration time 
c csumm  I  C  input summury line  
c dxstep I  R  *8 period resultion
c kmax   I  I  index for best period array 
c dpera   I  R  array of periods
c iflags I  I  flag for x y label   
c rflags I  r  flag for x y set up  
c cmd    O  C  character array with PLT commnads 
c ncmd   O  I  total number of commands for plt
c
c NOTE : plotting labels for fold serach will be wrong in more than 
c        a energy series is done.
c
c NOTE to figure out which color need to be set off or on 
c      and the viewport probably needs to be done from here
c
c input
       INTEGER*4 iqdp(*), ito(*), ita(*), itu(*), kmax(*), iflags(*)
       REAL*4 rflags(*)
       REAL*8 dtnb, dxstep, dpera(*)
       character(80) csumm
       character(160) cqdp(*)
       character(8) cpro
c output 
       INTEGER*4 mxcmd,ncmd
       PARAMETER(mxcmd=50)
       character(132) cmd(mxcmd)
c local          
       INTEGER*4 k, i, nvec, width 
       character(80) context, text, fmt,string, str
c
       ncmd=0
c
c write the READ statment for array with vector depending on iflags(18)
c plus the xaxis
c
       nvec=((iflags(8)-1)/2) + 1
       WRITE(text, 500) nvec
500    FORMAT('( '' READ SERR '',', I2,'I3)                      ')
       if(cpro(1:2).eq.'es') then
          WRITE(context,text)(i, i=2,nvec)
       else
          WRITE(context,text)(i, i=1,nvec)
       endif
       ncmd=ncmd+1
       cmd(ncmd)= context
c       
c       write(*,*)'cqdp(2)',cqdp(2)
       IF(cqdp(2).ne.' ')THEN
          ncmd=ncmd+1
          cmd(ncmd)= '@'//cqdp(2)      
       ELSE
          context = 'Can''t access plot file'//cqdp(2)
          CALL XWRITE(context,5)
       ENDIF   
c time off
       ncmd=ncmd+1
       cmd(ncmd)= 'TIME OFF'
c label file 
       ncmd=ncmd+1
       cmd(ncmd)= 'LABEL FI'
c top label
       ncmd=ncmd+1
       cmd(ncmd)= 'LA T '//csumm(1:72)
       ncmd=ncmd+1
c start and stop time
       if(iflags(2).eq.4) then
          WRITE(text, 1000) (itu(k), k=1, 5), (ito(k), k=1, 5)
       else
          WRITE(text, 1000) (ita(k), k=1, 5), (ito(k), k=1, 5)
       endif
1000   FORMAT(' LA 1 "Start Time ',i5,i3,':',i2.2,':',i2.2,':',i3.3,
     &      4x,'Stop Time ',i5,i3,':',i2.2,':',i2.2,':',i3.3,
     &      '" VP .5 .05') 
       cmd(ncmd)= text
c binning
       text=' '
       ncmd=ncmd+1
       if ( cpro(1:2) .eq. 'ef' ) then
          WRITE(text, 1100) dpera(1)
          call rmvxbk(text)
       else
          WRITE(text, 2000) dtnb
       endif
1100   FORMAT(' LA 2 "Folded period: ',
     &          g22.15,' s" CEN BOT JUS RIG VP .9 .93')
2000   FORMAT(' LA 2 "Bin time: ',
     &          1g12.4,' s" CEN BOT JUS RIG VP .9 .93')

       cmd(ncmd)= text
c set size 
       ncmd=ncmd+1
       cmd(ncmd)= 'CSIZE 1.'
c only for efs
       IF (cpro(1:2). eq. 'es') THEN 
           WRITE (text, 3000) dpera(kmax(1))
3000       FORMAT(' LA 3 "Best Period:',g22.15,'   s" VP 0.5 0.87')
           ncmd=ncmd+1
           cmd(ncmd)=text
           WRITE(text, 4000)dxstep
4000       FORMAT(' LA 4 " Resolution:',g22.3,'   s" VP 0.5 0.84')
           ncmd=ncmd+1
           cmd(ncmd)=text
           IF(iflags(10).eq.2) THEN
              WRITE (text, 3200) dpera(kmax(2))
3200          FORMAT(' LA 5 "Best Period:',g22.15,'   s" VP 0.5 0.50')
              ncmd=ncmd+1
              cmd(ncmd)=text
           ENDIF                
           IF(iflags(10).eq.3) THEN
              WRITE (text, 3300) dpera(kmax(2))
3300          FORMAT(' LA 5 "Best Period:',g22.15,'   s" VP 0.5 0.62')
              ncmd=ncmd+1
              cmd(ncmd)=text
              WRITE (text, 3310) dpera(kmax(3))
3310          FORMAT(' LA 6 "Best Period:',g22.15,'   s" VP 0.5 0.35')
              ncmd=ncmd+1
              cmd(ncmd)=text
           ENDIF                
           IF(iflags(10).eq.4) THEN
              WRITE (text, 3400) dpera(kmax(2))
3400          FORMAT(' LA 5 "Best Period:',g22.15,'   s" VP 0.5 0.68')
              ncmd=ncmd+1
              cmd(ncmd)=text
              WRITE (text, 3400) dpera(kmax(3))
3410          FORMAT(' LA 6 "Best Period:',g22.15,'   s" VP 0.5 0.48')
              ncmd=ncmd+1
              cmd(ncmd)=text
              WRITE (text, 3420) dpera(kmax(4))
3420          FORMAT(' LA 7 "Best Period:',g22.15,'   s" VP 0.5 0.31')
              ncmd=ncmd+1
              cmd(ncmd)=text
           ENDIF                
       ENDIF
c 
c multicolor task
c if 'lc' and 'ef' xaxis is TIME and PHASE respectivaly
c 
c         for #plot,iflags(11), .GT. 1 or if nseries,iflags(10), .EQ. 1
c if 'ef' yaxis is Normal Intensity 
c          
c         iflags(7) .EQ. 0 
c    and  are true the xaxis condition 
c              or
c    and  hardness for 2 series
c label for x-axis for time 
c
c       write(*,*)'cpro(1:2)', cpro(1:2)
c       write(*,*)'iflags(11), iflags(10)',iflags(11), iflags(10)
       IF (cpro(1:2).eq.'lc') THEN
          IF (iflags(11).GT.1.OR.iflags(10).EQ.1) THEN
c             write(*,*)'iflags(11), iflags(10)',iflags(11), iflags(10)
             ncmd=ncmd+1
             If (iflags(2).EQ.0.OR.iflags(2).EQ.1.or.iflags(2).eq.4) 
     &          cmd(ncmd)=' LA Y1 Time (s)' 
             If (iflags(2).EQ.2) cmd(ncmd)=' LA Y1 Time (h)' 
             If (iflags(2).EQ.3) cmd(ncmd)=' LA Y1 Time (d)'
          ENDIF 
       ELSEIF (cpro(1:2).eq.'ef') THEN
          IF (iflags(11).GT.1.OR.iflags(10).EQ.1) THEN
c             write(*,*)'iflags(11), iflags(10)',iflags(11), iflags(10)
             ncmd=ncmd+1
             cmd(ncmd)=' LA Y1 Phase'
          ENDIF
c          write(*,*)'iflags(7)',iflags(7), iflags(10), iflags(11)
          IF (iflags(7).eq. 1) THEN
             IF(iflags(11).GT.1.OR.iflags(10).EQ.1) THEN
                DO i=1,iflags(10)
                   ncmd=ncmd+1
                   width=1
                   write(fmt,'(a,i2,a)') '(i', width ,')'
                   write(str,fmt)i+1
                   string='la y'//str
                   WRITE(text,5000)string,i
5000               FORMAT(' ',a6,' Norm Intens Ser ',i2)
c                   write(*,*)text
c
c                   WRITE(text,5000)i+1,i
c5000               FORMAT(' LA Y',i2,' Norm Intens Ser ',i2)
                   cmd(ncmd)=text
                ENDDO
             ELSEIF(iflags(11).EQ.1.and.iflags(10).EQ.2) THEN
                   ncmd=ncmd+1
                   cmd(ncmd)=' LA Y5 Norm Intens Ser 1+2'
             ENDIF     
          ENDIF   
       ENDIF
c
c
c       write(*,*)'iqdp(5)',iqdp(5)
       If(iqdp(5).eq.3.and.
     &   (cpro(1:2).eq.'ef'.or.cpro(1:2).eq.'lc'))then 
          ncmd=ncmd+1
c          write(*,*)'iqdp(5)',iqdp(5),iflags(10)
          If (iflags(10).eq.2) then 
             cmd(ncmd)=' Color 1 on 4'
             ncmd=ncmd+1
             cmd(ncmd)=' Plot vertical'  
          endif
       endif 
c
c add log-log for rebin results.
c
       IF(rflags(1).LT.0)THEN 
          ncmd=ncmd+1
          cmd(ncmd)=' LOG X'
          ncmd=ncmd+1
          cmd(ncmd)=' LOG Y'
       ENDIF         
c
c device
       ncmd=ncmd+1
       cmd(ncmd)=' DEV '//cqdp(1)
c       write(*,*)'cqdp(3)',cqdp(3)
       IF(cqdp(3).ne.'none') THEN
c       IF((cqdp(3).ne.'INDEF').or.(cqdp(3).ne. ' ').or.
c     &    (cqdp(3).ne.'-').or.(cqdp(3).ne.'default')) THEN
          ncmd=ncmd+1
          cmd(ncmd)='@'//cqdp(3)
       ENDIF
       RETURN 
       END  
