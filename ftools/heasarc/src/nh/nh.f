c
c This program returns, for a given position in the sky,
c the Nh value as mesaured by the HI map of Lockman or
c by that of ...
c 
        subroutine nh
c
c Input paramaters
        INTEGER*4 usemap, tchat, lchat, ierr
        
        DOUBLE PRECISION equinox, disio, size
        
        character(80) rastr, decstr
        character(160) map, altmap
c 
c Local paramaters
        INTEGER parse, lenact
        
        REAL*4 nh1, nh2, altnh1, altnh2
        
        character(255) context
        character(80) istring, log_fil 
        character(16) program
        
        EXTERNAL lenact
        
        DATA istring,parse /' ',0/
        DATA program /'nh'/
c
c Initialize variable
        ierr=0
        context=' ' 
c        write(*,*)'write before'
c
c retrive paramaters
        CALL nhinit(equinox, rastr, decstr, size, disio, map, altmap,
     &              usemap, tchat, lchat, ierr)
        IF(ierr.eq.0) THEN
           call xchaty (tchat, lchat)
           IF (lchat.GE.tchat) THEN
               log_fil = '+'//program(:lenact(program))// '.log'
               CALL setlog(istring,parse,log_fil,' ')
           ENDIF
c
c calculate Nh
           CALL NHMAKE(equinox, rastr, decstr, size, disio, map,
     &                 altmap, nh1, nh2, altnh1, altnh2,
     &                 usemap, tchat, lchat, ierr)
           IF(ierr.NE.0) THEN
               WRITE(context, '('' Status from nhmake: '',I4)')ierr
               CALL XAERROR(context, 1)
               GOTO 1000
           ENDIF

           CALL uclpsr('avnh', nh1, ierr)
           IF(ierr.NE.0) THEN
               WRITE(context, '('' Status from uclpsr: '',I4)')ierr
               CALL XAERROR(context, 1)
               GOTO 1000
           ENDIF
           CALL uclpsr('avwnh', nh2, ierr)
           IF(ierr.NE.0) THEN
               WRITE(context, '('' Status from uclpsr: '',I4)')ierr
               CALL XAERROR(context, 1)
               GOTO 1000
           ENDIF
           CALL uclpsr('alnh', altnh1, ierr)
           IF(ierr.NE.0) THEN
               WRITE(context, '('' Status from uclpsr: '',I4)')ierr
               CALL XAERROR(context, 1)
               GOTO 1000
           ENDIF
           CALL uclpsr('alwnh', altnh2, ierr)
           IF(ierr.NE.0) THEN
               WRITE(context, '('' Status from uclpsr: '',I4)')ierr
               CALL XAERROR(context, 1)
               GOTO 1000
           ENDIF

        ELSE
           WRITE(context, '('' Status from nhinit: '', I4)')ierr
           CALL XAERROR(context, 1)
        ENDIF
1000    RETURN
        END
