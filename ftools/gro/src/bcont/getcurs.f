c       --------------------------------------------------
c       filename: getcurs.f
c       purpose:  plot light curves by one-dimensional arry
c                 x(1:npt), and y(1:npt)
c       author/date: C. Pan, Feb., 2002


        subroutine getcurs(xmin,xmax,ymin,ymax)
        INTEGER N, idet
        PARAMETER (N=8)
        real xx, yy, xmin, xmax, ymin, ymax 
        character(12) ss*1,sx,sy

        status = 0
        call uclgsi('idet',idet,status)
      
        IF (status.NE.0) THEN
           write(*,*) 'Error getting detector value!' 
           return     
        ENDIF
        idet = idet + 1
        CALL PGSVP(0.10, 0.95, (0.7+REAL(N-idet))/REAL(N)-0.07,
     :                         (0.7+REAL(N-idet+1))/REAL(N)-0.11)

        ss ='A' 
        write(*,*)'Click left mouse button to get the position'
        write(*,*)'Click right mouse button to exits cursor'
 20     call pgcurs(xx,yy,ss)
        if (xx.lt.xmin .or. xx.gt.xmax .or.
     $     yy.le.0 .or. yy.gt.ymax*1.005) then
           write(*,*)'Out of data range'
           goto 20
        else
           write(*,*)'TJD =', xx, ' Flux =', yy
           if (ss .eq. 'A' .or. ss .eq. 'a') then
             write(sx,30) xx
             write(sy,31) yy
 30          format(f12.3)
 31          format(f12.3)
             CALL PGSCH(0.65)
             CALL PGMTXT('T', 0.1, 0.40, 0.5, sx)
             CALL PGMTXT('T', 0.1, 0.56, 0.5, sy)
             CALL pgstbg(0)
             goto 20
            
           endif
        endif
        
        return
        end
     
