c       --------------------------------------------------
c       filename: bgetdet.f
c       purpose:  plot light curves by one-dimensional arry
c                 x(1:npt), and y(1:npt)
c       author/date: C. Pan, Feb., 2002


        subroutine bgetdet(ndet, det,xmin,xmax,ymin,ymax)
        real yc(8), xx, yy, xmin, xmax, ymin, ymax 
        data yc/0.89,0.77,0.65,0.52,0.40,0.26,0.15,0.02/
        INTEGER N, ndet, det(8), k, I
        PARAMETER (N=8)
        character(1)  ss*1

    
        CALL PGSVP(0.0,1.0,0.0,1.0) 
        CALL PGSWIN(0.0,1.0,0.0,1.0)    
        do I=1,8
           det(I) = 0
        enddo
        ss ='A' 
        write(*,*)'Click left mouse to select detector/detectors'
        write(*,*)'to be used in statistical calculation:' 
        write(*,*)'For example, move the mouse on Det2 on the left'
        write(*,*)'side of the graphic, then click left button to' 
        write(*,*)'select it, then move mouse on Det4, click left' 
        write(*,*)'button to select it. After finishing the' 
        write(*,*)'selection(s), click the right button to exit.'
        write(*,*)'To close the graphic, go to the options menu and'
        write(*,*)'click on Exit.'
        
        k = 1   
 20     call pgcurs(xx,yy,ss) 
        do I = 1, 8        
           if (yy.ge.(yc(I)-0.03) .and. yy.le.(yc(I)+0.03)) then
              det(k) = I-1
              ndet = k-1
              k = k+1
           endif
        enddo
        if (ss .eq. 'A' .or. ss .eq. 'a') then
           goto 20
        endif
 
        return
        end
     
