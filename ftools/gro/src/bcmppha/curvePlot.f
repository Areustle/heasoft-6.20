
c       --------------------------------------------------
c       filename: curvePlot.f
c       purpose:  plot spectra curves by one-dimensional arry
c                 x(1:npt), and y(1:npt)
c                 return number of spectra,x1,x2,x3,x4
c       author/date: C. Pan, Feb., 2002


        subroutine curvePlot(x1,x2,x3,x4,npt,x,y,npar,msg)
        real            x(5000), y(5000),xx,yy
        character(15)     s1,s2,ss*1,sx,sy,si,msg*1,msg1*10
        integer          x1,x2,x3,x4,i
        
c       find max. and min. 
        call MaxMin(x,npt, xmax,xmin) 
        call MaxMin(y,npt, ymax,ymin) 
    
c       Define coordinate range of graph (xmin < x < xmax, ymin < y < ymax),
c       and draw axes.
        
        CALL PGENV(xmin*0.9999,xmax*1.0001, 0, ymax*1.25,  0, 0)
c       Label the axes (note use of \u and \d for raising exponent).
        
         CALL PGMTXT('B', 3.2, 0.5, 0.5, 'Spectra Number')
         CALL PGMTXT('L', 3.4, 0.5, 0.5, 'Spectra')
      
c       Plot the line graph.
        CALL PGLINE(npt, x, y)
        CALL PGPT(npt, x, y, 18)
        write(s2,11) xmax
        write(s1,11) xmin
 11     format(f7.0)
        CALL PGMTXT('T', 1.0, 0.05, 0.5, 'spec_num'//s1)
        CALL PGMTXT('T', 1.0, 0.92, 0.5, 'spec_num='//s2)
       
        ss ='A' 
        write(*,*)'Click left mouse button to get the position' 
        write(*,*)'Click right mouse button to fix the position and 
     $  exits cursor'
       
        do i = 1, npar
           write(si,19) i
 19        format(i8)
           msg1 = msg//si
           call crmvblk(msg1)
           write(*,*) 'Chose spectra number: ',msg1
 20        call pgcurs(xx,yy,ss)
           if (xx.lt.xmin .or. xx.gt.xmax .or.  
     $        yy.lt.0 .or. yy.gt.ymax) then
              write(*,*)'Out of data range'
              goto 20
           else  
              write(*,*)'spec_num =', int(xx), ' spec =', yy
              if (ss .eq. 'A' .or. ss .eq. 'a') then
                write(sx,21)int(xx)
 21             format(i9)
                write(sy,22)yy
 22             format(f12.4)
                CALL PGMTXT('T', 1.0, 0.45, 0.5, sx)
                CALL PGMTXT('T', 1.0, 0.60, 0.5, sy)
                CALL pgstbg(0)
                goto 20
              endif
           endif
        
           if (i .eq. 1) then
             x1 = xx
           else if (i .eq. 2) then
              x2 = xx
           else if (i .eq. 3) then
              x3 = xx
           else if (i .eq. 4) then 
              x4 = xx
           end if
           call vlineplot(xx, xx, 0, ymax*1.25)
        enddo 
       return
        END

