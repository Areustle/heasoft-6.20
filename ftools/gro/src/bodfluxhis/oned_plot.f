
c       --------------------------------------------------
c       filename: oned_plot.f
c       purpose:  plot light curves by one-dimensional arry
c                 x(1:npt), and y(1:npt)
c       author/date: C. Pan, Feb., 2002


        subroutine oned_plot(npt,x,y,z,msg,devflg,sta1,stp1,sta2,stp2,
     &           tjdb, tjde, nvp)
        real     x(10000), y(10000),z(10000),xx,yy,yhi,ylo
        real     tjdb(*),tjde(*)
        character*(*)   msg
        character(15)     s1,s2,ss*1,sx,sy
       
c       /* find max. and min. */
        call MaxMin(x,npt, xmax,xmin) 
        call MaxMin(y,npt, ymax,ymin) 
    
c       Define coordinate range of graph (xmin < x < xmax, ymin < y < ymax),
c       and draw axes.
        
        CALL PGENV(xmin*0.9998,xmax*1.0002, 0, ymax*1.25,  0, 0)
c       Label the axes (note use of \u and \d for raising exponent).
         CALL PGMTXT('B', 3.2, 0.5, 0.5, 'TDJ Start Time')
         CALL PGMTXT('L', 3.4, 0.5, 0.5, 'Flux')
       
c       Plot the line graph.
        CALL PGLINE(npt, x, y)
        CALL PGPT(npt, x, y, 18)
        call vlineplot(sta1, stp1, -0, ymax*1.25)
        call vlineplot(sta2, stp2, -0, ymax*1.25)
        do i = 1, nvp
           call vlineplot(tjdb(i), tjde(i), -0, ymax*1.25)
        enddo
     
        write(s2,10) xmax
        write(s1,10) xmin
 10     format(f8.0)
        
        CALL PGMTXT('T', 1.0, 0.05, 0.5, 'TJD='//s1)
        CALL PGMTXT('T', 1.0, 0.92, 0.5, 'TJD='//s2)
c       plot error bar
        do i=1,npt
          yhi = y(i)+2.0*z(i)
          ylo = y(i)-2.0*z(i)
          call pgpoint(1,x(i),y(i),17)
          call pgerry(1,x(i), ylo, yhi, 1.0)
        enddo
      
        if (devflg .ne. 0) then
           return
        endif
         
        ss ='A' 
        write(*,*)'Click left mouse button to get the position' 
        write(*,*)'Click right mouse button to exits cursor' 
 20     call pgcurs(xx,yy,ss)
        if (xx.lt.xmin .or. xx.gt.xmax .or.  
     $     yy.lt.0 .or. yy.gt.ymax*1.25) then
           write(*,*)'Out of data range'
           goto 20
        else  
           write(*,*)'TJD =', xx, ' flux =', yy
           if (ss .eq. 'A' .or. ss .eq. 'a') then
             write(sx,30) xx
             write(sy,31) yy
 30          format(f12.0)
 31          format(f12.6)
             CALL PGMTXT('T', 1.0, 0.45, 0.5, sx)
             CALL PGMTXT('T', 1.0, 0.56, 0.5, sy)
             CALL pgstbg(0)
             goto 20
           endif
        endif 
       return
        END

