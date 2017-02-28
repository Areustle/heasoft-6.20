
c       --------------------------------------------------
c       filename: onedplot
c       purpose:  plot light curves by one-dimensional arry
c                 x(1:npt), and y(1:npt)
c       author/date: C. Pan, Feb., 2002


        subroutine onedplot(npt,x,y,z)
        double precision  x(200000),z(200000)
        integer              y(200000)
        character(12)     s1,s2,ss*1,sx,sy,p1*2,p2*2,s3*2
        character(2)      schani, schanj, schanm, schann
        integer  i, npt
        real    xr(200000), yr(200000), zr(200000),xmax,xmin,ymax
        real ymin,yhi,ylo
        real    tjd1,tjd2
                
        IF (npt.eq.1) THEN
        write(*,*)'No enough data to plot!'
        return
        ENDIF
 
        do i=1,npt
         xr(i)= (x(i)-x(1))*1.0   
         yr(i)= y(i)*1.0  
         zr(i)= z(i)*1.0
         write(*,*)'x,y,z',i,  x(i), y(i),  z(i)
        enddo

c       /* find max. and min. */
        call MaxMin(xr,npt, xmax,xmin) 
        call MaxMin(yr,npt, ymax,ymin) 
     
c       Define coordinate range of graph (xmin < x < xmax, ymin < y < ymax),
c       and draw axes.
        
        CALL PGENV(xmin,xmax, 20000, 24000,  0, 0)
c       Label the axes (note use of \u and \d for raising exponent).
        CALL PGMTXT('B', 2.5, 0.5, 0.5, 'Time(TJD)')
        CALL PGMTXT('L', 2.5, 0.5, 0.5, 'Counts')
         CALL PGSCH(1.5)
         CALL PGMTXT('T', 1.6, 0.5, 0.5, 'COUNTS vs. TJD')
         CALL PGSCH(1.0)
     
c       Plot the line graph.
        CALL PGLINE(npt, xr, yr)
        CALL PGPT(npt, xr, yr, 18)
        write(*,*)'we are here',xmax,xmin,ymax,ymin
c       plot error bar
        do i=1,npt
          yhi = yr(i)+2.0*z(i)
          ylo = yr(i)-2.0*z(i)
         
          call pgpoint(1,xr(i),yr(i),17)
          call pgerry(1,xr(i), ylo, yhi, 1.0)
        enddo
      
        if (devflg .ne. 0) then
           return
        endif
         
        ss ='A' 
        write(*,*)'Click left mouse button to get the position' 
        write(*,*)'Click right mouse button to exits cursor' 
 20     call pgcurs(xx,yy,ss)
        if (xx.lt.0.0 .or. xx.gt.1.0 .or.  
     $     yy.lt.0 .or. yy.gt.ymax*1.25) then
           write(*,*)'Out of data range'
           goto 20
        else  
           write(*,*)'Phase =', xx, ' spec =', yy
           if (ss .eq. 'A' .or. ss .eq. 'a') then
             write(sx,30) xx
             write(sy,31) yy
 30          format(f12.6)
 31          format(f12.6)
             CALL PGMTXT('T', 0.90, 0.40, 0.5, sx)
             CALL PGMTXT('T', 0.90, 0.56, 0.5, sy)
             CALL pgstbg(0)
             goto 20
           endif
        endif 
       return
        END

