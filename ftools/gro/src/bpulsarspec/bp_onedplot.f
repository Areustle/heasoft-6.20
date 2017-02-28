
c       --------------------------------------------------
c       filename: bp_onedplot.f
c       purpose:  plot light curves by one-dimensional arry
c                 x(1:npt), and y(1:npt)
c       author/date: C. Pan, Feb., 2002


        subroutine bp_onedplot(npt,x,y,z, ph1, ph2, decnum,
     &  chani, chanj, chanm, chann, tjdsta, tjdstp)
        double precision x(5000), y(5000),tjdsta, tjdstp
        double precision z(5000)
        character(12)     s1,s2,ss*1,sx,sy,p1*2,p2*2,s3*2
        integer  i,npt,ph1,ph2,decnum,chani,chanj,chanm,chann
        character(2) schani,schanj,schanm,schann
        real  xr(5000), yr(5000),zr(5000),xmax,xmin,ymax,ymin,yhi,ylo
       
        write(p1,1) ph1
        write(p2,1) ph2
 1      format(i2)

        IF (npt.eq.1) THEN
        write(*,*)'No enough data to plot!'
        return
        ENDIF

        do i=1,npt
         xr(i)= x(i)-x(1)
         yr(i)= y(i)*1.0  
         zr(i)= z(i)*1.0     
        enddo

c       /* find max. and min. */
        call MaxMin(xr,npt, xmax,xmin) 
        call MaxMin(yr,npt, ymax,ymin) 
     
c       Define coordinate range of graph (xmin < x < xmax, ymin < y < ymax),
c       and draw axes.
        
        CALL PGENV(xmin,xmax, ymin/1.25, ymax*1.25,  0, 0)
c       Label the axes (note use of \u and \d for raising exponent).
        CALL PGMTXT('B', 2.5, 0.5, 0.5, 'TJDstop - TJDstart')
        CALL PGMTXT('L', 2.0, 0.5, 0.5, 'Hardness')
         CALL PGSCH(1.5)
         CALL PGMTXT('T', 1.6, 0.5, 0.5, 
     &  'Hardness vs. (TJDstop - TJDstart)')
         CALL PGSCH(1.0)
     
c       Plot the line graph.
        CALL PGLINE(npt, xr, yr)
        CALL PGPT(npt, xr, yr, 18)
       
        write(s2,10) tjdsta
        write(s1,10) tjdstp
 10     format(f12.6)
        write(s3,11) decnum
 11     format(i2) 

        write(schani,12) chani
        write(schanj,12) chanj
        write(schanm,12) chanm
        write(schann,12) chann
 12     format(i2) 
       
        CALL PGMTXT('T', -2.0, 0.25, 0.5, 'TJD:'//s2//' -'//s1)
        CALL PGMTXT('T', -4.0, 0.15, 0.5, 'Phase: '//p1//'- '//p2)
        CALL PGMTXT('T', -6.0, 0.13, 0.5, 'Detector:'//s3)
        CALL PGMTXT('T', -2.0, 0.85, 0.5, 'Soft:'//schani//' -'//schanj)
        CALL PGMTXT('T', -4.0, 0.85, 0.5, 'Hard:'//schanm//'-'//schann)

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
        if (xx.lt.xmin .or. xx.gt.xmax .or.  
     $     yy.lt.0 .or. yy.gt.ymax*1.25) then
           write(*,*)'Out of data range'
           goto 20
        else  
           write(*,*)'TJD =', xx, ' spec =', yy
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

