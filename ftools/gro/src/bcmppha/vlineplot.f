c       --------------------------------------------------
c       filename: vlineplot.f
c       purpose:  plot vertical line to indicate TDJ
c       author/date: C. Pan, Feb., 2002
        
        subroutine vlineplot(xmin, xmax, ymin, ymax)
        parameter (npt = 100)
        real       x(npt),y(npt), xo(npt),xn(npt)
      
c       make five point arry to draw vertical line        
        do i=1,npt
          x(i) = xmin+(i-1)*(xmax-xmin)/((npt-1)*1.0)
          y(i) = ymin+(i-1)*(ymax-ymin)/((npt-1)*1.0)
          xo(i) = xmin
          xn(i) = xmax
        enddo
        call pgpt(npt,xo,y,-1)
        call pgpt(npt,xn,y,-1) 
        return
        end
