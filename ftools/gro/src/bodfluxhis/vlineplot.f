c       --------------------------------------------------
c       filename: vlineplot.f
c       purpose:  plot vertical line to indicate TDJ
c       author/date: C. Pan, Feb., 2002

        subroutine vlineplot(xmin, xmax, ymin, ymax)
        real  x(100),y(100), xo(100),xn(100)
      
c       make five point arry to draw vertical line        
        do i=1,100
          x(i) = xmin+(i-1)*(xmax-xmin)/(100-1)
          y(i) = ymin+(i-1)*(ymax-ymin)/(100-1)
          xo(i) = xmin
          xn(i) = xmax
        enddo
      
        call pgpt(100,xo,y,-1)
        call pgpt(100,xn,y,-1) 
        return
        end
