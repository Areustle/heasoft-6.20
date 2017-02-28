      SUBROUTINE xrwrsta(otype,series,nstacol, colnam,colcom)
      implicit none

c
c  I otype    i type of output file
c  I series   i number of time series analysed (iflags(10))
c  O nstacol  i number of statistivc to add as keywords or column
c  O colnam   c  array of column name
c  O colcom   c  array of column/keyowrds comment 
c Input variable
       INTEGER otype,series 
c
c Output variable
       INTEGER cmax, nstacol
       PARAMETER (cmax=100)  
       character(16) colnam(cmax)
       character(32) colcom(cmax)
c Local variable
       INTEGER i, j,istat
       character(16) names
       character(32) comment
c
       do i=1,cmax
          colnam(i)=' '
          colcom(i)=' '
       enddo
       i=1 
       DO j=1,series
          call ftkeyn('AVRGE_',j,colnam(i),istat)
          colcom(i)='Avg, count/s in interval'
          call ftkeyn('FREXP_',j,colnam(i+1),istat)
          colcom(i+1)='Avg. Frac exposure in frame'
          call ftkeyn('VAROB_',j,colnam(i+2),istat)
          colcom(i+2)='observed variance '
          call ftkeyn('VAREX_',j,colnam(i+3),istat)
          colcom(i+3)='expected variance '
          call ftkeyn('THRDM_',j,colnam(i+4),istat)
          colcom(i+4)='third moment'
          call ftkeyn('MININ_',j,colnam(i+5),istat)
          colcom(i+5)='Minimun Intensity'
          call ftkeyn('MAXIN_',j,colnam(i+6),istat)
          colcom(i+6)='Maximun Intensity'
          call ftkeyn('EXVAR_',j,colnam(i+7),istat)
          colcom(i+7)='excess of variance '
          call ftkeyn('CHI2_',j,colnam(i+8),istat)
          colcom(i+8)='Chi squared '
          call ftkeyn('RMS_',j,colnam(i+9),istat)
          colcom(i+9)=' RMS variability'
          call ftkeyn('AVRGE_E',j,colnam(i+10),istat)
          colcom(i+10)='Error Avg count/s'
          call ftkeyn('VAROB_E',j,colnam(i+11),istat)
          colcom(i+11)='Error observed variance '
          call ftkeyn('VAREX_E',j,colnam(i+12),istat)
          colcom(i+12)='Error expected variance '
          call ftkeyn('CHI2_E',j,colnam(i+13),istat)
          colcom(i+13)='Error Chi2'
          call ftkeyn('RMS_E',j,colnam(i+14),istat)
          colcom(i+14)='RMS variability'
          i=i+15
       ENDDO
       Nstacol=i
       RETURN
       END 
