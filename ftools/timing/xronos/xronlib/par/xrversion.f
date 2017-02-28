c
      SUBROUTINE xrversion(tasknm, taskvr)
c
c This routine takes the name of a task and returns the
c task and its version plus the version for the entire package
c
c I  tasknm  s  Task name
c O  taskvr  s  Full version string for task
c
c Input variable
       CHARACTER*(*) tasknm, taskvr
c Local variable
       LOGICAL found
       INTEGER i, slen, numt, LENACT
       PARAMETER(numt = 12)
       character(10) taskls(numt), verls(numt)
       CHARACTER*(*) pkgver
       character(100) ds
c
c Edit this section for version changes
c
       PARAMETER(pkgver = 'xronos5.22')

       DATA taskls/'autocor' ,'crosscor','earth2sun','efold'   ,
     &             'efsearch','lcmath'  ,'lcstats'  ,'lcurve'  ,
     &             'listdata','powspec' ,'rbf2fits' ,'timeskew'/

       DATA verls /'1.0'     ,'1.0'     ,'1.0'      ,'1.1'     ,
     &             '1.1'     ,'1.1'     ,'1.0'      ,'1.0'     ,
     &             '1.0'     ,'1.0'     ,'1.0'      ,'1.0'     /
c
       ds = tasknm
       call locase(ds)
       slen = LENACT(ds)
       i = 1
       found = .FALSE.
       do while ( .not.found .and. i.le.numt ) 
          if ( ds(:slen).eq.taskls(i)(:LENACT(taskls(i))) ) then
             found = .TRUE.
          else
             i = i + 1
          endif
       enddo

       if ( found ) then
          write(taskvr,'(2(a,1x),3a)') ds(:slen),
     &                                 verls(i)(:LENACT(verls(i))),
     &                                '(', pkgver, ')'
       else
          taskvr = ' '
       endif

       RETURN
       END
