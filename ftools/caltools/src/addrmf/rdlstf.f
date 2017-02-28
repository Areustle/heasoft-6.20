
      SUBROUTINE rdlstf(lstfil, lstu, MXRMFS, nfiles, 
     &                  inrmfs, infact, contxt, errstat)

      INTEGER MXRMFS

      REAL    infact(*)

      INTEGER lstu, nfiles, errstat

      CHARACTER*(*) inrmfs(*)
      CHARACTER*(*) lstfil, contxt

c  Subroutine to read the list file and load the arrays of RMF names and 
c  weighting factors.

c  Arguments :
c     lstfil        c       i: name of file with list of RMFs
c     wgtfil        c       i: name of file with weightings
c     lstu          i       i: I/O unit for above
c     MXRMFS        i       i: Max number of RMFs allowed (parameter)
c     nfiles        i       r: Number of RMFs in lstfil
c     inrmfs        c       r: Array of RMF names
c     infact        r       r: Array of RMF weighting factors
c     contxt        c       r: error description
c     errstatus     i       r: Status  : 0 = OK, !0 = !OK

      INTEGER ierr, i

      character(511) instrg, outstr

      errstat = 0

c Open the list file

      OPEN(unit=lstu, file=lstfil, status='old')

c Read the lines of the list file, loading the arrays as we go

      nfiles = 0
      READ(lstu,'(a)',iostat=ierr) instrg

      CALL fcecho('Summing ...')

      DO WHILE ( ierr .EQ. 0 )

         IF ( nfiles .EQ. MXRMFS ) THEN
            contxt = 'I can only deal with 100 RMFs'
            CALL fcecho(contxt)
            errstat = 1
            RETURN
         ENDIF

         nfiles = nfiles + 1

         CALL crmvlbk(instrg)
         i = 1
         DO WHILE ( instrg(i:i) .NE. ' ' )
            i = i + 1
         ENDDO
         inrmfs(nfiles) = instrg(:i-1)
         READ(instrg(i:),'(f22.0)') infact(nfiles)

         outstr(14:) = inrmfs(nfiles)
         WRITE(outstr(1:13),'(1pe10.3, a3)') infact(nfiles), ' * '
         CALL fcecho(outstr)

         READ(lstu,'(a)',iostat=ierr) instrg

      ENDDO

      CLOSE(lstu)

      IF ( nfiles .EQ. 0 ) THEN
         contxt = 'Failed to read any input RMFs'
         CALL fcecho(contxt)
         errstat = 1
         RETURN
      ENDIF

      RETURN
      END






