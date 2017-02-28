*+      
      subroutine gt_crf_grids(iunit,n_energy,ntheta,nphi,
     $     lecol,hecol,thcol,phcol,datnam,
     $     crf_col,lo_energy,hi_energy,
     $     theta,phi,tdim,ndims,thind,phind,status)
      implicit none
      integer iunit,n_energy,ntheta,nphi,crf_col,status
      integer tdim(3),ndims,thind,phind
      double precision lo_energy(n_energy),hi_energy(n_energy)
      double precision theta(*),phi(*)
      integer lecol,hecol,thcol,phcol
      character*(*) datnam

C Description
C  Subroutine to get everything you need to know about an CRF file to 
C  use the CRF array column entry. Handles up to 3 dimensions in the CRF 
C  array.
C Passed Parameters
C  IUNIT            i   : The Unit number of the CRF file
C  N_ENERGY         i   : Size of Energy column element vectors
C  N_THETA          i   : Size of Theta column element vector
C  N_PHI            i   : Size of Phi column element vector
C  LECOL,HECOL      i   : Low and High Energy column numbers
C  THCOL            i   : Theta column number
C  PHCOL            i   : Phi column number
C  DATNAM           i   : Name of the CRF column
C  CRF_COL            o : Column number of CRF array
C  LO_ENERGY,HI_ENERGY o : Energy grids
C  THETA              o : Theta grid
C  PHI                o : Phi grid
C  TDIM               o : Dimensions of the CRF array
C  NDIMS              o : Number of Dimensions in the CRF array
C  THIND,PHIND        o : Theta and Phi dimension indices
C                        (e.g., thind=2 ==> the second dimension of the CRF
C                        array is the theta dimension) _ind=0 means dimension
C                        not present in array.
C  IERR               o : Subroutine error flag (zero = OK)
C
C
C Author/Modification History
C  Lawrence E Brown (1.0.1:94 Dec) changed tnull to double precision
C  Lawrence E Brown (1.0.0:94 Aug) Original

      character(7) version
      parameter (version='1.0.1') 
*-

C
     
C LOCAL variables
      character(80) context,comment
      logical anyf
      double precision tnull
      integer icol,i
      character(20) ctyp(3),unitst
      character(8) keyroot,keywrd
      character(32) errstr, wrnstr
      errstr = '** GT_CRF_GRIDS '//version//' ERROR:'
      wrnstr = '** GT_CRF_GRIDS '//version//' WARNING:'
      

      tnull=0
      if(status.ne.0) return
      call ftgcvd(iunit,lecol,1,1,n_energy,tnull,lo_energy,anyf,status)
      if (status .ne. 0) then
         context = errstr//
     $        ' getting energ_lo column in CRF file'
         call fcerr (context)
         call fcerrm(status)
         goto 999
      endif
      call ftgcvd(iunit,hecol,1,1,n_energy,tnull,hi_energy,anyf,status)
      if (status .ne. 0) then
         context = errstr//
     $        ' getting energ_hi column in CRF file'
         call fcerr (context)
         call fcerrm(status)
         goto 999
      endif
      if(thcol.ne.0) then
         call ftgcvd(iunit,thcol,1,1,ntheta,tnull,theta,anyf,status)
         if (status .ne. 0) then
            context = errstr//
     $           ' getting theta column in CRF file'
            call fcerr (context)
            call fcerrm(status)
            goto 999
         endif 
C check units
         keyroot= '        '
         unitst='                    '
         call ftkeyn('TUNIT',thcol,keyroot,status)
         call ftgkys(iunit,keyroot,unitst,comment,status)
         if(unitst(1:6).eq.'arcmin') then
            continue
         else if (unitst(1:6).eq.'degrees') then
            do i=1,ntheta
               theta(i)=theta(i)/60.0
            enddo
         else
            context = errstr//
     $           'Unknown units on THETA column in CRF file'
            call fcerr(context)
            status= 3
            goto 999
         endif
      endif
      if(phcol.ne.0) then
         call ftgcvd(iunit,phcol,1,1,nphi,tnull,phi,anyf,status)
         if (status .ne. 0) then
            context = errstr//
     $           ' getting phi column in CRF file'
            call fcerr(context)
            call fcerrm(status)
            goto 999
         endif 
C check units
         keyroot='        '
         unitst='                    '
         call ftkeyn('TUNIT',phcol,keyroot,status)
         call ftgkys(iunit,keyroot,unitst,comment,status)
         if(unitst(1:6).eq.'arcmin') then
            continue
         else if (unitst(1:6).eq.'degrees') then
            do i=1,nphi
               phi(i)=phi(i)/60.0
            enddo
         else
            context = errstr//
     $           'Unknown units on PHI column in CRF file'
            call fcerr(context)
            status=3
            goto 999
         endif
      endif
      call ftgcno(iunit,.false.,datnam,crf_col,status)
      if (status .ne. 0) then
         context = errstr //
     $        ' getting column in CRF file:'//datnam
         call fcerr (context)
         call fcerrm(status)
         goto 999
      endif
      call ftgtdm(iunit,crf_col,3,ndims,tdim,status)
      if(status.ne.0) then
         context = errstr //
     $        ' getting dimensions in CRF file'
         call fcerr (context)
         call fcerrm(status)
         goto 999
      endif
      keyroot='        '
      do icol=1,3
         ctyp(icol)='                     '
      enddo
      call ftkeyn('CTYP',crf_col,keyroot,status)
      do icol=1,ndims
         keywrd='        '
         call ftnkey(icol,keyroot,keywrd,status)
         call ftgkys(iunit,keywrd,ctyp(icol),comment,status)
      enddo
      if(status.ne.0) then
         context = errstr //
     $        ' getting CTYP keywords from CRF file'
         call fcerr (context)
         call fcerrm(status)
         goto 999
      endif
      if(ctyp(1).ne.'ENERGY') then
         context=errstr //
     $        'Energy axis is not first axis in CRF file'
         call fcerr(context)
         context=errstr //
     $        'Can''t continue'
         call fcerr(context)
         goto 999
      endif
      thind=0
      phind=0
      do i=2,3
         if(ctyp(i).eq.'THETA') thind=i
         if(ctyp(i).eq.'PHI') phind=i
      enddo
 999  return
      end
      
