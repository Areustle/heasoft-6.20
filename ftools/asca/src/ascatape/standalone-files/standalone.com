$! Command file for the standalone version of ascatape
$!
$!	Don Jennings
$!	NASA/GSFC
$!	May, 1994
$!
$! before the standalone version of readtape can be compiled, one line of
$! code within the readtape.f file must be changed. Edit this file and
$! replace the statement:
$!   
$!                standalone = .false.
$!
$! with
$!
$!                standalone = .true.
$!
$!
$! set the following variable to point to the location of the FITSIO 
$! subroutine library on your system.
$!
$ fitsio = "ftools:[host]fitsio/lib"
$!
$if f$getsyi("ARCH_TYPE") .eq. 1
$then align_qual = ""
$else align_qual := -
      /WARN=ALIGN /ALIGN=(COMMONS=STANDARD,RECORDS=PACKED)
$endif
$fortran 'align_qual' /extend ftread.f
$set command/object ftread_subcommands
$fortran 'align_qual' standalone.f
$fortran 'align_qual' ascatape.f
$fortran 'align_qual' utility_vms.f
$fortran 'align_qual'  subdir_sort.f
$link/exe=ascatape standalone,ascatape,utility_vms,subdir_sort,ftread,-
                   ftread_subcommands,'fitsio'
$dele *.obj;*
$purge
$exit
