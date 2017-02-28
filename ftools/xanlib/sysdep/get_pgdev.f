c get_pgdev - get PGPLOT device
      subroutine get_pgdev(device)
c
c Description :
c  Looks for a translation of the environment variable PGPLOT_TYPE 
c  and if it doesn't find it it puts '?' into character variable device.
c  This is only a temporary version to get the QDP part of XRONOS
c  working.
c
c Export :
      character*(*) device
c Local variables :
      character(80) name
      integer len_pgplot_type, len_name, len
c-
      len_pgplot_type=len('PGPLOT_TYPE')
      call trlog ('PGPLOT_TYPE',len_pgplot_type,name,len_name)
      if(len_name.gt.0) then
         device=name(:len_name)
      else
         device='?'
      endif

      end

