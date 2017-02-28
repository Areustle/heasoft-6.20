         Subroutine xsel_cmd(commnd, noption)

c --------------------------------------------------------------------
c
c This is the routine to process xselect command. It will be called by the
C Tcl-main program.
C
C The codes are borrowed from the xsel.f. For details and Modification
C history, please read the  original code.
C
c  Authors:    Ning Gan, 1999 January V2.0
c              kaa  5/8/13  modified to use common command processing routine
c
c -------------------------------------------------------------------

      implicit none
      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'

c ---------------------------------------------
c DECLARATIONS
c ---------------------------------------------
c Scratch variables
c Strings used, reused, reused again as temporary space
      character(255) str1

      character(16) commnd
      integer noption     


      IF(status.ne.0) THEN
            write(str1,'(a,i4)') 'Error in Xselect, no. ',status
            call XWRITE(str1,7)
C            call GTBUFSTOPSCRIPT()
            status = 0
            IF ( QBATCH ) call EXIT(1)
            return
      ENDIF

c set the option number  
      comno = noption

c Now process the command line

      call XSEL_PROC_COMMAND(commnd, 0, .true.)

C ------------------------------------------
C Scan the control logicals
C ------------------------------------------

      call xsel_set_control
      RETURN
      END
c
c ---------------------------------------------
c Code ends.
c --------------------------------------------- 
      
      Subroutine xsel_set_control()
      implicit none
      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc' 
      LOGICAL mytestt, mytestf,mytestu
      
      mytestf = .false.
      call set_xsel_ctrl("mytestf",mytestf)
      mytestt = .true.
      call set_xsel_ctrl("mytestt",mytestt)
      call set_xsel_ctrl("mytestu",mytestu)

      call set_xsel_ctrl("READ",read)
      call set_xsel_ctrl("SPEC",spec)
      call set_xsel_ctrl("CURV",curv)
      call set_xsel_ctrl("IMAGE",image)
      call set_xsel_ctrl("CLEANI",cleani)
      call set_xsel_ctrl("CLEAN",clean)
      call set_xsel_ctrl("DIRTY",dirty)
      call set_xsel_ctrl("FFCURV",ffcurv)
      call set_xsel_ctrl("HKCURV",hkcurv)
      call set_xsel_ctrl("SELCT",selct)
      call set_xsel_ctrl("HKSEL",hksel)
      call set_xsel_ctrl("FFTFL",fftfl)
      call set_xsel_ctrl("MERGED",merged)
      call set_xsel_ctrl("USEHK",usehk)
      call set_xsel_ctrl("HKREAD",hkread)
      call set_xsel_ctrl("MANYHK",manyhk)
      call set_xsel_ctrl("MERGHK",merghk)
      call set_xsel_ctrl("EXPAND",expand)
      call set_xsel_ctrl("MANY",many)
      call set_xsel_ctrl("MADE",made)
      call set_xsel_ctrl("LOADED",loaded)
      call set_xsel_ctrl("SHOWOC",showoc)
      call set_xsel_ctrl("WORK",work)
      call set_xsel_ctrl("WRKGTI",wrkgti)
      call set_xsel_ctrl("WORKHK",workhk)
      call set_xsel_ctrl("FILTER",filter)
      call set_xsel_ctrl("REGION",region)
      call set_xsel_ctrl("ASCTFL",asctfl)
      call set_xsel_ctrl("XWNTFL",xwntfl)
      call set_xsel_ctrl("XPHTFL",xphtfl)
      call set_xsel_ctrl("XPHSAV",xphsav)
      call set_xsel_ctrl("FITTFL",fittfl)
      call set_xsel_ctrl("DETFL",detfl)
      call set_xsel_ctrl("BININ",binin)
      call set_xsel_ctrl("BINOUT",binout)
      call set_xsel_ctrl("EVTSAV",evtsav)
      call set_xsel_ctrl("CLNSAV",clnsav)
      call set_xsel_ctrl("WTMAPB",wtmapb)
      call set_xsel_ctrl("ECHO",echo)
      call set_xsel_ctrl("TORDER",torder)
      call set_xsel_ctrl("UNBIN",unbin)
      call set_xsel_ctrl("USEQDP",useqdp)
      call set_xsel_ctrl("FAST",fast)
      call set_xsel_ctrl("FAINT",faint)
      call set_xsel_ctrl("VIMAGE",vimage)
      call set_xsel_ctrl("VSPEC",vspec)
      call set_xsel_ctrl("VREGION",vregion)
      call set_xsel_ctrl("VPHACUT",vphacut)
      call set_xsel_ctrl("VGISCLEAN",vgisclean)
      call set_xsel_ctrl("INTENS",intens)
      call set_xsel_ctrl("HAVEMKF",havemkf)
      call set_xsel_ctrl("SMOOTH",smooth)
      call set_xsel_ctrl("SAVSMOOTH",savsmooth)
      return 
      end
