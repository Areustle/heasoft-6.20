set env(FITSVIEWER_LIBRARY) /
set env(FV) /
set env(FV_ISEXEC) 1

# The order is important! The derived class file should come after 
# the base class file from which it inherits. 

#source http stuff
source tklib/tcl8/8.4/http-2.7.12.tm
source tklib/tcl8/8.5/msgcat-1.5.2.tm

#source the iwidgets stuff
source tklib/itk3.3/itk.tcl
source tklib/itk3.3/Archetype.itk
source tklib/itk3.3/Toplevel.itk
source tklib/itk3.3/Widget.itk
source tklib/iwidgets4.0.1/scripts/colors.itcl
source tklib/iwidgets4.0.1/scripts/scopedobject.itcl
source tklib/iwidgets4.0.1/scripts/buttonbox.itk
source tklib/iwidgets4.0.1/scripts/extbutton.itk
source tklib/iwidgets4.0.1/scripts/shell.itk
source tklib/iwidgets4.0.1/scripts/dialogshell.itk
source tklib/iwidgets4.0.1/scripts/dialog.itk
source tklib/iwidgets4.0.1/scripts/calendar.itk
source tklib/iwidgets4.0.1/scripts/canvasprintbox.itk
source tklib/iwidgets4.0.1/scripts/canvasprintdialog.itk
source tklib/iwidgets4.0.1/scripts/labeledframe.itk
source tklib/iwidgets4.0.1/scripts/checkbox.itk
source tklib/iwidgets4.0.1/scripts/colors.itcl
source tklib/iwidgets4.0.1/scripts/labeledwidget.itk
source tklib/iwidgets4.0.1/scripts/entryfield.itk
source tklib/iwidgets4.0.1/scripts/combobox.itk
source tklib/iwidgets4.0.1/scripts/datefield.itk
source tklib/iwidgets4.0.1/scripts/dateentry.itk
source tklib/iwidgets4.0.1/scripts/disjointlistbox.itk
source tklib/iwidgets4.0.1/scripts/extfileselectionbox.itk
source tklib/iwidgets4.0.1/scripts/extfileselectiondialog.itk
source tklib/iwidgets4.0.1/scripts/feedback.itk
source tklib/iwidgets4.0.1/scripts/fileselectionbox.itk
source tklib/iwidgets4.0.1/scripts/fileselectiondialog.itk
source tklib/iwidgets4.0.1/scripts/finddialog.itk
source tklib/iwidgets4.0.1/scripts/scrolledwidget.itk
source tklib/iwidgets4.0.1/scripts/hierarchy.itk
source tklib/iwidgets4.0.1/scripts/hyperhelp.itk
source tklib/iwidgets4.0.1/scripts/mainwindow.itk
source tklib/iwidgets4.0.1/scripts/menubar.itk
source tklib/iwidgets4.0.1/scripts/messagebox.itk
source tklib/iwidgets4.0.1/scripts/messagedialog.itk
source tklib/iwidgets4.0.1/scripts/notebook.itk
source tklib/iwidgets4.0.1/scripts/optionmenu.itk
source tklib/iwidgets4.0.1/scripts/pane.itk
source tklib/iwidgets4.0.1/scripts/panedwindow.itk
source tklib/iwidgets4.0.1/scripts/promptdialog.itk
source tklib/iwidgets4.0.1/scripts/pushbutton.itk
source tklib/iwidgets4.0.1/scripts/radiobox.itk
source tklib/iwidgets4.0.1/scripts/regexpfield.itk
source tklib/iwidgets4.0.1/scripts/roman.itcl
source tklib/iwidgets4.0.1/scripts/scrolledcanvas.itk
source tklib/iwidgets4.0.1/scripts/scrolledframe.itk
source tklib/iwidgets4.0.1/scripts/scrolledtext.itk
source tklib/iwidgets4.0.1/scripts/scrolledhtml.itk
source tklib/iwidgets4.0.1/scripts/scrolledlistbox.itk
source tklib/iwidgets4.0.1/scripts/selectionbox.itk
source tklib/iwidgets4.0.1/scripts/selectiondialog.itk
source tklib/iwidgets4.0.1/scripts/spindate.itk
source tklib/iwidgets4.0.1/scripts/spinner.itk
source tklib/iwidgets4.0.1/scripts/spinint.itk
source tklib/iwidgets4.0.1/scripts/spintime.itk
source tklib/iwidgets4.0.1/scripts/tabnotebook.itk
source tklib/iwidgets4.0.1/scripts/tabset.itk
source tklib/iwidgets4.0.1/scripts/timefield.itk
source tklib/iwidgets4.0.1/scripts/timeentry.itk
source tklib/iwidgets4.0.1/scripts/toolbar.itk
source tklib/iwidgets4.0.1/scripts/watch.itk

#source the pow
source tklib/pow/Notifications.tcl
source tklib/pow/Shape.tcl
source tklib/pow/Region.tcl
source tklib/pow/RegionList.tcl
source tklib/pow/html_library.tcl
source tklib/pow/notebook.tcl
source tklib/pow/pow.tcl
source tklib/pow/powEdit.tcl
source tklib/pow/powImgProbe.tcl
source tklib/pow/powMovie.tcl
source tklib/pow/powProfile.tcl
source tklib/pow/powRgn.tcl
source tklib/pow/powRuler.tcl
source tklib/pow/powScript.tcl
source tklib/pow/PowCmdsClass.tcl
source tklib/pow/powXRange.tcl
# source tklib/pow/POWplot.tcl
# source visu_widgets.tcl
# POWplot

# source  fv
#

source ../class/FVFile.tcl	
source ../class/FVFtool.tcl	
source ../class/FVSkyview.tcl	
source ../class/FVVizier.tcl	
source ../class/Table.tcl	
source ../class/FitsTable.tcl	
source ../class/FitsBaseCalculator.tcl	
source ../class/FitsCalculator.tcl	
source ../class/FitsClipBoard.tcl	
source ../class/FitsDelCalculator.tcl	
source ../class/FitsExtension.tcl	
source ../class/FitsFile.tcl	
source ../class/FitsFileSelection.tcl	
source ../class/FitsHeader.tcl	
source ../class/FitsHistoParam.tcl	
source ../class/FitsImage.tcl	
source ../class/CubeImage.tcl	
source ../class/FitsImgPlotSel.tcl	
source ../class/FitsPlotSel.tcl	
source ../class/FitsSelCalculator.tcl	
source ../class/NewExtension.tcl	
source ../class/NewFITS.tcl	
source ../class/NewImage.tcl	
source ../class/NewTable.tcl	
source ../class/RemoteAccess.tcl	
source ../class/VectorTable.tcl	
source ../class/WFPC2Image.tcl	
source ../class/fvApp.tcl	
source ../class/fvPreferences.tcl	
source ../class/fvWinKeeper.tcl	

#plain fv tcl/tk files
source ../class/ftp_lib.tcl
source ../class/XPA_access.tcl
source ../class/fedit.tcl

set origPath ""
set xpaBinPath ""

# check to see if fv is part of environment 
if [info exists env(PATH)] {
   set origPath $env(PATH)
   set token [split $origPath ":"]
   foreach dir $token {
      set checkName [format "%s/fv" $dir]
      if [file exists $checkName] {
         set xpaBinPath $dir/xpabin
         if [file exists $xpaBinPath] {
            break
         } else {
            set xpaBinPath ""
         }
      }
   }
}

# hopefully it is going to be part of command line
if { $xpaBinPath == "" } {
   set xpaBinPath $argv0

   if { [string range $xpaBinPath 0 0] != "/" } {
      # relative path
      set xpaBinPath [format "%s/%s" [pwd] $xpaBinPath]
   }
   set xpaBinPath [file dirname $xpaBinPath]/xpabin
}

set env(PATH) "$origPath:$xpaBinPath"

eval fvInit $argv
