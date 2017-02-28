itcl::class FtoolParameter {

   constructor {args} {}
   destructor {}

   public {
      variable name
      variable value
      variable descr
      variable mode
      variable type
      variable isFits
      variable isOutput
      variable frame
      variable cframe 

      method GetParameter { par2Content frame frameWidth prevNameLen prevDescrLen }
      method CheckParameter {}
   }

   private {
      variable pseparator
      variable temp_value 
      variable callRoutine
      variable descrWidth

      method ParBrowser { wndw }
      method ParReset {}
      method ParOK {parobj flag}
   }
}

itcl::body FtoolParameter::constructor {args} {
   set name [lindex $args 0]
   set value [lindex $args 1]
   set type [lindex $args 2]
   set mode [lindex $args 3]
   if { $type == "b" } {
      set value [string tolower [lindex $args 1]]
   }
   set descr [lindex $args 4]
   set descrWidth [lindex $args 5]
   set callRoutine [lindex $args 6]
   set isFits 0
   set isOutput 0
   set pseparator ","
}

itcl::body FtoolParameter::destructor {} {
}


itcl::body FtoolParameter::GetParameter { par2Content frame frameWidth prevNameLen prevDescrLen } {
# variable is never used anywhere
  global g_entryFont
  global g_titleFont
  global tcl_platform

  set temp [string tolower $name]
  set cframe ${frame}.${temp}
  set fr $cframe
  if [winfo exists $fr] {
      return
  }
  frame $fr -relief groove 

  set nameLen [string length $name]
  set descrLen [string length $descr]

  if { $descrLen < $prevDescrLen } {
     set descrLen $prevDescrLen
  }
  if { $nameLen < $prevNameLen } {
     set nameLen $prevNameLen
  }

  label $fr.name -text $name -width 10 -anchor w
  label $fr.descr -text $descr -width $descrWidth -anchor w -font {Arial 10 bold}
  if {$type == "b" } { 
     text  $fr.value -width 25 -height 1.1 -relief flat
     radiobutton $fr.value.yes -variable [itcl::scope value] -value "yes" \
          -font g_titleFont -text "Yes"
     radiobutton $fr.value.no -variable [itcl::scope value] -value "no" \
          -font g_titleFont -text "No"
     $fr.value window create end -window $fr.value.yes
     $fr.value window create end -window $fr.value.no
  } else {
     entry $fr.value -width 25 -textvariable [itcl::scope value] -font g_titleFont 
  }

  pack $fr.name -side left -ipadx 1
  pack $fr.descr -side left -ipadx 1
  pack $fr.value -side left -ipadx 1 -expand true
#  pack $fr.isfits -side left -ipadx 1
#  pack $fr.isoutput -side left -ipadx 1

  if {$type == "s" || $type == "f" || $type == "fr" } { 
     set idx [lsearch -glob $par2Content "$name|*"]
     set ret ""
     if { $idx >= 0 } {
        set ret [lindex [split [lindex $par2Content $idx] "|"] 1]
     }
     if { $ret == "I" || $ret == "F" } {
        button $fr.browser -text browse -command [itcl::code $this ParBrowser $fr.value]
        bind $fr.browser <Enter> {
           %W configure -cursor hand2
        }
        pack $fr.browser -side left -padx 2
     }
  }

  update idletask
  set width [expr [string length $name] + [string length $descr] + 30]

  if [winfo exists $fr.browser] {
     set width [expr $width + 35]
  }

  set returnWidth $frameWidth

  if { $width > $frameWidth } {
     set returnWidth $width
     .pframe.params.list configure -width $width
     update idletask
  }

  return [list $returnWidth $nameLen $descrLen]
}

itcl::body FtoolParameter::CheckParameter {} {
   
  switch -exact -- $type {
        b { 
             if { $value != "yes" && $value !="no" } { 
                set str "Parameter $name has invalid value, $value.\n"
                set str "$str Only yes and no are allowed."
                tk_dialog .parerror "Parameter Error" \
                         $str error 0 Ok
                return 1
             }
          }
       i {
            if { [regexp -nocase {[a-z_,\.]} $value ] && \
                 $value != "INDEF" } {
                set str "Parameter $name has invalid value, $value.\n"
                set str "$str Only integer is allowed."
                tk_dialog .parerror "Parameter Error" \
                         $str error 0 Ok
                return 1
            } 
       }
       r {
            if { [regexp -nocase {[a-z_,]} $value ] && \
                 $value != "INDEF" } {
                set str "Parameter $name has invalid value, $value.\n"
                set str "$str Only numerical value is allowed."
                tk_dialog .parerror "Parameter Error" \
                         $str error 0 Ok
                return 1
            } 
       }
  } 
  return 0
}

itcl::body FtoolParameter::ParBrowser { wndw } {
   global g_titleFont
   if ![winfo exists .fD] {
      FitsFileselectionbox ::.fD $callRoutine -title "fgui: File Dialog"
   }
   .fD activate Open $wndw
}

itcl::body FtoolParameter::ParReset { } {
   $callRoutine ParClear
   $callRoutine ParSet
} 

itcl::body FtoolParameter::ParOK {parobj flag} {
   if { $flag == 1 } {
     set value $temp_value 
   }
   itcl::delete object $parobj 
} 

itcl::class FtoolInstance {
      
   constructor {args} {}
   destructor {}

   public {
      method MainMenu {}
      method notify { obj msg opts }
      method displayHelpMessage { topic_ { package "NONE" } { website "NONE" } } 
      method FtoolRunInteractive { cmd }
      method addNewXterminal { page entry } { set xTerminalList($page) $entry }
      method getTerminalId { page } { return $xTerminalList($page) }
      method setCurrentTab { page }
   }
   
   private {
      method MenuInit {}

      method PackageMenu { }
      method PackageGet { package }
      method constructToolList {}
      
      method FtoolGet {}
      method FtoolRun {}
      method FtoolCancel {}
      method FtoolTerminate {}
      method FtoolHelp {}
      
      method FavoriteSave {}
      method FavoriteGet {}
      method FavoriteAdd { { entry "none" } }
      method FavoriteDelete {}
      method FavoriteReset {}
      method FavoriteCheck {}
      
      method ConfigSet {}
      method ConfigOk {}
      
      method HelpGet {}
      
      method LogClear {}
      method LogError {}
      
      method ParExist {}  
      method ParClear {}  
      method ParSave {}  
      method ParSet {}  
      method ParDialog {}  
      method ParsRedraw {fpar}
      method ParsReset {fpar}
      method ParsOk {}
      method ParsCancel {}
      
      method CmdDialog {}  
      method CmdOk {}
      method CmdCancel {}
      method runTool {}
      method selectCmd { args }
      
      method DumpText {wtext chan termCmd}  
      method SearchDir {pathname filename}  
      method processToolList { package fd }

      method changeBrowser {}
      method updateBrowserSetting {}
      method changeHeadasRoot {}
      method updateHeadasRootSetting {}

      method paneManager { command args }

      method _expandDirectoryView { cDirWndw }
      method _changeDirectory { dir }
      method _resolveSymLinks { filename_ }

      method _expandDirectoryViewX { cDirWndw }
      method _changeDirectoryX { dir }
      method _addNewTab {}
      method _deleteTab {}
      method _modifyTabText {}

      variable origPath
      variable ftoolPath
      variable helpPath
      variable parPath
      variable par2Path
      variable autoSave
      variable autoClear
      
      variable currPackage
      variable currFtool
      variable currCmd 
      
      variable favorite
      variable imageFlag
      
      variable wlist 
      variable cmdNum 0
      variable logtext 
      variable logchan 
      
      variable currParFile 
      variable currParList
      variable parObjs
      
      variable fhelpname

      variable isParTool
      
      variable showHidden
      variable goodCommand
      variable outputFits
      
      variable curPos
      variable tmp_ftoolPath
      variable tmp_helpPath
      variable tmp_parPath
      variable tmp_autoSave
      variable tmp_autoClear
      variable tmp_currCmd
      variable tmp_outputFits
      variable runcmd ""
      variable currSelection ""
      variable prevSelection ""
      variable openEntryList {}
      variable prevOpenEntryList {}

      variable delimiter "#"
      variable frameWidth 80
      variable prevNameLen 0.0
      variable prevDescrLen 0.0

      variable startY 0
      variable endY 0
      variable toolHListTextRatio 1
      variable cWorkingDirectory [pwd]

      variable hammerIcon
      variable allTools {}
      variable toolHList
      variable headasPrefix "headas"
      variable availablePackage [list "Favorite" "asca" "caltools" "einstein" "exosat" "fimage" \
                                      "futils" "gro" "heao1" "heasarc" "oso" "rosat" \
                                      "time" "vela5b" "xronos" "xte" ]
      variable preDefineDocWebSite "http://heasarc.gsfc.nasa.gov/docs/software/ftools/headas/"
      variable docWebSite "http://heasarc.gsfc.nasa.gov/docs/software/ftools/headas/"
      variable htmlBrowser 

      variable xTerminal
      variable currentPage
      variable xTerminalList 
      variable toolServer
      variable totalXtermCnt 0
      variable xtermCnt -1
   }
}
      
      
itcl::body FtoolInstance::constructor {args} {
   global env tcl_platform
   global pWorkingDirectory

   catch { fguiPreferences ::fguiPref } err
   if { $tcl_platform(platform) == "windows" } {
      if { $fguiPref::FtoolsHeadasRoot == "None Defined"} {
         catch { changeHeadasRoot } err
         vwait fguiPref::FtoolsHeadasRoot
      } else {
         set ::env(LHEASOFT) $::fguiPref::FtoolsHeadasRoot
      }

      set idx [string first $headasPrefix $env(LHEASOFT)]
      set ::env(CYGWIN_HOME) [string range $env(LHEASOFT) 0 [expr $idx - 2]]
      set token [split [file normalize $env(LHEASOFT)] "/"]
      set ::env(CYGWIN_ROOT) [format "%s/%s" [lindex $token 0] [lindex $token 1]]
      set ::env(CYGWIN_BASH_HOME) [string range $env(CYGWIN_HOME) [string length $env(CYGWIN_ROOT)] end]
      set idx [string first $env(CYGWIN_BASH_HOME) $env(LHEASOFT)]
      set ::env(CYGWIN_BASH_LHEASOFT) [string range $env(LHEASOFT) $idx end]
      set ::env(ORIGINAL_PWD) [pwd]
      set pWorkingDirectory [pwd]

#puts "HOME: $env(CYGWIN_HOME)"
#puts "ROOT: $env(CYGWIN_ROOT)"
#puts "BASH_HOME: $env(CYGWIN_BASH_HOME)"
      set ::env(HOME) [file normalize $env(HOME)]
      set ::env(WIN_HOME) [file normalize $env(HOME)]
      set ::env(HEADAS) $env(LHEASOFT)
      set ::env(PATH) "$env(PATH);$env(LHEASOFT)/lib;$env(LHEASOFT)/bin"
      set ::env(LHEA_HELP) "$env(LHEASOFT)/help"
      set ::env(PFILES) "$env(LHEASOFT)/syspfiles"
      set ::env(P2FILES) "$env(LHEASOFT)/fguipfiles"
      set ::env(HEADASPROMPT) "stdout"
      set ::env(FTOOLSOUTPUT) "stdout"
      set ::env(PGPLOT_DIR) $env(HEADAS)/lib
      set ::env(PGPLOT_RGB) $env(HEADAS)/lib/rgb.txt
      set ::env(PGPLOT_FONT) $env(HEADAS)/lib/grfont.dat
      set ::env(LHEAPERL) /usr/bin/perl
      set ::env(PERLLIB) $env(CYGWIN_BASH_LHEASOFT)/lib/perl
      
   } else {
      set fguiPref::FtoolsHeadasRoot $env(LHEASOFT)
      fguiPref save 
   }

   set ftoolPath ""

   if { $tcl_platform(platform) != "windows"} {
      set htmlBrowser "netscape"
   } else {
      set htmlBrowser "start"
   }

   if [info exists env(PATH) ] {
       set origPath $env(PATH)
   } else {
       set origPath ""
   }

   if [info exists env(LHEA_HELP) ] {
       set helpPath $env(LHEA_HELP)
   } else {
       set helpPath ""
   }

   if [info exists env(PFILES) ] {
       set parPath $env(PFILES)
       set par2Path $env(P2FILES)
   } else {
       set parPath ""
       set par2Path ""
   }

   set autoSave 1
   set autoClear 0
   set currCmd ""
   set currFtool ""
   set currPackage ""
   set currParList ""
   set currParFile ""
   set parObjs ""
   set favorite ""
   set showHidden 0
   set goodCommand 0
   set outputFits ""
   set logchan ""
   set isParTool 1

   set tmp_ftoolPath ""
   set tmp_helpPath ""
   set tmp_parPath ""
   set tmp_autoSave ""
   set tmp_autoClear ""
   set tmp_currCmd ""
   set tmp_outputFits ""

}

itcl::body FtoolInstance::destructor {} {
} 

itcl::body FtoolInstance::MainMenu {} {

   global g_titleFont
   global g_entryFont

   if [winfo exist .ftoolframe ] { 
      focus .ftoolframe
      raise .ftoolframe
      return 
   }

   FavoriteAdd init
   package require Tix
   toplevel .ftoolframe -class Dialog
   wm title .ftoolframe "fgui: Run Ftool"


####################################
#                                  #
#  Set up the Ftools Menu Bar      #
#                                  #
####################################

   frame .ftoolframe.menu
   pack .ftoolframe.menu -fill x -pady 2 -pady 2
   bind .ftoolframe <Destroy> { exit }
   set fm .ftoolframe.menu

#  Ftool menu  button
   menubutton $fm.ftool -text "Action" -menu $fm.ftool.menu -font g_titleFont
   pack $fm.ftool -padx 2 -pady 2 -side left

   set m3 [menu $fm.ftool.menu -tearoff 1 ]
   $m3 add command -label "Run..." \
      -command [itcl::code $this FtoolRun] -font g_titleFont
   $m3 add command -label "Cancel..." \
      -command [itcl::code $this FtoolCancel] -font g_titleFont
   $m3 add command -label "Ftool Help" \
      -command [itcl::code $this FtoolHelp] -font g_titleFont
   $m3 add separator
   $m3 add command -label "Preference" -command "fguiPref edit"  -font g_titleFont
   $m3 add separator
   $m3 add command -label "Exit" \
      -command "[itcl::code $this FavoriteSave] ; destroy ."  -font g_titleFont

#  package menu button
   menubutton $fm.packages -text Packages -menu $fm.packages.menu -font g_titleFont
   #pack $fm.packages -padx 2 -pady 2 -side left
   set m1 [menu $fm.packages.menu -tearoff 1] 

   PackageMenu 

#  favorite menu button
   menubutton  $fm.favorite -text "Favorite" -menu $fm.favorite.menu -font g_titleFont
   pack $fm.favorite -padx 2 -pady 2 -side left

   set m2 [menu $fm.favorite.menu -tearoff 1 ]
   $m2 add command -label "Go to..." \
      -command [itcl::code $this FavoriteGet]  -font g_titleFont
   $m2 add command -label "Add..." \
      -command [itcl::code $this FavoriteAdd]  -font g_titleFont
   $m2 add command -label "Delete..." \
      -command [itcl::code $this FavoriteDelete]  -font g_titleFont
   $m2 add command -label "Reset..." \
      -command  [itcl::code $this FavoriteReset]  -font g_titleFont
   $m2 add separator
   $m2 add command -label "Configure" \
     -command [itcl::code $this ConfigSet]  -font g_titleFont

#  help menu button
   menubutton $fm.help -text "Help" -menu $fm.help.menu -font g_titleFont
   pack $fm.help -padx 2 -pady 2 -side left
   set m4 [menu $fm.help.menu -tearoff 1 ]
   $m4 add command -label "About this program" \
          -command [itcl::code $this displayHelpMessage fgui.html NONE local] -font g_titleFont
   $m4 add command -label "About selected ftool" \
          -command [itcl::code $this FtoolHelp] -font g_titleFont

####################################
#                                  #
#  Set up the Ftools List          #
#                                  #
#################################### 

   set newAvailablePackage { "Favorite" }
   foreach name $availablePackage {
      set name [string trim $name]
      if { $name == "" } continue
      if [file exists $helpPath/${name}.txt] {
         set f [open $helpPath/${name}.txt r]
         processToolList $name $f
         close $f
         lappend newAvailablePackage $name
      }
   }
   set availablePackage $newAvailablePackage

   frame .ftoolframe.tools -relief groove -bd 4  
   pack .ftoolframe.tools -fill x -padx 2 -padx 2

   set ft .ftoolframe.tools

   frame .ftoolframe.tools.info -relief flat -bd 4  
   label $ft.info.label -font g_titleFont -text "Working Directory:"
   entry $ft.info.entry -font g_titleFont -width 50 -bg white -selectbackground yellow
   button $ft.info.button -font g_titleFont -text "Browse"
   button $ft.info.hbutton -font g_titleFont -text "Home"
   pack $ft.info.label -side left -padx 2 -padx 2
   pack $ft.info.entry -side left -padx 2 -padx 2 -fill x -expand true
   pack $ft.info.button -side left -padx 2 -padx 4
   pack $ft.info.hbutton -side left -padx 2 -padx 4
   pack .ftoolframe.tools.info -side top -fill x

   bind $ft.info.button <Enter> {
      %W configure -cursor hand2
   }

   bind $ft.info.button <ButtonRelease-1> [itcl::code $this _expandDirectoryView .ftoolframe.tools.info.entry]

   bind $ft.info.hbutton <Enter> {
      %W configure -cursor hand2
   }

   bind $ft.info.entry <Return> {
      set dir [%W get]
      focus .ftoolframe.tools.info.entry
      .ftoolframe.tools.info.entry selection range 0 end
      if ![file exists $dir] {
         .ftoolframe.tools.info.entry configure -selectbackground red
         .ftoolframe.tools.info.entry configure -selectforeground white
         tk_messageBox -icon warning -type ok \
                       -message "$dir does not exists, operation might failed."
      } else {
         .ftoolframe.tools.info.entry configure -selectbackground yellow
         .ftoolframe.tools.info.entry configure -selectforeground black
         cd $dir
      }
   }

   bind $ft.info.hbutton <ButtonRelease-1> {
      global env

      .ftoolframe.tools.info.entry delete 0 end
      .ftoolframe.tools.info.entry insert end $env(HOME)
      cd $env(HOME)
   }

   $ft.info.entry insert end [pwd]
   catch { constructToolList } err


   frame $ft.f2  
   pack $ft.f2 -side top -fill x -pady 3 -padx 3
   button $ft.f2.fav -text "Add to Favorite" -width 15 -font g_titleFont
   button $ft.f2.run -text "Run" -command [itcl::code $this FtoolRun ] \
           -width 12 -font g_titleFont
   button $ft.f2.cancel -text "Cancel" -command [itcl::code $this FtoolCancel ] \
           -width 12 -font g_titleFont
   button $ft.f2.help -text "Ftool Help" -command [itcl::code $this FtoolHelp ] \
           -width 12 -font g_titleFont
   pack $ft.f2.fav -side left -padx 20 
   pack $ft.f2.run -side left -padx 20 
   pack $ft.f2.cancel -side left -padx 20
   pack $ft.f2.help -side left -padx 20
   
   frame $ft.grip -width 10 -height 10 -background blue -bd 1 -relief raised -cursor double_arrow
   place $ft.grip -anchor c -relx 0.5 -rely 1.0

   bind  $ft.grip <ButtonPress-1>   [itcl::code $this paneManager start %W]
   #bind  $ft.grip <B1-Motion>       [itcl::code $this paneManager motion %W %Y]
   bind  $ft.grip <ButtonRelease-1> [itcl::code $this paneManager stop %W]

#################################### 

####################################
#                                  #
#  Set up the Ftools Log           #
#                                  #
#################################### 

   frame .ftoolframe.log -relief groove -bd 4
   pack .ftoolframe.log -fill both -padx 2 -padx 2 -expand true

   frame .ftoolframe.log.tabcontrol -relief flat
   pack .ftoolframe.log.tabcontrol -fill x -padx 2 -padx 2

   button .ftoolframe.log.tabcontrol.add -text "New Tab" \
                                         -command [itcl::code $this _addNewTab ]
   pack .ftoolframe.log.tabcontrol.add -side left

   catch { xterminalTabs ::xtermTab .ftoolframe.log $this } err
   _addNewTab
   set fl .ftoolframe.log  
}

itcl::body FtoolInstance::_addNewTab {} {
   incr totalXtermCnt
   xtermTab addPage Tab_[incr xtermCnt]
}

itcl::body FtoolInstance::_deleteTab {} {
   incr totalXtermCnt -1
   xtermTab deletePage $currentPage
}

itcl::body FtoolInstance::_modifyTabText {} {
   xtermTab updateTitle $currentPage $currFtool
}

itcl::body FtoolInstance::_expandDirectoryView { cDirWndw } {
   package require Tix
   global pWorkingDirectory

   set pWorkingDirectory [.ftoolframe.tools.info.entry get]
   if [winfo exists .expand] {
      destroy .expand
   }

   toplevel .expand
   wm title .expand "choose working directory"
   wm geom .expand 300x200

   frame .expand.frame -relief sunken
   tixDirTree .expand.frame.expandDirectoryView -value [$cDirWndw get] \
                                          -command [itcl::code $this _changeDirectory] \
                                          -browsecmd [itcl::code $this _changeDirectory]
   pack .expand.frame.expandDirectoryView -fill both -expand true
   pack .expand.frame -fill both -expand true

   frame .expand.tool
   button .expand.tool.cancel -text "Cancel" \
                       -command { global pWorkingDirectory ; \
                                  .ftoolframe.tools.info.entry delete 0 end ; \
                                  .ftoolframe.tools.info.entry insert end $pWorkingDirectory ; \
                                  destroy .expand }
   button .expand.tool.done -text "Done" -command { destroy .expand }
   pack .expand.tool.cancel -padx 20 -side left
   pack .expand.tool.done -padx 20 -side right
   pack .expand.tool -fill x -expand true
   update idletask
}

itcl::body FtoolInstance::_changeDirectory { dir } {
   set cWorkingDirectory $dir   
   cd $cWorkingDirectory

   .ftoolframe.tools.info.entry delete 0 end
   .ftoolframe.tools.info.entry insert end $cWorkingDirectory
}

itcl::body FtoolInstance::constructToolList {} {

   option add *TixHList.selectBackground yellow
   option add *TixHList.selectForeground black

   set ft .ftoolframe.tools  

   if [winfo exists $ft.toolTree] {
      $ft.toolTree delete all
   } else {
      tixTree $ft.toolTree -browsecmd [itcl::code $this selectCmd] -options {
              hlist.separator #
              hlist.itemType imagetext
              hlist.drawBranch true
              hlist.font      g_titleFont
              hlist.indent    18
              hlist.height    12
              hlist.background white
      }
      pack $ft.toolTree -fill both -expand true
   }

   set toolHList [$ft.toolTree subwidget hlist]
   set catagory ""

   set imageFlag false
   set flagErr [catch {
       image create bitmap systemIcon -data {
           #define system_width 15
           #define system_height 15
           static unsigned char system_bits[] = {
              0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x3f,
              0x02, 0x20, 0x02, 0x20, 0xfe, 0x3f, 0xfe, 0x3f, 0x00, 0x00, 0x00, 0x00,
              0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
       }
   } err]

   if { !$flagErr } {
      set imageFlag true
      image create bitmap dropIcon -data {
           #define drop_width 16
           #define drop_height 16
           #define drop_x_hot 6
           #define drop_y_hot 4
           static unsigned char drop_bits[] = {
              0x00, 0x00, 0xfe, 0x07, 0x02, 0x04, 0x02, 0x04, 0x42, 0x04, 0xc2, 0x04,
              0xc2, 0x05, 0xc2, 0x07, 0xc2, 0x07, 0xc2, 0x0f, 0xfe, 0x1f, 0xc0, 0x07,
              0xc0, 0x06, 0x00, 0x0c, 0x00, 0x1c, 0x00, 0x08};
      } -foreground red
   }

   image create bitmap hammerIcon -data {
       #define hammer2_width 13
       #define hammer2_height 13
       static char hammer2_bits[] = {
          0x00,0x00,0xc0,0x03,0xf0,0x01,0xf8,0x01,0xf8,0x00,0x9e,0x00,0xbe,0x00,0x4c,
          0x01,0x80,0x02,0x00,0x0c,0x00,0x1a,0x00,0x14,0x00,0x08};
   }

   ####################################################
   # first add favorite tool item
   ####################################################

   ####################################################
   # add available tool item
   ####################################################

   set getFavorite true

   foreach targetList [list $favorite $allTools] {

      if { [llength $targetList] <= 0 && $getFavorite == "true" } {
         $toolHList add "Favorite" -image [tix getimage folder] -text Favorite
         continue
      }

      set targetList [lsort -increasing $targetList]
   
      set catagoryCnt 0
      foreach line $targetList {
           if { [string range $line 0 0] == "#" || [string trim $line] == "" } {
              # ignore comment out tool name
              continue
           }
   
           set toolstring $delimiter
           set toolroot [lindex $line 0]
           if [string match $toolroot "Featured"] {
              continue
           }
   
           if ![string match $toolroot $catagory] {
               incr catagoryCnt
               set catagory $toolroot
               if ![string match $catagory "None"] {
                  set toolstring [format "%s" $toolroot]
                  $toolHList add $toolstring -image [tix getimage folder] -text [file tail $toolstring]
               }
           }
   
           set toolsubpath [lindex $line 1]
           if { $toolsubpath != "none" } {
              regsub -all _ $toolsubpath " " toolsubpath
              set toolstring [format "%s%s%s" $toolroot $delimiter $toolsubpath]
              if ![$toolHList info exists $toolstring] {
                 $toolHList add $toolstring -image [tix getimage folder] -text [file tail $toolstring]
                 $toolHList hide entry $toolstring
              }
           } else {
              set toolstring $toolroot
           }
   
           set toolpath [lindex $line 2]
   
           set tooldesc [lindex $line 3]
           set idx [string first $tooldesc $line]
           set tooldesc [string range $line $idx end]
   
           if ![string match $toolroot "None"] {
              set toolentry [format "%s%s%s - %s" $toolstring $delimiter $toolpath $tooldesc]
              set toolstring [format "%s - %s" $toolpath $tooldesc]
           } else {
              set toolstring [format "%s - %s" $toolpath $tooldesc]
           }
   
           if { $imageFlag == "false" } {
              $toolHList add $toolentry -image [tix getimage system] -text $toolstring
           } else {
              $toolHList add $toolentry -image hammerIcon -text $toolstring
           }
   
           if ![string match $toolroot "None"] {
              $toolHList hide entry $toolentry
           }
         }
         if { $getFavorite == "true" } {
            set getFavorite "false"

            set childrenList [$toolHList info children "Favorite"]
            for {set i 0} {$i < [llength $childrenList]} {incr i} {
                $toolHList show entry [lindex $childrenList $i]
            }
         }

         update idletask
      }
      $ft.toolTree autosetmode
   
      bind $toolHList <Double-Button-1> [itcl::code $this runTool]
      update
      scan [winfo geometry $toolHList] "%dx%d+%d+%d" Rw Rh Rx Ry
      set textheight [$toolHList cget -height]
      set toolHListTextRatio [expr $Rh / $textheight]
}

itcl::body FtoolInstance::MenuInit {} {
   .ftoolframe.menu.favorite.menu entryconfigure 2 -state disabled
   .ftoolframe.menu.favorite.menu entryconfigure 3 -state disabled
   .ftoolframe.menu.favorite.menu entryconfigure 4 -state disabled

   .ftoolframe.menu.ftool.menu entryconfigure 1 -state disabled
   .ftoolframe.menu.ftool.menu entryconfigure 2 -state disabled
   .ftoolframe.menu.ftool.menu entryconfigure 3 -state disabled

   .ftoolframe.menu.help.menu entryconfigure 2 -state disabled

   .ftoolframe.tools.f2.fav  configure -state disable
   .ftoolframe.tools.f2.run  configure -state disable
    #bind .ftoolframe.tools.f1.list <Double-Button-1> {}  
   .ftoolframe.tools.f2.cancel  configure -state disable
   .ftoolframe.tools.f2.help  configure -state disable

   $logtext configure -state disabled   
}

itcl::body FtoolInstance::PackageMenu {} {
   global g_titleFont
   set fm .ftoolframe.menu
   set m1 .ftoolframe.menu.packages.menu

   $m1 delete 1 end

   set packagelist [list "asca" "caltools" "einstein" "exosat" "fimage" \
       "futils" "gro" "heao1" "heasarc" "oso" "rosat" "time" "vela5b" "xronos" \
         "xte" ]  
   set numPacks [llength $packagelist] 
   for { set i 0 } { $i < $numPacks } {incr i } { 
       set package [lindex $packagelist $i]
       set b [SearchDir $helpPath $package.txt]
       if {$b != ""}  { 
          $m1 add command -label $package \
              -font g_titleFont \
              -command [itcl::code $this PackageGet $package ]
       }
   }
}

itcl::body FtoolInstance::PackageGet {package } {
   set currPackage $package
   if [winfo  exists .ftoolframe.tools.info.label ] {
       .ftoolframe.tools.info.label configure -text $package
   }
   if [winfo  exists $wlist ] {
       $wlist delete 0 end
       set txtfile [ SearchDir  $helpPath $package.txt]
       set channel [open $txtfile r]
       set i 0
       while {[gets $channel line ] >= 0 } {
         incr i
         set line [string trim $line ]
         if {$line == ""} {
             continue
         }
         set line [string trim $line \#\*\+]
         set line [string trim $line ]
         if  [regexp ^\[P\] $line]  {
            break
         }
         if { [string first " - " $line] == -1 } {
            set line [string trim $line]
            set line "$temp $line" 
            $wlist delete end 
         }
         $wlist insert end $line
         set temp $line
       }
       close $channel
       #.ftoolframe.tools.f1.list selection set 0
       .ftoolframe.tools.f2.fav configure -command [itcl::code $this FavoriteAdd]
       .ftoolframe.tools.f2.fav configure -text "Add to Favorite"
       .ftoolframe.tools.f2.fav configure -state normal
       .ftoolframe.tools.f2.run configure -state normal
       # bind .ftoolframe.tools.f1.list <Double-Button-1> [itcl::code $this FtoolRun ] 
       .ftoolframe.tools.f2.cancel configure -state disabled
       .ftoolframe.tools.f2.help configure -state normal
       .ftoolframe.menu.ftool.menu entryconfigure 1 -state normal
       .ftoolframe.menu.ftool.menu entryconfigure 2 -state disabled
       .ftoolframe.menu.ftool.menu entryconfigure 3 -state normal
       .ftoolframe.menu.help.menu entryconfigure 2 -state normal
       if {$i > 0} {
          .ftoolframe.menu.favorite.menu entryconfigure 2 -state normal
       } else { 
          .ftoolframe.menu.favorite.menu entryconfigure 2 -state disabled
       }
   }
   .ftoolframe.menu.favorite.menu entryconfigure 3 -state disabled
   .ftoolframe.menu.favorite.menu entryconfigure 4 -state disabled

}

itcl::body FtoolInstance::FavoriteGet {} {
   global g_titleFont

   if [winfo exists .ftoolframe.tools.info.label ] {
      .ftoolframe.tools.info.label configure -text "My favorite" -font g_titleFont
   }
   #if [winfo exists $wlist ] {
   constructToolList 
   #.ftoolframe.tools.f2.fav configure -command [itcl::code $this FavoriteDelete]
   #.ftoolframe.tools.f2.fav configure -text "Delete "
   .ftoolframe.menu.favorite.menu entryconfigure 2 -state disabled
   .ftoolframe.menu.favorite.menu entryconfigure 3 -state normal
   #   $wlist delete 0 end 
   #   set num [FavoriteCheck]
   #   if {$num == 0} { 
   #      return 
   #   }
   #   for {set i 0} {$i < $num} {incr i} {
   #     $wlist insert end [lindex $favorite $i]
   #   }
   #   $wlist selection set 0
   #}
}

itcl::body FtoolInstance::FavoriteReset {} {
     set favorite ""
     #.ftoolframe.tools.f1.list  delete 0 end 
}

itcl::body FtoolInstance::ConfigSet {} {
  global g_titleFont

  set tmp_ftoolPath $ftoolPath
  set tmp_parPath $parPath
  set tmp_helpPath $helpPath
  set tmp_autoSave $autoSave
  set tmp_autoClear $autoClear

  toplevel .cframe   -class Dialog
  wm title .cframe "fgui: Ftool Configuration "
  
  frame .cframe.frm -relief groove -bd 2
  pack .cframe.frm -side top -ipadx 2 -ipadx 2

  frame .cframe.frm.ft
  set f1 .cframe.frm.ft
  label $f1.label -text "Ftool Path" -width 15 -anchor w -font g_titleFont
  entry $f1.entry -textvariable [itcl::scope tmp_ftoolPath ] -width 40 \
       -font g_titleFont
  pack $f1.label -side left -anchor w
  pack $f1.entry -side left -anchor w
         
  frame .cframe.frm.fp
  set f2 .cframe.frm.fp
  label $f2.label -text "Par File Path" -width 15 -anchor w -font g_titleFont
  entry $f2.entry -textvariable [itcl::scope tmp_parPath ] -width 40 \
       -font g_titleFont
  pack $f2.label -side left -anchor w
  pack $f2.entry -side left -anchor w

  frame .cframe.frm.fh
  set f3 .cframe.frm.fh
  label $f3.label -text "Help File Path" -width 15 -anchor w -font g_titleFont
  entry $f3.entry -textvariable [itcl::scope tmp_helpPath ] -width 40 \
       -font g_titleFont
  pack $f3.label -side left -anchor w
  pack $f3.entry -side left  -anchor w

  frame .cframe.frm.fo
  set f4 .cframe.frm.fo
  checkbutton $f4.save -variable [itcl::scope tmp_autoSave] \
       -onvalue 1 -offvalue 0 \
       -text  "Save the parameter file automatically" -font g_titleFont
  checkbutton $f4.clear -variable [itcl::scope tmp_autoClear] \
       -onvalue 1 -offvalue 0 \
       -text  "Clear the Log window  automatically" -font g_titleFont
  pack $f4.save -side top -anchor w
  pack $f4.clear -side top  -anchor w

  pack $f1 -side top -anchor w -fill x
  pack $f2 -side top -anchor w -fill x
  pack $f3 -side top -anchor w -fill x
  pack $f4 -side top -anchor w -fill x

  frame .cframe.cmd 
  pack .cframe.cmd -side top -padx 5 -pady 5 -fill x
  button .cframe.cmd.ok -text "OK" -command [itcl::code $this ConfigOk ] -font g_titleFont
  button .cframe.cmd.cancel -text "Cancel" -command {destroy .cframe} -font g_titleFont
  pack .cframe.cmd.ok -side left -padx 10 
  pack .cframe.cmd.cancel -side right  -padx 10
}

itcl::body FtoolInstance::ConfigOk {} {
      global env
      set ftoolPath [string trim $tmp_ftoolPath]
      set parPath [string trim $tmp_parPath]
      set helpPath [string trim $tmp_helpPath]
      set autoSave [string trim $tmp_autoSave ]
      set autoClear [string trim $tmp_autoClear ]


#      set env(FTOOLS) $ftoolPath
      if {$ftoolPath != "" } {
          set env(PATH) "$ftoolPath;$origPath"
      }

      set env(LHEA_HELP) $helpPath
      if {$helpPath == "" } {
          unset env(LHEA_HELP)
      }

      set env(PFILES)  $parPath  
      if {$parPath == "" } {
          unset env(PFILES)
      }

      PackageMenu
      MenuInit
      if [winfo  exists $wlist ] {
         $wlist delete 0 end 
      }

      ParClear
      set currFtool ""

      destroy .cframe
}

itcl::body FtoolInstance::HelpGet {} {
    hhelp ftool
}

itcl::body FtoolInstance::FtoolHelp {} {
   set fhelpname $runcmd
   displayHelpMessage $runcmd
}

itcl::body FtoolInstance::FtoolRun {} {
   global tcl_platform env
   global oldHOME oldPFILES

#  if it is the new ftool, initialize it.
   .ftoolframe.tools.f2.run configure -state disabled
   #bind .ftoolframe.tools.f1.list <Double-Button-1>  {} 
   .ftoolframe.tools.f2.cancel configure -state normal
   .ftoolframe.menu.favorite.menu entryconfigure 2 -state normal
 
   # set temp [ FtoolGet ]
   set temp $runcmd
   if {$currFtool != $temp } {
      ParClear
      set currFtool $temp
      set isParTool [ ParExist ]
      if {$isParTool == 1} {
          ParSet
      }
   } 

#  open the dialog 
   if {$isParTool == 1} {
      ParDialog 
      tkwait window .pframe  
   } else { 
      CmdDialog
      tkwait window .cmdframe  
   }

#  run the command
   if {$goodCommand == 0} {
      .ftoolframe.tools.f2.run configure -state normal
      #bind .ftoolframe.tools.f1.list <Double-Button-1> [itcl::code $this FtoolRun ]  
      .ftoolframe.tools.f2.cancel configure -state disabled
      .ftoolframe.menu.ftool.menu entryconfigure 1 -state normal
      .ftoolframe.menu.ftool.menu entryconfigure 2 -state disabled
      return
   } else {
      # set curPos [$logtext index end]
      if {$autoClear == 1} {
      #   LogClear
      }
   }

   if {$isParTool == 1} {
      if { $tcl_platform(platform) == "windows" } {
         #set currFtool ${currFtool}.exe
      }
      set currCmd $currFtool
      set npar [llength $currParList]
      if {$npar < 1} { 
        return
      }
      for {set i 0} {$i < $npar} {incr i} {
         set objname [lindex $parObjs $i]
         set parname [$objname cget -name]
         set parvalue [$objname cget -value]
         if {$parname == "page" } {
            set parvalue "no"
         }
         if {$parname == "confirm" } {
            set parvalue "no"
         }
         if {$parname == "proceed" } {
            set parvalue "yes"
         }
         if {$parvalue == "" } {
            set parvalue " "
         }
         if { $tcl_platform(platform) == "windows" } {
            # since we are going to work around the cygwin structure, the file
            # needs to be in full path
           # if ![file exists $parvalue] {
           #    if [file exists [pwd]/$parvalue] {
           #       set parvalue [pwd]/$parvalue
           #    }
           # }
         }
         set currCmd "$currCmd \"${parname}=${parvalue}\" "
      }
   }  

   set n  [string length $currCmd]
   set n1 $n
   set cmdstr ""
   set i 0
   while {$n1 > 0} {
      if { $n1 > 80 } { 
          set j [expr $i + 79]
      } else {
          set j $n  
      }
      set cmdstr "$cmdstr [string range $currCmd $i $j] \n"
      set i [expr $j + 1 ]
      set n1 [expr $n1 - 80]
   } 
     
   set toolServer [gToolServer $currFtool]
   set xTerminal $xTerminalList($currentPage)
   _modifyTabText
   $xTerminal changeToolServer $toolServer
   [gNotifications default] addObserver $this notify $toolServer "*"

   $xTerminal addToHistory "$currCmd"
   $xTerminal appendOutput "$currCmd" OutputChannel USER_INPUT
   $toolServer runTool $currCmd
}

itcl::body FtoolInstance::FtoolRunInteractive { cmd } {
   set tokenList [split $cmd " "]
   set currFtool [lindex $tokenList 0]
   set toolServer [gToolServer $currFtool]
   [gNotifications default] addObserver $this notify $toolServer "*"
   $toolServer runTool $cmd
   _modifyTabText
   return $toolServer
}

itcl::body FtoolInstance::notify { obj msg opts } {
   global toolHasFinishFlag

   set idx [string first " " $msg]
   set msgType [string range $msg 0 [expr $idx - 1]]
   set nextMessage [string range $msg [expr $idx + 1] end]
   set xTerminal $xTerminalList($currentPage)

   switch -- $msgType {
      "toolHasFinished" {
         set toolHasFinishFlag($toolServer) true
         FtoolTerminate
         $toolServer shutdownToolServer
         $xTerminal setToolName "cmd"
         $xTerminal appendOutput "cmd> " OutputChannel
         $xTerminal changeToolServer NONE
         return
      }
      "toolHasOutputAvailable" {
         catch { $xTerminal appendOutput $nextMessage OutputChannel } err
         set toolHasFinishFlag($toolServer) continue
         return
      }
      "toolHasErrorAvailable" {
         catch { $xTerminal appendOutput $nextMessage ErrorChannel } err
         set toolHasFinishFlag($toolServer) continue
         return
      }
   }
}

itcl::body FtoolInstance::FtoolTerminate {} {
   global g_fitsFileMode
   global oldHOME oldPFILES env tcl_platform
   
   if { $tcl_platform(platform) == "windows" } {
      if { [file exists $env(CYGWIN_HOME)/pfiles/${currFtool}.par] && \
           $env(CYGWIN_HOME) != "$env(WIN_HOME)" } {
         file copy -force $env(CYGWIN_HOME)/pfiles/${currFtool}.par $env(WIN_HOME)/pfiles/${currFtool}.par 
      }
   }
   set ::env(HOME) $oldHOME
   set ::env(PFILES) $oldPFILES
   set parPath $env(PFILES)

   # display the file header
   set num [llength $outputFits] 
   for {set i 0} {$i < $num} {incr i} {
        set ftooloutput [lindex $outputFits $i]
        set ftooloutput [string trim $ftooloutput]
        set ftooloutput [string trim $ftooloutput \!]
        set oldMode $g_fitsFileMode
        # Set Read-Only flag
        set g_fitsFileMode 1
        set tmp [openFitsFile $ftooloutput]
        $tmp changeFile
        set g_fitsFileMode $oldMode
   }

   #  save the par file
   if {$autoSave == 1 && $goodCommand == 1 && $isParTool == 1 } {
        ParSave
   }
  .ftoolframe.tools.f2.run configure -state normal
  .ftoolframe.tools.f2.cancel configure -state disabled
  .ftoolframe.menu.ftool.menu entryconfigure 1 -state normal
  .ftoolframe.menu.ftool.menu entryconfigure 2 -state disabled

  return
}

itcl::body FtoolInstance::FtoolCancel {} {
  if {$logchan != "" } {
     catch {close $logchan}
  }
  .ftoolframe.tools.f2.run configure -state normal
  #bind .ftoolframe.tools.f1.list <Double-Button-1> [itcl::code $this FtoolRun ]  
  .ftoolframe.tools.f2.cancel configure -state disabled
  .ftoolframe.menu.ftool.menu entryconfigure 1 -state normal
  .ftoolframe.menu.ftool.menu entryconfigure 2 -state disabled
}

itcl::body FtoolInstance::FavoriteSave {} {
  global env
  set f [open $env(HOME)/.fguiFavorite w+]
  foreach entry $favorite {
      set token [split $entry " "]
      set catagory [lindex $token 0]
      set toolName [lindex $token 2]
      set toolDesc [lindex $token 3]
      set idx [string first $toolDesc $entry]
      set toolDesc [string range $entry $idx end]
      set outputStr [format "%s%s%s - %s" $catagory $delimiter $toolName $toolDesc]
      puts $f $outputStr
  }
  close $f
}

itcl::body FtoolInstance::FavoriteAdd { { entry "none" } } {
   global env

   set action addGroup
   set entryList {}
   if { $entry == "none" } {
      lappend entryList [lindex [$toolHList info selection] 0]
      set action addOneItem
   } else {
      if [file exists $env(HOME)/.fguiFavorite] {
         set f [open $env(HOME)/.fguiFavorite r]
         set entryList [split [read $f [file size $env(HOME)/.fguiFavorite]] \n]
         close $f
      }
   }

   if { [llength $favorite] > 0 } {
      set childrenList [$toolHList info children "Favorite"]
      for {set i 0} {$i < [llength $childrenList]} {incr i} {
          $toolHList show entry [lindex $childrenList $i]
      }
   }

   foreach entry $entryList {
      set entry [string trim $entry]
      if { $entry == "" } continue
      set catagory [lindex [split $entry $delimiter] 0]
      set toolInfo [lindex [split $entry $delimiter] 1]
   
      set toolName [string trim [lindex [split $toolInfo "-"] 0]]
      set toolDesc [string trim [lindex [split $toolInfo "-"] 1]]
      set idx [string first $toolDesc $entry]
      set toolDesc [string range $entry $idx end]

      set outputStr [format "%s none %s %s" "Favorite" $toolName $toolDesc]

      set idx [lsearch -exact $favorite $outputStr]

      if { $idx < 0 } {
         lappend favorite $outputStr
         if { $action == "addOneItem" } {
            set addEntry [format "Favorite%s%s" $delimiter $toolInfo]
            if { $imageFlag == "false" } {
               $toolHList add $addEntry -image [tix getimage system] -text $toolInfo
            } else {
               $toolHList add $addEntry -image hammerIcon -text $toolInfo
            }
         }
      }
   }

   if [winfo exists .ftoolframe.tools.toolTree] {
      .ftoolframe.tools.toolTree autosetmode
   }

   FavoriteSave
   return $favorite
}

itcl::body FtoolInstance::FavoriteDelete {} {
   set select [lindex [$toolHList info selection] 0]
   set selectionList [$toolHList info children $select]

   if { [llength $selectionList] <= 0 } {
      # child
      lappend selectionList $select
   }

   foreach selection $selectionList {
      set catagory [string trim [lindex [split $selection $delimiter] 0]]
      set toolName [string trim [lindex [split [lindex [split $selection $delimiter] 1] "-"] 0]]
      set parent [$toolHList info parent $selection]
      $toolHList delete entry $selection

      set idx [lsearch -glob $favorite "$catagory none $toolName *"]

      set favorite [lreplace $favorite $idx $idx]
      # check to see if parent has no more child
      set children [$toolHList info children $parent]
   }

   if [winfo exists .ftoolframe.tools.toolTree] {
      .ftoolframe.tools.toolTree autosetmode
   }

   FavoriteSave
}


itcl::body FtoolInstance::FtoolGet {} {
   set i [$wlist curselection]
   set temp [$wlist get $i]
   set temp [split $temp]
   return [lindex $temp 0]
}

itcl::body FtoolInstance::FavoriteCheck {} {
   set num [llength  $favorite] 
   if {$num == 0 } { 
       .ftoolframe.tools.f2.fav  configure -state disable
       .ftoolframe.tools.f2.run  configure -state disable
       #bind .ftoolframe.tools.f1.list <Double-Button-1>  {} 
       .ftoolframe.tools.f2.cancel  configure -state disable
       .ftoolframe.tools.f2.help  configure -state disable
       .ftoolframe.menu.ftool.menu entryconfigure 1 -state disabled
       .ftoolframe.menu.ftool.menu entryconfigure 2 -state disabled
       .ftoolframe.menu.ftool.menu entryconfigure 3 -state disabled
       .ftoolframe.menu.favorite.menu entryconfigure 2 -state disabled
       .ftoolframe.menu.favorite.menu entryconfigure 3 -state disabled
       .ftoolframe.menu.favorite.menu entryconfigure 4 -state disabled
       .ftoolframe.menu.help.menu entryconfigure 2 -state disabled
   } else {   
       .ftoolframe.tools.f2.fav configure -state normal
       .ftoolframe.tools.f2.run configure -state normal
       #bind .ftoolframe.tools.f1.list <Double-Button-1> [itcl::code $this FtoolRun ]  
       .ftoolframe.tools.f2.cancel configure -state disabled
       .ftoolframe.tools.f2.help configure -state normal
       .ftoolframe.menu.ftool.menu entryconfigure 1 -state normal
       .ftoolframe.menu.ftool.menu entryconfigure 2 -state disabled
       .ftoolframe.menu.ftool.menu entryconfigure 3 -state normal
       .ftoolframe.menu.favorite.menu entryconfigure 2 -state disabled
       .ftoolframe.menu.favorite.menu entryconfigure 3 -state normal
       .ftoolframe.menu.favorite.menu entryconfigure 4 -state normal
       .ftoolframe.menu.help.menu entryconfigure 2 -state normal
   }
  return $num
}

itcl::body FtoolInstance::LogClear {} { 
   $logtext configure -state normal 
   $logtext delete 1.0 end
   $logtext configure -state disabled
   eval { $logtext tag delete } [$logtext tag names]
   set cmdNum 0
} 

itcl::body FtoolInstance::LogError {} { 
   $logtext configure -state normal
   $logtext insert end \
  "$currFtool needs runtime input and can not be handled by Fv. \n"
   $logtext configure -state disabled 
   FtoolCancel
}  
   
itcl::body FtoolInstance::ParExist {} {
  global env tcl_platform

  set homeDir $env(HOME)
  set pfile "${currFtool}.par"

  set b [split $parPath \;]
  set localPFileDir $env(HOME)/pfiles
  if ![file exists $localPFileDir] {
     file mkdir $localPFileDir
  }

  if { $tcl_platform(platform) == "windows" } {
     set currParFile [file join $localPFileDir $pfile]
     if ![ file exists $currParFile ] {
        set currParFile [file join $env(PFILES) $pfile]

        if { [file type $currParFile] == "link" } {
           file copy -force [_resolveSymLinks $currParFile] $localPFileDir
        } else {
           file copy -force $currParFile $localPFileDir
        }
        set currParFile [file join $localPFileDir [file tail $currParFile]]
        set ::env(PFILES) $localPFileDir
     }
     return 1
  } else {
     for { set i 0 } { $i < [llength $b] } { incr i } {
         set dir0 [lindex $b $i]
         set currParFile [file join $dir0 $pfile]

         if [ file exists $currParFile ] {
            if { $dir0 != $localPFileDir } {
               if { [file type $currParFile] == "link" } {
                  file copy -force [_resolveSymLinks $currParFile] $localPFileDir
               } else {
                  file copy -force $currParFile $localPFileDir
               }
               set currParFile [file join $localPFileDir [file tail $currParFile]]
            }
            return 1
         }
      }
  }  

  set tmpfile [SearchDir $parPath $pfile]

  if { $tmpfile != "" && [file type $tmpfile] == "link" } {
     set tmpfile [format "%s/%s" [file dirname $tmpfile] [file readlink $tmpfile]]
  }

  if {$tmpfile != "" } {
      file copy -force $tmpfile $currParFile
  }

  if ![ file exists $currParFile ] {
     return 0
  } 
  return 1
}
  
itcl::body FtoolInstance::ParSet {} { 

  set parObjs ""
  set currParList ""

  set pchannel [open $currParFile r]
  set resultList {}
  set descrWidth 0
  while { [gets $pchannel line] >=0 } {
      set line [string trim $line] 
      if [regexp  ^\# $line ] { 
        continue
      }
      set par [split $line \,]
      set parname [string trim [lindex $par 0] ]
      if {$parname ==""} {
          continue
      }
      set entry {}
      lappend entry $parname
      set parvalue  [string trim [lindex $par 3] ]
      lappend entry [string trim $parvalue \"] 
      set pardescr  [string trim [lindex $par 6] ]
      set pardescr  [string trim $pardescr \"] 
      if { [string length $pardescr] > $descrWidth } {
         set descrWidth [string length $pardescr]
      }
      lappend entry $pardescr
      lappend entry [string trim [lindex $par 1] ]
      lappend entry [string trim [lindex $par 2] ]
      lappend resultList $entry
  }

  for { set i 0 } {$i < [llength $resultList]} { incr i } {
      set entry [lindex $resultList $i]
      if { $entry == {} } continue
      set parname  [lindex $entry 0]
      set parvalue [lindex $entry 1]
      set pardescr [lindex $entry 2]
      set partype  [lindex $entry 3]
      set parmode  [lindex $entry 4]

      lappend currParList $parname
  
      set objname par_${parname}
      FtoolParameter $objname $parname $parvalue $partype $parmode \
          $pardescr $descrWidth $this
      lappend parObjs $objname
  }
  close $pchannel
}

itcl::body FtoolInstance::ParSave {} { 
  set buffer ""
  set pchannel [open  $currParFile r+]
  while { [gets $pchannel line] >=0 } {
     lappend buffer $line
  }
  close $pchannel

  set cauto a
  if ![string match l [par_mode cget -value] ] {
     set cauto x
  }
  set pchannel [open "partemp.par" w]
  set npar [llength $currParList] 
  for {set i 0} {$i < $npar} {incr i} { 
     set objname [lindex $parObjs $i]
     set line [lindex $buffer $i]
     set temp [split $line \,]
     set parvalue [lindex $temp 3]
     set parmode [lindex $temp 2]
     set partype [lindex $temp 1]
     set currvalue [$objname cget -value]
     set currvalue [string trim $currvalue]
     set parvalue [string trim $parvalue]

     if  ![ string match {[l$cauto]} $parmode ]  {
        puts $pchannel $line
        continue
     }
     if { $partype == "s" } { 
        set currvalue "\"$currvalue\""
     }  
     if {$parvalue != $currvalue } { 
        set temp [lreplace $temp 3 3 $currvalue]
        set line [join $temp \,]
     }
     puts $pchannel $line
  }
  close $pchannel
  file copy  -force "partemp.par" $currParFile
  file delete  "partemp.par" 
}
    
itcl::body FtoolInstance::ParClear {} { 

  set npar [llength $currParList]
  if {$npar == 0 } { 
     return 
  }
  for {set i 0} {$i < $npar} {incr i} {
     set objname [lindex $parObjs $i]
     itcl::delete object $objname 
  }
  set parObjs ""
  set currParList ""
  set outputFits ""
}

itcl::body FtoolInstance::ParDialog {} { 
  set pfile "${currFtool}.par"

  if [winfo exists .pframe] {
     destroy .pframe
  }

  if [winfo exists .cmdframe] {
     destroy .cmdframe
  }

  toplevel .pframe -class Dialog
  wm title .pframe "fgui: $pfile"

  frame .pframe.title
  set title .pframe.title
  label $title.name -text "Name" -width 10 -anchor w
  label $title.descr -text "Description" -width 33 -anchor w
  label $title.value -text "Value" -width 20 -anchor w
#  label $title.isfits -text "Fits?" -width 5 -anchor w
#  label $title.isoutput -text "Output?" -width 7 -anchor w
  label $title.browser -text "" -width 6 -anchor w
  pack  $title.name -side left -ipadx 1
  pack  $title.descr -side left -ipadx 1 
  pack  $title.value -side left -ipadx 1
#  pack  $title.isfits -side left -ipadx 1
#  pack  $title.isoutput -side left -ipadx 1
  pack  $title.browser -side left -ipadx 1
  pack  $title -fill x 

  frame .pframe.params
  text .pframe.params.list -yscrollcommand {.pframe.params.scrolly set } \
                    -wrap none -width 80
  scrollbar .pframe.params.scrolly -orient vertical \
                -command {.pframe.params.list yview}
  pack .pframe.params.scrolly -side right -fill both
  pack .pframe.params.list  -side left -fill both -expand true
  pack .pframe.params  -side top -fill both -expand true

  set fpar .pframe.params.list
  ParsRedraw $fpar

  frame .pframe.commands
  set pfm .pframe.commands
  checkbutton $pfm.hidden -variable [itcl::scope showHidden] -onvalue 1 -offvalue 0 \
            -text "Show hidden" -command [itcl::code $this ParsRedraw $fpar]
  button $pfm.reset -text "Reset to System Default" -width 20 \
       -command  [itcl::code $this ParsReset $fpar ]
  button $pfm.help -text Help -width 6 -command  [itcl::code $this FtoolHelp]
  button $pfm.cancel -text Cancel -width 6 -command  [itcl::code $this ParsCancel]
  button $pfm.ok -text OK -width 6 -command  [itcl::code $this ParsOk]

  pack $pfm.hidden -side left -padx 10
  pack $pfm.reset -side left -padx 10
  pack $pfm.help -side right -padx 10 
  pack $pfm.cancel -side right -padx 10 
  pack $pfm.ok -side right  -padx 10
  pack $pfm -side top -fill x -pady 5
}

itcl::body FtoolInstance::ParsReset {fpar} { 
  global env
# force to recopy the system one. 
   
  if { [string first "syspfiles" $currParFile] < 0 } {
     file delete $currParFile  
  }
  
  ParClear
  if [ ParExist ] {
      ParSet 
  }

  ParsRedraw $fpar

}


itcl::body FtoolInstance::ParsRedraw {fpar } { 
  set npar [llength $currParList]
  if {$npar < 1} { 
     return
  }
  $fpar delete 1.0 end 
  set nshow 0 
  set f [open ${par2Path}/${runcmd}.par2 r]
  set par2Content [split [read $f [file size ${par2Path}/${runcmd}.par2]] \n]
  close $f
  if {$showHidden == 1} {
     for {set i 0} {$i < $npar} {incr i} {
        set objname [lindex $parObjs $i]
        set retList [$objname GetParameter $par2Content $fpar $frameWidth $prevNameLen $prevDescrLen]
        set frameWidth [lindex $retList 0]
        set prevNameLen [lindex $retList 1]
        set prevDescrLen [lindex $retList 2]
        set cf [$objname cget -cframe]
        $fpar window create end -window $cf
        $fpar insert end "\n" 
        incr nshow
     }
     set cauto a
     if ![string match q [par_mode cget -value] ] {
        set cauto x
     }
     for {set i 0} {$i < $npar} {incr i} {
        set objname [lindex $parObjs $i]
        set parmode [$objname cget -mode]
        if   [ string match {[q$cauto]} $parmode ]  {
             set cf [ $objname cget -cframe ]
             ${cf}.name configure -bg green
        }
     }
  } else {
     set cauto x
     if [string match h [par_mode cget -value] ] {
        set cauto a
     }
     for {set i 0} {$i < $npar} {incr i} {
         set objname [lindex $parObjs $i]
         set parmode [$objname cget -mode]
         if {$parmode == "h" || $parmode == $cauto} {
             continue
         }
         set retList [$objname GetParameter $par2Content $fpar $frameWidth $prevNameLen $prevDescrLen]
         set frameWidth [lindex $retList 0]
         set prevNameLen [lindex $retList 1]
         set prevDescrLen [lindex $retList 2]
         set cf [$objname cget -cframe]
         ${cf}.name configure -bg green
         $fpar window create end -window $cf
         $fpar insert end "\n" 
         incr nshow
     } 
  } 
  set nshow [expr $nshow*2]
  if {$nshow > 24 } {
        set nshow 24 
  }
  $fpar configure -height $nshow
}

itcl::body FtoolInstance::ParsCancel {} { 
     set goodCommand 0 
     ParClear
     ParSet
     destroy .pframe 
}

itcl::body FtoolInstance::ParsOk {} { 
  set npar [llength $currParList]
  if {$npar < 1} { 
     set goodCommand 0 
     destroy .pframe
     return
  }

# validify the parameter value 
  for {set i 0} {$i < $npar} {incr i} {
     set objname [lindex $parObjs $i]
     set status [$objname CheckParameter]
     if {$status == 1} {
         set goodCommand 0 
         focus .pframe 
         raise .pframe 
         return
     }
  }

# check the output fits 
  set outputFits ""
  for {set i 0} {$i < $npar} {incr i} {
     set objname [lindex $parObjs $i]
     set isoutput [$objname cget -isOutput]
     set isfits [$objname cget -isFits]
     if {$isoutput == 1 && $isfits == 1} {
        lappend outputFits [$objname cget -value] 
     } 
  }
  set goodCommand 1 
  destroy .pframe 
}

itcl::body FtoolInstance::CmdDialog {} { 
  global g_titleFont

  if [winfo exists .pframe] {
     destroy .pframe
  }
  if [winfo exists .cmdframe] {
     destroy .cmdframe
  }

  toplevel .cmdframe -class Dialog
  wm title .cmdframe "fgui: $currFtool"

  set tmp_currCmd $currFtool
  frame .cmdframe.cmd -relief groove
  set fc .cmdframe.cmd  
  label $fc.labelc -text "Command"  -anchor w
  entry $fc.entryc -textvariable [itcl::scope tmp_currCmd] -width 40  \
    -font g_titleFont
  label $fc.labelo -text "Output Fits File" -anchor w
  entry $fc.entryo -textvariable [itcl::scope tmp_outputFits] -width 40 \
    -font g_titleFont

  pack $fc.labelc -side top -anchor w -padx 2 -pady 2
  pack $fc.entryc -side top -anchor w -padx 2 -pady 2
  pack $fc.labelo -side top -anchor w -padx 2 -pady 2
  pack $fc.entryo -side top -anchor w -padx 2 -pady 2
  pack $fc -fill x

  frame .cmdframe.buttons
  set  fb .cmdframe.buttons
  button $fb.help -text Help -width 6 -command  [itcl::code $this FtoolHelp]
  button $fb.cancel -text Cancel -width 6 -command  [itcl::code $this CmdCancel]
  button $fb.ok -text OK -width 6 -command  [itcl::code $this CmdOk]

  pack $fb.help -side right -padx 10 
  pack $fb.cancel -side right -padx 10 
  pack $fb.ok -side right  -padx 10
  pack $fb -side top -fill x -pady 5
} 

itcl::body FtoolInstance::CmdCancel {} {
  set goodCommand 0
  destroy .cmdframe
} 

itcl::body FtoolInstance::CmdOk {} {
  set currCmd $tmp_currCmd
  set outputFits $tmp_outputFits
  set goodCommand 1
  destroy .cmdframe
} 

itcl::body FtoolInstance::DumpText {wtext chan args} { 
   if [eof $chan] {
      catch {close $chan}
      if { [llength $args ] > 0 } {
         set termcmd [lindex $args 0]
         $termcmd
      }
      return
   }
   gets $chan line
   $wtext configure -state normal
   $wtext insert end "$line \n"
   $wtext configure -state disabled
}

itcl::body FtoolInstance::SearchDir {pathname  filename} { 
   set b [split $pathname \;\:]
   for {set i 0} {$i < [llength $b] } {incr i} {
       set dirf [lindex $b $i]
       set fullfile [file join $dirf $filename]
       if [file exists $fullfile]  {
          return $fullfile
       }
   } 
   return ""
} 

itcl::body FtoolInstance::processToolList { package fd } {
   global env tcl_platform

   set temp ""
   set reminderStrFound false
   while {[gets $fd line ] >= 0 } {
      set line [string trim $line ]
      if {$line == ""} {
          continue
      }
      set line [string trim $line \#\*\+]
      set line [string trim $line ]
      if { [regexp {^\[P\]} $line] == 1 } {
         break
      }

      if { [string first " - " $line] == -1 } {
         set line [string trim $line]
         if { $temp != "" } {
            set reminderStrFound true
            set line "$temp $line"
         }
         # set allTools [lreplace $allTools end end]
      } else {
         set reminderStrFound false
         set temp ""
         regsub " - " $line " " line 
      }

      set tool [file join $env(HEADAS)/bin [lindex $line 0]]
      if { $tcl_platform(platform) == "windows" } {
         set tool ${tool}.exe
      }
      if { $reminderStrFound == "false" } {
         set line [format "%s none %s" $package $line]
      }
      set temp $line

      if [file exists $tool] {
         lappend allTools $line
      }
 
   }
}

itcl::body FtoolInstance::runTool {} {

   set selection [$toolHList info selection]
   if { [lsearch -exact $availablePackage $selection] < 0 } {
      set runcmd [string trim [lindex [split [lindex [split $selection "-"] 0] "#"] 1]]
   } else {
      return
   }
   .ftoolframe.tools.f2.run  configure -state normal
   .ftoolframe.menu.favorite.menu entryconfigure 3 -state normal
   .ftoolframe.tools.f2.fav configure -command [itcl::code $this FavoriteAdd]
   .ftoolframe.tools.f2.fav configure -text "Add to Favorite"
   .ftoolframe.tools.f2.fav configure -state normal
   .ftoolframe.menu.favorite.menu entryconfigure 2 -state normal
   .ftoolframe.menu.favorite.menu entryconfigure 3 -state disable
   .ftoolframe.tools.f2.run configure -state normal
   .ftoolframe.tools.f2.cancel configure -state disabled
   .ftoolframe.tools.f2.help configure -state normal
   .ftoolframe.menu.ftool.menu entryconfigure 1 -state normal
   .ftoolframe.menu.ftool.menu entryconfigure 2 -state disabled
   .ftoolframe.menu.ftool.menu entryconfigure 3 -state normal
   .ftoolframe.menu.help.menu entryconfigure 2 -state normal
   FtoolRun
}

itcl::body FtoolInstance::selectCmd { args } {
   set selection [lindex [$toolHList info selection] 0]
   set currSelection $selection
   set favoriteList false

   set children [$toolHList info children $selection] 

   if { [lsearch -exact $availablePackage $selection] < 0 } {
      set runcmd [string trim [lindex [split [lindex [split $selection "-"] 0] "#"] 1]]
     .ftoolframe.tools.f2.run  configure -state normal
     .ftoolframe.menu.favorite.menu entryconfigure 3 -state normal

      set catagory [string trim [lindex [split [lindex [split $selection "-"] 0] "#"] 0]]
      if { $catagory == "Favorite" } {
        .ftoolframe.tools.f2.fav configure -command [itcl::code $this FavoriteDelete]
        .ftoolframe.tools.f2.fav configure -text "Delete from Favorite" -width 20
        .ftoolframe.menu.favorite.menu entryconfigure 3 -state normal
        .ftoolframe.menu.favorite.menu entryconfigure 2 -state disable
      } else {
        .ftoolframe.tools.f2.fav configure -command [itcl::code $this FavoriteAdd]
        .ftoolframe.tools.f2.fav configure -text "Add to Favorite"
        .ftoolframe.menu.favorite.menu entryconfigure 2 -state normal
        .ftoolframe.menu.favorite.menu entryconfigure 3 -state disable
      }
      .ftoolframe.tools.f2.fav configure -state normal
      .ftoolframe.tools.f2.run configure -state normal
      .ftoolframe.tools.f2.cancel configure -state disabled
      .ftoolframe.tools.f2.help configure -state normal
      .ftoolframe.menu.ftool.menu entryconfigure 1 -state normal
      .ftoolframe.menu.ftool.menu entryconfigure 2 -state disabled
      .ftoolframe.menu.ftool.menu entryconfigure 3 -state normal
      .ftoolframe.menu.help.menu entryconfigure 2 -state normal
   } else {
      return
   }
}

itcl::body FtoolInstance::changeBrowser {} {
     global g_titleFont

     set top .changeBrowserWndw
     toplevel .changeBrowserWndw
     wm geometry $top +[winfo pointerx .]+[winfo pointery .]
     wm title .changeBrowserWndw "Choose Your Browser Preference"

     label $top.label1 -text "FGui uses your standard Web browser to display help text and other" \
                       -font [list Helvetica 12 ]
     label $top.label2 -text "information.  Please enter the command (or full path) used to start" \
                       -font [list Helvetica 12 ]
     label $top.label3 -text "your browser (e.g., netscape, firefox, safari, opera, ..) here: \n" \
                       -font [list Helvetica 12 ]

     entry $top.entry -text "" -width 20 -background white -font g_titleFont
     label $top.label4 -text "\nYou may modify your browse preference in \"Preference\" at any time" \
                       -font [list Helvetica 12 ]
     label $top.label5 -text "under the \"Action\" button at the upper left of the main Hera window.\n" \
                       -font [list Helvetica 12 ]

     grid $top.label1 -row 0 -column 0 -columnspan 10 -sticky nws
     grid $top.label2 -row 1 -column 0 -columnspan 10 -sticky nws
     grid $top.label3 -row 2 -column 0 -columnspan 10  -sticky nws
     grid $top.entry  -row 3 -column 3 -columnspan 6  -sticky nws
     grid $top.label4 -row 4 -column 0 -columnspan 10 -sticky nws
     grid $top.label5 -row 5 -column 0 -columnspan 10 -sticky nws

     frame $top.actionFrame
     set actionFrame $top.actionFrame
     button $actionFrame.ok -text "Go" -command [itcl::code $this updateBrowserSetting]

     button $actionFrame.cancel -text "Cancel" -command [itcl::code $this updateBrowserSetting]
     grid $actionFrame.ok -row 0 -column 4
     grid $actionFrame.cancel -row 0 -column 5
     grid $actionFrame -row 6 -column 0 -columnspan 10 -sticky news

     focus $top.entry
     update idletask

     bind $top.entry <Return> [itcl::code $this updateBrowserSetting]
}

itcl::body FtoolInstance::changeHeadasRoot {} {
     global g_titleFont

     set top .changeHeadasRootWndw
     toplevel .changeHeadasRootWndw
     wm geometry $top +[winfo pointerx .]+[winfo pointery .]
     wm title .changeHeadasRootWndw "Set Headas Root Directory "

     label $top.label1 -text "FGui will need the root directory of installed Ftools package" \
                       -font [list Helvetica 12 ]
     label $top.label2 -text "to accurately run ftools command.  Please enter the full path" \
                       -font [list Helvetica 12 ]
     label $top.label3 -text "or use \"browse\" button select the directory name." \
                       -font [list Helvetica 12 ]
     entry $top.entry -text "" -width 50 -background white -font g_titleFont
     button $top.browse -text "browse" -font g_titleFont 
     label $top.label4 -text "\nYou may modify the directory path in \"Preference\" at any time " \
                       -font [list Helvetica 12 ]
     label $top.label5 -text "under the \"Action\" button at the upper left of the main FGui window.\n" \
                       -font [list Helvetica 12 ]

     grid $top.label1 -row 0 -column 0 -columnspan 10 -sticky nws
     grid $top.label2 -row 1 -column 0 -columnspan 10 -sticky nws
     grid $top.label3 -row 2 -column 0 -columnspan 10  -sticky nws
     grid $top.entry  -row 3 -column 3 -columnspan 5  -sticky nws
     grid $top.browse -row 3 -column 9 -sticky nws
     grid $top.label4 -row 4 -column 0 -columnspan 10 -sticky nws
     grid $top.label5 -row 5 -column 0 -columnspan 10 -sticky nws

     frame $top.actionFrame
     set actionFrame $top.actionFrame
     button $actionFrame.ok -text "Go" -command [itcl::code $this updateHeadasRootSetting]

     button $actionFrame.cancel -text "Cancel" -command [itcl::code $this updateHeadasRootSetting]
     grid $actionFrame.ok -row 0 -column 4
     grid $actionFrame.cancel -row 0 -column 5
     grid $actionFrame -row 6 -column 0 -columnspan 10 -sticky news

     bind $top.entry <Return> [itcl::code $this updateHeadasRootSetting]

     bind $top.browse <Enter> {
        %W configure -cursor hand2
     }

     bind $top.browse <ButtonRelease-1> [itcl::code $this _expandDirectoryViewX .changeHeadasRootWndw.entry]

     bind $top.entry <Return> {
        set dir [%W get]
        focus .changeHeadasRootWndw.entry
        .changeHeadasRootWndw.entry selection range 0 end
        if ![file exists $dir] {
           .changeHeadasRootWndw.entry configure -selectbackground red
           .changeHeadasRootWndw.entry configure -selectforeground white
           tk_messageBox -icon warning -type ok \
                         -message "$dir does not exists, operation might failed."
        } else {
           .changeHeadasRootWndw.entry configure -selectbackground yellow
           .changeHeadasRootWndw.entry configure -selectforeground black
        }
        fguiPref save
     }

}

itcl::body FtoolInstance::updateHeadasRootSetting {} {

     set setting [string trim [.changeHeadasRootWndw.entry get]]
     set previousChoice $::fguiPref::FtoolsHeadasRoot

     if { $setting == "" } {
        set ::fguiPref::FtoolsHeadasRoot $previousChoice
     } else {
        set ::fguiPref::FtoolsHeadasRoot $setting
        fguiPref save
     }

     destroy .changeHeadasRootWndw
}

itcl::body FtoolInstance::_expandDirectoryViewX { cDirWndw } {
   package require Tix
   global pDirectory

   set pDirectory [pwd]
   if [winfo exists .expandDVx] {
      destroy .expandDVx
   }

   toplevel .expandDVx
   wm title .expandDVx "choose Headas Root directory"
   wm geom .expandDVx 300x200

   frame .expandDVx.frame -relief sunken
   tixDirTree .expandDVx.frame.expandDirectoryView -value [pwd] \
                                          -command [itcl::code $this _changeDirectoryX] \
                                          -browsecmd [itcl::code $this _changeDirectoryX]
   pack .expandDVx.frame.expandDirectoryView -fill both -expand true
   pack .expandDVx.frame -fill both -expand true

   frame .expandDVx.tool
   button .expandDVx.tool.done -text "Done" \
                               -command { global pDirectory env ; \
                                          .changeHeadasRootWndw.entry delete 0 end ; \
                                          .changeHeadasRootWndw.entry insert end $pDirectory; \
                                          fguiPref save ; \
                                          set ::env(LHEASOFT) $pDirectory ; \
                                          destroy .expandDVx }
   pack .expandDVx.tool.done -padx 20 -side right
   pack .expandDVx.tool -fill x -expand true
   update idletask
}

itcl::body FtoolInstance::_changeDirectoryX { dir } {
   global pDirectory env

   set pDirectory $dir
   .changeHeadasRootWndw.entry delete 0 end
   .changeHeadasRootWndw.entry insert end $dir
}

itcl::body FtoolInstance::updateBrowserSetting {} {

     set setting [string trim [.changeBrowserWndw.entry get]]
     set previousChoice $::fguiPref::BrowserSetting

     if { $setting == "" } {
        set ::fguiPref::BrowserSetting $previousChoice
     } else {
        set ::fguiPref::BrowserSetting $setting
        fguiPref save
     }

     destroy .changeBrowserWndw
}

itcl::body FtoolInstance::displayHelpMessage { topic_ { package "NONE" } { website "NONE" } } {
     global heraHelpViewer
     global g_backupDir
     global tcl_platform env

     set docWebSite $preDefineDocWebSite
     if { $website != "NONE" } {
        set docWebSite $website
        if { $website == "local" } {
           set docWebSite $env(LHEASOFT)/lib/fgui/doc/
        }
     }

     if { $package == "NONE" } {
        if { [string range $topic_ [expr [string length $topic_] - 5] end] != ".txt" && \
             [string range $topic_ [expr [string length $topic_] - 5] end] != ".html" } {
           set topic_ ${topic_}.txt
        }

        if { $tcl_platform(platform) != "windows"} {
           if [info exists fguiPref::BrowserSetting] {
              if { $fguiPref::BrowserSetting == "None Defined" } {
                 catch { changeBrowser } err
                 vwait fguiPref::BrowserSetting
              }
              set htmlBrowser [string trim $fguiPref::BrowserSetting]

              if { $htmlBrowser == "" } {
                 set errorFlag 1
              } else {
                 set errorFlag [ catch {
                     exec $htmlBrowser ${docWebSite}${topic_} &
                 } err ]
              }

              if { ![info exists ::fguiPref::BrowserSetting] || \
                   ([info exists errorFlag] && $errorFlag == 1) } {

                 if [winfo exists .fhelpframe] {
                   destroy .fhelpframe
                 }
              
                 toplevel .fhelpframe -class Dialog
                 wm title .fhelpframe "fgui: ${runcmd}.txt"
              
                 frame .fhelpframe.f1
                 set f1 .fhelpframe.f1
                 text $f1.text -bd 1 -width 80 -font {courier 13 bold}\
                     -yscrollcommand {.fhelpframe.f1.yscroll set} \
                     -xscrollcommand {.fhelpframe.f1.xscroll set}
                 set htext $f1.text
                 scrollbar $f1.yscroll \
                           -command {.fhelpframe.f1.text yview}
                 scrollbar $f1.xscroll -orient horizontal \
                          -command {.fhelpframe.f1.text xview}
                 pack $f1.yscroll -side right -fill y
                 pack $f1.xscroll -side bottom -fill x
                 pack $f1.text -side left  -fill x -fill y
                 pack $f1
              
                 button .fhelpframe.button -text OK -command  "destroy .fhelpframe"
                 pack .fhelpframe.button -pady 4

                 $htext delete 1.0 end
              
                 set txtfile [SearchDir $helpPath $fhelpname.txt ]
                 if {$txtfile != "" }  {
                    set hchan [open $txtfile r]
                    fileevent $hchan readable [itcl::code $this DumpText $htext $hchan ]
                 } else {
                    $htext insert end "404 - No text help file found"
                 }
              }
           }
        } else {
           exec cmd /c $htmlBrowser /max ${docWebSite}${topic_} &
        }
     } else {
        # help format:
        # 1. ftools : $helpDocWebSite/<toolname>.html
        # 2. chandra: $helpDocWebSite/<toolname>
        # 3. xmm-sas: $helpDocWebSite/<toolname>

        set idx [lsearch -glob $helpDocWebSiteList [list $package *]]
        switch $package {
            "ftools" {
               set helpDocWebSite [lindex [lindex $helpDocWebSiteList $idx] 1]/${topic_}.html
            }
            "chandra" {
               set helpDocWebSite [lindex [lindex $helpDocWebSiteList $idx] 1]/ahelp
               if { $topic_ != "ahelp" } {
                  set helpDocWebSite $helpDocWebSite/${topic_}.html
               }
            }
            "xmm-sas" {
               set helpDocWebSite [lindex [lindex $helpDocWebSiteList $idx] 1]/${topic_}
            }
            default {
               set helpDocWebSite $topic_
            }
        }

        if { !$isWin } {
           catch { exec $htmlBrowser ${helpDocWebSite} & } err
        } else {
           exec cmd /c $htmlBrowser /max ${helpDocWebSite} &
        }
     }
}

itcl::body FtoolInstance::paneManager { command args } {

    set w [lindex $args 0]
    set wndwA $toolHList
    set wndwB .ftoolframe.log

    switch $command {
        start {
            set startY [winfo pointery $w ]
        }

        motion {
            set y [lindex $args 1]
            set realY [expr $y + [winfo rooty $w]]
            set Ymax  [winfo height .ftoolframe]
#puts "y: $y, rootY: [winfo rooty $w], realY: $realY, Ymax: $Ymax"
            set frac [expr double($realY)/$Ymax]
#puts "frac: $frac"
            if { $frac < 0.05 } {
               set frac 0.05
            }
            if { $frac > 1.0 } {
               set frac 1.0
            }
            place .ftoolframe.tools.grip -rely $frac
        }

        stop {
            set endY [winfo pointery $w ]
            scan [winfo geometry $wndwA] "%dx%d+%d+%d" Rw Rh Rx Ry
            scan [winfo geometry $wndwB] "%dx%d+%d+%d" Lw Lh Lx Ly
#puts "orginial: [winfo geometry $wndwA]"
#puts "          [winfo geometry $wndwB]"
#puts "text new height: [expr abs($Rh + [expr $endY - $startY]) / $toolHListTextRatio]"
#puts "log new height : [expr $Lh - [expr $endY - $startY]]"
            set newHeight [expr ($Rh + [expr $endY - $startY]) / $toolHListTextRatio]
            if { $newHeight <= 0 } {
               set newHeight 1
            }
            $wndwA configure -height $newHeight
            $wndwB configure -height \
                     [expr ($Lh - [expr $endY - $startY])]
#pack .ftoolframe.tools.log.f2 -side top -fill x -pady 3 -padx 3

            update idletasks
#puts "after: [winfo geometry $wndwA]"
#puts "       [winfo geometry $wndwB]"

        }
    }
}

itcl::body FtoolInstance::setCurrentTab { page } {
    set currentPage $page
    set xTerminal $xTerminalList($page)
}

itcl::body FtoolInstance::_resolveSymLinks { filename_ } {

   if { [file type $filename_] == "link" } {
      set lName [file readlink $filename_]
      if { [file pathtype $lName] == "relative" } {
         set root [file dirname $filename_]
         set lName [file join $root $lName]
      }
      return [_resolveSymLinks $lName]
   }
   return $filename_
}
