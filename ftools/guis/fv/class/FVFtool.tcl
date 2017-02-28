itcl::class FtoolParameter {
public variable name
public variable value
public variable descr
public variable mode
public variable type
public variable isFits
public variable isOutput
public variable frame
public variable cframe 

private variable pseparator
private variable temp_value 

constructor {args} {}
destructor {}

public method GetParameter {frame}
public method CheckParameter {}

private method ParBrowser {}
private method ParApply  {}
private method ParAppend {}
private method ParReset {}
private method ParOK {parobj flag}
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
   set isFits 0
   set isOutput 0
   set pseparator ","
}

itcl::body FtoolParameter::destructor {} {
}


itcl::body FtoolParameter::GetParameter {frame} {
# variable is never used anywhere
  global g_entryFont
  global g_titleFont
  set temp [string tolower $name]
  set cframe ${frame}.${temp}
  set fr $cframe
  if [winfo exists $fr] {
      return
  }
  frame $fr -relief groove 

  label $fr.name -text $name -width 10 -anchor w
  label $fr.descr -text $descr -width 36 -anchor w -font {Arial 10 bold}
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

  if {$type == "s" } { 
    button $fr.browser -text browser -command [itcl::code $this ParBrowser]
    pack $fr.browser -side left -padx 2
  }
}

itcl::body FtoolParameter::CheckParameter {} {
   
  switch -exact -- $type {
        b { 
             if {$value != "yes" && $value !="no"} { 
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

itcl::body FtoolParameter::ParBrowser {} {
   global g_titleFont

   set temp_value [string trim $value]
   set parobj [ iwidgets::fileselectiondialog .parbrowser \
        -childsitepos s  -selectionon false -title $name -height 500 ] 
   .parbrowser hide 3 

   set  f "[.parbrowser childsite].f" 
   frame $f -relief groove -bd 2 
   pack $f -side top -fill x -expand yes -pady 3
   
   set f1 $f.f1
   frame $f1  
   pack $f1 -side left
   radiobutton $f1.output -variable [itcl::scope isOutput] -value 1 \
          -font g_titleFont -text "Output" -anchor w \
          -command [itcl::code $this ParReset ]
   radiobutton $f1.input -variable [itcl::scope isOutput] -value  0 \
          -font g_titleFont -text "Input" -anchor w  \
          -command [itcl::code $this ParReset ]
   checkbutton $f1.isfits -variable [itcl::scope isFits] -onvalue 1 -offvalue 0 \
          -anchor w -text "Fits file"
   pack $f1.output -side top -anchor w
   pack $f1.input -side top -anchor w
   pack $f1.isfits -side top -anchor w

   set f2 $f.f2
   frame $f2  
   pack $f2 -side right -padx 10
   button $f2.apply -text "Apply to..." -command [itcl::code $this ParApply]
   button $f2.append -text "Append to..." -command [itcl::code $this ParAppend]
   iwidgets::entryfield $f2.sep -labeltext "Separator"  \
        -textvariable [itcl::scope pseparator] -width 6 
   pack $f2.apply -side top -pady 2 -fill x -expand yes
   pack $f2.append -side top -pady 2 -fill x -expand yes
   pack $f2.sep -side top -anchor w -pady 2 -fill x -expand yes

   set ventry "[.parbrowser childsite].value" 
   iwidgets::entryfield $ventry -labeltext "Value"  \
        -textvariable [itcl::scope temp_value]  
   pack $ventry -fill x -expand yes -pady 3 

   .parbrowser  buttonconfigure 0 -command [itcl::code $this ParOK $parobj 1]
   .parbrowser  buttonconfigure 2 -command [itcl::code $this ParOK $parobj 0]

   ParReset 
   .parbrowser activate
}

itcl::body FtoolParameter::ParApply {} {
   if {$isOutput == 1} {  
     set temp [.parbrowser get]  
     set temp_value "[file dirname $temp]\/"
   } else {
     set temp_value [.parbrowser get] 
   }
} 

itcl::body FtoolParameter::ParAppend {} {
   if {$temp_value == ""} { 
     set temp_value [.parbrowser get]
   } else {   
     set temp_value "${temp_value}${pseparator}[.parbrowser get]" 
   }
} 

itcl::body FtoolParameter::ParReset { } {
   if {$isOutput == 1} { 
       [.parbrowser childsite].f.f2.append configure -state disabled 
   } else {
       [.parbrowser childsite].f.f2.append configure -state normal
   } 
} 

itcl::body FtoolParameter::ParOK {parobj flag} {
   if { $flag == 1 } {
     set value $temp_value 
   }
   itcl::delete object $parobj 
} 

itcl::class FtoolInstance {

private variable origPath
private variable ftoolPath
private variable helpPath
private variable parPath
private variable autoSave
private variable autoClear

private variable currPackage
private variable currFtool
private variable currCmd 

private variable favorite

private variable wlist 
private variable logtext 
private variable logchan 

private variable currParFile 
private variable currParList
private variable parObjs

private variable isParTool

private variable showHidden
private variable goodCommand
private variable outputFits

private variable curPos
private variable tmp_ftoolPath
private variable tmp_helpPath
private variable tmp_parPath
private variable tmp_autoSave
private variable tmp_autoClear
private variable tmp_currCmd
private variable tmp_outputFits

constructor {args} {}
destructor {}

public method MainMenu {}

private method MenuInit {}

private method PackageMenu { }
private method PackageGet { package }

private method FtoolGet {}
private method FtoolRun {}
private method FtoolCancel {}
private method FtoolTerminate {}
private method FtoolHelp {}

private method FavoriteGet {}
private method FavoriteAdd {}
private method FavoriteDelete {}
private method FavoriteReset {}
private method FavoriteCheck {}

private method ConfigSet {}
private method ConfigOk {}

private method HelpGet {}

private method LogDump {}
private method LogClear {}
private method LogError {}

private method ParExist {}  
private method ParClear {}  
private method ParSave {}  
private method ParSet {}  
private method ParDialog {}  
private method ParsRedraw {fpar}
private method ParsReset {fpar}
private method ParsOk {}
private method ParsCancel {}

private method CmdDialog {}  
private method CmdOk {}
private method CmdCancel {}

private method DumpText {wtext chan termCmd}  
private method SearchDir {pathname filename}  

}

itcl::body FtoolInstance::constructor {args} {
global env
   set ftoolPath ""

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
   } else {
       set parPath ""
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

   toplevel .ftoolframe -class Dialog
   wm title .ftoolframe "fv: Run Ftool"


####################################
#                                  #
#  Set up the Ftools Menu Bar      #
#                                  #
####################################

   frame .ftoolframe.menu
   pack .ftoolframe.menu -fill x -pady 2 -pady 2
   set fm .ftoolframe.menu

#  Ftool menu  button
   menubutton $fm.ftool -text "Ftool" -menu $fm.ftool.menu -font g_titleFont
   pack $fm.ftool -padx 2 -pady 2 -side left

   set m3 [menu $fm.ftool.menu -tearoff 1 ]
   $m3 add command -label "Run..." \
      -command [itcl::code $this FtoolRun] -font g_titleFont
   $m3 add command -label "Cancel..." \
      -command [itcl::code $this FtoolCancel] -font g_titleFont
   $m3 add command -label "Ftool Help" \
      -command [itcl::code $this FtoolHelp] -font g_titleFont
   $m3 add separator
   $m3 add command -label "Clear Log" \
      -command [itcl::code $this LogClear] -font g_titleFont
   $m3 add command -label "Dump Log" \
      -command [itcl::code $this LogDump] -font g_titleFont
   $m3 add separator
   $m3 add command -label "Exit" \
      -command "destroy .ftoolframe"  -font g_titleFont

#  package menu button
   menubutton $fm.packages -text Packages -menu $fm.packages.menu -font g_titleFont
   pack $fm.packages -padx 2 -pady 2 -side left
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
          -command [itcl::code $this HelpGet] -font g_titleFont
   $m4 add command -label "About selected ftool" \
          -command [itcl::code $this FtoolHelp] -font g_titleFont

####################################
#                                  #
#  Set up the Ftools List          #
#                                  #
#################################### 

   frame .ftoolframe.tools -relief groove -bd 4  
   pack .ftoolframe.tools -fill x -padx 2 -padx 2
   set ft .ftoolframe.tools  

   label $ft.label -font {System 15 bold}   
   pack $ft.label -side top -anchor w -padx 2 -padx 2

   frame $ft.f1  
   pack $ft.f1 -side top -fill x 
   listbox $ft.f1.list -bd 1  -selectmode single \
       -yscrollcommand {.ftoolframe.tools.f1.yscroll set} \
       -xscrollcommand {.ftoolframe.tools.f1.xscroll set} \
       -exportselection 0 -takefocus 0 -font g_titleFont
   set wlist $ft.f1.list
   scrollbar $ft.f1.yscroll \
             -command {.ftoolframe.tools.f1.list yview}
   scrollbar $ft.f1.xscroll -orient horizontal \
            -command {.ftoolframe.tools.f1.list xview}
   bind $ft.f1.list <Double-Button-1> [itcl::code $this FtoolRun ]  
   pack $ft.f1.yscroll -side right -fill y
   pack $ft.f1.xscroll -side bottom -fill x
   pack $ft.f1.list -side left -fill x  -expand true

   frame $ft.f2  
   pack $ft.f2 -side top -fill x -pady 3 -padx 3
   button $ft.f2.fav -text "Add to Favorite" -width 12 -font g_titleFont
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
   

####################################
#                                  #
#  Set up the Ftools Log           #
#                                  #
#################################### 

   frame .ftoolframe.log -relief groove -bd 4  
   pack .ftoolframe.log -fill x -padx 2 -padx 2
   set fl .ftoolframe.log  

   label $fl.label -font g_titleFont -text Log  
   pack $fl.label -side top -anchor w -padx 2 -padx 2

   frame $fl.f1 
   set f1 $fl.f1 
   text $f1.text -bd 1 \
       -yscrollcommand {.ftoolframe.log.f1.yscroll set} \
       -xscrollcommand {.ftoolframe.log.f1.xscroll set} \
       -wrap none -font g_entryFont
   set logtext $f1.text 
   scrollbar $f1.yscroll \
             -command {.ftoolframe.log.f1.text yview}
   scrollbar $f1.xscroll -orient horizontal \
             -command {.ftoolframe.log.f1.text xview}
   pack $f1.yscroll -side right -fill y
   pack $f1.xscroll -side bottom -fill x
   pack $f1.text -side left  -fill x -fill y -expand true
   pack $f1 -fill x -side top -padx 2 -padx 2 -expand true

   frame $fl.f2 
   pack $fl.f2 -side top -fill x -pady 3 -padx 3
   button $fl.f2.dump -text "Dump" -command [itcl::code $this LogDump] -font g_titleFont
   button $fl.f2.clear -text "Clear" -command [itcl::code $this LogClear ] -font g_titleFont
      
   button $fl.f2.close -text "Exit" -command \
      "destroy .ftoolframe" -font g_titleFont
   pack $fl.f2.dump -side left -padx 20
   pack $fl.f2.clear -side left -padx 20
   pack $fl.f2.close -side right -padx 20 

   bind .ftoolframe <Destroy> { destroy . }
   MenuInit
    
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
    bind .ftoolframe.tools.f1.list <Double-Button-1> {}  
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
   if [winfo  exists .ftoolframe.tools.label ] {
       .ftoolframe.tools.label configure -text $package
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
       .ftoolframe.tools.f1.list selection set 0
       .ftoolframe.tools.f2.fav configure -command [itcl::code $this FavoriteAdd]
       .ftoolframe.tools.f2.fav configure -text "Add to Favorite"
       .ftoolframe.tools.f2.fav configure -state normal
       .ftoolframe.tools.f2.run configure -state normal
        bind .ftoolframe.tools.f1.list <Double-Button-1> [itcl::code $this FtoolRun ] 
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

   if [winfo  exists .ftoolframe.tools.label ] {
       .ftoolframe.tools.label configure -text "My favorite" -font g_titleFont
   }
   if [winfo  exists $wlist ] {
      .ftoolframe.tools.f2.fav configure -command [itcl::code $this FavoriteDelete]
      .ftoolframe.tools.f2.fav configure -text "Delete "
      .ftoolframe.menu.favorite.menu entryconfigure 2 -state disabled
      $wlist delete 0 end 
      set num [FavoriteCheck]
      if {$num == 0} { 
         return 
      }
      for {set i 0} {$i < $num} {incr i} {
        $wlist insert end [lindex $favorite $i]
      }
      $wlist selection set 0
   }
}

itcl::body FtoolInstance::FavoriteReset {} {
     set favorite ""
     .ftoolframe.tools.f1.list  delete 0 end 
}

itcl::body FtoolInstance::ConfigSet {} {
  global g_titleFont

  set tmp_ftoolPath $ftoolPath
  set tmp_parPath $parPath
  set tmp_helpPath $helpPath
  set tmp_autoSave $autoSave
  set tmp_autoClear $autoClear

  toplevel .cframe   -class Dialog
  wm title .cframe "fv: Ftool Configuration "
  
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
      .ftoolframe.tools.label configure -text ""

      ParClear
      set currFtool ""

      destroy .cframe
}

itcl::body FtoolInstance::HelpGet {} {
    hhelp ftool
}

itcl::body FtoolInstance::FtoolHelp {} {
   set fhelpname [ FtoolGet ]

   if [winfo  exists .fhelpframe] {
     return 
   } 

   toplevel .fhelpframe -class Dialog
   wm title .fhelpframe "fv: ${fhelpname}.txt"

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
   
   button .fhelpframe.button -text OK -command  " destroy .fhelpframe"
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

  

itcl::body FtoolInstance::FtoolRun {} {

#  if it is the new ftool, initialize it.
   .ftoolframe.tools.f2.run configure -state disabled
   bind .ftoolframe.tools.f1.list <Double-Button-1>  {} 
   .ftoolframe.tools.f2.cancel configure -state normal
   .ftoolframe.menu.favorite.menu entryconfigure 2 -state normal
 
   set temp [ FtoolGet ]
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
      bind .ftoolframe.tools.f1.list <Double-Button-1> [itcl::code $this FtoolRun ]  
      .ftoolframe.tools.f2.cancel configure -state disabled
      .ftoolframe.menu.ftool.menu entryconfigure 1 -state normal
      .ftoolframe.menu.ftool.menu entryconfigure 2 -state disabled
      return
   } else {
      set curPos [$logtext index end]
      if {$autoClear == 1} {
         LogClear
      }
   }

   if {$isParTool == 1} {
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
     
   $logtext configure -state normal
   $logtext insert end "$cmdstr \n"
   $logtext configure -state disabled 

   set logchan [open "| $currCmd |& cat " r]
   fileevent $logchan readable \
       [itcl::code $this DumpText $logtext $logchan FtoolTerminate ] 
}

itcl::body FtoolInstance::FtoolTerminate {} {
   global g_fitsFileMode

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
  bind .ftoolframe.tools.f1.list <Double-Button-1> [itcl::code $this FtoolRun ]  
  .ftoolframe.tools.f2.cancel configure -state disabled
  .ftoolframe.menu.ftool.menu entryconfigure 1 -state normal
  .ftoolframe.menu.ftool.menu entryconfigure 2 -state disabled

  # set the pointer position in log window
  if {$autoClear == 1} {
      $logtext see 0.1
  } else {
      $logtext see $curPos
  }
  return
}

itcl::body FtoolInstance::FtoolCancel {} {
  if {$logchan != "" } {
     catch {close $logchan}
  }
  .ftoolframe.tools.f2.run configure -state normal
  bind .ftoolframe.tools.f1.list <Double-Button-1> [itcl::code $this FtoolRun ]  
  .ftoolframe.tools.f2.cancel configure -state disabled
  .ftoolframe.menu.ftool.menu entryconfigure 1 -state normal
  .ftoolframe.menu.ftool.menu entryconfigure 2 -state disabled
}

itcl::body FtoolInstance::FavoriteAdd {} {
   set i [$wlist curselection]
   set temp [$wlist get $i]
   lappend favorite $temp
}

itcl::body FtoolInstance::FavoriteDelete {} {
   set num [FavoriteCheck]
   if {$num == 0} { 
       return 
   }
   set i [$wlist curselection]
   $wlist delete $i
   set favorite [$wlist get 0 end ]  
   set num [FavoriteCheck] 
   if {$num != 0} { 
      $wlist selection set $i
   }
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
       bind .ftoolframe.tools.f1.list <Double-Button-1>  {} 
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
       bind .ftoolframe.tools.f1.list <Double-Button-1> [itcl::code $this FtoolRun ]  
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
} 

itcl::body FtoolInstance::LogError {} { 
   $logtext configure -state normal
   $logtext insert end \
  "$currFtool needs runtime input and can not be handled by Fv. \n"
   $logtext configure -state disabled 
   FtoolCancel
}  
   

itcl::body FtoolInstance::LogDump {} { 
  
  set logfile [ tk_getSaveFile -initialfile "ftool.log"]
  if { $logfile == "" } { 
     return 
  }

  $logtext configure -state normal 
  set channel [open $logfile a]
  set buffer [split [$logtext get 1.0 end ] \n]
  set num [llength $buffer]
  for {set i 0} {$i < $num} {incr i} {
    puts $channel [lindex $buffer $i]
  }
  close $channel
  $logtext configure -state disabled
}  

itcl::body FtoolInstance::ParExist {} {
  set pfile "${currFtool}.par"

  set b [split $parPath \;]
  set dir0 [lindex $b 0]
  set currParFile [file join $dir0 $pfile]

  if [ file exists $currParFile ] {
     return 1
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

  set pchannel [open  $currParFile r]
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
      set parvalue [string trim [lindex $par 3] ]
      set parvalue [string trim $parvalue \"] 
      set pardescr [string trim [lindex $par 6] ]
      set pardescr [string trim $pardescr \"] 
      set partype  [string trim [lindex $par 1] ]
      set parmode  [string trim [lindex $par 2] ]
      lappend currParList $parname
      set objname par_${parname}
      FtoolParameter $objname $parname $parvalue $partype $parmode \
          $pardescr 
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

  toplevel .pframe -class Dialog
  wm title .pframe "fv: $pfile"


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
  pack .pframe.params.scrolly -side right -fill y -expand true
  pack .pframe.params.list  -side left -fill x -fill y -expand true
  pack .pframe.params  -side top -fill x 

  set fpar .pframe.params.list
  ParsRedraw $fpar

  frame .pframe.commands
  set pfm .pframe.commands
  checkbutton $pfm.hidden -variable [itcl::scope showHidden] -onvalue 1 -offvalue 0 \
            -text "Show hidden" -command [itcl::code $this ParsRedraw $fpar]
  button $pfm.reset -text Reset -width 6 \
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

# force to recopy the system one. 
  file delete  $currParFile  
  
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
  if {$showHidden == 1} {
     for {set i 0} {$i < $npar} {incr i} {
        set objname [lindex $parObjs $i]
        $objname GetParameter $fpar
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
         $objname GetParameter $fpar
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
  toplevel .cmdframe -class Dialog
  wm title .cmdframe "fv: $currFtool"

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
   set b [split $pathname \;]
   for {set i 0} {$i < [llength $b] } {incr i} {
       set dirf [lindex $b $i]
       set fullfile [file join $dirf $filename]
       if [file exists $fullfile]  {
          return $fullfile
       }
   } 
   return ""
} 
