package require Iwidgets 3.0
lappend auto_path $env(XSPEC)/manager $env(XSPEC)/manager/widget
package require AllWidgets 1.0
global   unknown_handler_order unknown_handlers unknown_pending powContainer
global   xsaux

load libpow.so

 package require Iwidgets
 wm withdraw .

global globalBgColor 
global globalFgColor
global activeBgColor
global activeFgColor 
global checkBBgColor 


set globalBgColor #c0c0c0
set globalHLColor #ffffff
set globalFgColor #00009a
set activeBgColor #0c106c
set activeFgColor #f8fcfe
set checkBBgColor #b03060
set defaultFont {fixed}
set fontSize 10
set fontStyle bold
#set defaultFont courier

#if { ![lsearch  [font families] $defaultFont ]} {
##   if { [lsearch  [font families] lucidatypewriter ]} {
#       set defaultFont [list lucidatypewriter $fontsize $fontStyle]
#   } else {
#       set defaultFont [list fixed 10 ]
#   }
#} else {
#   set defaultFont [list $defaultFont $fontSize $fontStyle]
#}


option add *Background          $globalBgColor
option add *Foreground          $globalFgColor
option add *HighlightBackground $globalHLColor
option add *activeForeground    $activeFgColor
option add *activeBackground    $activeBgColor
option add *selectForeground    $activeFgColor
option add *selectBackground    $activeBgColor
option add *selectColor         $checkBBgColor
option add *textFont            $defaultFont

set XSoutput .xstext
powSetupColormap $XSoutput 60 0
set powGUIflag 0

wm geometry $XSoutput -0+0
wm protocol $XSoutput WM_DELETE_WINDOW {XStkExit}
wm title $XSoutput "Xspec++ Output"
set XStext [iwidgets::scrolledtext $XSoutput.text  -wrap none  \
                -vscrollmode dynamic -hscrollmode static -visibleitems 132x24]
pack $XStext -padx 10 -pady 10  -expand true

global Console XScon XSplot

;proc XStkExit {} {
        set message "Do you really want to quit?";
        set title "XSPEC++ exit?"
        set exx [ list tk_messageBox -title $title -type yesno -default no -message $message -icon question ]
        if ![catch $exx answer]  {
           if  ![ string compare yes $answer ]  {
                 if ![string compare tclexit  [info commands tclexit] ] {
                        tclexit
                 } else {  puts "Exit command removed: Ctrl-C to exit" }
           } else {
                return
           }
        }
}

proc XSplotConfig { container {x 500} {y 500} args } {
  if {[winfo exists $container]} {
         $container configure -height $x -width $y
  } else {
        puts stderr "Can't size plot window now\n"
        return 1
  }
}

array set unknown_handlers {
        xspec  XSunknown
        tcl    tcl_unknown
}

set unknown_handler_order [list  xspec tcl]

interp alias {} exit {} XStkExit
interp alias {} quit {} XStkExit

toplevel .xstop -colormap $XSoutput  -visual [list [winfo visual $XSoutput]  [winfo depth $XSoutput]]
wm withdraw .xstop

 set w [iwidgets::mainwindow .xstop.mw -height [winfo screenheight .xstop] -width [winfo screenwidth .xstop] -title Xspec++]
 set r [$w childsite]     
 
 set xsleft [frame $r.left ]
 set auxHeight 350
 
 set xscon [ console $xsleft.console -height 24 -font $defaultFont] 
 set xsaux [ ::iwidgets::scrolledframe  $xsleft.aux \
        -hscrollmode dynamic -vscrollmode dynamic -height $auxHeight]  
# pack $xscon -fill both -expand 1
# pack $xsaux -fill x


 
 
toplevel .xstop -colormap $XSoutput -height [winfo screenheight .] -width [winfo screenwidth .] -visual [list [winfo visual $XSoutput]  [winfo depth $XSoutput]]
wm withdraw .xstop

 set xsright [::iwidgets::scrolledframe $r.right -relief raised -width 568 -height 670 \
                -hscrollmode none -vscrollmode none]

 set xsplotl [label [$xsright childsite].lab -font {helvetica 16 bold} -text "Xspec Plot"]
 set xsplotf [frame [$xsright childsite].win -container true ]
 
 set powBg \#FFFFFF
 
        grid $xscon -row 0 -column 0 -sticky news
        grid $xsaux -row 1 -column 0 -sticky news 
        grid rowconfigure $xsleft 1 -minsize 350
        #pane $xsleft.console $xsleft.aux -orient vertical
        set XSplot $xsplotf
        set XScon $xscon
 
        pack $xsleft -side left 
  
  grid $xsplotl -row 0 -column 0 -columnspan 8 -pady 10 -sticky news
  grid columnconfigure [$xsright childsite] 0 -minsize 25
  grid columnconfigure [$xsright childsite] 3 -minsize 25
  grid $xsplotf -row 1 -column 1 -columnspan 8
   powInit $XSoutput $xsplotf $powGUIflag
  .pow.bottom configure -background $globalBgColor
  .pow.pow configure -background $powBg
  .pow.scrollx configure -bd 2 -bg $globalBgColor
  .pow.scrolly configure -bd 2 -bg $globalBgColor
  pack $xsright -side right  
#  wm geometry .pow [winfo geometry $powContainer]
# set leftHeight [expr [winfo height $xscon.console] + $auxHeight]
# set leftWidth [winfo width $xscon.console] 
# XSplotConfig 


 $w center
 focus -force $xscon
 $w activate

