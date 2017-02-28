 wm withdraw .
package require Iwidgets 3.0
lappend auto_path $env(XSPEC)/manager $env(XSPEC)/manager/widget
package require AllWidgets 1.0
global   unknown_handler_order unknown_handlers unknown_pending powContainer
global   xsaux

load libpow.so

 package require Iwidgets

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
wm withdraw .xstext
powSetupColormap $XSoutput 60 0
set powGUIflag 0

toplevel .xstop -colormap $XSoutput -visual [list [winfo visual $XSoutput]  [winfo depth $XSoutput]]
wm withdraw .xstop
 
 set w [iwidgets::mainwindow .xstop.mw -height [winfo screenheight .xstop] -width [winfo screenwidth .xstop] -title Xspec++]
 set r [$w childsite]     

$w activate

