namespace eval pgtk {
#
#  Operational overview
#
#    Each plot item has an associated state, an array called
#     state0, state1, and so on.  Each pimage, pcontour, and
#     surface command creates a state# variable by executing
#     pgtk::initstate.
#
#  Contents of state variables (e.g. state0):
#
#  state0(valid) - Starts out as 1, when map is freed or
#                  overwritten, pgtk::expire sets to 0
#  state0(cmd)   - pimage, pcontour or surface
#  state0(mapid) - Map used in plot
#  state0(wcsid) - WCS info used in plot
#
#  state0(szx,szy)  - Size of map
#  state0(zmx,zmy)  - Rebin factor of map
#  state0(trfstate) - Transformation state, holds the coordinate system
#                     to which this state's coords are rotated, resized, 
#                     recentered when plotting (pcontour only)
#  state0(jrnlst)   - Journalling, a list of commands that is executed
#                     at every refresh (e.g. label, draw commands)
#  state0(levels)   - The levels used to draw this plot
#
#  state0(xmin0,xmax0,ymin0,ymax0) - The initial array bounds to plot
#  state0(xmin,xmax,ymin,ymax)     - The current array bounds to plot
#                                    (i.e. zoomed portion of map)
#  state0(slant)    - Number of slant pixels (surface only)
#  state0(winzm)    - Current zoom factor (Reported in zoom label)
#  state0(vpnum)    - Current viewport number
#  state0(vpfile)   - Current viewport file
#  state0(vpmax)    - Maximum viewports in vpfile
#  state0(vplist)   - Current defined viewport (format: v1 v2 v3 v4)
#  state0(truvp)    - Current actual viewport (Note: vplist is the
#                      user-entered viewport, while truvp is the actual
#                      viewport after its been adjusted to maintain
#                      aspect ratio)
#  state0(truwin)   - Current mapping of coordinates onto viewport
#
#    The variable named state
#     contains the array name corresponding to the current
#     plot (i.e. the one which coordinates, etc. are returned
#     from) is stored in the state variable.
#
#    The plotary array contains elements designated as plotary(1),
#     plotary(2), etc. which represent the plot items present in
#     each viewport of that number.  Each element is a list of 
#     state variable names in the order that they were plotted.
#
#  Commands defined in this file:
#   (Some pgtk:: commands are also defined in implot/tclimplot.c)
#
#  proc newpage {}
#     Restart plot counter on new page
#
#  proc initstate {cmd vpnum}
#     Initialize state of /TK device
#
#  proc flushxtk {}
#     Execute commands buffered in xtkcmds variable
#
#  proc slantset {slant}
#     Update slant factor for current plot (important for surface)
#
#  proc setup {width height}
#     Set up GUI for /TK device
#
#  proc modct {key val}
#     Modify color table settings (Brightness/Constrast sliders)
#
#  proc greyct {}
#     Set to greyscale color table (Print... option)
#
#  proc revertct {}
#     Restore color table after switching to greyscale (Print... option)
#
#  proc track { x y }
#     Coordinate tracker
#
#  proc zoom { xpix ypix zm }
#     Zoom image
#
#  proc cursor {mode x y {ci 1}}
#     Use cursor to select point while allowing zooming/tracking to
#     continue to respond.  Must use Shift-click to select.
#
#  proc nocursor {}
#     Disable cursor setting
#
#  proc find {widget}
#     Return list of widgets of the Pgplot type at and under $widget
#     Standard usage to find all Pgplot widgets: pgtk::find .
#
#  proc tkisup {}
#     Return 1 if a pgtk window is up
#
#  proc tkexists {}
#     Return 1 if a pgtk window exists
#
#  proc istruecol {}
#     Return 1 if current device is truecolor /xtk device
#
#  proc id {device}
#     Return numeric id of open device which corresponds to $device
#
#  proc tracetrigger {name1 name2 op}
#     Trace routine (writing xtkcols sets pgtk::destroy)
#
#  proc close {}
#     Close all /xtk devices
#
#  proc refresh {}
#     Refresh (redraw) /xtk plot
#
#  proc replot {}
#     Revert to initial plot
#
#  proc outputdialog {}
#     Print... option widget
#
#  proc outcmd {}
#     Print... option action
#
#  proc dumpdialog {}
#     Screen Grab... option
#
#  proc dump {{viewer xv} {outfile pgplot.gif}}
#     Dump screen and crop with viewer
#
# proc askdialog { msg {type question} {blist {OK Cancel}} {defidx 1}}
#     Question widget
#
#  proc journal {cmdlist {add 1}}
#     Journal plotting commands to run upon refresh
#
#  proc curwcsid {}
#    Return wcsid for current state
#
#  proc curlevs {}
#    Return current image color levels for current viewport
#
#  proc coordvp {vpnum}
#     Return coordinate dependence for $vpnum viewport (Contour overlay)
#      in the form of a wcsid
#
#  proc switchvp {vpnum}
#     Switch to new viewport for coordinate output and zoom
#
#  proc fswitchvp {vpnum}
#     Switch to new viewport for coordinate output and zoom
#     (Special wrapper for calling from FORTRAN, to avoid
#      printing obscure Tcl errors)
#
#  proc curvp {}
#     Return current vp number used for coordinate readout
#
#  proc infovp {}
#     Print information about viewport data to screen
#
#  proc plotvp {vpnum vplist}
#     Plot label and viewport given in normalized device coords
#     into viewport selector
#
#  proc updtwin {instate}
#     Update input state with current window and vp settings
#
#  proc expire {mapid}
#     Go through all the states and invalidate the ones using
#     mapid as data
#
#  proc upcopy {frommap tomap}
#     Update copy information (MAPCOPY in header)
#

#
#  Variables containing the state of the pgtk device
#
variable plotidx -1   ;# Index of plot state variables
variable plotary      ;# List of plot state variables
variable state ""     ;# Current plot state variable name
variable xtkcmds {}   ;# To buffer commands for execution after xtk device
                       # exists (special case: pcontour)

variable grid
variable scale
variable coltab

variable curscmd {} ;# Cursor command in effect
variable cursci  1  ;# Cursor command color index
variable journal 0  ;# Whether journalling is active
variable defwid 760 ;# Default width of pgplot widget
variable defhgt 576 ;# Default height of pgplot widget
variable contrast   ;# Current contrast applied to pgplot widget
variable bright     ;# Current brightness applied to pgplot widget
#
# Variables needed for outputdialog
#
   variable prcmd
   variable def_fname
   variable def_gname
   variable prcmd_fname
   variable prcolor
   variable prland
   variable prsavct

#
# Other variables in the pgtk namespace
#
   variable prgname "Ximage"
   variable DWP ".ximage"     ;#DialogWindowPrefix - For popup dialogs
   variable cmdkey "Alt"      ;#May be different on other platforms
   variable destroy 0         ;#Whether to destroy /xtk widget on close
   variable quietrefresh 0    ;#Whether to complain about plotting
                              ;# invalid states

proc newpage {} {
#
#  Restart plot counter on new page
#
   variable plotidx
   variable plotary
   variable quietrefresh
   variable state

   set plotidx -1

   if [info exists plotary] { 
      foreach vp [array names plotary] {
         foreach locstate $plotary($vp) {
            variable $locstate
            set wcsid [set ${locstate}(wcsid)]
            if { $wcsid != "" } { wcs decr $wcsid }
            unset $locstate
         }
      }
      unset plotary
   }
   if [tkexists] { $pgtk::vpcanvas delete all }
   set quietrefresh 0
   set state ""
}

proc initstate {cmd vpnum} {
#
#  Initialize state of /TK device
#
   global dismap
   variable plotidx
   variable plotary
   variable grid
   variable scale
   variable zmlbl
   variable bright
   variable contrast
   variable coltab
   variable briscl
   variable conscl
   variable state
   variable journal
   variable jrnstate
   variable xtkcmds

   set xtkcmds {} ; # Buffer commands for execution after xtk device
                    # exists

   if { $dismap == "" } {
      txwrite "No display map" 5
      error {}
   }
#
#  Flag vpnum=-1 means overlaying.  Use vpnum from current state.
#
   if { $vpnum == -1 } {
      variable $state
      set vpnum [set ${state}(vpnum)]
   }

   if { $vpnum < 1 } {
      txwrite "Invalid vpnum: $vpnum" 5
      error {}
   }
   
   incr plotidx
   set state "state${plotidx}"
   set jrnstate $state
   variable $state

   if { $cmd != "surface" } { upvar levels lvls }
#
#  If pimage, throw out all other plots in same viewport 
#  They would be overwritten on refresh anyway
#
   if { $cmd == "pimage" } {
      if [info exists plotary($vpnum)] {
         foreach plot $plotary($vpnum) {
            unset pgtk::$plot
         }
      }
      set plotary($vpnum) $state
   } else {
      lappend plotary($vpnum) $state
   }
   set mapid $dismap

   set ${state}(valid) 1
   set ${state}(cmd) $cmd 
   set ${state}(mapid) $mapid 
   set wcsid [chheader mapid=$mapid key=wcsid]
   if { $wcsid == "" } {
      wcs upwcs mapid=$mapid
      set wcsid [chheader mapid=$mapid key=wcsid]
   }
   wcs incr $wcsid
   set ${state}(wcsid) $wcsid
   set ${state}(szx) [chheader mapid=$mapid key=szx]
   set ${state}(szy) [chheader mapid=$mapid key=szy]
   set ${state}(zmx) [chheader mapid=$mapid key=zmx]
   set ${state}(zmy) [chheader mapid=$mapid key=zmy]
   set ${state}(trfstate) ""
   set ${state}(jrnlst) {}
   if { $cmd == "surface" } {
      set ${state}(levels) {}
   } else {
      set ${state}(levels) $lvls(list)
   }
   set ${state}(xmin0) 1
   set ${state}(xmax0) [set ${state}(szx)]
   set ${state}(ymin0) 1
   set ${state}(ymax0) [set ${state}(szy)]
   set ${state}(xmin) [set ${state}(xmin0)]
   set ${state}(xmax) [set ${state}(xmax0)]
   set ${state}(ymin) [set ${state}(ymin0)]
   set ${state}(ymax) [set ${state}(ymax0)]
   set ${state}(slant) 0
#
#  Set zoom label
#
   set ${state}(winzm) 1
   lappend xtkcmds "variable zmlbl"
   lappend xtkcmds "\$zmlbl configure -text \"Zoom: [set ${state}(winzm)]\""
#
#  Save viewport properties
#
   set ${state}(vpnum) $vpnum
   viewport
   set ${state}(vpfile) $viewport(file)
   set ${state}(vpmax) $viewport(max)
   set ${state}(vplist) $viewport(list)
   if { $viewport(file) !="" } { 
      lappend xtkcmds [list plotvp $vpnum $viewport(list)]
   }

   set grid 0
   set scale 0
#
#  Save color table properties
#
   cct
   set bright $cct(defbri)
   set contrast $cct(defcon)
   lappend xtkcmds "variable briscl"
   lappend xtkcmds "\$briscl set $bright"
   lappend xtkcmds "variable conscl"
   lappend xtkcmds "\$conscl set $contrast"
   set coltab $cct(default)
   set journal 1
#
#  Check to see if contour needs transformation
#  by searching for last valid trfstate
#
   if { $cmd == "pcontour" } {
      set stlist $plotary($vpnum)
      set found 0
      set i [expr [llength $stlist] - 2]  ;# Start with one before current
      while { $i >= 0 && !$found } {
         set tmpstate [lindex $stlist $i]
         variable $tmpstate
         if { [set ${tmpstate}(trfstate)] != "" } {
            set ${state}(trfstate) [set ${tmpstate}(trfstate)]
         } else {
            set ${state}(trfstate) $tmpstate
         }
         set state [set ${state}(trfstate)]
         setdismap [set ${state}(mapid)]
         incr i -1
      }
   } else {
#
#  Execute buffered commands for all but pcontour
#  For pcontour, must explicitly use flushxtk from FORTRAN
#
      flushxtk
   }
}

proc flushxtk {} {
#
#  Execute commands buffered in xtkcmds variable
#
   variable xtkcmds

   if { ![tkexists] } {
      return
   }

   foreach bufcmd $xtkcmds {
      eval $bufcmd
   }
   set xtkcmds {}
}

proc slantset {slant} {
#
#  Update slant factor for current plot (important for surface)
#
   variable state
   variable $state

   if { [set ${state}(cmd)] != "surface" } {
      error "Can only set slant for surface"
   }
   set ${state}(slant) $slant
}

proc setup {width height} {
#
#  Set up GUI for /TK device
#
   global mainpg xtkcols
   variable prgname
   variable cmdkey
   variable briscl
   variable conscl
   variable zmlbl
   variable inftx
   variable vpwid
   variable vphgt
   variable vpcanvas

   if { ![info exists ::tk_version] } {
      txwrite "Tk is unavailable: /xtk device disabled" 5
      error {}
   }

   txwrite " Creating ${width}x${height} pgplot widget" 20
 
   if [isfreecmap $xtkcols] {
      set w [tk::toplevel .ximage]
   } else {
      set w [tk::toplevel .ximage -colormap new]
   }
   tk::wm withdraw .ximage
   tk::wm title .ximage $prgname
   tk::wm protocol .ximage WM_DELETE_WINDOW close_pg_window
#
#  Menubar
#
   bind .ximage <Alt-Key> "pgtk::stopcrash %K"

   set m [tk::menu $w.menubar -tearoff 0]
   $m add cascade -label File -menu [tk::menu $m.file]
   $m.file add command -label "Print..." -command pgtk::outputdialog \
                                         -accelerator "$cmdkey+P"

   $m.file add command -label "Screen Grab..." -command pgtk::dumpdialog \
                                         -accelerator "$cmdkey+G"

   $m.file add separator 
   $m.file add command -label Close -command close_pg_window \
                                    -accelerator "$cmdkey+W"
#   $m.file add command -label Quit -command exit \
#                                    -accelerator "$cmdkey+Q"

   $m add cascade -label Edit -menu [tk::menu $m.edit]
   $m.edit add checkbutton -label "Show Grid" \
       -variable ::pgtk::grid -onvalue 1 -offvalue 0 \
       -command {pgtk::journal grid $::pgtk::grid; pgtk::refresh}
   $m.edit add checkbutton -label "Show Scale" \
       -variable ::pgtk::scale -onvalue 1 -offvalue 0 \
       -command {pgtk::journal scale $::pgtk::scale; pgtk::refresh}
   $m add cascade -label Help -menu [tk::menu $m.help]
   $m.help add command -label Ximage -command {
      pgtk::askdialog \
      \
{Help is available via the web at:
http://ximage.gsfc.nasa.gov/
or through the help command.

If Netscape is running, you may
go directly to that page by pressing OK}

      if { $pgtk::askvalue == 0 } {
         catch {exec netscape -remote openURL(http://ximage.gsfc.nasa.gov/)}
      }
   }
   $w configure -menu $m
#
#  Main plot frame
#
   set f [tk::frame $w.f1]
   pgplot $f.plot -width $width -height $height -maxcolors $xtkcols
   set mainpg $f.plot
   tk::scrollbar $f.xscroll -command "$f.plot xview" -orient horizontal
   tk::scrollbar $f.yscroll -command "$f.plot yview" -orient vertical
   $f.plot configure -xscrollcommand "$f.xscroll set"
   $f.plot configure -yscrollcommand "$f.yscroll set"
   tk::grid $f.plot $f.yscroll -sticky news
   tk::grid $f.xscroll -sticky news
   tk::grid rowconfigure $f 0 -weight 1
   tk::grid columnconfigure $f 0 -weight 1
   tk::grid $f -column 0 -row 0 -sticky news
#
#  Side frame (contains info, vp and ctab adjustment)
#
   set sidewid 200
   set sdf [tk::frame $w.f2 -width $sidewid]
   tk::grid $sdf -column 1 -row 0 -sticky nw
#
#  Info frame
#
   set f [tk::frame $sdf.finf -width $sidewid]
   set trackwid 30
   set c [tk::canvas $f.canvas -width $sidewid -height 1]
   tk::grid $c -row 0 -column 0
   set inftx $f.tx
   tk::grid [tk::frame $f.tx] -row 1 -column 0 -padx 6 -pady 6 -sticky nsew
   tk::grid [tk::label $f.tx.sky -relief sunken -justify left -bd 1 \
                                 -width $trackwid -anchor w] \
       -row 0 -column 0 -sticky nw
   tk::grid [tk::label $f.tx.gal -relief sunken -justify left -bd 1 \
                                 -width $trackwid -anchor w] \
       -row 1 -column 0 -sticky nw
   tk::grid [tk::label $f.tx.wc -relief sunken -justify left -bd 1 \
                                -width $trackwid -anchor w] \
       -row 2 -column 0 -sticky nw
   tk::grid [tk::label $f.tx.im -relief sunken -justify left -bd 1\
                                -width $trackwid -anchor w] \
       -row 3 -column 0 -sticky nw
   tk::grid [tk::label $f.tx.val -relief sunken -justify left -bd 1\
                                 -width $trackwid -anchor w] \
       -row 4 -column 0 -sticky nw
   $f.tx.sky configure -text "Primary coordinates (System):\n   ( x , y )"
   $f.tx.gal configure -text "Galactic coordinates:\n   ( l , b )"
   $f.tx.wc configure -text "Detector coordinates:\n   ( x , y )"
   $f.tx.im configure -text "Image coordinates:\n   ( x , y )"
   $f.tx.val configure -text "Pixel value:\n   ( )"
#
#  Zoom buttons
#
   tk::grid [tk::frame $f.bt] -row 2 -column 0 -padx 6 -pady 6 -sticky nw
   tk::grid [tk::button $f.bt.zin -text "Zoom In" \
       -command "pgtk::zoom {} {} 2"] -row 0 -column 0 -sticky nw
   tk::grid [tk::button $f.bt.rplt -text "Replot" \
       -command pgtk::replot] -row 0 -column 1 -sticky nw
   tk::grid [tk::button $f.bt.zout -text "Zoom Out" \
       -command "pgtk::zoom {} {} 0.5"] -row 0 -column 2 -sticky nw
   tk::grid [tk::label $f.bt.zm -relief sunken ] \
       -row 3 -column 1 -pady 3 -sticky new
   set zmlbl $f.bt.zm
   $zmlbl configure -text "Zoom: 1" -bd 1
   tk::grid $f -column 0 -row 0 -padx 6 -pady 6 -sticky nw
#
#  Viewport switcher
#
   set f [tk::frame $sdf.vp -width $sidewid]
   set vpwid 150
   set vphgt [expr round(double([$mainpg cget -height])/  \
                         double([$mainpg cget -width])*double($vpwid))]
   set vpcanvas [tk::canvas $f.cvp -width $vpwid -height $vphgt \
                                   -bd 1 -relief sunken]
   tk::grid $vpcanvas -row 0 -column 0 -sticky nsew
   tk::grid $f -column 0 -row 1 -padx 6 -pady 6 -sticky ew
#
#  Brightness/contrast sliders
#
   set f [tk::frame $sdf.ct -width $sidewid]
   tk::grid [tk::scale $f.brt -from 0.0 -to 1.0 -resolution 0.01 \
       -length $sidewid -command "pgtk::modct bright" \
       -orient horizontal \
       -label Brightness -tickinterval 0.25 -showvalue true] \
            -row 0 -column 0 -sticky nw
   set briscl $f.brt
   tk::grid [tk::scale $f.cnt -from 0.0 -to +4.0 -resolution 0.01 \
       -length $sidewid -command "pgtk::modct contrast" \
       -orient horizontal \
       -label Contrast -tickinterval 1.0 -showvalue true] \
            -row 1 -column 0 -sticky nw
   set conscl $f.cnt
   tk::grid [tk::button $f.reset -text Reset \
       -command "pgtk::modct bright 0.5 contrast 1.0;
                 $pgtk::briscl set 0.5;$pgtk::conscl set 1.0" ] \
            -row 2 -column 0 -sticky ns
   tk::grid $f -column 0 -row 2 -padx 6 -pady 6 -sticky sew
#
#  Necessary to set window size before plotting when withdrawn
#  (Otherwise image is a tiny dot)
#
   update idletasks
#
#  Set scopewindow (column 1) to stay same size
#  Everything else can resize
#
   tk::grid columnconfigure $w 0 -weight 1
   tk::grid rowconfigure $w 0 -weight 1
   tk::grid columnconfigure $w 1 -weight 0
#
#  Link mouseover to world coordinates
#
   tk::bind $mainpg <Motion> { 
      pgtk::track %x %y
   }
#
#  Enable arrow keys for fine movement
#
   tk::bind .ximage <Down>  {tk::event generate $mainpg <Motion> -warp yes \
                         -x %x -y [expr %y+1]}
   tk::bind .ximage <Up>    {tk::event generate $mainpg <Motion> -warp yes \
                         -x %x -y [expr %y-1]}
   tk::bind .ximage <Left>  {tk::event generate $mainpg <Motion> -warp yes \
                         -x [expr %x-1] -y %y}
   tk::bind .ximage <Right> {tk::event generate $mainpg <Motion> -warp yes \
                         -x [expr %x+1] -y %y}
#
#  Zoom on click
#
   tk::bind $mainpg <ButtonPress> {

      set zm 0
      switch %b {
         1   { set zm 2 }
         2   { set zm 1 }
         3   { set zm 0.5 }
      }
      if { $zm > 0 } {
         pgtk::zoom [$mainpg world x %x] [$mainpg world y %y] $zm
      }
   }
#  tk::wm deiconify .ximage
}

proc modct {args} {
#
#  Modify color table settings (Brightness/Contrast sliders)
#
   set idxbri [lsearch -exact $args bright]
   set idxcon [lsearch -exact $args contrast]
   set quals ""

   cct
   if { $idxbri >= 0 } { 
      set newbri [lindex $args [expr $idxbri+1]] 
      set oldbri [expr round($cct(defbri)*100.)/100.]
      if { $newbri != $oldbri } {
         lappend quals "bright=$newbri"
      }
   }
   if { $idxcon >= 0 } { 
      set newcon [lindex $args [expr $idxcon+1]] 
      set oldcon [expr round($cct(defcon)*100.)/100.]
      if { $newcon != $oldcon } {
         if { $oldcon < 0 } {
            lappend quals "contrast=-$newcon"
         } else {
            lappend quals "contrast=$newcon"
         }
      }
   }
   if { $quals != "" } {
      eval cct set $quals
      if { [winfo visual .ximage ] != "pseudocolor" } {
         refresh
      }
   }
}

proc greyct {} {
#
#  Set to greyscale color table (Print... option)
#
   global env
   variable prsavct

   if { [info exists prsavct] } {
      if { $prsavct != {} } {
#        If prsavct defined, then we have already switched to grey
         return
      }
   }

   cct
   if [regexp "^$env(XANADU)/image/ximage/files/\(\\w+\)\.tab$" \
              $cct(default) full ct] {
      set prsavct $ct
   } else {
      set prsavct $cct(default)
   }
   colors setcolor=0 swapcolor=1
   cct set invgray
   if { [winfo visual .ximage ] != "pseudocolor" } { refresh }
}

proc revertct {} {
#
#  Restore color table after switching to greyscale (Print... option)
#
   variable prsavct

   if { [info exists prsavct] } {
      if {$prsavct != {} } {
         colors setcolor=0 swapcolor=1
         cct set $prsavct
         if { [winfo visual .ximage ] != "pseudocolor" } { refresh }
         set prsavct {}
      }
   }
}

proc track { x y } {
#
#  Coordinate tracker
#
   global mainpg
   variable state
   variable $state
   variable inftx

   set sky0 "Primary coordinates (System):\n   ( x , y )"
   set gal0 "Galactic coordinates:\n   ( l , b )"
   set wc0 "Detector coordinates:\n   ( x , y )"
   set im0 "Image coordinates:\n   ( x , y )"
   set val0 "Pixel value:\n   ( )"

#      Validity no longer applies to coordinates, only image map
#  if { ![set ${state}(valid)] } 

   if { $state == "" } { 
       set skytxt $sky0
       set galtxt $gal0
       set wctxt $wc0
       set imtxt $im0
       set valtxt $val0
       return 
   }
   
   set xpix [$mainpg world x $x]
   set ypix [$mainpg world y $y]

   set mapid [set ${state}(mapid)]
   set wcsid [set ${state}(wcsid)]
   set xmin [set ${state}(xmin)]
   set xmax [set ${state}(xmax)]
   set ymin [set ${state}(ymin)]
   set ymax [set ${state}(ymax)]
   set slant [set ${state}(slant)]

   if { $xmin < 1 } { set xmin 1 }
   if { $xmax > [set ${state}(szx)] } { set xmax [set ${state}(szx)] }
   if { $ymin < 1 } { set xmin 1 }
   if { $ymax > [set ${state}(szy)] } { set ymax [set ${state}(szy)] }

   coord xpix=$xpix ypix=$ypix mapid=$mapid wcsid=$wcsid
   set ximg $coord(ximg)
   set yimg $coord(yimg)
#
#  Adjust coordinates for slant if surface
#
   if { $slant != 0 } {
      set ximg [expr $ximg - $slant*($yimg - ($ymin + $ymax)/2.)]
      coord ximg=$ximg yimg=$yimg mapid=$mapid wcsid=$wcsid
   }
   if { round($ximg) >= $xmin && round($ximg) <= $xmax &&
        round($yimg) >= $ymin && round($yimg) <= $ymax } {
      set rastr $coord(xsfmt)
      set decstr $coord(ysfmt)
      set sysstr $coord(system)
      if [regexp {^FK} $sysstr] {
         set sysstr $coord(equinox)
      }
      if [regexp -nocase {Cartesian} $sysstr] {
         set sysstr $coord(unit)
      }
      set skytxt "Primary coordinates ($sysstr):\n   ( $rastr , $decstr )"
      if [ isastbad $coord(lii) ] {
         set lstr ""
      } else {
         set lstr [format "%.3f" $coord(lii)]
      }
      if [ isastbad $coord(bii) ] {
         set bstr ""
      } else {
         set bstr [format "%.3f" $coord(bii)]
      }
      set galtxt "Galactic coordinates:\n   ( $lstr , $bstr )"
      set xstr [format "%.3f" $coord(xpix)]
      set ystr [format "%.3f" $coord(ypix)]
      set wctxt "Detector coordinates:\n   ( $xstr , $ystr )"
      set xstr [format "%.3f" $coord(ximg)]
      set ystr [format "%.3f" $coord(yimg)]
      set imtxt "Image coords:\n   ( $xstr , $ystr )"
      set valtxt "Pixel value:\n   $coord(value)"
   } else {
      set skytxt $sky0
      set galtxt $gal0
      set wctxt $wc0
      set imtxt $im0
      set valtxt $val0
   }

   $inftx.sky configure -text $skytxt
   $inftx.gal configure -text $galtxt
   $inftx.wc configure -text $wctxt
   $inftx.im configure -text $imtxt
   $inftx.val configure -text $valtxt
}

proc zoom { xpix ypix zm } {
#
#  xpix, ypix: new center in detector coords
#  If {}, leave center as is
#
#  Zoom image zm>1 in, zm<1 out
#
   global mainpg
   
   variable state
   variable $state

   if { ![set ${state}(valid)] } { 
      error "Original map data is unavailable" 
   }
   
   set mapid [set ${state}(mapid)]
   set wcsid [set ${state}(wcsid)]
   set xmin [set ${state}(xmin)]
   set xmax [set ${state}(xmax)]
   set ymin [set ${state}(ymin)]
   set ymax [set ${state}(ymax)]
   set szx [set ${state}(szx)]
   set szy [set ${state}(szy)]
   set slant [set ${state}(slant)]

   if { $xpix == {} && $ypix == {} } {
      set ximg [expr ($xmin+$xmax)/2.]
      set yimg [expr ($ymin+$ymax)/2.]
   } else {
#     Do pixel to image conversion
      coord xpix=$xpix ypix=$ypix mapid=$mapid wcsid=$wcsid
      set ximg $coord(ximg)
      set yimg $coord(yimg)
#
#  Adjust coordinates for slant if surface
#
      if { $slant != 0 } {
         set ximg [expr $ximg - $slant*($yimg - ($ymin + $ymax)/2.)]
      }
   }
   set xhlf [expr int(($xmax-$xmin+1)/2.)/$zm]
   set yhlf [expr int(($ymax-$ymin+1)/2.)/$zm]
   if { $xhlf<=0 || $yhlf<= 0 } {
#     txwrite "Maximum zoom reached" 5
      error "Maximum zoom reached"
   }
   set nxmin [expr ceil($ximg-$xhlf)]
   set nxmax [expr floor($ximg+$xhlf)]
   set nymin [expr ceil($yimg-$yhlf)]
   set nymax [expr floor($yimg+$yhlf)]
#
#  If requested area is completely outside of image, ignore request
#
   if { ($nxmin < 1 && $nxmax < 1 ) || ($nymin < 1 && $nymax < 1 ) || 
        ($nxmin > $szx && $nxmax > $szx) || 
        ($nymin > $szy && $nymax > $szy) } {
      return
   }
#
#  Update zoom factor
#
   set winzmx [expr $szx/($nxmax-$nxmin+1)]
   set winzmy [expr $szy/($nymax-$nymin+1)]
   if { $winzmx < $winzmy } {
      set ${state}(winzm) $winzmx
   } else {
      set ${state}(winzm) $winzmy
   }
   if { [set ${state}(winzm)] < 1 } { 
      set ${state}(winzm) 1 
      set nxmin 1
      set nxmax $szx
      set nymin 1
      set nymax $szy
   }
   set ${state}(xmin) $nxmin
   set ${state}(xmax) $nxmax
   set ${state}(ymin) $nymin
   set ${state}(ymax) $nymax
   refresh
}

proc cursor {mode {x 0} {y 0}} {
#
#  Use cursor to select point while allowing zooming/tracking to
#  continue to respond.  Must use Shift-click to select.
#
   global mainpg
   variable curscmd
   variable cursci
   variable svbind
   variable butpr
   variable xcur
   variable ycur

   if { [tk::wm state [tk::winfo toplevel $mainpg]] != "normal" } {
      tk::wm deiconify [tk::winfo toplevel $mainpg]
   }

   set curscmd "$mainpg setcursor $mode $x $y $cursci"
   eval $curscmd

   set svbind [bind $mainpg <ButtonPress>]
   tk::bind $mainpg <ButtonPress> {
      set pgtk::butpr %b
      set pgtk::xcur [$mainpg world x %x] 
      set pgtk::ycur [$mainpg world y %y]
      pgtk::nocursor
   }
   tk::bind $mainpg <Shift-Button> $svbind
   tk::tkwait variable pgtk::butpr
   return [list $xcur $ycur]
}

proc nocursor {} {
#
#  Disable cursor setting
#
   global mainpg
   variable curscmd
   variable svbind

   set curscmd {}
   $mainpg clrcursor

   tk::bind $mainpg <Shift-Button> {}
   tk::bind $mainpg <ButtonPress> $svbind
}


proc find {widget} {
#
#  Return list of widgets of the Pgplot type at and under $widget
#  Standard usage to find all Pgplot widgets: pgtk::find .
#
   set loclist {}

   catch {
      foreach w [tk::winfo children $widget] {
         if { [tk::winfo class $w] == "Pgplot" } {
            lappend loclist $w
         } else {
            set loclist [concat $loclist [find $w]]
         }
      }
   } 
   return $loclist
}

proc tkexists {} {
#
#  Return 1 if a pgtk window exists
#
   set found 0
   foreach w [find .] {
      set found 1
   }
   return $found
}

proc tkisup {} {
#
#  Return 1 if a pgtk window is up
# 
   set found 0
   foreach w [find .] {
      set found 1
      set fw $w
   }
   if { $found } {
      if { [wm state [tk::winfo toplevel $fw]] == "withdrawn" } {
         return 0
      } else {
         return 1
      }
   } else {
      return 0
   }
}

proc istruecol {} {
#
#  Return 1 if current device is truecolor /xtk device
#
   set curid [pgqid]
   if { $curid == 0 } { return 0 }

   foreach w [find .] {
      if { [id $w/xtk] == $curid } {
         if { [winfo visual .ximage ] == "pseudocolor" } {
            return 0
         } else {
            return 1
         }
      }
   }
   return 0
}

proc id {device} {
#
#  Return numeric id of open device which corresponds to $device
#  $device should be of form "widget-path/xtk"
#  If no widget-path is given, assume global $mainpg
#  If 0 returned, corresponding device not found
#
   global mainpg

   set locdev $device
   if [regexp -nocase {^/xtk$} $device] {
      if [info exists mainpg] {
         set locdev $mainpg/xtk
      } else {
         return 0
      }
   }

   foreach w [find .] {
      if [regexp -nocase "^$w/xtk$" $locdev] {
         return [$w id]
      }
   }
   return 0
}

proc tracetrigger {name1 name2 op} {
#
#     Trace routine (writing xtkcols sets pgtk::destroy)
#
   variable destroy

   if { $op == "w" } {
      if { $name1 == "xtkcols" || $name1 == "xtkwid" || 
           $name1 == "xtkhgt" } {
              set destroy 1
      }
   }
}

proc close {} {

   variable DWP
   variable destroy
#
#  Exit out of any dialogs that are up
#
   if { [lsearch [winfo children .] ${DWP}out] >= 0 } {
      ${DWP}out.botfr.exitbutton invoke
   }
   if { [lsearch [winfo children .] ${DWP}err] >= 0 } {
      ${DWP}err.botfr.okbutton invoke
   }
   if { $destroy } {
#
#  Destroy toplevel window containing pgplot /xtk devices
#
      foreach w [find .] {
         set i [id $w/xtk]
         pgslct $i
         pgclos
         tk::destroy [tk::winfo toplevel $w]
      }
      set destroy 0
   } else {
#
#  Withdraw toplevel window containing pgplot /xtk devices
#
      foreach w [find .] {
         set i [id $w/xtk]
         pgslct $i
         pgclos
         set top [tk::winfo toplevel $w]
         tk::wm withdraw $top
      }
   }
   newpage
   return 0
}

proc refresh {} {
#
#     Refresh (redraw) /xtk plot
#
   global tchat lchat curmap mainpg default
   variable curscmd
   variable journal
   variable coltab
   variable bright
   variable contrast
   variable zmlbl

   variable plotidx
   variable plotary
   variable state
   variable $state
   variable quietrefresh

   .ximage configure -cursor watch
   set cleanup "map set $curmap; 
                levels num=$default(numlevs);
                chat $tchat $lchat;
                .ximage configure -cursor {}"

   if { ![info exists ::debug] } { chat 5 5 }

   set vpall [lsort -integer [array names plotary]]
#
#  Check that all states are valid
#   (Make sure only ask once, quietrefresh)
#
   if { !$quietrefresh } {
      set valid 1
      foreach vp $vpall {
        foreach curstate $plotary($vp) {
           if { ![set pgtk::${curstate}(valid)] } { set valid 0 }
           if { !$valid } { break }
        }
        if { !$valid } { break }
      }
      if { !$valid } {
         askdialog "Some map data is unavailable. Redrawing the image\nwill lose some elements.  Redraw it anyway?"

         if { $pgtk::askvalue == 0 } {
            set quietrefresh 1
         } else {
            eval $cleanup
            return
         }
      }
   }
#
#  Check for color table change (Need to pgpage on truecolor devices)
#
   set newpage 0
   if { [winfo visual .ximage ] != "pseudocolor" } {
      cct
      if { $cct(default) != $coltab } { 
         set coltab $cct(default)
         set newpage 1
      }
      if { $cct(defbri) != $bright } { 
         set bright $cct(defbri)
         set newpage 1
      }
      if { $cct(defcon) != $contrast } { 
         set contrast $cct(defcon)
         set newpage 1
      }
   }
   set first 1
#
#  Save current viewport 
#
   viewport
   foreach item [array names viewport] {
      set vpsave($item) $viewport($item)
   }

   set svstate $state
   foreach vp $vpall {

     foreach curstate $plotary($vp) {

      variable $curstate

      set state $curstate

      if { ![set ${curstate}(valid)] } { continue }

      set mapid [set ${curstate}(mapid)]
      set wcsid [set ${curstate}(wcsid)]
      set trfstate [set ${curstate}(trfstate)]
      set xmin [set ${curstate}(xmin)]
      set xmax [set ${curstate}(xmax)]
      set ymin [set ${curstate}(ymin)]
      set ymax [set ${curstate}(ymax)]
      set jrnlst [set ${curstate}(jrnlst)]
      set lvls [set ${curstate}(levels)]

      if { $first } { 
#
#        SPECIAL CONSIDERATIONS FOR VIEWPORT REFRESHING
#
#        If first state of viewport 1 contains overlay, remove it
#
         if { $vp == 1 } {
            set cmd0 [lindex $jrnlst 0]
            set iover [lsearch -regexp $cmd0 {^[oO]}]
            if { $iover >= 0 } {
               set jrnlst [lreplace $jrnlst 0 0 [lreplace $cmd0 $iover $iover]]
            }
         } elseif { $vp > 1 } {
#
#        If first viewport > 1 add pgeras to avoid overwriting images
#
            pgeras
         }
         if { $newpage } { pgpage }
         set first 0
      }
      
      set vpfile [set ${curstate}(vpfile)]
      if { $vpfile == "" } {
         set vplist [set ${curstate}(vplist)]
         set v1 [lindex $vplist 0]
         set v2 [lindex $vplist 1]
         set v3 [lindex $vplist 2]
         set v4 [lindex $vplist 3]
         viewport v1=$v1 v2=$v2 v3=$v3 v4=$v4
      } else {
         viewport number=[set ${curstate}(vpnum)] $vpfile
      }

      map set $mapid

      if { $journal } {
         set journal 0
         foreach jcmd $jrnlst {
#           puts $jcmd
            set cmdlist $jcmd
            set cmd [lindex $cmdlist 0]
            if { [regexp {^pimage} $cmd] } {
               levels list=$lvls
               lappend cmdlist xmin=$xmin
               lappend cmdlist xmax=$xmax
               lappend cmdlist ymin=$ymin
               lappend cmdlist ymax=$ymax
               lappend cmdlist refresh
               eval $cmdlist
            } elseif { [regexp {^pcontour} $cmd] } {
               levels list=$lvls
               if { $trfstate != "" && [lsearch $cmdlist trf*] == -1 } {
                  lappend cmdlist fromwcs=$wcsid towcs=[set ${trfstate}(wcsid)]
               } else {
                  lappend cmdlist xmin=$xmin
                  lappend cmdlist xmax=$xmax
                  lappend cmdlist ymin=$ymin
                  lappend cmdlist ymax=$ymax
               }
               lappend cmdlist refresh
               eval $cmdlist
#
#              Ensure proper display map is set in overlay case
#              for grid, scale, etc.
#
               if { $trfstate != "" } {
                  setdismap [set ${trfstate}(mapid)] 
               }
            } elseif { [regexp {^surface} $cmd] } {
               lappend cmdlist xmin=$xmin
               lappend cmdlist xmax=$xmax
               lappend cmdlist ymin=$ymin
               lappend cmdlist ymax=$ymax
               lappend cmdlist refresh
               eval $cmdlist
            } elseif { [regexp {^label} $cmd] } {
#              lappend cmdlist clip
               eval $cmdlist
            } else {
               eval $cmdlist
            }
         }
         set journal 1
      }
      updtwin $curstate
     }
   }
   set state $svstate
#
#  Restore viewport
#
   if { $vpsave(file) == "" } {
      set v1 [lindex $vpsave(list) 0]
      set v2 [lindex $vpsave(list) 1]
      set v3 [lindex $vpsave(list) 2]
      set v4 [lindex $vpsave(list) 3]
      viewport v1=$v1 v2=$v2 v3=$v3 v4=$v4
   } else {
      viewport number=$vpsave(next)] $vpsave(file)
   }
#
#  Update zoom label
#
#  set winzm [set ${state}(winzm)]

#  set zmstr [format "%.1f" $winzm]
#  if [regexp {^([0-9]+)\.0$} $zmstr full int] {
#     set zmstr $int
#  }
#  $zmlbl configure -text "Zoom: $zmstr"
   switchvp [set ${state}(vpnum)]
#
#  Re-issue cursor command to maintain proper anchor point
#  for current viewable area
#
   if { $curscmd != {} } { eval $curscmd }

   eval $cleanup
#  update idletasks
}

proc replot {} {
#
#  Revert to initial plot
#
   variable state
   variable $state

   if { ![set ${state}(valid)] } { return }

   set ${state}(xmin) [set ${state}(xmin0)]
   set ${state}(xmax) [set ${state}(xmax0)]
   set ${state}(ymin) [set ${state}(ymin0)]
   set ${state}(ymax) [set ${state}(ymax0)]
   set ${state}(winzm) 1
   refresh
}

#  Output dialog -> Printing, saving as ps or gif

proc outputdialog { } {
#
#  Print... option
#
   variable prgname
   variable DWP
   variable prtype
   variable def_fname
   variable def_gname
   variable prcmd
   variable prcmd_fname
   variable prland

#
#  Raise if already up
#
   if { [lsearch [winfo children .] ${DWP}out] >= 0 } {
      tk::raise ${DWP}out
      return
   }
#
#  Create dialog
#
   tk::toplevel ${DWP}out -colormap .ximage
   tk::wm title ${DWP}out "$prgname: Print"
   tk::bind ${DWP}out <<CloseWindow>> "tk::destroy ${DWP}"
    
   set prtype "Print Command:"
#prland - orientation is 0-portrait 1-landscape
   set prland 1
   set prcolor 1
   set prcmd_fname "lpr"
   set prcmd "lpr"
   set def_fname "pgplot.ps"
   set def_gname "pgplot.gif"
   set prsavct {}
#
#  Top frame - Printer/File name entry
#
   set f [tk::frame ${DWP}out.topfr]

   tk::grid [
      tk::label $f.olabel \
         -anchor e -text "Print To:"
# -width 16
   ] -row 0 -column 0 -sticky ew

   tk::grid [
      tk::radiobutton $f.pbutton \
         -text Printer -value {Print Command:} \
         -variable pgtk::prtype -command {set pgtk::prcmd_fname $pgtk::prcmd}
   ] -row 0 -column 1 

   tk::grid [
      tk::radiobutton $f.fbutton \
         -text "PS File" -value {PS File:} -variable pgtk::prtype \
         -command {set pgtk::prcmd_fname $pgtk::def_fname}
   ] -row 0 -column 2 

   tk::grid [
      tk::radiobutton $f.gbutton \
         -text "GIF File" -value {GIF File:} -variable pgtk::prtype \
         -command {set pgtk::prcmd_fname $pgtk::def_gname}
   ] -row 0 -column 3 

   tk::grid [
      tk::label $f.ocomlabel \
         -anchor e -textvariable pgtk::prtype -width 14
   ] -row 1 -column 0 -sticky ew

   tk::grid [
      tk::entry ${DWP}out.topfr.prcmdentry \
         -textvariable pgtk::prcmd_fname
   ] -row 1 -column 1 -columnspan 3 -sticky ew

   tk::grid $f -row 0 -column 0 -padx 6 -pady 6
#
#  Separator
#
   tk::grid [
      tk::frame ${DWP}out.sep1 -height 2 -borderwidth 1 -relief sunken
   ] -row 1 -column 0 -sticky ew -padx 6 -pady 6
#
#  Middle frame - Output options
#
   set f [tk::frame ${DWP}out.midfr]

   tk::grid [
      tk::label $f.orlabel \
         -anchor e -text Orientation: -justify right
# -width 16
   ] -row 0 -column 0 -sticky e

   tk::grid [
      tk::radiobutton $f.porbutton \
         -text Portrait -value 0 -variable pgtk::prland
   ] -row 0 -column 1 -sticky w

   tk::grid [
      tk::radiobutton $f.landbutton \
         -text Landscape -value 1 -variable pgtk::prland
   ] -row 0 -column 2 -sticky w

   tk::grid [
      tk::label $f.collabel \
         -anchor e -text Print: -justify right
   ] -row 1 -column 0 -sticky e

   tk::grid [
      tk::radiobutton $f.greybutton \
         -text Greyscale -value 0 -variable pgtk::prcolor \
         -command pgtk::greyct
   ] -row 1 -column 1 -sticky w

   tk::grid [
      tk::radiobutton $f.colorbutton \
         -text Color -value 1 -variable pgtk::prcolor \
         -command pgtk::revertct
   ] -row 1 -column 2 -sticky w

   tk::grid $f -row 2 -column 0 -padx 6 -pady 6
#
#  Separator
#
   tk::grid [
      tk::frame ${DWP}out.sep2 -height 2 -borderwidth 1 -relief sunken
   ] -row 3 -column 0 -sticky ew -padx 6 -pady 6
#
#  Bottom frame - Action buttons
#
   set f [tk::frame ${DWP}out.botfr]

   tk::pack [
      tk::button $f.printbutton \
         -command pgtk::outcmd -text Print -default active
   ] -side left -expand yes

   tk::pack [
      tk::button $f.exitbutton \
         -command "pgtk::revertct;tk::destroy ${DWP}out" -text Cancel
   ] -side left -expand yes

   tk::grid $f -row 4 -column 0 -padx 6 -pady 6 -sticky ew


# Resize behavior management

#  tk::grid rowconfigure ${DWP}out 1 -weight 0 -minsize 30
#  tk::grid rowconfigure ${DWP}out 2 -weight 0 -minsize 30
#  tk::grid rowconfigure ${DWP}out 3 -weight 0 -minsize 50
#  tk::grid columnconfigure ${DWP}out 1 -weight 0 -minsize 30
#  tk::grid columnconfigure ${DWP}out 2 -weight 0 -minsize 30
#  tk::grid columnconfigure ${DWP}out 3 -weight 0 -minsize 15

#  tk::grid rowconfigure ${DWP}out.frame 1 -weight 0 -minsize 30
#  tk::grid columnconfigure ${DWP}out.frame 1 -weight 0 -minsize 30
#  tk::grid columnconfigure ${DWP}out.frame 2 -weight 1 -minsize 30
#  tk::grid columnconfigure ${DWP}out.frame 3 -weight 0 -minsize 96

   ${DWP}out.topfr.fbutton invoke
   ${DWP}out.midfr.landbutton invoke
   ${DWP}out.midfr.colorbutton invoke

   tk::bind ${DWP}out <Key-Return> "${DWP}out.botfr.printbutton invoke"
}

proc outcmd { } {
#
#  Output to file (optionally fed to printer afterward)
#
   global tchat lchat mainpg
   variable DWP
   variable prcmd_fname
   variable prcolor
   variable prland
   variable prtype
   variable prsavct

   variable state
   variable $state

   if { ![set ${state}(valid)] } { 
      error "Displayed plot's data is no longer available"
   }

   set xmin [set ${state}(xmin)]
   set xmax [set ${state}(xmax)]
   set ymin [set ${state}(ymin)]
   set ymax [set ${state}(ymax)]

   set cleanup "chat $tchat $lchat"
   chat 5 5

   update idletasks
   if [regexp -nocase "^gif" $prtype] {
      set device "gif"
   } else {
      set device "ps"
      if { $prcolor } { set device "c$device" }
   }
   if { !$prland } { set device "v$device" }
   
   if [regexp -nocase "^print" $prtype] {
      set fname "pgplot.ps"
   } else {
      set fname $prcmd_fname
   }

   cpd leaveopen $fname/$device
   refresh
   cpd $mainpg/xtk

   if [regexp -nocase "^print" $prtype] {
      syscall $prcmd_fname pgplot.ps
   }
      
   if { !$prcolor } { revertct }
   tk::destroy ${DWP}out

   eval $cleanup
}


#  Screen grab dialog

proc dumpdialog { } {
#
#  Screen Grab... option
#
   variable prgname
   variable DWP
   variable dumptype "xv"
   variable dumpfile "pgplot.gif"
#
#  Raise if already up
#
   if { [lsearch [winfo children .] ${DWP}dump] >= 0 } {
      tk::raise ${DWP}dump
      return
   }
#
#  Create dialog
#
   tk::toplevel ${DWP}dump -colormap .ximage
   tk::wm title ${DWP}dump "$prgname: Screen Grab"
   tk::bind ${DWP}dump <<CloseWindow>> "tk::destroy ${DWP}"
#
#  Top frame - Image editor/filename entry
#
   set f [tk::frame ${DWP}dump.topfr]

   tk::grid [
      tk::label $f.olabel \
         -anchor e -text "Image editor:"
   ] -row 0 -column 0 -sticky ew

   tk::grid [
      tk::radiobutton $f.xvbutton \
         -text xv -value xv \
         -variable pgtk::dumptype -command \
         "${DWP}dump.topfr.imentry configure -state disabled;
          ${DWP}dump.topfr.imentry configure -foreground gray60;
          ${DWP}dump.topfr.imlabel configure -state disabled"
   ] -row 0 -column 1 

   tk::grid [
      tk::radiobutton $f.imbutton \
         -text "ImageMagick convert" -value convert -variable pgtk::dumptype \
         -command \
         "${DWP}dump.topfr.imentry configure -state normal;
          ${DWP}dump.topfr.imentry configure -foreground black;
          ${DWP}dump.topfr.imlabel configure -state normal"
   ] -row 0 -column 2 

   tk::grid [
      tk::label $f.imlabel \
         -anchor e -text "Image file:" -width 14 \
         -disabledforeground "" -state disabled
   ] -row 1 -column 0 -sticky ew

   tk::grid [
      tk::entry ${DWP}dump.topfr.imentry \
         -textvariable pgtk::dumpfile
   ] -row 1 -column 1 -columnspan 2 -sticky ew

   tk::pack $f -side top -expand yes -padx 6 -pady 6 -fill both
#
#  Separator
#
   tk::pack [
      tk::frame ${DWP}dump.sep1 -height 2 -borderwidth 1 -relief sunken
   ] -side top -expand yes -padx 6 -pady 6 -fill x
#
#  Bottom frame - Action buttons
#
   set f [tk::frame ${DWP}dump.botfr]

   tk::pack [
      tk::button $f.dumpbutton \
         -command "tk::destroy ${DWP}dump; update; 
                   screengrab \$pgtk::dumptype \$pgtk::dumpfile" \
         -text Grab -default active
   ] -side left -expand yes

   tk::pack [
      tk::button $f.exitbutton \
         -command "tk::destroy ${DWP}dump" -text Cancel
   ] -side left -expand yes

   tk::pack $f -side top -expand yes -padx 6 -pady 6 -fill both

   ${DWP}dump.topfr.xvbutton invoke

   tk::bind ${DWP}dump <Key-Return> "${DWP}dump.botfr.dumpbutton invoke"
#
#  Placement
#
   set x [expr [tk::winfo rootx $DWP]+50]
   set y [expr [tk::winfo rooty $DWP]+50]
   tk::wm geometry ${DWP}dump "+$x+$y"
}

proc askdialog { msg {type question} {blist {OK Cancel}} {defidx 1}} {
#
#  Creates dialog with buttons corresponding to blist
#  If defidx >=0, button corresponding to that index is set as default
#  Sets pgtk::askvalue to index of pressed button
#
   variable prgname
   variable DWP
#
#  Raise if already up
#
   if { [lsearch [winfo children .] ${DWP}ask] >= 0 } {
      tk::destroy ${DWP}ask
   }
   if [regexp -nocase {^q} $type] {
      set typestr "Query"
      set bitmap questhead
   } elseif [regexp -nocase {^e} $type] {
      set typestr "Error"
      set bitmap error
   } elseif [regexp -nocase {^w} $type] {
      set typestr "Warning"
      set bitmap warning
   } else {
      set typestr "Info"
      set bitmap info
   }
#
#  Create dialog
#
   tk::toplevel ${DWP}ask -colormap .ximage
   tk::wm title ${DWP}ask "$prgname: $typestr"
   tk::bind ${DWP}ask <<CloseWindow>> "tk::destroy ${DWP}"
#
#  Top frame - Question message/icon
#
   set f [tk::frame ${DWP}ask.topfr]

   tk::pack [
      tk::label $f.icon -bitmap $bitmap
   ] -side left -expand yes

   tk::pack [
      tk::label $f.msg -text $msg -padx 3
   ] -side right -expand yes

   tk::grid $f -row 0 -column 0 -padx 6 -pady 6
#
#  Separator
#
   tk::grid [tk::frame ${DWP}ask.sep -height 2 -borderwidth 1 -relief sunken
   ] -row 1 -column 0 -sticky ew -padx 6 -pady 6
#
#  Bottom frame - OK button
#
   set f [tk::frame ${DWP}ask.botfr]

   set i 0
   foreach b $blist {
      tk::pack [
         tk::button $f.b$i \
            -command "set pgtk::askvalue $i;tk::destroy ${DWP}ask" -text $b
      ] -side left -expand yes
      incr i
   }

   tk::grid $f -row 2 -column 0 -padx 6 -pady 6 -sticky ew

   if { $defidx >= 0 } {
      $f.b$defidx configure -default active
      tk::bind ${DWP}ask <Key-Return> "${DWP}ask.botfr.b$defidx invoke"
   }

#
#  Placement
#
   set x [expr [tk::winfo rootx $DWP]+50]
   set y [expr [tk::winfo rooty $DWP]+50]
   tk::wm geometry ${DWP}ask "+$x+$y"
#
#  Wait for answer
#
   update idletasks
   tk::grab set ${DWP}ask
   vwait pgtk::askvalue
   tk::grab release ${DWP}ask
}

proc stopcrash {key} {
#
#  Alt-Key is causing Bus error for some reason
#
   if { $key == "p" } {
      outputdialog
   } elseif { $key == "g" } { 
      dumpdialog
   } elseif { $key == "w" } { 
      close_pg_window
#  } elseif { $key == "q" } {
#     exit
   }
   error {}
}

proc journal {cmdlist {add 1}} {
#
#  Journal plotting commands to run upon refresh
#
   variable journal
#
#  if { $add } add command, otherwise delete
#
   variable jrnstate
   variable $jrnstate

   set jrnlst [set ${jrnstate}(jrnlst)]

   if { $cmdlist == {} } {
      set ${jrnstate}(jrnlst) {}
   } elseif { $journal } {
      if { $add } {
         lappend ${jrnstate}(jrnlst) $cmdlist
      } else {
         set i [lsearch $jrnlst [lindex $cmdlist 0]]
         if { $i >= 0 } { 
            set ${jrnstate}(jrnlst) [lreplace $jrnlst $i $i]
         }
      }
   }
}

proc curwcsid {} {
#
#  Return wcsid for current state
#
   variable state
   if { $state != "" } {
      variable $state
      return [set ${state}(wcsid)]
   }
   return {}
}

proc curlevs {} {
#
#  Return current image color levels for current viewport
#
   variable state
   if { $state != "" } {
      variable $state
      if { [set ${state}(cmd)] == "pimage" } {
         return [set ${state}(levels)]
      }
   }
   return {}
}

proc coordvp {vpnum} {
#
#  Return coordinate dependence for vp
#  If nothing is in viewport, return ""
#  Otherwise return wcsid from state which determines coordinates
#
   variable plotary
   variable state
#
#  Flag vpnum=-1 means overlaying.  Use vpnum from current state.
#
   if { $vpnum == -1 } {
      variable $state
      set vpnum [set ${state}(vpnum)]
   }

   if { $vpnum < 1 } {
      txwrite "Invalid vpnum: $vpnum" 5
      error {}
   }

   set i [llength $plotary($vpnum)]
   if { $i <= 1 } {
      return ""
   } else {
      set vpstate [lindex $plotary($vpnum) [expr $i-1]]
      variable $vpstate
      if { [set ${vpstate}(trfstate)] != "" } {
         set vpstate [set ${vpstate}(trfstate)]
         variable $vpstate
      }
   }
   if { $vpstate == "" } { return "" } 
   return [set ${vpstate}(wcsid)]
}

   proc fswitchvp {vpnum} {
#
#  Switch to new viewport for coordinate output and zoom
#     (Special wrapper for calling from FORTRAN, to avoid
#      printing obscure Tcl errors)
#
      if [catch "switchvp $vpnum" errmsg] {
         txwrite " $errmsg" 10
      }
   }

proc switchvp {vpnum} {
#
#  Switch to new viewport for coordinate output and zoom
#
   variable plotary
   variable state
   variable jrnstate
   variable zmlbl

   if { ![info exists plotary($vpnum)] } {
      error "Invalid viewport number: $vpnum"
   }
#
#  Switch to state corresponding to viewport number
#
   set stlist $plotary($vpnum)
   set found 0
   set i [expr [llength $stlist] - 1]  ;# Start with last state
   while { $i >= 0 && !$found } {
      set tmpstate [lindex $stlist $i]
      variable $tmpstate
      if { [set ${tmpstate}(trfstate)] != "" } {
         set state [set ${tmpstate}(trfstate)]
      } else {
         set state $tmpstate
      }
      set jrnstate $state
      set found 1
#
#  Set viewport and window coordinates based on state
#
      variable $state
      setvp [set ${state}(truvp)]
      setwin [set ${state}(truwin)]
      setdismap [set ${state}(mapid)]
#
#  Update zoom label if it exists
#
      if [winfo exists $zmlbl] {
         set winzm [set ${state}(winzm)]
         set zmstr [format "%.1f" $winzm]
         if [regexp {^([0-9]+)\.0$} $zmstr full int] {
            set zmstr $int
         }
         $zmlbl configure -text "Zoom: $zmstr"
      }
      incr i -1
   }
   if { !$found && [llength $stlist] > 1 } {
      error "Viewport $vpnum state not found"
   }
}

proc curvp {} {
#
#  Return current vp number used for coordinate readout
#
   variable state
   if { $state != "" } {
      variable $state
      return [set ${state}(vpnum)]
   } else {
      return 1
   }
}

proc infovp {} {
#
#  Print viewport information to the screen
#
   if [info exists pgtk::plotary] {
      variable plotary
      variable state
      if [info exists pgtk::$state] {
         variable $state
      }
   } else {
      txwrite " No display map" 10
      return
   }

   txwrite " " 10
#
#  Look at current state for vpfile and vpmax
#
   set vpfile [set ${state}(vpfile)]
   set vpmax [set ${state}(vpmax)]
   set vpcur [curvp]
   if { $vpfile != "" } {
      txwrite "  Viewport file: $vpfile" 10
      txwrite " " 10
   }
   set maxwid [string length $vpmax]
   viewport

   for { set vp 1 } { $vp <= $vpmax } { incr vp } {
      set ptr ""
      if { $vp == $vpcur } {
         set ptr "  C->"
      }
      if { $vpfile == $viewport(file) && $vp == $viewport(next) } {
         if { $ptr == "" } {
            set ptr "  N->"
         } else {
            set ptr " CN->"
         }
      }
      set line "[format %5s $ptr] [format %${maxwid}d $vp]"
      if [info exists plotary($vp)] {
         foreach curstate $plotary($vp) {
            set cmd [set pgtk::${curstate}(cmd)]
            switch $cmd {
               pimage   {set type display}
               pcontour {set type contour}
               surface  {set type surface}
               default  {set type unknown}
            }
            if { [set pgtk::${curstate}(valid)] } {
               set mapid [set pgtk::${curstate}(mapid)]
            } else {
               set mapid "*"
            }
            set line "$line ${type}(${mapid})"
        }
     }
     txwrite $line 10
   }
   txwrite " " 10
   txwrite " C = Current viewport   N = Next viewport" 10
   txwrite " * = Map unavailable" 10
   txwrite " " 10
}

proc plotvp {vpnum vplist} {
#
#  Plot viewport and label given in normalized device coords
#
   variable vphgt
   variable vpwid
   variable vpcanvas

   if { $vpnum <= 0 } { return }

   set v1 [lindex $vplist 0]
   set v2 [lindex $vplist 1]
   set v3 [lindex $vplist 2]
   set v4 [lindex $vplist 3]

   set x1 [expr $v1*$vpwid]
   set x2 [expr $v2*$vpwid]
   set y1 [expr (1.-$v3)*$vphgt]
   set y2 [expr (1.-$v4)*$vphgt]
   set xcen [expr ($x1+$x2)/2.]
   set ycen [expr ($y1+$y2)/2.]
   
#
#  Make all boxes black except current
#
   $vpcanvas itemconfigure bxall -outline black
   $vpcanvas itemconfigure lball -fill black
   $vpcanvas create rectangle $x1 $y1 $x2 $y2 -outline blue \
             -fill [$vpcanvas cget -bg] -tags "vp${vpnum} bx${vpnum} bxall"
   $vpcanvas create text $xcen $ycen -text $vpnum -fill blue \
                         -tags "vp${vpnum} lb${vpnum} lball"
#
#  Bind box and label to switch viewports
#
   $vpcanvas bind vp${vpnum} <Button-1> \
      "viewport switch num=$vpnum; 
       $vpcanvas itemconfigure bxall -outline black;
       $vpcanvas itemconfigure lball -fill black;
       $vpcanvas itemconfigure bx${vpnum} -outline blue;
       $vpcanvas itemconfigure lb${vpnum} -fill blue"
}

proc updtwin {{instate ""}} {
#
#  Update input state with current window and vp settings
#
   if { $instate == "" } {
      variable state
      set upstate $state
   } else {
      set upstate $instate
   }
   variable $upstate

   set ${upstate}(truvp) [getvp]
   set ${upstate}(truwin) [getwin]
}

proc expire {mapid} {
#
#  Expire a map
#
   global tchat lchat
   variable state
   variable plotidx

   set cleanup "chat $tchat $lchat"
   if { ![info exists ::debug] } { chat 5 5 }
#
#  Go through all the states and invalidate the ones using mapid
#  as data, unless a copy exists in MAPCOPY in the header.
#  In that case, set the first copy as the new mapid in the state 
#
   for { set i 0 } { $i <= $plotidx } { incr i } {
      set locst "state$i"
      if { ![info exists pgtk::$locst] } { continue }
      variable $locst

      if [regexp -nocase $mapid [set ${locst}(mapid)]] {
         set copylst [chheader map=$mapid key=mapcopy]
         set idx [lsearch -exact $copylst $mapid]
         if { $idx < 0 } {
            set ${locst}(valid) 0
         } else {
            set copylst [lreplace $copylst $idx $idx]
            if { [llength $copylst] == 0 } {
               set ${locst}(valid) 0
            } else {
               set ${locst}(mapid) [lindex $copylst 0]
               txwrite " Use copy ${locst}: $mapid -> [lindex $copylst 0]" 20
            }
         }
      }
   }
#
#  Update display map
#
   if { $state != "" } {
      setdismap [set ${state}(mapid)]
   }
#
#  Remove all references to input map from mapcopy in header
#
   set copylst [chheader map=$mapid key=mapcopy]
   set idx [lsearch -exact $copylst $mapid]
   if { $idx < 0 } { 
      eval $cleanup
      return 
   }
   set copylst [lreplace $copylst $idx $idx]
   foreach map $copylst {
      chheader map=$map key=mapcopy val=$copylst
   }
   chheader map=$mapid key=mapcopy val=""
   eval $cleanup
}

proc upcopy {frommap tomap} {
#
#  Update copy information (MAPCOPY in header)
#
#  If no existing copies, maps involved in copy are added
#
   global tchat lchat

   set cleanup "chat $tchat $lchat"
   if { ![info exists ::debug] } { chat 5 5 }
   
   if { [chheader map=$frommap key=mapcopy] == "" } {
      set fromlst $frommap
   } else {
      set fromlst [chheader map=$frommap key=mapcopy]
   }
#
#  Add tomap to mapcopy list of frommap, and set mapcopy
#  list of tomap to same
#
   lappend fromlst $tomap

   set last ""
   set copylst {}
   foreach item $fromlst {
      if { $last == "" || [string compare $item $last] != 0 } {
         lappend copylst $item
         set last $item
      }
   }

   if { [string length [join $copylst]] > 70 } {
      txwrite " MAPCOPY keyword length exceeded, truncating..." 5
      set copylst [lreplace $copylst end end]
   }
   
   foreach map $copylst {
      chheader map=$map key=mapcopy val=$copylst
   }
   eval $cleanup
}

}  ;# End of namespace pgtk
