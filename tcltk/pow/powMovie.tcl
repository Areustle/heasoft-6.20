#########################################################
#   All the routines relating to Animation...
#        Blink Graphs
#        Blink Images
#########################################################

proc powMovie { } {
   global powPlotParam movieParam powbg currgn
   global powDWP
   global g_titleFont

   if { $powPlotParam(images,$currgn)=="NULL" } {
      tk_messageBox -icon warning \
	    -message "Select a graph with images first." \
	    -parent .pow -title "Blink Warning" -type ok
      return
   }

   if {[winfo exists ${powDWP}movie]} {destroy ${powDWP}movie}
   powToplevel ${powDWP}movie .pow "-bg $powbg"
   bind ${powDWP}movie <<CloseWindow>> "powMovieExitDlg"
   catch {wm title ${powDWP}movie "Blink Images"}


   powMovieLoadInfo

   set movieParam(playing)   0
   set movieParam(speed)    11
   set movieParam(all)       1
   set movieParam(loop)      1
   set movieParam(direction) 1
   set movieParam(list)     ""
   set movieParam(update)    0

   label ${powDWP}movie.title -textvariable movieParam(title) \
	 -bg yellow -fg black -font g_titleFont
   frame ${powDWP}movie.step -bg $powbg
   button ${powDWP}movie.step.forward  -text "Next" -command {powMovieNext 1} \
	 -bg $powbg -font g_titleFont
   button ${powDWP}movie.step.addtolist -text "Add to list" \
	 -command {powMovieAddtoList} -bg $powbg -font g_titleFont
   button ${powDWP}movie.step.backward -text "Prev" -command {powMovieNext -1} \
	 -bg $powbg -font g_titleFont
   button ${powDWP}movie.step.help -text "Help" \
	 -command {powHelp Blinking.html} \
	 -bg $powbg -font g_titleFont
   pack ${powDWP}movie.step.addtolist -in ${powDWP}movie.step -side left -padx 7
   pack ${powDWP}movie.step.backward  -in ${powDWP}movie.step -side left
   pack ${powDWP}movie.step.forward   -in ${powDWP}movie.step -side left
   pack ${powDWP}movie.step.help      -in ${powDWP}movie.step -side left \
	 -padx 12

   label ${powDWP}movie.play -text "Play:" -bg $powbg -fg black -font g_titleFont
   radiobutton ${powDWP}movie.all -text "All Frames" -variable movieParam(all) \
	 -value 1 -bg $powbg -highlightthickness 0 \
	 -command {set movieParam(update) 1} -font g_titleFont
   frame ${powDWP}movie.list -bg $powbg
   radiobutton ${powDWP}movie.list.lab -text "List " -variable movieParam(all) \
	 -value 0 -bg $powbg -highlightthickness 0 \
	 -command {set movieParam(update) 1} -font g_titleFont
   entry ${powDWP}movie.list.txt -width 15 -bg $powbg \
	 -textvariable movieParam(list) -font g_titleFont
   pack ${powDWP}movie.list.lab -in ${powDWP}movie.list -side left
   pack ${powDWP}movie.list.txt -in ${powDWP}movie.list -side left \
	 -expand 1 -fill x

   label ${powDWP}movie.speed -text "Speed:" -bg $powbg -fg black -font g_titleFont
   frame ${powDWP}movie.speedslide -bg $powbg
   label ${powDWP}movie.speedslide.fast -text "Fast" -bg $powbg -font g_titleFont
   label ${powDWP}movie.speedslide.slow -text "Slow" -bg $powbg -font g_titleFont
   scale ${powDWP}movie.speedslide.slide -from 0 -to 20 \
	 -orient horizontal -variable movieParam(speed) \
	 -highlightbackground $powbg -bg $powbg \
	 -showvalue 0 -font g_titleFont
   pack ${powDWP}movie.speedslide.fast -in ${powDWP}movie.speedslide -side left
   pack ${powDWP}movie.speedslide.slide -in ${powDWP}movie.speedslide \
	 -side left -expand 1 -fill x
   pack ${powDWP}movie.speedslide.slow -in ${powDWP}movie.speedslide -side right
   
   checkbutton ${powDWP}movie.loop -variable movieParam(loop) -text Loop \
	 -bg $powbg -highlightthickness 0 -font g_titleFont
   checkbutton ${powDWP}movie.reverse -variable movieParam(direction) \
	 -text Reverse -bg $powbg -highlightthickness 0 \
	 -offvalue 1 -onvalue -1 -font g_titleFont

   frame ${powDWP}movie.buttons -bg $powbg
   button ${powDWP}movie.buttons.reload -text "Reload Info" -bg $powbg \
	 -command {powMovieLoadInfo} -font g_titleFont
   button ${powDWP}movie.buttons.play -text "Blink Images" -bg $powbg \
	 -command {powPlayMovie} -width 12 -font g_titleFont
   button ${powDWP}movie.buttons.exit -text "Exit" -bg $powbg \
	 -command {powMovieExitDlg} -font g_titleFont
   pack ${powDWP}movie.buttons.reload -in ${powDWP}movie.buttons -side left \
	 -padx 4 -pady 3 
   pack ${powDWP}movie.buttons.play -in ${powDWP}movie.buttons -side left \
	 -padx 4 -pady 3
   pack ${powDWP}movie.buttons.exit -in ${powDWP}movie.buttons -side left \
	 -padx 4 -pady 3
    
   grid ${powDWP}movie.title -in ${powDWP}movie -row 0 -column 0 -columnspan 3
   grid ${powDWP}movie.step -in ${powDWP}movie -row 1 -column 0 -columnspan 3 \
	 -pady 5

   grid ${powDWP}movie.play -in ${powDWP}movie -row 2 -column 0 -sticky e
   grid ${powDWP}movie.all -in ${powDWP}movie -row 2 -column 1 -sticky w
   grid ${powDWP}movie.loop -in ${powDWP}movie -row 2 -column 2 -sticky w \
	 -padx 10
   grid ${powDWP}movie.list -in ${powDWP}movie -row 3 -column 1 -sticky ew
   grid ${powDWP}movie.reverse -in ${powDWP}movie -row 3 -column 2 -sticky w \
	 -padx 10

   grid ${powDWP}movie.speed -in ${powDWP}movie -row 4 -column 0 -sticky e \
	 -pady 3
   grid ${powDWP}movie.speedslide -in ${powDWP}movie -row 4 -column 1 \
	 -sticky ew -pady 3

   grid ${powDWP}movie.buttons -in ${powDWP}movie -row 5 -column 0 \
	 -columnspan 3 -pady 8

   grid columnconfigure ${powDWP}movie 1 -weight 1
   grid rowconfigure ${powDWP}movie 0 -weight 1
   grid rowconfigure ${powDWP}movie 1 -weight 1
   grid rowconfigure ${powDWP}movie 5 -weight 1
}

proc powMovieExitDlg { } {
    global movieParam powDWP
    set movieParam(playing) 0
    destroy ${powDWP}movie
}


proc powMovieLoadInfo { } {
   global movieParam currgn powPlotParam

   set movieParam(gn) $currgn

   set images $powPlotParam(images,$currgn)
   if { [lindex $images 0] == "NULL" } {
      set nFrames 0
      set fNum 0
   } else {
      set nFrames [llength $images]
      set img [powMovieGetTopImg]
      if { $img == "" } {
	 set fNum 0
      } else {
	 set fNum [lsearch -exact $images $img]
	 incr fNum
      }
   }

   set movieParam(nframes) $nFrames
   set movieParam(title) "$currgn: $fNum of $nFrames frames"
}

proc powMovieGetTopImg { } {
   global movieParam

   set gn $movieParam(gn)
   set visFrames [.pow.pow find withtag disp$gn]
   if { [llength $visFrames]==0 } {
      set img ""
   } else {
      set top [lindex $visFrames end]
      regexp "(\[^ \]+)disp$gn" [.pow.pow gettags $top] tag img
   }
   return $img
}

proc powMovieNext { step } {
   global movieParam currgn powPlotParam
   
   set gn $movieParam(gn)
   set images $powPlotParam(images,$gn)
   set img [powMovieGetTopImg]

   if { [lindex $images 0] == "NULL" || $img=="" } return

   set fNum [lsearch -exact $images $img]
   if { $fNum < 0 } {
      puts "Something went wrong!  $img not in $images"
      return
   }

   # Locate next displayed image

   set nframes [llength $images]
   while { 1 } {
      incr fNum $step
      if { $fNum >= $nframes } {
	 set fNum 0
      } elseif { $fNum < 0 } {
	 set fNum [expr $nframes-1]
      }
      set newimg [lindex $images $fNum]
      set newid [.pow.pow find withtag ${newimg}disp${gn}]
      if { $newid != "" } break
   }

   .pow.pow raise $newid ${img}disp${gn}
   set movieParam(title) "$gn: [expr $fNum+1] of $nframes frames"

   incr movieParam(Fidx) $movieParam(direction)
   if { $gn == $currgn } {
      powSelectImage $gn $newimg
   }
}

proc powMovieAddtoList { } {
   global movieParam powPlotParam

   set gn $movieParam(gn)
   set fNum [lsearch -exact $powPlotParam(images,$gn) [powMovieGetTopImg] ]
   incr fNum

   if {$movieParam(list)==""} {set movieParam(list) $fNum} \
	 else {set movieParam(list) "$movieParam(list),$fNum"}
   set movieParam(all) 0
}

proc powPlayMovie { } {
   global movieParam powPlotParam
   global powDWP

   set gn $movieParam(gn)
   set images $powPlotParam(images,$gn)
   if { [lindex $images 0] == "NULL" } return

   set nframes [llength $images]

   set movieParam(playing) 1
   if { [winfo exists ${powDWP}movie] } {
      ${powDWP}movie.buttons.play configure -text "Stop Movie" \
	    -command {set movieParam(playing) 0}
   }
   
   set movieParam(expdlist) [powUpdateMovieList $nframes]
   set ndisp [llength $movieParam(expdlist)]
   if {$movieParam(direction)>0} { 
      if ![info exists movieParam(Fidx)] {
        set movieParam(Fidx) 0
      }
   } else {
      if ![info exists movieParam(Fidx)] {
        set movieParam(Fidx) [expr $ndisp - 1]
      }
   }

   powPlayNextFrame
}

proc powPlayNextFrame { } {
   global movieParam powPlotParam
   global powDWP currimg

   set gn $movieParam(gn)
   set images $powPlotParam(images,$gn)
   if { [lindex $images 0] == "NULL" } return

   set nframes [llength $images]

   if { $movieParam(update) } {
      set movieParam(expdlist) [powUpdateMovieList $nframes]
   }
   set ndisp [llength $movieParam(expdlist)]

   set nextframe [expr [lindex $movieParam(expdlist) $movieParam(Fidx)]-1]
   if {$nextframe<$nframes && $nextframe>=0} {
      set img [lindex $images $nextframe]
      set currimg $img
      set next ${img}disp$gn
      if { [winfo exists .pow.pow] && [.pow.pow find withtag $next] != "" } {
         powSelectImage $gn $img
         set movieParam(title) \
               "$gn: [expr $nextframe+1] of $movieParam(nframes) frames"
      }
   }

   incr movieParam(Fidx) $movieParam(direction)
   if {$movieParam(Fidx)<0} {
      set movieParam(Fidx) [expr $ndisp-1]
      if {!$movieParam(loop)} { set movieParam(playing) 0 }
   } elseif {$movieParam(Fidx)>=$ndisp} {
      set movieParam(Fidx) 0
      if {!$movieParam(loop)} { set movieParam(playing) 0 }
   }

   if { ![winfo exists .pow.pow] } { set movieParam(playing) 0 }

   if { $movieParam(playing) } {

      # Setup delay... top speed ~30 fps
      set speed 33
      for {set j 0} {$j<$movieParam(speed)} {incr j} {
         set speed [expr $speed*1.33]
      }
      set speed [expr int($speed)]
      after $speed powPlayNextFrame

   } else {

      # Check that dialog box is still around before trying to change
      # one of its objects

      if {[winfo exists ${powDWP}movie.buttons.play]} {
         ${powDWP}movie.buttons.play configure -text "Blink Images" \
               -command {powPlayMovie}
      }
   }
   update idletasks
}

proc powUpdateMovieList { nframes } {
   global movieParam

   set expdlist {}
   if {$movieParam(all)} {
      for {set i 1} {$i<=$nframes} {incr i} { lappend expdlist $i }
   } else {
      set list [split $movieParam(list) " ,"]
      while { [llength $list] } {
	 set elem [lindex $list 0]
	 set list [lreplace $list 0 0]
	 if {$elem==""} continue
	 if { [regexp -- - $elem] } {
	    set elem [split $elem "-"]
	    set a1 [lindex $elem 0]
	    set a2 [lindex $elem 1]
	    if { $a1 <= $a2 } {
	       for {set i $a1} {$i<=$a2} {incr i} \
		     {lappend expdlist $i}
	    } else {
	       for {set i $a1} {$i>=$a2} {incr i -1} \
		     {lappend expdlist $i}
	    }
	 } elseif { [expr $elem] > 0 && [expr $elem] <= $nframes} {
	    lappend expdlist [expr $elem]
	 }
      }
   }    
   set movieParam(update) 0
   return $expdlist
}


############################
###     Blink Graphs     ###
############################

proc powBlinkGraphDlg { } {
   global powPlotParam powbg currgn movieParam
   global powDWP
   global g_titleFont
   
   if {[winfo exists ${powDWP}blink]} {destroy ${powDWP}blink}
   powToplevel ${powDWP}blink .pow "-bg $powbg"
   bind ${powDWP}blink <<CloseWindow>> "powBlinkExitDlg"
   catch {wm title ${powDWP}blink "Blink Graphs"}
   
   set movieParam(playing) 0

   listbox ${powDWP}blink.listall -width 20 -height 10 \
	 -selectmode extended -bg $powbg -exportselection 0 -font g_titleFont
   label ${powDWP}blink.titleall -text "Available Graphs:" -fg black -bg yellow -font g_titleFont

   listbox ${powDWP}blink.listblnk -width 20 -height 10 \
	 -selectmode extended -bg $powbg -exportselection 0 -font g_titleFont
   label ${powDWP}blink.titleblnk -text "Blink Order:" -fg black -bg yellow -font g_titleFont
   
   button ${powDWP}blink.help -text "Help" \
	 -command {powHelp Blinking.html} \
	 -bg $powbg -font g_titleFont

   grid ${powDWP}blink.titleall -in ${powDWP}blink -row 1 -column 1 -sticky w \
	 -padx 5 -pady 5
   grid ${powDWP}blink.listall  -in ${powDWP}blink -row 2 -column 1 -sticky news
   grid ${powDWP}blink.help     -in ${powDWP}blink -row 1 -column 2
   grid ${powDWP}blink.titleblnk -in ${powDWP}blink -row 1 -column 3 -sticky w \
	 -padx 5 -pady 5
   grid ${powDWP}blink.listblnk  -in ${powDWP}blink -row 2 -column 3 -sticky news

   frame ${powDWP}blink.grphbtns -bg $powbg
   button ${powDWP}blink.grphbtns.add -text "Add -->" -bg $powbg \
	 -command {powBlinkAdd} -font g_titleFont
   button ${powDWP}blink.grphbtns.insert -text "Insert -->" -bg $powbg \
	 -command {powBlinkInsert} -font g_titleFont
   button ${powDWP}blink.grphbtns.delete -text "Delete <--" -bg $powbg \
	 -command {powBlinkDelete} -font g_titleFont

   frame ${powDWP}blink.grphbtns.shft -bg $powbg
   label ${powDWP}blink.grphbtns.shft.lab -text "Shift\n& Align\nGraphs" \
	 -bg $powbg -font g_titleFont
   canvas ${powDWP}blink.grphbtns.shft.but -width 56 -height 56 -bg $powbg \
	 -highlightthickness 0
   ${powDWP}blink.grphbtns.shft.but create polygon  3 30 18 22 18 38 \
	 -fill grey -outline black -tag "blinkShftLeft btn"
   ${powDWP}blink.grphbtns.shft.but create polygon 30  3 22 18 38 18 \
	 -fill grey -outline black -tag "blinkShftUp btn"
   ${powDWP}blink.grphbtns.shft.but create polygon 57 30 42 22 42 38 \
	 -fill grey -outline black -tag "blinkShftRght btn"
   ${powDWP}blink.grphbtns.shft.but create polygon 30 57 22 42 38 42 \
	 -fill grey -outline black -tag "blinkShftDown btn"
   ${powDWP}blink.grphbtns.shft.but create rectangle 23 23 37 37 \
	 -fill grey -outline black -tag "blinkShftCntr btn"
   pack ${powDWP}blink.grphbtns.shft.lab -in ${powDWP}blink.grphbtns.shft \
	 -side left -padx 5
   pack ${powDWP}blink.grphbtns.shft.but -in ${powDWP}blink.grphbtns.shft \
	 -side left -padx 5

   bind ${powDWP}blink.grphbtns.shft.but <ButtonPress> {powBlinkShift 10}
   bind ${powDWP}blink.grphbtns.shft.but <Shift-ButtonPress> {powBlinkShift 1}
   bind ${powDWP}blink.grphbtns.shft.but <ButtonRelease> {set powShiftLoop 0}
   bind ${powDWP}blink.grphbtns.shft.but <Motion> {powBlinkShiftHilight %x %y}
   bind ${powDWP}blink.grphbtns.shft.but <Leave> {powBlinkShiftHilight %x %y}
   
   pack ${powDWP}blink.grphbtns.add -in ${powDWP}blink.grphbtns -fill x \
	 -padx 5 -pady 2
   pack ${powDWP}blink.grphbtns.insert -in ${powDWP}blink.grphbtns -fill x \
	 -padx 5 -pady 2
   pack ${powDWP}blink.grphbtns.shft  -in ${powDWP}blink.grphbtns \
	 -fill none -padx 5 -pady 2
   pack ${powDWP}blink.grphbtns.delete -in ${powDWP}blink.grphbtns -fill x \
	 -padx 5 -pady 2
   grid ${powDWP}blink.grphbtns -in ${powDWP}blink -row 2 -column 2

   set movieParam(speed) 11
   label ${powDWP}blink.speedfast -text "Speed: Fast" -bg $powbg -font g_titleFont
   label ${powDWP}blink.speedslow -text "Slow" -bg $powbg -font g_titleFont
   scale ${powDWP}blink.speedslide -from 0 -to 20 \
	 -orient horizontal -variable movieParam(speed) \
	 -highlightbackground $powbg -bg $powbg \
	 -showvalue 0  -font g_titleFont
   grid ${powDWP}blink.speedfast -in ${powDWP}blink -row 3 -column 1 \
	 -sticky e -pady 5
   grid ${powDWP}blink.speedslide -in ${powDWP}blink -row 3 -column 2 \
	 -sticky ew -pady 5
   grid ${powDWP}blink.speedslow -in ${powDWP}blink -row 3 -column 3 \
	 -sticky w -pady 5
   
   frame ${powDWP}blink.blnkbtns -bg $powbg
   button ${powDWP}blink.blink -text "Blink Graphs" -bg $powbg \
	 -command {powBlinkBlink} -font g_titleFont
   button ${powDWP}blink.reload -text "Reload Info" -bg $powbg \
	 -command {powBlinkReload} -font g_titleFont
   button ${powDWP}blink.exit -text "Exit"  -bg $powbg \
	 -command {powBlinkExitDlg} -font g_titleFont

   grid ${powDWP}blink.reload -in ${powDWP}blink -row 4 -column 1 -sticky e \
	 -pady 7 -padx 10
   grid ${powDWP}blink.blink -in ${powDWP}blink -row 4 -column 2 -sticky ew \
	 -pady 7
   grid ${powDWP}blink.exit -in ${powDWP}blink -row 4 -column 3 -sticky w \
	 -pady 7 -padx 10

   grid columnconfigure ${powDWP}blink 1 -weight 1
   grid columnconfigure ${powDWP}blink 3 -weight 1
   grid rowconfigure ${powDWP}blink 2 -weight 1

   powBlinkReload
}


proc powBlinkExitDlg { } {
    global movieParam powDWP
    set movieParam(playing) 0
    destroy ${powDWP}blink
}


proc powBlinkReload { } {
   global currgn
   global powDWP

   set graphlist ""
   foreach graph [powListGraphs] {
      if { ![regexp scope\$ $graph] } {lappend graphlist $graph}
   }
   
   ${powDWP}blink.listall delete 0 end
   eval [concat ${powDWP}blink.listall insert end $graphlist]
   ${powDWP}blink.listall selection clear 0 end
   set curridx [lsearch -exact $graphlist $currgn]
   if {$curridx>=0} {${powDWP}blink.listall selection set $curridx} \
   else {${powDWP}blink.listall selection set 0}
}

proc powBlinkBlink { } {
   global movieParam currgn
   global powDWP
   
   ${powDWP}blink.blink configure -text "Stop Blinking" \
	 -command {set movieParam(playing) 0}

   set movieParam(playing) 1
   set movieParam(Fidx) 0

   powBlinkBlinkNext
}

proc powBlinkBlinkNext { } {
   global movieParam currgn
   global powDWP
   
   if { ![winfo exists ${powDWP}blink.listblnk] } {
      set movieParam(playing) 0
   } else {
      set graphlist [${powDWP}blink.listblnk get 0 end]
      set cnt [llength $graphlist]
      if { [winfo exists .pow.pow] } {
         if {$movieParam(Fidx)<$cnt} {
            .pow.pow raise [lindex $graphlist $movieParam(Fidx)]
         }
      } else {
         set movieParam(playing) 0
      }
      incr movieParam(Fidx)
      if {$movieParam(Fidx)>=$cnt} {
         set movieParam(Fidx) 0
      }
   }
   
   if { $movieParam(playing) } {

      # Setup delay... top speed ~30 fps
      set speed 33
      for {set j 0} {$j<$movieParam(speed)} {incr j} {
         set speed [expr $speed*1.33]
      }
      set speed [expr int($speed)]
      after $speed powBlinkBlinkNext

   } else {

      # Check that dialog box is still around before trying to change
      # its objects

      if {[winfo exists ${powDWP}blink]} {
         ${powDWP}blink.blink configure -text "Blink Graphs" \
               -command {powBlinkBlink}
      }
      if { [winfo exists .pow.pow] } {.pow.pow raise $currgn}
   }
   update idletasks
}

proc powBlinkShift { size } {
   global currgn powShiftLoop powShiftCurrBtn
   global powDWP

   set powShiftLoop 1
   set origobj $powShiftCurrBtn
   set firstloop 1

   while {$powShiftLoop} {
      set obj $powShiftCurrBtn
      if {$obj!="" && $obj==$origobj} {
	 set tags [${powDWP}blink.grphbtns.shft.but gettags $obj]
	 regexp "blinkShft(\[^ \]+)" $tags "" direction
	 if {$direction=="Cntr"} {
	    set select [${powDWP}blink.listblnk get 0 end]
	 } else {
	    set select ""
	    foreach idx [${powDWP}blink.listblnk curselection] {
	       lappend select [${powDWP}blink.listblnk get $idx]
	    }
	 }
	 foreach graph $select {
	    set dx 0
	    set dy 0
	    switch -exact $direction {
	       Cntr {
		  if { ![info exists bbox1] } {
		     set cbbox [.pow.pow cget -scrollregion]
		     set bbox1 [.pow.pow bbox $graph]
		     set xloc [expr double( \
			   [lindex $bbox1 0]+[lindex $bbox1 2]) \
			   / [lindex $cbbox 2] / 2.0 ]
		     set yloc [expr double( \
			   [lindex $bbox1 1]+[lindex $bbox1 3]) \
			   / [lindex $cbbox 3] / 2.0 ]
		     set xv [.pow.pow xview]
		     if {$xloc<[lindex $xv 0] || $xloc>[lindex $xv 1]} {
			.pow.pow xview moveto \
			      [expr double([lindex $bbox1 0]-30) \
			      / [lindex $cbbox 2] ]
		     }
		     set yv [.pow.pow yview]
		     if {$yloc<[lindex $yv 0] || $yloc>[lindex $yv 1]} {
			.pow.pow yview moveto \
			      [expr double([lindex $bbox1 1]-30) \
			      / [lindex $cbbox 3] ]
		     }
		  } else {
		     set bbox2 [.pow.pow bbox $graph]
		     set dx [expr [lindex $bbox1 2]-[lindex $bbox2 2]]
		     set dy [expr [lindex $bbox1 1]-[lindex $bbox2 1]]
		  }
	       }
	       Up   { set dy -$size }
	       Down { set dy  $size }
	       Left { set dx -$size }
	       Rght { set dx  $size }
	    }
	    powMoveGraph $graph $dx $dy
	 }
      }
      if {$firstloop} {
	 for {set delay 0} {$delay<200} {incr delay} {update}
      }
      update
      set firstloop 0
   }
}

proc powBlinkShiftHilight { x y } {
   global powbg powShiftCurrBtn
   global powDWP

   ${powDWP}blink.grphbtns.shft.but itemconfigure btn -fill grey
   set obj [${powDWP}blink.grphbtns.shft.but find overlapping $x $y $x $y]
   if {[regexp btn [${powDWP}blink.grphbtns.shft.but gettags $obj] ]} {
      set powShiftCurrBtn $obj
      ${powDWP}blink.grphbtns.shft.but itemconfigure $obj -fill white
   } else {
      set powShiftCurrBtn ""
   }
}


proc powBlinkAdd { } {
   global powDWP

   set count 0
   set select [${powDWP}blink.listall curselection]
   foreach item $select {
      set additem [${powDWP}blink.listall get $item]
      ${powDWP}blink.listblnk insert end $additem
      incr count
   }
   if {$count>0} {
      set nelem [llength [${powDWP}blink.listblnk get 0 end]]
      ${powDWP}blink.listblnk selection clear 0 end
      ${powDWP}blink.listblnk selection set [expr $nelem-$count] end
   }
}

proc powBlinkInsert { } {
   global powDWP

   set count 0
   set select [${powDWP}blink.listall curselection]
   set insloc [lindex [${powDWP}blink.listblnk curselection] 0]
   if {$insloc==""} {set insloc 0}
   foreach item $select {
      set additem [${powDWP}blink.listall get $item]
      ${powDWP}blink.listblnk insert [expr $insloc+$count] $additem
      incr count
   }
   if {$count>0} {
      ${powDWP}blink.listblnk selection clear 0 end
      ${powDWP}blink.listblnk selection set $insloc [expr $insloc+$count-1]
   }
}

proc powBlinkDelete { } {
   global powDWP
   
   set count 0
   set select [${powDWP}blink.listblnk curselection]
   foreach item $select {
      ${powDWP}blink.listblnk delete [expr $item-$count]
      incr count
   }
   if {$count>0} {
      ${powDWP}blink.listblnk selection set [lindex $select 0]
   }
}
