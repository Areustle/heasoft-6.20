# From canvas coordinates to the pixel and physical coord.

#Update the label in the Option box
proc UpdateRuler {obj} { 
    global currimg
    global currgn
    global powDWP

    set xruler ${powDWP}xruler

    if ![info exists currimg] {
       return
    }


    foreach [list tx0 ty0 tx1 ty1] [$obj getGraphCoords] {}
    set e0 [powGraphToPixel $currimg $tx0 $ty0]
    set e1 [powGraphToPixel $currimg $tx1 $ty1]
    

    foreach [list x0 y0] $e0 {}
    foreach [list x1 y1] $e1 {}


   set dx  [expr $x1 - $x0 ]
   set dy  [expr $y1 - $y0 ]
   set dd  [expr hypot($dx, $dy)]
   set ruler_pix [ format \
      "Image Pixel \n(dX, dY): (%-.6g ,%-.6g)\n Pixel Distance: %-.6g " \
      $dx $dy $dd ]
   $xruler.frame.message.pixel configure -text $ruler_pix

  
   set tx  [expr $tx1 - $tx0 ]
   set ty  [expr $ty1 - $ty0 ] 
   set deg2rad [expr 3.1415926/180.0 ]
   if [powWCSexists $currgn ] { 
#      angular distance 
       set sinx2 [expr sin($tx/2.0*$deg2rad)]
       set siny2 [expr sin($ty/2.0*$deg2rad)]
       set cosy1 [expr cos($ty1*$deg2rad)]
       set cosy0 [expr cos($ty0*$deg2rad)]
       set dd [expr ($siny2*$siny2) + ($cosy1*$cosy0*$sinx2*$sinx2) ]
       set dd  [expr sqrt($dd)]
       if {$dd > 1.0} {
          set dd 1.0
       }
       set dd [expr asin($dd)*2.0/$deg2rad]

#      angle to north
       set a [expr (90 - $ty1)*$deg2rad]
       set b [expr (90 - $ty0)*$deg2rad]
       set c [expr $dd*$deg2rad]
       if {$a == 0.0 || $b == 180.0 || $c == 0.0 } { 
           set angle 0
       } elseif {$a == 180.0 || $b == 0.0 || $c == 180.0 } { 
          set angle 180
       } else {
          set sinb [expr sin($b)]
          set sinc [expr sin($c)]
          set s [expr 0.5 * ($a + $b + $c)]
          set sinsc [expr sin($s - $c)] 
          set sinsb [expr sin($s - $b)]
          set temp2 [expr sqrt(($sinsc/$sinc)*($sinsb/$sinb))]
          if {$temp2 > 1.0} { 
              set temp2 1.0
          }
          set angtmp [expr 2.0*asin($temp2)/$deg2rad] 

          if {$tx >= 0 && $tx < 180} {
             set angle $angtmp 
          } elseif  {$tx >= 180 && $tx < 360 } {
             set angle [expr 360 -  $angtmp ] 
          } elseif  {$tx < 0 && $tx >= -180 } {
             set angle [expr 360.0 - $angtmp ] 
          } elseif  {$tx < -180 && $tx >= -360 } {
             set angle $angtmp  
          } else {
          }
       }

       set ruler_graph [format \
 "Graph Coordinate (deg) \n(dRA, dDec): (%-.6g, %-.6g)\n" $tx $ty ]
       set temp [format \
 "Ang. Distance: %-.6g \nAngle to North: %-.6g \n" \
       $dd $angle ]
       set ruler_graph "${ruler_graph}${temp}"
    } else {
       set dd  [expr hypot($tx, $ty)]
       set angle [expr atan2($ty,$tx)/$deg2rad ]
       set ruler_graph [ format \
 "Graph Coordinate \n(dX, dY): (%-.6g, %-.6g)\n" $tx $ty ]
       set temp [format \
 "Distance: %-.6g \nAngle to X-axis(deg): %-.6g \n" \
       $dd $angle ]
       set ruler_graph "${ruler_graph}${temp}"
    }
    $xruler.frame.message.graph configure -text $ruler_graph

   foreach {cx0 cy0} [powGraphToCanvas $currgn $tx0 $ty0] {}
   foreach {cx1 cy1} [powGraphToCanvas $currgn $tx1 $ty1] {} 
   set temp [list $cx0 $cy0 $cx0 $cy1 $cx1 $cy1]
   .pow.pow delete ruler_line
   .pow.pow create line $temp -fill lightblue -tags ruler_line
}

proc RulerCallback { obj msg } {
    global powDWP

    set xruler ${powDWP}xruler
    if ![winfo exists $xruler ] {
       return
    }

    if {$msg == "shapeIsBeingModified" || $msg == "shapeHasChanged"} {
       UpdateRuler $obj
    }

}

proc OpenRuler { } {
    global rulerRegion
    global currgn
    global currimg
    global powDWP
    global powbg
    global g_titleFont
    global powRotation
    global storePowRotation

    set xruler ${powDWP}xruler

    if [winfo exists $xruler ] {
       focus $xruler
       raise $xruler
       return
    }

    if ![info exists currimg ] {
       tk_messageBox -type ok -icon error \
           -message "Select an image first."
       return
    }


    powToplevel $xruler .pow  "-width 200 -bg $powbg"
    wm title $xruler "Ruler"

    frame $xruler.frame -borderwidth 4  
    frame $xruler.frame.title 
    frame $xruler.frame.message -borderwidth 3 -relief groove 
    label $xruler.frame.title.title -text "Ruler:" -background yellow \
            -relief flat   -width 8 -anchor w -font g_titleFont
    button $xruler.frame.title.help -text Help -anchor e \
            -command {powHelp Ruler.html}  -font g_titleFont

    set ruler_pix "Image Pixel \n(dX, dY): \n  Pixel Distance: " 
    if [powWCSexists $currgn ] { 
      set ruler_graph "Graph Coordinate (deg)\n(dRA, dDEC): \n"
      set ruler_graph "${ruler_graph}Ang. Distance: \nAngle to North:"
    } else {
      set ruler_graph "Graph Coordinate \n(dX, dY): \n"
      set ruler_graph "${ruler_graph}Distance: \nAngle to X-axis:"
    } 
    message $xruler.frame.message.pixel -text $ruler_pix \
         -anchor w -relief flat -width 200 -font g_titleFont
    message $xruler.frame.message.graph -text $ruler_graph \
         -anchor w -relief flat -width 400 -font g_titleFont
    button $xruler.frame.close -text Exit -command {
        global storePowRotation
        global currimg
        global powRotation

        if { [info exists currimg] && [info exists storePowRotation($currimg)] } {
           set powRotation($currimg) $storePowRotation($currimg)
        }
        itcl::delete object $rulerRegion
        .pow.pow delete ruler_line
        destroy  ${powDWP}xruler
    } -font g_titleFont

    pack $xruler.frame.title.title -side left -anchor w
    pack $xruler.frame.title.help  -side right -anchor e 
    pack $xruler.frame.message.pixel -side left -anchor nw
    pack $xruler.frame.message.graph -side left -anchor nw

    pack $xruler.frame.title -anchor w -pady 5 -padx 2 -fill x
    pack $xruler.frame.message -anchor w -pady 2  -padx 2 -fill x
    pack $xruler.frame.close  -expand 1 -anchor w -pady 5 -padx 2
    pack $xruler.frame -fill x

    set ruler_gn $currgn
    set ruler_img $currimg

    set rulerRegion [gRegionList $currgn .pow.pow]
    $rulerRegion setOwner RulerCallback
    $rulerRegion setAllowsMultiple 0
    $rulerRegion setDefault "+" Line

    set temp [powFetchImageInfoHash  $ruler_img ]
    set temp [split $temp ]
    set rulerWidth [lindex $temp 3]
    set rulerHeight [lindex $temp 5]

    set halfx [expr round($rulerWidth*0.5) ]
    set halfy [expr round($rulerHeight*0.5) ]
    set gx0 [expr round($halfx*0.5) + 1]
    set gy0 [expr round($halfy*0.5) + 1]
    set gx1 [expr round($halfx*1.5) + 1]
    set gy1 [expr round($halfy*1.5) + 1]
    set descr [list $gx0 $gy0 $gx1 $gy1]

    if [info exists powRotation($currimg)] {
       set storePowRotation($currimg) $powRotation($currimg)
       catch { unset powRotation($currimg) }
    }

    $rulerRegion addRegion + Line $descr pixels
    set elem [$rulerRegion rgnAtIndex 0]

    set powROIButton 1
    powSaveConfig

    bind $xruler <<CloseWindow>> {
      itcl::delete object rulerRegion
      .pow.pow delete ruler_line
      destroy ${powDWP}xruler
    }

    tkwait window $xruler
}
    
