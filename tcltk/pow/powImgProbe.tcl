proc gImgProbe { args } {
   return [uplevel #0 ImgProbe #auto $args]
}

itcl::class ImgProbe {
   constructor {args} {}
   destructor {} 

   private variable probeRegion
   private variable probeCentX
   private variable probeCentY
   private variable probeStdX
   private variable probeStdY
   private variable probeFlux
   private variable probeNPix 
   private variable probeFormat 
   private variable probeMean
   private variable probeDMean
   private variable probeFile 
   private variable graphx
   private variable graphy
   private variable probeNewFile
   private variable imgprobe

   public method openProbe {} 
   public method updateProbe {} 
   public method setShape { } 
   public method setUnit {}

   public method saveProbe {} 
   public method closeProbeFile {} 
}

itcl::body ImgProbe::constructor {args} {
   global currgn
   global powDWP 

   global powDrawOriginalFlag

   set powDrawOriginalFlag "false"
  
   set probeRegion  [gRegionList $currgn .pow.pow]
   $probeRegion setOwner imgProbeCallback
   $probeRegion setAllowsMultiple 0
   $probeRegion setDefault "+" Circle 


   set probeCentX 0.0
   set probeCentY 0.0
   set probeStdX 0.0
   set probeStdY 0.0
   set probeFlux 0.0
   set probeNPix 0.0 
   set probeMean 0.0
   set probeFormat decimal 
   set probeNewFile 0
   
   set imgprobe ${powDWP}probe 
}

itcl::body ImgProbe::destructor {} { 
   itcl::delete object $probeRegion  
}

itcl::body ImgProbe::setShape { } {

   set probeSelected [$probeRegion rgnAtIndex 0 ]
   if {$probeSelected == ""} {
       return
   }
   set oldshape [$probeSelected getShape ] 
   set descr [$probeSelected getFunction "pixels" ] 
   set rotation [$probeSelected getRotation ] 

   set shape [$imgprobe.options.shape get]
   if {$oldshape == $shape } { 
      return;
   }

   if {$oldshape == "Circle" && $shape != "Circle" } {
      if {$shape == "Box"} { 
         set a [lindex $descr 2]
         set descr [lreplace $descr 2 2 [expr 2.0*$a] ]
      }
      lappend descr [lindex $descr 2]
      lappend descr 0
   }

   if {$oldshape != "Circle" && $shape == "Circle"} { 
      set a [lindex $descr 2]
      set b [lindex $descr 3]
      if {$oldshape == "Box"} { 
         set descr [lreplace $descr 2 2 [expr 0.25*$a + 0.25 * $b ] ]
      } else {
         set descr [lreplace $descr 2 2 [expr 0.5*$a + 0.5 * $b ] ]
      }
      set descr [lrange $descr 0 2 ]
   }

   if {$oldshape == "Ellipse" && $shape == "Box"} { 
         set a [lindex $descr 2]
         set descr [lreplace $descr 2 2 [expr 2.0*$a] ]
         set a [lindex $descr 3]
         set descr [lreplace $descr 3 3 [expr 2.0*$a] ]
   }

   if {$oldshape == "Box" && $shape == "Ellipse"} { 
         set a [lindex $descr 2]
         set descr [lreplace $descr 2 2 [expr 0.5*$a] ]
         set a [lindex $descr 3]
         set descr [lreplace $descr 3 3 [expr 0.5*$a] ]
   }
       
   $probeRegion deleteAll 
   $probeRegion setDefault "+" $shape 
   $probeRegion addRegion + $shape $descr pixels
   updateProbe
} 

itcl::body ImgProbe::setUnit { } {
   set probeFormat [$imgprobe.options.unit get] 
   updateProbe
} 

itcl::body ImgProbe::updateProbe {} {
   global currimg

#   Use SAO Format 

   set probeSelected [$probeRegion rgnAtIndex 0 ]
   if {$probeSelected == ""} {
       return
   }
   set shape [$probeSelected getShape ]  
   set descr [$probeSelected getFunction "pixels" ] 
   
   set results [powGetRegionStatistics $currimg NONE $descr $shape + ]
   set good  [lindex $results 0]
    if {$good == 1} {
        set probeCentX [lindex $results 1]
        set probeCentY [lindex $results 2]
        set probeStdX [lindex $results 3]
        set probeStdY [lindex $results 4]
        set probeFlux [lindex $results 5]
        set probeNPix [lindex $results 6]
        set probeMean [ format "%.10g" [lindex $results 7] ]
        set probeDMean [ format "%.10g" [lindex $results 8] ]  
        set pixel [format "(%.2f, %.2f) +- (%.2f, %.2f)"  \
            $probeCentX  $probeCentY  $probeStdX $probeStdY ] 
        foreach {graphx graphy} [powPixelToGraph $currimg \
            [expr $probeCentX - 1]  [expr $probeCentY - 1] ] {} 
        foreach {gx1 gy1} [powPixelToGraph $currimg \
              [expr $probeCentX - 1 - $probeStdX ]  \
              [expr $probeCentY - 1 - $probeStdY ] ] {} 
        foreach {gx2 gy2} [powPixelToGraph $currimg \
              [expr $probeCentX - 1 + $probeStdX ]  \
              [expr $probeCentY - 1 + $probeStdY ] ] {}  
        set graphdx [expr abs($gx2 - $gx1)/2.0 ]
        set graphdy [expr abs($gy2 - $gy1)/2.0 ]
        if {$probeFormat == "decimal" } { 
	   set graphx [format %.6g $graphx]
	   set graphy [format %.6g $graphy]
	   set graphdx [format %.3g $graphdx]
	   set graphdy [format %.3g $graphdy]
        } else { 
           set graphx [powHourRA $graphx "%02d:%02d:%05.2f"]
           set graphy [powDegDec $graphy]
           set graphdx [powHourRA $graphdx "%02d:%02d:%05.2f"]
           set graphdy [powDegDec $graphdy]
       }    
       set coord "($graphx, $graphy) +- ($graphdx, $graphdy)"  
    } else {
        set probeCentX X
        set probeCentY Y
        set probeStdX ""
        set probeStdY ""
        set probeFlux 0
        set probeNPix 0 
        set pixel  "(X,Y) +- (dX,dY)"  
        set coord  "(X,Y) +- (dX,dY)"  
        set probeMean   0 
        set probeDMean   0 
    } 

#  Update the readout in dialog box. 
   set cen $imgprobe.centroid 
   set childsite [$cen.pixel childsite] 
   $childsite.c  configure -text $pixel 
   set childsite [$cen.coord childsite] 
   $childsite.c  configure -text $coord 

   set sta $imgprobe.info.statistics 
   set childsite [$sta.pixels childsite] 
   $childsite.c  configure -text $probeNPix 
   set childsite [$sta.flux childsite] 
   $childsite.c  configure -text $probeFlux 
   set childsite [$sta.mean childsite] 
   $childsite.c  configure -text "$probeMean +- $probeDMean"

}

itcl::body ImgProbe::saveProbe {} { 
    global currimg
    if {$probeCentX == "X"} return 
 
    if ![info exists probeFile ] {
        set probeFile [tk_getSaveFile -initialfile "pow.stat"] 
        if [file exists $probeFile] {
           file delete -force $probeFile
        }
    }
    if {$probeFile ==  "" } {
        unset probeFile
        return
    }

    set probeNewFile [file exists $probeFile] 
    set fprob [open $probeFile a]
    if {$probeNewFile == 0 } { 
       set probeNewFile 1
       set title \
"Xpix     Ypix     Xgraph       Ygraph       Npix     Flux         Mean"
       puts $fprob $title
    }	
    if {$probeFormat == "decimal" } {
       set temp [ format \
             "%-8.2f %-8.2f %-12.6g %-12.6g %-8.0f %-12.6g %-12.6g" \
            $probeCentX $probeCentY $graphx $graphy $probeNPix $probeFlux \
            $probeMean ]
       puts $fprob $temp 
    } else {
       set temp [ format \
             "%-8.2f %-8.2f %-12s %-12s %-8.0f %-12.6g %-12.6g" \
            $probeCentX $probeCentY $graphx $graphy $probeNPix $probeFlux \
            $probeMean ]
       puts $fprob $temp 
    }
    close $fprob
}

itcl::body ImgProbe::closeProbeFile {} {
  if [info exist probeFile] {
     unset probeFile 
  }
}

itcl::body ImgProbe::openProbe {} {
    global powbg
    global currgn
    global currimg
    global g_titleFont
    global ROIbbox powPlotParam
    global roi_xo roi_yo
    global roi_xn roi_yn
    global roi_pixelxo roi_pixelyo
    global roi_pixelxn roi_pixelyn
    global powImgProbe

    global ROIunits

    powToplevel $imgprobe .pow  "-width 200 -bg $powbg"
    wm title $imgprobe "Image Probe"

   frame $imgprobe.title
   label $imgprobe.title.label -text "Image Probe" -bg yellow -font g_titleFont
   button $imgprobe.title.help -text Help \
    -command {powHelp Probe.html} -font g_titleFont
   pack $imgprobe.title.label -side left
   pack $imgprobe.title.help  -side right
   
   frame $imgprobe.options
   set opt $imgprobe.options 
   iwidgets::optionmenu $opt.shape -labeltext "Probe Shape" \
        -command [itcl::code $this setShape] \
        -font g_titleFont \
        -labelfont g_titleFont
   $opt.shape insert end "Circle" 
   $opt.shape insert end "Ellipse" 
   $opt.shape insert end "Box" 
#   $opt.shape insert end "Polygon"
   iwidgets::optionmenu $opt.unit -labeltext "Coord. Format" \
        -command [itcl::code $this setUnit]  \
        -font g_titleFont \
        -labelfont g_titleFont
   $opt.unit insert end "decimal"
   if [ powWCSexists $currgn ] { 
       $opt.unit insert end "hms"
   }
   pack $opt.shape -side left
   pack $opt.unit  -side right
        

   frame $imgprobe.centroid
   set cen $imgprobe.centroid 
   label $cen.label -text "Centroid:" -anchor w -font g_titleFont
   iwidgets::labeledwidget  $cen.pixel -labeltext "Pixel:" \
                          -labelfont g_titleFont 
   set childsite [$cen.pixel childsite] 
   label $childsite.c  -width 55 -relief sunken  -font g_titleFont
   pack $childsite.c
   iwidgets::labeledwidget  $cen.coord -labeltext "Coordinate:"  \
                          -labelfont g_titleFont 
   set childsite [$cen.coord childsite] 
   label $childsite.c  -width 55 -relief sunken  -font g_titleFont
   pack $childsite.c
   iwidgets::Labeledwidget::alignlabels  $cen.pixel $cen.coord
   pack $cen.label -fill x -anchor w
   pack $cen.pixel -fill x
   pack $cen.coord -fill x

   frame $imgprobe.info 
   frame $imgprobe.info.statistics
   set sta $imgprobe.info.statistics 
   label $sta.label -text "Statistics:" -anchor w -font g_titleFont
   iwidgets::labeledwidget  $sta.pixels -labeltext "N pixels:" \
                          -labelfont g_titleFont
   set childsite [$sta.pixels childsite] 
   label $childsite.c  -width 30 -relief sunken  -font g_titleFont
   pack $childsite.c
   iwidgets::labeledwidget  $sta.flux -labeltext "Total Flux:" \
                          -labelfont g_titleFont
   set childsite [$sta.flux childsite] 
   label $childsite.c  -width 30 -relief sunken  -font g_titleFont
   pack $childsite.c
   iwidgets::labeledwidget  $sta.mean -labeltext "Mean Flux:" \
                          -labelfont g_titleFont
   set childsite [$sta.mean childsite] 
   label $childsite.c  -width 30 -relief sunken  -font g_titleFont
   pack $childsite.c
   iwidgets::Labeledwidget::alignlabels  $sta.pixels $sta.flux $sta.mean
   pack $sta.label -fill x -anchor w
   pack $sta.pixels -fill x
   pack $sta.flux -fill x
   pack $sta.mean -fill x

   frame $imgprobe.info.log
   set sta $imgprobe.info.log
   set cmd $imgprobe.info.log 
   button $cmd.record -text Record -command [itcl::code $this saveProbe] \
        -width 8 -font g_titleFont
   button $cmd.close -text "Close Log" -command [itcl::code $this closeProbeFile] \
        -width 8 -font g_titleFont
   button $cmd.exit -text  Exit  -width 8 -command {
       global powImgProbe
       destroy ${powDWP}probe
       if [info exists powImgProbe] {
          itcl::delete object $powImgProbe 
          unset powImgProbe
       }
   } -font g_titleFont
   pack $cmd.record  -side top  -padx 40
   pack $cmd.close  -side top   -padx 40
   pack $cmd.exit  -side top  -padx 40  

   pack $imgprobe.info.statistics  -side left -anchor nw
   pack $imgprobe.info.log  -side right -anchor nw


   pack $imgprobe.title -side top -fill x -pady 2
   pack $imgprobe.options -side top -fill x -pady 2
   pack $imgprobe.centroid  -side top -anchor nw -pady 2
   pack $imgprobe.info -side top -fill x -pady 2
   
   bind $imgprobe <<CloseWindow>> {
      global powImgProbe
      destroy ${powDWP}probe
      if [info exists powImgProbe] {
         itcl::delete object $powImgProbe
         unset powImgProbe
      }
   }

   bind $imgprobe <Destroy> {
      global powImgProbe
      destroy ${powDWP}probe
      if [info exists powImgProbe] {
         itcl::delete object $powImgProbe
         unset powImgProbe
      }
   }

   set temp [ powFetchImageInfoHash  $currimg ]
   set temp [split $temp ]

   if { $powPlotParam(zoomed,$currimg) == 1 } {
      # image is zoom
      set imgWidth  [expr $roi_pixelxn - $roi_pixelxo]
      set imgHeight [expr $roi_pixelyo - $roi_pixelyn]
   } else {
      set imgWidth [lindex $temp 3]
      set imgHeight [lindex $temp 5]
   }

   set imgMin $imgHeight
   if {$imgMin < $imgHeight} { 
      set $imgMin $imgHeight
   } 

   if { $powPlotParam(zoomed,$currimg) == 1 } {
      set halfx [expr round($imgWidth*0.5) + 1 + round($roi_pixelxo)]
      set halfy [expr round($roi_pixelyo) - round($imgHeight*0.5) + 1]
   } else {
      set halfx [expr round($imgWidth*0.5)+1 ]
      set halfy [expr round($imgHeight*0.5)+1 ]
   }

   set r [expr round($imgMin/10.0)]
   if {$r  < 1 } { 
      set r 1
   }
   set descr [list $halfx $halfy $r ]
   $probeRegion addRegion + Circle $descr pixels

   update idletasks

   scan [winfo geometry .pow] "%dx%d+%d+%d" Pw Ph Px Py

   set width [expr [winfo reqwidth $imgprobe] + 2]

   scan [winfo geometry $imgprobe] "%dx%d+%d+%d" Rw Rh Rx Ry
   catch { wm geometry $imgprobe ${width}x$Rh+[expr $Px + $Pw - 15 ]+$Py } err

   tkwait window $imgprobe
}

proc imgProbeDialog {} {
   global currimg
   global powDWP
   global powImgProbe

   if ![info exists currimg ] {
       tk_messageBox -type ok -icon error \
           -message "Select an image first."
       return
   }

   if [winfo exists ${powDWP}probe ] {
       focus ${powDWP}probe
       raise ${powDWP}probe
       return
   } 
    
   if [winfo exists ${powDWP}probe] {
      catch { destroy ${powDWP}probe 
              itcl::delete object $powImgProbe 
              unset powImgProbe }
   }

   set powImgProbe [gImgProbe]
   $powImgProbe openProbe
}

proc imgProbeCallback  {obj msg} { 
     global powImgProbe
      if {$msg == "shapeIsBeingModified" || $msg == "shapeHasChanged"} {
         $powImgProbe updateProbe
      } 
}
