proc GetPoint_Img {img x y } {
    global profileWidth
    global profileHeight

    if {$img == "NULL"} {
         return NULL
    }

    if {$x >= 0 && $x < $profileWidth &&
        $y >= 0 && $y < $profileHeight } { 
         set imgz [powGetImageZ $img $x $y] 
    } else {
         set imgz 0
    }
    return $imgz
}

proc UpdateProfile {obj} { 
    global powDWP
    global profile_gn
    global profile_img
    global currgn
    global xCount yCount powPlotParam

    set xdlg ${powDWP}xdlg
    foreach [list gx0 gy0 gx1 gy1] [$obj getGraphCoords] {}
    set e0 [powGraphToPixel $profile_img $gx0 $gy0]  
    set e1 [powGraphToPixel $profile_img $gx1 $gy1]   
    foreach [list x0 y0] $e0 {} 
    foreach [list x1 y1] $e1 {} 
    set x0 [expr round($x0 + 1)]
    set y0 [expr round($y0 + 1)]
    set x1 [expr round($x1 + 1)]
    set y1 [expr round($y1 + 1)] 
   
    set width  [image width  $profile_img]
    set height [image height $profile_img]

    if [info exist powPlotParam(flipD,$profile_gn)] {
       switch $powPlotParam(flipD,$profile_gn) {
          "X" {
             if { [info exists xCount($profile_gn)] && [expr $xCount($profile_gn) % 2] != 0 } {
                set x0 [expr $width - $x0 - 1]
                set x1 [expr $width - $x1 - 1]
             }
          }
          "Y" {
             if { [info exists yCount($profile_gn)] && [expr $yCount($profile_gn) % 2] != 0 } {
                set y0 [expr $height - $y0 - 1]
                set y1 [expr $height - $y1 - 1]
             }
          }
       }
    }

    set g0 [format "(%-.8g, %-.8g)" $gx0 $gy0 ] 
    set g1 [format "(%-.8g, %-.8g)" $gx1 $gy1 ] 
    set p0 [format "(%d, %d)" $x0 $y0 ] 
    set p1 [format "(%d, %d)" $x1 $y1 ] 

    $xdlg.frame.row1.pix1 configure -text $p0
    $xdlg.frame.row1.pix2 configure -text $p1

    $xdlg.frame.row2.graph1 configure -text $g0
    $xdlg.frame.row2.graph2 configure -text $g1
}

proc DrawProfile {obj} {
    global profile_gn
    global profile_img 
    global currgn
    global currimg
    global powWCS powFitsHeader powFitsHeaderCnt xCount yCount
    global powWCSList powWCSLabel powWCSName
    global powPlotParam
    global xlist
    global zlist
    global graphCoordList
    
    set graphCoordList {}

    foreach [list gx0 gy0 gx1 gy1] [$obj getGraphCoords] {}
    set e0 [powGraphToPixel $profile_img $gx0 $gy0]  
    set e1 [powGraphToPixel $profile_img $gx1 $gy1]   
    foreach [list x0 y0] $e0 {} 
    foreach [list x1 y1] $e1 {} 
    set x0 [expr round($x0)]
    set y0 [expr round($y0)]
    set x1 [expr round($x1)]
    set y1 [expr round($y1)]
   
    set tx [expr abs($x1-$x0)] 
   
    set tx [expr abs($x1-$x0)] 
    set ty [expr abs($y1-$y0)] 
    if {$tx == 0 && $ty == 0 } { 
       return 
    }
    if {$tx >= $ty } {
        if {$x0 > $x1 } {
          set step -1 
        } else {
          set step 1 
        }
        set k  [expr double($y1-$y0)/double($x1-$x0) ]
        set tpoints [expr (abs($x1-$x0+1))]
        set usex 1
    } else {
        if {$y0 > $y1 } {
          set step -1 
        } else {
          set step 1 
        }
        set k  [expr double($x1-$x0)/double($y1-$y0) ]
        set tpoints [expr abs(($y1-$y0+1))]
        set usex 0
    } 
   
    set xi $x0
    set yi $y0  

    set a [GetPoint_Img $profile_img $xi $yi]
    if {$a != "NULL"} {
         lappend zlist $a  
    } else {
         lappend zlist 0
    }
    set zlist [GetPoint_Img $profile_img $xi $yi]
    lappend graphCoordList [powPixelToGraph $profile_img $xi $yi]
    set xlist 1
    for {set i 1} {$i < $tpoints } {incr i} { 
       if {$usex == 1} {
          incr xi $step
          set yi [expr round($k*($xi-$x0))+$y0]
       } else {
          incr yi $step
          set xi [expr round($k*($yi-$y0))+$x0]
       }    

       lappend xlist [expr $i + 1]
       set a [GetPoint_Img $profile_img $xi $yi]

       lappend graphCoordList [powPixelToGraph $profile_img $xi $yi]

       if {$a != "NULL"} {
         lappend zlist $a  
       } else {
         lappend zlist 0
       }
    } 


    powCreateDataFromList ${profile_gn}_cx_data $xlist 
    set xlabel "pixel index "
    set xunit " pixel"

    set powWCSLabel(xlabel,${profile_gn}_xsec,DEFAULT) $xlabel
    set powWCSLabel(ylabel,${profile_gn}_xsec,DEFAULT) ""
    set powWCSLabel(xunit,${profile_gn}_xsec,DEFAULT) $xunit
    set powWCSLabel(yunit,${profile_gn}_xsec,DEFAULT) ""

    powCreateDataFromList ${profile_gn}_cz_data $zlist 

    powCreateVector ${profile_gn}_cx_vector ${profile_gn}_cx_data 0 NULL NULL
    powCreateVector ${profile_gn}_cz_vector ${profile_gn}_cz_data 0 NULL NULL

    set powWCSName(${profile_gn}_xsec) $powWCSName($profile_gn)
    set powWCSName(${profile_gn}_xsecscope) $powWCSName($profile_gn)
    set powWCSList(${profile_gn}_xsec) $powWCSList($profile_gn)
    set powWCSList(${profile_gn}_xsecscope) $powWCSList($profile_gn)

    powCreateCurve ${profile_gn}_xsec_curve ${profile_gn}_cx_vector \
        NULL ${profile_gn}_cz_vector NULL 

    if ![info exists powPlotParam(xdimdisp,${profile_gn}_xsec)] { 
       set powPlotParam(xdimdisp,${profile_gn}_xsec) 200
       set powPlotParam(ydimdisp,${profile_gn}_xsec) 200
    }

    set powWCSName(${profile_gn}_xsec_curve) $powWCSName($profile_gn)
    set powWCSList(${profile_gn}_xsec_curve) $powWCSList($profile_gn)
    set powFitsHeader(${profile_gn}_xsec) $powFitsHeader($profile_gn)
    set powFitsHeaderCnt(${profile_gn}_xsec) $powFitsHeaderCnt($profile_gn)
    set powPlotParam(graphType,${profile_gn}_xsec) "binary"
    set powPlotParam(zoomed,${profile_gn}_xsec) 0
    set xCount(${profile_gn}_xsec) 0
    set yCount(${profile_gn}_xsec) 0
    set powFitsHeader(${profile_gn}_xsec_curve) $powFitsHeader($profile_gn)
    set powFitsHeaderCnt(${profile_gn}_xsec_curve) $powFitsHeaderCnt($profile_gn)
    set powPlotParam(graphType,${profile_gn}_xsec_curve) "binary"
    set powPlotParam(zoomed,${profile_gn}_xsec_curve) 0
    set xCount(${profile_gn}_xsec_curve) 0
    set yCount(${profile_gn}_xsec_curve) 0
    set powFitsHeader(${profile_gn}_xsecscope) $powFitsHeader($profile_gn)
    set powFitsHeaderCnt(${profile_gn}_xsecscope) $powFitsHeaderCnt($profile_gn)
    set powPlotParam(graphType,${profile_gn}_xsecscope) "binary"
    set powPlotParam(zoomed,${profile_gn}_xsecscope) 0
    set xCount(${profile_gn}_xsecscope) 0
    set yCount(${profile_gn}_xsecscope) 0
    set powWCS(${profile_gn}_xsec) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
    set powWCS(${profile_gn}_xsec_curve) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}

    powCreateGraph ${profile_gn}_xsec ${profile_gn}_xsec_curve NULL \
            $xunit NULL $xlabel Counts \
        $powPlotParam(xdimdisp,${profile_gn}_xsec) \
        $powPlotParam(ydimdisp,${profile_gn}_xsec)
  
    powSetCurveOptions ${profile_gn}_xsec ${profile_gn}_xsec_curve pDisp No 
    powSetCurveOptions ${profile_gn}_xsec ${profile_gn}_xsec_curve lDisp Yes 
    powSetCurveOptions ${profile_gn}_xsec ${profile_gn}_xsec_curve lStyle " " 
    
} 

proc ProfileCallback { obj msg } {
    global powDWP
    global powFitsHeader powFitsHeaderCnt
    global currgn
    set xdlg ${powDWP}xdlg

    if ![winfo exists $xdlg ] {
       return 
    } 
    
    if {$msg == "shapeIsBeingModified"} {
       UpdateProfile $obj
    } 

    if {$msg == "shapeHasChanged"} {
       DrawProfile $obj
       UpdateProfile $obj
    } 

}

proc ProfileDlg {} {
    global currgn
    global currimg
    global powbg
    global powDWP
    global powWCS powFitsHeader powFitsHeaderCnt

    global profile_gn
    global profile_img
    global profileRegion
    global profileWidth
    global profileHeight
    global g_titleFont
    global profileFile
    global powRotation
    global storePowRotation

    set xdlg ${powDWP}xdlg

    if [winfo exists $xdlg ] {
       focus $xdlg
       raise $xdlg
       return
    } 

    if ![info exists currimg ] {
       tk_messageBox -type ok -icon error \
           -message "Select an image first."
       return
    }
    

    powToplevel $xdlg .pow  "-width 200 -bg $powbg"
    wm title $xdlg "Profile" 

    frame $xdlg.frame -borderwidth 4  
    frame $xdlg.frame.title -width 300
    frame $xdlg.frame.row0 
    frame $xdlg.frame.row1
    frame $xdlg.frame.row2
    label $xdlg.frame.title.title -text "Profile:" -background yellow \
            -relief flat   -width 8 -anchor w -font g_titleFont
    button $xdlg.frame.title.help -text Help -anchor e  \
            -command {powHelp Profile.html}  -font g_titleFont
    label $xdlg.frame.row0.holder -text "" -width 18 \
         -anchor w -relief flat -font g_titleFont
    label $xdlg.frame.row0.start -text "Start" -width 25 \
         -anchor w -relief flat -font g_titleFont
    label $xdlg.frame.row0.stop -text "Stop" -width 25  -anchor w -relief flat -font g_titleFont
    label $xdlg.frame.row1.pixelname -text "Image Pixel:" \
              -width 18 -relief flat -anchor w -font g_titleFont
    label $xdlg.frame.row1.pix1 -justify left -anchor w \
             -relief sunken -width 25 -font g_titleFont
    label $xdlg.frame.row1.pix2 -justify left -anchor w \
             -relief sunken -width 25 -font g_titleFont
    label $xdlg.frame.row2.graphname -text "Graph coordinates:" \
              -width 18 -relief flat -anchor w -font g_titleFont
    label $xdlg.frame.row2.graph1 -justify left -anchor w \
           -relief sunken -width 25 -font g_titleFont
    label $xdlg.frame.row2.graph2 -justify left -anchor w \
           -relief sunken -width 25 -font g_titleFont
    button $xdlg.frame.save -text Record -command { SaveProfile } -font g_titleFont
    button $xdlg.frame.close -text Exit -command {
           global storePowRotation
           global currimg
           global powRotation

           if { [info exists currimg] && [info exists storePowRotation($currimg)] } {
              set powRotation($currimg) $storePowRotation($currimg)
           }

           if [info exists profileFile] { unset profileFile } ; \
           itcl::delete object $profileRegion ; \
           destroy ${powDWP}xdlg ; \
    } -font g_titleFont
    pack $xdlg.frame.title.title -side left -anchor w
    pack $xdlg.frame.title.help  -side right -anchor e 
    pack $xdlg.frame.row0.holder  -side left 
    pack $xdlg.frame.row0.start  -side left 
    pack $xdlg.frame.row0.stop -side left
    pack $xdlg.frame.row1.pixelname -side left
    pack $xdlg.frame.row1.pix1 -side left
    pack $xdlg.frame.row1.pix2 -side left
    pack $xdlg.frame.row2.graphname -side left
    pack $xdlg.frame.row2.graph1 -side left
    pack $xdlg.frame.row2.graph2 -side left
    pack $xdlg.frame.title -anchor w -pady 5 -padx 2 -fill x
    pack $xdlg.frame.row0 -anchor w -pady 2  -padx 2
    pack $xdlg.frame.row1 -anchor w -pady 2 -padx 2
    pack $xdlg.frame.row2 -anchor w -pady 2 -padx 2
    pack $xdlg.frame.save  -expand 1 -side left  -anchor w -pady 5 -padx 2
    pack $xdlg.frame.close -expand 1 -side right -anchor e -pady 5 -padx 2
    pack $xdlg.frame

    set profileRegion [gRegionList $currgn .pow.pow]  
    $profileRegion setOwner ProfileCallback
    $profileRegion setAllowsMultiple 0
    $profileRegion setDefault "+" Line 

    set profile_gn $currgn
    set profile_img $currimg

    set temp [ powFetchImageInfoHash  $profile_img ] 
    set temp [split $temp ]
    set profileWidth [lindex $temp 3]
    set profileHeight [lindex $temp 5] 

    set halfx [expr round($profileWidth*0.5) ]
    set halfy [expr round($profileHeight*0.5) ]
    set gx0 [expr round($halfx*0.5) + 1]
    set gy0 [expr round($halfy*0.5) + 1]
    set gx1 [expr round($halfx*1.5) + 1]
    set gy1 [expr round($halfy*1.5) + 1]
    set descr [list $gx0 $gy0 $gx1 $gy1]

    if [info exists powRotation($currimg)] {
       set storePowRotation($currimg) $powRotation($currimg)
       catch { unset powRotation($currimg) }
    }

    $profileRegion addRegion + Line $descr pixels
    set elem [ $profileRegion rgnAtIndex 0]

    bind $xdlg <<CloseWindow>> {
       if [info exists profileFile] {
          unset profileFile
       }
       itcl::delete object profileRegion
       destroy ${powDWP}xdlg
    }

    tkwait window $xdlg
}

proc SaveProfile {} {
    global xlist
    global zlist
    global profileFile
    global graphCoordList

    if { ![info exists profileFile ] || $profileFile ==  "" } {
       set profileFile [tk_getSaveFile -initialfile "powProfile.txt" -filetypes {{ASCII Text .txt}}]
       if [file exists $profileFile] {
          file delete -force $profileFile
       }
    }

    if {$profileFile ==  "" } {
        unset profileFile
        return
    }

    set probeNewFile [file exists $profileFile]
    set fprofile [open $profileFile a]
    foreach ele $graphCoordList x $xlist z $zlist {
        set v1 [lindex $ele 0]
        set v2 [lindex $ele 1]
        set v3 $x
        set v4 $z
#puts "v1: $v1, v2: $v2, v3: $v3, v4: $v4"

        if { $v1 == "NULL" } {
           set v1 0.0
        }

        if { $v2 == "NULL" } {
           set v2 0.0
        }

        if { $v3 == "NULL" } {
           set v3 0.0
        }

        if { $v4 == "NULL" } {
           set v4 0.0
        }

        set temp [format "%-10.7g %-10.7g %-d %-10.7g" $v1 $v2 $v3 $v4]
#puts "data: $temp"
        puts $fprofile $temp
    }

    close $fprofile
}
